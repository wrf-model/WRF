! begin double moment convective microphysics for MSKF

 module module_cu_mp

!module  mskf_microphysics
! Adapted to WRF3.8 by Kiran Alapaty
!  !ckay = !dkay = Kiran Alapaty, EPA
! PSH : Sep 2015: copuled with CESM global climatological aerosol data
! TWG : Jun 2016: porting to WRFV3.8 
! TWG & cKAY: Feb 2017: replaced sheet cloud microphysics with that of cumulus clouds

! Purpose:
!!!!#define WRF_PORT
!   CAM Interface for cumulus microphysics
!
! Initial Authors: Xiaoliang Song and Guang Jun Zhang, June 2010
! MSKF adaptation authors: Kiran Alapaty, Tim Glotfelty, and Patrick Hawbecker
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Adapted to MSKF scheme: Kiran Alapaty at EPA March 2013 (WRF version)
!
!---------------------------------------------------------------------------------
  use shr_kind_mod,  only: r8=>shr_kind_r8

  use error_function, only: erf,erfc

!wrf  use rad_constituents, only: rad_cnst_get_clim_info,
!rad_cnst_get_clim_aer_props 
!xsong 2013-08-22  use module_ra_cam_support, only: naer_cu, idxsul,
!idxDUSTfirst, idxbcphi 

  implicit none
  private
! save

  public :: mskf_mphyi, mskf_mphy, mskf_GAMMA, mskf_polysvp

! Private module data

  integer, parameter :: naer_cu = 10        
  integer, parameter :: pcols = 1

!constants remaped
  real(r8), private::  g              !gravity
  real(r8), private::  mw             !molecular weight of water
  real(r8), private::   r        !Dry air Gas constant
  real(r8), private::   rv       !water vapor gas contstant
  real(r8), private::   rr   !universal gas constant
  real(r8), private::   cpp                  !specific heat of dry air
  real(r8), private::   rhow               !density of liquid water
  real(r8), private::  xlf !latent heat of freezing

!from physconst 
  real(r8), private, parameter ::  gravit = 9.80616_r8      ! acceleration of gravity ~ m/s^2
  real(r8), private, parameter ::  rair   = 287.04239_r8    ! Dry air gas constant     ~ J/K/kg 
  real(r8), private, parameter ::  tmelt  = 273.15_r8       ! freezing T of fresh water  ~ K
  real(r8), private, parameter ::  cpair  = 1.00464e3_r8    ! specific heat of dry air   ~ J/kg/K
  real(r8), private, parameter ::  rh2o   = 461.915_r8      ! Water vapor gas constant ~ J/K/kg
  real(r8), private, parameter ::  r_universal = 8.31447e3_r8  ! Universal gas constant ~ J/K/kmole
  real(r8), private, parameter ::  mwh2o  = 18._r8          ! molecular weight h2o
  real(r8), private, parameter ::  rhoh2o = 1.000e3_R8      ! density of fresh water     ~ kg/m^3
  real(r8), private, parameter ::  latvap = 2.501e6_r8      ! latent heat of evaporation ~ J/kg
  real(r8), private, parameter ::  latice = 3.337e5_r8      ! latent heat of fusion      ~ J/kg
  real(r8), private, parameter ::  epsilo = 0.622_r8        ! ratio of h2o to dry air molecular weights         

!from 'microconstants'
  real(r8), private:: rhosn  ! bulk density snow
  real(r8), private:: rhoi   ! bulk density ice

  real(r8), private:: ac,bc,as,bs,ai,bi,ar,br  !fall speed parameters 
  real(r8), private:: ci,di    !ice mass-diameter relation parameters
  real(r8), private:: cs,ds    !snow mass-diameter relation parameters
  real(r8), private:: cr,dr    !drop mass-diameter relation parameters
  real(r8), private:: Eii      !collection efficiency aggregation of ice
  real(r8), private:: Ecc      !collection efficiency
  real(r8), private:: Ecr      !collection efficiency cloud droplets/rain
  real(r8), private:: DCS      !autoconversion size threshold
  real(r8), private:: F14      !Ferrier (1994) Time scale parameter
  real(r8), private:: qsmall   !min mixing ratio 
  real(r8), private:: bimm,aimm !immersion freezing
  real(r8), private:: rhosu     !typical 850mn air density
  real(r8), private:: mi0       ! new crystal mass
  real(r8), private:: rin       ! radius of contact nuclei
  real(r8), private:: pi       ! pi

  real(r8), private:: rn_dst1, rn_dst2, rn_dst3, rn_dst4  !dust number mean radius for contact freezing
!..........................................................................

!needed for findsp
real(r8), private:: t0       ! Freezing temperature

! activate parameters

      integer, private:: psat
      parameter (psat=6) ! number of supersaturations to calc ccn concentration
      real(r8), private:: aten
!      
      real(r8), private:: alogsig(naer_cu) ! natl log of geometric standard dev of aerosol
      real(r8), private:: exp45logsig(naer_cu)
      real(r8), private:: argfactor(naer_cu)
      real(r8), private:: amcube(naer_cu) ! cube of dry mode radius (m)
      real(r8), private:: smcrit(naer_cu) ! critical supersatuation for activation
      real(r8), private:: lnsm(naer_cu) ! ln(smcrit)
      real(r8), private:: amcubesulfate(pcols) ! cube of dry mode radius (m) of sulfate
      real(r8), private:: smcritsulfate(pcols) ! critical supersatuation for activation of sulfate
      real(r8), private:: amcubefactor(naer_cu) ! factors for calculating mode radius
      real(r8), private:: smcritfactor(naer_cu) ! factors for calculating critical supersaturation
      real(r8), private:: super(psat)
      real(r8), private:: alogten,alog2,alog3,alogaten
      real(r8), private, parameter :: supersat(psat)= &! supersaturation (%) to determine ccn concentration
               (/0.02,0.05,0.1,0.2,0.5,1.0/)
      real(r8), private:: ccnfact(psat,naer_cu)

      real(r8), private:: f1(naer_cu),f2(naer_cu) ! abdul-razzak functions of width
      real(r8), private:: third, sixth,zero
      real(r8), private:: sq2, sqpi


!wrf      integer :: naer_all    ! number of aerosols affecting climate
!xsong 2013-08-22---------------
      integer :: idxsul = 1 ! index in aerosol list for sulfate  
      integer :: idxdst1 = 3 ! index in aerosol list for dust1
      integer :: idxdst2 = 4 ! index in aerosol list for dust2
      integer :: idxdst3 = 5 ! index in aerosol list for dust3
      integer :: idxdst4 = 6 ! index in aerosol list for dust4
      integer :: idxbcphi = 10 ! index in aerosol list for Soot (BCPHI)
!xsong 2013-08-22---------------
      ! aerosol properties
      character(len=20)  aername(naer_cu)
      real(r8) dryrad_aer(naer_cu)
      real(r8) density_aer(naer_cu)
      real(r8) hygro_aer(naer_cu)
      real(r8) dispersion_aer(naer_cu)
      real(r8) num_to_mass_aer(naer_cu)

!xsong 2013-08-22--------------------
   data aername /"SULFATE","SEASALT2","DUST1","DUST2","DUST3","DUST4","OCPHO","BCPHO",   &
                 "OCPHI","BCPHI"/
   data dryrad_aer /0.695E-7_r8,0.200E-5_r8,0.151E-5_r8,0.151E-5_r8,0.151E-5_r8,0.151E-5_r8,     &
                    0.212E-7_r8,0.118E-7_r8,0.212E-7_r8, 0.118E-7_r8/
   data density_aer /1770._r8,2200._r8,2600._r8,2600._r8,2600._r8,2600._r8,1800._r8,  &
                     1000._r8,2600._r8,1000._r8/
   data hygro_aer /0.507_r8,1.160_r8,0.140_r8,0.140_r8,0.140_r8,0.140_r8,0.100_r8,0.100_r8,  &
                   0.140_r8,0.100_r8/
   data dispersion_aer /2.030_r8,1.3732_r8,1.900_r8,1.900_r8,1.900_r8,1.900_r8,2.240_r8,  &
                        2.000_r8,2.240_r8,2.000_r8/
   data num_to_mass_aer /42097098109277080._r8,8626504211623._r8,3484000000000000._r8,213800000000000._r8,&
                         22050000000000._r8,3165000000000._r8,0.745645E+18_r8,0.167226E+20_r8,&
                         0.516216E+18_r8,0.167226E+20_r8/
!xsong 2013-08-22-----------------------


contains

!===============================================================================

subroutine mskf_mphyi

!----------------------------------------------------------------------- 
! 
! Purpose:
! initialize constants for the cumulus microphysics
! called from zm_conv_init() in zm_conv_intr.F90
!
! Author: Xiaoliang Song, June 2010
! 
!-----------------------------------------------------------------------

!     save    ! sep6
!wrf   use pmgrid, only: plev, plevp
      integer k

      integer l,m, iaer
      real(r8) surften       ! surface tension of water w/respect to air (N/m)
!      real(r8) arg

! hm modify to use my error function


!declarations for morrison codes (transforms variable names)

!   g= gravit                  !gravity
!   mw = mwh2o / 1000._r8      !molecular weight of water
!   r= rair                   !Dry air Gas constant: note units(phys_constants
!   are in J/K/kmol)
!   rv= rh2o                   !water vapor gas contstant
!   rr = r_universal           !universal gas constant
!   cpp = cpair                 !specific heat of dry air
!   rhow = rhoh2o              !density of liquid water

!NOTE:
! latent heats should probably be fixed with temperature 
! for energy conservation with the rest of the model
! (this looks like a +/- 3 or 4% effect, but will mess up energy balance)

   xlf = latice          ! latent heat freezing


! from microconstants

! parameters below from Reisner et al. (1998)
! density parameters (kg/m3)

      rhosn = 100._r8    ! bulk density snow
      rhoi = 500._r8     ! bulk density ice
      rhow = 1000._r8    ! bulk density liquid

! fall speed parameters, V = aD^b
! V is in m/s

! droplets
        ac = 3.e7_r8
        bc = 2._r8

! snow
        as = 11.72_r8
        bs = 0.41_r8

! cloud ice
        ai = 700._r8
        bi = 1._r8

! rain
        ar = 841.99667_r8
        br = 0.8_r8

! particle mass-diameter relationship
! currently we assume spherical particles for cloud ice/snow
! m = cD^d

        pi= 3.1415927_r8

! cloud ice mass-diameter relationship

        ci = rhoi*pi/6._r8
        di = 3._r8

! snow mass-diameter relationship

        cs = rhosn*pi/6._r8
        ds = 3._r8

! drop mass-diameter relationship

        cr = rhow*pi/6._r8
        dr = 3._r8

! collection efficiency, aggregation of cloud ice and snow

        Eii = 0.1_r8

! collection efficiency, accretion of cloud water by rain

        Ecr = 1.0_r8

! autoconversion size threshold for cloud ice to snow (m)

!          Dcs = 100.e-6_r8
        Dcs = 200.e-6_r8

! Ferrier [1994] time period parameter ! TWG Feb17
         F14 = 100.0 !180.0 Original
         

! smallest mixing ratio considered in microphysics

        qsmall = 1.e-28_r8 !Shaocai   !1.e-18_r8  

! immersion freezing parameters, bigg 1953

        bimm = 100._r8
        aimm = 0.66_r8

! contact freezing due to dust
! dust number mean radius (m), Zender et al JGR 2003 assuming number mode radius
! of 0.6 micron, sigma=2

        rn_dst1=0.258e-6_r8
        rn_dst2=0.717e-6_r8
        rn_dst3=1.576e-6_r8
        rn_dst4=3.026e-6_r8

! typical air density at 850 mb

        rhosu = 85000._r8/(rair * tmelt)

! mass of new crystal due to aerosol freezing and growth (kg)

        mi0 = 4._r8/3._r8*pi*rhoi*(10.e-6_r8)*(10.e-6_r8)*(10.e-6_r8)

! radius of contact nuclei aerosol (m)

        rin = 0.1e-6_r8

! freezing temperature
        t0=273.15_r8

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! set parameters for droplet activation, following abdul-razzak and ghan 2000,
! JGR

!      mathematical constants

      zero=0._r8
      third=1./3._r8
      sixth=1./6._r8
      sq2=sqrt(2._r8)
      pi=4._r8*atan(1.0_r8)
      sqpi=sqrt(pi)

      surften=0.076_r8
      aten=2.*mwh2o*surften/(r_universal*t0*rhoh2o)
      alogaten=log(aten)
      alog2=log(2._r8)
      alog3=log(3._r8)
      super(:)=0.01*supersat(:)

      do m=1,naer_cu
!         use only if width of size distribution is prescribed
          alogsig(m)=log(dispersion_aer(m))
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
          argfactor(m)=2./(3.*sqrt(2.)*alogsig(m))
          f1(m)=0.5*exp(2.5*alogsig(m)*alogsig(m))
          f2(m)=1.+0.25*alogsig(m)
          amcubefactor(m)=3._r8/(4._r8*pi*exp45logsig(m)*density_aer(m))
          smcritfactor(m)=2._r8*aten*sqrt(aten/(27._r8*max(1.e-10_r8,hygro_aer(m))))
!         use only if mode radius of size distribution is prescribed
          amcube(m)=amcubefactor(m)/(num_to_mass_aer(m))
!         use only if only one component per mode
          if(hygro_aer(m).gt.1.e-10) then
             smcrit(m)=smcritfactor(m)/sqrt(amcube(m))
          else
             smcrit(m)=100.
          endif
          lnsm(m)=log(smcrit(m))

      end do

   return
 end subroutine mskf_mphyi

!===============================================================================

subroutine mskf_mphy(su,    qu,   mu,   du,   cmel, cmei, zf,  pm,  te,   qe, eps0,    &
                   jb,    jt,   jlcl, msg,  il2g, grav, cp,  rd,  qc,   qi, qr, qni, & ! TWG
                    rprd,  wu,    eu,   nc,   ni, nr, ns, dum2l, sprd, frz, aer_mmr, deltat, & !TWG
                   Pver,PverP,gamhat,qsatzm,wu_mskf_act,qc_mskf_act,qi_mskf_act,effc,effi,effs)

! Purpose:
! microphysic parameterization for Zhang-McFarlane convection scheme
! called from cldprp() in zm_conv.F90
!
! Author: Xiaoliang Song, June 2010
!
! Adaptation: Kiran Alapaty at EPA 2013 for MSKF convection scheme in WRF

!wrf  use time_manager,    only: get_nstep, get_step_size

! variable declarations

  implicit none

! input variables

  integer, parameter :: naer_cu = 10   
  integer, parameter :: pcols = 1

  integer :: pver                  ! number of vertical levels(mid-layer)
  integer :: pverp                 ! number of vertical levels(interface)      
  real(r8) :: su(pcols,pver)        ! normalized dry stat energy of updraft
  real(r8) :: qu(pcols,pver)        ! spec hum of updraft
  real(r8) :: mu(pcols,pver)        ! updraft mass flux
  real(r8) :: du(pcols,pver)        ! detrainement rate of updraft
  real(r8) :: cmel(pcols,pver)      ! condensation rate of updraft
  real(r8) :: cmei(pcols,pver)      ! condensation rate of updraft
  real(r8) :: zf(pcols,pverp)       ! height of interfaces
  real(r8) :: pm(pcols,pver)        ! pressure of env
  real(r8) :: te(pcols,pver)        ! temp of env
  real(r8) :: qe(pcols,pver)        ! spec. humidity of env
  real(r8) :: eps0(pcols)
  real(r8) :: eu(pcols,pver)        ! entrainment rate of updraft
!ckay  real(r8) :: aer_mmr(:,:,:)        ! aerosol mass mixing ratio
  real(r8) :: aer_mmr(Pcols,Pver,naer_cu)        ! aerosol mass mixing ratio
!  real(r8) :: gamhat(pcols,pver)    ! kf_GAMMA=L/cp(dq*/dT) at interface
!ckay
  real(r8) :: qsatzm(pcols,pver)        ! spec hum of updraft
  real(r8) :: wu_mskf_act(pver)        ! KF incloud updraft velocity
  real(r8) :: qc_mskf_act(pver)        ! KF incloud updraft velocity
  real(r8) :: qi_mskf_act(pver)        ! KF incloud updraft velocity

  integer :: jb(pcols)              ! updraft base level
  integer :: jt(pcols)              ! updraft plume top
  integer :: jlcl(pcols)            ! updraft lifting cond level
  integer :: msg                    ! missing moisture vals
  integer :: il2g                   ! CORE GROUP REMOVE

  real(r8) grav                                 ! gravity
  real(r8) cp                                   ! heat capacity of dry air
  real(r8) rd                                   ! gas constant for dry air

! output variables
!ckay
  real(r8) qc(pcols,pver)       ! cloud water mixing ratio (kg/kg)
  real(r8) qi(pcols,pver)       ! cloud ice mixing ratio (kg/kg)
  real(r8) nc(pcols,pver)       ! cloud water number conc (1/kg)
  real(r8) ni(pcols,pver)       ! cloud ice number conc (1/kg)
  real(r8)  qni(pcols,pver)      ! snow mixing ratio
  real(r8)  qr(pcols,pver)       ! rain mixing ratio
  real(r8)  ns(pcols,pver)       ! snow number conc
  real(r8)  nr(pcols,pver)       ! rain number conc
  real(r8) rprd(pcols,pver)     ! rate of production of precip at that layer
!ckay  real(r8), intent(out) :: rprd(pcols,pver)     ! rate of production of
!precip at that layer
  real(r8) sprd(pcols,pver)     ! rate of production of snow at that layer
  real(r8) frz(pcols,pver)      ! rate of freezing

! tendency for output

  real(r8) :: autolm(pcols,pver)    !mass tendency due to autoconversion of droplets to rain
  real(r8) :: accrlm(pcols,pver)    !mass tendency due to accretion of droplets by rain
  real(r8) :: bergnm(pcols,pver)    !mass tendency due to Bergeron process
  real(r8) :: fhtimm(pcols,pver)    !mass tendency due to immersion freezing
  real(r8) :: fhtctm(pcols,pver)    !mass tendency due to contact freezing
  real(r8) :: fhmlm (pcols,pver)    !mass tendency due to homogeneous freezing
  real(r8) :: hmpim (pcols,pver)    !mass tendency due to HM process
  real(r8) :: accslm(pcols,pver)    !mass tendency due to accretion of droplets by snow
  real(r8) :: dlfm  (pcols,pver)    !mass tendency due to detrainment of droplet
  real(r8) :: autoln(pcols,pver)    !num tendency due to autoconversion of droplets to rain
  real(r8) :: accrln(pcols,pver)    !num tendency due to accretion of droplets by rain
  real(r8) :: bergnn(pcols,pver)    !num tendency due to Bergeron process
  real(r8) :: fhtimn(pcols,pver)    !num tendency due to immersion freezing
  real(r8) :: fhtctn(pcols,pver)    !num tendency due to contact freezing
  real(r8) :: fhmln (pcols,pver)    !num tendency due to homogeneous freezing
  real(r8) :: accsln(pcols,pver)    !num tendency due to accretion of droplets by snow
  real(r8) :: activn(pcols,pver)    !num tendency due to droplets activation
  real(r8) :: dlfn  (pcols,pver)    !num tendency due to detrainment of droplet
  real(r8) :: autoim(pcols,pver)    !mass tendency due to autoconversion of cloud ice to snow
  real(r8) :: accsim(pcols,pver)    !mass tendency due to accretion of cloud ice by snow
  real(r8) :: difm  (pcols,pver)    !mass tendency due to detrainment of cloud ice 
  real(r8) :: nuclin(pcols,pver)    !num tendency due to ice nucleation
  real(r8) :: nuclim(pcols,pver)    !mass tendency due to ice nucleation
  real(r8) :: collrm(pcols,pver)    !mass tendency due to rain-ice collection
  real(r8) :: collrn(pcols,pver)    !number tendency due to rain-ce collection
  real(r8) :: fhtcrm(pcols,pver)    !mass tendency due to rain freezing to snow
  real(r8) :: fhtcrn(pcols,pver)    !num tendency due to rain freezing to snow
  real(r8) :: autorn(pcols,pver)    !num tendency for autoconversion of clouds to rain (rain term)
  real(r8) :: aggrn(pcols,pver)     !num tendency for self collection of rain
  real(r8) :: aggsn(pcols,pver)     !num tendency for self collection of snow
  real(r8) :: autoin(pcols,pver)    !num tendency due to autoconversion of cloud ice to snow
  real(r8) :: accsin(pcols,pver)    !num tendency due to accretion of cloud ice by snow
  real(r8) :: hmpin (pcols,pver)    !num tendency due to HM process
  real(r8) :: difn  (pcols,pver)    !num tendency due to detrainment of cloud ice
  real(r8) :: trspcm(pcols,pver)    !LWC tendency due to convective transport
  real(r8) :: trspcn(pcols,pver)    !droplet num tendency due to convective transport
  real(r8) :: trspim(pcols,pver)    !IWC tendency due to convective transport
  real(r8) :: trspin(pcols,pver)    !ice crystal num tendency due to convective transport

  real(r8) :: ncadj(pcols,pver)     !droplet num tendency due to adjustment
  real(r8) :: niadj(pcols,pver)     !ice crystal num tendency due to adjustment
  real(r8) :: qcadj(pcols,pver)     !droplet mass tendency due to adjustment
  real(r8) :: qiadj(pcols,pver)     !ice crystal mass tendency due to adjustment

! output for ice nucleation
  real(r8) :: nimey(pcols,pver)     !output number conc of ice nuclei due to meyers deposition (1/m3)
  real(r8) :: nihf(pcols,pver)      !output number conc of ice nuclei due to heterogenous freezing (1/m3)
  real(r8) :: nidep(pcols,pver)     !output number conc of ice nuclei due to deoposion nucleation (hetero nuc) (1/m3)
  real(r8) :: niimm(pcols,pver)     !output number conc of ice nuclei due to immersion freezing (hetero nuc) (1/m3)
  real(r8) :: effc(pcols,pver)    ! droplet effective radius (micron)
  real(r8) :: effi(pcols,pver)    ! cloud ice effective radius (micron)
  real(r8) :: effs(pcols,pver)    ! snow effective radius (micron)

!................................................................................
! local workspace
! all units mks unless otherwise stated
  real(r8) :: deltat                ! time step (s)
  real(r8) :: omsm                  ! number near unity for round-off issues
  real(r8) :: dum                   ! temporary dummy variable
  real(r8) :: arg                   ! argument of erfc
  real(r8) :: dum1                  ! temporary dummy variable 
  real(r8) :: dum2                  ! temporary dummy variable

  real(r8) :: q(pcols,pver)         ! water vapor mixing ratio (kg/kg)
  real(r8) :: t(pcols,pver)         ! temperature (K)
  real(r8) :: rho(pcols,pver)       ! air density (kg m-3)
  real(r8) :: dz(pcols,pver)        ! height difference across model verticallevel

  real(r8) :: qcic(pcols,pver)      ! in-cloud cloud liquid mixing ratio
  real(r8) :: qiic(pcols,pver)      ! in-cloud cloud ice mixing ratio
!dkay
  real (r8) :: tot_qc_qi
  real(r8) :: qniic(pcols,pver)     ! in-precip snow mixing ratio
  real(r8) :: qric(pcols,pver)      ! in-precip rain mixing ratio
  real(r8) :: ncic(pcols,pver)      ! in-cloud droplet number conc
  real(r8) :: niic(pcols,pver)      ! in-cloud cloud ice number conc
  real(r8) :: nsic(pcols,pver)      ! in-precip snow number conc
  real(r8) :: nric(pcols,pver)      ! in-precip rain number conc

  real(r8) :: lami(pver)            ! slope of cloud ice size distr
  real(r8) :: n0i(pver)             ! intercept of cloud ice size distr
  real(r8) :: lamc(pver)            ! slope of cloud liquid size distr
  real(r8) :: n0c(pver)             ! intercept of cloud liquid size distr
  real(r8) :: lams(pver)            ! slope of snow size distr
  real(r8) :: n0s(pver)             ! intercept of snow size distr
  real(r8) :: lamr(pver)            ! slope of rain size distr
  real(r8) :: n0r(pver)             ! intercept of rain size distr
  real(r8) :: cdist1(pver)          ! size distr parameter to calculate droplet freezing
  real(r8) :: pgam(pver)            ! spectral width parameter of droplet size distr
  real(r8) :: lammax                ! maximum allowed slope of size distr
  real(r8) :: lammin                ! minimum allowed slope of size distr

  real(r8) :: mnuccc(pver)          ! mixing ratio tendency due to freezing of cloud water
  real(r8) :: nnuccc(pver)          ! number conc tendency due to freezing of cloud water
  real(r8) :: mnucct(pver)          ! mixing ratio tendency due to contact freezing of cloud water
  real(r8) :: nnucct(pver)          ! number conc tendency due to contact freezing of cloud water
  real(r8) :: msacwi(pver)          ! mixing ratio tendency due to HM ice multiplication
  real(r8) :: nsacwi(pver)          ! number conc tendency due to HM ice multiplication
  real(r8) :: prf(pver)             ! mixing ratio tendency due to fallout of rain
  real(r8) :: psf(pver)             ! mixing ratio tendency due to fallout of snow
  real(r8) :: pnrf(pver)            ! number conc tendency due to fallout of rain
  real(r8) :: pnsf(pver)            ! number conc tendency due to fallout of snow
  real(r8) :: prc(pver)             ! mixing ratio tendency due to autoconversion of cloud droplets
  real(r8) :: nprc(pver)            ! number conc tendency due to autoconversion of cloud droplets
  real(r8) :: nprc1(pver)           ! qr tendency due to autoconversion of cloud droplets
  real(r8) :: nsagg(pver)           ! ns tendency due to self-aggregation of snow
  real(r8) :: dc0                   ! mean size droplet size distr
  real(r8) :: ds0                   ! mean size snow size distr (area weighted)
  real(r8) :: eci                   ! collection efficiency for riming of snow by droplets
  real(r8) :: dv(pcols,pver)        ! diffusivity of water vapor in air
  real(r8) :: mua(pcols,pver)       ! viscocity of air
  real(r8) :: psacws(pver)          ! mixing rat tendency due to collection of droplets by snow
  real(r8) :: npsacws(pver)         ! number conc tendency due to collection of droplets by snow
  real(r8) :: pracs(pver)           ! mixing rat tendency due to collection of rain by snow
  real(r8) :: npracs(pver)          ! number conc tendency due to collection of rain by snow
  real(r8) :: mnuccr(pver)          ! mixing rat tendency due to freezing of rain
  real(r8) :: nnuccr(pver)          ! number conc tendency due to freezing of rain
  real(r8) :: pra(pver)             ! mixing rat tendnency due to accretion of droplets by rain
  real(r8) :: npra(pver)            ! nc tendnency due to accretion of droplets by rain
  real(r8) :: nragg(pver)           ! nr tendency due to self-collection of rain
  real(r8) :: prci(pver)            ! mixing rat tendency due to autoconversion of cloud ice to snow
  real(r8) :: nprci(pver)           ! number conc tendency due to autoconversion of cloud ice to snow
  real(r8) :: prai(pver)            ! mixing rat tendency due to accretion of cloud ice by snow
  real(r8) :: nprai(pver)           ! number conc tendency due to accretion of cloud ice by snow
  real(r8) :: prb(pver)             ! rain mixing rat tendency due to Bergeron process
  real(r8) :: nprb(pver)            ! number conc tendency due to Bergeron process


! fall speed
  real(r8) :: arn(pcols,pver)       ! air density corrected rain fallspeed
  real(r8) :: asn(pcols,pver)       ! air density corrected snow fallspeed
  real(r8) :: acn(pcols,pver)       ! air density corrected cloud droplet fallspeed parameter
  real(r8) :: ain(pcols,pver)       ! air density corrected cloud ice fallspeed
  real(r8) :: uns(pver)             ! number-weighted snow fallspeed
  real(r8) :: ums(pver)             ! mass-weighted snow fallspeed
  real(r8) :: unr(pver)             ! number-weighted rain fallspeed
  real(r8) :: umr(pver)             ! mass-weighted rain fallspeed

! conservation check
  real(r8) :: qce                   ! dummy qc for conservation check
  real(r8) :: qie                   ! dummy qi for conservation check
  real(r8) :: nce                   ! dummy nc for conservation check
  real(r8) :: nie                   ! dummy ni for conservation check
  real(r8) :: qre                   ! dummy qr for conservation check
  real(r8) :: nre                   ! dummy nr for conservation check
  real(r8) :: qnie                  ! dummy qni for conservation check
  real(r8) :: nse                   ! dummy ns for conservation check      
  real(r8) :: ratio                 ! parameter for conservation check

! sum of source/sink terms for cloud hydrometeor
  real(r8) :: qctend(pcols,pver)    ! microphysical tendency qc (1/s)
  real(r8) :: qitend(pcols,pver)    ! microphysical tendency qi (1/s)
  real(r8) :: nctend(pcols,pver)    ! microphysical tendency nc (1/(kg*s))
  real(r8) :: nitend(pcols,pver)    ! microphysical tendency ni (1/(kg*s))
  real(r8) :: qnitend(pcols,pver)   ! snow mixing ratio source/sink term
  real(r8) :: nstend(pcols,pver)    ! snow number concentration source/sink term
  real(r8) :: qrtend(pcols,pver)    ! rain mixing ratio source/sink term
  real(r8) :: nrtend(pcols,pver)    ! rain number concentration source/sink term

! terms for Bergeron process
  real(r8) :: bergtsf               !bergeron timescale to remove all liquid
  real(r8) :: plevap                ! cloud liquid water evaporation rate

! aerosol variables
  real(r8) :: naermod(naer_cu)      ! aerosol number concentration (/m3)
  real(r8) :: naer2(pcols,pver,naer_cu)    ! new aerosol number concentration (/m3)
  real(r8) :: naer2h(pcols,pver,naer_cu)   ! new aerosol number concentration (/m3) 
  real(r8) :: maerosol(1,naer_cu)   ! aerosol mass conc (kg/m3)
  real(r8) naer(pcols)

! droplet activation
  real(r8) :: dum2l(pcols,pver)     ! number conc of CCN (1/kg)
  real(r8) :: npccn(pver)           ! droplet activation rate
  real(r8) :: ncmax
  real(r8) :: mtimec                ! factor to account for droplet activation timescale

! ice nucleation
  real(r8) :: dum2i(pcols,pver)     ! number conc of ice nuclei available (1/kg)
  real(r8) :: qs(pcols,pver)        ! liquid-ice weighted sat mixing rat (kg/kg)
  real(r8) :: es(pcols,pver)        ! sat vapor press (pa) over water
  real(r8) :: relhum(pcols,pver)    ! relative humidity
  real(r8) :: esi(pcols,pver)       ! sat vapor press (pa) over ice
  real(r8) :: nnuccd(pver)          ! ice nucleation rate from deposition/cond.-freezing
  real(r8) :: mnuccd(pver)          ! mass tendency from ice nucleation
  real(r8) :: nimax
  real(r8) :: mtime                 ! factor to account for ice nucleation timescale
  real(r8) :: gamhat(pcols,pver)    ! kf_GAMMA=L/cp(dq*/dT) at interface


! loop array variables
  integer i,k,nstep,n, l
  integer ii,kk, m

! loop variables for iteration solution
  integer iter,it,ltrue(pcols)

! used in contact freezing via dust particles
  real(r8)  tcnt, viscosity, mfp
  real(r8)  slip1, slip2, slip3, slip4
  real(r8)  dfaer1, dfaer2, dfaer3, dfaer4
  real(r8)  nacon1,nacon2,nacon3,nacon4

! used in immersion freezing via soot
  real(r8) ttend(pver)
  real(r8) naimm
  real(r8) :: ntaer(pcols,pver)
  real(r8) :: ntaerh(pcols,pver)

! used in secondary ice production
  real(r8) ni_secp

! used in vertical velocity calculation
  real(r8) th(pcols,pver)
  real(r8) qh(pcols,pver)
  real(r8) wu(pcols,pver)
  real(r8) zkine(pcols,pver)
  real(r8) zbuo(pcols,pver)
  real(r8) zfacbuo, cwdrag, cwifrac, retv,  zbuoc
  real(r8) zbc, zbe,  zdkbuo, zdken
  real(r8) arcf(pcols,pver)
  real(r8) p(pcols,pver)
  real(r8) ph(pcols,pver)

  real(r8) :: rhoh(pcols,pver)    ! air density (kg m-3) at interface 
  real(r8) :: rhom(pcols,pver)    ! air density (kg m-3) at mid-level
  real(r8) :: tu(pcols,pver)      ! temperature in updraft (K)

  real(r8) :: fhmrm (pcols,pver)  !mass tendency due to homogeneous freezing of rain

  real(r8) ncorg,niorg,qcorg,qiorg

  integer  kqi(pcols),kqc(pcols)
  logical  lcbase(pcols), libase(pcols)

!ckay introduced save sep6
! save

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! initialization
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


! parameters for scheme
        omsm=0.99999_r8
        zfacbuo = 0.5_r8/(1._r8+0.5_r8)
        cwdrag  = 1.875_r8*0.506_r8
        cwifrac = 0.5_r8
        retv    = 0.608_r8
        bergtsf = 1800._r8

! initialize multi-level fields
        do i=1,il2g
          do k=1,pver
            q(i,k)= qu(i,k)
            tu(i,k)= su(i,k) - grav/cp*zf(i,k)
            t(i,k)= su(i,k) - grav/cp*zf(i,k)
            p(i,k) = 100._r8*pm(i,k)
            wu(i,k)  = 0._r8
            zkine(i,k)= 0._r8
            arcf(i,k) = 0._r8
            zbuo(i,k) = 0._r8
            nc(i,k) = 0._r8
            ni(i,k) = 0._r8
            qc(i,k) = 0._r8
            qi(i,k) = 0._r8
            qcic(i,k) = 0._r8
            qiic(i,k) = 0._r8
            ncic(i,k) = nc(i,k)
            niic(i,k) = ni(i,k)
            qr(i,k) = 0._r8
            qni(i,k)= 0._r8
            nr(i,k) = 0._r8
            ns(i,k) = 0._r8
            qric(i,k) = qr(i,k)
            qniic(i,k) = qni(i,k)
            nric(i,k) = nr(i,k)
            nsic(i,k) = ns(i,k)
            nimey(i,k) = 0._r8
            nihf(i,k)  = 0._r8
            nidep(i,k) = 0._r8
            niimm(i,k) = 0._r8

            autolm(i,k) = 0._r8
            accrlm(i,k) = 0._r8
            bergnm(i,k) = 0._r8
            fhtimm(i,k) = 0._r8
            fhtctm(i,k) = 0._r8
            fhmlm (i,k) = 0._r8
            hmpim (i,k) = 0._r8
            accslm(i,k) = 0._r8
            dlfm  (i,k) = 0._r8
            collrm(i,k) = 0._r8
            collrn(i,k) = 0._r8
            fhtcrm(i,k) = 0._r8
            fhtcrn(i,k) = 0._r8
            autorn(i,k) = 0._r8
             aggrn(i,k) = 0._r8
             aggsn(i,k) = 0._r8

            autoln(i,k) = 0._r8

            accrln(i,k) = 0._r8
            bergnn(i,k) = 0._r8
            fhtimn(i,k) = 0._r8
            fhtctn(i,k) = 0._r8
            fhmln (i,k) = 0._r8
            accsln(i,k) = 0._r8
            activn(i,k) = 0._r8
            dlfn  (i,k) = 0._r8
            ncadj (i,k) = 0._r8
            qcadj (i,k) = 0._r8
!cloud ice------------------------
           autoim(i,k) = 0._r8
            accsim(i,k) = 0._r8
            difm  (i,k) = 0._r8
            nuclin(i,k) = 0._r8
            nuclim(i,k) = 0._r8
            autoin(i,k) = 0._r8
            accsin(i,k) = 0._r8
            hmpin (i,k) = 0._r8
            difn  (i,k) = 0._r8
            niadj (i,k) = 0._r8
            qiadj (i,k) = 0._r8

            trspcm(i,k) = 0._r8
            trspcn(i,k) = 0._r8
            trspim(i,k) = 0._r8
            trspin(i,k) = 0._r8

            effc(i,k) = 0._r8
            effi(i,k) = 0._r8
            effs(i,k) = 0._r8

            fhmrm (i,k) = 0._r8
          end do
        end do


! initialize time-varying parameters
        do k=1,pver
          do i=1,il2g
!-------------Shaocai Yu
             if (k .eq.1) then
                rhoh(i,k) = p(i,k)/(t(i,k)*rd)
                rhom(i,k) = p(i,k)/(t(i,k)*rd)
                th (i,k) = te(i,k)
                qh (i,k) = qe(i,k)
                dz (i,k)  = zf(i,k) - zf(i,k+1)
                ph(i,k)   = p(i,k)
             else

               rhoh(i,k) = 0.5_r8*(p(i,k)+p(i,k-1))/(t(i,k)*rd)
                if (k .eq. pver) then
                  rhom(i,k) = p(i,k)/(rd*t(i,k))
                else
                  rhom(i,k) = 2.0_r8*p(i,k)/(rd*(t(i,k)+t(i,k+1)))
                end if
                th (i,k) = 0.5_r8*(te(i,k)+te(i,k-1))
                qh (i,k) = 0.5_r8*(qe(i,k)+qe(i,k-1))
                dz(i,k)  = zf(i,k-1) - zf(i,k)
                ph(i,k)  = 0.5_r8*(p(i,k) + p(i,k-1))
             end if

            dv(i,k) = 8.794E-5_r8*t(i,k)**1.81_r8/ph(i,k)
            mua(i,k) = 1.496E-6_r8*t(i,k)**1.5_r8/ &
                     (t(i,k)+120._r8)

            rho(i,k) = rhoh(i,k)

! air density adjustment for fallspeed parameters
! add air density correction factor to the power of 
! 0.54 following Heymsfield and Bansemer 2006

            arn(i,k)=ar*(rhosu/rho(i,k))**0.54
            asn(i,k)=as*(rhosu/rho(i,k))**0.54
            acn(i,k)=ac*(rhosu/rho(i,k))**0.54
            ain(i,k)=ai*(rhosu/rho(i,k))**0.54

          end do
        end do

! initialize aerosol number
        do k=1,pver
          do i=1,il2g
            naer2(i,k,:)=0._r8
            naer2h(i,k,:)=0._r8
            dum2l(i,k)=0._r8
            dum2i(i,k)=0._r8
          end do
        end do

        do k=1,pver
          do i=1,il2g
            ntaer(i,k) = 0.0_r8
            ntaerh(i,k) = 0.0_r8
            do m=1,naer_cu

              maerosol(1,m)=aer_mmr(i,k,m)*rhom(i,k)

!------------------------------------------------------------------           
        
! set number nucleated for sulfate based on Lohmann et al. 2000 (JGR) Eq.2
!    Na=340.*(massSO4)^0.58  where Na=cm-3 and massSO4=ug/m3
! convert units to Na [m-3] and SO4 [kgm-3]
!    Na(m-3)= 1.e6 cm3 m-3 Na(cm-3)=340. *(massSO4[kg/m3]*1.e9ug/kg)^0.58
!    or Na(m-3)= 1.e6* 340.*(1.e9ug/kg)^0.58 * (massSO4[kg/m3])^0.58
              if(m .eq. idxsul) then
                naer2(i,k,m)= 5.64259e13_r8 * maerosol(1,m)**0.58
              else
                naer2(i,k,m)=maerosol(1,m)*num_to_mass_aer(m)
              endif
                ntaer(i,k) = ntaer(i,k) + naer2(i,k,m)
            enddo
          end do ! i loop
        end do ! k loop

        do i=1,il2g
          ltrue(i)=0
          do k=1,pver
            if (qc(i,k).ge.qsmall.or.qi(i,k).ge.qsmall.or.cmel(i,k).ge.qsmall.or.cmei(i,k).ge.qsmall) ltrue(i)=1
!            print *,'qc flag =',ltrue(i)
          end do
        end do

! skip microphysical calculations if no cloud water
      do i=1,il2g
        if (ltrue(i).eq.0) then
          do k=1,pver
            qctend(i,k)=0._r8
            qitend(i,k)=0._r8
            qnitend(i,k)=0._r8
            qrtend(i,k)=0._r8
            nctend(i,k)=0._r8
            nitend(i,k)=0._r8
            nrtend(i,k)=0._r8
            nstend(i,k)=0._r8
            qniic(i,k)=0._r8
            qric(i,k)=0._r8
            nsic(i,k)=0._r8
            nric(i,k)=0._r8
            qni(i,k)=0._r8
            qr(i,k)=0._r8
            ns(i,k)=0._r8
            nr(i,k)=0._r8
            qc(i,k) = 0._r8
            qi(i,k) = 0._r8
            nc(i,k) = 0._r8
            ni(i,k) = 0._r8
            rprd(i,k) = 0._r8
            sprd(i,k) = 0._r8
            frz(i,k) = 0._r8
          end do
          goto 300
        end if

        kqc(i) = 1
        kqi(i) = 1
        lcbase(i) = .true.
        libase(i) = .true.

! assign number of steps for iteration
! use 2 steps following Song and Zhang, 2011, J. Clim.
        iter = 2  !5 !Shaocai Yu !2

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!  iteration
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

        do it=1,iter
! initialize sub-step microphysical tendencies
         do k=1,pver
           qctend(i,k)=0._r8
           qitend(i,k)=0._r8
           qnitend(i,k)=0._r8
           qrtend(i,k)=0._r8
           nctend(i,k)=0._r8
           nitend(i,k)=0._r8
           nrtend(i,k)=0._r8
           nstend(i,k)=0._r8
           rprd(i,k) = 0._r8
           sprd(i,k) = 0._r8
           frz(i,k)  = 0._r8
           qniic(i,k)=0._r8
           qric(i,k)=0._r8
           nsic(i,k)=0._r8
           nric(i,k)=0._r8
           qiic(i,k)=0._r8
           qcic(i,k)=0._r8
           niic(i,k)=0._r8
           ncic(i,k)=0._r8
!<songxl 2012-01-06---------------
            accrlm(i,k) = 0._r8
            bergnm(i,k) = 0._r8
            fhtimm(i,k) = 0._r8
            fhtctm(i,k) = 0._r8
            fhmlm (i,k) = 0._r8
            hmpim (i,k) = 0._r8
            accslm(i,k) = 0._r8
            dlfm  (i,k) = 0._r8

            autoln(i,k) = 0._r8
            accrln(i,k) = 0._r8
            bergnn(i,k) = 0._r8
            fhtimn(i,k) = 0._r8
            fhtctn(i,k) = 0._r8
            fhmln (i,k) = 0._r8
            accsln(i,k) = 0._r8
            activn(i,k) = 0._r8
            dlfn  (i,k) = 0._r8
            ncadj (i,k) = 0._r8
            qcadj (i,k) = 0._r8

            autoim(i,k) = 0._r8
            accsim(i,k) = 0._r8
            difm  (i,k) = 0._r8

            nuclin(i,k) = 0._r8
            nuclim(i,k) = 0._r8
            autoin(i,k) = 0._r8
            accsin(i,k) = 0._r8
            hmpin (i,k) = 0._r8
            difn  (i,k) = 0._r8
            niadj (i,k) = 0._r8
            qiadj (i,k) = 0._r8

            trspcm(i,k) = 0._r8
            trspcn(i,k) = 0._r8
            trspim(i,k) = 0._r8
            trspin(i,k) = 0._r8

            effc(i,k) = 0._r8
            effi(i,k) = 0._r8
            effs(i,k) = 0._r8

            fhmrm (i,k) = 0._r8
!songxl 2012-01-06>---------------
!songxl 2012-01-06>---------------
         end do

!---------------------Shaocai
!        goto 9910  !Stop2
        
         do k = pver,msg+2,-1

!within the cloud processing...
            if (k > jt(i) .and. k <= jb(i) .and. eps0(i) > 0._r8      &
              .and.mu(i,k).gt.0._r8 .and. mu(i,k-1).gt.0._r8) then

! initialize precip fallspeeds to zero
            ums(k)=0._r8
            uns(k)=0._r8
            umr(k)=0._r8
            unr(k)=0._r8
            prf(k)=0._r8
            pnrf(k)=0._r8
            psf(k) =0._r8
            pnsf(k) = 0._r8
            ttend(k)=0._r8
            nnuccd(k)=0._r8
            npccn(k)=0._r8

!************************************************************************************
! obtain values of cloud water/ice mixing ratios and number concentrations in
! updraft
! for microphysical process calculations
! units are kg/kg for mixing ratio, 1/kg for number conc
!************************************************************************************

! limit values to 0.005 kg/kg
!dkay      qc(i,k)=min(qc(i,k),5.e-3_r8)
!dkay      qi(i,k)=min(qi(i,k),5.e-3_r8)
           nc(i,k)=max(nc(i,k),0._r8)
           ni(i,k)=max(ni(i,k),0._r8)
           if (it.eq.1) then
             qcic(i,k) = qc(i,k)
             qiic(i,k) = qi(i,k)
!             print *,'at it=1',qcic(i,k),k,it
!dkay
!            qcic(i,k) = qc_kf_act(k)
!            qiic(i,k) = qi_kf_act(k)
!dkay
             ncic(i,k) = nc(i,k)
             niic(i,k) = ni(i,k)
             qniic(i,k)= qni(i,k)
             qric(i,k) = qr(i,k)
             nsic(i,k) = ns(i,k)
             nric(i,k) = nr(i,k)
           else  ! for it 
             if (k.le.kqc(i)) then
                qcic(i,k) = qc(i,k)
                ncic(i,k) = nc(i,k)
                if (k.eq.kqc(i)) then
                  qcic(i,k) = qc(i,k-1)
                  ncic(i,k) = nc(i,k-1)
                end if
! consider rain falling from above
                do kk= k,jt(i)+2,-1
                   qric(i,k) = qr(i,k) + max(0._r8, qr(i,kk-1)-qr(i,kk-2) )
                   if (qr(i,kk-1) .gt. 0._r8)  &
                   nric(i,k) = nr(i,k) + max(0._r8,qr(i,kk-1)-qr(i,kk-2))/qr(i,kk-1)*nr(i,kk-1)
                end do
             end if
             if(k.le.kqi(i)) then
                qiic(i,k) = qi(i,k)
                niic(i,k) = ni(i,k)
                if(k.eq.kqi(i)) then
                  qiic(i,k) = qi(i,k-1)
                  niic(i,k) = ni(i,k-1)
                end if
! consider snow falling from above
                do kk= k,jt(i)+2,-1
                  qniic(i,k) = qni(i,k) + max(0._r8, qni(i,kk-1)-qni(i,kk-2) )
                  if (qni(i,kk-1) .gt. 0._r8)  &
                  nsic(i,k) = ns(i,k) + max(0._r8,qni(i,kk-1)-qni(i,kk-2))/qni(i,kk-1)*ns(i,kk-1)
                end do
             end if
           end if

            if(it.eq.1) then
!              print
!              *,'qcic,qiic=',qcic(i,k),qiic(i,k),i,k,cmel(i,k),cmei(i,k),tu(i,k),it
            end if
!**********************************************************************
! boundary condition for cloud liquid water and cloud ice
!***********************************************************************

! boundary condition for provisional cloud water
        if (cmel(i,k-1).gt.qsmall .and. lcbase(i) .and. it.eq.1 ) then
             kqc(i) = k
             lcbase(i) = .false.
             qcic(i,k) = dz(i,k)*cmel(i,k-1)/(mu(i,k-1)+dz(i,k)*du(i,k-1))
             if(qcic(i,k).eq.0.0) then
              if(it.eq.1) then
!              print *,'dz,cmel...',
!              dz(i,k),cmel(i,k+1),mu(i,k+1),dz(i,k),du(i,k+1)
              end if
             end if
             ncic(i,k) = qcic(i,k)/(4._r8/3._r8*pi*11.e-6_r8**3*rhow)
         end if

! boundary condition for provisional cloud ice
         if (qiic(i,k).gt.qsmall .and. libase(i) .and. it.eq.1 ) then
             kqi(i) = k
             libase(i) = .false.
         else if ( cmei(i,k-1).gt.qsmall .and.   &
             cmei(i,k).lt.qsmall .and. k.lt.jb(i) .and. libase(i) .and. it.eq.1) then
             kqi(i)=k
             libase(i) = .false.
             qiic(i,k) = dz(i,k)*cmei(i,k-1)/(mu(i,k-1)+dz(i,k)*du(i,k-1))
             niic(i,k) = qiic(i,k)/(4._r8/3._r8*pi*25.e-6_r8**3*rhoi)
         end if

!***************************************************************************
! get size distribution parameters based on in-cloud cloud water/ice 
! these calculations also ensure consistency between number and mixing ratio
!***************************************************************************
! cloud ice
           if (qiic(i,k).ge.qsmall) then
! add upper limit to in-cloud number concentration to prevent numerical error
              niic(i,k)=min(niic(i,k),qiic(i,k)*1.e20_r8)
              lami(k) = (mskf_GAMMA(1._r8+di)*ci* &
                  niic(i,k)/qiic(i,k))**(1._r8/di)
              n0i(k) = niic(i,k)*lami(k)
! check for slope
              lammax = 1._r8/10.e-6_r8
              lammin = 1._r8/(2._r8*dcs)
! adjust vars
              if (lami(k).lt.lammin) then
                lami(k) = lammin
                n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*mskf_GAMMA(1._r8+di))
                niic(i,k) = n0i(k)/lami(k)
              else if (lami(k).gt.lammax) then
                lami(k) = lammax
                n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*mskf_GAMMA(1._r8+di))
                niic(i,k) = n0i(k)/lami(k)
              end if
           else
              lami(k) = 0._r8
              n0i(k) = 0._r8
           end if

!cloud water
           if (qcic(i,k).ge.qsmall) then

! add upper limit to in-cloud number concentration to prevent numerical error
              ncic(i,k)=min(ncic(i,k),qcic(i,k)*1.e20_r8)

! get pgam from fit to observations of martin et al. 1994

              pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8  ! TWG 2017 change / to * for consistenct with Morrison
              pgam(k)=1._r8/(pgam(k)**2)-1._r8
              pgam(k)=max(pgam(k),2._r8)
              pgam(k)=min(pgam(k),15._r8)

! calculate lamc
              lamc(k) = (pi/6._r8*rhow*ncic(i,k)*mskf_GAMMA(pgam(k)+4._r8)/ &
                 (qcic(i,k)*mskf_GAMMA(pgam(k)+1._r8)))**(1._r8/3._r8)

! lammin, 50 micron diameter max mean size
              lammin = (pgam(k)+1._r8)/50.e-6_r8
              lammax = (pgam(k)+1._r8)/2.e-6_r8

              if (lamc(k).lt.lammin) then
                 lamc(k) = lammin
                 ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                      mskf_GAMMA(pgam(k)+1._r8)/ &
                      (pi*rhow*mskf_GAMMA(pgam(k)+4._r8))
              else if (lamc(k).gt.lammax) then
                 lamc(k) = lammax
                 ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                       mskf_GAMMA(pgam(k)+1._r8)/ &
                      (pi*rhow*mskf_GAMMA(pgam(k)+4._r8))
              end if

! parameter to calculate droplet freezing

              cdist1(k) = ncic(i,k)/mskf_GAMMA(pgam(k)+1._r8)
           else
              lamc(k) = 0._r8
              cdist1(k) = 0._r8
           end if
! boundary condition for cloud liquid water
         if ( kqc(i) .eq. k  ) then
              qc(i,k) =  0._r8
              nc(i,k) = 0._r8
          end if
! boundary condition for cloud ice
          if (kqi(i).eq.k  ) then
             qi(i,k) = 0._r8
             ni(i,k) = 0._r8
          end if

!**************************************************************************
! begin micropysical process calculations 
!**************************************************************************

!.................................................................
! autoconversion of cloud liquid water to rain
! formula from Khrouditnov and Kogan (2000)
! minimum qc of 1 x 10^-8 prevents floating point error

           if (qcic(i,k).ge.1.e-8_r8) then

! nprc is increase in rain number conc due to autoconversion
! nprc1 is decrease in cloud droplet conc due to autoconversion

!TWG Feb 2017 Update from Khrouditnov and Kogan (2000) to Kogan (2013) for
!convection

!              prc(k) = 1350._r8*qcic(i,k)**2.47_r8*    &
!                    (ncic(i,k)/1.e6_r8*rho(i,k))**(-1.79_r8)
              prc(k) = 7.98E10_r8*qcic(i,k)**4.22_r8*    &
                    (ncic(i,k)/1.e6_r8*rho(i,k))**(-3.01_r8)

              nprc(k) = prc(k)/(4._r8/3._r8*pi*rhow*(25.e-6_r8)**3)
              nprc1(k) = prc(k)/(qcic(i,k)/ncic(i,k))
           else
              prc(k)=0._r8
              nprc(k)=0._r8
              nprc1(k)=0._r8
           end if

! provisional rain mixing ratio and number concentration (qric and nric)
! at boundary are estimated via autoconversion

         if (k.eq.kqc(i) .and. it.eq.1) then
             qric(i,k) = prc(k)*dz(i,k)/0.55_r8
             nric(i,k) = nprc(k)*dz(i,k)/0.55_r8
             qr(i,k) = 0.0_r8
             nr(i,k) = 0.0_r8
         end if
!          print *,'qric,nric,qr,nr afer autoconversion cld water to rain'
!          print *, 'qric=',qric
!          print *,
!          'nric=',nric(i,15),i,nprc(15),prc(15),ncic(i,15),rhow,qcic(i,15)
!          print *, 'qr=',qr
!          print *, 'qr=',qr

!.......................................................................

! similar to Ferrier (1994)

           if (t(i,k).le.273.15_r8.and.qiic(i,k).ge.qsmall) then

! note: assumes autoconversion timescale of 180 sec  !TWG Feb17 adjust
! autoconversion time scale
              ! nprci(k) = n0i(k)/(lami(k)*180._r8)*exp(-lami(k)*dcs)
              nprci(k) = n0i(k)/(lami(k)*F14)*exp(-lami(k)*dcs)
             ! prci(k) = pi*rhoi*n0i(k)/(6._r8*180._r8)* &
               prci(k) = pi*rhoi*n0i(k)/(6._r8*F14)* &  
                  (dcs**3/lami(k)+3._r8*dcs**2/lami(k)**2+ &
                  6._r8*dcs/lami(k)**3+6._r8/lami(k)**4)*exp(-lami(k)*dcs)
           else
              prci(k)=0._r8
              nprci(k)=0._r8
           end if

! provisional snow mixing ratio and number concentration (qniic and nsic) 
! at boundary are estimated via autoconversion

           if (k.eq.kqi(i) .and. it.eq.1) then
              qniic(i,k)= prci(k)*dz(i,k)*0.25_r8
              nsic(i,k)= nprci(k)*dz(i,k)*0.25_r8
              qni(i,k)= 0.0_r8
              ns(i,k)= 0.0_r8
           end if
! if precip mix ratio is zero so should number concentration
           if (qniic(i,k).lt.qsmall) then
              qniic(i,k)=0._r8
              nsic(i,k)=0._r8
           end if
           if (qric(i,k).lt.qsmall) then
              qric(i,k)=0._r8
              nric(i,k)=0._r8
           end if

! make sure number concentration is a positive number to avoid 
! taking root of negative later
           nric(i,k)=max(nric(i,k),0._r8)
           nsic(i,k)=max(nsic(i,k),0._r8)

!**********************************************************************
! get size distribution parameters for precip
!**********************************************************************
! rain

           if (qric(i,k).ge.qsmall) then
             lamr(k) = (pi*rhow*nric(i,k)/qric(i,k))**(1._r8/3._r8)
             n0r(k) = nric(i,k)*lamr(k)
! check for slope
             lammax = 1._r8/20.e-6_r8
             lammin = 1._r8/500.e-6_r8
! adjust vars
             if (lamr(k).lt.lammin) then
               lamr(k) = lammin
               n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
               nric(i,k) = n0r(k)/lamr(k)
             else if (lamr(k).gt.lammax) then
               lamr(k) = lammax
               n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
               nric(i,k) = n0r(k)/lamr(k)
             end if

! provisional rain number and mass weighted mean fallspeed (m/s)
! Eq.18 of Morrison and Gettelman, 2008, J. Climate
             unr(k) = min(arn(i,k)*mskf_GAMMA(1._r8+br)/lamr(k)**br,10._r8)
             umr(k) = min(arn(i,k)*mskf_GAMMA(4._r8+br)/(6._r8*lamr(k)**br),10._r8)
           else
             lamr(k) = 0._r8
             n0r(k) = 0._r8
             umr(k) = 0._r8
             unr(k) = 0._r8
           end if
!......................................................................
! snow
           if (qniic(i,k).ge.qsmall) then
             lams(k) = (mskf_GAMMA(1._r8+ds)*cs*nsic(i,k)/ &
                       qniic(i,k))**(1._r8/ds)
             n0s(k) = nsic(i,k)*lams(k)

! check for slope
             lammax = 1._r8/10.e-6_r8
             lammin = 1._r8/2000.e-6_r8
! adjust vars
             if (lams(k).lt.lammin) then
               lams(k) = lammin
               n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*mskf_GAMMA(1._r8+ds))
               nsic(i,k) = n0s(k)/lams(k)
             else if (lams(k).gt.lammax) then
               lams(k) = lammax
               n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*mskf_GAMMA(1._r8+ds))
               nsic(i,k) = n0s(k)/lams(k)
             end if

! provisional snow number and mass weighted mean fallspeed (m/s)
             ums(k) = min(asn(i,k)*mskf_GAMMA(4._r8+bs)/(6._r8*lams(k)**bs),3.6_r8)
             uns(k) = min(asn(i,k)*mskf_GAMMA(1._r8+bs)/lams(k)**bs,3.6_r8)
           else
             lams(k) = 0._r8
             n0s(k) = 0._r8
             ums(k) = 0._r8
             uns(k) = 0._r8
           end if

!.......................................................................
! snow self-aggregation from passarelli, 1978, used by Reisner(1998,Eq.A.35)
! this is hard-wired for bs = 0.4 for now
! ignore self-collection of cloud ice

          if (qniic(i,k).ge.qsmall .and. t(i,k).le.273.15_r8) then
              nsagg(k) = -1108._r8*asn(i,k)*Eii* &
                   pi**((1._r8-bs)/3._r8)*rhosn**((-2._r8-bs)/3._r8)*rho(i,k)** &
                   ((2._r8+bs)/3._r8)*qniic(i,k)**((2._r8+bs)/3._r8)* &
                   (nsic(i,k)*rho(i,k))**((4._r8-bs)/3._r8)/ &
                   (4._r8*720._r8*rho(i,k))
           else
              nsagg(k)=0._r8
           end if

!.......................................................................
! accretion of cloud droplets onto snow/graupel
! here use continuous collection equation with
! simple gravitational collection kernel
! ignore collisions between droplets/cloud ice

! ignore collision of snow with droplets above freezing

           if (qniic(i,k).ge.qsmall .and. t(i,k).le.273.15_r8 .and. &
              qcic(i,k).ge.qsmall) then

! put in size dependent collection efficiency
! mean diameter of snow is area-weighted, since
! accretion is function of crystal geometric area
! collection efficiency is from stoke's law (Thompson et al. 2004)

              dc0 = (pgam(k)+1._r8)/lamc(k)
              ds0 = 1._r8/lams(k)
              dum = dc0*dc0*uns(k)*rhow/(9._r8*mua(i,k)*ds0)
              eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))
              eci = max(eci,0._r8)
              eci = min(eci,1._r8)

              psacws(k) = pi/4._r8*asn(i,k)*qcic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*mskf_GAMMA(bs+3._r8)/ &
                  lams(k)**(bs+3._r8)   
              npsacws(k) = pi/4._r8*asn(i,k)*ncic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*mskf_GAMMA(bs+3._r8)/ &
                  lams(k)**(bs+3._r8)
           else
              psacws(k)=0._r8
              npsacws(k)=0._r8
           end if

! secondary ice production due to accretion of droplets by snow 
! (Hallet-Mossop process) (from Cotton et al., 1986)
           if((t(i,k).lt.270.16_r8) .and. (t(i,k).ge.268.16_r8)) then
              ni_secp   = 3.5e8_r8*(270.16_r8-t(i,k))/2.0_r8*psacws(k)
              nsacwi(k) = ni_secp
              msacwi(k) = min(ni_secp*mi0,psacws(k))
           else if((t(i,k).lt.268.16_r8) .and. (t(i,k).ge.265.16_r8)) then
              ni_secp   = 3.5e8_r8*(t(i,k)-265.16_r8)/3.0_r8*psacws(k)
              nsacwi(k) = ni_secp
              msacwi(k) = min(ni_secp*mi0,psacws(k))
           else
              ni_secp   = 0.0_r8
              nsacwi(k) = 0.0_r8
              msacwi(k) = 0.0_r8
           endif
           psacws(k) = max(0.0_r8,psacws(k)-ni_secp*mi0)

!.......................................................................
! accretion of rain water by snow
! formula from ikawa and saito, 1991, used by reisner et al., 1998

           if (qric(i,k).ge.1.e-8_r8 .and. qniic(i,k).ge.1.e-8_r8 .and. &
              t(i,k).le.273.15_r8) then

              pracs(k) = pi*pi*ecr*(((1.2_r8*umr(k)-0.95_r8*ums(k))**2+ &
                  0.08_r8*ums(k)*umr(k))**0.5_r8*rhow*rho(i,k)* &
                  n0r(k)*n0s(k)* &
                  (5._r8/(lamr(k)**6*lams(k))+ &
                  2._r8/(lamr(k)**5*lams(k)**2)+ &
                  0.5_r8/(lamr(k)**4*lams(k)**3)))

              npracs(k) = pi/2._r8*rho(i,k)*ecr*(1.7_r8*(unr(k)-uns(k))**2+ &
                  0.3_r8*unr(k)*uns(k))**0.5_r8*n0r(k)*n0s(k)* &
                  (1._r8/(lamr(k)**3*lams(k))+ &
                  1._r8/(lamr(k)**2*lams(k)**2)+ &
                  1._r8/(lamr(k)*lams(k)**3))
           else
              pracs(k)=0._r8
              npracs(k)=0._r8
           end if

!.......................................................................
! heterogeneous freezing of rain drops
! follows from Bigg (1953)

           if (t(i,k).lt.269.15_r8 .and. qric(i,k).ge.qsmall) then

              mnuccr(k) = 20._r8*pi*pi*rhow*nric(i,k)*bimm* &
                  exp(aimm*(273.15_r8-t(i,k)))/lamr(k)**3 &
                  /lamr(k)**3

              nnuccr(k) = pi*nric(i,k)*bimm* &
                   exp(aimm*(273.15_r8-t(i,k)))/lamr(k)**3
           else
              mnuccr(k)=0._r8
              nnuccr(k)=0._r8
           end if

!.......................................................................
! accretion of cloud liquid water by rain
! formula from Khrouditnov and Kogan (2000)
! gravitational collection kernel, droplet fall speed neglected

           if (qric(i,k).ge.qsmall .and. qcic(i,k).ge.qsmall) then
              pra(k) = 67._r8*(qcic(i,k)*qric(i,k))**1.15_r8
              npra(k) = pra(k)/(qcic(i,k)/ncic(i,k))
           else
              pra(k)=0._r8
              npra(k)=0._r8
           end if

!.......................................................................
! Self-collection of rain drops
! from Beheng(1994)

           if (qric(i,k).ge.qsmall) then
              nragg(k) = -8._r8*nric(i,k)*qric(i,k)*rho(i,k)
           else
              nragg(k)=0._r8
           end if

!.......................................................................
! Accretion of cloud ice by snow
! For this calculation, it is assumed that the Vs >> Vi
! and Ds >> Di for continuous collection

           if (qniic(i,k).ge.qsmall.and.qiic(i,k).ge.qsmall &
              .and.t(i,k).le.273.15_r8) then
              prai(k) = pi/4._r8*asn(i,k)*qiic(i,k)*rho(i,k)* &
                   n0s(k)*Eii*mskf_GAMMA(bs+3._r8)/ &
                   lams(k)**(bs+3._r8)  
              nprai(k) = pi/4._r8*asn(i,k)*niic(i,k)* &
                   rho(i,k)*n0s(k)*Eii*mskf_GAMMA(bs+3._r8)/ &
                   lams(k)**(bs+3._r8)
           else
              prai(k)=0._r8
              nprai(k)=0._r8
           end if

!.......................................................................
! fallout term
        prf(k)  = -umr(k)*qric(i,k)/dz(i,k)
        pnrf(k) = -unr(k)*nric(i,k)/dz(i,k)
        psf(k)  = -ums(k)*qniic(i,k)/dz(i,k)
        pnsf(k) = -uns(k)*nsic(i,k)/dz(i,k)

!........................................................................
! calculate vertical velocity in cumulus updraft

     if (k.eq.jb(i)) then
       zkine(i,jb(i)) = 0.5_r8
       wu   (i,jb(i)) = 1._r8
       zbuo (i,jb(i)) = (tu(i,jb(i))*(1._r8+retv*qu(i,jb(i)))-    &
                     th(i,jb(i))*(1._r8+retv*qh(i,jb(i))))/   &
                     (th(i,jb(i))*(1._r8+retv*qh(i,jb(i))))
     else
       if (.true.) then
!          print *,'before ecmwf qcs=',qc(i,k),qi(i,k),qr(i,k),k
! ECMWF formula
  !          print *,'using ecmwrf CKE, retv=',retv
           zbc = tu(i,k)*(1._r8+retv*qu(i,k)-qr(i,k)-qni(i,k)-qi(i,k)-qc(i,k))
           zbe = th(i,k)*(1._r8+retv*qh(i,k))
           zbuo(i,k) = (zbc-zbe)/zbe
           zbuoc= (zbuo(i,k)+zbuo(i,k+1))*0.5_r8
           zdkbuo = dz(i,k+1)*grav*zfacbuo*zbuoc
           zdken = min(.99_r8,(1._r8+cwdrag)*max(du(i,k),eu(i,k))*dz(i,k+1)/ &
                      max(1.e-10_r8,mu(i,k+1)))
           zkine(i,k) = (zkine(i,k+1)*(1._r8-zdken)+zdkbuo)/      &
                      (1._r8+zdken)
!        print *,'zkine=',(zkine(i,k)),dz(i,k),k
        else
! Gregory formula
           write(*,*) "Gregory vertical velocity"
           zbc = tu(i,k)*(1._r8+retv*qu(i,k))
           zbe = th(i,k)*(1._r8+retv*qh(i,k))
           zbuo(i,k) = (zbc-zbe)/zbe-qr(i,k)-qni(i,k)-qi(i,k)-qc(i,k)
           zbuoc= (zbuo(i,k)+zbuo(i,k+1))*0.5_r8
           zdkbuo = dz(i,k+1)*grav*zbuoc*(1.0-0.25)/6.
           zdken = du(i,k)*dz(i,k+1)/max(1.e-10_r8,mu(i,k+1))
           zkine(i,k) = (zkine(i,k+1)*(1._r8-zdken)+zdkbuo)/      &
                      (1._r8+zdken)
         end if
              wu(i,k) = min(15._r8,sqrt(2._r8*max(0.1_r8,zkine(i,k) )))
!dkay         wu(i,k) = wu_kf_act(k)
       end if
!             print *,'wu from cke= & kf',wu(i,k),wu_kf_act(k),k
!ckay
       arcf(i,k)= mu(i,k)/wu(i,k)

!............................................................................
! droplet activation
! calculate potential for droplet activation if cloud water is present
! formulation from Abdul-Razzak and Ghan (2000) and Abdul-Razzak et al. (1998),
! AR98

       naer2h(i,k,:) = 0.5_r8*(naer2(i,k,:) + naer2(i,k+1,:))
       ntaerh(i,k)   = 0.5_r8*(ntaer(i,k) + ntaer(i,k+1))

!      write(*,*)'naer2h(i,k,:)',naer2h(i,k,:)

       if (qcic(i,k).ge.qsmall.or.cmel(i,k+1).ge.qsmall ) then

!dkay
! added qsatzm
!         print *, 'before activate'
         call mskf_activate(wu(i,k),t(i,k),rho(i,k), &
                 naer2h(i,k,:), naer_cu,naer_cu, maerosol,  &
                 dispersion_aer,hygro_aer, density_aer, dum2,qsatzm(i,k))
!             print *,'ccn, massmixing ratio of aerosols='
!            print *, dum2, maerosol
         dum2l(i,k) = dum2
       else
         dum2l(i,k) = 0._r8
       end if

! get droplet activation rate
       if (qcic(i,k).ge.qsmall .and. t(i,k).gt.238.15_r8 .and. k.gt.jt(i)+2) then

! assume aerosols already activated are equal number of existing droplets for
! simplicity
         if (k.eq.kqc(i))  then
              npccn(k) = dum2l(i,k)/deltat
         else
              npccn(k) = (dum2l(i,k)-ncic(i,k))/deltat
         end if
! make sure number activated > 0
         npccn(k) = max(0._r8,npccn(k))
         ncmax = dum2l(i,k)
       else
         npccn(k)=0._r8
         ncmax = 0._r8
       end if

!..............................................................................
!ice nucleation

       esi(i,k)= mskf_polysvp(t(i,k),1)      ! over ice          
       es(i,k) = mskf_polysvp(t(i,k),0)
       qs(i,k) = 0.622_r8*es(i,k)/(ph(i,k) - (1.0_r8-0.622_r8)*es(i,k))
       qs(i,k) = min(1.0_r8,qs(i,k))
       if (qs(i,k) < 0.0_r8)  qs(i,k) = 1.0_r8

       relhum(i,k)= 1.0_r8

       if (t(i,k).lt.tmelt ) then
         if (.true.) then

! Liu et al.,J. climate, 2007
!         print *, 'before ice nuke'

            call mskf_nucleati(wu(i,k),t(i,k),p(i,k),q(i,k),qcic(i,k),rho(i,k),  & ! TWG add p and replace relhum with q
                         naer2h(i,k,:),naer_cu,dum2i(i,k) &
                        , nihf(i,k),     &
                        niimm(i,k),nidep(i,k),nimey(i,k))

             nihf(i,k)=nihf(i,k)*rho(i,k)           !  convert from #/kg -> #/m3)
             niimm(i,k)=niimm(i,k)*rho(i,k)
             nidep(i,k)=nidep(i,k)*rho(i,k)
             nimey(i,k)=nimey(i,k)*rho(i,k)
          else

! cooper curve (factor of 1000 is to convert from L-1 to m-3)
            dum2i(i,k)=0.005_r8*exp(0.304_r8*(273.15_r8-t(i,k)))*1000._r8
! put limit on number of nucleated crystals, set to number at T=-30 C
! cooper (limit to value at -35 C)
            dum2i(i,k)=min(dum2i(i,k),208.9e3_r8)/rho(i,k) ! convert from m-3 to kg-1
          endif
        else
          dum2i(i,k)=0._r8
        end if
!ckay
!       print *,'nucleated ccn=',dum2i(i,k),k

! ice nucleation if activated nuclei exist at t<0C 

        if (dum2i(i,k).gt.0._r8.and.t(i,k).lt.tmelt.and. &
           relhum(i,k)*es(i,k)/esi(i,k).gt. 1.05_r8  .and. k.gt.jt(i)+1) then

           if (k.eq.kqi(i)) then
                nnuccd(k)=dum2i(i,k)/deltat
           else
                nnuccd(k)=(dum2i(i,k)-niic(i,k))/deltat
           end if
           nnuccd(k)=max(nnuccd(k),0._r8)
           nimax = dum2i(i,k)

!Calc mass of new particles using new crystal mass...
!also this will be multiplied by mtime as nnuccd is...
           mnuccd(k) = nnuccd(k) * mi0
         else
           nnuccd(k)=0._r8
           nimax = 0._r8
           mnuccd(k) = 0._r8
         end if
!................................................................................
! Bergeron process
! If 0C< T <-40C and both ice and liquid exist

         if (t(i,k).le.273.15_r8 .and. t(i,k).gt.233.15_r8 .and.  &
              qiic(i,k).gt.0.5e-6_r8 .and. qcic(i,k).gt. qsmall)  then
              plevap = qcic(i,k)/bergtsf
              prb(k) = max(0._r8,plevap)
              nprb(k) = prb(k)/(qcic(i,k)/ncic(i,k))
         else
              prb(k)=0._r8
              nprb(k)=0._r8
         end if

!................................................................................
! heterogeneous freezing of cloud water (-5C < T < -35C)

        if (qcic(i,k).ge.qsmall .and.ncic(i,k).gt.0._r8 .and. ntaerh(i,k).gt.0._r8 .and.  &
              t(i,k).le.268.15_r8 .and. t(i,k).gt.238.15_r8 ) then

          if (.false.)  then
! immersion freezing (Diehl and Wurzler, 2004)
              ttend(k) = -grav*wu(i,k)/cp/(1.0_r8+gamhat(i,k))
              naimm = (0.00291_r8*naer2h(i,k,idxbcphi)+32.3_r8*(naer2h(i,k,idxdst1)  &
                      +naer2h(i,k,idxdst2)+naer2h(i,k,idxdst3)+              &
                       naer2h(i,k,idxdst4)))/ntaerh(i,k)             !m-3
              if (ttend(k) .lt. 0._r8) then
                 nnuccc(k) = -naimm*exp(273.15_r8-t(i,k))*ttend(k)*qcic(i,k)/rhow   ! kg-1s-1                        
                 mnuccc(k) = nnuccc(k)*qcic(i,k)/ncic(i,k)
              end if
          else


! immersion freezing (Bigg, 1953)
              mnuccc(k) = pi*pi/36._r8*rhow* &
                    cdist1(k)*mskf_GAMMA(7._r8+pgam(k))* &
                    bimm*exp(aimm*(273.15_r8-t(i,k)))/ &
                    lamc(k)**3/lamc(k)**3

              nnuccc(k) = pi/6._r8*cdist1(k)*mskf_GAMMA(pgam(k)+4._r8) &
                    *bimm*exp(aimm*(273.15_r8-t(i,k)))/lamc(k)**3
           end if

! contact freezing (Young, 1974) with hooks into simulated dust

           tcnt=(270.16_r8-t(i,k))**1.3_r8
           viscosity=1.8e-5_r8*(t(i,k)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s)          
           mfp=2.0_r8*viscosity/(ph(i,k)  &                  ! Mean free path (m)
               *sqrt(8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i,k))))

           slip1=1.0_r8+(mfp/rn_dst1)*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rn_dst1/mfp))))! Slip correction factor
           slip2=1.0_r8+(mfp/rn_dst2)*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rn_dst2/mfp))))
           slip3=1.0_r8+(mfp/rn_dst3)*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rn_dst3/mfp))))
           slip4=1.0_r8+(mfp/rn_dst4)*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rn_dst4/mfp))))

           dfaer1=1.381e-23_r8*t(i,k)*slip1/(6._r8*pi*viscosity*rn_dst1)  !aerosol diffusivity (m2/s)
           dfaer2=1.381e-23_r8*t(i,k)*slip2/(6._r8*pi*viscosity*rn_dst2)
           dfaer3=1.381e-23_r8*t(i,k)*slip3/(6._r8*pi*viscosity*rn_dst3)
           dfaer4=1.381e-23_r8*t(i,k)*slip4/(6._r8*pi*viscosity*rn_dst4)

           nacon1=0.0_r8
           nacon2=0.0_r8
           nacon3=0.0_r8
           nacon4=0.0_r8


           if (idxdst1.gt.0) then
              nacon1=naer2(i,k,idxdst1)*tcnt *0.0_r8
           endif
           if (idxdst2.gt.0) then
              nacon2=naer2(i,k,idxdst2)*tcnt ! 1/m3
           endif
           if (idxdst3.gt.0) then
              nacon3=naer2(i,k,idxdst3)*tcnt
           endif
           if (idxdst4.gt.0) then
              nacon4=naer2(i,k,idxdst4)*tcnt
           endif

           mnucct(k) = (dfaer1*nacon1+dfaer2*nacon2+dfaer3*nacon3+dfaer4*nacon4)*pi*pi/3._r8*rhow* &
                       cdist1(k)*mskf_GAMMA(pgam(k)+5._r8)/lamc(k)**4

           nnucct(k) = (dfaer1*nacon1+dfaer2*nacon2+dfaer3*nacon3+dfaer4*nacon4)*2._r8*pi*  &
                       cdist1(k)*mskf_GAMMA(pgam(k)+2._r8)/lamc(k)

!              if (nnuccc(k).gt.nnuccd(k)) then
!                 dum=nnuccd(k)/nnuccc(k)
! scale mixing ratio of droplet freezing with limit
!                 mnuccc(k)=mnuccc(k)*dum
!                 nnuccc(k)=nnuccd(k)
!              end if

           else
             mnuccc(k) = 0._r8
             nnuccc(k) = 0._r8
             mnucct(k) = 0._r8
             nnucct(k) = 0._r8
           end if

!****************************************************************************************
! conservation to ensure no negative values of cloud water/precipitation
! in case microphysical process rates are large
! note: for check on conservation, processes are multiplied by omsm
! to prevent problems due to round off error

! since activation/nucleation processes are fast, need to take into account
! factor mtime = mixing timescale in cloud / model time step
! for now mixing timescale is assumed to be 15 min
!*****************************************************************************************

       mtime=deltat/900._r8
       mtimec=deltat/900._r8

       mtime = AMAX1(1.0,mtime)   !TWG remove time scale limitation from CAM5
       mtimec = AMAX1(1.0,mtimec)    

! conservation of qc

        qce = mu(i,k)*qc(i,k)+dz(i,k)*(cmel(i,k-1)-du(i,k-1)*qc(i,k))
        dum = arcf(i,k)*(pra(k)+prc(k)+prb(k)+mnuccc(k)+mnucct(k)+msacwi(k)+   &
                         psacws(k)  )*dz(i,k)
        if( qce.lt.0._r8)  then
          prc(k) = 0._r8
          pra(k) = 0._r8
          prb(k) = 0._r8
          mnuccc(k) = 0._r8
          mnucct(k) = 0._r8
          msacwi(k) = 0._r8
          psacws(k) = 0._r8
        else  if (dum.gt.qce) then
          ratio = qce/dum*omsm
          prc(k) = prc(k)*ratio
          pra(k) = pra(k)*ratio
          prb(k) = prb(k)*ratio
          mnuccc(k) = mnuccc(k)*ratio
          mnucct(k) = mnucct(k)*ratio
          msacwi(k) = msacwi(k)*ratio
          psacws(k) = psacws(k)*ratio
        end if

! conservation of nc
        nce = mu(i,k)*nc(i,k)+(arcf(i,k)*npccn(k)*mtimec-du(i,k-1)*nc(i,k))*dz(i,k)
        dum = arcf(i,k)*dz(i,k)*(nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+ &
              npsacws(k)+ nprb(k) )
        if (nce.lt.0._r8) then
          nprc1(k) = 0._r8
!          nprc(k) = 0._r8
          npra(k) = 0._r8
          nnuccc(k) = 0._r8
          nnucct(k) = 0._r8
          npsacws(k) = 0._r8
          nprb(k) = 0._r8
        else if (dum.gt.nce) then
          ratio = nce/dum*omsm
          nprc1(k) = nprc1(k)*ratio
          npra(k) = npra(k)*ratio
          nnuccc(k) = nnuccc(k)*ratio
          nnucct(k) = nnucct(k)*ratio
          npsacws(k) = npsacws(k)*ratio
          nprb(k) = nprb(k)*ratio
        end if

! conservation of qi
        qie = mu(i,k)*qi(i,k)+dz(i,k)*(cmei(i,k-1)-du(i,k-1)*qi(i,k)+  &
                   ( mnuccc(k)+mnucct(k)+msacwi(k)+prb(k))*arcf(i,k) )
        dum = arcf(i,k)*(prci(k)+ prai(k))*dz(i,k)
        if (qie.lt.0._r8) then
          prci(k) = 0._r8
          prai(k) = 0._r8
        else if (dum.gt.qie) then
          ratio = qie/dum*omsm
          prci(k) = prci(k)*ratio
          prai(k) = prai(k)*ratio
        end if

! conservation of ni
         nie = mu(i,k)*ni(i,k)+dz(i,k)*(nnuccd(k)*mtime*arcf(i,k)-du(i,k-1)*ni(i,k)  &
                       + nnucct(k)*arcf(i,k) )
         dum = arcf(i,k)*dz(i,k)*(-nsacwi(k)+nprci(k)+ &
               nprai(k))
         if( nie.lt.0._r8) then
           nsacwi(k)= 0._r8
           nprci(k) = 0._r8
           nprai(k) = 0._r8
         else  if (dum.gt.nie) then
           ratio = nie/dum*omsm
           nsacwi(k)= nsacwi(k)*ratio
           nprci(k) = nprci(k)*ratio
           nprai(k) = nprai(k)*ratio
         end if

! conservation of qr

        qre = mu(i,k)*qr(i,k)+dz(i,k)*(pra(k)+prc(k))*arcf(i,k)
        dum = arcf(i,k)*dz(i,k)*(pracs(k)+ mnuccr(k)-prf(k))
        if (qre.lt.0._r8) then
           prf(k) = 0._r8
           pracs(k) = 0._r8
           mnuccr(k) = 0._r8
        else if (dum.gt.qre) then
           ratio = qre/dum*omsm
           prf(k) = prf(k)*ratio
           pracs(k) = pracs(k)*ratio
           mnuccr(k) = mnuccr(k)*ratio
        end if

! conservation of nr
         nre = mu(i,k)*nr(i,k)
         dum = arcf(i,k)*dz(i,k)*(-nprc(k)+npracs(k)+nnuccr(k) &
                   -nragg(k)-pnrf(k))
         if(nre.lt.0._r8) then
           nprc(k) = 0._r8
           npracs(k)= 0._r8
           nnuccr(k)= 0._r8
           nragg(k) = 0._r8
           pnrf(k) = 0._r8
         else if (dum.gt.nre) then
           ratio = nre/dum*omsm
           nprc(k) = nprc(k)*ratio
           npracs(k)= npracs(k)*ratio
           nnuccr(k)= nnuccr(k)*ratio
           nragg(k) = nragg(k)*ratio
           pnrf(k) = pnrf(k)*ratio
         end if

! conservation of qni

        qnie = mu(i,k)*qni(i,k)+dz(i,k)*( (prai(k)+psacws(k)+prci(k)+     &
                   pracs(k)+mnuccr(k))*arcf(i,k) )
        dum = arcf(i,k)*dz(i,k)*(-psf(k))

        if(qnie.lt.0._r8) then
           psf(k) = 0._r8
        else if (dum.gt.qnie) then
           ratio = qnie/dum*omsm
           psf(k) = psf(k)*ratio
        end if

! conservation of ns
        nse = mu(i,k)*ns(i,k)+dz(i,k)*(nprci(k)+nnuccr(k))*arcf(i,k)
        dum = arcf(i,k)*dz(i,k)*(-nsagg(k)-pnsf(k))
        if (nse.lt.0._r8) then
           nsagg(k) = 0._r8
           pnsf(k) = 0._r8
        else if (dum.gt.nse) then
           ratio = nse/dum*omsm
           nsagg(k) = nsagg(k)*ratio
           pnsf(k) = pnsf(k)*ratio
        end if

!*****************************************************************************
! get tendencies due to microphysical conversion processes
!*****************************************************************************

      if (k.le.kqc(i))   then
        qctend(i,k) = qctend(i,k)+  &
                 (-pra(k)-prc(k)-prb(k)-mnuccc(k)-mnucct(k)-msacwi(k)- &
                  psacws(k))

!             print *,'qctend components=',qctend(i,k),pra(k),prc(k), &
!             mnuccc(k)-mnucct(k)-msacwi(k),psacws(k)

        qitend(i,k) = qitend(i,k)+  &
                  (prb(k)+mnuccc(k)+mnucct(k)+msacwi(k)-prci(k)- &
                  prai(k)+mnuccd(k)*mtimec) !TWG ice nucleation change

        qrtend(i,k) = qrtend(i,k)+ &
                 (pra(k)+prc(k))+(-pracs(k)- &
                  mnuccr(k))


        qnitend(i,k) = qnitend(i,k)+ &
                (prai(k)+psacws(k)+prci(k))+( &
                   pracs(k)+mnuccr(k))

! multiply activation/nucleation by mtime to account for fast timescale

        nctend(i,k) = nctend(i,k)+ npccn(k)*mtimec+&
                  (-nnuccc(k)-nnucct(k)-npsacws(k) &
                  -npra(k)-nprc1(k)-nprb(k))

        nitend(i,k) = nitend(i,k)+ nnuccd(k)*mtime+&
                  (nnuccc(k)+ nnucct(k)+nsacwi(k)-nprci(k)- &
                  nprai(k))

        nstend(i,k) = nstend(i,k)+( &
                  nsagg(k)+nnuccr(k))+nprci(k)

        nrtend(i,k) = nrtend(i,k)+ &
                  nprc(k)+(-npracs(k)-nnuccr(k) +nragg(k))

! for output
! cloud liquid water-------------
        autolm(i,k) = -prc(k)*arcf(i,k)
        accrlm(i,k) = -pra(k)*arcf(i,k)
        bergnm(i,k) = -prb(k)*arcf(i,k)
        fhtimm(i,k) = -mnuccc(k)*arcf(i,k)
        fhtctm(i,k) = -mnucct(k)*arcf(i,k)
        hmpim (i,k) = -msacwi(k)*arcf(i,k)
        accslm(i,k) = -psacws(k)*arcf(i,k)
        collrm(i,k) = -pracs(k)*arcf(i,k)
        collrn(i,k) = -npracs(k)*arcf(i,k)
        fhtcrm(i,k) = -mnuccr(k)*arcf(i,k)
        fhtcrn(i,k) = -nnuccr(k)*arcf(i,k)
        dlfm  (i,k) = -du(i,k)*qc(i,k)

        autoln(i,k) = -nprc1(k)*arcf(i,k)*rho(i,k)
        autorn(i,k) = -nprc(k)*arcf(i,k)*rho(i,k)
        aggrn(i,k) =  nragg(k)*arcf(i,k)*rho(i,k)
        aggsn(i,k) =  nsagg(k)*arcf(i,k)*rho(i,k)
        accrln(i,k) = -npra(k)*arcf(i,k)*rho(i,k)
        bergnn(i,k) = -nprb(k)*arcf(i,k)*rho(i,k)
        fhtimn(i,k) = -nnuccc(k)*arcf(i,k)*rho(i,k)
        fhtctn(i,k) = -nnucct(k)*arcf(i,k)*rho(i,k)
        accsln(i,k) = -npsacws(k)*arcf(i,k)*rho(i,k)
        activn(i,k) = npccn(k)*mtimec*arcf(i,k)*rho(i,k)
        dlfn  (i,k) = -du(i,k)*nc(i,k)*rho(i,k)
!cloud ice------------------------        
        autoim(i,k) = -prci(k)*arcf(i,k)
        accsim(i,k) = -prai(k)*arcf(i,k)
        difm  (i,k) = -du(i,k)*qi(i,k)             !TWG 2017 change -du(i,k+1)*qi(i,k)

        nuclin(i,k) = nnuccd(k)*mtime*arcf(i,k)*rho(i,k)
        nuclim(i,k) = mnuccd(k)*mtime*arcf(i,k)*rho(i,k)
        autoin(i,k) = -nprci(k)*arcf(i,k)*rho(i,k)
        accsin(i,k) = -nprai(k)*arcf(i,k)*rho(i,k)
        hmpin (i,k)  = nsacwi(k)*arcf(i,k)*rho(i,k)
        difn  (i,k) = -du(i,k)*ni(i,k)*rho(i,k)
      else
        qctend(i,k) = 0._r8
        qitend(i,k) = 0._r8
        qrtend(i,k) = 0._r8
        qnitend(i,k) = 0._r8
        nctend(i,k) = 0._r8
        nitend(i,k) = 0._r8
        nstend(i,k) = 0._r8
        nrtend(i,k) = 0._r8
      end if

!********************************************************************************
!  vertical integration
!********************************************************************************
! snow
        if ( k.le.kqi(i) ) then
          qni(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*qni(i,k)+dz(i,k)*(qnitend(i,k)+psf(k))*arcf(i,k) )

          ns(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*ns(i,k)+dz(i,k)*(nstend(i,k)+pnsf(k))*arcf(i,k) )

         else
           qni(i,k-1)=0._r8
           ns(i,k-1)=0._r8
         end if

         if (qni(i,k-1).le.0._r8) then
          qni(i,k-1)=0._r8
          ns(i,k-1)=0._r8
         end if

! rain
         if (k.le.kqc(i) ) then
          qr(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*qr(i,k)+dz(i,k)*(qrtend(i,k)+prf(k))*arcf(i,k) )

          nr(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*nr(i,k)+dz(i,k)*(nrtend(i,k)+pnrf(k))*arcf(i,k) )

        else
          qr(i,k-1)=0._r8
          nr(i,k-1)=0._r8
        end if

        if( qr(i,k-1) .le. 0._r8) then
          qr(i,k-1)=0._r8
          nr(i,k-1)=0._r8
        end if

! freeze rain homogeneously at -40 C

         if (t(i,k-1) < 233.15_r8 .and. qr(i,k-1) > 0._r8) then

! make sure freezing rain doesn't increase temperature above threshold
          dum = xlf/cp*qr(i,k-1)
          if (t(i,k-1)+dum.gt.233.15_r8) then
              dum = -(t(i,k-1)-233.15_r8)*cp/xlf
!bugfix 2012-01-06              dum = dum/(xlf/cp*qr(i,k-1))
              dum = dum/qr(i,k-1)
              dum = max(0._r8,dum)
              dum = min(1._r8,dum)
          else
              dum = 1._r8
          end if
          qni(i,k-1)=qni(i,k-1)+dum*qr(i,k-1)
          ns(i,k-1)=ns(i,k-1)+dum*nr(i,k-1)
          qr(i,k-1)=(1._r8-dum)*qr(i,k-1)
          nr(i,k-1)=(1._r8-dum)*nr(i,k-1)
          fhmrm(i,k-1) = -mu(i,k-1)*dum*qr(i,k-1)/dz(i,k)
        end if

!        if( qr(i,k-1) .le. 0._r8) then
!          qr(i,k-1)=0._r8
!          nr(i,k-1)=0._r8
!        end if  

! cloud water
         if ( k.le.kqc(i) ) then
          qc(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*qc(i,k)-dz(i,k)*du(i,k-1)*qc(i,k)             &
                    +dz(i,k)*qctend(i,k)*arcf(i,k)+dz(i,k)*cmel(i,k-1) )

          nc(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*nc(i,k)-dz(i,k)*du(i,k-1)*nc(i,k)             &
                    +dz(i,k)*nctend(i,k)*arcf(i,k) )

        else
          qc(i,k-1)=0._r8
          nc(i,k-1)=0._r8
        end if

        qcorg = qc(i,k-1)
        ncorg = nc(i,k-1)
        if (qc(i,k-1).le. 0._r8) then
          qc(i,k-1)=0._r8
          nc(i,k-1)=0._r8
        end if
        qcadj(i,k-1)= (qc(i,k-1)- qcorg)*mu(i,k-1)/dz(i,k)*rho(i,k)
        ncadj(i,k-1)= (nc(i,k-1)- ncorg)*mu(i,k-1)/dz(i,k)*rho(i,k)

! cloud ice
         if( k.le.kqi(i)) then
           qi(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*qi(i,k)-dz(i,k)*du(i,k-1)*qi(i,k)             &
                    +dz(i,k)*qitend(i,k)*arcf(i,k)+dz(i,k)*cmei(i,k-1) )

           ni(i,k-1) = 1._r8/mu(i,k-1)*                                    &
                   (mu(i,k)*ni(i,k)-dz(i,k)*du(i,k-1)*ni(i,k)             &
                    +dz(i,k)*nitend(i,k)*arcf(i,k) )

         else
          qi(i,k-1)=0._r8
          ni(i,k-1)=0._r8
         end if

        qiorg = qi(i,k-1)
        niorg = ni(i,k-1)
        if (qi(i,k-1).le. 0._r8) then
          qi(i,k-1)=0._r8
          ni(i,k-1)=0._r8
        end if
        qiadj(i,k-1)= (qi(i,k-1)- qiorg)*mu(i,k-1)/dz(i,k)*rho(i,k)
        niadj(i,k-1)= (ni(i,k-1)- niorg)*mu(i,k-1)/dz(i,k)*rho(i,k)

!        trspcm(i,k-1) = (mu(i,k)*qc(i,k) - mu(i,k-1)*qc(i,k-1))/dz(i,k)
!        trspcn(i,k-1) = (mu(i,k)*nc(i,k) -
!        mu(i,k-1)*nc(i,k-1))/dz(i,k)*rho(i,k)
!        trspim(i,k-1) = (mu(i,k)*qi(i,k) - mu(i,k-1)*qi(i,k-1))/dz(i,k)
!        trspin(i,k-1) = (mu(i,k)*ni(i,k) -
!        mu(i,k-1)*ni(i,k-1))/dz(i,k)*rho(i,k)


! freeze rain homogeneously at -38 C
         if (t(i,k-1) < 233.15_r8 .and. qc(i,k-1) > 0._r8) then
! make sure freezing rain doesn't increase temperature above threshold
          dum = xlf/cp*qc(i,k-1)
          if (t(i,k-1)+dum.gt.233.15_r8) then
              dum = -(t(i,k-1)-233.15_r8)*cp/xlf
!bugfix 2012-01-06      dum = dum/(xlf/cp*qc(i,k-1))
              dum = dum/qc(i,k-1)
              dum = max(0._r8,dum)
              dum = min(1._r8,dum)
          else
              dum = 1._r8
          end if
          qi(i,k-1)=qi(i,k-1)+dum*qc(i,k-1)
          ni(i,k-1)=ni(i,k-1)+dum*nc(i,k-1)
          fhmlm(i,k-1) = -mu(i,k-1)*dum*qc(i,k-1)/dz(i,k)
          fhmln(i,k-1) = -mu(i,k-1)*dum*nc(i,k-1)/dz(i,k)*rho(i,k)
          qc(i,k-1)=(1._r8-dum)*qc(i,k-1)
          nc(i,k-1)=(1._r8-dum)*nc(i,k-1)
        end if

        frz(i,k-1) = cmei(i,k-1) + arcf(i,k)*(prb(k)+mnuccc(k)+mnucct(k)+msacwi(k)+   &
                     pracs(k)+mnuccr(k)+psacws(k) )-fhmlm(i,k-1)-fhmrm(i,k-1)

!******************************************************************************
! get size distribution parameters based on in-cloud cloud water/ice
! these calculations also ensure consistency between number and mixing ratio

! following equation(2,3,4) of Morrison and Gettelman, 2008, J. Climate.
! Gamma(n)= (n-1)! 
! lamc <-> lambda for cloud liquid water
! pgam <-> meu    for cloud liquid water
! meu=0 for ice,rain and snow         
!*******************************************************************************
!songxl 2011-12-31

           niorg = ni(i,k-1)

! cloud ice
           if (qi(i,k-1).ge.qsmall) then
! add upper limit to in-cloud number concentration to prevent numerical error
              ni(i,k-1)=min(ni(i,k-1),qi(i,k-1)*1.e20_r8)
              lami(k-1) = (mskf_gamma(1._r8+di)*ci* &
                  ni(i,k-1)/qi(i,k-1))**(1._r8/di)
              n0i(k-1) = ni(i,k-1)*lami(k-1)
! check for slope
              lammax = 1._r8/10.e-6_r8
              lammin = 1._r8/(2._r8*dcs)
! adjust vars
              if (lami(k-1).lt.lammin) then
                lami(k-1) = lammin
                n0i(k-1) = lami(k-1)**(di+1._r8)*qi(i,k-1)/(ci*mskf_gamma(1._r8+di))
                ni(i,k-1) = n0i(k-1)/lami(k-1)
              else if (lami(k-1).gt.lammax) then
                lami(k-1) = lammax
                n0i(k-1) = lami(k-1)**(di+1._r8)*qi(i,k-1)/(ci*mskf_gamma(1._r8+di))
                ni(i,k-1) = n0i(k-1)/lami(k-1)
              end if
              effi(i,k-1) = 1.5_r8/lami(k-1)*1.e6_r8
           else
              lami(k-1) = 0._r8
              n0i(k-1) = 0._r8
              effi(i,k-1) = 0._r8
           end if

!songxl 2011-12-31-----
           niadj(i,k-1)= niadj(i,k-1)+(ni(i,k-1)-niorg)*mu(i,k-1)/dz(i,k)*rho(i,k)
!................................................................................
!songxl 2011-12-31
              ncorg = nc(i,k-1)

!cloud water
           if (qc(i,k-1).ge.qsmall) then

! add upper limit to in-cloud number concentration to prevent numerical error
              nc(i,k-1)=min(nc(i,k-1),qc(i,k-1)*1.e20_r8)

! get pgam from fit to observations of martin et al. 1994

              pgam(k-1)=0.0005714_r8*(nc(i,k-1)/1.e6_r8*rho(i,k-1))+0.2714_r8 !TWG 2017 change / to * in front of rho 
              pgam(k-1)=1._r8/(pgam(k-1)**2)-1._r8
              pgam(k-1)=max(pgam(k-1),2._r8)
              pgam(k-1)=min(pgam(k-1),15._r8)
! calculate lamc

              lamc(k-1) = (pi/6._r8*rhow*nc(i,k-1)*mskf_gamma(pgam(k-1)+4._r8)/ &
                 (qc(i,k-1)*mskf_gamma(pgam(k-1)+1._r8)))**(1._r8/3._r8)

! lammin, 50 micron diameter max mean size
              lammin = (pgam(k)+1._r8)/50.e-6_r8
              lammax = (pgam(k-1)+1._r8)/2.e-6_r8

              if (lamc(k-1).lt.lammin) then
                 lamc(k-1) = lammin
                 nc(i,k-1) = 6._r8*lamc(k-1)**3*qc(i,k-1)* &
                      mskf_gamma(pgam(k-1)+1._r8)/ &
                      (pi*rhow*mskf_gamma(pgam(k-1)+4._r8))
              else if (lamc(k-1).gt.lammax) then
                 lamc(k-1) = lammax
                 nc(i,k-1) = 6._r8*lamc(k-1)**3*qc(i,k-1)* &
                       mskf_gamma(pgam(k-1)+1._r8)/ &
                      (pi*rhow*mskf_gamma(pgam(k-1)+4._r8))
              end if
              effc(i,k-1) = mskf_gamma(pgam(k-1)+4._r8)/ &
                            mskf_gamma(pgam(k-1)+3._r8)/lamc(k-1)/2._r8*1.e6_r8
! parameter to calculate droplet freezing

              cdist1(k-1) = nc(i,k-1)/mskf_gamma(pgam(k-1)+1._r8)
           else
              lamc(k-1) = 0._r8
              cdist1(k-1) = 0._r8
              effc(i,k-1) = 0._r8
           end if

!songxl 2011-12-31-----
           ncadj(i,k-1) = ncadj(i,k-1)+ (nc(i,k-1)-ncorg)*mu(i,k-1)/dz(i,k)*rho(i,k)

           trspcm(i,k-1) = (mu(i,k)*qc(i,k) - mu(i,k-1)*qc(i,k-1))/dz(i,k)
           trspcn(i,k-1) = (mu(i,k)*nc(i,k) - mu(i,k-1)*nc(i,k-1))/dz(i,k)*rho(i,k)
           trspim(i,k-1) = (mu(i,k)*qi(i,k) - mu(i,k-1)*qi(i,k-1))/dz(i,k)
           trspin(i,k-1) = (mu(i,k)*ni(i,k) - mu(i,k-1)*ni(i,k-1))/dz(i,k)*rho(i,k)

           if (k-1 .eq. jt(i)+1)  then
             trspcm(i,k-2) =  mu(i,k-1)*qc(i,k-1)/dz(i,k)
             trspcn(i,k-2) =  mu(i,k-1)*nc(i,k-1)/dz(i,k)*rho(i,k)
             trspim(i,k-2) =  mu(i,k-1)*qi(i,k-1)/dz(i,k)
             trspin(i,k-2) =  mu(i,k-1)*ni(i,k-1)/dz(i,k)*rho(i,k)
             dlfm  (i,k-2) = -du(i,k-2)*qc(i,k-1)
             dlfn  (i,k-2) = -du(i,k-2)*nc(i,k-1)*rho(i,k)
             difm  (i,k-2) = -du(i,k-2)*qi(i,k-1)
             difn  (i,k-2) = -du(i,k-2)*ni(i,k-1)*rho(i,k)
           end if
!.......................................................................
! get size distribution parameters for precip
!......................................................................
! rain
           if (qr(i,k-1).ge.qsmall) then

             lamr(k-1) = (pi*rhow*nr(i,k-1)/qr(i,k-1))**(1._r8/3._r8)
             n0r(k-1) = nr(i,k-1)*lamr(k-1)
! check for slope
             lammax = 1._r8/20.e-6_r8
             lammin = 1._r8/500.e-6_r8
! adjust vars
             if (lamr(k-1).lt.lammin) then
               lamr(k-1) = lammin
               n0r(k-1) = lamr(k-1)**4*qr(i,k-1)/(pi*rhow)
               nr(i,k-1) = n0r(k-1)/lamr(k-1)
             else if (lamr(k-1).gt.lammax) then
               lamr(k-1) = lammax
               n0r(k-1) = lamr(k-1)**4*qr(i,k-1)/(pi*rhow)
               nr(i,k-1) = n0r(k-1)/lamr(k-1)
             end if
           else
             lamr(k-1) = 0._r8
             n0r(k-1) = 0._r8
           end if
!......................................................................
! snow
           if (qni(i,k-1).ge.qsmall) then
             lams(k-1) = (mskf_gamma(1._r8+ds)*cs*ns(i,k-1)/ &
                       qni(i,k-1))**(1._r8/ds)
             n0s(k-1) = ns(i,k-1)*lams(k-1)

! check for slope
             lammax = 1._r8/10.e-6_r8
             lammin = 1._r8/2000.e-6_r8
! adjust vars
             if (lams(k-1).lt.lammin) then
               lams(k-1) = lammin
               n0s(k-1) = lams(k-1)**(ds+1._r8)*qni(i,k-1)/(cs*mskf_gamma(1._r8+ds))
               ns(i,k-1) = n0s(k-1)/lams(k-1)
             else if (lams(k-1).gt.lammax) then
               lams(k-1) = lammax
               n0s(k-1) = lams(k-1)**(ds+1._r8)*qni(i,k-1)/(cs*mskf_gamma(1._r8+ds))
               ns(i,k-1) = n0s(k-1)/lams(k-1)
             end if
             effs(i,k-1) = 1.5_r8/lams(k-1)*1.e6_r8
           else
             lams(k-1) = 0._r8
             n0s(k-1) = 0._r8
             effs(i,k-1) = 0._r8
           end if

!dkay : since KF treats rain and snow separately, no need to add snow to the
!rprd (kg/kg/m)
!dkay        rprd(i,k-1)= (qnitend(i,k) + qrtend(i,k))*arcf(i,k)    ! original
        rprd(i,k-1)=  qrtend(i,k)  *arcf(i,k)
        sprd(i,k-1)=  qnitend(i,k) *arcf(i,k)

!dkay
!dkay       print *,'k,rprd,qrtend,qcic
!=',k,rprd(i,k-1),qrtend(i,k-1),qcic(i,k-1)
!dkay 
     end if  ! k<jlcl

! if rain/snow mix ratio is zero so should number concentration

         if (qni(i,k-1).lt.qsmall) then
           qni(i,k-1)=0._r8
           ns(i,k-1)=0._r8
         end if

         if (qr(i,k-1).lt.qsmall) then
           qr(i,k-1)=0._r8
           nr(i,k-1)=0._r8
         end if
         if (qi(i,k-1).lt.qsmall) then
           qi(i,k-1)=0._r8
           ni(i,k-1)=0._r8
         end if

         if (qc(i,k-1).lt.qsmall) then
           qc(i,k-1)=0._r8
           nc(i,k-1)=0._r8
         end if

! make sure number concentration is a positive number to avoid 
! taking root of negative

         nr(i,k-1)=max(nr(i,k-1),0._r8)
         ns(i,k-1)=max(ns(i,k-1),0._r8)
         ni(i,k-1)=max(ni(i,k-1),0._r8)
         nc(i,k-1)=max(nc(i,k-1),0._r8)
!!......................................................................
!dkay
  !      frz(i,k) = amin1(1.E-07, frz(i,k))  ! constrain frz 
       end do ! k loop
!dkay          print *,'jb, jt=',jb(i), jt(i)
       end do ! it loop, iteration
300    continue  ! continue if no cloud water
       end do ! i loop
!........................................................................

!        deallocate( &
!         naermod,  &
!         naer2,    &
!         naer2h,    &
!         maerosol)

!        deallocate( &
!         naermod,  &
!         naer2,    &
!         naer2h,    &
!         maerosol)

return
end subroutine mskf_mphy

!##############################################################################


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION mskf_GAMMA(X)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!D    DOUBLE PRECISION FUNCTION DGAMMA(X)
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
!*******************************************************************
!*******************************************************************
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
! REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
!              FUNCTIONS   W. J. CODY, LECTURE NOTES IN MATHEMATICS,
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
!----------------------------------------------------------------------
      INTEGER I,N
      LOGICAL PARITY

      real(r8) mskf_GAMMA
      REAL(r8) &
!D    DOUBLE PRECISION
         C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE, &
         TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)
!----------------------------------------------------------------------
!  MATHEMATICAL CONSTANTS
!----------------------------------------------------------------------
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0_r8,0.5E0_r8,12.0E0_r8,2.0E0_r8,0.0E0_r8/, &
          SQRTPI/0.9189385332046727417803297E0_r8/, &
          PI/3.1415926535897932384626434E0_r8/
!D    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,
!D   1     SQRTPI/0.9189385332046727417803297D0/,
!D   2     PI/3.1415926535897932384626434D0/
!----------------------------------------------------------------------
!  MACHINE DEPENDENT PARAMETERS
!----------------------------------------------------------------------
      DATA XBIG,XMININ,EPS/35.040E0_r8,1.18E-38_r8,1.19E-7_r8/, &
          XINF/3.4E38_r8/
!D    DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,
!D   1     XINF/1.79D308/
!----------------------------------------------------------------------
!  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
!     APPROXIMATION OVER (1,2).
!----------------------------------------------------------------------
      DATA P/-1.71618513886549492533811E+0_r8,2.47656508055759199108314E+1_r8,&
            -3.79804256470945635097577E+2_r8,6.29331155312818442661052E+2_r8,&
            8.66966202790413211295064E+2_r8,-3.14512729688483675254357E+4_r8,&
            -3.61444134186911729807069E+4_r8,6.64561438202405440627855E+4_r8/
      DATA Q/-3.08402300119738975254353E+1_r8,3.15350626979604161529144E+2_r8,&
           -1.01515636749021914166146E+3_r8,-3.10777167157231109440444E+3_r8,&
             2.25381184209801510330112E+4_r8,4.75584627752788110767815E+3_r8,&
           -1.34659959864969306392456E+5_r8,-1.15132259675553483497211E+5_r8/
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
!----------------------------------------------------------------------
      DATA C/-1.910444077728E-03_r8,8.4171387781295E-04_r8, &
          -5.952379913043012E-04_r8,7.93650793500350248E-04_r8,&
          -2.777777777777681622553E-03_r8,8.333333333333333331554247E-02_r8,&
           5.7083835261E-03_r8/
!D    DATA C/-1.910444077728D-03,8.4171387781295D-04,
!D   1     -5.952379913043012D-04,7.93650793500350248D-04,
!D   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,
!D   3      5.7083835261D-03/
!----------------------------------------------------------------------
!  STATEMENT FUNCTIONS FOR CONVERSION BETWEEN INTEGER AND FLOAT
!----------------------------------------------------------------------
      CONV(I) = REAL(I,r8)
!D    CONV(I) = DBLE(I)
      PARITY=.FALSE.
      FACT=ONE
      N=0
      Y=X
      IF(Y.LE.ZERO)THEN
!----------------------------------------------------------------------
!  ARGUMENT IS NEGATIVE
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!  ARGUMENT IS POSITIVE
!----------------------------------------------------------------------
      IF(Y.LT.EPS)THEN
!----------------------------------------------------------------------
!  ARGUMENT .LT. EPS
!----------------------------------------------------------------------
        IF(Y.GE.XMININ)THEN
          RES=ONE/Y
        ELSE
          RES=XINF
          GOTO 900
        ENDIF
      ELSEIF(Y.LT.TWELVE)THEN
        Y1=Y
        IF(Y.LT.ONE)THEN
!----------------------------------------------------------------------
!  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          Z=Y
          Y=Y+ONE
        ELSE
!----------------------------------------------------------------------
!  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
!----------------------------------------------------------------------
          N=INT(Y)-1
          Y=Y-CONV(N)
          Z=Y-ONE
        ENDIF
!----------------------------------------------------------------------
!  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
!----------------------------------------------------------------------
        XNUM=ZERO
        XDEN=ONE
        DO 260 I=1,8
          XNUM=(XNUM+P(I))*Z
          XDEN=XDEN*Z+Q(I)
  260   CONTINUE
        RES=XNUM/XDEN+ONE
        IF(Y1.LT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
!----------------------------------------------------------------------
          RES=RES/Y1
        ELSEIF(Y1.GT.Y)THEN
!----------------------------------------------------------------------
!  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
!----------------------------------------------------------------------
          DO 290 I=1,N
            RES=RES*Y
            Y=Y+ONE
  290     CONTINUE
        ENDIF
      ELSE
!----------------------------------------------------------------------
!  EVALUATE FOR ARGUMENT .GE. 12.0,
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
!  FINAL ADJUSTMENTS AND RETURN
!----------------------------------------------------------------------
      IF(PARITY)RES=-RES
      IF(FACT.NE.ONE)RES=FACT/RES
  900 mskf_GAMMA=RES
!D900 DGAMMA = RES
      RETURN
! ---------- LAST LINE OF mskf_GAMMA ----------
      END function mskf_GAMMA

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! error function in single precision
!
!    Copyright(C) 1996 Takuya OOURA (email: ooura@mmm.t.u-tokyo.ac.jp).
!    You may use, copy, modify this code for any purpose and
!    without fee. You may distribute this ORIGINAL package.

      function derf(x)
      implicit real (a - h, o - z)
      real(r8) a,b,x
      dimension a(0 : 64), b(0 : 64)
      integer i,k
      data (a(i), i = 0, 12) / &
         0.00000000005958930743d0, -0.00000000113739022964d0, &
         0.00000001466005199839d0, -0.00000016350354461960d0, &
         0.00000164610044809620d0, -0.00001492559551950604d0, &
         0.00012055331122299265d0, -0.00085483269811296660d0, &
         0.00522397762482322257d0, -0.02686617064507733420d0, &
         0.11283791670954881569d0, -0.37612638903183748117d0, &
         1.12837916709551257377d0 /
      data (a(i), i = 13, 25) / &
         0.00000000002372510631d0, -0.00000000045493253732d0, &
         0.00000000590362766598d0, -0.00000006642090827576d0, &
         0.00000067595634268133d0, -0.00000621188515924000d0, &
         0.00005103883009709690d0, -0.00037015410692956173d0, &
         0.00233307631218880978d0, -0.01254988477182192210d0, &
         0.05657061146827041994d0, -0.21379664776456006580d0, &
         0.84270079294971486929d0 /
      data (a(i), i = 26, 38) / &
         0.00000000000949905026d0, -0.00000000018310229805d0, &
         0.00000000239463074000d0, -0.00000002721444369609d0, &
         0.00000028045522331686d0, -0.00000261830022482897d0, &
         0.00002195455056768781d0, -0.00016358986921372656d0, &
         0.00107052153564110318d0, -0.00608284718113590151d0, &
         0.02986978465246258244d0, -0.13055593046562267625d0, &
         0.67493323603965504676d0 /
      data (a(i), i = 39, 51) / &
         0.00000000000382722073d0, -0.00000000007421598602d0, &
         0.00000000097930574080d0, -0.00000001126008898854d0, &
         0.00000011775134830784d0, -0.00000111992758382650d0, &
         0.00000962023443095201d0, -0.00007404402135070773d0, &
         0.00050689993654144881d0, -0.00307553051439272889d0, &
         0.01668977892553165586d0, -0.08548534594781312114d0, &
         0.56909076642393639985d0 /
      data (a(i), i = 52, 64) / &
         0.00000000000155296588d0, -0.00000000003032205868d0, &
         0.00000000040424830707d0, -0.00000000471135111493d0, &
         0.00000005011915876293d0, -0.00000048722516178974d0, &
         0.00000430683284629395d0, -0.00003445026145385764d0, &
         0.00024879276133931664d0, -0.00162940941748079288d0, &
         0.00988786373932350462d0, -0.05962426839442303805d0, &
         0.49766113250947636708d0 /
     data (b(i), i = 0, 12) / &
         -0.00000000029734388465d0, 0.00000000269776334046d0, &
         -0.00000000640788827665d0, -0.00000001667820132100d0, &
         -0.00000021854388148686d0, 0.00000266246030457984d0, &
         0.00001612722157047886d0, -0.00025616361025506629d0, &
         0.00015380842432375365d0, 0.00815533022524927908d0, &
         -0.01402283663896319337d0, -0.19746892495383021487d0,&
         0.71511720328842845913d0 /
      data (b(i), i = 13, 25) / &
         -0.00000000001951073787d0, -0.00000000032302692214d0, &
         0.00000000522461866919d0, 0.00000000342940918551d0, &
         -0.00000035772874310272d0, 0.00000019999935792654d0, &
         0.00002687044575042908d0, -0.00011843240273775776d0, &
         -0.00080991728956032271d0, 0.00661062970502241174d0, &
         0.00909530922354827295d0, -0.20160072778491013140d0, &
         0.51169696718727644908d0 /
      data (b(i), i = 26, 38) / &
         0.00000000003147682272d0, -0.00000000048465972408d0, &
         0.00000000063675740242d0, 0.00000003377623323271d0, &
         -0.00000015451139637086d0, -0.00000203340624738438d0,&
         0.00001947204525295057d0, 0.00002854147231653228d0, &
         -0.00101565063152200272d0, 0.00271187003520095655d0, &
         0.02328095035422810727d0, -0.16725021123116877197d0, &
         0.32490054966649436974d0 /
      data (b(i), i = 39, 51) / &
         0.00000000002319363370d0, -0.00000000006303206648d0, &
         -0.00000000264888267434d0, 0.00000002050708040581d0, &
         0.00000011371857327578d0, -0.00000211211337219663d0, &
         0.00000368797328322935d0, 0.00009823686253424796d0, &
         -0.00065860243990455368d0, -0.00075285814895230877d0,&
         0.02585434424202960464d0, -0.11637092784486193258d0, &
         0.18267336775296612024d0 /
      data (b(i), i = 52, 64) / &
         -0.00000000000367789363d0, 0.00000000020876046746d0, &
         -0.00000000193319027226d0, -0.00000000435953392472d0, &
         0.00000018006992266137d0, -0.00000078441223763969d0, &
         -0.00000675407647949153d0, 0.00008428418334440096d0, &
         -0.00017604388937031815d0, -0.00239729611435071610d0, &
         0.02064129023876022970d0, -0.06905562880005864105d0, &
         0.09084526782065478489d0 /
      w = abs(x)
      if (w .lt. 2.2d0) then
          t = w * w
          k = int(t)
          t = t - k
          k = k * 13
          y = ((((((((((((a(k) * t + a(k + 1)) * t + &
             a(k + 2)) * t + a(k + 3)) * t + a(k + 4)) * t + &
             a(k + 5)) * t + a(k + 6)) * t + a(k + 7)) * t + &
             a(k + 8)) * t + a(k + 9)) * t + a(k + 10)) * t + &
             a(k + 11)) * t + a(k + 12)) * w
      else if (w .lt. 6.9d0) then
          k = int(w)
          t = w - k
          k = 13 * (k - 2)
          y = (((((((((((b(k) * t + b(k + 1)) * t + &
             b(k + 2)) * t + b(k + 3)) * t + b(k + 4)) * t + &
             b(k + 5)) * t + b(k + 6)) * t + b(k + 7)) * t + &
             b(k + 8)) * t + b(k + 9)) * t + b(k + 10)) * t + &
             b(k + 11)) * t + b(k + 12)
          y = y * y
          y = y * y
          y = y * y
          y = 1 - y * y
      else
          y = 1
      end if
      if (x .lt. 0) y = -y
      derf = y
      end function derf

!-----------------------------------------------------------------------
        real function erfc_num_recipes( x )
!
!   from press et al, numerical recipes, 1990, page 164
!
        implicit none
        real x
        double precision erfc_dbl, dum, t, zz

        zz = abs(x)
        t = 1.0/(1.0 + 0.5*zz)

!       erfc_num_recipes =
!     &   t*exp( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
!     &   t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
!     &                                    t*(-1.13520398 +
!     &   t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

        dum =  ( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
          t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
                                           t*(-1.13520398 +   &
          t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

        erfc_dbl = t * exp(dum)
        if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl

        erfc_num_recipes = erfc_dbl

        return
        end function erfc_num_recipes

!-----------------------------------------------------------------------
    real function erf_alt( x )

    implicit none

    real,intent(in) :: x

    erf_alt = 1. - erfc_num_recipes(x)

    end function erf_alt
      subroutine mskf_activate(wbar, tair, rhoair,  &
                 na, pmode, nmode, ma, sigman, hygro, rhodry, nact,qs)
!      calculates number, surface, and mass fraction of aerosols activated as
!      CCN
!      calculates flux of cloud droplets, surface area, and aerosol mass into
!      cloud
!      assumes an internal mixture within each of up to pmode multiple aerosol
!      modes
!      a gaussiam spectrum of updrafts can be treated.

!      mks units

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

!      use physconst, only: rair, epsilo, cpair, rh2o, latvap, gravit,   &
!                                 rhoh2o, mwh2o, r_universal
!ckay      use wv_saturation, only: estblf, epsqs

      implicit none

!      save  ! sep6

!      input

      integer pmode,ptype ! dimension of modes, types in modes
      real(r8) wbar          ! grid cell mean vertical velocity (m/s)
      real(r8) tair          ! air temperature (K)
      real(r8) rhoair        ! air density (kg/m3)
      real(r8) na(pmode)           ! aerosol number concentration (/m3)
      integer nmode      ! number of aerosol modes
      real(r8) ma(pmode)     ! aerosol mass concentration (kg/m3)
      real(r8) rhodry(pmode) ! density of aerosol material
      real(r8) sigman(pmode)  ! geometric standard deviation of aerosol size distribution
      real(r8) hygro(pmode)  ! hygroscopicity of aerosol mode


!      output

      real(r8) nact      ! number fraction of aerosols activated

!      local
#if (defined AIX)
#define ERF erf
#define ERFC erfc
#else
#define ERF derf
#define ERFC derfc
#define ERF_ALT erf_alt
      real(r8) derf,derfc, erf_alt
#endif

      integer, parameter:: nx=200
      integer :: maxmodes

      real(r8) surften       ! surface tension of water w/respect to air (N/m)
      data surften/0.076/
      save surften
      real(r8) p0     ! reference pressure (Pa)
      data p0/1013.25e2/
      save p0

      real(r8) :: volc(naer_cu) ! total aerosol volume  concentration (m3/m3)
      real(r8) tmass ! total aerosol mass concentration (g/cm3)
      real(r8) rm ! number mode radius of aerosol at max supersat (cm)
      real(r8) pres ! pressure (Pa)
      real(r8) path ! mean free path (m)
      real(r8) diff ! diffusivity (m2/s)
      real(r8) conduct ! thermal conductivity (Joule/m/sec/deg)
      real(r8) diff0,conduct0
      real(r8) qs ! water vapor saturation mixing ratio
      real(r8) dqsdt ! change in qs with temperature
      real(r8) dqsdp ! change in qs with pressure
      real(r8) gloc ! thermodynamic function (m2/s)
      real(r8) zeta
      real(r8) :: eta(naer_cu)
      real(r8) :: smc(naer_cu)
      real(r8) lnsmax ! ln(smax)
      real(r8) alpha
      real(r8) gammaloc
      real(r8) beta
      real(r8) sqrtg
      real(r8) alogam
      real(r8) rlo,rhi,xint1,xint2,xint3,xint4
      real(r8) w,wnuc,wb
      real(r8) alw,sqrtalw
      real(r8) smax
      real(r8) x,arg
      real(r8) xmincoeff,xcut,volcut,surfcut
      real(r8) z,z1,z2,wf1,wf2,zf1,zf2,gf1,gf2,gf
      real(r8) :: etafactor1,etafactor2max
      real(r8) :: etafactor2(naer_cu)
      real(r8) es
      integer m,n

      real(r8) :: amcubeloc(naer_cu)
      real(r8) :: lnsmloc(naer_cu)
      maxmodes = naer_cu

      if(maxmodes<pmode)then
!         write(*,*)'maxmodes,pmode in activate =',maxmodes,pmode
!         call endrun('kf_activate')
      endif

      nact=0._r8

      if(nmode.eq.1.and.na(1).lt.1.e-20)return

      if(wbar.le.0.)return

      pres=rair*rhoair*tair
      diff0=0.211e-4*(p0/pres)*(tair/t0)**1.94
      conduct0=(5.69+0.017*(tair-t0))*4.186e2*1.e-5 ! convert to J/m/s/deg
!ckay      es = estblf(tair)
!ckay      qs = epsilo*es/(pres-(1.0_r8 - epsqs)*es)
!        print *,'rh2o=',rh2o
      dqsdt=latvap/(rh2o*tair*tair)*qs
      alpha=gravit*(latvap/(cpair*rh2o*tair*tair)-1./(rair*tair))
      gammaloc=(1+latvap/cpair*dqsdt)/(rhoair*qs)
!     growth coefficent Abdul-Razzak & Ghan 1998 eqn 16
!     should depend on mean radius of mode to account for gas kinetic effects
      gloc=1./(rhoh2o/(diff0*rhoair*qs)                                    &
          +latvap*rhoh2o/(conduct0*tair)*(latvap/(rh2o*tair)-1.))
      sqrtg=sqrt(gloc)
      beta=4.*pi*rhoh2o*gloc*gammaloc
      etafactor2max=1.e10/(alpha*wbar)**1.5 ! this should make eta big if na is very small.

      do m=1,nmode
!         internal mixture of aerosols
          volc(m)=ma(m)/(rhodry(m)) ! only if variable size dist
         if(volc(m).gt.1.e-39_r8.and.na(m).gt.1.e-39_r8)then
            etafactor2(m)=1./(na(m)*beta*sqrtg)  !fixed or variable size dist
!            number mode radius (m)
            amcubeloc(m)=(3.*volc(m)/(4.*pi*exp45logsig(m)*na(m)))  ! only if variable size dist
            smc(m)=smcrit(m) ! only for prescribed size dist

!May30,2014
                 if(hygro(m).gt.1.e-10)then   ! loop only if variable size dist
                    smc(m)=2.*aten*sqrt(aten/(27.*hygro(m)*amcubeloc(m)))
                 else
                   smc(m)=100.
                 endif
         else
            smc(m)=1.
            etafactor2(m)=etafactor2max ! this should make eta big if na is very small.
         endif
         lnsmloc(m)=log(smc(m)) ! only if variable size dist
      enddo

!         single  updraft
         wnuc=wbar
!        write(iulog,*)'uniform updraft =',wnuc

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)
            etafactor1=2.*alw*sqrtalw

            do m=1,nmode
               eta(m)=etafactor1*etafactor2(m)
            enddo

!             print *,' kf_maxsat '
            call mskf_maxsat(zeta,eta,nmode,smc,smax)

            lnsmax=log(smax)
!           print *,'smc,smax=',smc,smax
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

            nact=0._r8
            do m=1,nmode
               x=2*(lnsmloc(m)-lnsmax)/(3*sq2*alogsig(m))
!original ghan code
!               nact=nact+0.5*(1.-ERF(x))*na(m)
!++ag replace sg erf with hm derf pre 1.68
!               nact=nact+0.5*(1.-derf(x))*na(m)
!++ag 1.68 new error function
                nact=nact+0.5*(1.-erf(x))*na(m)  
!               nact=nact+0.5*(1.-derf(x))*na(m)  
!       write(*,*)'nact',nact,derf(x),na(m),m
!       write(*,*) 'lnsmloc(m)',lnsmloc(m),lnsmax,alogsig(m)
!                write(*,*) 'wbar=',wbar
            enddo
            nact=nact/rhoair ! convert from #/m3 to #/kg

!      write(*,*)'na(m),qs',na(m),m,qs
!      write(*,*)'nact',nact
!      deallocate( &
!         volc,       &
!         eta,        &
!         smc,        &
!         etafactor2, &
!         amcubeloc,  &
!         lnsmloc     )

      return
      end subroutine mskf_activate

      subroutine mskf_maxsat(zeta,eta,nmode,smc,smax)

!      calculates maximum supersaturation for multiple
!      competing aerosol modes.

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      implicit none
!     save ! sep6
      integer nmode ! number of modes
      real(r8) :: smc(:) ! critical supersaturation for number mode radius
      real(r8) zeta
      real(r8) :: eta(:)
      real(r8) smax ! maximum supersaturation
      integer m  ! mode index
      real(r8) sum, g1, g2

      do m=1,nmode
         if(zeta.gt.1.e5*eta(m).or.smc(m)*smc(m).gt.1.e5*eta(m))then
!            weak forcing. essentially none activated
            smax=1.e-20
         else
!            significant activation of this mode. calc activation all modes.
            go to 1
         endif
      enddo

      return

  1   continue

      sum=0
      do m=1,nmode
         if(eta(m).gt.1.e-20)then
            g1=sqrt(zeta/eta(m))
            g1=g1*g1*g1
            g2=smc(m)/sqrt(eta(m)+3*zeta)
            g2=sqrt(g2)
            g2=g2*g2*g2
            sum=sum+(f1(m)*g1+f2(m)*g2)/(smc(m)*smc(m))
!    write(*,*)'f1(m)',f1(m),m
         else
            sum=1.e20
         endif
      enddo

      smax=1./sqrt(sum)

      return

      end subroutine mskf_maxsat

subroutine mskf_nucleati(wbar, tair, pair, qv,  qc,  rhoair, & ! TWG add pair and replace relhum with qv
       na,  naer_all, nuci  &
       , onihf, oniimm, onidep, onimey)

!---------------------------------------------------------------
! Purpose:
!  The parameterization of ice nucleation.
!
! Method: The current method is based on Liu & Penner (2005)
!  It related the ice nucleation with the aerosol number, temperature and the
!  updraft velocity. It includes homogeneous freezing of sulfate, immersion
!  freezing of soot, and Meyers et al. (1992) deposition nucleation
!
! Authors: Xiaohong Liu, 01/2005, modifications by A. Gettelman 2009-2010
!----------------------------------------------------------------
! Input Arguments
!
!    save   ! sep6
  integer  naer_all
  real(r8) :: wbar                ! grid cell mean vertical velocity (m/s)
  real(r8) :: tair                ! temperature (K)
  real(r8) :: qv                  ! water vapor mixing ratio (kg/kg)
  real(r8) :: pair                ! pressure (Pa)

  real(r8) :: qc                  ! liquid water mixing ratio (kg/kg)
  real(r8) :: rhoair              ! air density (kg/m3)
  real(r8) :: na(naer_all)        ! aerosol number concentration (/m3)

!
! Output Arguments
!
  real(r8) :: nuci               ! ice number nucleated (#/kg)
  real(r8) :: onihf              ! nucleated number from homogeneous freezing of so4
  real(r8) :: oniimm             ! nucleated number from immersion freezing
  real(r8) :: onidep             ! nucleated number from deposition nucleation
  real(r8) :: onimey             ! nucleated number from deposition nucleation (meyers: mixed phase)
!
! Local workspace
!
  real(r8)  so4_num                                      ! so4 aerosol number (#/cm^3)
  real(r8)  soot_num                                     ! soot (hydrophilic) aerosol number (#/cm^3)
  real(r8)  dst1_num,dst2_num,dst3_num,dst4_num          ! dust aerosol number (#/cm^3)
  real(r8)  dst_num                                      ! total dust aerosol number (#/cm^3)
  real(r8)  nihf                                         ! nucleated number from homogeneous freezing of so4
  real(r8)  niimm                                        ! nucleated number from immersion freezing
  real(r8)  nidep                                        ! nucleated number from deposition nucleation
  real(r8)  nimey                                        ! nucleated number from deposition nucleation (meyers)
  real(r8)  n1,ni                                        ! nucleated number
  real(r8)  tc,A,B,C,regm                                ! work variable
  real(r8)  esl,esi,deles,qsi,qsl,relhum                 ! work variable
  real(r8)  dst_scale
  real(r8)  subgrid
  real(r8)  dmc,ssmc         ! variables for modal scheme.

    so4_num=0.0_r8
    soot_num=0.0_r8
    dst_num=0.0_r8
    dst1_num = 0.0_r8
    dst2_num = 0.0_r8
    dst3_num = 0.0_r8
    dst4_num = 0.0_r8

!For modal aerosols, assume for the upper troposphere:
! soot = accumulation mode
! sulfate = aiken mode
! dust = coarse mode
! since modal has internal mixtures.

    if(idxsul .gt. 0) then
      so4_num=na(idxsul)*1.0e-6_r8 ! #/cm^3
    end if

    if(idxbcphi .gt. 0) then
      soot_num=na(idxbcphi)*1.0e-6_r8 !#/cm^3
    end if

    if(idxdst1 .gt. 0) then
       dst1_num=na(idxdst1)*1.0e-6_r8 !#/cm^3
    end if

    if(idxdst2 .gt. 0) then
       dst2_num=na(idxdst2)*1.0e-6_r8 !#/cm^3
    end if

    if(idxdst3 .gt. 0) then
       dst3_num=na(idxdst3)*1.0e-6_r8 !#/cm^3
    end if

    if(idxdst4 .gt. 0) then
       dst4_num=na(idxdst4)*1.0e-6_r8 !#/cm^3
    end if

    dst_num =dst1_num+dst2_num+dst3_num+dst4_num
! no soot nucleation for now.
   ! soot_num=0.0_r8

    ni=0._r8
    tc=tair-273.15_r8

!TWG calculate Ice saturation ratio
    esi = 611.2*exp(21.87*(tair-273.16)/(tair-7.66))
    esl = 611.2*exp(17.67*(tair-273.16)/(243.5+tair-273.16))
    qsi = 0.622*esi/(pair-esi)
    qsl = 0.622*esl/(pair-esl)
    deles = qv/qsi
    relhum = qv/qsl

    ! initialize
    niimm=0._r8
    nidep=0._r8
    nihf=0._r8

    if(so4_num.ge.1.0e-10_r8 .and. (soot_num+dst_num).ge.1.0e-10_r8 ) then

      subgrid = 1.0_r8

!          print *,'nucleiate wbar=', wbar
     if((wbar.lt.4.0_r8) .and. (tc.le.-35.0_r8) .and.((deles*subgrid).ge.1.0_r8)) then !TWG Make RHi consistent
!< regm => T in Eq.10 of Liu et al., J. Climate, 2007>
       print*,'Aerosol Ice Nucleation is Doing Something'
       A = -1.4938_r8 * log(soot_num+dst_num) + 12.884_r8
       B = -10.41_r8  * log(soot_num+dst_num) - 67.69_r8
       regm = A * log(wbar) + B

!         print *,'before bunch of hetero'
       if(tc.gt.regm) then    ! heterogeneous nucleation only
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then ! exclude T<-40 & W>1m/s from hetero. nucleation
           call mskf_hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else
           call mskf_hetero(tc,wbar,soot_num+dst_num,niimm,nidep)
           nihf=0._r8
           n1=niimm+nidep
         endif
       elseif (tc.lt.regm-5._r8) then ! homogeneous nucleation only
         call mskf_hf(tc,wbar,relhum,subgrid,so4_num,nihf)
         niimm=0._r8
         nidep=0._r8
         n1=nihf
       else        ! transition between homogeneous and heterogeneous: interpolate in-between
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then ! exclude T<-40 & W>1m/s from hetero. nucleation
           call mskf_hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else
           call mskf_hf(regm-5._r8,wbar,relhum,subgrid,so4_num,nihf)
           call mskf_hetero(regm,wbar,soot_num+dst_num,niimm,nidep)
           if(nihf.le.(niimm+nidep)) then
             n1=nihf
           else
              n1=(niimm+nidep)*((niimm+nidep)/nihf)**((tc-regm)/5._r8)
           endif
         endif
       endif

       ni=n1

    endif
    endif
1100  continue

! deposition/condensation nucleation in mixed clouds (-37<T<0C) (Meyers, 1992)
!<Eq.12 of Liu et al., J. Climate, 2007
! Nid(L-1)*1.e-3 => Nid(m-3)
! Question:  RHi=RHw*esl/esi

    if(tc.lt.0._r8 .and. tc.gt.-37._r8 .and. qc.gt.1.e-12_r8) then
!     if(tc.lt.0._r8 .and. tc.gt.-37._r8) then  ! TWG  remove cloud water constraint
!      esl = kf_polysvp(tair,0)     ! over water in mixed clouds
!      esi = kf_polysvp(tair,1)     ! over ice
!songxl      deles = (esl - esi)
!      deles = (relhum*esl - esi)
       if (deles.gt.1.5) THEN
             deles = 1.5
        end if
      nimey=1.e-3_r8*exp(12.96_r8*(deles-1.0_r8) - 0.639_r8) ! TWG fix Meyers formulation
    else
      nimey=0._r8
    endif

    nuci=ni+nimey
    if(nuci.gt.9999._r8.or.nuci.lt.0._r8) then
       write(*, *) 'incorrect ice nucleation number'
       write(*, *) ni, tair, relhum, wbar, nihf, niimm,nidep,deles,esi,dst2_num,dst3_num,dst4_num
       nuci=0._r8
         CALL wrf_error_fatal ( 'Incorrect Ice Nucleation Number, diags' )
    endif

    nuci=nuci*1.e+6_r8/rhoair    ! change unit from #/cm3 to #/kg
    onimey=nimey*1.e+6_r8/rhoair
    onidep=nidep*1.e+6_r8/rhoair
    oniimm=niimm*1.e+6_r8/rhoair
    onihf=nihf*1.e+6_r8/rhoair

!     print *,'inputs=',wbar, tair, relhum,  qc,  rhoair, &
!      na,  naer_all, nuci,onimey,onidep,oniimm,onihf 
!     print *,'na,tari,nuci.. =', na,tair,nuci,onimey,onidep,oniimm,onihf
  return
  end subroutine mskf_nucleati

  subroutine mskf_hetero(T,ww,Ns,Nis,Nid)

    real(r8) :: T, ww, Ns
    real(r8) :: Nis, Nid

    real(r8) A11,A12,A21,A22,B11,B12,B21,B22
    real(r8) A,B,C

!    save    ! spe6
!---------------------------------------------------------------------
! parameters

      A11 = 0.0263_r8
      A12 = -0.0185_r8
      A21 = 2.758_r8
      A22 = 1.3221_r8
      B11 = -0.008_r8
      B12 = -0.0468_r8
      B21 = -0.2667_r8
      B22 = -1.4588_r8
!<Eq.11 of Liu et al., J. Climate, 2007>
!     ice from immersion nucleation (cm-3)

      B = (A11+B11*log(Ns)) * log(ww) + (A12+B12*log(Ns))
      C =  A21+B21*log(Ns)

      Nis = exp(A22) * Ns**B22 * exp(B*T) * ww**C
      Nis = min(Nis,Ns)

      Nid = 0.0_r8    ! don't include deposition nucleation for cirrus clouds when T<-37C

      return
  end subroutine mskf_hetero

 subroutine mskf_hf(T,ww,RH,subgrid,Na,Ni)

      real(r8) :: T, ww, RH, subgrid, Na
      real(r8), intent(out) :: Ni

      real(r8)    A1_fast,A21_fast,A22_fast,B1_fast,B21_fast,B22_fast
      real(r8)    A2_fast,B2_fast
      real(r8)    C1_fast,C2_fast,k1_fast,k2_fast
      real(r8)    A1_slow,A2_slow,B1_slow,B2_slow,B3_slow
      real(r8)    C1_slow,C2_slow,k1_slow,k2_slow
      real(r8)    regm
      real(r8)    A,B,C
      real(r8)    RHw

!     save   ! sep6
!---------------------------------------------------------------------
!<Table 1 of  Liu et al., J. Climate, 2007>
! parameters

      A1_fast  =0.0231_r8
      A21_fast =-1.6387_r8  !(T>-64 deg)
      A22_fast =-6.045_r8   !(T<=-64 deg)
      B1_fast  =-0.008_r8
      B21_fast =-0.042_r8   !(T>-64 deg)
      B22_fast =-0.112_r8   !(T<=-64 deg)
      C1_fast  =0.0739_r8
      C2_fast  =1.2372_r8

      A1_slow  =-0.3949_r8
      A2_slow  =1.282_r8
      B1_slow  =-0.0156_r8
      B2_slow  =0.0111_r8
      B3_slow  =0.0217_r8
      C1_slow  =0.120_r8
      C2_slow  =2.312_r8

      Ni = 0.0_r8

!----------------------------
!<Eq.6 of Liu et al., J. Climate, 2007 
! w~m/s, T~degree C, RHw~% => RHw*0.01~fraction  >
!RHw xiaohong's parameter
      A = 6.0e-4_r8*log(ww)+6.6e-3_r8
      B = 6.0e-2_r8*log(ww)+1.052_r8
      C = 1.68_r8  *log(ww)+129.35_r8
      RHw=(A*T*T+B*T+C)*0.01_r8

      if((T.le.-37.0_r8) .and. ((RH*subgrid).ge.RHw)) then

!<Eq.9 of Liu et al., J. Climate, 2007>
        regm = 6.07_r8*log(ww)-55.0_r8

        if(T.ge.regm) then    ! fast-growth regime

          if(T.gt.-64.0_r8) then
            A2_fast=A21_fast
            B2_fast=B21_fast
          else
            A2_fast=A22_fast
            B2_fast=B22_fast
          endif
!<Eq.7 of Liu et al., J. Climate, 2007> 
          k1_fast = exp(A2_fast + B2_fast*T + C2_fast*log(ww))
          k2_fast = A1_fast+B1_fast*T+C1_fast*log(ww)

          Ni = k1_fast*Na**(k2_fast)
          Ni = min(Ni,Na)
        else       ! slow-growth regime
!<Eq.7 of Liu et al., J. Climate, 2007>
          k1_slow = exp(A2_slow + (B2_slow+B3_slow*log(ww))*T + C2_slow*log(ww))
          k2_slow = A1_slow+B1_slow*T+C1_slow*log(ww)

          Ni = k1_slow*Na**(k2_slow)
          Ni = min(Ni,Na)
        endif
      end if

      return
  end subroutine mskf_hf

      function mskf_polysvp (T,type)
!  Compute saturation vapor pressure by using
! function from Goff and Gatch (1946)

!  Polysvp returned in units of pa.
!  T is input in units of K.
!  type refers to saturation with respect to liquid (0) or ice (1)

      real(r8) dum

      real(r8) T,mskf_polysvp

      integer type

! ice

      if (type.eq.1) then

! Goff Gatch equation (good down to -100 C)

         mskf_polysvp = 10._r8**(-9.09718_r8*(273.16_r8/t-1._r8)-3.56654_r8* &
          log10(273.16_r8/t)+0.876793_r8*(1._r8-t/273.16_r8)+ &
          log10(6.1071_r8))*100._r8

      end if


! Goff Gatch equation, uncertain below -70 C

      if (type.eq.0) then
         mskf_polysvp = 10._r8**(-7.90298_r8*(373.16_r8/t-1._r8)+ &
             5.02808_r8*log10(373.16_r8/t)- &
             1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/373.16_r8))-1._r8)+ &
             8.1328e-3_r8*(10._r8**(-3.49149_r8*(373.16_r8/t-1._r8))-1._r8)+ &
             log10(1013.246_r8))*100._r8
         end if


      end function mskf_polysvp

 end module module_cu_mp
!end module zm_microphysics

!----------------------------------------------------------------------------------------------
!dkay begin MSKF
!.........................................

MODULE module_cu_mskf

   USE module_wrf_error
 
   !dkay
   USE module_cu_mp
!
!ckay=Kiran Alapaty, EPA
!CGM = Chris Marciano, NCSU
!
!multi-scale KF scheme
!  (1) With diagnosed deep and shallow KF cloud fraction using
!       CAM3-CAM5 methodology, along with captured liquid and ice condensates.
!       and linking with the RRTMG & Other radiation schemes
! (2) Scale-dependent Dynamic adjustment timescale for KF clouds (both shallow and deep)
! (3) Scale-dependent LCL-based entrainment Methodology that avoids 2-km cloud radius method
! (4) Scale-dependent Fallout Rate
! (5) Scale-dependent Stabilization Capacity
! (6) Elimination of "double counting" when environment is saturated
!
! Alapaty et al., 2012: Introducing subgrid-scale cloud feedbacks to radiation
!    for regional meteorological and climate modeling. GRL, V39, I24.
!
! Alapaty et al., 2013:  The Kain-Fritsch Scheme: Science Updates and revisiting
!     gray-scale issues from the NWP and regional climate perspectives. 2013 WRF
!     workshop: URL: http://www.mmm.ucar.edu/wrf/users/workshops/WS2013/ppts/9.2.pdf
!
! Herwehe et al., 2014: Increasing the credibility of regional climate simulations 
!     by introducing subgrid-scale cloud-radiation interactions. JGR, 119,
!     5317-5330, doi:10.1002/2014JD021504.
!
! Zheng et al., 2015: Improving High-Resolution Weather Forecasts using the
!     Weather Research and Forecasting (WRF) Model with an Updated Kain-Fritsch
!     Scheme. Revision - Mon. Wea. Rev.
!ckay
!--------------------------------------------------------------------
! Lookup table variables:
      INTEGER, PARAMETER :: KFNT=250,KFNP=220
      REAL, DIMENSION(KFNT,KFNP),PRIVATE, SAVE :: TTAB,QSTAB
      REAL, DIMENSION(KFNP),PRIVATE, SAVE :: THE0K
      REAL, DIMENSION(200),PRIVATE, SAVE :: ALU
      REAL, PRIVATE, SAVE :: RDPR,RDTHK,PLUTOP
! Note:  KF Lookup table is used by subroutines KF_eta_PARA, TPMIX2,
!        TPMIX2DD, ENVIRTHT
! End of Lookup table variables:

CONTAINS

   SUBROUTINE MSKF_CPS(                                      &
              ids,ide, jds,jde, kds,kde                      &
             ,ims,ime, jms,jme, kms,kme                      &
             ,its,ite, jts,jte, kts,kte                      &
             ,trigger                                        &
             ,DT,KTAU,DX,CUDT,ADAPT_STEP_FLAG                &
             ,rho,RAINCV,PRATEC,NCA                          &
             ,U,V,TH,T,W,dz8w,Pcps,pi                        &
             ,W0AVG,XLV0,XLV1,XLS0,XLS1,CP,R,G,EP1           &
             ,EP2,SVP1,SVP2,SVP3,SVPT0                       &
             ,STEPCU,CU_ACT_FLAG,warm_rain,CUTOP,CUBOT       &
             ,QV                                             &
            ! optionals
             ,F_QV    ,F_QC    ,F_QR    ,F_QI    ,F_QS       &
             ,RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN            &
             ,RQICUTEN,RQSCUTEN, RQVFTEN                     &
!ckay
             ,cldfra_dp_KF,cldfra_sh_KF,w_up                 &
             ,qc_KF,qi_KF,qr_KF,qs_KF                        & ! TWG
             ,nc_KF,ni_KF,nr_KF,ns_KF                        & ! TWG
             ,ccn_KF,ainc_frac                               & ! TWG
!kf_edrates
             ,UDR_KF,DDR_KF                                  &
             ,UER_KF,DER_KF                                  &
             ,TIMEC_KF,KF_EDRATES                            & 
             ,ZOL,WSTAR,UST,PBLH                             &   !ckay
             ,aerocu,no_src_types_cu,aercu_fct,aercu_opt     & !PSH/TWG
             ,EFCS,EFIS,EFSS)
!
!-------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 Add to avoid memory issues
!-------------------------------------------------------------
   INTEGER,      INTENT(IN   ) ::                            &
                                  ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte

   INTEGER,      INTENT(IN   ) :: trigger
   INTEGER,      INTENT(IN   ) :: STEPCU
   LOGICAL,      INTENT(IN   ) :: warm_rain

   REAL,         INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1
   REAL,         INTENT(IN   ) :: CP,R,G,EP1,EP2
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
                                                       Pcps, &
                                                        rho, &
                                                         pi

  INTEGER,      INTENT(IN   ) :: no_src_types_cu !PSH/TWG
  INTEGER,      INTENT(IN   ) :: aercu_opt       !PSH/TWG
  REAL,         INTENT(IN   ) :: aercu_fct       !PSH/TWG
  REAL,  DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types_cu), OPTIONAL, &
          INTENT(INOUT) ::                                   aerocu !PSH/TWG
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      W0AVG   

   REAL,  INTENT(IN   ) :: DT, DX
   REAL,  INTENT(IN   ) :: CUDT
   LOGICAL,OPTIONAL,INTENT(IN   ) :: ADAPT_STEP_FLAG
!
   REAL, DIMENSION( ims:ime , jms:jme ),                     &
          INTENT(INOUT) ::                           RAINCV

   REAL,    DIMENSION( ims:ime , jms:jme ),                  &
          INTENT(INOUT) ::                           PRATEC

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
         INTENT(INOUT) ::                                    &
                                                   RTHCUTEN, &
                                                   RQVCUTEN, &
                                                   RQCCUTEN, &
                                                   RQRCUTEN, &
                                                   RQICUTEN, &
                                                   RQSCUTEN, &
                                                   RQVFTEN
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

!ckay
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                               cldfra_dp_KF, &
                                               cldfra_sh_KF, &
                                                      qc_KF, &
                                                      qi_KF

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                                   &
                                                      qr_KF, & ! TWG
                                                      qs_KF, & ! TWG
                                                      nc_KF, & ! TWG
                                                      ni_KF, & ! TWG
                                                      nr_KF, & ! TWG
                                                      ns_KF, & ! TWG
                                                     ccn_KF, & ! TWG
                                                       EFCS, & ! TWG
                                                       EFIS, & ! TWG
                                                       EFSS

   REAL, DIMENSION( ims:ime , jms:jme ),                     & !TWG
          INTENT(INOUT) ::                           ainc_frac

!kf_edrates
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         , &
          INTENT(INOUT) ::                         &
                                                     UDR_KF, &
                                                     DDR_KF, &
                                                     UER_KF, &
                                                     DER_KF

   REAL,  DIMENSION( ims:ime , jms:jme )                   , &
          INTENT(INOUT) ::                         &
                                                   TIMEC_KF

   INTEGER, INTENT(IN) ::              KF_EDRATES

!ckay
   REAL, DIMENSION( ims:ime, jms:jme )                     , &
         INTENT(   IN) ::                               ZOL, &
                                                      WSTAR, &
                                                        UST, &
                                                       PBLH
!ckaywup
   REAL, DIMENSION( ims:ime,  kms:kme , jms:jme )          , &
           INTENT(INOUT) ::                             w_up

! LOCAL VARS

   LOGICAL :: flag_qr, flag_qi, flag_qs

   REAL, DIMENSION( kts:kte ) ::                             &
                                                        U1D, &
                                                        V1D, &
                                                        T1D, &
                                                       DZ1D, &
                                                       QV1D, &
                                                        P1D, &
                                                      RHO1D, &
                                                  tpart_v1D, &
                                                  tpart_h1D, &
                                                    W0AVG1D

   REAL, DIMENSION( kts:kte )::                              &
                                                       DQDT, &
                                                      DQIDT, &
                                                      DQCDT, &
                                                      DQRDT, &
                                                      DQSDT, &
                                                       DTDT

  REAL, DIMENSION (its-1:ite+1,kts:kte,jts-1:jte+1) ::  aveh_t, aveh_q
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) :: aveh_qmax, aveh_qmin
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) ::  avev_t, avev_q 
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) :: avev_qmax, avev_qmin
  REAL, DIMENSION (its:ite,kts:kte,jts:jte) ::  coef_v, coef_h, tpart_h, tpart_v
  INTEGER :: ii,jj,kk

  REAL :: ttop
  REAL, DIMENSION (kts:kte)  :: z0

   REAL    ::         TST,tv,PRS,RHOE,W0,SCR1,DXSQ,tmp
   integer :: ibegh,iendh,jbegh,jendh
   integer :: istart,iend,jstart,jend
   INTEGER :: i,j,k,NTST
   REAL    :: lastdt = -1.0
   REAL    :: W0AVGfctr, W0fctr, W0den
   
!
   DXSQ=DX*DX

!----------------------
   NTST=STEPCU
   TST=float(NTST*2)
   flag_qr = .FALSE.
   flag_qi = .FALSE.
   flag_qs = .FALSE.
   IF ( PRESENT(F_QR) ) flag_qr = F_QR
   IF ( PRESENT(F_QI) ) flag_qi = F_QI
   IF ( PRESENT(F_QS) ) flag_qs = F_QS
!
   if (lastdt < 0) then
      lastdt = dt
   endif
   
   if (ADAPT_STEP_FLAG) then
      W0AVGfctr = 2 * MAX(CUDT*60,dt) - dt
      W0fctr = dt
      W0den = 2 * MAX(CUDT*60,dt)
   else
      W0AVGfctr = (TST-1.)
      W0fctr = 1.
      W0den = TST
   endif

  DO J = jts,jte
      DO K=kts,kte
         DO I= its,ite
!            SCR1=-5.0E-4*G*rho(I,K,J)*(w(I,K,J)+w(I,K+1,J))
!            TV=T(I,K,J)*(1.+EP1*QV(I,K,J))
!            RHOE=Pcps(I,K,J)/(R*TV)
!            W0=-101.9368*SCR1/RHOE
            W0=0.5*(w(I,K,J)+w(I,K+1,J))

!           Old:            
!
!            W0AVG(I,K,J)=(W0AVG(I,K,J)*(TST-1.)+W0)/TST            
!
!           New, to support adaptive time step:
!
            W0AVG(I,K,J) = ( W0AVG(I,K,J) * W0AVGfctr + W0 * W0fctr ) / W0den
!ckaywup
!           w(I,K,J)=w(I,K,J)+w_up(i,K,j)  

         ENDDO
      ENDDO
   ENDDO


   lastdt = dt

! New trigger function
     IF (trigger.eq.2) THEN         
!
! calculate 9-point average of moisture advection and temperature using halo (Horizontal)
!
     aveh_t=-999   ! horizontal 9-point ave
     aveh_q=-999
     avev_t=0   ! vertical 3-level ave
     avev_q=0
     avev_qmax=0
     avev_qmin=0
     aveh_qmax=0
     aveh_qmin=0
     tpart_h=0
     tpart_v=0
     coef_h=0
     coef_v=0
     ibegh=max(its-1, ids+1)   ! start from 2
     jbegh=max(jts-1, jds+1)
     iendh=min(ite+1, ide-2)   ! end at ide-2
     jendh=min(jte+1, jde-2)
        DO J = jbegh,jendh
        DO K = kts,kte
        DO I = ibegh,iendh
          aveh_t(i,k,j)=(T(i-1,k,j-1)+T(i-1,k,j)  +T(i-1,k,j+1)+ &
                         T(i,k,j-1)   +T(i,k,j)   +T(i,k,j+1)+         &
                         T(i+1,k,j-1) +T(i+1,k,j) +T(i+1,k,j+1))/9.
          aveh_q(i,k,j)=(rqvften(i-1,k,j-1)+rqvften(i-1,k,j)  +rqvften(i-1,k,j+1)+ &
                         rqvften(i,k,j-1)   +rqvften(i,k,j)   +rqvften(i,k,j+1)+         &
                         rqvften(i+1,k,j-1) +rqvften(i+1,k,j) +rqvften(i+1,k,j+1))/9.
        ENDDO
        ENDDO
        ENDDO
! boundary value ( all processors will do the following? Or just those processsors handling sub-area including boundary)
        DO K = kts,kte
           DO J = jts-1,jte+1
            DO I = its-1,ite+1

            if(i.eq.ids) then
            aveh_t(i,k,j)=aveh_t(i+1,k,j)
            aveh_q(i,k,j)=aveh_q(i+1,k,j)
            elseif(i.eq.ide-1) then
            aveh_t(i,k,j)=aveh_t(i-1,k,j)
            aveh_q(i,k,j)=aveh_q(i-1,k,j)
            endif

            if(j.eq.jds) then
             aveh_t(i,k,j)=aveh_t(i,k,j+1)
             aveh_q(i,k,j)=aveh_q(i,k,j+1)
            elseif(j.eq.jde-1) then
            aveh_t(i,k,j)=aveh_t(i,k,j-1)
            aveh_q(i,k,j)=aveh_q(i,k,j-1)
            endif

            if(j.eq.jds.and.i.eq.ids) then
            aveh_q(i,k,j)=aveh_q(i+1,k,j+1) 
            aveh_t(i,k,j)=aveh_t(i+1,k,j+1) 
            endif

            if(j.eq.jde-1.and.i.eq.ids) then
            aveh_q(i,k,j)=aveh_q(i+1,k,j-1) 
            aveh_t(i,k,j)=aveh_t(i+1,k,j-1) 
            endif

            if(j.eq.jde-1.and.i.eq.ide-1) then
            aveh_q(i,k,j)=aveh_q(i-1,k,j-1) 
            aveh_t(i,k,j)=aveh_t(i-1,k,j-1) 
            endif

            if(j.eq.jds.and.i.eq.ide-1) then
            aveh_q(i,k,j)=aveh_q(i-1,k,j+1) 
            aveh_t(i,k,j)=aveh_t(i-1,k,j+1) 
            endif

            ENDDO
           ENDDO
        ENDDO
! search for max/min moisture advection in 9-point range, calculate horizontal T-perturbation (tpart_h)
     istart=max(its, ids+1)   ! start from 2
     jstart=max(jts, jds+1)
     iend=min(ite, ide-2)   ! end at ide-2
     jend=min(jte, jde-2)
        DO K = kts,kte
        DO J = jstart,jend
        DO I = istart,iend
           aveh_qmax(i,k,j)=aveh_q(i,k,j)
           aveh_qmin(i,k,j)=aveh_q(i,k,j)
          DO ii=-1, 1
           DO jj=-1,1
             if(aveh_q(i+II,k,j+JJ).gt.aveh_qmax(i,k,j)) aveh_qmax(i,k,j)=aveh_q(i+II,k,j+JJ)
             if(aveh_q(i+II,k,j+JJ).lt.aveh_qmin(i,k,j)) aveh_qmin(i,k,j)=aveh_q(i+II,k,j+JJ)
           ENDDO
          ENDDO 
          if(aveh_qmax(i,k,j).gt.aveh_qmin(i,k,j))then
          coef_h(i,k,j)=(aveh_q(i,k,j)-aveh_qmin(i,k,j))/(aveh_qmax(i,k,j)-aveh_qmin(i,k,j))
          else
          coef_h(i,k,j)=0.
          endif
          coef_h(i,k,j)=amin1(coef_h(i,k,j),1.0)
          coef_h(i,k,j)=amax1(coef_h(i,k,j),0.0)
          tpart_h(i,k,j)=coef_h(i,k,j)*(T(i,k,j)-aveh_t(i,k,j))
        ENDDO
        ENDDO
        ENDDO
    89 continue 
! vertical 3-layer calculation
        DO J = jts, jte
        DO I = its, ite
          z0(1) = 0.5 * dz8w(i,1,j)
          DO K = 2, kte
            Z0(K) = Z0(K-1) + .5 * (DZ8W(i,K,j) + DZ8W(i,K-1,j))
          ENDDO
        DO K = kts+1,kte-1
	  ttop = t(i,k,j) + ((t(i,k,j) - t(i,k+1,j)) / (z0(k) - z0(k+1))) * (z0(k)-z0(k-1))
          avev_t(i,k,j)=(T(i,k-1,j) + T(i,k,j) + ttop)/3.
!         avev_t(i,k,j)=(T(i,k-1,j)+T(i,k,j) + T(i,k+1,j))/3.
          avev_q(i,k,j)=(rqvften(i,k-1,j)+rqvften(i,k,j) + rqvften(i,k+1,j))/3.
        ENDDO
          avev_t(i,kts,j)=avev_t(i,kts+1,j)   ! lowest level value, is it the same as avev_t(i,kds,j)=avev_t(i,kds+1,j)?
          avev_q(i,kts,j)=avev_q(i,kts+1,j)   
          avev_t(i,kte,j)=avev_t(i,kte-1,j)   ! highest level value
          avev_q(i,kte,j)=avev_q(i,kte-1,j)   
        ENDDO
        ENDDO
! max /min value
        DO J = jts, jte
        DO I = its, ite
        DO K = kts+1,kte-1
          avev_qmax(i,k,j)=avev_q(i,k,j)
          avev_qmin(i,k,j)=avev_q(i,k,j)
         DO kk=-1,1
         if(avev_q(i,k+kk,j).gt.avev_qmax(i,k,j)) avev_qmax(i,k,j)=avev_q(i,k+kk,j) 
         if(avev_q(i,k+kk,j).lt.avev_qmin(i,k,j)) avev_qmin(i,k,j)=avev_q(i,k+kk,j) 
         ENDDO
         if(avev_qmax(i,k,j).gt.avev_qmin(i,k,j)) then
         coef_v(i,k,j)=(avev_q(i,k,j)-avev_qmin(i,k,j))/(avev_qmax(i,k,j)-avev_qmin(i,k,j))
         else
         coef_v(i,k,j)=0
         endif
         tpart_v(i,k,j)=coef_v(i,k,j)*(T(i,k,j)-avev_t(i,k,j))
        ENDDO
         tpart_v(i,kts,j)= tpart_v(i,kts+1,j)    ! lowest level
         tpart_v(i,kte,j)= tpart_v(i,kte-1,j)    ! highest level
        ENDDO
        ENDDO
     ENDIF       ! endif (trigger.eq.2)          
!
     DO J = jts,jte
     DO I= its,ite
        CU_ACT_FLAG(i,j) = .true.
     ENDDO
     ENDDO

     DO J = jts,jte
       DO I=its,ite
          

         IF ( NCA(I,J) .ge. 0.5*DT ) then
            CU_ACT_FLAG(i,j) = .false.
         ELSE

            DO k=kts,kte
               DQDT(k)=0.
               DQIDT(k)=0.
               DQCDT(k)=0.
               DQRDT(k)=0.
               DQSDT(k)=0.
               DTDT(k)=0.
!ckay
               cldfra_dp_KF(I,k,J)=0.
               cldfra_sh_KF(I,k,J)=0.
               qc_KF(I,k,J)=0.
               qi_KF(I,k,J)=0.
             IF (aercu_opt.gt.0) THEN
               qr_KF(I,k,J)=0.
               qs_KF(I,k,J)=0.
               nc_KF(I,k,J)=0.
               ni_KF(I,k,J)=0.
               nr_KF(I,k,J)=0.
               ns_KF(I,k,J)=0.
               ccn_KF(I,k,J)=0.
               EFSS(I,k,J)=10.01
               EFCS(I,k,J)=2.51
               EFIS(I,k,J)=5.01
              END IF
               w_up(I,k,J)=0.
            ENDDO
             IF (aercu_opt.gt.0) THEN
               ainc_frac(I,J) = 0. ! TWG
             END IF
            IF (KF_EDRATES == 1) THEN
               DO k=kts,kte
                  UDR_KF(I,k,J)=0.
                  DDR_KF(I,k,J)=0.
                  UER_KF(I,k,J)=0.
                  DER_KF(I,k,J)=0.
               ENDDO
               TIMEC_KF(I,J)=0.
            ENDIF
            RAINCV(I,J)=0.
            CUTOP(I,J)=KTS
            CUBOT(I,J)=KTE+1
            PRATEC(I,J)=0.
!
! assign vars from 3D to 1D

            DO K=kts,kte
               U1D(K) =U(I,K,J)
               V1D(K) =V(I,K,J)
               T1D(K) =T(I,K,J)
               RHO1D(K) =rho(I,K,J)
               QV1D(K)=QV(I,K,J)
               P1D(K) =Pcps(I,K,J)
               W0AVG1D(K) =W0AVG(I,K,J)
               DZ1D(k)=dz8w(I,K,J)

               IF (trigger.eq.2) THEN
                  tpart_h1D(K) =tpart_h(I,K,J)
                  tpart_v1D(K) =tpart_v(I,K,J)
               ELSE
                  tpart_h1D(K) = 0.
                  tpart_v1D(K) = 0.
               ENDIF
            ENDDO
!dkay
            IF (aercu_opt.gt.0) THEN
                call mskf_mphyi ()
            END IF

            CALL MSKF_eta_PARA(I, J,                  &
                 U1D,V1D,T1D,QV1D,P1D,DZ1D,W0AVG1D, &
                 tpart_h1D,tpart_v1D,               &
                 trigger,                           &
                 DT,DX,DXSQ,RHO1D,                  &
                 XLV0,XLV1,XLS0,XLS1,CP,R,G,        &
                 EP2,SVP1,SVP2,SVP3,SVPT0,          &
                 DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT, &
                 RAINCV,PRATEC,NCA,                 &
                 flag_QI,flag_QS,warm_rain,         &
                 CUTOP,CUBOT,CUDT,                  &
                 ids,ide, jds,jde, kds,kde,         &
                 ims,ime, jms,jme, kms,kme,         &
                 its,ite, jts,jte, kts,kte,         &
!ckay
                 cldfra_dp_KF,cldfra_sh_KF,w_up,    &
                 qc_KF,qi_KF,qr_KF,qs_KF,           &
                 nc_KF,ni_KF,nr_KF,ns_KF,ccn_KF,    & !TWG
                 ainc_frac,                         & !TWG
!kf_edrates
                 UDR_KF,DDR_KF,                     &
                 UER_KF,DER_KF,                     &
                 TIMEC_KF,KF_EDRATES,               &                 
                 ZOL,WSTAR,UST,PBLH,                &
                 aerocu,no_src_types_cu,aercu_fct,  &
                 aercu_opt,EFCS,EFIS,EFSS) !PSH/TWG

              DO K=kts,kte
                 RTHCUTEN(I,K,J)=DTDT(K)/pi(I,K,J)
                 RQVCUTEN(I,K,J)=DQDT(K)
              ENDDO

              IF( F_QR )THEN
                DO K=kts,kte
                   RQRCUTEN(I,K,J)=DQRDT(K)
                   RQCCUTEN(I,K,J)=DQCDT(K)
                ENDDO
              ELSE
! This is the case for Eta microphysics without 3d rain field
                DO K=kts,kte
                   RQRCUTEN(I,K,J)=0.
                   RQCCUTEN(I,K,J)=DQRDT(K)+DQCDT(K)
                ENDDO
              ENDIF

!......     QSTEN STORES GRAUPEL TENDENCY IF IT EXISTS, OTHERISE SNOW (V2)

              IF ( F_QI ) THEN
                DO K=kts,kte
                   RQICUTEN(I,K,J)=DQIDT(K)
                ENDDO
              ENDIF

              IF ( F_QS ) THEN
                DO K=kts,kte
                   RQSCUTEN(I,K,J)=DQSDT(K)
                ENDDO
              ENDIF
!
         ENDIF 
       ENDDO     ! i-loop
     ENDDO       ! j-loop
!
   END SUBROUTINE MSKF_CPS
! ****************************************************************************
!-----------------------------------------------------------
   SUBROUTINE MSKF_eta_PARA (I, J,                           &
                      U0,V0,T0,QV0,P0,DZQ,W0AVG1D,         &
                      TPART_H0,TPART_V0,                   &
                      trigger,                             &
                      DT,DX,DXSQ,rhoe,                     &
                      XLV0,XLV1,XLS0,XLS1,CP,R,G,          &
                      EP2,SVP1,SVP2,SVP3,SVPT0,            &
                      DQDT,DQIDT,DQCDT,DQRDT,DQSDT,DTDT,   &
                      RAINCV,PRATEC,NCA,                   &
                      F_QI,F_QS,warm_rain,                 &
                      CUTOP,CUBOT,CUDT,                    &
                      ids,ide, jds,jde, kds,kde,           &
                      ims,ime, jms,jme, kms,kme,           &
                      its,ite, jts,jte, kts,kte,           &
!ckay
                      cldfra_dp_KF,cldfra_sh_KF,w_up,      &
                      qc_KF,qi_KF,qr_KF,qs_KF,             & !TWG
                      nc_KF,ni_KF,nr_KF,ns_KF,ccn_KF,      & !TWG
                      ainc_frac,                           & !TWG
!kf_edrates
                      UDR_KF,DDR_KF,                       &
                      UER_KF,DER_KF,                       &
                      TIMEC_KF,KF_EDRATES,                 &
                      ZOL,WSTAR,UST,PBLH,                  &
                      aerocu,no_src_types_cu,aercu_fct,    &
                      aercu_opt,EFCS,EFIS,EFSS) !PSH/TWG

!-----------------------------------------------------------
!***** The KF scheme that is currently used in experimental runs of EMCs 
!***** Eta model....jsk 8/00
!
      IMPLICIT NONE
!     SAVE  !TWG 2017 Add to avoid memory issues
!-----------------------------------------------------------
      INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde, &
                                ims,ime, jms,jme, kms,kme, &
                                its,ite, jts,jte, kts,kte, &
                                I,J
          ! ,P_QI,P_QS,P_FIRST_SCALAR
      INTEGER, INTENT(IN   ) ::  trigger

      LOGICAL, INTENT(IN   ) :: F_QI, F_QS

      LOGICAL, INTENT(IN   ) :: warm_rain
!
      REAL, DIMENSION( kts:kte ),                          &
            INTENT(IN   ) ::                           U0, &
                                                       V0, &
                                                 TPART_H0, &
                                                 TPART_V0, &
                                                       T0, &
                                                      QV0, &
                                                       P0, &
                                                     rhoe, &
                                                      DZQ, &
                                                  W0AVG1D
!
      REAL,  INTENT(IN   ) :: DT,DX,DXSQ
!

      REAL,  INTENT(IN   ) :: XLV0,XLV1,XLS0,XLS1,CP,R,G
      REAL,  INTENT(IN   ) :: EP2,SVP1,SVP2,SVP3,SVPT0

      INTEGER, INTENT(IN   ) :: no_src_types_cu  !PSH/TWG
      REAL,    INTENT(IN   ) :: aercu_fct        !PSH/TWG
      INTEGER, INTENT(IN   ) :: aercu_opt        !PSH/TWG
      REAL,  DIMENSION( ims:ime, kms:kme, jms:jme, no_src_types_cu), OPTIONAL, &
      INTENT(INOUT) ::                                   aerocu !PSH/TWG


!ckay
      REAL, DIMENSION( ims:ime, jms:jme ),                 &
            INTENT(   IN) ::                          ZOL, &
                                                    WSTAR, &
                                                      UST, &
                                                     PBLH
!
      REAL, DIMENSION( kts:kte ), INTENT(INOUT) ::         &
                                                     DQDT, &
                                                    DQIDT, &
                                                    DQCDT, &
                                                    DQRDT, &
                                                    DQSDT, &
                                                     DTDT

      REAL,    DIMENSION( ims:ime , jms:jme ),             &
            INTENT(INOUT) ::                          NCA

      REAL,    DIMENSION( ims:ime , jms:jme ),             & !TWG
            INTENT(INOUT) ::                      ainc_frac

!ckay
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),      &
            INTENT(INOUT) ::                 cldfra_dp_KF, &
                                             cldfra_sh_KF, &
                                                    qc_KF, &
                                                    qi_KF

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),      &
            INTENT(INOUT) ::                        qr_KF, & !TWG
                                                    qs_KF, & !TWG
                                                    nc_KF, & !TWG
                                                    ni_KF, & !TWG
                                                    nr_KF, & !TWG
                                                    ns_KF, & !TWG
                                                   ccn_KF, & !TWG
                                                     EFCS, & !TWG
                                                     EFIS, & !TWG
                                                     EFSS

!kf_edrates
      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),      &
            INTENT(INOUT) ::       UDR_KF,       &
                                             DDR_KF,       &
                                             UER_KF,       &
                                             DER_KF

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::     TIMEC_KF

      INTEGER, INTENT(IN) ::         KF_EDRATES

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       RAINCV

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(INOUT) ::                       PRATEC

      REAL, DIMENSION( ims:ime , jms:jme ),                &
            INTENT(OUT) ::                          CUBOT, &
                                                    CUTOP
      REAL,  INTENT(IN   ) :: CUDT

!ckaywup
  REAL, DIMENSION( ims:ime,  kms:kme , jms:jme )         , &
        INTENT(  OUT) ::                             w_up                       
!
!...DEFINE LOCAL VARIABLES...
!
      REAL, DIMENSION( kts:kte ) ::                        &
            Q0,Z0,TV0,TU,TVU,QU,TZ,TVD,                    &
            QD,QES,THTES,TG,TVG,QG,WU,WD,W0,EMS,EMSD,      &
            UMF,UER,UDR,DMF,DER,DDR,UMF2,UER2,             &
            UDR2,DMF2,DER2,DDR2,DZA,THTA0,THETEE,          &
            THTAU,THETEU,THTAD,THETED,QLIQ,QICE,           &
!TWG 06/14/16
            QRAIN,QSNOW,NLIQ,NICE,NRAIN,NSNOW,CCN,         &
            EFFCH,EFFIH,EFFSH, &

!EBD TWG
            QLQOUT,QICOUT,PPTLIQ,PPTICE,DETLQ,DETIC,       &
            DETLQ2,DETIC2,RATIO,RATIO2


      REAL, DIMENSION( kts:kte ) ::                        &
            DOMGDP,EXN,TVQU,DP,RH,EQFRC,WSPD,              &
            QDT,FXM,THTAG,THPA,THFXOUT,                    &
            THFXIN,QPA,QFXOUT,QFXIN,QLPA,QLFXIN,           &
            QLFXOUT,QIPA,QIFXIN,QIFXOUT,QRPA,              &
            QRFXIN,QRFXOUT,QSPA,QSFXIN,QSFXOUT,            &
            QL0,QLG,QI0,QIG,QR0,QRG,QS0,QSG


      REAL, DIMENSION( kts:kte+1 ) :: OMG
      REAL, DIMENSION( kts:kte ) :: RAINFB,SNOWFB
      REAL, DIMENSION( kts:kte ) ::                        &
            CLDHGT,QSD,DILFRC,DDILFRC,TKE,TGU,QGU,THTEEG

! LOCAL VARS

      REAL    :: P00,T00,RLF,RHIC,RHBC,PIE,         &
                 TTFRZ,TBFRZ,C5,RATE
      REAL    :: GDRY,ROCP,ALIQ,BLIQ,                      &
                 CLIQ,DLIQ
      REAL    :: FBFRC,P300,DPTHMX,THMIX,QMIX,ZMIX,PMIX,   &
                 ROCPQ,TMIX,EMIX,TLOG,TDPT,TLCL,TVLCL,     &
                 CPORQ,PLCL,ES,DLP,TENV,QENV,TVEN,TVBAR,   &
                 ZLCL,WKL,WABS,TRPPT,WSIGNE,DTLCL,GDT,WLCL,&
                 TVAVG,QESE,WTW,RHOLCL,AU0,VMFLCL,UPOLD,   &
                 UPNEW,ABE,WKLCL,TTEMP,FRC1,   &
                 QNEWIC,RL,R1,QNWFRZ,EFFQ,BE,BOTERM,ENTERM,&
                 DZZ,UDLBE,REI,EE2,UD2,TTMP,F1,F2,         &
                 THTTMP,QTMP,TMPLIQ,TMPICE,TU95,TU10,EE1,  &
                 UD1,DPTT,QNEWLQ,DUMFDP,EE,TSAT,           &
                 THTA,VCONV,TIMEC,SHSIGN,VWS,PEF, &
                 CBH,RCBH,PEFCBH,PEFF,PEFF2,TDER,THTMIN,   &
                 DTMLTD,QS,TADVEC,DPDD,FRC,DPT,RDD,A1,     &
                 DSSDT,DTMP,T1RH,QSRH,PPTFLX,CPR,CNDTNF,   &
                 UPDINC,AINCM2,DEVDMF,PPR,RCED,DPPTDF,     &
                 DMFLFS,DMFLFS2,RCED2,DDINC,AINCMX,AINCM1, &
                 AINC,TDER2,PPTFL2,FABE,STAB,DTT,DTT1,     &
                 DTIME,TMA,TMB,TMM,BCOEFF,ACOEFF,QVDIFF,   &
                 TOPOMG,CPM,DQ,ABEG,DABE,DFDA,FRC2,DR,     &
                 UDFRC,TUC,QGS,RH0,RHG,QINIT,QFNL,ERR2,    &
                 RELERR,RLC,RLS,RNC,FABEOLD,AINCOLD,UEFRC, &
                 DDFRC,TDC,DEFRC,RHBAR,DMFFRC,DPMIN,DILBE
   REAL    ::    ASTRT,TP,VALUE,AINTRP,TKEMAX,QFRZ,&
                 QSS,PPTMLT,DTMELT,RHH,EVAC,BINC
!
      INTEGER :: INDLU,NU,NUCHM,NNN,KLFS
   REAL    :: CHMIN,PM15,CHMAX,DTRH,RAD,DPPP
   REAL    :: TVDIFF,DTTOT,ABSOMG,ABSOMGTC,FRDP
!ckay
   REAL    :: xcldfra,UMF_new,DMF_new,FXM_new
   REAL    :: sourceht, Scale_Fac, TOKIOKA, RATE_kay
   REAL    :: capeDX, tempKay
   REAL    :: SCLvel, ZLCL_KAY, zz_kay

!ckaywup
   REAL    :: envEsat, envQsat, envRH, envRHavg, denSplume
   REAL    :: updil, Drag
!TWG Mar 2017
   REAL, PARAMETER :: P1_HU10 = 7.6725
   REAL, PARAMETER :: P2_HU10 = 1.0118
   REAL, PARAMETER :: P3_HU10 = 0.1422
   REAL, PARAMETER :: P4_HU10 = 0.0106
   REAL, PARAMETER :: P5_HU10 = 3.39E-4
   REAL, PARAMETER :: P6_HU10 = 3.95E-6
   REAL    :: SF_HU10, TC_HU10
!END TWG
!dkay for dccmp
   real :: a1kay
   LOGICAL :: DCCMP
   REAL :: eps1u, alatent, Qsu
   LOGICAL :: onetime
   Data onetime/.true./
   integer, parameter :: r8 = 8
   integer, parameter :: naer_cu = 10
   integer, parameter :: pcols = 1
   REAL(r8) muu(pcols, KTS:KTE)
   REAL(r8) su(pcols, KTS:KTE)
   REAL(r8) quu(pcols, KTS:KTE)
   REAL(r8) duu(pcols, KTS:KTE)
   REAL(r8) euu(pcols, KTS:KTE)
   REAL(r8) cmel(pcols, KTS:KTE)
   REAL(r8) cmei(pcols, KTS:KTE)
   REAL(r8) zfu(pcols, KTS:KTE+1)
   REAL(r8) zf_wrf(0:KTE)
   REAL(r8) pru(pcols, KTS:KTE)
   REAL(r8) tee(pcols, KTS:KTE)
   REAL(r8) qee(pcols, KTS:KTE)
   REAL(r8) qsatzm(pcols, KTS:KTE)
   REAL(r8) gamhat(pcols, KTS:KTE)
   REAL(r8) aer_mmr(pcols, KTS:KTE,naer_cu)
   REAL(r8) Aqnewic(KTS:KTE)
   REAL(r8) Aqnewlq(KTS:KTE)
   REAL(r8) wu_mskf_act(KTS:KTE)
   REAL(r8) qc_mskf_act(KTS:KTE)
   REAL(r8) qi_mskf_act(KTS:KTE)
   REAL(r8) effc(pcols, KTS:KTE)
   REAL(r8) effi(pcols, KTS:KTE)
   REAL(r8) effs(pcols, KTS:KTE)
   real(r8) QSATu(KTS:KTE), oldQU(KTS:KTE),oldTU(KTS:KTE)      ! rate of freezing
   REAL(r8) EPSI0(pcols)
   REAL(r8) dLfmzmp(pcols,KTS:KTE),dIfmzmp(pcols,KTS:KTE)
!junk
   REAL(r8) oldpptliq(KTS:KTE)
   REAL(r8) oldpptice(KTS:KTE)

   REAL(r8) wump(pcols, KTS:KTE)
   real(r8) zmqliq(pcols,KTS:KTE)       ! cloud water mixing ratio (kg/kg)
   real(r8) zmqice(pcols,KTS:KTE)       ! cloud ice mixing ratio (kg/kg)
   real(r8) zmqrain(pcols,KTS:KTE)      ! rain mixing ratio (kg/kg) !TWG
   real(r8) zmqsnow(pcols,KTS:KTE)      ! snow mixing ratio (kg/kg) !TWG
   real(r8) ncmp(pcols,KTS:KTE)       ! cloud water number conc (1/kg)
   real(r8) nimp(pcols,KTS:KTE)       ! cloud ice number conc (1/kg)
   real(r8) nrmp(pcols,KTS:KTE)       ! rain number conc (1/kg) !TWG
   real(r8) nsmp(pcols,KTS:KTE)       ! snow number conc (1/kg) !TWG
   real(r8) zmccn(pcols,KTS:KTE)       ! ccn conc (1/kg) !TWG
   real(r8) rprd(pcols,KTS:KTE)     ! rate of production of precip at that layer
   real(r8) sprd(pcols,KTS:KTE)     ! rate of production of snow at that layer
   real(r8) frz(pcols,KTS:KTE)      ! rate of freezing

   REAL(r8) grav, Rdry , DTZMP, CPIN, psh_fac

   Integer KQ, JK, JBB(1), JTT(1), JLCL(1), msg1, il2g , JZM, KA
   Integer NLEVZM, NLEVZMP1, KKAY, Miter, Itest, KC
!dkay

      INTEGER :: KX,K,KL
!
      INTEGER :: NCHECK
      INTEGER, DIMENSION (kts:kte) :: KCHECK

      INTEGER :: ISTOP,ML,L5,KMIX,LOW,                     &
                 LC,MXLAYR,LLFC,NLAYRS,NK,                 &
                 KPBL,KLCL,LCL,LET,IFLAG,                  &
                 NK1,LTOP,NJ,LTOP1,                        &
                 LTOPM1,LVF,KSTART,KMIN,LFS,               &
                 ND,NIC,LDB,LDT,ND1,NDK,                   &
                 NM,LMAX,NCOUNT,NOITR,                     &
                 NSTEP,NTC,NCHM,ISHALL,NSHALL
      LOGICAL :: IPRNT
      REAL :: u00,qslcl,rhlcl,dqssdt    !jfb
      CHARACTER*1024 message
!
      DATA P00,T00/1.E5,273.16/
      DATA RLF/3.339E5/
      DATA RHIC,RHBC/1.,0.90/
      DATA PIE,TTFRZ,TBFRZ,C5/3.141592654,268.16,248.16,1.0723E-3/
      DATA RATE/0.03/   ! wrf default
!      DATA RATE/0.01/  ! value used in NRCM
!      DATA RATE/0.001/  ! effectively turn off autoconversion
!-----------------------------------------------------------
   IF (aercu_opt.gt.0) THEN
     DCCMP = .TRUE.
   ELSE
     DCCMP = .FALSE.
   END IF

      IPRNT=.FALSE.
      GDRY=-G/CP
      ROCP=R/CP
      NSHALL = 0
      KL=kte
      KX=kte
!
!     ALIQ = 613.3
!     BLIQ = 17.502
!     CLIQ = 4780.8
!     DLIQ = 32.19
      ALIQ = SVP1*1000.
      BLIQ = SVP2
      CLIQ = SVP2*SVPT0
      DLIQ = SVP3
!
      IF(DX.GE.24.999E3) THEN
         Scale_Fac = 1.0
         capeDX = 0.1
      ELSE
         Scale_Fac = 1.0 + (log(25.E3/DX))
         capeDX = 0.1 *SQRT(Scale_Fac)
      END IF
!
!****************************************************************************
!                                                      ! PPT FB MODS
!...OPTION TO FEED CONVECTIVELY GENERATED RAINWATER    ! PPT FB MODS
!...INTO GRID-RESOLVED RAINWATER (OR SNOW/GRAUPEL)     ! PPT FB MODS
!...FIELD.  "FBFRC" IS THE FRACTION OF AVAILABLE       ! PPT FB MODS
!...PRECIPITATION TO BE FED BACK (0.0 - 1.0)...        ! PPT FB MODS
      FBFRC=0.0                                        ! PPT FB MODS
!...mods to allow shallow convection...
      NCHM = 0
      ISHALL = 0
      DPMIN = 5.E3
!...
      P300=P0(1)-30000.
!
!...PRESSURE PERTURBATION TERM IS ONLY DEFINED AT MID-POINT OF
!...VERTICAL LAYERS...SINCE TOTAL PRESSURE IS NEEDED AT THE TOP AND
!...BOTTOM OF LAYERS BELOW, DO AN INTERPOLATION...
!
!...INPUT A VERTICAL SOUNDING ... NOTE THAT MODEL LAYERS ARE NUMBERED
!...FROM BOTTOM-UP IN THE KF SCHEME...
!
      ML=0 
!SUE  tmprpsb=1./PSB(I,J)
!SUE  CELL=PTOP*tmprpsb
!
      DO K=1,KX
!
!  Saturation vapor pressure (ES) is calculated following Buck (1981)
!...IF Q0 IS ABOVE SATURATION VALUE, REDUCE IT TO SATURATION LEVEL...
!
         ES=ALIQ*EXP((BLIQ*T0(K)-CLIQ)/(T0(K)-DLIQ))
         QES(K)=0.622*ES/(P0(K)-ES)
         Q0(K)=AMIN1(QES(K),QV0(K))
         Q0(K)=AMAX1(0.000001,Q0(K))
         QL0(K)=0.
         QI0(K)=0.
         QR0(K)=0.
         QS0(K)=0.
         RH(K) = Q0(K)/QES(K)
         DILFRC(K) = 1.
         TV0(K)=T0(K)*(1.+0.608*Q0(K))
!        RHOE(K)=P0(K)/(R*TV0(K))
!   DP IS THE PRESSURE INTERVAL BETWEEN FULL SIGMA LEVELS...
         DP(K)=rhoe(k)*g*DZQ(k)
! IF Turbulent Kinetic Energy (TKE) is available from turbulent mixing scheme
! use it for shallow convection...For now, assume it is not available....
!         TKE(K) = Q2(I,J,NK)
         TKE(K) = 0.
         CLDHGT(K) = 0.
!        IF(P0(K).GE.500E2)L5=K
         IF(P0(K).GE.0.5*P0(1))L5=K
         IF(P0(K).GE.P300)LLFC=K
      ENDDO
!
!...DZQ IS DZ BETWEEN SIGMA SURFACES, DZA IS DZ BETWEEN MODEL HALF LEVEL
        Z0(1)=.5*DZQ(1)
!cdir novector
        DO K=2,KL
          Z0(K)=Z0(K-1)+.5*(DZQ(K)+DZQ(K-1))
          DZA(K-1)=Z0(K)-Z0(K-1)
        ENDDO   
        DZA(KL)=0.
!
!
!  To save time, specify a pressure interval to move up in sequential
!  check of different ~50 mb deep groups of adjacent model layers in
!  the process of identifying updraft source layer (USL).  Note that 
!  this search is terminated as soon as a buoyant parcel is found and 
!  this parcel can produce a cloud greater than specifed minimum depth
!  (CHMIN)...For now, set interval at 15 mb...
!
       NCHECK = 1
       KCHECK(NCHECK)=1
       PM15 = P0(1)-15.E2
       DO K=2,LLFC
         IF(P0(K).LT.PM15)THEN
           NCHECK = NCHECK+1
           KCHECK(NCHECK) = K
           PM15 = PM15-15.E2
         ENDIF
       ENDDO
!
       NU=0
       NUCHM=0
usl:   DO
           NU = NU+1
           IF(NU.GT.NCHECK)THEN 
             IF(ISHALL.EQ.1)THEN
               CHMAX = 0.
               NCHM = 0
               DO NK = 1,NCHECK
                 NNN=KCHECK(NK)
                 IF(CLDHGT(NNN).GT.CHMAX)THEN
                   NCHM = NNN
                   NUCHM = NK
                   CHMAX = CLDHGT(NNN)
                 ENDIF
               ENDDO
               NU = NUCHM-1
               FBFRC=1.
               CYCLE usl
             ELSE
               RETURN
             ENDIF
           ENDIF      
           KMIX = KCHECK(NU)
           LOW=KMIX
!...
           LC = LOW
!
!...ASSUME THAT IN ORDER TO SUPPORT A DEEP UPDRAFT YOU NEED A LAYER OF
!...UNSTABLE AIR AT LEAST 50 mb DEEP...TO APPROXIMATE THIS, ISOLATE A
!...GROUP OF ADJACENT INDIVIDUAL MODEL LAYERS, WITH THE BASE AT LEVEL
!...LC, SUCH THAT THE COMBINED DEPTH OF THESE LAYERS IS AT LEAST 50 mb..
!   
           NLAYRS=0
           DPTHMX=0.
           NK=LC-1
           IF ( NK+1 .LT. KTS ) THEN
             WRITE(message,*)'WOULD GO OFF BOTTOM: MSKF_PARA I,J,NK',I,J,NK
             CALL wrf_message (TRIM(message)) 
           ELSE
             DO 
               NK=NK+1   
               IF ( NK .GT. KTE ) THEN
                 WRITE(message,*)'WOULD GO OFF TOP: MSKF_PARA I,J,DPTHMX,DPMIN',I,J,DPTHMX,DPMIN
                 CALL wrf_message (TRIM(message))
                 EXIT
               ENDIF
               DPTHMX=DPTHMX+DP(NK)
               NLAYRS=NLAYRS+1
               IF(DPTHMX.GT.DPMIN)THEN
                 EXIT 
               ENDIF
             END DO    
           ENDIF
           IF(DPTHMX.LT.DPMIN)THEN 
             RETURN
           ENDIF
           KPBL=LC+NLAYRS-1   
!
!...********************************************************
!...for computational simplicity without much loss in accuracy,
!...mix temperature instead of theta for evaluating convective
!...initiation (triggering) potential...
!          THMIX=0.
           TMIX=0.
           QMIX=0.
           ZMIX=0.
           PMIX=0.
!
!...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY
!...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL
!...LAYERS...
!
!cdir novector
           DO NK=LC,KPBL
             TMIX=TMIX+DP(NK)*T0(NK)
             QMIX=QMIX+DP(NK)*Q0(NK)
             ZMIX=ZMIX+DP(NK)*Z0(NK)
             PMIX=PMIX+DP(NK)*P0(NK)
           ENDDO   
!         THMIX=THMIX/DPTHMX
          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ZMIX=ZMIX/DPTHMX
          PMIX=PMIX/DPTHMX
          EMIX=QMIX*PMIX/(0.622+QMIX)
!
!...FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL...
!
!        TLOG=ALOG(EMIX/ALIQ)
! ...calculate dewpoint using lookup table...
!
          astrt=1.e-3
          ainc=0.075
          a1=emix/aliq
          tp=(a1-astrt)/ainc
          indlu=int(tp)+1
          value=(indlu-1)*ainc+astrt
          aintrp=(a1-value)/ainc
          tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
          TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
          TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
          TLCL=AMIN1(TLCL,TMIX)
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
     !     NK = LC-1
     !     DO 
     !       NK = NK+1
     !       KLCL=NK
     !       IF(ZLCL.LE.Z0(NK) .or. NK.GT.KL)THEN
     !         EXIT
     !       ENDIF 
     !     ENDDO   
     !     IF(NK.GT.KL)THEN
     !       RETURN  
     !     ENDIF

       DO NK = LC, KL
         KLCL = NK
         IF ( ZLCL.LE.Z0(NK) )  EXIT
     END DO
     IF ( ZLCL.GT.Z0(KL) )  RETURN

          K=KLCL-1
! calculate DLP using Z instead of log(P)
          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))
!     
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!     
          TENV=T0(K)+(T0(KLCL)-T0(K))*DLP
          QENV=Q0(K)+(Q0(KLCL)-Q0(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)
!     
! ww: this needs to be initialized
          DTRH = 0.

! Bechtold 2001 trigger with my Beta parameter
           DTLCL = W0AVG1D(KLCL)/Scale_Fac
           if(DTLCL.lt.0.0) then
              tempKay = -1.0
              DTLCL = tempKay * DTLCL
              DTLCL = (DTLCL)**0.3333
           else
              tempKay = 1.0
              DTLCL = tempKay * DTLCL
              DTLCL = (DTLCL)**0.3333
           end if

           DTLCL = 6.0 * tempKay * DTLCL 
!
! old trigger
! Stick with the old trigger for now... CGM July 2015
!
          IF(ZLCL.LT.2.E3)THEN        ! Kain (2004) Eq. 2
            WKLCL=0.02*ZLCL/2.E3
          ELSE
            WKLCL=0.02                ! units of m/s
          ENDIF
!TWG.ckay c
         if(DX.GE.25.E3) then
           WKL=(W0AVG1D(K)+(W0AVG1D(KLCL)-W0AVG1D(K))*DLP)*DX/25.E3-WKLCL
         else
           WKL=(W0AVG1D(K)+(W0AVG1D(KLCL)-W0AVG1D(K))*DLP)-WKLCL
         end if 
!TWG ckay, Modified WKL
          IF(WKL.LT.0.0001)THEN
            DTLCL=0.
          ELSE
            DTLCL=4.64*WKL**0.33      ! Kain (2004) Eq. 1
          ENDIF


!         IF(ISHALL.EQ.1)IPRNT=.TRUE.
!         IPRNT=.TRUE.
!         IF(TLCL+DTLCL.GT.TENV)GOTO 45

           IF(TLCL+DTLCL.LT.TENV)THEN
!
! Parcel not buoyant, CYCLE back to start of trigger and evaluate next potential
! USL...
!
            CYCLE usl
!
          ELSE                            ! Parcel is buoyant, determine updraft
!     
!...CONVECTIVE TRIGGERING CRITERIA HAS BEEN SATISFIED...COMPUTE
!...EQUIVALENT POTENTIAL TEMPERATURE
!...(THETEU) AND VERTICAL VELOCITY OF THE RISING PARCEL AT THE LCL...
!     
            CALL ENVIRTHT(PMIX,TMIX,QMIX,THETEU(K),ALIQ,BLIQ,CLIQ,DLIQ)
!
!...modify calculation of initial parcel vertical velocity...jsk 11/26/97
!
            DTTOT = DTLCL+DTRH
            IF(DTTOT.GT.1.E-4)THEN
              GDT=2.*G*DTTOT*500./TVEN     ! Kain (2004) Eq. 3  (sort of)
              WLCL=1.+0.5*SQRT(GDT)
              WLCL = AMIN1(WLCL,3.)
            ELSE
              WLCL=1.
            ENDIF
            PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
            WTW=WLCL*WLCL
!
            TVLCL=TLCL*(1.+0.608*QMIX)
            RHOLCL=PLCL/(R*TVLCL)
!        
            LCL=KLCL
            LET=LCL
!ckay
! new formulation based on the LCL replacing the cloud radius concept
!introduce LCL instead of RAD based on WKL here
            RAD = ZLCL
!ckay Dec20
            sourceht = Z0(KPBL)
            RAD = amax1(sourceht, RAD)

            RAD = AMIN1(4000.,RAD)  ! max trap
            RAD = AMAX1(500.,RAD)  ! min trap
!     
!*******************************************************************
!                                                                  *
!                 COMPUTE UPDRAFT PROPERTIES                       *
!                                                                  *
!*******************************************************************
!     
!     
!...
!...ESTIMATE INITIAL UPDRAFT MASS FLUX (UMF(K))...
!     
            WU(K)=WLCL
            AU0=0.01*DXSQ
            UMF(K)=RHOLCL*AU0

            VMFLCL=UMF(K)
            UPOLD=VMFLCL
            UPNEW=UPOLD
!     
!...RATIO2 IS THE DEGREE OF GLACIATION IN THE CLOUD (0 TO 1),
!...UER IS THE ENVIR ENTRAINMENT RATE, ABE IS AVAILABLE
!...BUOYANT ENERGY, TRPPT IS THE TOTAL RATE OF PRECIPITATION
!...PRODUCTION...
!     
            RATIO2(K)=0.
            UER(K)=0.
            ABE=0.
            TRPPT=0.
            TU(K)=TLCL
            TVU(K)=TVLCL
            QU(K)=QMIX
            EQFRC(K)=1.
            QLIQ(K)=0.
            QICE(K)=0.
         IF (aercu_opt .GT. 0) THEN
            QRAIN(K)=0.
            QSNOW(K)=0.
            NLIQ(K)=0.
            NICE(K)=0.
            NRAIN(K)=0.
            NSNOW(K)=0.
            CCN(K)=0.
            EFFCH(K) = 2.5
            EFFIH(K) = 4.99
            EFFSH(K) = 9.99
         END IF
            QLQOUT(K)=0.
            QICOUT(K)=0.
            DETLQ(K)=0.
            DETIC(K)=0.
            PPTLIQ(K)=0.
            PPTICE(K)=0.
            IFLAG=0
!     
!...TTEMP IS USED DURING CALCULATION OF THE LINEAR GLACIATION
!...PROCESS; IT IS INITIALLY SET TO THE TEMPERATURE AT WHICH
!...FREEZING IS SPECIFIED TO BEGIN.  WITHIN THE GLACIATION
!...INTERVAL, IT IS SET EQUAL TO THE UPDRAFT TEMP AT THE
!...PREVIOUS MODEL LEVEL...
!     
            TTEMP=TTFRZ
!     
!...ENTER THE LOOP FOR UPDRAFT CALCULATIONS...CALCULATE UPDRAFT TEMP,
!...MIXING RATIO, VERTICAL MASS FLUX, LATERAL DETRAINMENT OF MASS AND
!...MOISTURE, PRECIPITATION RATES AT EACH MODEL LEVEL...
!     
!    **1 variables indicate the bottom of a model layer
!    **2 variables indicate the top of a model layer
!     
            EE1=1.
            UD1=0.
            REI = 0.
            DILBE = 0.

!dkay
      IF (aercu_opt.gt.0) THEN
                zf_wrf(0) = 0.0  ! ground
                DO KQ=KTS,KTE
                 zf_wrf(KQ) = zf_wrf(KQ-1)+DZQ(KQ)
                 Aqnewlq(kq) = 0.0
                 Aqnewic(kq) = 0.0
                 rprd(1,kq) = 0.0
                 wump(1,kq) =0.0
                 ncmp(1,kq) =0.0
                 nimp(1,kq) =0.0
                 sprd(1,kq) =0.0
                 frz(1,kq) =0.0
                 jk = kq
                 muu(1,JK) = 0.0
                 duu(1,JK) =0.0
                 EUU(1,JK) =0.0
                 cmel(1,JK) =0.0
                 cmei(1,JK) =0.0
                 oldTU(kq) = t0(kq)
                 oldQU(kq) = Q0(kq)
                End do
              Miter = 0
      END IF

updraft:    DO NK=K,KL-1
              NK1=NK+1
              RATIO2(NK1)=RATIO2(NK)
              FRC1=0.
              TU(NK1)=T0(NK1)
              THETEU(NK1)=THETEU(NK)
              QU(NK1)=QU(NK)
!dkay
     IF (aercu_opt.gt.0) THEN
              oldQU(NK) = QU(NK)
              oldTU(NK) = TU(NK)
     END IF
!dkay
              QLIQ(NK1)=QLIQ(NK)
              QICE(NK1)=QICE(NK)
              call tpmix2(p0(nk1),theteu(nk1),tu(nk1),qu(nk1),qliq(nk1),        &
                     qice(nk1),qnewlq,qnewic,XLV1,XLV0,QSu)

!dkay QSu has been added to the tpmix2

!dkay
! saturation value of Q of updraft for use with gamma hat in DCCMP routine
!   IF (aercu_opt.gt.0) THEN
!        QSATu(NK) = QSu/(1.+QSu)   ! saturated specific hum
!         Aqnewlq(NK) = qnewlq
!         Aqnewic(NK) = qnewic

!        Aqnewlq(NK) = qnewlq + Qliq(nk )   This is to be removed
!        Aqnewic(NK) = qnewic + Qice(nk )

!dkaydec26
     !    if(TU(NK).le.273.) then
     !     Aqnewlq(NK) = 0.0
     !     Aqnewic(NK) = qnewlq + qnewic
      !   else
       !   Aqnewlq(NK) = qnewlq + qnewic
        !  Aqnewic(NK) = 0.0
        ! end if
!   END IF
!
!
!...CHECK TO SEE IF UPDRAFT TEMP IS ABOVE THE TEMPERATURE AT WHICH
!/dec26...GLACIATION IS ASSUMED TO INITIATE; IF IT IS, CALCULATE THE
!...FRACTION OF REMAINING LIQUID WATER TO FREEZE...TTFRZ IS THE
!...TEMP AT WHICH FREEZING BEGINS, TBFRZ THE TEMP BELOW WHICH ALL
!...LIQUID WATER IS FROZEN AT EACH LEVEL...
!
              IF(TU(NK1).LE.TTFRZ)THEN
                IF(TU(NK1).GT.TBFRZ)THEN
                  IF(TTEMP.GT.TTFRZ)TTEMP=TTFRZ
                  FRC1=(TTEMP-TU(NK1))/(TTEMP-TBFRZ)
                ELSE
                  FRC1=1.
                  IFLAG=1
                ENDIF
                TTEMP=TU(NK1)
                !print*,'OLD FRC1',FRC1

!TWG Mar 2017
!Refine FRC1 to match super cooled water path estimate from Hu et al. [2010]
           IF (aercu_opt.gt.0) THEN
            IF(TU(NK1).GT.TBFRZ)THEN
             TC_HU10 = TU(NK1)-273.15
             SF_HU10 = -1.0*(P1_HU10+(P2_HU10*TC_HU10)+(P3_HU10*(TC_HU10**2))+ &
             (P4_HU10*(TC_HU10**3))+(P5_HU10*(TC_HU10**4))+(P6_HU10*(TC_HU10**5)))
             FRC1 = 1.0 - (1.0/(1.0 + EXP(SF_HU10)))
            ELSE
             FRC1=1.
             IFLAG=1
            ENDIF
           END IF
!END TWG 
!
!  DETERMINE THE EFFECTS OF LIQUID WATER FREEZING WHEN TEMPERATURE
!...IS BELOW TTFRZ...
!
                QFRZ = (QLIQ(NK1)+QNEWLQ)*FRC1
                QNEWIC=QNEWIC+QNEWLQ*FRC1
                QNEWLQ=QNEWLQ-QNEWLQ*FRC1
                QICE(NK1) = QICE(NK1)+QLIQ(NK1)*FRC1
                QLIQ(NK1) = QLIQ(NK1)-QLIQ(NK1)*FRC1
                CALL DTFRZNEW(TU(NK1),P0(NK1),THETEU(NK1),QU(NK1),QFRZ,         &
                          QICE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
              ENDIF
              TVU(NK1)=TU(NK1)*(1.+0.608*QU(NK1))

   IF (aercu_opt.gt.0) THEN
        QSATu(NK) = QSu/(1.+QSu)   ! saturated specific hum
         Aqnewlq(NK) = qnewlq
         Aqnewic(NK) = qnewic
   END IF

!
!  CALCULATE UPDRAFT VERTICAL VELOCITY AND PRECIPITATION FALLOUT...
!
              IF(NK.EQ.K)THEN
                BE=(TVLCL+TVU(NK1))/(TVEN+TV0(NK1))-1.
                BOTERM=2.*(Z0(NK1)-ZLCL)*G*BE/1.5
                DZZ=Z0(NK1)-ZLCL
              ELSE
                BE=(TVU(NK)+TVU(NK1))/(TV0(NK)+TV0(NK1))-1.
                BOTERM=2.*DZA(NK)*G*BE/1.5
                DZZ=DZA(NK)
              ENDIF
              ENTERM=2.*REI*WTW/UPOLD
!
! ckay
! using corrected RATE_kay for Test simulation #2... CGM July 2015
!
              IF(DX.GE.24.999E3) then
                RATE_kay = RATE
              else
                RATE_kay = RATE / Scale_Fac 
               end if
              CALL CONDLOAD(QLIQ(NK1),QICE(NK1),WTW,DZZ,BOTERM,ENTERM,      &
                   RATE_kay,QNEWLQ,QNEWIC,QLQOUT(NK1),QICOUT(NK1),G)

!
!...IF VERT VELOCITY IS LESS THAN ZERO, EXIT THE UPDRAFT LOOP AND,
!...IF CLOUD IS TALL ENOUGH, FINALIZE UPDRAFT CALCULATIONS...
!
              IF(WTW.LT.1.E-3)THEN
                EXIT
              ELSE
                WU(NK1)=SQRT(WTW)
              ENDIF
!...Calculate value of THETA-E in environment to entrain into updraft...
!
              CALL ENVIRTHT(P0(NK1),T0(NK1),Q0(NK1),THETEE(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
!
!...REI IS THE RATE OF ENVIRONMENTAL INFLOW...
! New formulation for entrainment
!ckay introduce DX dependcy for the TOKIOKA Parameter =0.03
!ckay Kim et al 2011; Kang et al 2009; Lin et al 2013;  GCM findings

              TOKIOKA = 0.03
              TOKIOKA = TOKIOKA * Scale_Fac
              REI=VMFLCL*DP(NK1)*TOKIOKA/RAD         
!ckay
              TVQU(NK1)=TU(NK1)*(1.+0.608*QU(NK1)-QLIQ(NK1)-QICE(NK1))
              IF(NK.EQ.K)THEN
                DILBE=((TVLCL+TVQU(NK1))/(TVEN+TV0(NK1))-1.)*DZZ
              ELSE
                DILBE=((TVQU(NK)+TVQU(NK1))/(TV0(NK)+TV0(NK1))-1.)*DZZ
              ENDIF
              IF(DILBE.GT.0.)ABE=ABE+DILBE*G
!
!...IF CLOUD PARCELS ARE VIRTUALLY COLDER THAN THE ENVIRONMENT, MINIMAL 
!...ENTRAINMENT (0.5*REI) IS IMPOSED...
!
              IF(TVQU(NK1).LE.TV0(NK1))THEN    ! Entrain/Detrain IF BLOCK
                EE2=0.5       ! Kain (2004)  Eq. 4
                UD2=1.
                EQFRC(NK1)=0.
              ELSE
                LET=NK1
                TTMP=TVQU(NK1)
!
!...DETERMINE THE CRITICAL MIXED FRACTION OF UPDRAFT AND ENVIRONMENTAL AIR...
!
                F1=0.95
                F2=1.-F1
                THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                QTMP=F1*Q0(NK1)+F2*QU(NK1)
                TMPLIQ=F2*QLIQ(NK1)
                TMPICE=F2*QICE(NK1)
                call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                           qnewlq,qnewic,XLV1,XLV0,QSu)
                TU95=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                IF(TU95.GT.TV0(NK1))THEN
                  EE2=1.
                  UD2=0.
                  EQFRC(NK1)=1.0
                ELSE
                  F1=0.10
                  F2=1.-F1
                  THTTMP=F1*THETEE(NK1)+F2*THETEU(NK1)
                  QTMP=F1*Q0(NK1)+F2*QU(NK1)
                  TMPLIQ=F2*QLIQ(NK1)
                  TMPICE=F2*QICE(NK1)
                  call tpmix2(p0(nk1),thttmp,ttmp,qtmp,tmpliq,tmpice,        &
                               qnewlq,qnewic,XLV1,XLV0,QSu)
                  TU10=TTMP*(1.+0.608*QTMP-TMPLIQ-TMPICE)
                  TVDIFF = ABS(TU10-TVQU(NK1))
                  IF(TVDIFF.LT.1.e-3)THEN
                    EE2=1.
                    UD2=0.
                    EQFRC(NK1)=1.0
                  ELSE
                    EQFRC(NK1)=(TV0(NK1)-TVQU(NK1))*F1/(TU10-TVQU(NK1))
                    EQFRC(NK1)=AMAX1(0.0,EQFRC(NK1))
                    EQFRC(NK1)=AMIN1(1.0,EQFRC(NK1))
                    IF(EQFRC(NK1).EQ.1)THEN
                      EE2=1.
                      UD2=0.
                    ELSEIF(EQFRC(NK1).EQ.0.)THEN
                      EE2=0.
                      UD2=1.
                    ELSE
!
!...SUBROUTINE PROF5 INTEGRATES OVER THE GAUSSIAN DIST TO DETERMINE THE
!   FRACTIONAL ENTRAINMENT AND DETRAINMENT RATES...
!
                      CALL PROF5(EQFRC(NK1),EE2,UD2)
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF                            ! End of Entrain/Detrain IF BLOCK
!
!
!...NET ENTRAINMENT AND DETRAINMENT RATES ARE GIVEN BY THE AVERAGE FRACTIONAL
!   VALUES IN THE LAYER...
!
              EE2 = AMAX1(EE2,0.5)
              UD2 = 1.5*UD2
              UER(NK1)=0.5*REI*(EE1+EE2)
              UDR(NK1)=0.5*REI*(UD1+UD2)
!
!...IF THE CALCULATED UPDRAFT DETRAINMENT RATE IS GREATER THAN THE TOTAL
!   UPDRAFT MASS FLUX, ALL CLOUD MASS DETRAINS, EXIT UPDRAFT CALCULATIONS...
!
              IF(UMF(NK)-UDR(NK1).LT.10.)THEN
!
!...IF THE CALCULATED DETRAINED MASS FLUX IS GREATER THAN THE TOTAL UPD MASS
!   FLUX, IMPOSE TOTAL DETRAINMENT OF UPDRAFT MASS AT THE PREVIOUS MODEL LVL..
!   First, correct ABE calculation if needed...
!
                IF(DILBE.GT.0.)THEN
                  ABE=ABE-DILBE*G
                ENDIF
                LET=NK
!               WRITE(98,1015)P0(NK1)/100.
                EXIT 
              ELSE
                EE1=EE2
                UD1=UD2
                UPOLD=UMF(NK)-UDR(NK1)
                UPNEW=UPOLD+UER(NK1)
                UMF(NK1)=UPNEW
                DILFRC(NK1) = UPNEW/UPOLD
!
!...DETLQ AND DETIC ARE THE RATES OF DETRAINMENT OF LIQUID AND
!...ICE IN THE DETRAINING UPDRAFT MASS...
!
                DETLQ(NK1)=QLIQ(NK1)*UDR(NK1)
                DETIC(NK1)=QICE(NK1)*UDR(NK1)
                QDT(NK1)=QU(NK1)
                QU(NK1)=(UPOLD*QU(NK1)+UER(NK1)*Q0(NK1))/UPNEW
                THETEU(NK1)=(THETEU(NK1)*UPOLD+THETEE(NK1)*UER(NK1))/UPNEW
                QLIQ(NK1)=QLIQ(NK1)*UPOLD/UPNEW
                QICE(NK1)=QICE(NK1)*UPOLD/UPNEW
!
!...PPTLIQ IS THE RATE OF GENERATION (FALLOUT) OF
!...LIQUID PRECIP AT A GIVEN MODEL LVL, PPTICE THE SAME FOR ICE,
!...TRPPT IS THE TOTAL RATE OF PRODUCTION OF PRECIP UP TO THE
!...CURRENT MODEL LEVEL...
!
                PPTLIQ(NK1)=QLQOUT(NK1)*UMF(NK)
                PPTICE(NK1)=QICOUT(NK1)*UMF(NK)
!
                TRPPT=TRPPT+PPTLIQ(NK1)+PPTICE(NK1)
                IF(NK1.LE.KPBL)UER(NK1)=UER(NK1)+VMFLCL*DP(NK1)/DPTHMX
              ENDIF


!dkay
              IF (aercu_opt.gt.0) THEN
                  eps1u = 0.622
                  alatent = 2.54E6
                  KQ = NK
                  JK = KTE-KQ+1
                   muu(1,JK) = UMF(KQ)/VMFLCL     ! normalized updraft mass flux
                   duu(1,JK) = UDR(KQ)/DZQ(KQ)/VMFLCL  ! fractional detrainment rate in units of per meter
                   EUU(1,JK) = UER(KQ)/DZQ(KQ)/VMFLCL  ! normalized entrainment rate in unts of per meter
                   cmel(1,JK) = muu(1,JK)*AQNEWLQ(KQ)/DZQ(KQ)
                   cmei(1,JK) = muu(1,JK)*AQNEWIC(KQ)/DZQ(KQ)
                  gamhat(1,JK) = QSATu(KQ)*(1.+QSATu(KQ)/eps1u)   &
                 *eps1u*alatent/(R*oldTU(KQ)**2)*alatent/CP
                   wu_mskf_act(JK) = WU(KQ)  ! kf updraft velocity incloud
                   qc_mskf_act(JK) = AQNEWLQ(KQ)
                   qi_mskf_act(JK)=AQNEWIC(KQ)
              END IF
!end dkay


!
            END DO updraft

!dkay
          IF (aercu_opt.gt.0) THEN
            Zfu(1,KTE+1) = 0.0

                CPin   = CP

            EPSI0(1) = 2.0E-4

            DO KQ=KTS,KTE
              JK = KTE-KQ+1
                   zfu(1,JK) = zf_wrf(KQ)
                   su(1,JK) = oldTU(KQ)*(1.0+0.622*QSATu(KQ)) + (G*zf_wrf(KQ))/CP !TWG updraft temperature calulation
                   quu(1,JK) = oldQU(KQ)/(1.+oldQU(KQ)) ! specific humidity of updraft
                   pru(1,JK) = P0(KQ)/100.0    ! in millibars
                   TEE(1,JK) = T0(KQ)    ! ccc
                   QEE(1,JK) = Q0(KQ)/(1.+Q0(KQ))           ! specific humidity of environment

                   qee(1,JK) = oldQU(KQ)/(1.+oldQU(KQ)) ! specific humidity of updraft
                   QSATZM(1,JK) = QSATu(KQ)
!
!psh: Now, using aerosol concs from CESM
!
                    denSplume = P0(KQ)/(R*oldTU(KQ))
                    psh_fac = 1.0E-09/denSplume        ! convert ug/m3 to kg/kg

                    aer_mmr(1,JK, 1) = aercu_fct*aerocu(I,KQ,J, 6)*psh_fac
                    aer_mmr(1,JK, 2) = aercu_fct*aerocu(I,KQ,J, 5)*psh_fac
                    aer_mmr(1,JK, 3) = aercu_fct*1.44*aerocu(I,KQ,J, 1)*psh_fac
                    aer_mmr(1,JK, 4) = aercu_fct*1.44*aerocu(I,KQ,J, 2)*psh_fac
                    aer_mmr(1,JK, 5) = aercu_fct*1.44*aerocu(I,KQ,J, 3)*psh_fac
                    aer_mmr(1,JK, 6) = aercu_fct*1.44*aerocu(I,KQ,J, 4)*psh_fac
                    aer_mmr(1,JK, 7) = aercu_fct*1.54*aerocu(I,KQ,J, 9)*psh_fac
                    aer_mmr(1,JK, 8) = aercu_fct*1.37*aerocu(I,KQ,J, 7)*psh_fac
                    aer_mmr(1,JK, 9) = aercu_fct*1.25*aerocu(I,KQ,J,10)*psh_fac
                    aer_mmr(1,JK,10) = aercu_fct*1.37*aerocu(I,KQ,J, 8)*psh_fac

!psh
                  gamhat(1,JK) = QSATu(KQ)*(1.+QSATu(KQ)/eps1u)   &
                 *eps1u*alatent/(R*oldTU(KQ)**2)*alatent/CP

              END DO
                  JTT(1) = KX-NK+1
                  JBB(1) = KX-K+1  ! updraft base level   =====>>> flipped for CAM5 indexing
                  if(jtt(1).gt.jbb(1))  then
                   JTT(1) = JBB(1)
                  end if
                  JLCL(1) = JBB(1) - 1
                  msg1 = 0
                  il2g = 1
                  grav  = G
                  Rdry = R
                  DTzmp = DT
!                 print *,'jtt,jbb=', JTT(1), JBB(1)

!dkay: call the new DCCMP scheme here
              NLEVZM = KTE-KTS+1      !  this is equal to pver in zm_mp
              NLEVZMP1 = NLEVZM + 1   !  pverp

                if(jtt(1).eq.1) then
                   print *,' cloud bottom is on ground!'
                   print*,'I ',I,' J ',J
                   CALL wrf_error_fatal ('MSKF Cloud Bottom IS ON THE GROUND, diags' )
                 end if
                if(jbb(1).eq.KTE) then
                   print *,' cloud top went through the roof!'
                   print *,'JTT, jbb, jlcl=',JTT(1),JBB(1),JLCL(1)
                   CALL wrf_error_fatal ( 'MSKF CLOUD TOP WENT OVER MODEL TOP, diags' )
                 end if
            if(DCCMP) then

!           do kq=KTE,1,-1
!             print *,'wrf dz=',dzq(kq),(KTE-KQ+1)
!           end do

          call mskf_mphy(su,quu,muu,duu,cmel,cmei,zfu, pru,tee,qee,epsi0, &
               jbb,jtt,jlcl, msg1,il2g, grav, cpin, rdry,zmqliq,zmqice,zmqrain,zmqsnow,&
               rprd,wump, euu, ncmp,nimp,nrmp,nsmp,zmccn,sprd, frz, aer_mmr, dtzmp, &
               NLEVZM,NLEVZMP1,gamhat,qsatzm,wu_mskf_act,qc_mskf_act,qi_mskf_act,effc,effi,effs)
              end if

               Itest = 0
                if(Itest.eq.1) then

               write(121,*),'k,nk, kq,jk,su,quuE3,muu,duu,cmel,zfu,pru,tee, &
               qeeE3,zmqliqE4,zmqiceE4,rprd,wump,euu,ncmp,nimp,sprd,frz'
               do kq=K,NK
               JK = KTE-KQ+1
               write (121,2021) k,nk,kq,jk,su(1,jk),quu(1,jk)*1000,muu(1,jk),duu(1,jk),cmel(1,jk)
               write (121,2022) zfu(1,jk),pru(1,jk),tee(1,jk),qee(1,jk)*1000,zmqliq(1,jk)*1.e3,zmqice(1,jk)*1.e3
               write (121,2022) rprd(1,jk),wump(1,jk),euu(1,jk),ncmp(1,jk),nimp(1,jk),sprd(1,jk)
               write (121,2023) frz(1,jk)

2021          format(4I3,6(1x,E13.6))
2022          format(6(1x,e13.6))
2023          format(2(1x,e13.6))
               end do
                end if ! itest
            

               if(DCCMP) then
              do kq=KTS,KTE
               QLIQ(KQ) = 0.0
               QICE(KQ) = 0.0
               QRAIN(KQ) = 0.0
               QSNOW(KQ) = 0.0
               NLIQ(KQ) = 0.0
               NICE(KQ) = 0.0
               NRAIN(KQ) = 0.0
               NSNOW(KQ) = 0.0
               CCN(KQ) = 0.0
               EFFCH(KQ) = 2.51
               EFFIH(KQ) = 4.99
               EFFSH(KQ) = 9.99
               PPTLIQ(KQ)=0.0  ! nov23
               PPTICE(KQ)=0.0  ! nov23
               QLQOUT(KQ)=0.0  ! nov23
               QICOUT(KQ)=0.0  ! nov23
               DETLQ(KQ)=0.0  ! dec26
               DETIC(KQ)=0.0  ! dec26
              end do
              TRPPT = 0.0
              DO KQ=KTS, KTE

               JK = KX-KQ+1
!               print *,'kf qliq=', QLIQ(KQ)
                 QLIQ(KQ) = amax1(0.0,zmqliq(1,JK))
                 QICE(KQ) = amax1(0.0,zmqice(1,JK))
!TWG 06/14/16
                 QRAIN(KQ) = amax1(0.0,zmqrain(1,JK))
                 QSNOW(KQ) = amax1(0.0,zmqsnow(1,JK))
                 NLIQ(KQ) = amax1(0.0,ncmp(1,JK))
                 NICE(KQ) = amax1(0.0,nimp(1,JK))
                 NRAIN(KQ) = amax1(0.0,nrmp(1,JK))
                 NSNOW(KQ) = amax1(0.0,nsmp(1,JK))
                 CCN(KQ) = amax1(0.0,zmccn(1,JK))
                 EFFCH(KQ) = MAX(2.49, MIN(effc(1,JK), 50.))
                 EFFIH(KQ) = MAX(4.99, MIN(effi(1,JK), 125.))
                 EFFSH(KQ) = MAX(9.99, MIN(effs(1,JK), 999.))
! END TWG        
                  DETLQ(KQ)= QLIQ(KQ)*UDR(KQ)
                  DETIC(KQ)= QICE(KQ)*UDR(KQ)
!               print *,'zm qliq=', QLIQ(KQ)
                   densPlume = PPTLIQ(KQ)
!nov23
                  if(rprd(1,JK).lt.0.0) rprd(1,JK) = 0.0
                  if(sprd(1,JK).lt.0.0) sprd(1,JK) = 0.0

                   QLQOUT(KQ)=rprd(1,JK)*dzq(KQ)
                  QICOUT(KQ)=sprd(1,JK)*dzq(KQ)
                  PPTLIQ(KQ)=QLQOUT(KQ)*VMFLCL   ! check this out
                  PPTICE(KQ)=QICOUT(KQ)*VMFLCL   !   ditto

                 TRPPT=TRPPT+PPTLIQ(KQ)+PPTICE(KQ)
!                  if(densPlume.gt.0.0) then
!               print *,'zm pptliq=', &
!        PPTLIQ(KQ),'kf pptliq=',oldPPTLIQ(kq),'KQ=',KQ
!               end if
!                 print *,'zmQliqout=',kq,densPlume,VMFLCL,pptliq(kq)
!                if((i.ge.60.and.i.le.65).and.(j.ge.60.and.j.le.65)) then
!                     print *, 'KF & MP qliq=', Aqnewlq(nk),QLIQ(NK)
!                     print *, 'mu & du=', muu(1,JK), duu(1,JK)
!                      print *,'i,j,k=',I,J,KQ
!                   end if

              END DO

              end if  ! dccmp
!dkay
     
2999         CONTINUE
     END IF


!
!...CHECK CLOUD DEPTH...IF CLOUD IS TALL ENOUGH, ESTIMATE THE EQUILIBRIUM
!   TEMPERATURE LEVEL (LET) AND ADJUST MASS FLUX PROFILE AT CLOUD TOP SO
!   THAT MASS FLUX DECREASES TO ZERO AS A LINEAR FUNCTION OF PRESSURE BETWEEN
!   THE LET AND CLOUD TOP...
!     
!...LTOP IS THE MODEL LEVEL JUST BELOW THE LEVEL AT WHICH VERTICAL VELOCITY
!   FIRST BECOMES NEGATIVE...
!     
            LTOP=NK
            CLDHGT(LC)=Z0(LTOP)-ZLCL 
!
!...Instead of using the same minimum cloud height (for deep convection)
!...everywhere, try specifying minimum cloud depth as a function of TLCL...
!
!   Kain (2004)  Eq. 7
!
            IF(TLCL.GT.293.)THEN
              CHMIN = 4.E3
            ELSEIF(TLCL.LE.293. .and. TLCL.GE.273)THEN
              CHMIN = 2.E3 + 100.*(TLCL-273.)
            ELSEIF(TLCL.LT.273.)THEN
              CHMIN = 2.E3
            ENDIF
!ckay
            DO NK=K,LTOP
              qc_KF(I,NK,J)=QLIQ(NK)
              qi_KF(I,NK,J)=QICE(NK)
! TWG 06/14/16
             IF (aercu_opt .GT. 0) THEN
              qr_KF(I,NK,J)=QRAIN(NK)
              qs_KF(I,NK,J)=QSNOW(NK)
              nc_KF(I,NK,J)=NLIQ(NK)
              ni_KF(I,NK,J)=NICE(NK)
              nr_KF(I,NK,J)=NRAIN(NK)
              ns_KF(I,NK,J)=NSNOW(NK)
              ccn_KF(I,NK,J)=CCN(NK)
              EFCS(I,NK,J)=MAX(2.49, MIN(EFFCH(NK), 50.))
              EFIS(I,NK,J)=MAX(4.99, MIN(EFFIH(NK), 120.))
              EFSS(I,NK,J)=MAX(9.99, MIN(EFFSH(NK), 999.))
             END IF
! END TWG
            END DO
!ckay: if mean env RH with respect to water/ice is over 99% then dont allow KF
!ckay: added saturation w.r.to ice june 10, 2015
! to avoid double counting
          envRHavg = 0.0
          DO NK=K-1,LTOP+1
           if(T0(NK).LE.273.16) then
             envEsat = 6.112*exp(21.87*(T0(NK)-273.16)/(T0(NK)-7.66))
           else
             envEsat = 6.112*exp(17.67*(T0(NK)-273.16)/(243.5+T0(NK)-273.16))
           end if
           envEsat = envEsat * 100.0  ! to hPa
           envQsat = 0.622*envEsat/(P0(NK)-envEsat)
           envRH  = Q0(NK)/envQsat
            if(NK.GT.K.and.envRH.LT.0.99)  then
              envRHavg = 0.0
              goto 2020
            end if
            envRHavg = envRHavg + envRH
          END DO
!ckay ; get vertically averaged envRHavg
         envRHavg = envRHavg / float(LTOP-K+1+2)
2020     continue

!     
!...If cloud top height is less than the specified minimum for deep 
!...convection, save value to consider this level as source for 
!...shallow convection, go back up to check next level...
!     
!...Try specifying minimum cloud depth as a function of TLCL...
!
!
!...DO NOT ALLOW ANY CLOUD FROM THIS LAYER IF:
!
!...            1.) if there is no CAPE, or 
!...            2.) cloud top is at model level just above LCL, or
!...            3.) cloud top is within updraft source layer, or
!...            4.) cloud-top detrainment layer begins within 
!...                updraft source layer.
!...ckay        5.) if the environment is supersaturated i.e., RH > 100%
!...ckay            For now, with respect to water
!

            IF(LTOP.LE.KLCL .or. LTOP.LE.KPBL .or. LET+1.LE.KPBL &
              .or. envRHavg.ge.1.01)THEN  ! No Convection Allowed
!ckay

              CLDHGT(LC)=0.
              DO NK=K,LTOP
                UMF(NK)=0.
                UDR(NK)=0.
                UER(NK)=0.
                DETLQ(NK)=0.
                DETIC(NK)=0.
                PPTLIQ(NK)=0.
                PPTICE(NK)=0.
!ckay
                cldfra_dp_KF(I,NK,J)=0.
                cldfra_sh_KF(I,NK,J)=0.
                qc_KF(I,NK,J)=0.
                qi_KF(I,NK,J)=0.
!TWG 06/14/16
              IF (aercu_opt .GT. 0) THEN
                qr_KF(I,NK,J)=0.
                qs_KF(I,NK,J)=0.
                nc_KF(I,NK,J)=0.
                ni_KF(I,NK,J)=0.
                nr_KF(I,NK,J)=0.
                ns_KF(I,NK,J)=0.
                ccn_KF(I,NK,J)=0.
                EFCS(I,NK,J)=2.51
                EFIS(I,NK,J)=5.01
                EFSS(I,NK,J)=10.01
              END IF
! END TWG
                w_up(I,NK,J)=0.
              ENDDO
!        
            ELSEIF(CLDHGT(LC).GT.CHMIN .and. ABE.GT.1)THEN      ! Deep Convection allowed
              ISHALL=0
!ckay
              DO NK=K,LTOP
                cldfra_sh_KF(I,NK,J)=0.
              ENDDO
              EXIT usl
            ELSE
!
!...TO DISALLOW SHALLOW CONVECTION, COMMENT OUT NEXT LINE !!!!!!!!
              ISHALL = 1
!ckay
              DO NK=K,LTOP
                cldfra_dp_KF(I,NK,J)=0.
                w_up(I,NK,J)=0.
              ENDDO
              IF(NU.EQ.NUCHM)THEN
                EXIT usl               ! Shallow Convection from this layer
              ELSE
! Remember this layer (by virtue of non-zero CLDHGT) as potential shallow-cloud layer
                DO NK=K,LTOP
                  UMF(NK)=0.
                  UDR(NK)=0.
                  UER(NK)=0.
                  DETLQ(NK)=0.
                  DETIC(NK)=0.
                  PPTLIQ(NK)=0.
                  PPTICE(NK)=0.
!ckay
                  cldfra_dp_KF(I,NK,J)=0.
                  cldfra_sh_KF(I,NK,J)=0.
                  qc_KF(I,NK,J)=0.
                  qi_KF(I,NK,J)=0.
!TWG 06/14/16
                IF (aercu_opt .GT. 0) THEN
                  qr_KF(I,NK,J)=0.
                  qs_KF(I,NK,J)=0.
                  nc_KF(I,NK,J)=0.
                  ni_KF(I,NK,J)=0.
                  nr_KF(I,NK,J)=0.
                  ns_KF(I,NK,J)=0.
                  ccn_KF(I,NK,J)=0.
                  EFCS(I,NK,J)=2.51
                  EFIS(I,NK,J)=5.01
                  EFSS(I,NK,J)=10.01
                END IF
! END TWG
                  w_up(I,NK,J)=0.
                ENDDO
              ENDIF
            ENDIF
          ENDIF  ! for trigger
        END DO usl
    IF(ISHALL.EQ.1)THEN
      KSTART=MAX0(KPBL,KLCL)
      LET=KSTART
    endif
!     
!...IF THE LET AND LTOP ARE THE SAME, DETRAIN ALL OF THE UPDRAFT MASS FL
!   THIS LEVEL...
!     
    IF(LET.EQ.LTOP)THEN
      UDR(LTOP)=UMF(LTOP)+UDR(LTOP)-UER(LTOP)
      DETLQ(LTOP)=QLIQ(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      DETIC(LTOP)=QICE(LTOP)*UDR(LTOP)*UPNEW/UPOLD
      UER(LTOP)=0.
      UMF(LTOP)=0.
    ELSE 
!     
!   BEGIN TOTAL DETRAINMENT AT THE LEVEL ABOVE THE LET...
!     
      DPTT=0.
      DO NJ=LET+1,LTOP
        DPTT=DPTT+DP(NJ)
      ENDDO
      DUMFDP=UMF(LET)/DPTT
!     
!...ADJUST MASS FLUX PROFILES, DETRAINMENT RATES, AND PRECIPITATION FALL
!   RATES TO REFLECT THE LINEAR DECREASE IN MASS FLX BETWEEN THE LET AND
!     
      DO NK=LET+1,LTOP
!
!...entrainment is allowed at every level except for LTOP, so disallow
!...entrainment at LTOP and adjust entrainment rates between LET and LTOP
!...so the the dilution factor due to entrainment is not changed but 
!...the actual entrainment rate will change due due forced total 
!...detrainment in this layer...
!
        IF(NK.EQ.LTOP)THEN
          UDR(NK) = UMF(NK-1)
          UER(NK) = 0.
          DETLQ(NK) = UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK) = UDR(NK)*QICE(NK)*DILFRC(NK)
        ELSE
          UMF(NK)=UMF(NK-1)-DP(NK)*DUMFDP
          UER(NK)=UMF(NK)*(1.-1./DILFRC(NK))
          UDR(NK)=UMF(NK-1)-UMF(NK)+UER(NK)
          DETLQ(NK)=UDR(NK)*QLIQ(NK)*DILFRC(NK)
          DETIC(NK)=UDR(NK)*QICE(NK)*DILFRC(NK)
        ENDIF
        IF(NK.GE.LET+2)THEN
          TRPPT=TRPPT-PPTLIQ(NK)-PPTICE(NK)
          PPTLIQ(NK)=UMF(NK-1)*QLQOUT(NK)
          PPTICE(NK)=UMF(NK-1)*QICOUT(NK)
          TRPPT=TRPPT+PPTLIQ(NK)+PPTICE(NK)
        ENDIF
      ENDDO
    ENDIF
!     
! Initialize some arrays below cloud base and above cloud top...
!
    DO NK=1,LTOP
      IF(T0(NK).GT.T00)ML=NK
    ENDDO
    DO NK=1,K
      IF(NK.GE.LC)THEN
        IF(NK.EQ.LC)THEN
          UMF(NK)=VMFLCL*DP(NK)/DPTHMX
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
        ELSEIF(NK.LE.KPBL)THEN
          UER(NK)=VMFLCL*DP(NK)/DPTHMX
          UMF(NK)=UMF(NK-1)+UER(NK)
        ELSE
          UMF(NK)=VMFLCL
          UER(NK)=0.
        ENDIF
        TU(NK)=TMIX+(Z0(NK)-ZMIX)*GDRY
        QU(NK)=QMIX
        WU(NK)=WLCL
      ELSE
        TU(NK)=0.
        QU(NK)=0.
        UMF(NK)=0.
        WU(NK)=0.
        UER(NK)=0.
!ckay
        cldfra_dp_KF(I,NK,J)=0.
        cldfra_sh_KF(I,NK,J)=0.
        qc_KF(I,NK,J)=0.
        qi_KF(I,NK,J)=0.
!TWG 06/14/16
       IF (aercu_opt .GT. 0) THEN
        qr_KF(I,NK,J)=0.
        qs_KF(I,NK,J)=0.
        nc_KF(I,NK,J)=0.
        ni_KF(I,NK,J)=0.
        nr_KF(I,NK,J)=0.
        ns_KF(I,NK,J)=0.
        ccn_KF(I,NK,J)=0.
        EFCS(I,NK,J)=2.51
        EFIS(I,NK,J)=5.01
        EFSS(I,NK,J)=10.01
       END IF
! END TWG
        w_up (I,NK,J)=0.
      ENDIF
      UDR(NK)=0.
      QDT(NK)=0.
      QLIQ(NK)=0.
      QICE(NK)=0.
      QLQOUT(NK)=0.
      QICOUT(NK)=0.
      PPTLIQ(NK)=0.
      PPTICE(NK)=0.
      DETLQ(NK)=0.
      DETIC(NK)=0.
      RATIO2(NK)=0.
      CALL ENVIRTHT(P0(NK),T0(NK),Q0(NK),THETEE(NK),ALIQ,BLIQ,CLIQ,DLIQ)
      EQFRC(NK)=1.0
    ENDDO
!     
      LTOP1=LTOP+1
      LTOPM1=LTOP-1
!     
!...DEFINE VARIABLES ABOVE CLOUD TOP...
!     
      DO NK=LTOP1,KX
        UMF(NK)=0.
        UDR(NK)=0.
        UER(NK)=0.
        QDT(NK)=0.
        QLIQ(NK)=0.
        QICE(NK)=0.
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
!ckay
          cldfra_dp_KF(I,NK,J)=0.
          cldfra_sh_KF(I,NK,J)=0.
          qc_KF(I,NK,J)=0.
          qi_KF(I,NK,J)=0.
!TWG 06/14/16
         IF (aercu_opt .GT. 0) THEN
          qr_KF(I,NK,J)=0.
          qs_KF(I,NK,J)=0.
          nc_KF(I,NK,J)=0.
          ni_KF(I,NK,J)=0.
          nr_KF(I,NK,J)=0.
          ns_KF(I,NK,J)=0.
          ccn_KF(I,NK,J)=0.
          EFSS(I,NK,J)=10.01
          EFCS(I,NK,J)=2.51
          EFIS(I,NK,J)=5.01
         END IF
! END TWG
          w_up(I,NK,J)=0.
        ENDIF
        THTA0(NK)=0.
        THTAU(NK)=0.
        EMS(NK)=0.
        EMSD(NK)=0.
        TG(NK)=T0(NK)
        QG(NK)=Q0(NK)
        QLG(NK)=0.
        QIG(NK)=0.
        QRG(NK)=0.
        QSG(NK)=0.
        OMG(NK)=0.
      ENDDO
        OMG(KX+1)=0.
        DO NK=1,LTOP
          EMS(NK)=DP(NK)*DXSQ/G
          EMSD(NK)=1./EMS(NK)
!     
!...INITIALIZE SOME VARIABLES TO BE USED LATER IN THE VERT ADVECTION SCHEME
!     
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QDT(NK)))
          THTAU(NK)=TU(NK)*EXN(NK)
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*Q0(NK)))
          THTA0(NK)=T0(NK)*EXN(NK)
          DDILFRC(NK) = 1./DILFRC(NK)
          OMG(NK)=0.
        ENDDO
!     IF (XTIME.LT.10.)THEN
!      WRITE(98,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,
!    * TMIX-T00,PMIX,QMIX,ABE
!      WRITE(98,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,
!    * WLCL,CLDHGT
!     ENDIF
!     
!...COMPUTE CONVECTIVE TIME SCALE(TIMEC). THE MEAN WIND AT THE LCL
!...AND MIDTROPOSPHERE IS USED.
!     
        WSPD(KLCL)=SQRT(U0(KLCL)*U0(KLCL)+V0(KLCL)*V0(KLCL))
        WSPD(L5)=SQRT(U0(L5)*U0(L5)+V0(L5)*V0(L5))
        WSPD(LTOP)=SQRT(U0(LTOP)*U0(LTOP)+V0(LTOP)*V0(LTOP))
        VCONV=.5*(WSPD(KLCL)+WSPD(L5))
!...for ETA model, DX is a function of location...
        TIMEC=DX/VCONV
        TADVEC=TIMEC
!
!ckay
!new dynTau based on subcloud layer scales : note Z0(KPBL)=altitude of source layer

         TIMEC = Amax1(CHMIN,CLDHGT(LC))
         TIMEC = TIMEC*Scale_Fac

!ckay SCLvel = SubCloudLayerVELOCITY = Wsb
         SCLvel = WSTAR(I,J)**3
         ZLCL_KAY = amax1(ZLCL,Z0(KPBL))
         SCLvel = SCLvel/PBLH(I,J)
         SCLvel = SCLvel*ZLCL_kay   
         SCLvel = SCLvel**0.333   ! Wsb=SubCloudLayerVelocity for ConvectivePBL
         if(ZOL(i,J).le.0.0) then
           FRC2=3.8*Ust(I,J)*Ust(I,J)
           FRC2 = FRC2 + 0.22*SCLvel*SCLvel
           zz_kay = -1.0*ZOL(I,j)
           ZLCL_KAY = zz_kay**(2./3.)
           ZLCL_KAY = ZLCL_KAY * (1.9*Ust(I,J)*Ust(I,J))
           FRC2 = FRC2 + ZLCL_KAY
         else
           FRC2=3.8*Ust(I,J)*Ust(I,J)
         end if 

          FRC2 = SQRT(FRC2)  
          SCLvel =  FRC2  ! Wsb=new subcloud layer velocity scale for all conditions
         IF(SCLvel.lt.0.1) SCLvel = 0.1
         if(ABE.le.0.0) ABE = 1.0 
         TIMEC = TIMEC/((0.03*SCLvel*ABE)**0.3333)

!ckay: this dynTau is good for the Deep as well as Shallow Cu clouds
        TIMEC = AMAX1(TADVEC, TIMEC)
 
         NIC=NINT(TIMEC/DT)
         TIMEC=FLOAT(NIC)*DT
!     
!...COMPUTE WIND SHEAR AND PRECIPITATION EFFICIENCY.
!     
        IF(WSPD(LTOP).GT.WSPD(KLCL))THEN
          SHSIGN=1.
        ELSE
          SHSIGN=-1.
        ENDIF
        VWS=(U0(LTOP)-U0(KLCL))*(U0(LTOP)-U0(KLCL))+(V0(LTOP)-V0(KLCL))*   &
            (V0(LTOP)-V0(KLCL))
        VWS=1.E3*SHSIGN*SQRT(VWS)/(Z0(LTOP)-Z0(LCL))
        PEF=1.591+VWS*(-.639+VWS*(9.53E-2-VWS*4.96E-3))
        PEF=AMAX1(PEF,.2)
        PEF=AMIN1(PEF,.9)
!     
!...PRECIPITATION EFFICIENCY IS A FUNCTION OF THE HEIGHT OF CLOUD BASE.
!     
        CBH=(ZLCL-Z0(1))*3.281E-3
        IF(CBH.LT.3.)THEN
          RCBH=.02
        ELSE
          RCBH=.96729352+CBH*(-.70034167+CBH*(.162179896+CBH*(-            &
               1.2569798E-2+CBH*(4.2772E-4-CBH*5.44E-6))))
        ENDIF
        IF(CBH.GT.25)RCBH=2.4
        PEFCBH=1./(1.+RCBH)
        PEFCBH=AMIN1(PEFCBH,.9)
!     
!... MEAN PEF. IS USED TO COMPUTE RAINFALL.
!     
        PEFF=.5*(PEF+PEFCBH)
        PEFF2 = PEFF                                ! JSK MODS
       IF(IPRNT)THEN  
!         WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS
         WRITE(message,1035)PEF,PEFCBH,LC,LET,WKL,VWS
         CALL wrf_message( message )
!       flush(98)   
       endif     
!        WRITE(98,1035)PEF,PEFCBH,LC,LET,WKL,VWS
!*****************************************************************
!                                                                *
!                  COMPUTE DOWNDRAFT PROPERTIES                  *
!                                                                *
!*****************************************************************
!     
!     
       TDER=0.
 devap:IF(ISHALL.EQ.1)THEN
         LFS = 1
       ELSE
!
!...start downdraft about 150 mb above cloud base...
!
!        KSTART=MAX0(KPBL,KLCL)
!        KSTART=KPBL                                  ! Changed 7/23/99
         KSTART=KPBL+1                                ! Changed 7/23/99
         KLFS = LET-1
         DO NK = KSTART+1,KL
           DPPP = P0(KSTART)-P0(NK)
!          IF(DPPP.GT.200.E2)THEN
           IF(DPPP.GT.150.E2)THEN
             KLFS = NK
             EXIT 
           ENDIF
         ENDDO
         KLFS = MIN0(KLFS,LET-1)
         LFS = KLFS
!
!...if LFS is not at least 50 mb above cloud base (implying that the 
!...level of equil temp, LET, is just above cloud base) do not allow a
!...downdraft...
!
        IF((P0(KSTART)-P0(LFS)).GT.50.E2)THEN
          THETED(LFS) = THETEE(LFS)
          QD(LFS) = Q0(LFS)
!
!...call tpmix2dd to find wet-bulb temp, qv...
!
          call tpmix2dd(p0(lfs),theted(lfs),tz(lfs),qss,i,j)
          THTAD(LFS)=TZ(LFS)*(P00/P0(LFS))**(0.2854*(1.-0.28*QSS))
!     
!...TAKE A FIRST GUESS AT THE INITIAL DOWNDRAFT MASS FLUX...
!     
          TVD(LFS)=TZ(LFS)*(1.+0.608*QSS)
          RDD=P0(LFS)/(R*TVD(LFS))
          A1=(1.-PEFF)*AU0
          DMF(LFS)=-A1*RDD
          DER(LFS)=DMF(LFS)
          DDR(LFS)=0.
          RHBAR = RH(LFS)*DP(LFS)
          DPTT = DP(LFS)
          DO ND = LFS-1,KSTART,-1
            ND1 = ND+1
            DER(ND)=DER(LFS)*EMS(ND)/EMS(LFS)
            DDR(ND)=0.
            DMF(ND)=DMF(ND1)+DER(ND)
            THETED(ND)=(THETED(ND1)*DMF(ND1)+THETEE(ND)*DER(ND))/DMF(ND)
            QD(ND)=(QD(ND1)*DMF(ND1)+Q0(ND)*DER(ND))/DMF(ND)    
            DPTT = DPTT+DP(ND)
            RHBAR = RHBAR+RH(ND)*DP(ND)
          ENDDO
          RHBAR = RHBAR/DPTT
          DMFFRC = 2.*(1.-RHBAR)    ! Kain (2004) eq. 11
          DPDD = 0.
!...Calculate melting effect
!... first, compute total frozen precipitation generated...
!
          pptmlt = 0.
          DO NK = KLCL,LTOP
            PPTMLT = PPTMLT+PPTICE(NK)
          ENDDO
          if(lc.lt.ml)then
!...For now, calculate melting effect as if DMF = -UMF at KLCL, i.e., as
!...if DMFFRC=1.  Otherwise, for small DMFFRC, DTMELT gets too large!
!...12/14/98 jsk...
            DTMELT = RLF*PPTMLT/(CP*UMF(KLCL))
          else
            DTMELT = 0.
          endif
          LDT = MIN0(LFS-1,KSTART-1)
!
          call tpmix2dd(p0(kstart),theted(kstart),tz(kstart),qss,i,j)
!
          tz(kstart) = tz(kstart)-dtmelt
          ES=ALIQ*EXP((BLIQ*TZ(KSTART)-CLIQ)/(TZ(KSTART)-DLIQ))
          QSS=0.622*ES/(P0(KSTART)-ES)
          THETED(KSTART)=TZ(KSTART)*(1.E5/P0(KSTART))**(0.2854*(1.-0.28*QSS))*    &
                EXP((3374.6525/TZ(KSTART)-2.5403)*QSS*(1.+0.81*QSS))
!....  
          LDT = MIN0(LFS-1,KSTART-1)
          DO ND = LDT,1,-1
            DPDD = DPDD+DP(ND)
            THETED(ND) = THETED(KSTART)
            QD(ND)     = QD(KSTART)       
!
!...call tpmix2dd to find wet bulb temp, saturation mixing ratio...
!
            call tpmix2dd(p0(nd),theted(nd),tz(nd),qss,i,j)
            qsd(nd) = qss
!
!...specify RH decrease of 20%/km in downdraft...
!
            RHH = 1.-0.2/1000.*(Z0(KSTART)-Z0(ND))
!
!...adjust downdraft TEMP, Q to specified RH:
!
            IF(RHH.LT.1.)THEN
              DSSDT=(CLIQ-BLIQ*DLIQ)/((TZ(ND)-DLIQ)*(TZ(ND)-DLIQ))
              RL=XLV0-XLV1*TZ(ND)
              DTMP=RL*QSS*(1.-RHH)/(CP+RL*RHH*QSS*DSSDT)
              T1RH=TZ(ND)+DTMP
              ES=RHH*ALIQ*EXP((BLIQ*T1RH-CLIQ)/(T1RH-DLIQ))
              QSRH=0.622*ES/(P0(ND)-ES)
!
!...CHECK TO SEE IF MIXING RATIO AT SPECIFIED RH IS LESS THAN ACTUAL
!...MIXING RATIO...IF SO, ADJUST TO GIVE ZERO EVAPORATION...
!
              IF(QSRH.LT.QD(ND))THEN
                QSRH=QD(ND)
                T1RH=TZ(ND)+(QSS-QSRH)*RL/CP
              ENDIF
              TZ(ND)=T1RH
              QSS=QSRH
              QSD(ND) = QSS
            ENDIF         
            TVD(nd) = tz(nd)*(1.+0.608*qsd(nd))
            IF(TVD(ND).GT.TV0(ND).OR.ND.EQ.1)THEN
              LDB=ND
              EXIT
            ENDIF
          ENDDO
          IF((P0(LDB)-P0(LFS)) .gt. 50.E2)THEN   ! minimum Downdraft depth! 
            DO ND=LDT,LDB,-1
              ND1 = ND+1
              DDR(ND) = -DMF(KSTART)*DP(ND)/DPDD
              DER(ND) = 0.
              DMF(ND) = DMF(ND1)+DDR(ND)
              TDER=TDER+(QSD(nd)-QD(ND))*DDR(ND)
              QD(ND)=QSD(nd)
              THTAD(ND)=TZ(ND)*(P00/P0(ND))**(0.2854*(1.-0.28*QD(ND)))
            ENDDO
          ENDIF
        ENDIF
      ENDIF devap
!
!...IF DOWNDRAFT DOES NOT EVAPORATE ANY WATER FOR SPECIFIED RELATIVE
!...HUMIDITY, NO DOWNDRAFT IS ALLOWED...
!
d_mf:   IF(TDER.LT.1.)THEN
!           WRITE(98,3004)I,J 
!3004       FORMAT(' ','No Downdraft!;  I=',I3,2X,'J=',I3,'ISHALL =',I2)
          PPTFLX=TRPPT
          CPR=TRPPT
          TDER=0.
          CNDTNF=0.
          UPDINC=1.
          LDB=LFS
          DO NDK=1,LTOP
            DMF(NDK)=0.
            DER(NDK)=0.
            DDR(NDK)=0.
            THTAD(NDK)=0.
            WD(NDK)=0.
            TZ(NDK)=0.
            QD(NDK)=0.
          ENDDO
          AINCM2=100.
        ELSE 
          DDINC = -DMFFRC*UMF(KLCL)/DMF(KSTART)
          UPDINC=1.
          IF(TDER*DDINC.GT.TRPPT)THEN
            DDINC = TRPPT/TDER
          ENDIF
          TDER = TDER*DDINC
          DO NK=LDB,LFS
            DMF(NK)=DMF(NK)*DDINC
            DER(NK)=DER(NK)*DDINC
            DDR(NK)=DDR(NK)*DDINC
          ENDDO
         CPR=TRPPT
         PPTFLX = TRPPT-TDER
         PEFF=PPTFLX/TRPPT
         IF(IPRNT)THEN
!           write(98,*)'PRECIP EFFICIENCY =',PEFF
           write(message,*)'PRECIP EFFICIENCY =',PEFF
           CALL wrf_message(message)
!          flush(98)   
         ENDIF
!
!
!...ADJUST UPDRAFT MASS FLUX, MASS DETRAINMENT RATE, AND LIQUID WATER AN
!   DETRAINMENT RATES TO BE CONSISTENT WITH THE TRANSFER OF THE ESTIMATE
!   FROM THE UPDRAFT TO THE DOWNDRAFT AT THE LFS...
!     
!         DO NK=LC,LFS
!           UMF(NK)=UMF(NK)*UPDINC
!           UDR(NK)=UDR(NK)*UPDINC
!           UER(NK)=UER(NK)*UPDINC
!           PPTLIQ(NK)=PPTLIQ(NK)*UPDINC
!           PPTICE(NK)=PPTICE(NK)*UPDINC
!           DETLQ(NK)=DETLQ(NK)*UPDINC
!           DETIC(NK)=DETIC(NK)*UPDINC
!         ENDDO
!     
!...ZERO OUT THE ARRAYS FOR DOWNDRAFT DATA AT LEVELS ABOVE AND BELOW THE
!...DOWNDRAFT...
!     
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
           THTAD(NK)=0.
         ENDDO
       ENDIF d_mf
!
!...SET LIMITS ON THE UPDRAFT AND DOWNDRAFT MASS FLUXES SO THAT THE INFLOW
!   INTO CONVECTIVE DRAFTS FROM A GIVEN LAYER IS NO MORE THAN IS AVAILABLE
!   IN THAT LAYER INITIALLY...
!     
       AINCMX=1000.
       LMAX=MAX0(KLCL,LFS)
       DO NK=LC,LMAX
         IF((UER(NK)-DER(NK)).GT.1.e-3)THEN
           AINCM1=EMS(NK)/((UER(NK)-DER(NK))*TIMEC)
           AINCMX=AMIN1(AINCMX,AINCM1)
         ENDIF
       ENDDO
       AINC=1.
       IF(AINCMX.LT.AINC)AINC=AINCMX
!     
!...SAVE THE RELEVENT VARIABLES FOR A UNIT UPDRAFT AND DOWNDRAFT...THEY WILL 
!...BE ITERATIVELY ADJUSTED BY THE FACTOR AINC TO SATISFY THE STABILIZATION
!...CLOSURE...
!     
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
       ENDDO
       FABE=1.
       STAB=0.95
       NOITR=0
       ISTOP=0
!
        IF(ISHALL.EQ.1)THEN                              ! First for shallow convection
!
! No iteration for shallow convection; if turbulent kinetic energy (TKE) is available
! from a turbulence parameterization, scale cloud-base updraft mass flux as a function
! of TKE, but for now, just specify shallow-cloud mass flux using TKEMAX = 5...
!
!...find the maximum TKE value between LC and KLCL...
!         TKEMAX = 0.
          TKEMAX = 5.
!          DO 173 K = LC,KLCL
!            NK = KX-K+1
!            TKEMAX = AMAX1(TKEMAX,Q2(I,J,NK))
! 173      CONTINUE
!          TKEMAX = AMIN1(TKEMAX,10.)
!          TKEMAX = AMAX1(TKEMAX,5.)
!c         TKEMAX = 10.
!c...3_24_99...DPMIN was changed for shallow convection so that it is the
!c...          the same as for deep convection (5.E3).  Since this doubles
!c...          (roughly) the value of DPTHMX, add a factor of 0.5 to calcu-
!c...          lation of EVAC...
!c         EVAC  = TKEMAX*0.1
          EVAC  = 0.5*TKEMAX*0.1
!         AINC = 0.1*DPTHMX*DXIJ*DXIJ/(VMFLCL*G*TIMEC)
!          AINC = EVAC*DPTHMX*DX(I,J)*DX(I,J)/(VMFLCL*G*TIMEC)
          AINC = EVAC*DPTHMX*DXSQ/(VMFLCL*G*TIMEC)
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
          ENDDO
        ENDIF                                           ! Otherwise for deep convection
! use iterative procedure to find mass fluxes...
iter:     DO NCOUNT=1,10
!     
!*****************************************************************
!                                                                *
!           COMPUTE PROPERTIES FOR COMPENSATIONAL SUBSIDENCE     *
!                                                                *
!*****************************************************************
!     
!...DETERMINE OMEGA VALUE NECESSARY AT TOP AND BOTTOM OF EACH LAYER TO
!...SATISFY MASS CONTINUITY...
!     
            DTT=TIMEC
            DO NK=1,LTOP
              DOMGDP(NK)=-(UER(NK)-DER(NK)-UDR(NK)-DDR(NK))*EMSD(NK)
              IF(NK.GT.1)THEN
                OMG(NK)=OMG(NK-1)-DP(NK-1)*DOMGDP(NK-1)
                ABSOMG = ABS(OMG(NK))
                ABSOMGTC = ABSOMG*TIMEC
                FRDP = 0.75*DP(NK-1)
                IF(ABSOMGTC.GT.FRDP)THEN
                  DTT1 = FRDP/ABSOMG
                  DTT=AMIN1(DTT,DTT1)
                ENDIF
              ENDIF
            ENDDO
            DO NK=1,LTOP
              THPA(NK)=THTA0(NK)
              QPA(NK)=Q0(NK)
              NSTEP=NINT(TIMEC/DTT+1)
              DTIME=TIMEC/FLOAT(NSTEP)
              FXM(NK)=OMG(NK)*DXSQ/G
            ENDDO
!     
!...DO AN UPSTREAM/FORWARD-IN-TIME ADVECTION OF THETA, QV...
!     
        DO NTC=1,NSTEP
!     
!...ASSIGN THETA AND Q VALUES AT THE TOP AND BOTTOM OF EACH LAYER BASED ON
!...SIGN OF OMEGA...
!     
            DO  NK=1,LTOP
              THFXIN(NK)=0.
              THFXOUT(NK)=0.
              QFXIN(NK)=0.
              QFXOUT(NK)=0.
            ENDDO
            DO NK=2,LTOP
              IF(OMG(NK).LE.0.)THEN
                THFXIN(NK)=-FXM(NK)*THPA(NK-1)
                QFXIN(NK)=-FXM(NK)*QPA(NK-1)
                THFXOUT(NK-1)=THFXOUT(NK-1)+THFXIN(NK)
                QFXOUT(NK-1)=QFXOUT(NK-1)+QFXIN(NK)
              ELSE
                THFXOUT(NK)=FXM(NK)*THPA(NK)
                QFXOUT(NK)=FXM(NK)*QPA(NK)
                THFXIN(NK-1)=THFXIN(NK-1)+THFXOUT(NK)
                QFXIN(NK-1)=QFXIN(NK-1)+QFXOUT(NK)
              ENDIF
            ENDDO
!     
!...UPDATE THE THETA AND QV VALUES AT EACH LEVEL...
!     
            DO NK=1,LTOP
              THPA(NK)=THPA(NK)+(THFXIN(NK)+UDR(NK)*THTAU(NK)+DDR(NK)*      &
                       THTAD(NK)-THFXOUT(NK)-(UER(NK)-DER(NK))*THTA0(NK))*  &
                       DTIME*EMSD(NK)
              QPA(NK)=QPA(NK)+(QFXIN(NK)+UDR(NK)*QDT(NK)+DDR(NK)*QD(NK)-    &
                      QFXOUT(NK)-(UER(NK)-DER(NK))*Q0(NK))*DTIME*EMSD(NK)
            ENDDO   
          ENDDO   
          DO NK=1,LTOP
            THTAG(NK)=THPA(NK)
            QG(NK)=QPA(NK)
          ENDDO
!     
!...CHECK TO SEE IF MIXING RATIO DIPS BELOW ZERO ANYWHERE;  IF SO, BORROW
!...MOISTURE FROM ADJACENT LAYERS TO BRING IT BACK UP ABOVE ZERO...
!     
        DO NK=1,LTOP
          IF(QG(NK).LT.0.)THEN
            IF(NK.EQ.1)THEN                             ! JSK MODS
!              PRINT *,' PROBLEM WITH KF SCHEME:  ' ! JSK MODS
!              PRINT *,'QG = 0 AT THE SURFACE!!!!!!!'    ! JSK MODS
              CALL wrf_error_fatal ( 'QG, QG(NK).LT.0') ! JSK MODS
            ENDIF                                       ! JSK MODS
            NK1=NK+1
            IF(NK.EQ.LTOP)THEN
              NK1=KLCL
            ENDIF
            TMA=QG(NK1)*EMS(NK1)
            TMB=QG(NK-1)*EMS(NK-1)
            TMM=(QG(NK)-1.E-9)*EMS(NK  )
            BCOEFF=-TMM/((TMA*TMA)/TMB+TMB)
            ACOEFF=BCOEFF*TMA/TMB
            TMB=TMB*(1.-BCOEFF)
            TMA=TMA*(1.-ACOEFF)
            IF(NK.EQ.LTOP)THEN
              QVDIFF=(QG(NK1)-TMA*EMSD(NK1))*100./QG(NK1)
!              IF(ABS(QVDIFF).GT.1.)THEN
!             PRINT *,'!!!WARNING!!! CLOUD BASE WATER VAPOR CHANGES BY ',     &
!                      QVDIFF,                                                &
!                     '% WHEN MOISTURE IS BORROWED TO PREVENT NEGATIVE ',     &
!                     'VALUES IN KAIN-FRITSCH'
!              ENDIF
            ENDIF
            QG(NK)=1.E-9
            QG(NK1)=TMA*EMSD(NK1)
            QG(NK-1)=TMB*EMSD(NK-1)
          ENDIF
        ENDDO
        TOPOMG=(UDR(LTOP)-UER(LTOP))*DP(LTOP)*EMSD(LTOP)
        IF(ABS(TOPOMG-OMG(LTOP)).GT.1.E-3)THEN
!       WRITE(99,*)'ERROR:  MASS DOES NOT BALANCE IN KF SCHEME;            &
!      TOPOMG, OMG =',TOPOMG,OMG(LTOP)
!      TOPOMG, OMG =',TOPOMG,OMG(LTOP)
          ISTOP=1
          IPRNT=.TRUE.
          EXIT iter
        ENDIF
!     
!...CONVERT THETA TO T...
!     
        DO NK=1,LTOP
          EXN(NK)=(P00/P0(NK))**(0.2854*(1.-0.28*QG(NK)))
          TG(NK)=THTAG(NK)/EXN(NK)
          TVG(NK)=TG(NK)*(1.+0.608*QG(NK))
        ENDDO
        IF(ISHALL.EQ.1)THEN
          EXIT iter
        ENDIF
!     
!*******************************************************************
!                                                                  *
!     COMPUTE NEW CLOUD AND CHANGE IN AVAILABLE BUOYANT ENERGY.    *
!                                                                  *
!*******************************************************************
!     
!...THE FOLLOWING COMPUTATIONS ARE SIMILAR TO THAT FOR UPDRAFT
!     
!        THMIX=0.
          TMIX=0.
          QMIX=0.
!
!...FIND THE THERMODYNAMIC CHARACTERISTICS OF THE LAYER BY
!...MASS-WEIGHTING THE CHARACTERISTICS OF THE INDIVIDUAL MODEL
!...LAYERS...
!
          DO NK=LC,KPBL
            TMIX=TMIX+DP(NK)*TG(NK)
            QMIX=QMIX+DP(NK)*QG(NK)  
          ENDDO
          TMIX=TMIX/DPTHMX
          QMIX=QMIX/DPTHMX
          ES=ALIQ*EXP((TMIX*BLIQ-CLIQ)/(TMIX-DLIQ))
          QSS=0.622*ES/(PMIX-ES)
!     
!...REMOVE SUPERSATURATION FOR DIAGNOSTIC PURPOSES, IF NECESSARY...
!     
          IF(QMIX.GT.QSS)THEN
            RL=XLV0-XLV1*TMIX
            CPM=CP*(1.+0.887*QMIX)
            DSSDT=QSS*(CLIQ-BLIQ*DLIQ)/((TMIX-DLIQ)*(TMIX-DLIQ))
            DQ=(QMIX-QSS)/(1.+RL*DSSDT/CPM)
            TMIX=TMIX+RL/CP*DQ
            QMIX=QMIX-DQ
            TLCL=TMIX
          ELSE
            QMIX=AMAX1(QMIX,0.)
            EMIX=QMIX*PMIX/(0.622+QMIX)
            astrt=1.e-3
            binc=0.075
            a1=emix/aliq
            tp=(a1-astrt)/binc
            indlu=int(tp)+1
            value=(indlu-1)*binc+astrt
            aintrp=(a1-value)/binc
            tlog=aintrp*alu(indlu+1)+(1-aintrp)*alu(indlu)
            TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
            TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(TMIX-T00))*(TMIX-TDPT)
            TLCL=AMIN1(TLCL,TMIX)
          ENDIF
          TVLCL=TLCL*(1.+0.608*QMIX)
          ZLCL = ZMIX+(TLCL-TMIX)/GDRY
          DO NK = LC,KL
            KLCL=NK
            IF(ZLCL.LE.Z0(NK))THEN
              EXIT 
            ENDIF
          ENDDO
          K=KLCL-1
          DLP=(ZLCL-Z0(K))/(Z0(KLCL)-Z0(K))
!     
!...ESTIMATE ENVIRONMENTAL TEMPERATURE AND MIXING RATIO AT THE LCL...
!     
          TENV=TG(K)+(TG(KLCL)-TG(K))*DLP
          QENV=QG(K)+(QG(KLCL)-QG(K))*DLP
          TVEN=TENV*(1.+0.608*QENV)
          PLCL=P0(K)+(P0(KLCL)-P0(K))*DLP
          THETEU(K)=TMIX*(1.E5/PMIX)**(0.2854*(1.-0.28*QMIX))*             &
                  EXP((3374.6525/TLCL-2.5403)*QMIX*(1.+0.81*QMIX))
!     
!...COMPUTE ADJUSTED ABE(ABEG).
!     
          ABEG=0.
          DO NK=K,LTOPM1
            NK1=NK+1

!new ckay adding FRZ effect 01-30-2015
         IF (aercu_opt.GT.0.0) THEN
            JK = KX-NK+1
            a1kay =  FRZ(1,JK)*DZA(NK)*3.337E5/CP
            a1kay = a1kay * ((1.E5/P0(NK))**ROCP)
            THETEU(NK)  =  a1kay + THETEU(NK)
         END IF
!ckay freezing effect included in ThetaU for cape calculation

            THETEU(NK1) = THETEU(NK)
!
            call tpmix2dd(p0(nk1),theteu(nk1),tgu(nk1),qgu(nk1),i,j)
!
            TVQU(NK1)=TGU(NK1)*(1.+0.608*QGU(NK1)-QLIQ(NK1)-QICE(NK1))
            IF(NK.EQ.K)THEN
              DZZ=Z0(KLCL)-ZLCL
              DILBE=((TVLCL+TVQU(NK1))/(TVEN+TVG(NK1))-1.)*DZZ
            ELSE
              DZZ=DZA(NK)
              DILBE=((TVQU(NK)+TVQU(NK1))/(TVG(NK)+TVG(NK1))-1.)*DZZ
            ENDIF
            IF(DILBE.GT.0.)ABEG=ABEG+DILBE*G
!
!...DILUTE BY ENTRAINMENT BY THE RATE AS ORIGINAL UPDRAFT...
!
            CALL ENVIRTHT(P0(NK1),TG(NK1),QG(NK1),THTEEG(NK1),ALIQ,BLIQ,CLIQ,DLIQ)
            THETEU(NK1)=THETEU(NK1)*DDILFRC(NK1)+THTEEG(NK1)*(1.-DDILFRC(NK1))
          ENDDO
!     
!...ASSUME AT LEAST 90% OF CAPE (ABE) IS REMOVED BY CONVECTION DURING
!...THE PERIOD TIMEC...
!     
          IF(NOITR.EQ.1)THEN
!         write(98,*)' '
!         write(98,*)'TAU, I, J, =',NTSD,I,J
!         WRITE(98,1060)FABE
!          GOTO 265
          EXIT iter
          ENDIF
          DABE=AMAX1(ABE-ABEG,capeDX*ABE)

          FABE=ABEG/ABE
          IF(FABE.GT.1. .and. ISHALL.EQ.0)THEN
!          WRITE(98,*)'UPDRAFT/DOWNDRAFT COUPLET INCREASES CAPE AT THIS
!     *GRID POINT; NO CONVECTION ALLOWED!'
            RETURN  
          ENDIF
          IF(NCOUNT.NE.1)THEN
            IF(ABS(AINC-AINCOLD).LT.0.0001)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
            DFDA=(FABE-FABEOLD)/(AINC-AINCOLD)
            IF(DFDA.GT.0.)THEN
              NOITR=1
              AINC=AINCOLD
              CYCLE iter
            ENDIF
          ENDIF
          AINCOLD=AINC
          FABEOLD=FABE
          IF(AINC/AINCMX.GT.0.999.AND.FABE.GT.1.05-STAB)THEN
!           write(98,*)' '
!           write(98,*)'TAU, I, J, =',NTSD,I,J
!           WRITE(98,1055)FABE
!            GOTO 265
            EXIT
          ENDIF
          IF((FABE.LE.1.05-STAB.AND.FABE.GE.0.95-STAB) .or. NCOUNT.EQ.10)THEN
            EXIT iter
          ELSE
            IF(NCOUNT.GT.10)THEN
!             write(98,*)' '
!             write(98,*)'TAU, I, J, =',NTSD,I,J
!             WRITE(98,1060)FABE
!             GOTO 265
              EXIT
            ENDIF
!     
!...IF MORE THAN 10% OF THE ORIGINAL CAPE REMAINS, INCREASE THE CONVECTIVE
!...MASS FLUX BY THE FACTOR AINC:
!     
            IF(FABE.EQ.0.)THEN
              AINC=AINC*0.5
            ELSE
              IF(DABE.LT.1.e-4)THEN
                NOITR=1
                AINC=AINCOLD
                CYCLE iter
              ELSE
                AINC=AINC*STAB*ABE/DABE
              ENDIF
            ENDIF
!           AINC=AMIN1(AINCMX,AINC)
            AINC=AMIN1(AINCMX,AINC)
!...IF AINC BECOMES VERY SMALL, EFFECTS OF CONVECTION ! JSK MODS
!...WILL BE MINIMAL SO JUST IGNORE IT...              ! JSK MODS
            IF(AINC.LT.0.05)then
              RETURN                          ! JSK MODS
            ENDIF
!            AINC=AMAX1(AINC,0.05)                        ! JSK MODS
            TDER=TDER2*AINC
            PPTFLX=PPTFL2*AINC
!           IF (XTIME.LT.10.)THEN
!           WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,
!          *              FABEOLD,AINCOLD 
!           ENDIF
            DO NK=1,LTOP
              UMF(NK)=UMF2(NK)*AINC
              DMF(NK)=DMF2(NK)*AINC
              DETLQ(NK)=DETLQ2(NK)*AINC
              DETIC(NK)=DETIC2(NK)*AINC
              UDR(NK)=UDR2(NK)*AINC
              UER(NK)=UER2(NK)*AINC
              DER(NK)=DER2(NK)*AINC
              DDR(NK)=DDR2(NK)*AINC
            ENDDO
!     
!...GO BACK UP FOR ANOTHER ITERATION...
!     
          ENDIF
        ENDDO iter

!ckay
! get the cloud fraction for layer NK+1=NK1
            updil = (100.-AINC)
            updil = updil/100.
           IF (aercu_opt .GT. 0) THEN
            ainc_frac(I,J) = 1.0-updil !TWG
           END IF
            updil = updil*dxsq  
            Drag = 0.5   

        IF(ISHALL.EQ.1) THEN
          DO NK=KLCL, LTOP
            UMF_new = UMF(NK)/updil
            denSplume = P0(NK)/(R*TU(NK))
            xcldfra = 0.07*alog(1.+(500.*UMF_new))
            xcldfra = amax1(0.01,xcldfra)
            cldfra_sh_KF(I,NK,J) = amin1(0.2,xcldfra)
!ckaywup
            DMF_new=DMF(NK)/updil
            FXM_new=FXM(NK)/dxsq
!           w_up(I,NK,J) = (UMF_new+DMF_new-FXM_new)/denSplume
!           w_up(I,NK,J) = w_up(I,NK,J)*Drag*DT/TIMEC
           w_up(I,NK,J) = (UMF_new/denSplume)*Drag*DT/TIMEC
          ENDDO
        ELSE 
          DO NK=KLCL, LTOP
! ww: moved the next line up
            UMF_new = UMF(NK)/updil
            denSplume = P0(NK)/(R*TU(NK))
            xcldfra = 0.14*alog(1.+(500.*UMF_new))
            xcldfra = amax1(0.01,xcldfra)
            cldfra_dp_KF(I,NK,J) = amin1(0.6,xcldfra)
!new added downdraft impact
            DMF_new = DMF(NK)/updil
            FXM_new = FXM(NK)/dxsq
!           w_up(I,NK,J) = (UMF_new+DMF_new-FXM_new)/denSplume
!           w_up(I,NK,J) = w_up(I,NK,J)*Drag*DT/TIMEC
           w_up(I,NK,J) = (UMF_new/denSplume)*Drag*DT/TIMEC

          ENDDO
        ENDIF
!ckaywup
          envRHavg = 0.0
          DO NK=KLCL-1,LTOP1
           envEsat = 6.112*exp(17.67*(T0(NK)-273.16)/(243.5+T0(NK)-273.16))
           envEsat = envEsat * 100.0  ! to hPa
           envQsat = 0.622*envEsat/(P0(NK)-envEsat)
           envRH  = Q0(NK)/envQsat
            envRHavg = envRHavg + envRH
           if(envRH.gt.1.01) then
             w_up(I,NK,J) = 0.0
           end if
          END DO

!kf_edrates
!Save up/down entrainment/detrainment rates as 3D variables
        IF (KF_EDRATES == 1) THEN 
           DO NK=1,LTOP
             UDR_KF(I,NK,J)=UDR(NK)
             DDR_KF(I,NK,J)=DDR(NK)
             UER_KF(I,NK,J)=UER(NK)
             DER_KF(I,NK,J)=DER(NK)
           ENDDO
        ENDIF

!     
!...COMPUTE HYDROMETEOR TENDENCIES AS IS DONE FOR T, QV...
!     
!...FRC2 IS THE FRACTION OF TOTAL CONDENSATE      !  PPT FB MODS
!...GENERATED THAT GOES INTO PRECIPITIATION       !  PPT FB MODS
!
!  Redistribute hydormeteors according to the final mass-flux values:
!
        IF(CPR.GT.0.)THEN 
          FRC2=PPTFLX/(CPR*AINC)                    !  PPT FB MODS
        ELSE
           FRC2=0.
        ENDIF
        DO NK=1,LTOP
          QLPA(NK)=QL0(NK)
          QIPA(NK)=QI0(NK)
          QRPA(NK)=QR0(NK)
          QSPA(NK)=QS0(NK)
          RAINFB(NK)=PPTLIQ(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS
          SNOWFB(NK)=PPTICE(NK)*AINC*FBFRC*FRC2   !  PPT FB MODS
        ENDDO
        DO NTC=1,NSTEP
!     
!...ASSIGN HYDROMETEORS CONCENTRATIONS AT THE TOP AND BOTTOM OF EACH LAYER
!...BASED ON THE SIGN OF OMEGA...
!     
          DO NK=1,LTOP
            QLFXIN(NK)=0.
            QLFXOUT(NK)=0.
            QIFXIN(NK)=0.
            QIFXOUT(NK)=0.
            QRFXIN(NK)=0.
            QRFXOUT(NK)=0.
            QSFXIN(NK)=0.
            QSFXOUT(NK)=0.
          ENDDO   
          DO NK=2,LTOP
            IF(OMG(NK).LE.0.)THEN
              QLFXIN(NK)=-FXM(NK)*QLPA(NK-1)
              QIFXIN(NK)=-FXM(NK)*QIPA(NK-1)
              QRFXIN(NK)=-FXM(NK)*QRPA(NK-1)
              QSFXIN(NK)=-FXM(NK)*QSPA(NK-1)
              QLFXOUT(NK-1)=QLFXOUT(NK-1)+QLFXIN(NK)
              QIFXOUT(NK-1)=QIFXOUT(NK-1)+QIFXIN(NK)
              QRFXOUT(NK-1)=QRFXOUT(NK-1)+QRFXIN(NK)
              QSFXOUT(NK-1)=QSFXOUT(NK-1)+QSFXIN(NK)
            ELSE
              QLFXOUT(NK)=FXM(NK)*QLPA(NK)
              QIFXOUT(NK)=FXM(NK)*QIPA(NK)
              QRFXOUT(NK)=FXM(NK)*QRPA(NK)
              QSFXOUT(NK)=FXM(NK)*QSPA(NK)
              QLFXIN(NK-1)=QLFXIN(NK-1)+QLFXOUT(NK)
              QIFXIN(NK-1)=QIFXIN(NK-1)+QIFXOUT(NK)
              QRFXIN(NK-1)=QRFXIN(NK-1)+QRFXOUT(NK)
              QSFXIN(NK-1)=QSFXIN(NK-1)+QSFXOUT(NK)
            ENDIF
          ENDDO   
!     
!...UPDATE THE HYDROMETEOR CONCENTRATION VALUES AT EACH LEVEL...
!     
          DO NK=1,LTOP
            QLPA(NK)=QLPA(NK)+(QLFXIN(NK)+DETLQ(NK)-QLFXOUT(NK))*DTIME*EMSD(NK)
            QIPA(NK)=QIPA(NK)+(QIFXIN(NK)+DETIC(NK)-QIFXOUT(NK))*DTIME*EMSD(NK)
            QRPA(NK)=QRPA(NK)+(QRFXIN(NK)-QRFXOUT(NK)+RAINFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS
            QSPA(NK)=QSPA(NK)+(QSFXIN(NK)-QSFXOUT(NK)+SNOWFB(NK))*DTIME*EMSD(NK)         !  PPT FB MODS
          ENDDO     
        ENDDO
        DO NK=1,LTOP
          QLG(NK)=QLPA(NK)
          QIG(NK)=QIPA(NK)
          QRG(NK)=QRPA(NK)
          QSG(NK)=QSPA(NK)
        ENDDO   

!kf_edrates
!Save convective timescale (TIMEC) as 2D variable
        IF (KF_EDRATES == 1) THEN
           TIMEC_KF(I,J)=TIMEC
        ENDIF

!
!...CLEAN THINGS UP, CALCULATE CONVECTIVE FEEDBACK TENDENCIES FOR THIS
!...GRID POINT...
!     
!     IF (XTIME.LT.10.)THEN
!     WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC 
!     ENDIF
       IF(IPRNT)THEN  
!         WRITE(98,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
         WRITE(message,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
         CALL wrf_message(message)
!        flush(98)   
       endif  
!     
!...SEND FINAL PARAMETERIZED VALUES TO OUTPUT FILES...
!     
!297   IF(IPRNT)then 
       IF(IPRNT)then 
!    if(I.eq.16 .and. J.eq.41)then
!      IF(ISTOP.EQ.1)THEN
!        write(98,*)
!        write(98,*)'At t(h), I, J =',float(NTSD)*72./3600.,I,J
         write(message,*)'P(LC), DTP, WKL, WKLCL =',p0(LC)/100.,       &
                     TLCL+DTLCL+dtrh-TENV,WKL,WKLCL
         call wrf_message(message)
         write(message,*)'TLCL, DTLCL, DTRH, TENV =',TLCL,DTLCL,       &
                      DTRH,TENV   
         call wrf_message(message)
         WRITE(message,1025)KLCL,ZLCL,DTLCL,LTOP,P0(LTOP),IFLAG,       &
         TMIX-T00,PMIX,QMIX,ABE
         call wrf_message(message)
         WRITE(message,1030)P0(LET)/100.,P0(LTOP)/100.,VMFLCL,PLCL/100.,  &
         WLCL,CLDHGT(LC)
         call wrf_message(message)
         WRITE(message,1035)PEF,PEFCBH,LC,LET,WKL,VWS 
         call wrf_message(message)
         write(message,*)'PRECIP EFFICIENCY =',PEFF 
         call wrf_message(message)
      WRITE(message,1080)LFS,LDB,LDT,TIMEC,TADVEC,NSTEP,NCOUNT,FABE,AINC
         call wrf_message(message)
!      ENDIF
!!!!! HERE !!!!!!!
           WRITE(message,1070)'  P  ','   DP ',' DT K/D ',' DR K/D ','   OMG  ',        &
          ' DOMGDP ','   UMF  ','   UER  ','   UDR  ','   DMF  ','   DER  '        &
          ,'   DDR  ','   EMS  ','    W0  ','  DETLQ ',' DETIC '
         call wrf_message(message)
           write(message,*)'just before DO 300...'
         call wrf_message(message)
!          flush(98)
           DO NK=1,LTOP
             K=LTOP-NK+1
             DTT=(TG(K)-T0(K))*86400./TIMEC
             RL=XLV0-XLV1*TG(K)
             DR=-(QG(K)-Q0(K))*RL*86400./(TIMEC*CP)
             UDFRC=UDR(K)*TIMEC*EMSD(K)
             UEFRC=UER(K)*TIMEC*EMSD(K)
             DDFRC=DDR(K)*TIMEC*EMSD(K)
             DEFRC=-DER(K)*TIMEC*EMSD(K)
             WRITE(message,1075)P0(K)/100.,DP(K)/100.,DTT,DR,OMG(K),DOMGDP(K)*1.E4,       &
             UMF(K)/1.E6,UEFRC,UDFRC,DMF(K)/1.E6,DEFRC,DDFRC,EMS(K)/1.E11,           &
             W0AVG1D(K)*1.E2,DETLQ(K)*TIMEC*EMSD(K)*1.E3,DETIC(K)*                   &
             TIMEC*EMSD(K)*1.E3
         call wrf_message(message)
           ENDDO
           WRITE(message,1085)'K','P','Z','T0','TG','DT','TU','TD','Q0','QG',             &
                  'DQ','QU','QD','QLG','QIG','QRG','QSG','RH0','RHG'
         call wrf_message(message)
           DO NK=1,KL
             K=KX-NK+1
             DTT=TG(K)-T0(K)
             TUC=TU(K)-T00
             IF(K.LT.LC.OR.K.GT.LTOP)TUC=0.
             TDC=TZ(K)-T00
             IF((K.LT.LDB.OR.K.GT.LDT).AND.K.NE.LFS)TDC=0.
             IF(T0(K).LT.T00)THEN
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ELSE
               ES=ALIQ*EXP((BLIQ*TG(K)-CLIQ)/(TG(K)-DLIQ))
             ENDIF  
             QGS=ES*0.622/(P0(K)-ES)
             RH0=Q0(K)/QES(K)
             RHG=QG(K)/QGS
             WRITE(message,1090)K,P0(K)/100.,Z0(K),T0(K)-T00,TG(K)-T00,DTT,TUC,            &
             TDC,Q0(K)*1000.,QG(K)*1000.,(QG(K)-Q0(K))*1000.,QU(K)*                   &
             1000.,QD(K)*1000.,QLG(K)*1000.,QIG(K)*1000.,QRG(K)*1000.,                &
             QSG(K)*1000.,RH0,RHG
         call wrf_message(message)
           ENDDO
!     
!...IF CALCULATIONS ABOVE SHOW AN ERROR IN THE MASS BUDGET, PRINT OUT A
!...TO BE USED LATER FOR DIAGNOSTIC PURPOSES, THEN ABORT RUN...
!     
!         IF(ISTOP.EQ.1 .or. ISHALL.EQ.1)THEN

!         IF(ISHALL.NE.1)THEN
!            write(98,4421)i,j,iyr,imo,idy,ihr,imn
!           write(98)i,j,iyr,imo,idy,ihr,imn,kl
! 4421       format(7i4)
!            write(98,4422)kl
! 4422       format(i6) 
            DO 310 NK = 1,KL
              k = kl - nk + 1
!             write(98,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,       &
!                      u0(k),v0(k),W0AVG1D(K),dp(k),tke(k)
!             write(98) p0,t0,q0,u0,v0,w0,dp,tke
!           WRITE(98,1115)Z0(K),P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,
!    *               U0(K),V0(K),DP(K)/100.,W0AVG(I,J,K)
 310        CONTINUE
            IF(ISTOP.EQ.1)THEN
              CALL wrf_error_fatal ( 'KAIN-FRITSCH, istop=1, diags' )
            ENDIF
!         ENDIF
  4455  format(8f11.3) 
       ENDIF
        CNDTNF=(1.-EQFRC(LFS))*(QLIQ(LFS)+QICE(LFS))*DMF(LFS)
        PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ
        RAINCV(I,J)=DT*PRATEC(I,J)     !  PPT FB MODS
!        RAINCV(I,J)=.1*.5*DT*PPTFLX/DXSQ               !  PPT FB MODS
!         RNC=0.1*TIMEC*PPTFLX/DXSQ
        RNC=RAINCV(I,J)*NIC
!       IF(ISHALL.EQ.0.AND.IPRNT)write (98,909)I,J,RNC

!     WRITE(98,1095)CPR*AINC,TDER+PPTFLX+CNDTNF
!     
!  EVALUATE MOISTURE BUDGET...
!     

        QINIT=0.
        QFNL=0.
        DPT=0.
        DO 315 NK=1,LTOP
          DPT=DPT+DP(NK)
          QINIT=QINIT+Q0(NK)*EMS(NK)
          QFNL=QFNL+QG(NK)*EMS(NK)
          QFNL=QFNL+(QLG(NK)+QIG(NK)+QRG(NK)+QSG(NK))*EMS(NK)
  315   CONTINUE
        QFNL=QFNL+PPTFLX*TIMEC*(1.-FBFRC)       !  PPT FB MODS
!        QFNL=QFNL+PPTFLX*TIMEC                 !  PPT FB MODS
        ERR2=(QFNL-QINIT)*100./QINIT
!       IF(IPRNT)WRITE(98,1110)QINIT,QFNL,ERR2
      IF(ABS(ERR2).GT.0.05 .AND. ISTOP.EQ.0)THEN 
!       write(99,*)'!!!!!!!! MOISTURE BUDGET ERROR IN KFPARA !!!'
!       WRITE(99,1110)QINIT,QFNL,ERR2
        IPRNT=.FALSE.
        ISTOP=1
!            write(98,4422)kl
 4422       format(i6)
            DO 311 NK = 1,KL
              k = kl - nk + 1
!             write(99,4455) p0(k)/100.,t0(k)-273.16,q0(k)*1000.,       &
!                      u0(k),v0(k),W0AVG1D(K),dp(k)
!             write(98) p0,t0,q0,u0,v0,w0,dp,tke
!           WRITE(98,1115)P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,          &
!                    U0(K),V0(K),W0AVG1D(K),dp(k)/100.,tke(k)
!            WRITE(98,4456)P0(K)/100.,T0(K)-273.16,Q0(K)*1000.,          &
!                     U0(K),V0(K),W0AVG1D(K),dp(k)/100.,tke(k)
 311        CONTINUE
!           flush(98)

!        GOTO 297
!         STOP 'QVERR'
      ENDIF
 1115 FORMAT (2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)
 4456  format(8f12.3)
        IF(PPTFLX.GT.0.)THEN
          RELERR=ERR2*QINIT/(PPTFLX*TIMEC)
        ELSE
          RELERR=0.
        ENDIF
     IF(IPRNT)THEN
!        WRITE(98,1120)RELERR
!        WRITE(98,*)'TDER, CPR, TRPPT =',              &
!          TDER,CPR*AINC,TRPPT*AINC
     ENDIF
!     
!...FEEDBACK TO RESOLVABLE SCALE TENDENCIES.
!     
!...IF THE ADVECTIVE TIME PERIOD (TADVEC) IS LESS THAN SPECIFIED MINIMUM
!...TIMEC, ALLOW FEEDBACK TO OCCUR ONLY DURING TADVEC...
!     
        IF(TADVEC.LT.TIMEC)NIC=NINT(TADVEC/DT)
        NCA(I,J) = REAL(NIC)*DT
        IF(ISHALL.EQ.1)THEN
!          TIMEC = 2400.
           NCA(I,J) = CUDT*60.
           NSHALL = NSHALL+1
        ENDIF

        DO K=1,KX
!         IF(IMOIST(INEST).NE.2)THEN
!
!...IF HYDROMETEORS ARE NOT ALLOWED, THEY MUST BE EVAPORATED OR SUBLIMATED
!...AND FED BACK AS VAPOR, ALONG WITH ASSOCIATED CHANGES IN TEMPERATURE.
!...NOTE:  THIS WILL INTRODUCE CHANGES IN THE CONVECTIVE TEMPERATURE AND
!...WATER VAPOR FEEDBACK TENDENCIES AND MAY LEAD TO SUPERSATURATED VALUE
!...OF QG...
!
!           RLC=XLV0-XLV1*TG(K)
!           RLS=XLS0-XLS1*TG(K)
!           CPM=CP*(1.+0.887*QG(K))
!           TG(K)=TG(K)-(RLC*(QLG(K)+QRG(K))+RLS*(QIG(K)+QSG(K)))/CPM
!           QG(K)=QG(K)+(QLG(K)+QRG(K)+QIG(K)+QSG(K))
!           DQLDT(I,J,NK)=0.
!           DQIDT(I,J,NK)=0.
!           DQRDT(I,J,NK)=0.
!           DQSDT(I,J,NK)=0.
!         ELSE
!
!...IF ICE PHASE IS NOT ALLOWED, MELT ALL FROZEN HYDROMETEORS...
!
          IF(warm_rain)THEN

            CPM=CP*(1.+0.887*QG(K))
            TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(.NOT. F_QS)THEN
!
!...IF ICE PHASE IS ALLOWED, BUT MIXED PHASE IS NOT, MELT FROZEN HYDROMETEORS
!...BELOW THE MELTING LEVEL, FREEZE LIQUID WATER ABOVE THE MELTING LEVEL
!
            CPM=CP*(1.+0.887*QG(K))
            IF(K.LE.ML)THEN
              TG(K)=TG(K)-(QIG(K)+QSG(K))*RLF/CPM
            ELSEIF(K.GT.ML)THEN
              TG(K)=TG(K)+(QLG(K)+QRG(K))*RLF/CPM
            ENDIF
            DQCDT(K)=(QLG(K)+QIG(K)-QL0(K)-QI0(K))/TIMEC
            DQIDT(K)=0.
            DQRDT(K)=(QRG(K)+QSG(K)-QR0(K)-QS0(K))/TIMEC
            DQSDT(K)=0.
          ELSEIF(F_QS) THEN
!
!...IF MIXED PHASE HYDROMETEORS ARE ALLOWED, FEED BACK CONVECTIVE TENDENCIES
!...OF HYDROMETEORS DIRECTLY...
!
            DQCDT(K)=(QLG(K)-QL0(K))/TIMEC
            DQSDT(K)=(QSG(K)-QS0(K))/TIMEC
            DQRDT(K)=(QRG(K)-QR0(K))/TIMEC
            IF (F_QI) THEN
               DQIDT(K)=(QIG(K)-QI0(K))/TIMEC
            ELSE
               DQSDT(K)=DQSDT(K)+(QIG(K)-QI0(K))/TIMEC
            ENDIF
          ELSE
!              PRINT *,'THIS COMBINATION OF IMOIST, IEXICE, IICE NOT ALLOWED!'
              CALL wrf_error_fatal ( 'KAIN-FRITSCH, THIS MICROPHYSICS CHOICE IS NOT ALLOWED' )
          ENDIF
          DTDT(K)=(TG(K)-T0(K))/TIMEC
          DQDT(K)=(QG(K)-Q0(K))/TIMEC
        ENDDO
        PRATEC(I,J)=PPTFLX*(1.-FBFRC)/DXSQ
        RAINCV(I,J)=DT*PRATEC(I,J)
!        RAINCV(I,J)=.1*.5*DT*PPTFLX/DXSQ               !  PPT FB MODS
!         RNC=0.1*TIMEC*PPTFLX/DXSQ
        RNC=RAINCV(I,J)*NIC
 909     FORMAT('AT I, J =',i3,1x,i3,' CONVECTIVE RAINFALL =',F8.4,' mm')
!      write (98,909)I,J,RNC
!      write (6,909)I,J,RNC
!      WRITE(98,*)'at NTSD =',NTSD,',No. of KF points activated =',
!     *            NCCNT
!      flush(98)
1000  FORMAT(' ',10A8)
1005  FORMAT(' ',F6.0,2X,F6.4,2X,F7.3,1X,F6.4,2X,4(F6.3,2X),2(F7.3,1X))
1010  FORMAT(' ',' VERTICAL VELOCITY IS NEGATIVE AT ',F4.0,' MB')
1015   FORMAT(' ','ALL REMAINING MASS DETRAINS BELOW ',F4.0,' MB')
1025   FORMAT(5X,' KLCL=',I2,' ZLCL=',F7.1,'M',                         &
        ' DTLCL=',F5.2,' LTOP=',I2,' P0(LTOP)=',-2PF5.1,'MB FRZ LV=',   &
        I2,' TMIX=',0PF4.1,1X,'PMIX=',-2PF6.1,' QMIX=',3PF5.1,          &
        ' CAPE=',0PF7.1)
1030   FORMAT(' ',' P0(LET) = ',F6.1,' P0(LTOP) = ',F6.1,' VMFLCL =',   &
      E12.3,' PLCL =',F6.1,' WLCL =',F6.3,' CLDHGT =',                  &
      F8.1)
1035  FORMAT(1X,'PEF(WS)=',F4.2,'(CB)=',F4.2,'LC,LET=',2I3,'WKL='       &
      ,F6.3,'VWS=',F5.2)
!1055  FORMAT('*** DEGREE OF STABILIZATION =',F5.3,                  &
!      ', NO MORE MASS FLUX IS ALLOWED!')
!1060     FORMAT(' ITERATION DOES NOT CONVERGE TO GIVE THE SPECIFIED    &
!      &DEGREE OF STABILIZATION!  FABE= ',F6.4) 
 1070 FORMAT (16A8) 
 1075 FORMAT (F8.2,3(F8.2),2(F8.3),F8.2,2F8.3,F8.2,6F8.3) 
 1080 FORMAT(2X,'LFS,LDB,LDT =',3I3,' TIMEC, TADVEC, NSTEP=',           &
              2(1X,F5.0),I3,'NCOUNT, FABE, AINC=',I2,1X,F5.3,F6.2) 
 1085 FORMAT (A3,16A7,2A8) 
 1090 FORMAT (I3,F7.2,F7.0,10F7.2,4F7.3,2F8.3) 
 1095 FORMAT(' ','  PPT PRODUCTION RATE= ',F10.0,' TOTAL EVAP+PPT= ',F10.0)
1105   FORMAT(' ','NET LATENT HEAT RELEASE =',E12.5,' ACTUAL HEATING =',&
       E12.5,' J/KG-S, DIFFERENCE = ',F9.3,'%')
1110   FORMAT(' ','INITIAL WATER =',E12.5,' FINAL WATER =',E12.5,       &
       ' TOTAL WATER CHANGE =',F8.2,'%')
! 1115 FORMAT (2X,F6.0,2X,F7.2,2X,F5.1,2X,F6.3,2(2X,F5.1),2X,F7.2,2X,F7.4)
1120   FORMAT(' ','MOISTURE ERROR AS FUNCTION OF TOTAL PPT =',F9.3,'%')
!
!-----------------------------------------------------------------------
!--------------SAVE CLOUD TOP AND BOTTOM FOR RADIATION------------------
!-----------------------------------------------------------------------
!
      CUTOP(I,J)=REAL(LTOP)
      CUBOT(I,J)=REAL(LCL)
!
!-----------------------------------------------------------------------
   END SUBROUTINE  MSKF_eta_PARA
!********************************************************************
! ***********************************************************************
!dkay
!dkay: added QSu as output to get saturated Q of updraft

   SUBROUTINE TPMIX2(p,thes,tu,qu,qliq,qice,qnewlq,qnewic,XLV1,XLV0,Qsu)
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
!  SAVE !TWG 2017 add to avoid memory issues
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES,XLV1,XLV0
   REAL,         INTENT(OUT  )   :: QNEWLQ,QNEWIC,QSu
   REAL,         INTENT(INOUT)   :: TU,QU,QLIQ,QICE
   REAL    ::    TP,QQ,BTH,TTH,PP,T00,T10,T01,T11,Q00,Q10,Q01,Q11,          &
                 TEMP,QS,QNEW,DQ,QTOT,RLL,CPP
   INTEGER ::    IPTB,ITHTB
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
!         write(98,*)'**** OUT OF BOUNDS *********'
!        flush(98)
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

!dkay
       QSu = qs
!

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
!******************************************************************************
      SUBROUTINE DTFRZNEW(TU,P,THTEU,QU,QFRZ,QICE,ALIQ,BLIQ,CLIQ,DLIQ)
!-----------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 Add to avoid memory issues
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,QFRZ,ALIQ,BLIQ,CLIQ,DLIQ
   REAL,         INTENT(INOUT)   :: TU,THTEU,QU,QICE
   REAL    ::    RLC,RLS,RLF,CPP,A,DTFRZ,ES,QS,DQEVAP,PII
!-----------------------------------------------------------------------
!
!...ALLOW THE FREEZING OF LIQUID WATER IN THE UPDRAFT TO PROCEED AS AN 
!...APPROXIMATELY LINEAR FUNCTION OF TEMPERATURE IN THE TEMPERATURE RANGE 
!...TTFRZ TO TBFRZ...
!...FOR COLDER TEMPERATURES, FREEZE ALL LIQUID WATER...
!...THERMODYNAMIC PROPERTIES ARE STILL CALCULATED WITH RESPECT TO LIQUID WATER
!...TO ALLOW THE USE OF LOOKUP TABLE TO EXTRACT TMP FROM THETAE...
!
      RLC=2.5E6-2369.276*(TU-273.16)
      RLS=2833922.-259.532*(TU-273.16)
      RLF=RLS-RLC
      CPP=1004.5*(1.+0.89*QU)
!
!  A = D(es)/DT IS THAT CALCULATED FROM BUCK (1981) EMPERICAL FORMULAS
!  FOR SATURATION VAPOR PRESSURE...
!
      A=(CLIQ-BLIQ*DLIQ)/((TU-DLIQ)*(TU-DLIQ))
      DTFRZ = RLF*QFRZ/(CPP+RLS*QU*A)
      TU = TU+DTFRZ
      
      ES = ALIQ*EXP((BLIQ*TU-CLIQ)/(TU-DLIQ))
      QS = ES*0.622/(P-ES)
!
!...FREEZING WARMS THE AIR AND IT BECOMES UNSATURATED...ASSUME THAT SOME OF THE 
!...LIQUID WATER THAT IS AVAILABLE FOR FREEZING EVAPORATES TO MAINTAIN SATURA-
!...TION...SINCE THIS WATER HAS ALREADY BEEN TRANSFERRED TO THE ICE CATEGORY,
!...SUBTRACT IT FROM ICE CONCENTRATION, THEN SET UPDRAFT MIXING RATIO AT THE NEW
!...TEMPERATURE TO THE SATURATION VALUE...
!
      DQEVAP = QS-QU
      QICE = QICE-DQEVAP
      QU = QU+DQEVAP
      PII=(1.E5/P)**(0.2854*(1.-0.28*QU))
      THTEU=TU*PII*EXP((3374.6525/TU-2.5403)*QU*(1.+0.81*QU))
!
   END SUBROUTINE DTFRZNEW
! --------------------------------------------------------------------------------

      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ,           &
                          QNEWIC,QLQOUT,QICOUT,G)

!-----------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 add to avoid memory issues
!-----------------------------------------------------------------------
!  9/18/88...THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME US
!  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS CAL-
!  CULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A QUASI-
!  CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON VERTICAL
!  RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).

      REAL, INTENT(IN   )   :: G
      REAL, INTENT(IN   )   :: DZ,BOTERM,ENTERM,RATE
      REAL, INTENT(INOUT)   :: QLQOUT,QICOUT,WTW,QLIQ,QICE,QNEWLQ,QNEWIC
      REAL :: QTOT,QNEW,QEST,G1,WAVG,CONV,RATIO3,OLDQ,RATIO4,DQ,PPTDRG
!
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
      CONV=RATE*DZ/WAVG               ! KF90  Eq. 9
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
      QTOT=QTOT*EXP(-CONV)            ! KF90  Eq. 9                                              
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

! ----------------------------------------------------------------------
   SUBROUTINE PROF5(EQ,EE,UD)                                        
!
!***********************************************************************
!*****    GAUSSIAN TYPE MIXING PROFILE....******************************
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!  THIS SUBROUTINE INTEGRATES THE AREA UNDER THE CURVE IN THE GAUSSIAN  
!  DISTRIBUTION...THE NUMERICAL APPROXIMATION TO THE INTEGRAL IS TAKEN FROM
!  "HANDBOOK OF MATHEMATICAL FUNCTIONS WITH FORMULAS, GRAPHS AND MATHEMATICS TABLES"
!  ED. BY ABRAMOWITZ AND STEGUN, NATL BUREAU OF STANDARDS APPLIED
!  MATHEMATICS SERIES.  JUNE, 1964., MAY, 1968.                         
!                                     JACK KAIN                         
!                                     7/6/89                            
!  Solves for KF90 Eq. 2
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!-----------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 add to avoid memory issues
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: EQ
   REAL,         INTENT(INOUT)   :: EE,UD
   REAL ::       SQRT2P,A1,A2,A3,P,SIGMA,FE,X,Y,EY,E45,T1,T2,C1,C2

      DATA SQRT2P,A1,A2,A3,P,SIGMA,FE/2.506628,0.4361836,-0.1201676,       &
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
        UD=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*(0.5+EQ*EQ/2.-    &
           EQ)                                                          
      ELSE                                                              
        EE=SIGMA*(0.5*(EY*C2-E45*C1)+SIGMA*(E45-EY))-E45*EQ*EQ/2.       
        UD=SIGMA*(0.5*(SQRT2P-E45*C1-EY*C2)+SIGMA*(E45-EY))-E45*(0.5+EQ*   &
           EQ/2.-EQ)                                                    
      ENDIF                                                             
      EE=EE/FE                                                          
      UD=UD/FE                                                          

   END SUBROUTINE PROF5

! ------------------------------------------------------------------------
   SUBROUTINE TPMIX2DD(p,thes,ts,qs,i,j)
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
!  SAVE !TWG 2017 add to avoid memory issues
!-----------------------------------------------------------------------
   REAL,         INTENT(IN   )   :: P,THES
   REAL,         INTENT(INOUT)   :: TS,QS
   INTEGER,      INTENT(IN   )   :: i,j     ! avail for debugging
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

! -----------------------------------------------------------------------
  SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,ALIQ,BLIQ,CLIQ,DLIQ)                       
!
!-----------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 add to avoid memory issues
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
  END SUBROUTINE ENVIRTHT                                                              
! ***********************************************************************
!====================================================================
   SUBROUTINE mskf_init(RTHCUTEN,RQVCUTEN,RQCCUTEN,RQRCUTEN,        &
                     RQICUTEN,RQSCUTEN,NCA,W0AVG,P_QI,P_QS,         &
                     SVP1,SVP2,SVP3,SVPT0,                          &
                     P_FIRST_SCALAR,restart,allowed_to_read,        &
                     ids, ide, jds, jde, kds, kde,                  &
                     ims, ime, jms, jme, kms, kme,                  &
                     its, ite, jts, jte, kts, kte                   )
!--------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 add to avoid memeory issues
!--------------------------------------------------------------------
   LOGICAL , INTENT(IN)           ::  restart,allowed_to_read
   INTEGER , INTENT(IN)           ::  ids, ide, jds, jde, kds, kde, &
                                      ims, ime, jms, jme, kms, kme, &
                                      its, ite, jts, jte, kts, kte
   INTEGER , INTENT(IN)           ::  P_QI,P_QS,P_FIRST_SCALAR


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
         RQCCUTEN(i,k,j)=0.
         RQRCUTEN(i,k,j)=0.
      ENDDO
      ENDDO
      ENDDO

      IF (P_QI .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQICUTEN(i,k,j)=0.
         ENDDO
         ENDDO
         ENDDO
      ENDIF

      IF (P_QS .ge. P_FIRST_SCALAR) THEN
         DO j=jts,jtf
         DO k=kts,ktf
         DO i=its,itf
            RQSCUTEN(i,k,j)=0.
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
 
   CALL MSKF_LUTAB(SVP1,SVP2,SVP3,SVPT0)


   END SUBROUTINE mskf_init

!-------------------------------------------------------

      subroutine mskf_lutab(SVP1,SVP2,SVP3,SVPT0)
!
!  This subroutine is a lookup table.
!  Given a series of series of saturation equivalent potential 
!  temperatures, the temperature is calculated.
!
!--------------------------------------------------------------------
   IMPLICIT NONE
!  SAVE !TWG 2017 add to avoid memory issues
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
   END SUBROUTINE MSKF_LUTAB

END MODULE module_cu_mskf
