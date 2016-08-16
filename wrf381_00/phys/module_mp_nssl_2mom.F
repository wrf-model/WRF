!WRF:MODEL_LAYER:PHYSICS







!---------------------------------------------------------------------
! IMPORTANT: Best results are attained using the new 5th-order WENO advection option (4) for scalars:
! moist_adv_opt                       = 4,
! scalar_adv_opt                      = 4,
! (WENO = Weighted Essentially Non-Oscillatory)
!
! This module provides a 2-moment bulk microphysics scheme originally 
! developed by Conrad Ziegler (Zeigler, 1985, JAS) and modified/upgraded in 
! in Mansell, Zeigler, and Bruning (2010, JAS).  Two-moment adaptive sedimentation 
! follows Mansell (2010, JAS), using parameter infall = 4.
!
! Added info on graupel density and soaking is in Mansell and Ziegler (2013, JAS)
!
! Average graupel particle density is predicted, which affects fall speed as well. 
! Hail density prediction is by default disabled in this version, but may be enabled
! at some point if there is interest.
!
! Maintainer: Ted Mansell, National Severe Storms Laboratory <ted.mansell@noaa.gov>
!
! Microphysics References:
!
! Mansell, E. R., C. L. Ziegler, and E. C. Bruning, 2010: Simulated electrification of a small 
!   thunderstorm with two-moment bulk microphysics. J. Atmos. Sci., 67, 171-194, doi:10. 1175/2009JAS2965.1.
!
! Ziegler, C. L., 1985: Retrieval of thermal and microphysical variables in observed convective storms. 
!    Part I: Model development and preliminary testing. J. Atmos. Sci., 42, 1487-1509.
!
! Sedimentation reference:
!
! Mansell, E. R., 2010: On sedimentation and advection in multimoment bulk microphysics. 
!    J. Atmos. Sci., 67, 3084-3094, doi:10.1175/2010JAS3341.1.
!
! Possible parameters to adjust:
!
!  ccn : base cloud condensation nuclei concentration (use namelist.input value "nssl_cccn")
!  alphah, alphahl : Size distribution shape parameters for graupel (h) and hail (hl)
!  infall : changes sedimentation options to see effects (see below)
!
! lightning model references:
!
!    Fierro, A. O., E.R. Mansell, C. Ziegler and D. R. MacGorman 2013: The
!    implementation of an explicit charging and discharge lightning scheme
!    within the WRF-ARW model: Benchmark simulations of a continental squall line, a
!    tropical cyclone and a winter storm. Monthly Weather Review, Volume 141, 2390-2415 
!
!    Mansell et al. 2005: Charge structure and lightning sensitivity in a simulated 
!     multicell thunderstorm. J. Geophys. Res., 110, D12101, doi:10.1029/2004JD005287
!
! Note: Some parameters below apply to unreleased features.
!
! WRF 3.8 updates:
!   Fixed issue with reflectivity conservation for graupel melting into rain. Rain number concentrations were too low,
!       resulting in excessive reflectivity of a couple dBZ
!   Changed default value of iusewetgraupel to 1 (turns off diagnostic meltwater on graupel for reflectivity)
!   Apply a 70 m/s fall speed limit for sedimentation
!   Changed vapor ice nucleation to Meyers-Ferrier method (original scheme)
!   New method for Bigg freezing (ibiggopt=2)
!   Reduced snow aggregration efficiency and restricted aggregation to higher temperatures (assuming dendrites and mechanical aggregation)
!   Increased maximum graupel-droplet collection efficiency when hail is turned off (nssl_2momg)
!   Updates for compatibility with WRF-NMM
!   Added calculation of hail number concentration in calcnfromq (creates number concentration from mixing ratio
!       when starting from an analysis). And fixed error in graupel intercept
!   Bug fix in snow fall speeds
!   Further fix in snow reflectivity
!   Use diameter of maximum mass rather than mean diamter when checking maximum size
!   Helped performance in sedimentation with flag "do_accurate_sedimentation" to control recalculation of fall speeds when
!       more than one sub-time step is needed (often happens with large time steps and small dz near the ground):
!        = .true. : recalculates fall speed after each substep (more accurate)
!        = .false. : (default) reuses fall speeds calculated on the first substep (typical for most schemes), theoretically could cause an occasional glitch, but none seen in practice
!   Increased maximum mean droplet radius from 40 to 60 microns, which alleviates spurious number concentration increases at low CCN concentration.
!   Removed a duplicate factor from hail reflectivity that was causing a loss of about 6 dBZ (since WRF 3.5).
!
!---------------------------------------------------------------------



MODULE module_mp_nssl_2mom

  IMPLICIT NONE
  
  public nssl_2mom_driver
  public nssl_2mom_init
  private gamma,gamxinf,GAML02, GAML02d300, GAML02d500, fqvs, fqis
  private gamma_dp, gamxinfdp
  private delbk, delabk
  private gammadp
  
  logical, public :: cleardiag = .false.
  PRIVATE

#ifdef WRF_CHEM
  integer, parameter :: wrfchem_flag = 1
#else
  integer, parameter :: wrfchem_flag = 0
#endif
  
  integer, private :: eqtset = 1 ! Flag for use with cm1 to use alternate equation set (changes latent heating rates)
   double precision, parameter, public :: zscale = 1.0d0 ! 1.000e-10
   double precision, parameter, public :: zscaleinv = 1.0d0/zscale ! 1.000e-10

  
  real, parameter :: warmonly = 0.0 ! testing parameter, set to 1.0 to reduce to warm-rain physics (ice variables stay zero)
  
  logical, parameter :: lwsm6 = .false. ! act like wsm6 for some single moment interactions

! some constants from WSM6
  real, parameter  :: dimax = 500.e-6    ! limited maximum value for the cloud-ice diamter
  real, parameter  :: roqimax = 2.08e22*dimax**8
  
! Params for dbz:
  integer  :: iuseferrier = 1  ! =1: use dry graupel only from Ferrier 1994; = 0: Use Smith (wet graupel)
  integer  :: idbzci      = 0
 ! new version  integer  :: iusewetgraupel = 1 ! =1 to turn on use of QHW for graupel reflectivity (only for ZVDM -- mixedphase)
  integer  :: iusewetgraupel = 1 ! =1 to turn on use of QHW for graupel reflectivity (only for ZVDM -- mixedphase)
                                 ! =2 turn on for graupel density less than 300. only 
  integer  :: iusewethail = 0 ! =1 to turn on use of QHW for graupel reflectivity (only for ZVDM -- mixedphase)
  integer  :: iusewetsnow = 1 ! =1 to turn on diagnosed bright band

! microphysics

  real, private :: rho_qr = 1000., cnor = 8.0e5  ! cnor is set in namelist!!  rain params
  real, private :: rho_qs =  100., cnos = 3.0e6  ! set in namelist!!  snow params
  real, private :: rho_qh =  500., cnoh = 4.0e5  ! set in namelist!!  graupel params
  real, private :: rho_qhl=  900., cnohl = 4.0e4 ! set in namelist!!  hail params

  real, private :: hdnmn  = 170.0  ! minimum graupel density (for variable density graupel)
  real, private :: hldnmn = 500.0  ! minimum hail density (for variable density hail)

  real :: cnohmn  = 1.e-2 ! minimum intercept for 2-moment graupel (alphah < 0.5)
  real :: cnohlmn = 1.e-2 ! minimum intercept for 2-moment hail (alphahl < 0.5)
  
! Autoconversion parameters

  real   , private :: qcmincwrn      = 2.0e-3    ! qc threshold for autonconversion (LFO; for 10ICE use qminrncw for ircnw != 5)
  real   , private :: cwdiap         = 20.0e-6   ! threshold diameter of cloud drops (Ferrier 1994 autoconversion)
  real   , private :: cwdisp         = 0.15      ! assume droplet dispersion parameter (can be 0.3 for maritime)
  real   , private  :: ccn            = 0.6e+09   ! set in namelist!! Central plains CCN value
  real   , private :: qccn             ! ccn "mixing ratio"
  integer, private :: iauttim        = 1         ! 10-ice rain delay flag
  real   , private :: auttim         = 300.      ! 10-ice rain delay time
  real   , private :: qcwmntim       = 1.0e-5    ! 10-ice rain delay min qc for time accrual


! sedimentation flags
! itfall -> 0 = 1st order fallout (other options removed)
! iscfall, infall -> fallout options for charge and number concentration, respectively
!                    1 = mass-weighted fall speed; 2 = number-weighted fallspeed.
  integer, private :: itfall = 0
  integer, private :: iscfall = 1
  integer, private :: irfall = -1
  logical, private :: do_accurate_sedimentation = .false. ! if true, recalculate fall speeds on sub time steps; (more expensive)
                                                         ! if false, reuse fall speeds on multiple steps (can have a noticeable speedup)
                                                         ! Mainly is an issue for small dz near the surface. 
  integer, private :: infall = 4   ! 0 -> uses number-wgt for N; NO correction applied (results in excessive size sorting)
                          ! 1 -> uses mass-weighted fallspeed for N ALWAYS
                          ! 2 -> uses number-wgt for N and mass-weighted correction for N (Method II in Mansell, 2010 JAS)
                          ! 3 -> uses number-wgt for N and Z-weighted correction for N (Method I in Mansell, 2010 JAS)
                          ! 4 -> Hybrid of 2 and 3: Uses minimum N from each method (z-wgt and m-wgt corrections) (Method I+II in Mansell, 2010 JAS)
                          ! 5 -> uses number-wgt for N and uses average of N-wgt and q-wgt instead of Max.
  real    :: rainfallfac = 1.0 ! factor to adjust rain fall speed (single moment only)
  integer, private :: icdx = 3 ! (graupel) 0=Ferrier; 1=leave drag coef. cd fixed; 2=vary by density, 4=set by user with cdxmin,cdxmax,etc.
  integer, private :: icdxhl = 3 ! (hail) 0=Ferrier; 1=leave drag coef. cd fixed; 2=vary by density, 4=set by user with cdxmin,cdxmax,etc.
  real   , private :: cdhmin = 0.45, cdhmax = 0.8        ! defaults for graupel (icdx=4)
  real   , private :: cdhdnmin = 500., cdhdnmax = 800.0  ! defaults for graupel (icdx=4)
  real   , private :: cdhlmin = 0.45, cdhlmax = 0.6      ! defaults for hail (icdx=4)
  real   , private :: cdhldnmin = 500., cdhldnmax = 800.0  ! defaults for hail (icdx=4)
  real   , private :: vtmaxsed = 70. ! Limit on fall speed (m/s, all moments) for sedimentation calculations. Not applied to fall speeds for microphysical rates
  
  integer :: rssflg = 1   ! Rain size-sorting allowed (1, default), or disallowed (0).  If 0, sets N and Z-weighted fall speeds to q-weighted value
  integer :: sssflg = 1   ! As above but for snow
  integer :: hssflg = 1   ! As above but for graupel
  integer :: hlssflg = 1  ! As above but for hail

! input flags

  integer, private :: ndebug = -1, ncdebug = 0
  integer, private :: ipconc = 5
  integer, private :: ichaff = 0
  integer, private :: ilimit = 0
  
  real, private :: constccw = -1.

  real, private :: cimn = 1.0e3, cimx = 1.0e6


  real   , private :: ifrzg = 1.0 ! fraction of frozen drops going to graupel. 1=freeze all rain to graupel, 0=freeze all to hail
  real   , private :: ifrzs = 1.0 ! fraction of small frozen drops going to snow. 1=freeze rain to snow, 0=freeze to cloud ice
  real   , private :: ffrzs = 0.0 ! fraction of other initiated cloud ice going to snow. 1=freeze rain to snow, 0=freeze to cloud ice
  integer, private :: irwfrz = 1 ! compute total rain that can freeze (checks heat budget)
  integer, private :: irimtim = 0 ! future use
!  integer, private :: infdo = 1   ! 1 = calculate number-weighted fall speeds

  real   , private :: rimc1 = 300.0, rimc2 = 0.44  ! rime density coeff. and power (Default Heymsfield and Pflaum, 1985)
  real   , private :: rimc3 = 170.0                ! minimum rime density
  real    :: rimc4 = 900.0                ! maximum rime density
  real   , private :: rimtim = 120.0               ! cut-off rime time (10ICE)
  real   , private :: eqtot = 1.0e-9               ! threshold for mass budget reporting

  integer, private :: ireadmic = 0

  integer, private :: iccwflg = 1     ! sets max size of first droplets in parcel to 4 micron radius (in two-moment liquid)
                             ! (first nucleation is done with a KW sat. adj. step)
  integer, private :: issfilt = 0     ! flag to turn on filtering of supersaturation field
  integer, private :: irenuc = 2      ! =1 to always allow renucleation of droplets within the cloud
                                      ! =2 renucleation following Twomey/Cohard&Pinty
                             ! i.e., not only at cloud base
  integer, private :: irenuc3d = 0      ! =1 to include horizontal gradient in renucleation of droplets within the cloud
  real    :: renucfrac = 0.0 ! = 0 : cnuc = cwccn
                             ! = 1 : cnuc = actual available CCN
                             ! otherwise cnuc = cwccn*(1. - renufrac) + ccnc(1:ngscnt)*renucfrac
  real   , private :: cck = 0.6       ! exponent in Twomey expression
  real   , private :: ciintmx = 1.0e6 ! limit on ice concentration from primary nucleation

  real   , private :: cwccn ! , cwmasn,cwmasx
  real   , private :: ccwmx

  integer, private :: idocw = 1, idorw = 1, idoci = 1, idoir = 1, idoip = 1, idosw = 1
  integer, private :: idogl = 1, idogm = 1, idogh = 1, idofw = 1, idohw = 1, idohl = 1
!  integer, private :: ido(3:14) = / 12*1 /


! 0,2, 5.00e-10, 1, 0, 0, 0      : itype1,itype2,cimas0,icfn,ihrn,ibfc,iacr
  integer, private :: itype1 = 0, itype2 = 2  ! controls Hallett-Mossop process
  integer, private :: icenucopt = 1       ! =1 Meyers/Ferrier primary ice nucleation; =2 Thompson/Cooper, =3 Phillips (Meyers/Demott)
  integer, private :: icfn = 2                ! contact freezing: 0 = off; 1 = hack (ok for single moment); 2 = full Cotton/Meyers version
  integer, private :: ihrn = 0            ! Hobbs-Rangno ice multiplication (Ferrier, 1994; use in 10-ice only)
  integer, private :: ibfc = 1            ! Flag to use Bigg freezing on droplets (0 = off (uses alternate freezing), 1 = on)
  integer, private :: iacr = 2            ! Flag for drop contact freezing with crytals
                                 ! (0=off; 1=drops > 500micron diameter; 2 = > 300micron)
  integer, private :: ibfr = 2            ! Flag for Bigg freezing conversion of freezing drops to graupel
                                 ! (1=min graupel size is vr1mm; 2=use min size of dfrz, 5= as for 2 and apply dbz conservation)
  integer, private :: ibiggopt = 2        ! 1 = old Bigg; 2 = experimental Bigg (only for imurain = 1, however)
  integer, private :: iacrsize = 5        ! assumed min size of drops freezing by capture
                                 !  1: > 500 micron diam
                                 !  2: > 300 micron
                                 !  3: > 40 micron
                                 !  4: all sizes
                                 !  5: > 150 micron (only for imurain = 1)
  real   , private :: cimas0 = 6.62e-11   ! default mass of Hallett-Mossop crystals
                                 ! 6.62e-11kg results in half the diam. (60 microns) of old default value of 5.0e-10
  real   , private :: cimas1 = 6.88e-13   ! default mass of new ice crystals
  real   , private :: splintermass = 6.88e-13
  real   , private :: cfnfac = 0.1        ! Hack factor that goes with icfn=1
  integer, private :: iscni = 4           ! default option for ice crystal aggregation/conversion to snow
  logical, private :: imeyers5 = .false.  ! .false.=off, true=on for Meyers ice nucleation for temp > -5 C
  real   , private :: dmincw = 15.0e-6    ! minimum droplet diameter for collection for iehw=3
  integer, private :: iehw = 1            ! 0 -> ehw=ehw0; 1 -> old ehw; 2 -> test ehw with Mason table data
  integer, private :: iehlw = 1           ! 0 -> ehlw=ehlw0; 1 -> old ehlw; 2 -> test ehlw with Mason table data
                                 ! For ehw/ehlw = 1, ehw0/ehlw0 act as maximum limit on collection efficiency (defaults are 1.0)
  integer, private :: ierw = 1            ! for single-moment rain (LFO/Z)
  real   , private :: ehw0 = 0.5          ! constant or max assumed graupel-droplet collection efficiency
  real   , private :: erw0 = 1.0          ! constant assumed rain-droplet collection efficiency
  real   , private :: ehlw0 = 0.75        ! constant or max assumed hail-droplet collection efficiency
  real    :: ehr0 = 1.0          ! constant or max assumed graupel-rain collection efficiency
  real    :: ehlr0 = 1.0         ! constant or max assumed hail-rain collection efficiency
  

  real   , private :: esilfo0 = 1.0       ! factor for LFO collection efficiency of snow for cloud ice.
  real   , private :: ehslfo0 = 1.0       ! factor for LFO collection efficiency of hail/graupel for snow.

  integer, private :: ircnw    = 5        ! single-moment warm-rain autoconversion option.  5= Ferrier 1994.
  real   , private :: qminrncw = 2.0e-3   ! qc threshold for rain autoconversion (NA for ircnw=5)

  integer, private :: iqcinit = 2         ! For ZVDxx schemes, flag to choose which way to initialize droplets
                                 ! 1 = Soong-Ogura adjustment
                                 ! 2 = Saturation adjustment to value of ssmxinit
                                 ! 3 = KW adjustment

  real   , private :: ssmxinit = 0.4      ! saturation percentage to adjust down to for initial cloud
                                 ! formation (ZVDxx scheme only)

  real   , private :: ewfac = 1.0         ! hack factor applied to graupel and hail collection eff. for droplets
  real   , private :: eii0 = 0.1 ,eii1 = 0.1  ! graupel-crystal coll. eff. parameters: eii0*exp(eii1*min(temcg(mgs),0.0))
                                     ! set eii1 = 0 to get a constant value of eii0
  real   , private :: eii0hl = 0.2 ,eii1hl = 0.0  ! hail-crystal coll. eff. parameters: eii0hl*exp(eii1hl*min(temcg(mgs),0.0))
                                     ! set eii1hl = 0 to get a constant value of eii0hl
  real   , private :: eri0 = 0.1   ! rain efficiency to collect ice crystals
  real   , private :: esi0 = 0.1              ! linear factor in snow-ice collection efficiency
  real   , private :: ehs0 = 0.1, ehs1 = 0.1  ! graupel-snow coll. eff. parameters: ehs0*exp(ehs1*min(temcg(mgs),0.0))
                                     ! set ehs1 = 0 to get a constant value of ehs0
  real   , private :: ess0 = 0.5, ess1 = 0.05 ! snow aggregation coefficients: ess0*exp(ess1*min(temcg(mgs),0.0))
                                     ! set ess1 = 0 to get a constant value of ess0
  real   , private :: esstem1 = -10.  ! lower temperature where snow aggregation turns on
  real   , private :: esstem2 = -07.  ! higher temperature for linear ramp of ess from zero at esstem1 to formula value at esstem2
  real   , private :: ehsfrac = 1.0           ! multiplier for graupel collection efficiency in wet growth
  real   , private :: ehimin = 0.0 ! Minimum collection efficiency (graupel - ice crystal)
  real   , private :: ehimax = 1.0 ! Maximum collection efficiency (graupel - ice crystal)
  real   , private :: ehsmax = 0.5 ! Maximum collection efficiency (graupel - snow)
  real   , private :: ecollmx = 0.5 ! Maximum collision efficiency for graup/hail with ice; used only for charging rates
  integer, private :: iglcnvi = 2  ! flag for riming conversion from cloud ice to rimed ice/graupel
  integer, private :: iglcnvs = 2  ! flag for conversion from snow to rimed ice/graupel

  real   , private :: rz          ! reflectivity conservation factor for graupel/rain
                         ! now calculated in icezvd_dr.F from alphah and rnu
                         ! currently only used for graupel melting to rain
  real   , private :: rzhl        ! reflectivity conservation factor for hail/rain
                         ! now calculated in icezvd_dr.F from alphahl and rnu

  real   , private :: rzs     ! reflectivity conservation factor for snow(imusnow=3) with rain (imurain=1)

  real   , private :: alphahacx = 0.0 ! assumed minimum shape parameter for zhacw and zhacr

  real   , private :: fconv = 1.0  ! factor to boost max graupel depletion by riming conversions in 10ICE

  real   , private :: rg0 = 400.0  ! reference graupel density for graupel fall speed

  integer, private :: rcond = 2    ! (Z only) rcond = 2 includes rain condensation in loop with droplet condensation
                                   ! 0 = no condensation on rain; 1 = bulk condensation on rain
  integer, parameter, private :: icond = 1    ! (Z only) icond = 1 calculates ice deposition (crystals and snow) BEFORE droplet condensation
                          ! icond = 2 does not work (intended to calc. dep in loop with droplet cond.)
  
  real   , private :: dfrz = 0.15e-3 ! 0.25e-3  ! minimum diameter of frozen drops from Bigg freezing (used for vfrz) for iacr > 1
                            ! and for ciacrf for iacr=4
  real   , private :: dmlt = 3.0e-3  ! maximum diameter for rain melting from graupel and hail
  real   , private :: dshd = 1.0e-3  ! nominal diameter for drops shed from graupel/hail

  integer, private :: ihmlt = 2      ! 1=old melting with vmlt; 2=new melting using mean volume diam of graupel/hail
  integer, private :: imltshddmr = 1 ! 0 (default)=mean diameter of drops produced during melting+shedding as before (using mean diameter of graupel/hail
                            ! and max mean diameter of rain)
                            ! 1=new method where mean diameter of rain during melting is adjusted linearly downward 
                            ! toward 3 mm for large (> sheddiam) graupel and hail, to take into account shedding of 
                            ! smaller drops.  sheddiam0 controls the size of graupel/hail above which the assumed 
                            ! mean diameter of rain is set to 3 mm
                            ! Only valid for ihmlt = 2 for ZVD(H) but also applies to ZVD(H)M
                            ! 2 = method that sets the resulting rain size ( vshdgs ) according to the mass-weighted diameter of the ice


  integer, private :: nsplinter = 0  ! number of ice splinters per freezing drop, if negative, then per resulting graupel particle
  integer, private :: isnwfrac = 0   ! 0= no snow fragmentation; 1 = turn on snow fragmentation (Schuur, 2000)

!  integer, private :: denscale = 1  ! 1=scale num. conc. and charge by air density for advection, 0=turn off for comparison

  logical, private :: mixedphase = .false.   ! .false.=off, true=on to include mixed phase graupel
  integer, private :: imixedphase = 0
  logical, private :: qsdenmod = .false.     ! true = modify snow density by linear interpolation of snow and rain density
  logical, private :: qhdenmod = .false.     ! true = modify graupel density by linear interpolation of graupel and rain density
  logical, private :: qsvtmod = .false.      ! true = modify snow fall speed by linear interpolation of snow and rain vt
  real   , private :: sheddiam   = 8.0e-03  ! minimum diameter of graupel before shedding occurs
  real    :: sheddiamlg = 10.0e-03  ! diameter of hail to use fwmlarge
  real    :: sheddiam0  = 20.0e-03  ! diameter of hail at which all water is shed

  real   , private :: fwms = 0.5 ! maximum liquid water fraction on snow
  real   , private :: fwmh = 0.5 ! maximum liquid water fraction on graupel
  real   , private :: fwmhl = 0.5 ! maximum liquid water fraction on hail
  real    :: fwmlarge = 0.2 ! maximum liquid water fraction on hail larger than sheddiam
  integer :: ifwmfall = 0   ! whether to interpolate toward rain fall speed for graupel and hail
                            ! when diam < sheddiam and liquid fraction is predicted (0=no, 1=yes)
  
  logical :: rescale_high_alpha = .false.  ! whether to rescale number. conc. when alpha = alphamax (3-moment only)
  logical :: rescale_low_alpha = .true.    ! whether to rescale Z (graupel/hail) when alpha = alphamin (3-moment only)
  logical :: rescale_low_alphar = .true.    ! whether to rescale Z for rain when alpha = alphamin (3-moment only)

  real, parameter :: alpharmax = 8. ! limited for rwvent calculation
  
  integer, private ::  ihlcnh = 1  ! which graupel -> hail conversion to use
                          ! 1 = Milbrandt and Yau (2005) using Ziegler 1985 wet growth diameter
                          ! 2 = Straka and Mansell (2005) conversion using size threshold
  real, private :: hlcnhdia = 1.e-3 ! threshold diameter for graupel -> hail conversion for ihlcnh = 1 option.
  real, private :: hlcnhqmin = 0.1e-3 ! minimum graupel mass content for graupel -> hail conversion (ihlcnh = 1)
  real   , private :: hldia1 = 20.0e-3  ! threshold diameter for graupel -> hail conversion for ihlcnh = 2 option.
  integer :: icvhl2h = 0   ! allow conversion of hail back to graupel when hail density gets close to minimum allowed

  integer, private :: imurain = 1 ! 3 for gamma-volume, 1 for gamma-diameter DSD for rain.
  integer, private :: imusnow = 3 ! 3 for gamma-volume, 1 for gamma-diameter DSD for snow (=1 NOT IMPLEMENTED!!).
  integer, private :: iturbenhance = 0 ! warm-rain collision enhancement
                              ! 1 = enhance autoconversion only
                              ! 2 = add rain collection of cloud
                              ! 3 = add rain self-collection
  integer, private :: isedonly = 0 ! 1= only do sedimentation and skip other microphysics
  integer, private :: iferwisventr = 2 ! =1 for Ferrier rwvent, =2 for Wisner rwvent (imurain=1)
  integer, private :: izwisventr   = 2 ! =1 for old Ziegler rwvent, =2 for Wisner-style rwvent (imurain=3)
  integer :: iresetmoments = 0 ! if >0, then set all moments to zero when one of them is zero (3-moment only)
  integer, private :: imaxdiaopt    = 3 ! = 1 use mean diameter for breakup
                               ! = 2 use maximum mass diameter for breakup
                               ! = 3 use mass-weighted diameter for breakup
  integer, private :: dmrauto       = 0 ! = -1 no limiter on crcnw
                              ! =  0 limit crcnw when qr > 1.2*L (Cohard-Pinty 2002)
                              ! =  1 DTD version based on MY code
                              ! =  2 DTD mass-weighted version based on MY code
                              ! =  3 Milbrandt version (from Cohard and Pinty's code
  real, parameter :: alpharaut = 0.0 ! MY2005 for autoconversion
  real    :: cxmin = 1.e-4  ! threshold cutoff for number concentration
  real    :: zxmin = 1.e-28 ! threshold cutoff for reflectivity moment
  
  integer :: ithompsoncnoh = 0 ! For single moment graupel only
                           ! 0 = fixed intercept
                           ! 1 = intercept based on graupel mass

  integer :: ivhmltsoak = 1   ! 0=off, 1=on : flag to simulate soaking (graupel/hail) during melting 
                         ! when liquid fraction is not predicted
  integer, private :: ioldlimiter = 0 ! test switch for new(=0) or old(=1) size limiter at the end of GS for 3-moment categories
  integer, private  :: isnowfall = 2   ! Option for choosing between snow fall speed parameters
                         ! 1 = original Zrnic et al. (Mansell et al. 2010)
                         ! 2 = Ferrier 1994 (results in slower fall speeds)
  
  integer, private  :: ibiggsnow   = 3 ! 1 = switch conversion over to snow for small frozen drops from Bigg freezing
                                       ! 2 = switch conversion over to snow for small frozen drops from rain-ice interaction
                                       ! 3 = switch conversion over to snow for small frozen drops from both
  
  integer, private  :: ixtaltype = 1 ! =1 column, =2 disk (similar to Takahashi)

  real, private  :: takshedsize1 = 0.15 ! diameter (cm) of drop shed from ice with D > 1.9 cm
  real, private  :: takshedsize2 = 0.3 ! diameter (cm) of drop shed from ice with D < 1.9 cm and D > 0.8 cm
  
  real, private     :: evapfac     = 1.0 ! Multiplier on rain evaporation rate
  real, private     :: depfac      = 1.0 ! Multiplier on graupel/hail deposition/sublimation rate

  integer, private :: ibinhmlr = 0  ! =1 use incomplete gammas to determine melting from larger and smaller sizes of graupel, and appropriate shed drop sizes 
                           ! =2 to test melting by temporary bins
  integer, private :: ibinhlmlr = 0  ! =1 use incomplete gammas to determine melting from larger and smaller sizes of hail, and appropriate shed drop sizes 
                            ! =2 to test melting by temporary bins
  

  integer, parameter :: icespheres = 0 ! turn ice spheres (frozen droplets) on (1) or off (0). NOT COMPLETE IN WRF/ARPS/CM1 CODE!
  integer, parameter :: lqmx = 30
  integer, parameter :: lt = 1
  integer, parameter :: lv = 2
  integer, parameter :: lc = 3
  integer, parameter :: lr = 4
  integer, parameter :: li = 5
  integer, private :: lis = 0
  integer, private :: ls = 6
  integer, private :: lh = 7
  integer, private :: lhl = 0

  integer, private  :: lccn = 9 ! 0 or 9, other indices adjusted accordingly
  integer, private :: lccna = 0
  integer, private :: lcina = 0
  integer, private :: lcin = 0
  integer, private :: lnc = 9
  integer, private :: lnr = 10
  integer, private :: lni = 11
  integer, private :: lnis = 0
  integer, private :: lns = 12
  integer, private :: lnh = 13
  integer, private :: lnhl = 0
  integer, private :: lss = 0
  integer :: lvh = 15

  integer, private :: lhab = 8
  integer, private :: lg = 7

! Particle volume

  integer :: lvi = 0
  integer :: lvs = 0
  integer :: lvgl = 0
  integer :: lvgm = 0
  integer :: lvgh = 0
  integer :: lvf = 0
!  integer :: lvh = 16
  integer :: lvhl = 0

! liquid water fraction (not predicted here but tested for)
  integer :: lhw = 0
  integer :: lsw = 0
  integer :: lhlw = 0

! reflectivity (6th moment) ! not predicted here but may be tested against

  integer :: lzr = 0
  integer :: lzi = 0
  integer :: lzs = 0
  integer :: lzgl = 0
  integer :: lzgm = 0
  integer :: lzgh = 0
  integer :: lzf = 0
  integer :: lzh = 0
  integer :: lzhl = 0

! Space charge

  integer :: lscw = 0
  integer :: lscr = 0
  integer :: lsci = 0
  integer :: lscis = 0
  integer :: lscs = 0
  integer :: lsch = 0
  integer :: lschl = 0
  integer :: lscwi = 0
  integer :: lscpi = 0
  integer :: lscni = 0
  integer :: lscpli = 0
  integer :: lscnli = 0
  integer :: lschab = 0

  integer :: lscb = 0
  integer :: lsce = 0
  integer :: lsceq = 0

!  integer, parameter :: lscmx = 100

  integer :: lne = 0 ! last varible for transforming

  real :: cnoh0 = 4.0e+5
  real :: hwdn1 = 700.0

  real    :: alphai  = 0.0 ! shape parameter for ZIEG ice crystals ! not currently used
  real    :: alphas  = 0.0 ! shape parameter for ZIEG snow         ! used only for single moment
  real    :: alphar  = 0.0 ! shape parameter for rain (imurain=1 only)
  real, private    :: alphah  = 0.0 ! set in namelist!! shape parameter for ZIEG graupel
  real, private    :: alphahl = 1.0 ! set in namelist!! shape parameter for ZIEG hail

  real    :: dmuh    = 1.0  ! power in exponential part (graupel)
  real    :: dmuhl   = 1.0  ! power in exponential part (hail)

  real, parameter :: alphamax = 15.
  real, parameter :: alphamin = 0.
  real, parameter :: rnumin = -0.8
  real, parameter :: rnumax = 15.0

  
  real            :: cnu = 0.0
  real, parameter :: rnu = -0.8, snu = -0.8, cinu = 0.0
!      parameter ( cnu = 0.0, rnu = -0.8, snu = -0.8, cinu = 0.0 )
  
  real xnu(lc:lqmx) ! 1st shape parameter (mass)
  real xmu(lc:lqmx) ! 2nd shape parameter (mass)
  real dnu(lc:lqmx) ! 1st shape parameter (diameter)
  real dmu(lc:lqmx) ! 2nd shape parameter (diameter)
  
  real ax(lc:lqmx)
  real bx(lc:lqmx)
  real fx(lc:lqmx)

      real da0 (lc:lqmx)          ! collection coefficients from Seifert 2005
      real dab0(lc:lqmx,lc:lqmx)  ! collection coefficients from Seifert 2005
      real dab1(lc:lqmx,lc:lqmx)  ! collection coefficients from Seifert 2005
      real da1 (lc:lqmx)          ! collection coefficients from Seifert 2005
      real bb  (lc:lqmx)

! put ipelec here for now....
  integer :: ipelec = 0
  integer :: isaund = 0
  logical :: idonic = .false.
  integer :: jchgs = 3  ! number of points near boundary where charging is turned off (to keep lightning from getting wonky)
  integer :: jchgn = 2
  integer :: ichge = 3
  integer :: ichgw = 2
  real    :: charging_border = 4000. ! width of no-charging zone from boundary
      real, private    :: delqnw = -1.0e-10!-1.0e-12 !
      real, private    :: delqxw =  1.0e-10! 1.0e-12 !
      real :: tindmn = 233, tindmx = 298.0  ! min and max temperatures where inductive charging is allowed

!
!  gamma function lookup table
!
      integer ngm0,ngm1,ngm2
      parameter (ngm0=3001,ngm1=500,ngm2=500)
      double precision, parameter :: dgam = 0.01, dgami = 100.
      double precision gmoi(0:ngm0) ! ,gmod(0:ngm1,0:ngm2),gmdi(0:ngm1,0:ngm2)

      integer, parameter :: nqiacralpha =  240 !480 ! 240 ! 120 ! 15
      integer, parameter :: nqiacrratio =  100 ! 500 !50  ! 25
      real,    parameter :: maxratiolu = 25.
      real,    parameter :: maxalphalu = 15.
      real,    parameter :: dqiacralpha = maxalphalu/Float(nqiacralpha), dqiacrratio = maxratiolu/Float(nqiacrratio) 
      real,    parameter :: dqiacrratioinv = 1./dqiacrratio, dqiacralphainv = 1./dqiacralpha
      real :: ciacrratio(0:nqiacrratio,0:nqiacralpha)
      real :: qiacrratio(0:nqiacrratio,0:nqiacralpha)
      real :: ziacrratio(0:nqiacrratio,0:nqiacralpha)
      double precision :: gamxinflu(0:nqiacrratio,0:nqiacralpha,10,2) ! last index for graupel (1) or hail (2)

    integer, parameter :: ngdnmm = 9
    real :: mmgraupvt(ngdnmm,3)  ! Milbrandt and Morrison (2013) fall speed coefficients for graupel/hail

    DATA mmgraupvt(:,1) / 50., 150., 250., 350., 450., 550., 650., 750., 850./
    DATA mmgraupvt(:,2) / 62.923, 94.122, 114.74, 131.21, 145.26, 157.71, 168.98, 179.36, 189.02 /
    DATA mmgraupvt(:,3) / 0.67819, 0.63789, 0.62197, 0.61240, 0.60572, 0.60066, 0.59663, 0.59330, 0.59048 /

      integer lsc(lc:lqmx)
      integer ln(lc:lqmx)
      integer ipc(lc:lqmx)
      integer lvol(lc:lqmx)
      integer lz(lc:lqmx)
      integer lliq(li:lqmx)
      integer denscale(lc:lqmx) ! flag for density scaling (mixing ratio conversion)

      integer ido(lc:lqmx)
      logical ldovol

      real xdn0(lc:lqmx)
      real xdnmx(lc:lqmx), xdnmn(lc:lqmx)
      real cdx(lc:lqmx)
      real cno(lc:lqmx)
      real xvmn(lc:lqmx), xvmx(lc:lqmx)
      real qxmin(lc:lqmx)

      integer nqsat
      parameter (nqsat=1000001) ! (nqsat=20001)
      real fqsat,fqsati
      parameter (fqsat=0.002,fqsati=1./fqsat)
      real tabqvs(nqsat),tabqis(nqsat),dtabqvs(nqsat),dtabqis(nqsat)

!
!  constants
!
      real, parameter :: cp608 = 0.608          ! constant used in conversion of T to Tv
      real, parameter :: ar = 841.99666         ! rain terminal velocity power law coefficient (LFO)
      real, parameter :: br = 0.8               ! rain terminal velocity power law coefficient (LFO)
      real, parameter :: aradcw = -0.27544      !
      real, parameter :: bradcw = 0.26249e+06   !
      real, parameter :: cradcw = -1.8896e+10   !
      real, parameter :: dradcw = 4.4626e+14    !
      real, parameter :: bta1 = 0.6             ! beta-1 constant used for ice nucleation by deposition (Ferrier 94, among others)
      real, parameter :: cnit = 1.0e-02         ! No for ice nucleation by deposition (Cotton et al. 86)
      real, parameter :: dragh = 0.60           ! coefficient used to adjust fall speed for hail versus graupel (Pruppacher and Klett 78)
      real, parameter :: dnz00 = 1.225          ! reference/MSL air density
      real, parameter :: rho00 = 1.225          ! reference/MSL air density
!      cs = 4.83607122       ! snow terminal velocity power law coefficient (LFO)
!      ds = 0.25             ! snow terminal velocity power law coefficient (LFO)
!  new values for  cs and ds
      real, parameter :: cs = 12.42             ! snow terminal velocity power law coefficient 
      real, parameter :: ds = 0.42              ! snow terminal velocity power law coefficient 
      real, parameter :: pi = 3.141592653589793
      real, parameter :: piinv = 1./pi
      real, parameter :: pid4 = pi/4.0

      real, parameter :: gr = 9.8

!
! max and min mean volumes
!
      real xvrmn, xvrmx0  ! min, max rain volumes
      real xvsmn, xvsmx  ! min, max snow volumes
      real xvfmn, xvfmx  ! min, max frozen drop volumes
      real xvgmn, xvgmx  ! min, max graupel volumes
      real xvhmn, xvhmn0, xvhmx, xvhmx0  ! min, max hail volumes
      real xvhlmn, xvhlmx  ! min, max lg hail volumes

      real, parameter :: dhlmn = 0.3e-3, dhlmx = 40.e-3
      real, parameter :: dhmn0 = 0.3e-3
      real, private :: dhmn = dhmn0, dhmx = -1.

      real, parameter :: cwradn = 2.5e-6, xcradmn = cwradn    ! minimum radius
      real, parameter :: cwradx = 60.e-6, xcradmx = cwradx    ! maximum radius
      real, parameter :: cwc1 = 6.0/(pi*1000.)

!      parameter( xvcmn=4.188e-18 )   ! mks  min volume = 3 micron radius
      real, parameter :: xvcmn=0.523599*(2.*cwradn)**3    ! mks  min volume = 2.5 micron radius
      real, parameter :: xvcmx=0.523599*(2.*xcradmx)**3    ! mks  min volume = 2.5 micron radius
      real, parameter :: cwmasn = 1000.*xvcmn   ! minimum mass, defined by radius of 5.0e-6
      real, parameter :: cwmasx = 1000.*xvcmx   ! maximum mass, defined by radius of 50.0e-6
      real, parameter :: cwmasn5 = 1000.*0.523599*(2.*5.0e-6)**3 !  5.23e-13

      real, parameter :: xvimn=0.523599*(2.*5.e-6)**3    ! mks  min volume = 5 micron radius
      real, parameter :: xvimx=0.523599*(2.*1.e-3)**3    ! mks  max volume = 1 mm radius (solid sphere approx)
      
      real     :: xvdmx = -1.0 ! 3.0e-3
      real     :: xvrmx
      parameter( xvrmn=0.523599*(80.e-6)**3, xvrmx0=0.523599*(6.e-3)**3 ) !( was 4.1887e-9 )  ! mks
      parameter( xvsmn=0.523599*(0.1e-3)**3, xvsmx=0.523599*(10.e-3)**3 ) !( was 4.1887e-9 )  ! mks
      parameter( xvfmn=0.523599*(0.1e-3)**3, xvfmx=0.523599*(10.e-3)**3 )  ! mks xvfmx = (pi/6)*(10mm)**3
      parameter( xvgmn=0.523599*(0.1e-3)**3, xvgmx=0.523599*(10.e-3)**3 )  ! mks xvfmx = (pi/6)*(10mm)**3
      parameter( xvhmn0=0.523599*(0.3e-3)**3, xvhmx0=0.523599*(20.e-3)**3 )  ! mks xvfmx = (pi/6)*(10mm)**3
      parameter( xvhlmn=0.523599*(dhlmn)**3, xvhlmx=0.523599*(dhlmx)**3 )  ! mks xvfmx = (pi/6)*(10mm)**3

!
!  electrical permitivity of air C / (N m**2) -  check the units
!
      real eperao
      parameter (eperao  = 8.8592e-12 )

      real ec,eci  ! fundamental unit of charge
      parameter (ec = 1.602e-19)
      parameter (eci = 1.0/ec)

      real    :: scwppmx = 20.0e-12
      real    :: scippmx = 20.0e-12
!
!  constants
!
      real, parameter :: c1f3 = 1.0/3.0

      real, parameter :: cai = 21.87455
      real, parameter :: caw = 17.2693882
      real, parameter :: cbi = 7.66
      real, parameter :: cbw = 35.86

      real, parameter :: tfr = 273.15, tfrh = 233.15

      real, parameter :: cp = 1004.0, rd = 287.04
      real, parameter :: cpi = 1./cp
      real, parameter :: cap = rd/cp, poo = 1.0e+05

      real, parameter :: rw = 461.5              ! gas const. for water vapor
      real, parameter :: advisc0 = 1.832e-05     ! reference dynamic viscosity (SMT; see Beard & Pruppacher 71)
      real, parameter :: advisc1 = 1.718e-05     ! dynamic viscosity constant used in thermal conductivity calc
      real, parameter :: tka0 = 2.43e-02         ! reference thermal conductivity
      real, parameter :: tfrcbw = tfr - cbw
      real, parameter :: tfrcbi = tfr - cbi

     ! GHB: Needed for eqtset=2 in cm1
!     REAL, PRIVATE ::      cv = cp - rd
     real, private, parameter ::      cv = 717.0             ! specific heat at constant volume - air
     REAL, PRIVATE, parameter ::      cvv = 1408.5
     REAL, PRIVATE, parameter ::      cpl = 4190.0
     REAL, PRIVATE, parameter ::      cpigb = 2106.0
     ! GHB

      real, parameter ::  bfnu0 = (rnu + 2.0)/(rnu + 1.0) 
      real :: ventr, ventrn, ventc, c1sw


      real :: cckm,ccne,ccnefac,cnexp,CCNE0

      integer :: na = 9
      integer :: nxtra = 1
      real gf4p5, gf4ds, gf4br
      real gsnow1, gsnow53, gsnow73
      real gfcinu1, gfcinu1p47, gfcinu2p47

      real :: cwchtmp0 = 1.0
      real :: cwchltmp0 = 1.0

      real    :: esctot = 1.0e-13

      integer iexy(lc:lqmx,lc:lqmx)
      integer :: ieswi = 1,  ieswc = 1, ieswr = 0
      integer :: iehlsw = 1, iehli = 1,  iehlc = 1, iehlr = 0
      integer :: iehwsw = 1, iehwi = 1,  iehwc = 1, iehwr = 0

      logical, parameter :: do_satadj_for_wrfchem = .true.

! #####################################################################
! #####################################################################

 CONTAINS

! #####################################################################
! #####################################################################

 REAL FUNCTION fqvs(t)
  implicit none
  real :: t
  fqvs = exp(caw*(t-273.15)/(t-cbw))
 END FUNCTION fqvs

 REAL FUNCTION fqis(t)
  implicit none
  real :: t
  fqis = exp(cai*(t-273.15)/(t-cbi))
 END FUNCTION fqis

! #####################################################################
 
       SUBROUTINE nssl_2mom_init(  &
     & ims,ime, jms,jme, kms,kme, nssl_params, ipctmp, mixphase,ihvol,idonictmp)

  implicit none
  
   integer, intent(in) :: ims,ime, jms,jme, kms,kme
   real,  intent(in), dimension(20) :: nssl_params



   integer, intent(in) :: ipctmp,mixphase,ihvol
   logical, optional, intent(in) :: idonictmp
     double precision :: arg
     real    :: temq
     integer :: igam
     integer :: i,il,j,l
     integer :: ltmp
     integer :: isub
     real    :: bxh,bxhl

      real    :: alp,ratio,x,y,y7
     


!
! set some global values from namelist input
!

      ccn      = nssl_params(1)
      alphah   = nssl_params(2)
      alphahl  = nssl_params(3)
      cnoh     = nssl_params(4)
      cnohl    = nssl_params(5)
      cnor     = nssl_params(6)
      cnos     = nssl_params(7)
      rho_qh   = nssl_params(8)
      rho_qhl  = nssl_params(9)
      rho_qs   = nssl_params(10)


      cwccn = ccn

      lhab = 8
      lhl = 8
      IF ( icespheres >= 1 ) THEN
        lhab = lhab + 1
        lis = li + 1
        ls = ls + 1
        lh = lh + 1
        lhl = lhl + 1
      ENDIF
      IF ( ihvol == -1 ) THEN
        lhab = lhab - 1  ! turns off hail -- option for single moment, only!!
        lhl = 0
        ehw0 = 0.75
        iehw = 2
        dfrz = Max( dfrz, 0.5e-3 )
      ENDIF

!      IF ( ipelec > 0 ) idonic = .true.

!
! Build lookup table for saturation mixing ratio (Soong and Ogura 73)
!

      do l = 1,nqsat
      temq = 163.15 + (l-1)*fqsat
      tabqvs(l) = exp(caw*(temq-273.15)/(temq-cbw))
      dtabqvs(l) = ((-caw*(-273.15 + temq))/(temq - cbw)**2 + &
     &                 caw/(temq - cbw))*tabqvs(l)
      tabqis(l) = exp(cai*(temq-273.15)/(temq-cbi))
      dtabqis(l) = ((-cai*(-273.15 + temq))/(temq - cbi)**2 + &
     &                 cai/(temq - cbi))*tabqis(l)
      end do

      bx(lr) = 0.85
      ax(lr) = 1647.81
      fx(lr) = 135.477
      
      IF ( icdx == 6 ) THEN
        bx(lh) = 0.6 ! Milbrandt and Morrison (2013) for density of 550.
        ax(lh) = 157.71
      ELSEIF ( icdx > 0 ) THEN
        bx(lh) = 0.5
        ax(lh) = 75.7149
      ELSE
        bx(lh) = 0.37 ! 0.6  ! Ferrier 1994
        ax(lh) = 19.3
      ENDIF
!      bx(lh) = 0.6

      IF ( lhl .gt. 1 ) THEN
        IF ( icdx == 6 ) THEN
          bx(lhl) = 0.593 ! Milbrandt and Morrison (2013) for density of 750.
          ax(lhl) = 179.36
        ELSEIF (icdxhl > 0 ) THEN
          bx(lhl) = 0.5
          ax(lhl) = 75.7149
        ELSE
          ax(lhl) = 206.984  ! Ferrier 1994
          bx(lhl) = 0.6384
        ENDIF
      ENDIF

! fill in the complete gamma function lookup table
     gmoi(0) = 1.d32
     do igam = 1,ngm0
      arg = dgam*igam
      gmoi(igam) = gamma_dp(arg)
     end do

     ! build lookup table to compute the number and mass fractions of rain drops 
     ! (imurain=1) greater than a given diameter. Used for qiacr and ciacr
     ! Uses incomplete gamma functions
     ! The terms with bxh or bxhl will be off if the actual bxh or bxhl is different from the base value (icdx=6 option)
      
      bxh = bx(lh)
      bxhl = bx(Max(lh,lhl))
      
      DO j = 0,nqiacralpha
      alp = float(j)*dqiacralpha
      y = gamma(1.+alp)
      DO i = 1,nqiacrratio
        ratio = float(i)*dqiacrratio
        x = gamxinf( 1.+alp, ratio )
!        write(0,*) 'i, x/y = ',i, x/y
        ciacrratio(i,j) = x/y

        ! graupel (.,.,.,1)
        gamxinflu(i,j,1,1) = x/y
        gamxinflu(i,j,2,1) = gamxinf( 2.0+alp, ratio )/y
        gamxinflu(i,j,3,1) = gamxinf( 2.5+alp+0.5*bxh, ratio )/y
        gamxinflu(i,j,5,1) = (gamma(5.0+alp) - gamxinf( 5.0+alp, ratio ))/y
        gamxinflu(i,j,6,1) = (gamma(5.5+alp+0.5*bxh) - gamxinf( 5.5+alp+0.5*bxh, ratio ))/y
        gamxinflu(i,j,9,1) = gamxinf( 1.0+alp, ratio )/y
        gamxinflu(i,j,10,1)= gamxinf( 4.0+alp, ratio )/y
       
        ! hail (.,.,.,2)
        gamxinflu(i,j,1,2) = gamxinflu(i,j,1,1)
        gamxinflu(i,j,2,2) = gamxinflu(i,j,2,1)
        gamxinflu(i,j,3,2) = gamxinf( 2.5+alp+0.5*bxhl, ratio )/y
        gamxinflu(i,j,5,2) = gamxinflu(i,j,5,1)
        gamxinflu(i,j,6,2) = (gamma(5.5+alp+0.5*bxhl) - gamxinf( 5.5+alp+0.5*bxhl, ratio ))/y
        gamxinflu(i,j,9,2) = gamxinflu(i,j,9,1)
        gamxinflu(i,j,10,2)= gamxinflu(i,j,10,1)

      IF ( alp > 1.1 ) THEN
!       gamxinflu(i,j,7,1) = gamxinf( alp - 1., ratio )/y
       gamxinflu(i,j,7,1) = (gamma(alp - 1.) - gamxinf( alp - 1., ratio ))/y 
!       gamxinflu(i,j,8,1) = gamxinf( alp - 0.5 + 0.5*bxh, ratio )/y
       gamxinflu(i,j,8,1) = (gamma(alp - 0.5 + 0.5*bxh) - gamxinf( alp - 0.5 + 0.5*bxh, ratio ))/y
!       gamxinflu(i,j,8,2) = gamxinf( alp - 0.5 + 0.5*bxhl, ratio )/y
       gamxinflu(i,j,8,2) = (gamma(alp - 0.5 + 0.5*bxhl) - gamxinf( alp - 0.5 + 0.5*bxhl, ratio ))/y
      ELSE
!       gamxinflu(i,j,7,1) = gamxinf( .1, ratio )/y
       gamxinflu(i,j,7,1) = (gamma(0.1) - gamxinf( 0.1, ratio ) )/y
!       gamxinflu(i,j,8,1) = gamxinf( 1.1 - 0.5 + 0.5*bxh, ratio )/y
!       gamxinflu(i,j,8,2) = gamxinf( 1.1 - 0.5 + 0.5*bxhl, ratio )/y
       gamxinflu(i,j,8,1) = (gamma(1.1 - 0.5 + 0.5*bxh) - gamxinf( 1.1 - 0.5 + 0.5*bxh, ratio ) )/y
       gamxinflu(i,j,8,2) = (gamma(1.1 - 0.5 + 0.5*bxhl) - gamxinf( 1.1 - 0.5 + 0.5*bxhl, ratio ) )/y
      ENDIF
        
        gamxinflu(i,j,7,2) = gamxinflu(i,j,7,1)

      ENDDO
      ENDDO
      ciacrratio(0,:) = 1.0

      DO j = 0,nqiacralpha
      alp = float(j)*dqiacralpha
      y = gamma(4.+alp)
      y7 = gamma(7.+alp)
      DO i = 1,nqiacrratio
        ratio = float(i)*dqiacrratio
        x = gamxinf( 4.+alp, ratio )
!        write(0,*) 'i, x/y = ',i, x/y
        qiacrratio(i,j) = x/y
        gamxinflu(i,j,4,1) = x/y
        gamxinflu(i,j,4,2) = x/y

        x = gamxinf( 7.+alp, ratio )
        ziacrratio(i,j) = x/y7

      ENDDO
      ENDDO
      qiacrratio(0,:) = 1.0


      isub = Min( 0, ihvol) ! is -1 or 0

      lccn = 0
      lccna = 0
      lnc = 0
      lnr = 0
      lni = 0
      lnis = 0
      lns = 0
      lnh = 0
      lnhl = 0
      lvh = 0
      lvhl = 0
      lzr = 0
      lzh = 0
      lzhl = 0
      lsw = 0
      lhw = 0
      lhlw = 0

      denscale(:) = 0
      
!      lccn = 9

    ipconc = ipctmp

    IF ( ipconc == 0 ) THEN
       IF ( ihvol >= 0 ) THEN
       lvh = 9
       ltmp = 9
       denscale(lvh) = 1
       ELSE ! no hail
       ltmp = lhab
       lhl = 0
       ENDIF
    ELSEIF ( ipconc == 5 ) THEN
      lccn = lhab+1 ! 9
      lnc = lhab+2 ! 10
      lnr = lhab+3 ! 11
      lni = lhab+4 !12
      lns = lhab+5 !13
      lnh = lhab+6 !14
      IF ( ihvol >= 0 ) THEN
      lnhl = lhab+7 ! 15
      ENDIF
      lvh = lhab+8 + isub ! 16 + isub ! isub adjusts to 15 if hail is off
      ltmp = lvh
      denscale(lccn:lvh) = 1
      IF ( ihvol == 1 ) THEN
       lvhl = ltmp+1
       ltmp = lvhl
       denscale(lvhl) = 1
      ENDIF
      IF ( mixedphase ) THEN
      lsw  = ltmp+1
      lhw  = ltmp+2
      lhlw = ltmp+3
      ltmp = lhlw
      ENDIF
    ELSEIF ( ipconc >= 6 ) THEN
      write(0,*) 'NSSL microphysics has not been compiled for 3-moment. Sorry.'
        STOP
      lccn = 9
      lnc = 10
      lnr = 11
      lni = 12
      lns = 13
      lnh = 14
      IF ( ihvol >= 0 ) THEN
      lnhl = 15
      ENDIF
      IF ( ipconc == 6 ) THEN
      lzh = 16 + isub
      lvh = 17 + isub
      ELSEIF ( ipconc == 7 ) THEN
      lzh = 16
      lzr = 17
      lvh = 18
      ELSEIF ( ipconc == 8 ) THEN
      lzr = 16
      lzh = 17
      lzhl = 18
      lvh = 19
      ENDIF
      ltmp = lvh
      denscale(lccn:lvh) = 1
      IF ( ihvol == 1 ) THEN
       lvhl = ltmp+1
       ltmp = lvhl
       denscale(lvhl) = 1
      ENDIF
      IF ( mixedphase ) THEN
      lsw  = ltmp+1
      lhw  = ltmp+2
      lhlw = ltmp+3
      ltmp = lhlw
      ENDIF
    ELSE
      CALL wrf_error_fatal( 'nssl_2mom_init: Invalid value of ipctmp' )
    ENDIF


    
      na = ltmp
      
      ln(lc) = lnc
      ln(lr) = lnr
      ln(li) = lni
      ln(ls) = lns
      ln(lh) = lnh
      IF ( lhl .gt. 1 ) ln(lhl) = lnhl

      ipc(lc) = 2
      ipc(lr) = 3
      ipc(li) = 1
      ipc(ls) = 4
      ipc(lh) = 5
      IF ( lhl .gt. 1 ) ipc(lhl) = 5
      
      ldovol = .false.
      lvol(:) = 0
      lvol(li) = lvi
      lvol(ls) = lvs
      lvol(lh) = lvh
      IF ( lhl .gt. 1 .and. lvhl .gt. 1 ) lvol(lhl) = lvhl
      
      lne = Max(lnh,lnhl)
      lne = Max(lne,lvh)
      lne = Max(lne,lvhl)
      lne = Max(lne,na)

      lsc(:) = 0
      lsc(lc) = lscw
      lsc(lr) = lscr
      lsc(li) = lsci
      lsc(ls) = lscs
      lsc(lh) = lsch
      IF ( lhl .gt. 1 ) lsc(lhl) = lschl


      DO il = lc,lhab
        ldovol = ldovol .or. ( lvol(il) .gt. 1 )
      ENDDO

!      write(0,*) 'nssl_2mom_init: ldovol = ',ldovol

      lz(:) = 0
      lz(lr) = lzr
      lz(li) = lzi
      lz(ls) = lzs
      lz(lh) = lzh
      IF ( lhl .gt. 1 .and. lzhl > 1 ) lz(lhl) = lzhl

      lliq(:) = 0
      lliq(ls) = lsw
      lliq(lh) = lhw
      IF ( lhl .gt. 1 ) lliq(lhl) = lhlw
      IF ( mixedphase ) THEN
!       write(0,*) 'lsw,lhw,lhlw = ',lsw,lhw,lhlw
      ENDIF



      xnu(lc) = 0.0
      xmu(lc) = 1.
      
      IF ( imurain == 3 ) THEN
        xnu(lr) = -0.8
        xmu(lr) = 1.
      ELSEIF ( imurain == 1 ) THEN
        xnu(lr) = (alphar - 2.0)/3.0
        xmu(lr) = 1./3.
      ENDIF

      xnu(li) = 0.0
      xmu(li) = 1.

      IF ( lis >= 1 ) THEN
      xnu(lis) = 0.0
      xmu(lis) = 1.
      ENDIF

      dnu(lc) = 3.*xnu(lc) + 2. ! alphac
      dmu(lc) = 3.*xmu(lc)

      dnu(lr) = 3.*xnu(lr) + 2. ! alphar
      dmu(lr) = 3.*xmu(lr)

      dnu(ls) = -0.4 ! alphas
      dmu(ls) = 3.

      xnu(ls) = -0.8
      xmu(ls) = 1.

      dnu(lh) = alphah
      dmu(lh) = dmuh

      xnu(lh) = (dnu(lh) - 2.)/3.
      xmu(lh) = dmuh/3.

      IF ( imurain == 3 ) THEN ! rain is gamma of volume
      rz =  ((4. + alphah)*(5. + alphah)*(6. + alphah)*(1. + xnu(lr)))/ & 
     &  ((1 + alphah)*(2 + alphah)*(3 + alphah)*(2. + xnu(lr)))

!      IF ( ipconc .lt. 5 ) alphahl = alphah
      
      rzhl =  ((4. + alphahl)*(5. + alphahl)*(6. + alphahl)*(1. + xnu(lr)))/ & 
     &  ((1. + alphahl)*(2. + alphahl)*(3. + alphahl)*(2. + xnu(lr)))

      rzs =  1. ! assume rain and snow are both gamma volume

      ELSE ! rain is gamma of diameter
      
      rz =  ((4. + alphah)*(5. + alphah)*(6. + alphah)*(1. + alphar)*(2. + alphar)*(3. + alphar))/ & 
     &  ((1 + alphah)*(2 + alphah)*(3 + alphah)*(4. + alphar)*(5. + alphar)*(6. + alphar))
      
      rzhl =  ((4. + alphahl)*(5. + alphahl)*(6. + alphahl)*(1. + alphar)*(2. + alphar)*(3. + alphar))/ & 
     &  ((1 + alphahl)*(2 + alphahl)*(3 + alphahl)*(4. + alphar)*(5. + alphar)*(6. + alphar))

      
      rzs =   & 
     &  ((1. + alphar)*(2. + alphar)*(3. + alphar)*(2. + xnu(ls)))/  &
     &  ((4. + alphar)*(5. + alphar)*(6. + alphar)*(1. + xnu(ls)))
       

      ENDIF

      IF ( ipconc <= 5 ) THEN 
        imltshddmr = Min(1, imltshddmr)
        ibinhmlr = 0
        ibinhlmlr = 0
      ENDIF

      IF ( ipconc > 5 .and. (ibinhmlr == 0 .and. ibinhmlr == 0 ) ) THEN 
        imltshddmr = Min(1, imltshddmr)
      ENDIF

!      write(0,*) 'rz,rzhl = ', rz,rzhl

      IF ( ipconc .lt. 4 ) THEN

      dnu(ls) = alphas
      dmu(ls) = 1.

      xnu(ls) = (dnu(ls) - 2.)/3.
      xmu(ls) = 1./3.


      ENDIF

      IF ( lhl .gt. 1 ) THEN

      dnu(lhl) = alphahl
      dmu(lhl) = dmuhl

      xnu(lhl) = (dnu(lhl) - 2.)/3.
      xmu(lhl) = dmuhl/3.

      ENDIF

      cno(lc)  = 1.0e+08
      IF ( li .gt. 1 ) cno(li)  = 1.0e+08
      cno(lr)  = cnor
      IF ( ls .gt. 1 ) cno(ls)  = cnos ! 8.0e+06
      IF ( lh .gt. 1 ) cno(lh)  = cnoh ! 4.0e+05
      IF ( lhl .gt. 1 ) cno(lhl)  = cnohl ! 4.0e+05
!
!  density maximums and minimums
!
      xdnmx(:) = 900.0

      xdnmx(lr) = 1000.0
      xdnmx(lc) = 1000.0
      xdnmx(li) =  917.0
      xdnmx(ls) =  300.0
      xdnmx(lh) =  900.0
      IF ( lhl .gt. 1 ) xdnmx(lhl) = 900.0
!
      xdnmn(:) = 900.0

      xdnmn(lr) = 1000.0
      xdnmn(lc) = 1000.0
      xdnmn(li) =  100.0
      xdnmn(ls) =  100.0
      xdnmn(lh) =  hdnmn
      IF ( lhl .gt. 1 ) xdnmn(lhl) = hldnmn

      xdn0(:) = 900.0

      xdn0(lc) = 1000.0
      xdn0(li) = 900.0
      xdn0(lr) = 1000.0
      xdn0(ls) = rho_qs ! 100.0
      xdn0(lh) = rho_qh ! (0.5)*(xdnmn(lh)+xdnmx(lh))
      IF ( lhl .gt. 1 ) xdn0(lhl) = rho_qhl ! 800.0

!
!  Set terminal velocities...
!    also set drag coefficients
!
      cdx(lr) = 0.60
      cdx(lh) = 0.8 ! 1.0 ! 0.45
      cdx(ls) = 2.00
      IF ( lhl .gt. 1 ) cdx(lhl) = 0.45

      ido(lc) = idocw
      ido(lr) = idorw
      ido(li) = idoci
      ido(ls) = idosw
      ido(lh)  = idohw
      IF ( lhl .gt. 1 ) ido(lhl) = idohl

      IF ( irfall .lt. 0 ) irfall = infall
      IF ( lzr > 0 ) irfall = 0

      qccn = ccn/rho00
!      xvcmx = (4./3.)*pi*xcradmx**3

! set max rain diameter
      IF ( xvdmx .gt. 0.0 ) THEN
        xvrmx = 0.523599*(xvdmx)**3
      ELSE
        xvrmx = xvrmx0
      ENDIF

         IF ( dhmn <= 0.0 ) THEN
           xvhmn = xvhmn0
!           xvhmn = Min(xvhmn0, 0.523599*(dfrz)**3 )
         ELSE
           xvhmn = 0.523599*(dhmn)**3
!           xvhmn = 0.523599*(Min(dhmn,dfrz))**3
         ENDIF

         IF ( dhmx <= 0.0 ) THEN
           xvhmx = xvhmx0
         ELSE
           xvhmx = 0.523599*(dhmx)**3
         ENDIF

! load max/min diameters
      xvmn(lc) = xvcmn
      xvmn(li) = xvimn
      xvmn(lr) = xvrmn
      xvmn(ls) = xvsmn
      xvmn(lh) = xvhmn

      xvmx(lc) = xvcmx
      xvmx(li) = xvimx
      xvmx(lr) = xvrmx
      xvmx(ls) = xvsmx
      xvmx(lh) = xvhmx

      IF ( lhl .gt. 1 ) THEN
      xvmn(lhl) = xvhlmn
      xvmx(lhl) = xvhlmx
      ENDIF

!
!  cloud water constants in mks units
!
!      cwmasn = 4.25e-15  ! radius of 1.0e-6
!      cwmasn = 5.23e-13   ! minimum mass, defined by radius of 5.0e-6
!      cwmasn5 =  5.23e-13
!      cwradn = 5.0e-6     ! minimum radius
!      cwmasx = 5.25e-10   ! maximum mass, defined by radius of 50.0e-6
!      mwfac = 6.0**(1./3.)
      IF ( ipconc .ge. 2 ) THEN
!        cwmasn = xvmn(lc)*1000.  ! minimum mass, defined by minimum droplet volume
!        cwradn = 1.0e-6          ! minimum radius
!        cwmasx = xvmx(lc)*1000.  ! maximum mass, defined by maximum droplet volume
        
      ENDIF
!        rwmasn = xvmn(lr)*1000.  ! minimum mass, defined by minimum rain volume
!        rwmasx = xvmx(lr)*1000.  ! maximum mass, defined by maximum rain volume

      IF ( lhl < 1 ) ifrzg = 1

      ventr = 1.
      IF ( imurain == 3 ) THEN
!       IF ( izwisventr == 1 ) THEN
        ventr = Gamma(rnu + 4./3.)/((rnu + 1.)**(1./3.)*Gamma(rnu + 1.)) ! Ziegler 1985
!       ELSE
        ventrn =  Gamma(rnu + 1.5 + br/6.)/(Gamma(rnu + 1.)*(rnu + 1.)**((1.+br)/6. + 1./3.) ) ! adapted from Wisner et al. 1972; for second term in rwvent
!        ventr = Gamma(rnu + 4./3.)/((rnu + 1.)**(1./3.)*Gamma(rnu + 1.)) ! Ziegler 1985, still use for first term in rwvent
!        ventr  = Gamma(rnu + 4./3.)/Gamma(rnu + 1.) 
!       ENDIF
      ELSE ! imurain == 1
!       IF ( iferwisventr == 1 ) THEN
        ventr = Gamma(2. + alphar)  ! Ferrier 1994
!       ELSEIF ( iferwisventr == 2 ) THEN
        ventrn =  Gamma(alphar + 2.5 + br/2.)/Gamma(alphar + 1.) ! adapted from Wisner et al. 1972
!       ENDIF
      ENDIF
      ventc   = Gamma(cnu + 4./3.)/(cnu + 1.)**(1./3.)/Gamma(cnu + 1.)
      c1sw = Gamma(snu + 4./3.)*(snu + 1.0)**(-1./3.)/gamma(snu + 1.0) 

  ! set threshold mixing ratios

      qxmin(:) = 1.0e-12

      qxmin(lc) = 1.e-9
      qxmin(lr) = 1.e-7
      IF ( li > 1 ) qxmin(li) = 1.e-12
      IF ( ls > 1 ) qxmin(ls) = 1.e-7
      IF ( lh > 1 ) qxmin(lh) = 1.e-7
      IF ( lhl .gt. 1 ) qxmin(lhl) = 1.e-7

      IF ( lc .gt. 1 .and. lnc .gt. 1 ) qxmin(lc) = 1.0e-13
      IF ( lr .gt. 1 .and. lnr .gt. 1 ) qxmin(lr) = 1.0e-12

      IF ( li .gt. 1 .and. lni .gt. 1 ) qxmin(li ) = 1.0e-13
      IF ( ls .gt. 1 .and. lns .gt. 1 ) qxmin(ls ) = 1.0e-13
      IF ( lh .gt. 1 .and. lnh .gt. 1 ) qxmin(lh ) = 1.0e-12
      IF ( lhl.gt. 1 .and. lnhl.gt. 1 ) qxmin(lhl) = 1.0e-12

  ! constants for droplet nucleation

      cckm = cck-1.
      ccnefac =  (1.63/(cck * beta(3./2., cck/2.)))**(cck/(cck + 2.0))
      cnexp   = (3./2.)*cck/(cck+2.0)
! ccne is all the factors with w in eq. A7 in Mansell et al. 2010 (JAS).  The constant changes
! if k (cck) is changed!
      ccne = ccnefac*1.e6*(1.e-6*Abs(cwccn))**(2./(2.+cck))
      ccne0 = ccnefac*1.e6*(1.e-6)**(2./(2.+cck))
      write(0,*) 'cwccn, cck, ccne = ',cwccn,cck,ccne,ccnefac,cnexp
      IF ( cwccn .lt. 0.0 ) THEN
      cwccn = Abs(cwccn)
      ccwmx = 50.e9 ! cwccn
      ELSE
      ccwmx = 50.e9 ! cwccn ! *1.4
      ENDIF

!
!
!  Set collection coefficients (Seifert and Beheng 05)
!
      bb(:) = 1.0/3.0
      bb(li) = 0.3429
      DO il = lc,lhab
        da0(il) = delbk(bb(il), xnu(il), xmu(il), 0)
        da1(il) = delbk(bb(il), xnu(il), xmu(il), 1)

!        write(0,*) 'il, da0, da1, xnu, xmu = ', il, da0(il), da1(il), xnu(il), xmu(il)
      ENDDO

      dab0(:,:) = 0.0
      dab1(:,:) = 0.0

      DO il = lc,lhab
        DO j = lc,lhab
          IF ( il .ne. j ) THEN

            dab0(il,j) = delabk(bb(il), bb(j), xnu(il), xnu(j), xmu(il), xmu(j), 0)
            dab1(il,j) = delabk(bb(il), bb(j), xnu(il), xnu(j), xmu(il), xmu(j), 1)

!           write(0,*) 'il, j, dab0, dab1 = ',il, j, dab0(il,j), dab1(il,j)
          ENDIF
        ENDDO
      ENDDO

        gf4br = gamma(4.0+br)
        gf4ds = gamma(4.0+ds)
        gf4p5 = gamma(4.0+0.5)
        gfcinu1 = gamma(cinu + 1.0)
        gfcinu1p47 = gamma(cinu + 1.47167)
        gfcinu2p47 = gamma(cinu + 2.47167)

        gsnow1 = gamma(snu + 1.0)
        gsnow53 = gamma(snu + 5./3.)
        gsnow73 = gamma(snu + 7./3.)

        IF ( lh  .gt. 1 ) cwchtmp0 = 6.0/pi*gamma( (xnu(lh) + 1.)/xmu(lh) )/gamma( (xnu(lh) + 2.)/xmu(lh) )
        IF ( lhl .gt. 1 ) cwchltmp0 = 6.0/pi*gamma( (xnu(lhl) + 1)/xmu(lhl) )/gamma( (xnu(lhl) + 2)/xmu(lhl) )


      iexy(:,:)=0; ! sets to zero the ones Imight have forgotten

!     snow
      iexy(ls,li) = ieswi
      iexy(ls,lc) = ieswc ; iexy(ls,lr) = ieswr ;

!     graupel
      iexy(lh,ls)  = iehwsw ; iexy(lh,li) = iehwi ;
      iexy(lh,lc) = iehwc ; iexy(lh,lr)  = iehwr ;

!     hail
      IF (lhl .gt. 1 ) THEN
      iexy(lhl,ls)  = iehlsw ; iexy(lhl,li) = iehli ;
      iexy(lhl,lc) = iehlc ; iexy(lhl,lr)  = iehlr ;
      ENDIF


  RETURN
END SUBROUTINE nssl_2mom_init

! #####################################################################
! #####################################################################

SUBROUTINE nssl_2mom_driver(qv, qc, qr, qi, qs, qh, qhl, ccw, crw, cci, csw, chw, chl,  &
                              cn, vhw, vhl,                                             &
                              zrw, zhw, zhl,                                            &
                              qsw, qhw, qhlw,                                           &
                              th, pii, p, w, dn, dz, dtp, itimestep,                    &
                              RAINNC,RAINNCV,                                           &
                              dx, dy,                                                   &
                              SNOWNC, SNOWNCV, GRPLNC, GRPLNCV,                         &
                              SR,HAILNC, HAILNCV,                                       &
                              tkediss,                                                  &
                              re_cloud, re_ice, re_snow,                                &
                              has_reqc, has_reqi, has_reqs,                             &
                              rainncw2, rainnci2,                                       &
                              dbz, vzf,compdbz,                                         &
                              rscghis_2d,                                               &
                              scr,scw,sci,scs,sch,schl,sctot,noninduc,                  &
                              induc,elec,scion,sciona,                                  &
                              pcc2, pre2, depsubr,      &
                              mnucf2, melr2, ctr2,     &
                              rim1_2, rim2_2,rim3_2, &
                              nctr2, nnuccd2, nnucf2, &
                              effc2,effr2,effi2,       &
                              effs2, effg2,                       &
                              fc2, fr2,fi2,fs2,fg2, &
                              fnc2, fnr2,fni2,fns2,fng2, &
!                              qcond,qdep,qfrz,qrauto,qhcnvi,qhcollw,qscollw,            &
!                              ncauto, niinit,nifrz,                                     &
!                              re_liquid, re_graupel, re_hail, re_icesnow,               &
!                              vtcloud, vtrain, vtsnow, vtgraupel, vthail,               &
                              ipelectmp,                                                &
                              diagflag,                                                 &
                              nssl_progn,                                              & ! wrf-chem 
! 20130903 acd_mb_washout start
                              rainprod, evapprod,                                       & ! wrf-chem 
! 20130903 acd_mb_washout end
                              ids,ide, jds,jde, kds,kde,                                &  ! domain dims
                              ims,ime, jms,jme, kms,kme,                                &  ! memory dims
                              its,ite, jts,jte, kts,kte)                                   ! tile dims



      implicit none

      integer :: mytask = 0

 !Subroutine arguments:

      integer, intent(in)::                                                             &
                            ids,ide, jds,jde, kds,kde,                                   &
                            ims,ime, jms,jme, kms,kme,                                   &
                            its,ite, jts,jte, kts,kte
      real, dimension(ims:ime, kms:kme, jms:jme), intent(inout)::                        &
                            qv,qc,qr,qi,qs,qh,th
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout)::                        &
                              zrw, zhw, zhl,                                            &
                              qsw, qhw, qhlw,                                           &
                            qhl,ccw,crw,cci,csw,chw,chl,vhw,vhl
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout):: dbz, vzf, cn
      real, dimension(ims:ime, jms:jme), optional, intent(inout):: compdbz
      real, dimension(ims:ime, jms:jme), optional, intent(inout):: rscghis_2d
!      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout)::rscghis_3d
      real, dimension(ims:ime, kms:kme, jms:jme),  optional, intent(inout)::                   &
                            scr,scw,sci,scs,sch,schl,sciona,sctot,induc,noninduc  ! space charge
      real, dimension(ims:ime, kms:kme, jms:jme),  optional, intent(in) :: elec ! elecsave = Ez     
      real, dimension(ims:ime, kms:kme, jms:jme,2),optional, intent(inout) :: scion  
      real, dimension(ims:ime, kms:kme, jms:jme), intent(in)::  p,w,dz,dn

      real, dimension(ims:ime, kms:kme, jms:jme), intent(in)::  pii
      real, dimension(ims:ime, kms:kme, jms:jme), optional, intent(inout)::   &
                              pcc2, pre2, depsubr,      &
                              mnucf2, melr2, ctr2,     &
                              rim1_2, rim2_2,rim3_2, &
                              nctr2, nnuccd2, nnucf2, &
                              effc2,effr2,effi2,       &
                              effs2, effg2,                       &
                              fc2, fr2,fi2,fs2,fg2, &
                              fnc2, fnr2,fni2,fns2,fng2
!                              qcond,qdep,qfrz,qrauto,qhcnvi,qhcollw,qscollw,            &
!                              ncauto, niinit,nifrz,                                     &
!                              re_liquid, re_graupel, re_hail, re_icesnow,               &
!                              vtcloud, vtrain, vtsnow, vtgraupel, vthail               

      real, dimension(ims:ime, jms:jme), intent(inout)::                                 &
                            RAINNC,RAINNCV    ! accumulated precip (NC) and rate (NCV)
      real, dimension(ims:ime, jms:jme), optional, intent(inout)::                                 &
                            SNOWNC,SNOWNCV,GRPLNC,GRPLNCV,SR        ! accumulated precip (NC) and rate (NCV)
      real, dimension(ims:ime, jms:jme), optional, intent(inout)::                                 &
                            HAILNC,HAILNCV ! accumulated precip (NC) and rate (NCV)
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional, INTENT(INOUT):: &
                          re_cloud, re_ice, re_snow
      REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional, INTENT(IN):: tkediss
      INTEGER, INTENT(IN), optional :: has_reqc, has_reqi, has_reqs
      real, dimension(ims:ime, jms:jme), intent(out), optional ::                                 &
                            rainncw2, rainnci2       ! liquid rain, ice, accumulation rates
      real, optional, intent(in) :: dx,dy
      real, intent(in)::    dtp
      integer, intent(in):: itimestep !, ccntype
      logical, optional, intent(in) :: diagflag
      integer, optional, intent(in) :: ipelectmp

  LOGICAL, INTENT(IN), OPTIONAL ::    nssl_progn  ! wrf-chem
!   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional,INTENT(INOUT):: qndrop
  LOGICAL :: flag_qndrop  ! wrf-chem

! 20130903 acd_ck_washout start
! rainprod - total tendency of conversion of cloud water/ice and graupel to rain (kg kg-1 s-1)
! evapprod - tendency of evaporation of rain (kg kg-1 s-1)
! 20130903 acd_ck_washout end
   REAL, DIMENSION(ims:ime, kms:kme, jms:jme), optional,INTENT(INOUT)::  rainprod, evapprod

!
! local variables
!
     real, dimension(its:ite, 1, kts:kte) :: elec2 ! ez = elecsave slab
!     real, dimension(its:ite, 1, kts:kte,2) :: scion2 ! 1=- , 2=+
     real, dimension(its:ite, kts:kte) :: rainprod2d, evapprod2d,tke2d
     real, dimension(its:ite, 1, kts:kte, na) :: an
     real, dimension(its:ite, 1, kts:kte, nxtra) :: axtra
     real, dimension(its:ite, 1, kts:kte) :: t0,t1,t2,t3,t4,t5,t6,t7,t8,t9
     real, dimension(its:ite, 1, kts:kte) :: dn1,t00,t77,ssat,pn,wn,dz2d,dz2dinv,dbz2d,vzf2d
     real, dimension(its:ite, 1, na) :: xfall
     integer, parameter :: nor = 0, ng = 0
     integer :: nx,ny,nz
     integer ix,jy,kz,i,j,k,il,n
     integer :: infdo
     real :: ssival, ssifac, t8s, t9s, qvapor
     integer :: ltemq
     double precision :: dp1
     integer :: jye, lnb
     integer :: imx,kmx
     real    :: dbzmx
     integer :: vzflag0 = 0
     logical :: makediag
      real, parameter :: cnin20 = 1.0e3
      real, parameter :: cnin10 = 5.0e1
      real, parameter :: cnin1a = 4.5
      real, parameter :: cnin2a = 12.96
      real, parameter :: cnin2b = 0.639

#if (NMM_CORE == 1)
! NMM WRF core does not have special boundary conditions for CCN, therefore set invertccn to true
      logical, parameter :: invertccn = .true. ! =true for base state of ccn=0, =false for ccn initialized in the base state
#else
      logical, parameter :: invertccn = .false. ! =true for base state of ccn=0, =false for ccn initialized in the base state
#endif

      real :: tmp,dv

      double precision :: dt1,dt2
      double precision :: timesed,timesed1,timesed2,timesed3, timegs, timenucond, timedbz,zmaxsed
      double precision :: timevtcalc,timesetvt
      
#ifdef MPI

#if defined(MPI) 
      integer, parameter :: ntot = 50
      double precision  mpitotindp(ntot), mpitotoutdp(ntot)
      INTEGER :: mpi_error_code = 1
#endif
#endif


! -------------------------------------------------------------------


      
!      write(0,*) 'N2M: entering routine'

     flag_qndrop = .false.
     IF ( PRESENT ( nssl_progn ) ) flag_qndrop = nssl_progn
       
     IF ( present( vzf ) ) vzflag0 = 1
     
     IF ( present( ipelectmp ) ) THEN
       ipelec = ipelectmp
     ELSE
       ipelec = 0
     ENDIF
!       IF ( present( dbz ) ) THEN
!       DO jy = jts,jte
!         DO kz = kts,kte
!           DO ix = its,ite
!             dbz(ix,kz,jy) = 0.0
!           ENDDO
!         ENDDO
!       ENDDO
!       ENDIF

     
     makediag = .true.
     IF ( present( diagflag ) ) THEN
      makediag = diagflag
     ENDIF

!     write(0,*) 'N2M: makediag = ',makediag
     
     
     nx = ite-its+1
     ny = 1         ! set up as 2D slabs
     nz = kte-kts+1
     
! set up CCN array and some other static local values
     IF ( itimestep == 1 .and. .not. invertccn .and.  present( cn ) ) THEN
     ! this is not needed for WRF 3.8 and later because it is done in physics_init, 
     ! but kept for backwards compatibility with earlier versions
      IF ( cn((ite+its)/2,(kte+kts)/2,(jte+jts)/2) < 10.0 ) THEN ! initialize ccn if not already done
        DO jy = jts,jte
         DO kz = kts,kte
          DO ix = its,ite
            cn(ix,kz,jy) = qccn
          ENDDO
         ENDDO
        ENDDO
      ENDIF
     ENDIF
     
      IF ( invertccn .and.  present( cn ) ) THEN ! hack for WRF to convert activated ccn to unactivated, then don't have to 
                                              ! worry about initial and boundary conditions - they are zero
        DO jy = jts,jte
         DO kz = kts,kte
           DO ix = its,ite
             cn(ix,kz,jy) = Max( 0.0, qccn - cn(ix,kz,jy) )
           ENDDO
         ENDDO
       ENDDO
       ENDIF

!     ENDIF ! itimestep == 1

! sedimentation settings

      infdo = 2
      
      IF ( infall .ne. 1 .or. iscfall .ge. 2 ) THEN
         infdo = 1
      ELSE
         infdo = 0
      ENDIF

      IF ( infall .ge. 3 .or. ipconc .ge. 6 ) THEN
         infdo = 2
      ENDIF
     

      IF ( present( HAILNCV ) .and. lhl < 1 ) THEN ! for WRF 3.1 compatibility
        HAILNCV(its:ite,jts:jte) = 0.
      ENDIF

      tke2d(:,:) = 0.0 ! initialize if not used

     lnb = Max(lh,lhl)+1 ! lnc
!     IF ( lccn > 1 ) lnb = lccn

       jye = jte

     IF ( present( compdbz ) .and. makediag ) THEN
     DO jy = jts,jye
       DO ix = its,ite
        compdbz(ix,jy) = -3.0
       ENDDO
     ENDDO
     ENDIF

      zmaxsed = 0.0d0
      timevtcalc = 0.0d0
      timesetvt = 0.0d0
      timesed = 0.0d0
      timesed1 = 0.0d0
      timesed2 = 0.0d0
      timesed3 = 0.0d0
      timegs = 0.0d0
      timenucond = 0.0d0


!     write(0,*) 'N2M: jy loop 1, lhl,na = ',lhl,na,present(qhl)

     DO jy = jts,jye
     
     xfall(:,:,:) = 0.0

!     write(0,*) 'N2M: load an, jy,lccn = ',jy,lccn,qccn

     IF ( present( pcc2 ) .and. makediag ) THEN
         axtra(its:ite,1,kts:kte,:) = 0.0
     ENDIF
   ! copy from 3D array to 2D slab
   
       DO kz = kts,kte
        DO ix = its,ite
        
          an(ix,1,kz,lt)   = th(ix,kz,jy)
          an(ix,1,kz,lv)   = qv(ix,kz,jy)
          an(ix,1,kz,lc)   = qc(ix,kz,jy)
          an(ix,1,kz,lr)   = qr(ix,kz,jy)
          an(ix,1,kz,li)   = qi(ix,kz,jy)
          an(ix,1,kz,ls)   = qs(ix,kz,jy)
          an(ix,1,kz,lh)   = qh(ix,kz,jy)
          IF ( lhl > 1 ) an(ix,1,kz,lhl)  = qhl(ix,kz,jy)
          IF ( lccn > 1 ) THEN
           IF ( present( cn ) ) THEN
            an(ix,1,kz,lccn) = cn(ix,kz,jy)
           ELSE
            an(ix,1,kz,lccn) = qccn - ccw(ix,kz,jy)
           ENDIF
          ENDIF
          IF ( ipconc >= 5 ) THEN
             an(ix,1,kz,lnc)  = ccw(ix,kz,jy)
          IF ( constccw > 0.0 ) THEN
            an(ix,1,kz,lnc)  = constccw
          ENDIF
          an(ix,1,kz,lnr)  = crw(ix,kz,jy)
          an(ix,1,kz,lni)  = cci(ix,kz,jy)
          an(ix,1,kz,lns)  = csw(ix,kz,jy)
          an(ix,1,kz,lnh)  = chw(ix,kz,jy)
          IF ( lhl > 1 ) an(ix,1,kz,lnhl) = chl(ix,kz,jy)
          ENDIF
          IF ( lvh > 0 ) an(ix,1,kz,lvh)  = vhw(ix,kz,jy)
          IF ( lvhl > 0 .and. present( vhl ) ) an(ix,1,kz,lvhl)  = vhl(ix,kz,jy)

          



          
          t0(ix,1,kz) = th(ix,kz,jy)*pii(ix,kz,jy) ! temperature (Kelvin)
          t1(ix,1,kz) = 0.0
          t2(ix,1,kz) = 0.0
          t3(ix,1,kz) = 0.0
          t4(ix,1,kz) = 0.0
          t5(ix,1,kz) = 0.0
          t6(ix,1,kz) = 0.0
          t7(ix,1,kz) = 0.0
          t8(ix,1,kz) = 0.0
          t9(ix,1,kz) = 0.0
          t00(ix,1,kz) = 380.0/p(ix,kz,jy)
          t77(ix,1,kz) = pii(ix,kz,jy)
          dbz2d(ix,1,kz) = 0.0
          vzf2d(ix,1,kz) = 0.0

          dn1(ix,1,kz) = dn(ix,kz,jy)
          pn(ix,1,kz) = p(ix,kz,jy)
          wn(ix,1,kz) = w(ix,kz,jy)
          dz2d(ix,1,kz) = dz(ix,kz,jy)
          dz2dinv(ix,1,kz) = 1./dz(ix,kz,jy)
          
         ltemq = Int( (t0(ix,1,kz)-163.15)/fqsat+1.5 )
         ltemq = Min( nqsat, Max(1,ltemq) )
!
! saturation mixing ratio
!
      t8s = t00(ix,1,kz)*tabqvs(ltemq)  !saturation mixing ratio wrt water
      t9s = t00(ix,1,kz)*tabqis(ltemq)  !saturation mixing ratio wrt ice

!
!  calculate rate of nucleation
!
      ssival = Min(t8s,max(an(ix,1,kz,lv),0.0))/t9s  ! qv/qvi

      if ( ssival .gt. 1.0 ) then
!
      IF ( icenucopt == 1 ) THEN

      if ( t0(ix,1,kz).le.268.15 ) then

       dp1 = cnin20*exp( Min( 57.0 ,(cnin2a*(ssival-1.0)-cnin2b) ) )
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      end if

!
!   Default value of imeyers5 turns off nucleation by Meyer at higher temperatures
!  This is really from Ferrier (1994), eq. 4.31 - 4.34
      IF ( imeyers5 ) THEN
      if ( t0(ix,1,kz).lt.tfr .and. t0(ix,1,kz).gt.268.15 ) then
      qvapor = max(an(ix,1,kz,lv),0.0)
      ssifac = 0.0
      if ( (qvapor-t9s) .gt. 1.0e-5 ) then
      if ( (t8s-t9s) .gt. 1.0e-5 ) then
      ssifac = (qvapor-t9s) /(t8s-t9s)
      ssifac = ssifac**cnin1a
      end if
      end if
      t7(ix,1,kz) = cnin10*ssifac*exp(-(t0(ix,1,kz)-tfr)*bta1)
      end if
      ENDIF

      ELSEIF ( icenucopt == 2 ) THEN ! Thompson/Cooper; Note Thompson 2004 has constants of
                                     ! 0.005 and 0.304 because the line function was estimated from Cooper's plot
                                     ! Here, the fit line values from Cooper 1986 are converted. Very little difference 
                                     ! in practice
      
        t7(ix,1,kz) = 1000.*0.00446684*exp(0.3108*(273.16 - Max(233.0, t0(ix,1,kz) ) ) ) ! factor of 1000 to convert L**-1 to m**-3

!        write(0,*) 'Cooper t7,ssival = ',ix,kz,t7(ix,1,kz),ssival
      
      ELSEIF ( icenucopt == 3 ) THEN ! Phillips (Meyers/DeMott)

      if ( t0(ix,1,kz).le.268.15 .and.  t0(ix,1,kz) > 243.15 ) then ! Meyers with factor of Psi=0.06
        
       dp1 = 0.06*cnin20*exp( Min( 57.0 ,(cnin2a*(ssival-1.0)-cnin2b) ) )
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      elseif ( t0(ix,1,kz) <= 243.15 ) then ! Phillips estimate of DeMott et al (2003) data
       dp1 = 1000.*( exp( Min( 57.0 ,cnin2a*(ssival-1.1) ) ) )**0.3
       t7(ix,1,kz) = Min(dp1, 1.0d30)
      
      end if
      
      ENDIF ! icenucopt


!
      end if ! ( ssival .gt. 1.0 )
!

        ENDDO ! ix
       ENDDO ! kz

         IF ( wrfchem_flag > 0 ) THEN
           IF ( PRESENT( rainprod ) ) rainprod2d(its:ite,kts:kte) = 0
           IF ( PRESENT( evapprod ) ) evapprod2d(its:ite,kts:kte) = 0
         ENDIF
         

   ! transform from number mixing ratios to number conc.
     
     DO il = lnb,na
       IF ( denscale(il) == 1 ) THEN
         DO kz = kts,kte
          DO ix = its,ite
           an(ix,1,kz,il) = an(ix,1,kz,il)*dn(ix,kz,jy)
          ENDDO
         ENDDO
       ENDIF
     ENDDO ! il
        
! sedimentation
      xfall(:,:,:) = 0.0
       
      IF ( .true. ) THEN


! for real cases when hydrometeor mixing ratios have been initialized without concentrations
       IF ( itimestep == 1 .and. ipconc > 0 ) THEN
         call calcnfromq(nx,ny,nz,an,na,nor,nor,dn1)
       ENDIF
      call sediment1d(dtp,nx,ny,nz,an,na,nor,nor,xfall,dn1,dz2d,dz2dinv, &
     &                    t0,t7,infdo,jy,its,jts &
     &   ,timesed1,timesed2,timesed3,zmaxsed,timesetvt)


! copy xfall to appropriate places...

!     write(0,*) 'N2M: end sediment, jy = ',jy

       DO ix = its,ite
         IF ( lhl > 1 ) THEN
         RAINNCV(ix,jy) = dtp*dn1(ix,1,1)*(xfall(ix,1,lr) + xfall(ix,1,ls)*1000./xdn0(lr) + &
              &            xfall(ix,1,lh)*1000./xdn0(lr) + xfall(ix,1,lhl)*1000./xdn0(lr) )
         ELSE
         RAINNCV(ix,jy) = dtp*dn1(ix,1,1)*(xfall(ix,1,lr) + xfall(ix,1,ls)*1000./xdn0(lr) + &
              &            xfall(ix,1,lh)*1000./xdn0(lr) )
         ENDIF
         IF ( present ( rainncw2 ) ) THEN ! rain only
           rainncw2(ix,jy) =  rainncw2(ix,jy) +  dtp*dn1(ix,1,1)*xfall(ix,1,lr)
         ENDIF
         IF ( present ( rainnci2 ) ) THEN ! ice only
           IF ( lhl > 1 ) THEN
             rainnci2(ix,jy) =rainnci2(ix,jy) + dtp*dn1(ix,1,1)*(xfall(ix,1,ls)*1000./xdn0(lr) + &
     &            xfall(ix,1,lh)*1000./xdn0(lr) + xfall(ix,1,lhl)*1000./xdn0(lr) )
           ELSE
             rainnci2(ix,jy) = rainnci2(ix,jy) + dtp*dn1(ix,1,1)*(xfall(ix,1,ls)*1000./xdn0(lr) + &
     &            xfall(ix,1,lh)*1000./xdn0(lr) )
           ENDIF
         ENDIF
         IF ( present( SNOWNCV ) ) SNOWNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,ls)*1000./xdn0(lr)
         IF ( present( GRPLNCV ) ) GRPLNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lh)*1000./xdn0(lr)
         RAINNC(ix,jy)  = RAINNC(ix,jy) + RAINNCV(ix,jy)

         IF ( present (SNOWNC) .and. present (SNOWNCV) ) SNOWNC(ix,jy)  = SNOWNC(ix,jy) + SNOWNCV(ix,jy)
         IF ( lhl > 1 ) THEN
!#ifdef CM1
!           IF ( .true. ) THEN
!#else
           IF ( present( HAILNC ) ) THEN
!#endif
             HAILNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lhl)*1000./xdn0(lr)
             HAILNC(ix,jy)  = HAILNC(ix,jy) + HAILNCV(ix,jy)
           ELSEIF ( present( GRPLNCV ) ) THEN
             GRPLNCV(ix,jy) = dtp*dn1(ix,1,1)*xfall(ix,1,lhl)*1000./xdn0(lr)
           ENDIF
         ENDIF
         IF ( present( GRPLNCV ) ) GRPLNC(ix,jy)  = GRPLNC(ix,jy) + GRPLNCV(ix,jy)
        IF ( present( SR ) .and. present (SNOWNCV) .and. present(GRPLNCV) ) THEN
         IF ( present( HAILNC ) ) THEN
           SR(ix,jy)      = (SNOWNCV(ix,jy)+HAILNCV(ix,jy)+GRPLNCV(ix,jy))/(RAINNCV(ix,jy)+1.e-12)
         ELSE
           SR(ix,jy)      = (SNOWNCV(ix,jy)+GRPLNCV(ix,jy))/(RAINNCV(ix,jy)+1.e-12)
         ENDIF
        ENDIF
       ENDDO
       
      ENDIF ! .false.
 
      IF ( isedonly /= 1 ) THEN
   ! call nssl_2mom_gs: main gather-scatter routine to calculate microphysics

!     write(0,*) 'N2M: gs, jy = ',jy
!      IF ( isedonly /= 2 ) THEN


      
      call nssl_2mom_gs   &
     &  (nx,ny,nz,na,jy   &
     &  ,nor,nor          &
     &  ,dtp,dz2d       &
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9     &
     &  ,an,dn1,t77                        &
     &  ,pn,wn,0                           &
     &  ,t00,t77,                          &
     &   ventr,ventc,c1sw,1,ido,           &
     &   xdnmx,xdnmn,                      &
!     &   ln,ipc,lvol,lz,lliq,              &
     &   cdx,                              &
     &   xdn0,dbz2d,tke2d,                 &
     &   timevtcalc,axtra, makediag        &
     &   ,rainprod2d, evapprod2d           &
     & ,elec2,its,ids,ide,jds,jde          &
     & )





   ENDIF ! isedonly /= 1
   
 ! droplet nucleation/condensation/evaporation
   IF ( .true. ) THEN
   CALL NUCOND    &
     &  (nx,ny,nz,na,jy & 
     &  ,nor,nor,dtp,nx  &
     &  ,dz2d & 
     &  ,t0,t9 & 
     &  ,an,dn1,t77 & 
     &  ,pn,wn & 
     &  ,axtra, makediag  &
     &  ,ssat,t00,t77,flag_qndrop)


   ENDIF



! compute diagnostic S-band reflectivity if needed
     IF ( present( dbz ) .and. makediag ) THEN
   ! calc dbz
      
      IF ( .true. ) THEN
      call radardd02(nx,ny,nz,nor,na,an,t0,         &
     &    dbz2d,dn1,nz,cnoh,rho_qh,ipconc, 0)
      ENDIF ! .false.

     
       DO kz = kts,kte
        DO ix = its,ite
         dbz(ix,kz,jy) = dbz2d(ix,1,kz)
         IF ( present( vzf ) ) THEN
           vzf(ix,kz,jy) = vzf2d(ix,1,kz)
         ENDIF
          IF ( present( compdbz ) ) THEN
            compdbz(ix,jy) = Max( compdbz(ix,jy), dbz2d(ix,1,kz) )
          ENDIF
        ENDDO
       ENDDO


! Following Greg Thompson, calculation for effective radii. Used by RRTMG LW/SW schemes if enabled in module_physics_init.F
      IF ( present( has_reqc ).and. present( has_reqi ) .and. present( has_reqs ) .and.  &
           present( re_cloud ).and. present( re_ice ) .and. present( re_snow ) ) THEN
       IF ( has_reqc.ne.0 .and. has_reqi.ne.0 .and. has_reqs.ne.0) THEN
         DO kz = kts,kte
          DO ix = its,ite
             re_cloud(ix,kz,jy)  = 2.51E-6
             re_ice(ix,kz,jy)    = 10.01E-6
             re_snow(ix,kz,jy)   = 25.E-6
             t1(ix,1,kz) = 2.51E-6
             t2(ix,1,kz) = 10.01E-6
             t3(ix,1,kz) = 25.E-6
          ENDDO
         ENDDO

          call calc_eff_radius   &
     &         (nx,ny,nz,na,jy & 
     &          ,nor,nor & 
     &          ,t1,t2,t3  & 
     &          ,an,dn1 )

        DO kz = kts,kte
          DO ix = its,ite
             re_cloud(ix,kz,jy) = MAX(2.51E-6, MIN(t1(ix,1,kz), 50.E-6))
             re_ice(ix,kz,jy)   = MAX(10.01E-6, MIN(t2(ix,1,kz), 125.E-6))
             re_snow(ix,kz,jy)  = MAX(25.E-6, MIN(t3(ix,1,kz), 999.E-6))
          ENDDO
         ENDDO
       
         ENDIF
        ENDIF



       ENDIF
   
! transform concentrations back to mixing ratios
     DO il = lnb,na
      IF ( denscale(il) == 1 ) THEN
       DO kz = kts,kte
        DO ix = its,ite
         an(ix,1,kz,il) = an(ix,1,kz,il)/dn(ix,kz,jy)
        ENDDO
       ENDDO
      ENDIF
     ENDDO ! il
   
   ! copy 2D slabs back to 3D

   
       DO kz = kts,kte
        DO ix = its,ite
        
         th(ix,kz,jy)  = an(ix,1,kz,lt)
         qv(ix,kz,jy)  = an(ix,1,kz,lv)
         qc(ix,kz,jy)  = an(ix,1,kz,lc)
         qr(ix,kz,jy)  = an(ix,1,kz,lr)
         qi(ix,kz,jy)  = an(ix,1,kz,li)
         qs(ix,kz,jy)  = an(ix,1,kz,ls)
         qh(ix,kz,jy)  = an(ix,1,kz,lh)
         IF ( lhl > 1 ) qhl(ix,kz,jy) = an(ix,1,kz,lhl)
         IF ( present( cn ) .and. lccn > 1 .and. .not. flag_qndrop) THEN
           cn(ix,kz,jy) = an(ix,1,kz,lccn)
         ENDIF
         IF ( ipconc >= 5 ) THEN

          ccw(ix,kz,jy) = an(ix,1,kz,lnc)
          crw(ix,kz,jy) = an(ix,1,kz,lnr)
          cci(ix,kz,jy) = an(ix,1,kz,lni)
          csw(ix,kz,jy) = an(ix,1,kz,lns)
          chw(ix,kz,jy) = an(ix,1,kz,lnh)
          IF ( lhl > 1 ) chl(ix,kz,jy) = an(ix,1,kz,lnhl)
         ENDIF




         IF ( lvh > 0 )  vhw(ix,kz,jy) = an(ix,1,kz,lvh)
         IF ( lvhl > 0 .and. present( vhl ) ) vhl(ix,kz,jy) = an(ix,1,kz,lvhl)

#ifdef WRF_CHEM
         IF ( wrfchem_flag > 0 ) THEN
           IF ( PRESENT( rainprod ) ) rainprod(ix,kz,jy) = rainprod2d(ix,kz)
           IF ( PRESENT( evapprod ) ) evapprod(ix,kz,jy) = evapprod2d(ix,kz)
         ENDIF
#endif
        ENDDO
       ENDDO
  
     ENDDO ! jy

      IF (  invertccn .and. present( cn ) ) THEN ! hack to convert unactivated ccn  back to activated
        DO jy = jts,jte
         DO kz = kts,kte
           DO ix = its,ite
             cn(ix,kz,jy) = Max( 0.0, qccn - cn(ix,kz,jy) )
           ENDDO
         ENDDO
       ENDDO
       ENDIF





  RETURN
END SUBROUTINE nssl_2mom_driver

! #####################################################################
! #####################################################################

      REAL FUNCTION GAMMA(xx)

      implicit none
      real xx
      integer j

! Double precision ser,stp,tmp,x,y,cof(6)

      real*8 ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d+0,  &
     &            -86.50532032941677d0,   &
     &             24.01409824083091d0,   &
     &             -1.231739572450155d0,  &
     &              0.1208650973866179d-2,&
     &             -0.5395239384953d-5,   &
     &              2.5066282746310005d0/

      IF ( xx <= 0.0 ) THEN
        write(0,*) 'Argument to gamma must be > 0!! xx = ',xx
        STOP
      ENDIF
      
      x = xx
      y = x
      tmp = x + 5.5d0
      tmp = (x + 0.5d0)*Log(tmp) - tmp
      ser = 1.000000000190015d0
      DO j=1,6
        y = y + 1.0d0
        ser = ser + cof(j)/y
      END DO
      gamma = Exp(tmp + log(stp*ser/x))

      RETURN
      END FUNCTION GAMMA

! #####################################################################

        real function GAMXINF(A1,X1)

!       ===================================================
!       Purpose: Compute the incomplete gamma function
!                from x to infinity
!       Input :  a   --- Parameter ( a  170 )
!                x   --- Argument 
!       Output:  GIM --- gamma(a,x) t=x,Infinity
!       Routine called: GAMMA for computing gamma(x)
!       ===================================================

!        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        implicit none
        real :: a1,x1
        double precision :: xam,dlog,s,r,ga,t0,a,x
        integer :: k
        double precision :: gin, gim
        
        a = a1
        x = x1
        XAM=-X+A*DLOG(X)
        IF (XAM.GT.700.0.OR.A.GT.170.0) THEN
           WRITE(*,*)'a and/or x too large'
           STOP
        ENDIF
        IF (X.EQ.0.0) THEN
           GIN=0.0
           GIM = GAMMA(A1)
        ELSE IF (X.LE.1.0+A) THEN
           S=1.0D0/A
           R=S
           DO 10 K=1,60
              R=R*X/(A+K)
              S=S+R
              IF (DABS(R/S).LT.1.0D-15) GO TO 15
10         CONTINUE
15         GIN=DEXP(XAM)*S
           ga = GAMMA(A1)
           GIM=GA-GIN
        ELSE IF (X.GT.1.0+A) THEN
           T0=0.0D0
           DO 20 K=60,1,-1
              T0=(K-A)/(1.0D0+K/(X+T0))
20         CONTINUE
           GIM=DEXP(XAM)/(X+T0)
!           GA = GAMMA(A1)
!           GIN=GA-GIM
        ENDIF
        
        gamxinf = GIM
        return
        END function GAMXINF

! #####################################################################

        double precision function GAMXINFDP(A1,X1)

!       ===================================================
!       Purpose: Compute the incomplete gamma function
!                from x to infinity
!       Input :  a   --- Parameter ( a < 170 )
!                x   --- Argument 
!       Output:  GIM --- Gamma(a,x) t=x,Infinity
!       Routine called: GAMMA for computing â(x)
!       ===================================================

!        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        implicit none
        real :: a1,x1
! dont declare gamma_dp because it is within the module
!        double precision :: gamma_dp
        double precision :: xam,dlog,s,r,ga,t0,a,x
        integer :: k
        double precision :: gin, gim
        
        a = a1
        x = x1
        XAM=-X+A*DLOG(X)
        IF (XAM.GT.700.0.OR.A.GT.170.0) THEN
           WRITE(*,*)'a and/or x too large'
           STOP
        ENDIF
        IF (X.EQ.0.0) THEN
           GIN=0.0
           GIM = GAMMA_dp(A)
        ELSE IF (X.LE.1.0+A) THEN
           S=1.0D0/A
           R=S
           DO 10 K=1,60
              R=R*X/(A+K)
              S=S+R
              IF (DABS(R/S).LT.1.0D-15) GO TO 15
10         CONTINUE
15         GIN=DEXP(XAM)*S
           ga = GAMMA_DP(A)
           GIM=GA-GIN
        ELSE IF (X.GT.1.0+A) THEN
           T0=0.0D0
           DO 20 K=60,1,-1
              T0=(K-A)/(1.0D0+K/(X+T0))
20         CONTINUE
           GIM=DEXP(XAM)/(X+T0)
!           GA = GAMMA_dp(A)
!           GIN=GA-GIM
        ENDIF
        
        gamxinfdp = GIM
        return
        END function GAMXINFDP


! #####################################################################

! #####################################################################

!**************************** GAML02 *********************** 
!  This calculates Gamma(0.2,x)/Gamma[0.2], where is a ratio
!   It is used for qiacr with the gamma of volume to calculate what 
!   fraction of drops exceed a certain size (this version is for 40 micron drops)
! **********************************************************
      real FUNCTION GAML02(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=12)
      real gamxg(ng), xg(ng)
      DATA xg/0.01,0.02,0.025,0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/  &
     &  7.391019203578037e-8,0.02212726874591478,0.06959352407989682, &
     &  0.2355654024970809,0.46135930387500346,0.545435791452399,     &
     &  0.7371571313308203,                                           &
     &  0.8265676632204345,0.8640182781845841,0.8855756211304151,     &
     &  0.9245079225301251,                                           &
     &  0.9712578342732681/
      IF ( x .ge. xg(ng) ) THEN
        gaml02 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        gaml02 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN
!         GOTO 2 
          gaml02 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))* &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02

!**************************** GAML02d300 *********************** 
!  This calculates Gamma(0.2,x)/Gamma[0.2], where is a ratio
!   It is used for qiacr with the gamma of volume to calculate what 
!   fraction of drops exceed a certain size (this version is for 300 micron drops) (see zieglerstuff.nb)
! **********************************************************
      real FUNCTION GAML02d300(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=9)
      real gamxg(ng), xg(ng)
      DATA xg/0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/                           &
     &  0.0,                                  &
     &  7.391019203578011e-8,0.0002260640810600053,  &
     &  0.16567071824457152,                         &
     &  0.4231369044918005,0.5454357914523988,       &
     &  0.6170290936864555,                           &
     &  0.7471346054110058,0.9037156157718299 /
      IF ( x .ge. xg(ng) ) THEN
        GAML02d300 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        GAML02d300 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN
!         GOTO 2 
          GAML02d300 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))*  &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02d300
!c

! #####################################################################
! #####################################################################

!**************************** GAML02 *********************** 
!  This calculates Gamma(0.2,x)/Gamma[0.2], where is a ratio
!   It is used for qiacr with the gamma of volume to calculate what 
!   fraction of drops exceed a certain size (this version is for 500 micron drops) (see zieglerstuff.nb)
! **********************************************************
      real FUNCTION GAML02d500(x) 
      implicit none
      integer ig, i, ii, n, np
      real x
      integer ng
      parameter(ng=9)
      real gamxg(ng), xg(ng)
      DATA xg/0.04,0.075,0.1,0.25,0.5,0.75,1.,2.,10./ 
      DATA gamxg/  &
     &  0.0,0.0,   &
     &  2.2346039e-13, 0.0221272687459,  &
     &  0.23556540,  0.38710348,         &
     &  0.48136183,0.6565833,            &
     &  0.86918315 /
      IF ( x .ge. xg(ng) ) THEN
        GAML02d500 = xg(ng)
        RETURN
      ENDIF
      IF ( x .lt. xg(1) ) THEN
        GAML02d500 = 0.0
        RETURN
      ENDIF
      DO ii = 1,ng-1
        i = ng - ii
        n = i
        np = n + 1
        IF ( x .ge. xg(i) ) THEN
!         GOTO 2 
          GAML02d500 = gamxg(N)+((X-XG(N))/(XG(NP)-XG(N)))*  &
     &            ( gamxg(NP) - gamxg(N) ) 
          RETURN
        ENDIF
      ENDDO
      RETURN
      END FUNCTION GAML02d500
!c

! #####################################################################

! #####################################################################


        real function BETA(P,Q)
!
!       ==========================================
!       Purpose: Compute the beta function B(p,q)
!       Input :  p  --- Parameter  ( p > 0 )
!                q  --- Parameter  ( q > 0 )
!       Output:  BT --- B(p,q)
!       Routine called: GAMMA for computing gamma(x)
!       ==========================================
!
!        IMPLICIT real (A-H,O-Z)
        implicit none
        double precision p1,gp,q1,gq, ppq,gpq
        real p,q
        
        p1 = p
        q1 = q
        CALL GAMMADP(P1,GP)
        CALL GAMMADP(Q1,GQ)
        PPQ=P1+Q1
        CALL GAMMADP(PPQ,GPQ)
        beta=GP*GQ/GPQ
        RETURN
        END function BETA

! #####################################################################
! #####################################################################

      DOUBLE PRECISION FUNCTION GAMMA_DP(xx)

      implicit none
      double precision xx
      integer j

! Double precision ser,stp,tmp,x,y,cof(6)

      real*8 ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d+0,  &
     &            -86.50532032941677d0,   &
     &             24.01409824083091d0,   &
     &             -1.231739572450155d0,  &
     &              0.1208650973866179d-2,&
     &             -0.5395239384953d-5,   &
     &              2.5066282746310005d0/

      x = xx
      y = x
      tmp = x + 5.5d0
      tmp = (x + 0.5d0)*Log(tmp) - tmp
      ser = 1.000000000190015d0
      DO j=1,6
        y = y + 1.0d0
        ser = ser + cof(j)/y
      END DO
      gamma_dp = Exp(tmp + log(stp*ser/x))

      RETURN
      END function gamma_dp
! #####################################################################

        SUBROUTINE GAMMADP(X,GA)
!
!       ==================================================
!       Purpose: Compute gamma function Gamma(x)
!       Input :  x  --- Argument of Gamma(x)
!                       ( x is not equal to 0,-1,-2,...)
!       Output:  GA --- gamma(x)
!       ==================================================
!
!        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        implicit none
        
        double precision, parameter :: PI=3.141592653589793D0
        double precision :: x,ga,z,r,gr
        integer :: k,m1,m
        
        double precision :: G(26)
        
        IF (X.EQ.INT(X)) THEN
           IF (X.GT.0.0D0) THEN
              GA=1.0D0
              M1=X-1
              DO 10 K=2,M1
10               GA=GA*K
           ELSE
              GA=1.0D+300
           ENDIF
        ELSE
           IF (DABS(X).GT.1.0D0) THEN
              Z=DABS(X)
              M=INT(Z)
              R=1.0D0
              DO 15 K=1,M
15               R=R*(Z-K)
              Z=Z-M
           ELSE
              Z=X
           ENDIF
           DATA G/1.0D0,0.5772156649015329D0,                  &
     &          -0.6558780715202538D0, -0.420026350340952D-1,  &
     &          0.1665386113822915D0,-.421977345555443D-1,     &
     &          -.96219715278770D-2, .72189432466630D-2,       &
     &          -.11651675918591D-2, -.2152416741149D-3,       &
     &          .1280502823882D-3, -.201348547807D-4,          &
     &          -.12504934821D-5, .11330272320D-5,             &
     &          -.2056338417D-6, .61160950D-8,                 &
     &          .50020075D-8, -.11812746D-8,                   &
     &          .1043427D-9, .77823D-11,                       &
     &          -.36968D-11, .51D-12,                          &
     &          -.206D-13, -.54D-14, .14D-14, .1D-15/
           GR=G(26)
           DO 20 K=25,1,-1
20            GR=GR*Z+G(K)
           GA=1.0D0/(GR*Z)
           IF (DABS(X).GT.1.0D0) THEN
              GA=GA*R
              IF (X.LT.0.0D0) GA=-PI/(X*GA*DSIN(PI*X))
           ENDIF
        ENDIF
        RETURN
        END SUBROUTINE GAMMADP


! #####################################################################
! #####################################################################
!
!
! #####################################################################
      Function delbk(bb,nu,mu,k)
!   
!  Purpose: Caluculates collection coefficients following Siefert (2006)
!
!  delbk is equation (90) (b collecting b -- self-collection)
!  mass-diameter relationship: D = a*x**(b), where x = particle mass
!  general distribution: n(x) = A*x**(nu)*Exp(-lam*x**(mu))
!  where
!      A = mu*N/(Gamma((nu+1)/mu)) *lam**((nu+1)/mu)
!
!      lam = ( Gamma((nu+1)/mu)/Gamma((nu+2)/mu) * xbar )**(-mu)
!
!     where  xbar = L/N  (mass content)/(number concentration) = q*rhoa/N
!

      implicit none
      real delbk, gamma
      real nu, mu, bb
      integer k
      
      real tmp, del
      real x1, x2, x3, x4
      integer i

        tmp = ((1.0 + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x1 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((2.0 + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x2 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1.0 + 2.0*bb + k + nu)/mu)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x3 = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
      
!      delbk =  &
!     &  ((Gamma((1.0 + nu)/mu)/Gamma((2.0 + nu)/mu))**(2.0*bb + k)* &
!     &    Gamma((1.0 + 2.0*bb + k + nu)/mu))/Gamma((1.0 + nu)/mu)

      delbk =  &
     &  ((x1/x2)**(2.0*bb + k)* &
     &    x3)/x1
      
      RETURN
      END  Function delbk
      
! #####################################################################
!
!
! #####################################################################
! Equation (91) in Seifert and Beheng (2006) ("a" collecting "b")
      Function delabk(ba,bb,nua,nub,mua,mub,k)
      
      implicit none
      real delabk, gamma
      real nua, mua, ba
      integer k
      real nub, mub, bb
      
      integer i
      real tmp,del
      
      real g1pnua, g2pnua, g1pbapnua, g1pbbpk, g1pnub, g2pnub
      
        tmp = (1. + nua)/mua
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        IF ( i+1 > ngm0 ) THEN
          write(0,*) 'delabk: i+1 > ngm0!!!!',i,ngm0,nua,mua,tmp
          STOP
        ENDIF
        g1pnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
!        write(91,*) 'delabk: g1pnua,gamma = ',g1pnua,Gamma((1. + nua)/mua)

        tmp = ((2. + nua)/mua)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g2pnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + ba + nua)/mua)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pbapnua = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pnub = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((2 + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g2pnub = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = ((1. + bb + k + nub)/mub)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1pbbpk = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

      delabk =  &
     &  (2.*(g1pnua/g2pnua)**ba*     &
     &    g1pbapnua*                                               &
     &    (g1pnub/g2pnub)**(bb + k)*                                &
     &    g1pbbpk)/                                                &
     &  (g1pnua*g1pnub)              
      
      RETURN
      END Function delabk
      

! #####################################################################
!
! #####################################################################
!--------------------------------------------------------------------------
      subroutine cld_cpu(string)

      implicit none
      character( LEN = * ) string
      
      return
      
      end subroutine cld_cpu

!
!--------------------------------------------------------------------------
!
!--------------------------------------------------------------------------
!
      subroutine sediment1d(dtp,nx,ny,nz,an,na,nor,norz,xfall,dn,dz3d,dz3dinv, &
     &                    t0,t7,infdo,jslab,its,jts,  &
     &   timesed1,timesed2,timesed3,zmaxsed,timesetvt) ! used for timing
!
! Sedimentation driver -- column by column
!
!  Written by ERM 10/2011
!
!
!
      implicit none

      integer nx,ny,nz,nor,norz,ngt,jgs,na,ia
      integer id ! =1 use density, =0 no density
      integer :: its,jts ! SW point of local tile
      
      integer ng1
      parameter(ng1 = 1)

      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real dz3d(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real dz3dinv(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t7(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

!      real gz(-nor+ng1:nz+nor),z1d(-nor+ng1:nz+nor,4)
      real dtp
      real xfall(nx,ny,na)  ! array for stuff landing on the ground
      real xfall0(nx,ny)    ! dummy array
      integer infdo
      integer jslab ! which line of xfall to use
            
      integer ix,jy,kz,ndfall,n,k,il,in
      real tmp, vtmax, dtptmp, dtfrac
      real, parameter :: dz = 200.

      real :: xvt(nz+1,nx,3,lc:lhab) ! (nx,nz,2,lc:lhab) ! 1=mass-weighted, 2=number-weighted
      real :: tmpn(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real :: tmpn2(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real :: z(-nor+ng1:nx+nor,-norz+ng1:nz+norz,lr:lhab)
      real :: db1(nx,nz+1),dtz1(nz+1,nx,0:1),dz2dinv(nz+1,nx),db1inv(nx,nz+1)
      
      real :: rhovtzx(nz,nx)
      
      double precision :: timesed1,timesed2,timesed3, zmaxsed,timesetvt,dummy
      double precision :: dt1,dt2,dt3,dt4

      integer,parameter :: ngs = 128 
      integer :: ngscnt,mgs,ipconc0
      
      real ::  qx(ngs,lv:lhab) 
      real ::  qxw(ngs,ls:lhab) 
      real ::  cx(ngs,lc:lhab) 
      real ::  xv(ngs,lc:lhab) 
      real ::  vtxbar(ngs,lc:lhab,3) 
      real ::  xmas(ngs,lc:lhab) 
      real ::  xdn(ngs,lc:lhab) 
      real ::  xdia(ngs,lc:lhab,3) 
      real ::  vx(ngs,li:lhab) 
      real ::  alpha(ngs,lc:lhab) 
      real ::  zx(ngs,lr:lhab) 
      logical :: hasmass(nx,lc+1:lhab)

      integer igs(ngs),kgs(ngs)
      
      real rho0(ngs),temcg(ngs)

      real temg(ngs)
      
      real rhovt(ngs)
      
      real cwnc(ngs),cinc(ngs)
      real fadvisc(ngs),cwdia(ngs),cipmas(ngs)
      
      real cimasn,cimasx,cnina(ngs),cimas(ngs)
      
      real cnostmp(ngs)
      

!-----------------------------------------------------------------------------

      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze
      integer :: plo, phi

      logical :: debug_mpi = .TRUE.

! ###################################################################




      kzb = 1
      kze = nz

      ixb = 1
      ixe = nx


      jy = 1
      jgs = jy


!
!  zero the precip flux arrays (2d)
!

      xvt(:,:,:,:) = 0.0

      if ( ndebug .gt. 0 ) write(0,*) 'dbg = 3a'


      DO kz = kzb,kze
      DO ix = ixb,ixe
       db1(ix,kz) = dn(ix,jy,kz)
       db1inv(ix,kz) = 1./dn(ix,jy,kz)
       rhovtzx(kz,ix) = Sqrt(rho00*db1inv(ix,kz) )
      ENDDO
      ENDDO

      DO kz = kzb,kze
      DO ix = ixb,ixe
       dtz1(kz,ix,0) = dz3dinv(ix,jy,kz)
       dtz1(kz,ix,1) = dz3dinv(ix,jy,kz)*db1inv(ix,kz) 
       dz2dinv(kz,ix) = dz3dinv(ix,jy,kz)
      ENDDO
      ENDDO

      IF ( lzh .gt. 1 ) THEN
      DO kz = kzb,kze
      DO ix = ixb,ixe
        an(ix,jy,kz,lzh) = Max( 0., an(ix,jy,kz,lzh) )
      ENDDO
      ENDDO
      ENDIF

      
      DO il = lc+1,lhab
       DO ix = ixb,ixe
!        hasmass(ix,il) = Any( an(ix,jy,:,il) > qxmin(il) )
       ENDDO
      ENDDO




      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3a2'

! loop over columns
      DO ix = ixb,ixe
      
      dummy = 0.d0

      
      call ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ix, & 
     &  xvt, rhovtzx, & 
     &  an,dn,ipconc,t0,t7,cwmasn,cwmasx, & 
     &  cwradn, & 
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx, & 
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,              &
     &  infdo,0               &
     & )


! loop over each species and do sedimentation for all moments
     DO il = lc,lhab

!       IF ( .not. hasmass(ix,il) ) CYCLE

!      plo = nz
!      phi = 0


      vtmax = 0.0
      
      do kz = kzb,kze

      ! apply limit vtmaxsed (08/20/2015)
      xvt(kz,ix,1,il) = Min( vtmaxsed,  xvt(kz,ix,1,il) )
      xvt(kz,ix,2,il) = Min( vtmaxsed,  xvt(kz,ix,2,il) )
      xvt(kz,ix,3,il) = Min( vtmaxsed,  xvt(kz,ix,3,il) )
      
      vtmax = Max(vtmax,xvt(kz,ix,1,il)*dz2dinv(kz,ix))
      vtmax = Max(vtmax,xvt(kz,ix,2,il)*dz2dinv(kz,ix))
      vtmax = Max(vtmax,xvt(kz,ix,3,il)*dz2dinv(kz,ix))

!      IF ( dtp*xvt(kz,ix,1,il)*dz2dinv(kz,ix) >= 0.7 .or. &
!     &     dtp*xvt(kz,ix,2,il)*dz2dinv(kz,ix) >= 0.7 .or. &
!     &     dtp*xvt(kz,ix,3,il)*dz2dinv(kz,ix) >= 0.7 ) THEN
!          
!          zmaxsed = Max(zmaxsed, float(kz) )
!!          plo = Min(plo,kz)
!!          phi = Max(phi,kz)
!           
!      ENDIF
      
      ENDDO
      
      IF ( vtmax == 0.0 ) CYCLE


      
      IF ( dtp*vtmax .lt. 0.7 ) THEN ! check whether multiple steps are needed.
        ndfall = 1
      ELSE
       IF ( dtp > 20.0 ) THEN ! more stringent subdivision for large time steps
         ndfall = Max(2, Int(dtp*vtmax/0.7) + 1)
       ELSE ! more relaxed for small time steps, but might still be a problem for very thin vertical layers near the ground
         ndfall = 1+Int(dtp*vtmax + 0.301)
       ENDIF
      ENDIF
      
      IF ( ndfall .gt. 1 ) THEN
        dtptmp = dtp/Real(ndfall)
!        write(0,*) 'subdivide fallout on its,jts,ix,plo,phi = ',its,jts,ix,plo,phi
!        write(0,*) 'for il,jsblab,c,ndfall = ',il,jslab,dtp*vtmax,ndfall
      ELSE
        dtptmp = dtp
      ENDIF
      
      dtfrac = dtptmp/dtp


      DO n = 1,ndfall

      IF ( do_accurate_sedimentation .and. n .ge. 2 ) THEN
!
!  zero the precip flux arrays (2d)
!
      
!      xvt(:,:,:,il) = 0.0
      dummy = 0.d0
      call ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ix, & 
     &  xvt, rhovtzx, & 
     &  an,dn,ipconc,t0,t7,cwmasn,cwmasx, & 
     &  cwradn, & 
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx, & 
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,             &
     &  infdo,il)


      DO kz = kzb,kze
      ! apply limit vtmaxsed (08/20/2015)
        xvt(kz,ix,1,il) = Min( vtmaxsed,  xvt(kz,ix,1,il) )
        xvt(kz,ix,2,il) = Min( vtmaxsed,  xvt(kz,ix,2,il) )
        xvt(kz,ix,3,il) = Min( vtmaxsed,  xvt(kz,ix,3,il) )
      ENDDO




      ENDIF ! (n .ge. 2)


        IF ( il >= lr .and. ( infall .eq. 3 .or. infall .eq. 4 ) .and. ln(il) > 0 ) THEN
           IF ( (il .eq. lr .and. irfall .eq. infall .and. lzr < 1) .or. (il .ge. lh .and. lz(il) .lt. 1 ) ) THEN
            call calczgr1d(nx,ny,nz,nor,na,an,ixe,kze, & 
     &         z,db1,jgs,ipconc, dnu(il), il, ln(il), qxmin(il), xvmn(il), xvmx(il), lvol(il), xdn0(il), ix )
           ENDIF
        ENDIF

      if (ndebug .gt. 0 ) write(0,*) 'dbg = 1b'

! mixing ratio

      call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &             an,db1,il,1,xfall,dtz1,ix)


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3c'

! volume

      IF ( ldovol .and. il >= li ) THEN
        IF ( lvol(il) .gt. 1 ) THEN
         call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &              an,db1,lvol(il),0,xfall,dtz1,ix)
        ENDIF
      ENDIF


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3d'

      
      IF ( ipconc .gt. 0 ) THEN !{
        IF ( ipconc .ge. ipc(il) ) THEN

      IF ( ( infall .ge. 2 .or. (infall .eq. 0 .and. il .lt. lh) ) .and. lz(il) .lt. 1) THEN !{
!
! load number conc. into tmpn to do fallout by mass-weighted mean fall speed
!  to put a lower bound on number conc.
!

        IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and. ( il .eq. lh .or. il .eq. lhl .or.  & 
     &      ( il .eq. lr .and. irfall .eq. infall) ) ) THEN

          DO kz = kzb,kze
!            DO ix = ixb,ixe
              tmpn2(ix,jy,kz) = z(ix,kz,il)
!            ENDDO
          ENDDO
          DO kz = kzb,kze
!            DO ix = ixb,ixe
              tmpn(ix,jy,kz) = an(ix,jy,kz,ln(il))
!            ENDDO
          ENDDO
        
        ELSE
          
          DO kz = kzb,kze
!            DO ix = ixb,ixe
              tmpn(ix,jy,kz) = an(ix,jy,kz,ln(il))
!            ENDDO
          ENDDO

        ENDIF

      ENDIF !}


      if (ndebug .gt. 0 ) write(0,*) 'dbg = 3f'

       in = 2
       IF ( infall .eq. 1 ) in = 1

         call fallout1d(nx,ny,nz,nor,na,dtptmp,dtfrac,jgs,xvt(1,1,in,il), & 
     &        an,db1,ln(il),0,xfall,dtz1,ix)


         IF ( lz(il) .lt. 1 ) THEN ! if not 3-moment, run one of the correction schemes
         IF ( (infall .ge. 2 .or. infall .eq. 3) .and. .not. (infall .eq. 0 .and. il .ge. lh) & 
     &       .and. ( il .eq. lr .or. (il .ge. li .and. il .le. lhab) )) THEN
!     :        .or. il .eq. lhl )) THEN
           
           xfall0(:,jgs) = 0.0

           IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and.  & 
     &        ( il .ge. lh .or. (il .eq. lr .and. irfall .eq. infall) ) ) THEN
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,3,il), & 
     &         tmpn2,db1,1,0,xfall0,dtz1,ix)
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &         tmpn,db1,1,0,xfall0,dtz1,ix)
           ELSE
             call fallout1d(nx,ny,nz,nor,1,dtptmp,dtfrac,jgs,xvt(1,1,1,il), & 
     &         tmpn,db1,1,0,xfall0,dtz1,ix)
           ENDIF

           IF ( ( infall .eq. 3 .or. infall .eq. 4 ) .and. ( (il .eq. lr .and. irfall .eq. infall) & 
     &            .or. il .ge. lh ) ) THEN
! "Method I" - dbz correction

             call calcnfromz1d(nx,ny,nz,nor,na,an,tmpn2,ixe,kze, & 
     &       z,db1,jgs,ipconc, dnu(il), il, ln(il), qxmin(il), xvmn(il), xvmx(il),tmpn,  & 
     &       lvol(il), rho_qh, infall, ix)

           ELSEIF ( infall .eq. 5 .and. il .ge. lh .or. ( il == lr .and. irfall == 5 ) ) THEN

             DO kz = kzb,kze
!              DO ix = ixb,ixe
               an(ix,jgs,kz,ln(il)) = Max( an(ix,jgs,kz,ln(il)), 0.5* ( an(ix,jgs,kz,ln(il)) + tmpn(ix,jy,kz) ))
              
!              ENDDO
             ENDDO           

           ELSEIF ( .not. (il .eq. lr .and. irfall .eq. 0) ) THEN
! "Method II" M-wgt N-fallout correction

             DO kz = kzb,kze
!              DO ix = ixb,ixe

               an(ix,jgs,kz,ln(il)) = Max( an(ix,jgs,kz,ln(il)), tmpn(ix,jy,kz) )
              
!              ENDDO
             ENDDO
           ENDIF 
           ENDIF ! lz(il) .lt. 1
           

         ENDIF
        ENDIF


      ENDIF !}


      ENDDO ! n=1,ndfall
      ENDDO ! il
      
      ENDDO ! ix



      
      RETURN
      END SUBROUTINE SEDIMENT1D


! #####################################################################

!
! #####################################################################


!
!--------------------------------------------------------------------------
!
!--------------------------------------------------------------------------
!
      subroutine fallout1d(nx,ny,nz,nor,na,dtp,dtfrac,jgs,vt,   &
     &  a,db1,ia,id,xfall,dtz1,ixcol)
!
! First-order, upwind fallout scheme
!
!  Written by ERM 6/10/2011
!
!
!
      implicit none

      integer nx,ny,nz,nor,ngt,jgs,na,ia
      integer id ! =1 use density, =0 no density
      integer ng1
      parameter(ng1 = 1)
      integer :: ixcol

!      real dz3dinv(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
!      real a(nx,ny,nz,na)
      real a(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na) ! quantity to be 'advected'
      real vt(nz+1,nx)  ! terminal speed for a
      real dtp,dtfrac
      real cmax
      real xfall(nx,ny,na)  ! array for stuff landing on the ground
      real db1(nx,nz+1),dtz1(nz+1,nx,0:1)

! Local
           
      integer ix,jy,kz,n,k
      integer iv1,iv2
      real tmp
      integer imn,imx,kmn,kmx
      real qtmp1(nz+1)

!-----------------------------------------------------------------------------

      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze

      logical :: debug_mpi = .TRUE.

! ###################################################################

      jy = 1

      iv1 = 0
      iv2 = 0

      imn = nx
      imx = 1
      kmn = nz
      kmx = 1

      cmax = 0.0

      kzb = 1
      kze = nz

      ixb = ixcol
      ixe = ixcol
      ix  = ixcol

      qtmp1(nz+1) = 0.0
      
      DO kz = kzb,kze
!        DO ix = ixb,ixe
!         cmax = Max(cmax, vt(ix,kz)*dz3dinv(ix,jy,kz)) 
         
         IF ( id == 1 ) THEN
         qtmp1(kz) = a(ix,jgs,kz,ia)*vt(kz,ix)*db1(ix,kz)
         ELSE
         qtmp1(kz) = a(ix,jgs,kz,ia)*vt(kz,ix)
         ENDIF
         
         IF ( a(ix,jgs,kz,ia) .ne. 0.0 ) THEN
!           imn = Min(ix,imn)
!           imx = Max(ix,imx)
           kmn = Min(kz,kmn)
           kmx = Max(kz,kmx)
         ENDIF
!        ENDDO
      ENDDO
      
      kmn = Max(1,kmn-1)
      
! first check if fallout is worth doing
!      IF ( cmax .eq. 0.0 .or. imn .gt. imx ) THEN
!        RETURN
!      ENDIF
      
      IF ( kmn == 1 ) THEN
      
      kz = 1
!      do ix = imn,imx ! 1,nx-1
         xfall(ix,jy,ia) = xfall(ix,jy,ia) + a(ix,jgs,kz,ia)*vt(kz,ix)*dtfrac
!      enddo
      
      ENDIF

      do kz = 1,nz
!      do ix = 1,nx
        a(ix,jgs,kz,ia) =  a(ix,jgs,kz,ia) + dtp*dtz1(kz,ix,id)*(qtmp1(kz+1) - qtmp1(kz) )
!      enddo
      enddo

      
      RETURN
      END SUBROUTINE FALLOUT1D

! ##############################################################################
! ##############################################################################

      subroutine calczgr1d(nx,ny,nz,nor,na,a,ixe,kze,              &
     &    z,db,jgs,ipconc, alpha, l,ln, qmin, xvmn,xvmx, lvol, rho_qx, ixcol)


      implicit none

      integer nx,ny,nz,nor,na,ngt,jgs
      integer :: ixcol
      integer, parameter :: norz = 3
      real a(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,na)
      real z(-nor+1:nx+nor,-nor+1:nz+nor,lr:lhab)   ! reflectivity
      real db(nx,nz+1)  ! air density
!      real gt(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,ngt)

      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer l   ! index for q
      integer ln  ! index for N
      integer lvol ! index for volume
      real    rho_qx


      integer ix,jy,kz
      real vr,qr,nrx,rd,xv,g1,zx,chw,xdn
      
      
      jy = jgs
      ix = ixcol
      
      IF ( l .eq. lh .or. l .eq. lhl .or. ( l .eq. lr .and. imurain == 1 )  ) THEN
      
      
      DO kz = 1,kze
          
          
          
          IF (  a(ix,jy,kz,l) .gt. qmin .and. a(ix,jy,kz,ln) .gt. 1.e-15 ) THEN
            
            IF ( lvol .gt. 1 ) THEN
                IF ( a(ix,jy,kz,lvol) .gt. 0.0 ) THEN
                  xdn = db(ix,kz)*a(ix,jy,kz,l)/a(ix,jy,kz,lvol)
                  xdn = Min( 900., Max( hdnmn, xdn ) )
                ELSE
                  xdn = rho_qx
                ENDIF
            ELSE
                xdn = rho_qx
            ENDIF

            IF ( l == lr ) xdn = 1000.

            qr = a(ix,jy,kz,l)
            xv = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            chw = a(ix,jy,kz,ln)

             IF ( xv .lt. xvmn .or. xv .gt. xvmx ) THEN
              xv = Min( xvmx, Max( xvmn,xv ) )
              chw = db(ix,kz)*a(ix,jy,kz,l)/(xv*xdn)
             ENDIF

             g1 = (6.0 + alpha)*(5.0 + alpha)*(4.0 + alpha)/  &
     &            ((3.0 + alpha)*(2.0 + alpha)*(1.0 + alpha))
             zx = g1*db(ix,kz)**2*(a(ix,jy,kz,l))*a(ix,jy,kz,l)/chw
!             z(ix,kz,l)  = 1.e18*zx*(6./(pi*1000.))**2
             z(ix,kz,l)  = zx*(6./(pi*1000.))**2


!          IF ( ny.eq.2 .and. kz .ge. 25 .and. kz .le. 29 .and. z(ix,kz,l) .gt. 0. ) THEN
!             write(*,*) 'calczgr: z,dbz,xdn = ',ix,kz,z(ix,kz,l),10*log10(z(ix,kz,l)),xdn
!          ENDIF
          
          ELSE
           
            z(ix,kz,l) = 0.0
           
          ENDIF
          
      ENDDO
      
      ELSEIF ( l .eq. lr .and. imurain == 3) THEN

      xdn = 1000.
      
      DO kz = 1,kze
          IF (  a(ix,jy,kz,l) .gt. qmin .and. a(ix,jy,kz,ln) .gt. 1.e-15 ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
!            z(ix,kz,l) = 3.6e18*(rnu+2.0)*a(ix,jy,kz,ln)*vr**2/(rnu+1.0)
            z(ix,kz,l) = 3.6*(rnu+2.0)*a(ix,jy,kz,ln)*vr**2/(rnu+1.0)
!            qr = a(ix,jy,kz,lr)
!            nrx = a(ix,jy,kz,lnr)
          
          ELSE
           
            z(ix,kz,l) = 0.0
           
          ENDIF
      
          
      ENDDO
      
      ENDIF
      
      RETURN
      
      END subroutine calczgr1d

! ##############################################################################
! ##############################################################################
!
!  Subroutine to correct number concentration to prevent reflectivity growth by 
!  sedimentation in 2-moment ZXX scheme.
!  Calculation is in a slab (constant jgs)
!

      subroutine calcnfromz1d(nx,ny,nz,nor,na,a,t0,ixe,kze,    &
     &    z0,db,jgs,ipconc, alpha, l,ln, qmin, xvmn,xvmx,t1, &
     &    lvol, rho_qx, infall, ixcol)

      
      implicit none

      integer nx,ny,nz,nor,na,ngt,jgs,ixcol

      real a(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,na)  ! sedimented N and q
      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor)    ! sedimented reflectivity
      real t1(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor)    ! sedimented N (by Vm)
!      real gt(-nor+1:nx+nor,-nor+1:ny+nor,-nor+1:nz+nor,ngt)
      real z0(-nor+1:nx+nor,-nor+1:nz+nor,lr:lhab)   ! initial reflectivity

      real db(nx,nz+1)  ! air density
      
      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer l   ! index for q
      integer ln  ! index for N
      integer lvol ! index for volume
      real    rho_qx
      integer infall
      
      
      integer ix,jy,kz
      double precision vr,qr,nrx,rd,g1,zx,chw,z,znew,zt,zxt
      real xv,xdn
      integer :: ndbz, nmwgt, nnwgt, nwlessthanz
      
      ndbz = 0
      nmwgt = 0
      nnwgt = 0
      nwlessthanz = 0
      

      
      jy = jgs
      ix = ixcol
      
      IF ( l .eq. lh .or. l .eq. lhl .or. ( l == lr .and. imurain == 1 ) ) THEN
      
             g1 = (6.0 + alpha)*(5.0 + alpha)*(4.0 + alpha)/  &
     &            ((3.0 + alpha)*(2.0 + alpha)*(1.0 + alpha))
      
      DO kz = 1,kze

         
          IF (   t0(ix,jy,kz) .gt. 0. ) THEN ! {
            
            IF ( lvol .gt. 1 ) THEN
               IF ( a(ix,jy,kz,lvol) .gt. 0.0 ) THEN
                 xdn = db(ix,kz)*a(ix,jy,kz,l)/a(ix,jy,kz,lvol)
                 xdn = Min( 900., Max( hdnmn, xdn ) )
               ELSE 
                 xdn = rho_qx
               ENDIF
            ELSE
               xdn = rho_qx
            ENDIF
            
            IF ( l == lr ) xdn = 1000.
          
            qr = a(ix,jy,kz,l)
            xv = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            chw = a(ix,jy,kz,ln)

             IF ( xv .lt. xvmn .or. xv .gt. xvmx ) THEN
              xv = Min( xvmx, Max( xvmn,xv ) )
              chw = db(ix,kz)*a(ix,jy,kz,l)/(xv*xdn)
             ENDIF

             zx = g1*db(ix,kz)**2*( a(ix,jy,kz,l))*a(ix,jy,kz,l)/chw
             z  = zx*(6./(pi*1000.))**2

            
           IF ( (z .gt. t0(ix,jy,kz) .and. z .gt. 0.0 .and.  &
     &           t0(ix,jy,kz) .gt. z0(ix,kz,l) )) THEN !{
           
            zx = t0(ix,jy,kz)/((6./(pi*1000.))**2)
            
            nrx =  g1*db(ix,kz)**2*( a(ix,jy,kz,l))*a(ix,jy,kz,l)/zx
            IF ( infall .eq. 3 ) THEN
              IF ( nrx .gt. a(ix,jy,kz,ln) ) THEN
                ndbz = ndbz + 1
                IF ( t1(ix,jy,kz) .lt. ndbz ) nwlessthanz = nwlessthanz + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
              a(ix,jy,kz,ln) = Max( real(nrx), a(ix,jy,kz,ln) )
            ELSE
             IF (  nrx .gt. a(ix,jy,kz,ln) .and. t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
              IF ( nrx .lt. t1(ix,jy,kz)  ) THEN
                ndbz = ndbz + 1
              ELSE
                nmwgt = nmwgt + 1
                IF ( t1(ix,jy,kz) .lt. ndbz ) nwlessthanz = nwlessthanz + 1
              ENDIF
             ELSE
              nnwgt = nnwgt + 1
             ENDIF
              
              a(ix,jy,kz,ln) = Max(Min( real(nrx), t1(ix,jy,kz) ), a(ix,jy,kz,ln) )
            ENDIF

           ELSE ! } {
             IF ( t1(ix,jy,kz) .gt. 0 .and. a(ix,jy,kz,ln) .gt. 0 ) THEN
              IF ( t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
                nmwgt = nmwgt + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
            ENDIF
            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )
            nrx = a(ix,jy,kz,ln)



           ENDIF ! }

           ! }
          ELSE ! {
            IF ( t1(ix,jy,kz) .gt. 0 .and. a(ix,jy,kz,ln) .gt. 0 ) THEN
              IF ( t1(ix,jy,kz) .gt. a(ix,jy,kz,ln) ) THEN
                nmwgt = nmwgt + 1
              ELSE
                nnwgt = nnwgt + 1
              ENDIF
            ENDIF
          ENDIF! }
          
      ENDDO
      
      
      ELSEIF ( l .eq. lr .and. imurain == 3) THEN

      xdn = 1000.
      
      DO kz = 1,kze
          IF (  t0(ix,jy,kz) .gt. 0. ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn*a(ix,jy,kz,ln))
            z = 3.6*(rnu+2.0)*a(ix,jy,kz,ln)*vr**2/(rnu+1.0)
          
             IF ( z .gt. t0(ix,jy,kz) .and. z .gt. 0.0 .and.  &
     &          t0(ix,jy,kz) .gt. 0.0                         &
     &          .and. t0(ix,jy,kz) .gt. z0(ix,kz,l) ) THEN

            vr = db(ix,kz)*a(ix,jy,kz,l)/(xdn)
             chw =  a(ix,jy,kz,ln)
            nrx =   3.6*(rnu+2.0)*vr**2/((rnu+1.0)*t0(ix,jy,kz))
             IF ( infall .eq. 3 ) THEN
              a(ix,jy,kz,ln) = Max( real(nrx), a(ix,jy,kz,ln) )
            ELSEIF ( infall .eq. 4 ) THEN
              a(ix,jy,kz,ln) = Max( Min( real(nrx), t1(ix,jy,kz)), a(ix,jy,kz,ln) )
            ENDIF

           ELSE

            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )

           ENDIF

          ELSE

            a(ix,jy,kz,ln) = Max(t1(ix,jy,kz), a(ix,jy,kz,ln) )

          ENDIF


      ENDDO

      ENDIF

      RETURN

      END subroutine calcnfromz1d


! ##############################################################################
! ##############################################################################
!
!  Subroutine to calculate number concentrations from initial state that has only mixing ratio.
!  N will be in #/kg, NOT #/m^3, since sedimentation is done next.
!

!
! 10.27.2015: Added hail calculation
!
      subroutine calcnfromq(nx,ny,nz,an,na,nor,norz,dn)

      
      implicit none

      integer nx,ny,nz,nor,norz,na,ngt,jgs,ixcol

      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)  ! scalars (q, N, Z)

      real dn(nx,nz+1)  ! air density
      
      integer ixe,kze
      real    alpha
      real    qmin
      real    xvmn,xvmx
      integer ipconc
      integer lvol ! index for volume
      integer infall
      
      
      integer ix,jy,kz
      double precision vr,q,nrx,rd,g1h,g1hl,g1r,g1s,zx,chw,z,znew,zt,zxt,n1,laminv1
      double precision :: zr, zs, zh, dninv
      real, parameter :: xn0s = 3.0e6, xn0r = 8.0e6, xn0h = 4.0e4, xn0hl = 4.0e4
      real, parameter :: xdnr = 1000., xdns = 100. ,xdnh = 700.0, xdnhl = 900.0
      real, parameter :: zhlfac = 1./(pi*xdnhl*xn0hl)
      real, parameter :: zhfac = 1./(pi*xdnh*xn0h)
      real, parameter :: zrfac = 1./(pi*xdnr*xn0r)
      real, parameter :: zsfac = 1./(pi*xdns*xn0s)
      real, parameter :: g0 = (6.0)*(5.0)*(4.0)/((3.0)*(2.0)*(1.0))

      real xv,xdn
      integer :: ndbz, nmwgt, nnwgt, nwlessthanz

! ------------------------------------------------------------------
      
      
      jy = 1
      
      
         g1h = (6.0 + alphah)*(5.0 + alphah)*(4.0 + alphah)/  &
     &        ((3.0 + alphah)*(2.0 + alphah)*(1.0 + alphah))

         g1hl = (6.0 + alphahl)*(5.0 + alphahl)*(4.0 + alphahl)/  &
     &        ((3.0 + alphahl)*(2.0 + alphahl)*(1.0 + alphahl))
     
         IF ( imurain == 3 ) THEN
         g1r = (rnu+2.0)/(rnu+1.0)
         ELSE ! imurain == 1
         g1r = (6.0 + alphar)*(5.0 + alphar)*(4.0 + alphar)/  &
     &        ((3.0 + alphar)*(2.0 + alphar)*(1.0 + alphar))
         ENDIF

         g1s = (snu+2.0)/(snu+1.0)
      
      DO kz = 1,nz
       DO ix = 1,nx ! ixcol

         dninv = 1./dn(ix,kz)
         
   !  Cloud droplets
         
         IF ( lnc > 1 ) THEN
           IF ( an(ix,jy,kz,lnc) <= 0.1*cxmin .and. an(ix,jy,kz,lc) > qxmin(lc) ) THEN
             an(ix,jy,kz,lnc) = qccn
           ENDIF
         ENDIF

   !  rain
         
         IF ( lnr > 1 ) THEN
           IF ( an(ix,jy,kz,lnr) <= 0.1*cxmin .and. an(ix,jy,kz,lr) > qxmin(lr) ) THEN

             q = an(ix,jy,kz,lr)
             
             laminv1 = (dn(ix,kz) * q * zrfac)**(0.25)  ! inverse of slope
             
             n1 = laminv1*xn0r  ! number concentration for inv. exponential single moment input
             
             nrx =  n1*g1r/g0   ! number concentration for different shape parameter

             an(ix,jy,kz,lnr) = nrx ! *dninv ! convert to number mixing ratio
             
           ENDIF
         ENDIF

  ! snow
         IF ( lns > 1 ) THEN
           IF ( an(ix,jy,kz,lns) <= 0.1*cxmin .and. an(ix,jy,kz,ls) > qxmin(ls) ) THEN

             q = an(ix,jy,kz,ls)
             
             laminv1 = (dn(ix,kz) * q * zsfac)**(0.25)  ! inverse of slope
             
             n1 = laminv1*xn0s  ! number concentration for inv. exponential single moment input
             
             nrx =  n1*g1s/g0   ! number concentration for different shape parameter

             an(ix,jy,kz,lns) = nrx ! *dninv ! convert to number mixing ratio
             
           ENDIF
         ENDIF
         
    ! graupel

         IF ( lnh > 1 ) THEN
           IF ( an(ix,jy,kz,lnh) <= 0.1*cxmin .and. an(ix,jy,kz,lh) > qxmin(lh) ) THEN
             IF ( lvh > 1 ) THEN
               IF ( an(ix,jy,kz,lvh) <= 0.0 ) THEN
                 an(ix,jy,kz,lvh) = an(ix,jy,kz,lh)/xdnh
               ENDIF
             ENDIF

             q = an(ix,jy,kz,lh)
             
             laminv1 = (dn(ix,kz) * q * zhfac)**(0.25)  ! inverse of slope
             
             n1 = laminv1*xn0h  ! number concentration for inv. exponential single moment input
             
             nrx =  n1*g1h/g0   ! number concentration for different shape parameter

             an(ix,jy,kz,lnh) = nrx ! *dninv ! convert to number mixing ratio

           ENDIF
         ENDIF

    ! hail

         IF ( lnhl > 1 .and. lhl > 1 ) THEN
           IF ( an(ix,jy,kz,lnhl) <= 0.1*cxmin .and. an(ix,jy,kz,lhl) > qxmin(lhl) ) THEN
             IF ( lvhl > 1 ) THEN
               IF ( an(ix,jy,kz,lvhl) <= 0.0 ) THEN
                 an(ix,jy,kz,lvhl) = an(ix,jy,kz,lhl)/xdnhl
               ENDIF
             ENDIF

             q = an(ix,jy,kz,lhl)
             
             laminv1 = (dn(ix,kz) * q * zhlfac)**(0.25)  ! inverse of slope
             
             n1 = laminv1*xn0hl  ! number concentration for inv. exponential single moment input
             
             nrx =  n1*g1hl/g0   ! number concentration for different shape parameter

             an(ix,jy,kz,lnhl) = nrx ! *dninv ! convert to number mixing ratio

           ENDIF
         ENDIF
 
      ENDDO ! ix
      ENDDO ! kz
      
      RETURN
      
      END subroutine calcnfromq

! #####################################################################
! #####################################################################

   SUBROUTINE calc_eff_radius    &
     &  (nx,ny,nz,na,jyslab & 
     &  ,nor,norz & 
     &  ,t1,t2,t3  & 
     &  ,an,dn )

   implicit none

      integer, parameter :: ng1 = 1
      integer :: nx,ny,nz,na
      integer :: ng
      integer :: nor,norz, jyslab ! ,nht,ngt,igsr
      real    :: dtp  ! time step


!
! external temporary arrays
!

      real t1(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t3(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      

      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      


      
    ! local
    
      real pb(-norz+ng1:nz+norz)
      real pinit(-norz+ng1:nz+norz)

! 
!  declarations microphysics and for gather/scatter
!
      integer nxmpb,nzmpb,nxz
      integer mgs,ngs,numgs,inumgs
      parameter (ngs=1)
      integer ngscnt,igs(ngs),kgs(ngs)
      real rho0(ngs)

      integer ix,kz,i,n, kp1
      integer :: jy, jgs
      integer ixb,ixe,jyb,jye,kzb,kze
    
      integer itile,jtile,ktile
      integer ixend,jyend,kzend,kzbeg
      integer nxend,nyend,nzend,nzbeg

      real :: qx(ngs,lv:lhab)
      real :: cx(ngs,lc:lhab)
      real :: xv(ngs,lc:lhab)
      real :: xmas(ngs,lc:lhab)
      real :: xdn(ngs,lc:lhab)
      real :: xdia(ngs,lc:lhab,3)
      real :: alpha(ngs,lc:lhab)
      
      real :: gamc1,gamc2,gami1,gami2,gams1,gams2, factor_c, factor_i, factor_s
      real :: lam_c, lam_i, lam_s
      integer :: il


! -------------------------------------------------------------------------------
      itile = nx
      jtile = ny
      ktile = nz
      ixend = nx
      jyend = ny
      kzend = nz
      nxend = nx + 1
      nyend = ny + 1
      nzend = nz
      kzbeg = 1
      nzbeg = 1

       jy = 1
       pb(:) = 0.0
       pinit(:) = 0.0

     gamc1 = Gamma(2. + cnu)
     gamc2 = 1. ! Gamma[1 + alphac]
     gami1 = Gamma(2. + cinu)
     gami2 = 1. ! Gamma[1 + alphac]
     gams1 = Gamma(2. + cinu)
     gams2 = Gamma(1. + snu)

     factor_c = (1. + cnu)*Gamma(1. + cnu)/Gamma(5./3. + cnu)
     factor_i = (1. + cinu)*Gamma(1. + cinu)/Gamma(5./3. + cinu)
     factor_s = (1. + snu)*Gamma(1. + snu)/Gamma(5./3. + snu)

!
!     jy = 1 ! working on a 2d slab
!!  VERY IMPORTANT:  SET jgs = jy

      jgs = jy

      mgs = 1
      DO kz = 1,nz
       DO ix = 1,nx ! ixcol

         rho0(mgs) = dn(ix,jy,kz)
         DO il = lc,ls
          qx(mgs,il) = max(an(ix,jy,kz,il), 0.0) 
          cx(mgs,il) = max(an(ix,jy,kz,ln(il)), 0.0) 
         ENDDO
         
         IF ( qx(mgs,lc) > qxmin(lc) ) THEN
! Lambda for cloud droplets 
         lam_c = ((cx(mgs,lc)*(Pi/6.)*xdn0(lc)*Gamc1)/(qx(mgs,lc)*rho0(mgs)*Gamc2))**(1./3.)
          t1(ix,jy,kz) = 0.5*factor_c/lam_c
         ENDIF

         IF ( qx(mgs,li) > qxmin(li) ) THEN
! Lambda for cloud ice 
         lam_i = ((cx(mgs,li)*(Pi/6.)*xdn0(li)*Gami1)/(qx(mgs,li)*rho0(mgs)*Gami2))**(1./3.)
          t2(ix,jy,kz) = 0.5*factor_i/lam_i
         ENDIF

         IF ( qx(mgs,ls) > qxmin(ls) ) THEN
! Lambda for snow
         lam_s = ((cx(mgs,ls)*(Pi/6.)*xdn0(ls)*Gams1)/(qx(mgs,ls)*rho0(mgs)*Gams2))**(1./3.)
          t3(ix,jy,kz) = 0.5*factor_s/lam_s
         ENDIF

      
       ENDDO ! ix
      ENDDO ! kz

   RETURN
   END SUBROUTINE calc_eff_radius


! #####################################################################
! #####################################################################

      SUBROUTINE QVEXCESS(ngs,mgs,qwvp0,qv0,qcw1,pres,thetap0,theta0, &
     &    qvex,pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ss1,pk,ngscnt)
      
!#####################################################################
!  Purpose: find the amount of vapor that can be condensed to liquid
!#####################################################################

      implicit none

      integer ngs,mgs,ngscnt
      
      real theta2temp
      
      real qvex
      
      integer nqsat
      real fqsat, cbw
      
      real ss1  ! 'target' supersaturation
!
!  input arrays
!
      real qv0(ngs), qcw1(ngscnt), pres(ngs), qwvp0(mgs)
      real thetap0(ngs), theta0(ngs)
      real fcqv1(ngs), felvcp(ngs), pi0(ngs)
      real pk(ngs)
      
      real tabqvs(nqsat)
!
! Local stuff
!
      
      integer itertd
      integer ltemq
      real gamss
      real theta(ngs), qvap(ngs), pqs(ngs), qcw(ngs), qwv(ngs)
      real qcwtmp(ngs), qss(ngs), qvs(ngs), qwvp(ngs)
      real dqcw(ngs), dqwv(ngs), dqvcnd(ngs)
      real temg(ngs), temcg(ngs), thetap(ngs)
      
      real tfr
      parameter ( tfr = 273.15 )
            
!      real poo,cap
!      parameter ( cap = rd/cp, poo = 1.0e+05 )
!
!
!  Modified Straka adjustment (nearly identical to Tao et al. 1989 MWR)
!
!
!
!  set up temperature and vapor arrays
!
      pqs(mgs) = (380.0)/(pres(mgs))
      thetap(mgs) = thetap0(mgs)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      qwvp(mgs) = qwvp0(mgs)
      qvap(mgs) = max( (qwvp0(mgs) + qv0(mgs)), 0.0 )
      temg(mgs) = theta(mgs)*pk(mgs) ! ( pres(mgs) / poo ) ** cap
!      temg(mgs) = theta2temp( theta(mgs), pres(mgs) )
!
!
!
!  reset temporaries for cloud particles and vapor
!
      
      qwv(mgs) = max( 0.0, qvap(mgs) )
      qcw(mgs) = max( 0.0, qcw1(mgs) )
!
!
      qcwtmp(mgs) = qcw(mgs)
      temcg(mgs) = temg(mgs) - tfr
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )

      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qss(mgs) = (0.01*ss1 + 1.0)*qvs(mgs)
!
!  iterate  adjustment
!
      do itertd = 1,2
!
!
!  calculate super-saturation
!
      dqcw(mgs) = 0.0
      dqwv(mgs) = ( qwv(mgs) - qss(mgs) )
!
!  evaporation and sublimation adjustment
!
      if( dqwv(mgs) .lt. 0. ) then           !  subsaturated
        if( qcw(mgs) .gt. -dqwv(mgs) ) then  ! check if qc can make up all of the deficit
          dqcw(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                 !  otherwise make all qc available for evap
          dqcw(mgs) = -qcw(mgs)
          dqwv(mgs) = dqwv(mgs) + qcw(mgs)
        end if
!
        qwvp(mgs) = qwvp(mgs) - ( dqcw(mgs)  )  ! add to perturbation vapor
!
        qcw(mgs) = qcw(mgs) + dqcw(mgs)

        thetap(mgs) = thetap(mgs) +  &
     &                1./pi0(mgs)*  &
     &                (felvcp(mgs)*dqcw(mgs) )

      end if  ! dqwv(mgs) .lt. 0. (end of evap/sublim)
!
! condensation/deposition
!
      IF ( dqwv(mgs) .ge. 0. ) THEN
!
      dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/  &
     &  ((temg(mgs)-cbw)**2))
!
!
      dqcw(mgs) = dqvcnd(mgs)
!
      thetap(mgs) = thetap(mgs) +  &
     &   (felvcp(mgs)*dqcw(mgs) )    &
     & / (pi0(mgs))
      qwvp(mgs) = qwvp(mgs) - ( dqvcnd(mgs) )
      qcw(mgs) = qcw(mgs) + dqcw(mgs)
!
      END IF !  dqwv(mgs) .ge. 0.

      theta(mgs) = thetap(mgs) + theta0(mgs)
      temg(mgs) = theta(mgs)*pk(mgs) ! ( pres(mgs) / poo ) ** cap
!      temg(mgs) = theta2temp( theta(mgs), pres(mgs) )
      qvap(mgs) = Max((qwvp(mgs) + qv0(mgs)), 0.0)
      temcg(mgs) = temg(mgs) - tfr
!      tqvcon = temg(mgs)-cbw
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qcw(mgs) = max( 0.0, qcw(mgs) )
      qwv(mgs) = max( 0.0, qvap(mgs))
      qss(mgs) = (0.01*ss1 + 1.0)*qvs(mgs)
      end do
!
!  end the saturation adjustment iteration loop
!
!
      qvex = Max(0.0, qcw(mgs) - qcw1(mgs) )

      RETURN
      END SUBROUTINE QVEXCESS

! #####################################################################
! #####################################################################





!
! ##############################################################################
!
      SUBROUTINE setvtz(ngscnt,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp, &
     &                 xmas,vtxbar,xdn,xvmn0,xvmx0,xv,cdx,            &
     &                 ipconc1,ndebug1,ngs,nz,kgs,fadvisc,   &
     &                 cwmasn,cwmasx,cwradn,cnina,cimna,cimxa,      &
     &                 itype1a,itype2a,temcg,infdo,alpha,ildo,axh,bxh,axhl,bxhl)


      implicit none
      
      integer ngscnt,ngs0,ngs,nz
!      integer infall    ! whether to calculate number-weighted fall speeds
      
      real xv(ngs,lc:lhab)
      real qx(ngs,lv:lhab)
      real qxw(ngs,ls:lhab)
      real cx(ngs,lc:lhab)
      real vtxbar(ngs,lc:lhab,3)
      real xmas(ngs,lc:lhab)
      real xdn(ngs,lc:lhab)
      real xdia(ngs,lc:lhab,3)
      real xvmn0(lc:lhab), xvmx0(lc:lhab)
      real qxmin(lc:lhab)
      real cdx(lc:lhab)
      real alpha(ngs,lc:lhab)
      
      real rho0(ngs),rhovt(ngs),temcg(ngs)
      real cno(lc:lhab)
      real cnostmp(ngs)
      
      real cwc1, cimna, cimxa
      real cnina(ngs)
      integer kgs(ngs)
      real fadvisc(ngs)
      real fsw
      
      integer ipconc1
      integer ndebug1
      
      integer, intent (in) :: itype1a,itype2a,infdo
      integer, intent (in) :: ildo ! which species to do, or all if ildo=0

      real :: axh(ngs),bxh(ngs)
      real :: axhl(ngs),bxhl(ngs)
      
! Local vars

      
      
      real cd
      real cwc0 ! ,cwc1
      real :: cwch(ngscnt), cwchl(ngscnt)
      real :: cwchtmp,cwchltmp,xnutmp
      real pii
      real cimasx,cimasn
      real cwmasn,cwmasx,cwradn
      real cwrad
      real vr,rnux
      real alp
      
      real ccimx

      integer mgs
      
      real arx,frx,vtrain,fw
      real fwlo,fwhi,rfwdiff
      real ar,br,cs,ds
!      real gf4p5, gf4ds, gf4br, ifirst, gf1ds
!      real gfcinu1, gfcinu1p47, gfcinu2p47
      real gr
      real rwrad,rwdia
      real mwfac
      integer il

!      save gf4p5, gf4ds, gf4br, ifirst, gf1ds
!      save gfcinu1, gfcinu1p47, gfcinu2p47
!      data ifirst /0/
      
      real bta1,cnit
      parameter ( bta1 = 0.6, cnit = 1.0e-02 )
      real x,y,tmp,del
      real aax,bbx,delrho
      integer :: indxr
      real mwt, nwt
      real, parameter :: rho00 = 1.225
      integer i
      real xvbarmax

      integer l1, l2


!
! set values
!
!      cwmasn = 5.23e-13  ! radius of 5.0e-6
!      cwradn = 5.0e-6
!      cwmasx = 5.25e-10  ! radius of 50.0e-6

      fwlo = 0.2                ! water fraction to start weighting toward rain fall speed
      fwhi = 0.4                ! water fraction at which rain fall speed only is used
      rfwdiff = 1./(fwhi - fwlo)
      
!      pi = 4.0*atan(1.0)
      pii = piinv ! 1.0/pi

      arx = 10.
      frx = 516.575 ! raind fit parameters for arx*(1 - Exp(-fx*d)), where d is rain diameter in meters.

      ar = 841.99666  
      br = 0.8
      gr = 9.8
!  new values for  cs and ds
      cs = 12.42
      ds = 0.42

      IF ( ildo == 0 ) THEN
        l1 = lc
        l2 = lhab
      ELSE
        l1 = ildo
        l2 = ildo
      ENDIF

!      IF ( ifirst .eq. 0 ) THEN
!        ifirst = 1
!        gf4br = gamma(4.0+br)
!        gf4ds = gamma(4.0+ds)
!!        gf1ds = gamma(1.0+ds)
!        gf4p5 = gamma(4.0+0.5)
!        gfcinu1 = gamma(cinu + 1.0)
!        gfcinu1p47 = gamma(cinu + 1.47167)
!        gfcinu2p47 = gamma(cinu + 2.47167)
        
        IF ( lh  .gt. 1 ) THEN
          IF ( dmuh == 1.0 ) THEN
            cwchtmp = ((3. + dnu(lh))*(2. + dnu(lh))*(1.0 + dnu(lh)))**(-1./3.)
          ELSE
            cwchtmp = 6.0*pii*gamma( (xnu(lh) + 1.)/xmu(lh) )/gamma( (xnu(lh) + 2.)/xmu(lh) )
          ENDIF
        ENDIF
        IF ( lhl .gt. 1 ) THEN
          IF ( dmuhl == 1.0 ) THEN
            cwchltmp = ((3. + dnu(lhl))*(2. + dnu(lhl))*(1.0 + dnu(lhl)))**(-1./3.)
          ELSE
            cwchltmp = 6.0*pii*gamma( (xnu(lhl) + 1)/xmu(lhl) )/gamma( (xnu(lhl) + 2)/xmu(lhl) )
          ENDIF
        ENDIF

        IF ( ipconc .le. 5 ) THEN
          IF ( lh  .gt. 1 ) cwch(:) =  cwchtmp 
          IF ( lhl .gt. 1 ) cwchl(:) = cwchltmp
        ELSE
          DO mgs = 1,ngscnt
          
          IF ( lh  .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
           IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
            IF ( dmuh == 1.0 ) THEN
              cwch(mgs) = ((3. + alpha(mgs,lh))*(2. + alpha(mgs,lh))*(1.0 + alpha(mgs,lh)))**(-1./3.)
             ELSE
             xnutmp = (alpha(mgs,lh) - 2.0)/3.0
             cwch(mgs) =  6.0*pii*gamma( (xnutmp + 1.)/xmu(lh) )/gamma( (xnutmp + 2.)/xmu(lh) )
            ENDIF
           ELSE
             cwch(mgs) = cwchtmp
           ENDIF
          ENDIF
          IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
            IF ( dmuhl == 1.0 ) THEN
              cwchl(mgs) = ((3. + alpha(mgs,lhl))*(2. + alpha(mgs,lhl))*(1.0 + alpha(mgs,lhl)))**(-1./3.)
             ELSE
             xnutmp = (alpha(mgs,lhl) - 2.0)/3.0
             cwchl(mgs) = 6.0*pii*gamma( (xnutmp + 1)/xmu(lhl) )/gamma( (xnutmp + 2)/xmu(lhl) )
            ENDIF
           ELSE
             cwchl(mgs) = cwchltmp
           ENDIF
          ENDIF
          
          ENDDO
        
        ENDIF
       

      cimasn = Min( cimas0, 6.88e-13)
      cimasx = 1.0e-8
      ccimx = 5000.0e3   ! max of 5000 per liter

      cwc1 = 6.0/(pi*1000.)
      cwc0 = pii ! 6.0*pii
      mwfac = 6.0**(1./3.)

      
      if (ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set scale diameter'
!


!
!  cloud water variables
! ################################################################
!
!  DROPLETS
!
!
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set cloud water variables'
      
      IF ( ildo == 0 .or. ildo == lc ) THEN
      
      do mgs = 1,ngscnt
      xv(mgs,lc) = 0.0
      
      IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN !{
      
      IF ( ipconc .ge. 2 .and. cx(mgs,lc) .gt. 1.0e-9 ) THEN !{
        xmas(mgs,lc) =  &
     &    min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),cwmasn),cwmasx )
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
      ELSE
       IF ( ipconc .lt. 2 ) THEN
         cx(mgs,lc) = rho0(mgs)*ccn/rho00 ! scales to local density, relative to standard air density
       ENDIF
       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 0.01 ) THEN !{
        xmas(mgs,lc) =  &
     &     min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),xdn(mgs,lc)*xvmn(lc)), &
     &      xdn(mgs,lc)*xvmx(lc) )
        
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
        cx(mgs,lc) = qx(mgs,lc)*rho0(mgs)/xmas(mgs,lc)
        
       ELSEIF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .le. 0.01 ) THEN
        xmas(mgs,lc) = xdn(mgs,lc)*4.*pi/3.*(5.0e-6)**3
        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc)
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
        
       ELSE
        xmas(mgs,lc) = cwmasn
        xv(mgs,lc) = xmas(mgs,lc)/1000.
! do not define ccw here! it can feed back to ccn!!!    cx(mgs,lc) = 0.0 ! cwnc(mgs)
       ENDIF !}
      ENDIF !}
!      IF ( ipconc .lt. 2 ) THEN
!        xmas(mgs,lc) = &
!     &    min( max(qx(mgs,lc)*rho0(mgs)/cwnc(mgs),cwmasn),cwmasx )
!        cx(mgs,lc) = Max(1.0,qx(mgs,lc)*rho0(mgs)/xmas(mgs,lc))
!      ELSE
!        cwnc(mgs) = an(igs(mgs),jgs,kgs(mgs),lnc)
!        cx(mgs,lc) = cwnc(mgs)
!      ENDIF
      xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**(1./3.)
      xdia(mgs,lc,2) = xdia(mgs,lc,1)**2
      xdia(mgs,lc,3) = xdia(mgs,lc,1)
      cwrad = 0.5*xdia(mgs,lc,1)
      IF ( fadvisc(mgs) > 0.0 ) THEN
      vtxbar(mgs,lc,1) =  &
     &   (2.0*gr*xdn(mgs,lc) *(cwrad**2)) &
     &  /(9.0*fadvisc(mgs))
      ELSE
       vtxbar(mgs,lc,1) = 0.0
      ENDIF

      
      ELSE
       xmas(mgs,lc) = cwmasn
       IF ( ipconc .le. 1 ) cx(mgs,lc) = 0.01
       xdia(mgs,lc,1) = 2.*cwradn
       xdia(mgs,lc,2) = 4.*cwradn**2
       xdia(mgs,lc,3) = xdia(mgs,lc,1)
       vtxbar(mgs,lc,1) = 0.0
       
      ENDIF !} qcw .gt. qxmin(lc)
      
      end do
      
      ENDIF



!
! cloud ice variables
! columns
!
! ################################################################
!
!  CLOUD ICE
!
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set cip'
      
      IF ( li .gt. 1 .and. ( ildo == 0 .or. ildo == li ) ) THEN
      do mgs = 1,ngscnt
       xdn(mgs,li)  = 900.0
      IF ( ipconc .eq. 0 ) THEN
!       cx(mgs,li) = min(cnit*exp(-temcg(mgs)*bta1),1.e+09)
        cx(mgs,li) = cnina(mgs)
       IF ( cimna .gt. 1.0 ) THEN
         cx(mgs,li) = Max(cimna,cx(mgs,li))
       ENDIF
       IF ( cimxa .gt. 1.0 ) THEN
         cx(mgs,li) = Min(cimxa,cx(mgs,li))
       ENDIF
! erm 3/28/2002
       IF ( itype1a .ge. 1 .or. itype2a .ge. 1 ) THEN
        cx(mgs,li) = Max(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasx)
        cx(mgs,li) = Min(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasn)
       ENDIF
!
       cx(mgs,li) = max(1.0e-20,cx(mgs,li))
!       cx(mgs,li) = Min(ccimx, cx(mgs,li))

      
      ELSEIF ( ipconc .ge. 1 ) THEN
        IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
         cx(mgs,li) = Max(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasx)
         cx(mgs,li) = Min(cx(mgs,li),qx(mgs,li)*rho0(mgs)/cimasn)
!         cx(mgs,li) = Max(1.0,cx(mgs,li))
        ENDIF
      ENDIF
      
      IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
      xmas(mgs,li) = &
     &     max( qx(mgs,li)*rho0(mgs)/cx(mgs,li), cimasn )
!     &  min( max(qx(mgs,li)*rho0(mgs)/cx(mgs,li),cimasn),cimasx )
      
!      if ( temcg(mgs) .gt. 0.0 ) then
!      xdia(mgs,li,1) = 0.0
!      else
      if ( xmas(mgs,li) .gt. 0.0 ) THEN ! cimasn ) then
!c      xdia(mgs,li,1) = 0.4892*(xmas(mgs,li)**(0.4554))
!       xdia(mgs,li,1) = 0.1871*(xmas(mgs,li)**(0.3429))

!       xdia(mgs,li,1) = (132.694*5.40662/xmas(mgs,li))**(-1./2.9163)  ! for inverse exponential distribution
       IF ( ixtaltype == 1 ) THEN ! column
       xdia(mgs,li,1) = 0.1871*(xmas(mgs,li)**(0.3429))
       xdia(mgs,li,3) = 0.1871*(xmas(mgs,li)**(0.3429))
       ELSEIF  ( ixtaltype == 2 ) THEN ! disk
        xdia(mgs,li,1) = 0.277823*xmas(mgs,li)**0.359971
        xdia(mgs,li,3) = 0.277823*xmas(mgs,li)**0.359971
       ENDIF
      end if
!      end if
!      xdia(mgs,li,1) = max(xdia(mgs,li,1), 5.e-6)
!      xdia(mgs,li,1) = min(xdia(mgs,li,1), 1000.e-6)

       IF ( ipconc .ge. 0 ) THEN
!      vtxbar(mgs,li,1) = rhovt(mgs)*49420.*40.0005/5.40662*xdia(mgs,li,1)**(1.415) ! mass-weighted
!      vtxbar(mgs,li,1) = (4.942e4)*(xdia(mgs,li,1)**(1.4150))
        xv(mgs,li) = xmas(mgs,li)/xdn(mgs,li)
        IF ( ixtaltype == 1 ) THEN ! column
        tmp = (67056.6300748612*rhovt(mgs))/  &
     &   (((1.0 + cinu)/xv(mgs,li))**0.4716666666666667*gfcinu1)
        vtxbar(mgs,li,2) = tmp*gfcinu1p47
        vtxbar(mgs,li,1) = tmp*gfcinu2p47/(1. + cinu)
        vtxbar(mgs,li,3) = vtxbar(mgs,li,1) 
        ELSEIF  ( ixtaltype == 2 ) THEN ! disk -- but just use column fall speed for now
        tmp = (67056.6300748612*rhovt(mgs))/  &
     &   (((1.0 + cinu)/xv(mgs,li))**0.4716666666666667*gfcinu1)
        vtxbar(mgs,li,2) = tmp*gfcinu1p47
        vtxbar(mgs,li,1) = tmp*gfcinu2p47/(1. + cinu)
        vtxbar(mgs,li,3) = vtxbar(mgs,li,1) 
        
        ENDIF
!      vtxbar(mgs,li,1) = vtxbar(mgs,li,2)*(1.+cinu)/(1. + cinu)
!      xdn(mgs,li)   = min(max(769.8*xdia(mgs,li,1)**(-0.0140),300.0),900.0)
!      xdn(mgs,li) = 900.0
        xdia(mgs,li,2) = xdia(mgs,li,1)**2
!      vtxbar(mgs,li,1) = vtxbar(mgs,li,1)*rhovt(mgs)
       ELSE
         xdia(mgs,li,1) = max(xdia(mgs,li,1), 10.e-6)
         xdia(mgs,li,1) = min(xdia(mgs,li,1), 1000.e-6)
         vtxbar(mgs,li,1) = (4.942e4)*(xdia(mgs,li,1)**(1.4150))
!      xdn(mgs,li)   = min(max(769.8*xdia(mgs,li,1)**(-0.0140),300.0),900.0)
         xdn(mgs,li) = 900.0
         xdia(mgs,li,2) = xdia(mgs,li,1)**2
         vtxbar(mgs,li,1) = vtxbar(mgs,li,1)*rhovt(mgs)
         xv(mgs,li) = xmas(mgs,li)/xdn(mgs,li)
       ENDIF ! ipconc gt 3
      ELSE
       xmas(mgs,li) = 1.e-13
       xdn(mgs,li)  = 900.0
       xdia(mgs,li,1) = 1.e-7
       xdia(mgs,li,2) = (1.e-14)
       xdia(mgs,li,3) = 1.e-7
       vtxbar(mgs,li,1) = 0.0
!       cicap(mgs) = 0.0
!       ciat(mgs) = 0.0
      ENDIF
      end do
      
      ENDIF ! li .gt. 1


! ################################################################
!
!  RAIN
!
      
!
      IF ( ildo == 0 .or. ildo == lr ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then
      
!      IF ( qx(mgs,lr) .gt. 10.0e-3 ) &
!     &  write(0,*)  'RAIN1: ',igs(mgs),kgs(mgs),qx(mgs,lr)
      
      if ( ipconc .ge. 3 ) then
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*Max(1.0e-11,cx(mgs,lr)))
        xvbarmax = xvmx(lr)
        IF ( imaxdiaopt == 1 ) THEN
          xvbarmax = xvmx(lr)
        ELSEIF ( imaxdiaopt == 2 ) THEN ! test against maximum mass diameter
         IF ( imurain == 1 ) THEN
           xvbarmax = xvmx(lr)/((3. + alpha(mgs,lr))**3/((3. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(1. + alpha(mgs,lr))))
         ELSEIF ( imurain == 3 ) THEN
           
         ENDIF
        ELSEIF ( imaxdiaopt == 3 ) THEN ! test against mass-weighted diameter
         IF ( imurain == 1 ) THEN
           xvbarmax = xvmx(lr)/((4. + alpha(mgs,lr))**3/((3. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(1. + alpha(mgs,lr))))
         ELSEIF ( imurain == 3 ) THEN
           
         ENDIF
        ENDIF
       
        IF ( xv(mgs,lr) .gt. xvbarmax ) THEN
          xv(mgs,lr) = xvbarmax
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvbarmax*xdn(mgs,lr))
        ELSEIF ( xv(mgs,lr) .lt. xvmn(lr) ) THEN
          xv(mgs,lr) = xvmn(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmn(lr)*xdn(mgs,lr))
        ENDIF


        xmas(mgs,lr) = xv(mgs,lr)*xdn(mgs,lr)
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.) ! xdia(mgs,lr,1)
        IF ( imurain == 3 ) THEN
!          xdia(mgs,lr,1) = (6.*pii*xv(mgs,lr)/(alpha(mgs,lr)+1.))**(1./3.)
          xdia(mgs,lr,1) = xdia(mgs,lr,3) ! formulae for Ziegler (1985) use mean volume diameter, not lambda**(-1)
        ELSE ! imurain == 1, Characteristic diameter (1/lambda)
          xdia(mgs,lr,1) = (6.*pii*xv(mgs,lr)/((alpha(mgs,lr)+3.)*(alpha(mgs,lr)+2.)*(alpha(mgs,lr)+1.)))**(1./3.)
        ENDIF
!        rwrad(mgs) = 0.5*xdia(mgs,lr,1)

! Inverse exponential version:
!        xdia(mgs,lr,1) =
!     &  (qx(mgs,lr)*rho0(mgs)
!     & /(pi*xdn(mgs,lr)*cx(mgs,lr)))**(0.333333)
      ELSE
        xdia(mgs,lr,1) = &
     &  (qx(mgs,lr)*rho0(mgs)/(pi*xdn(mgs,lr)*cno(lr)))**(0.25) 
        xmas(mgs,lr) = xdn(mgs,lr)*(pi/6.)*xdia(mgs,lr,1)**3
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.)
        cx(mgs,lr) = cno(lr)*xdia(mgs,lr,1)
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*cx(mgs,lr))
      end if
      else
        xdia(mgs,lr,1) = 1.e-9
        xdia(mgs,lr,3) = 1.e-9
        xmas(mgs,lr) = xdn(mgs,lr)*(pi/6.)*xdia(mgs,lr,1)**3
!        rwrad(mgs) = 0.5*xdia(mgs,lr,1)
      end if
      xdia(mgs,lr,2) = xdia(mgs,lr,1)**2
!      xmas(mgs,lr) = xdn(mgs,lr)*(pi/6.)*xdia(mgs,lr,1)**3
      end do
      
      ENDIF
! ################################################################
!
!  SNOW
!

      IF ( ls .gt. 1 .and. ( ildo == 0 .or. ildo == ls ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,ls) .gt. qxmin(ls) ) then
      if ( ipconc .ge. 4 ) then ! 

        xv(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xdn(mgs,ls)*Max(1.0e-9,cx(mgs,ls)))
!      parameter( xvmn(lr)=2.8866e-13, xvmx(lr)=4.1887e-9 )  ! mks
        xmas(mgs,ls) = xv(mgs,ls)*xdn(mgs,ls)

        IF ( xv(mgs,ls) .lt. xvmn(ls) .or. xv(mgs,ls) .gt. xvmx(ls) ) THEN
          xv(mgs,ls) = Min( xvmx(ls), Max( xvmn(ls),xv(mgs,ls) ) )
          xmas(mgs,ls) = xv(mgs,ls)*xdn(mgs,ls)
          cx(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xmas(mgs,ls))
        ENDIF

        xdia(mgs,ls,1) = (xv(mgs,ls)*cwc0*6.0)**(1./3.)
        xdia(mgs,ls,3) = xdia(mgs,ls,1)

      ELSE
        xdia(mgs,ls,1) =  &
     &    (qx(mgs,ls)*rho0(mgs)/(pi*xdn(mgs,ls)*cnostmp(mgs)))**(0.25) 
        cx(mgs,ls) = cnostmp(mgs)*xdia(mgs,ls,1)
        xv(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xdn(mgs,ls)*cx(mgs,ls))
        xdia(mgs,ls,3) = (xv(mgs,ls)*cwc0*6.0)**(1./3.)
      end if
      else
      xdia(mgs,ls,1) = 1.e-9
      xdia(mgs,ls,3) = 1.e-9
      cx(mgs,ls) = 0.0
      end if
      xdia(mgs,ls,2) = xdia(mgs,ls,1)**2
!      swdia3(mgs) = xdia(mgs,ls,2)*xdia(mgs,ls,1)
!      xmas(mgs,ls) = xdn(mgs,ls)*(pi/6.)*swdia3(mgs)
      end do
      
      ENDIF ! ls .gt 1
!
!
! ################################################################
!
!  GRAUPEL
!

      IF ( lh .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lh) .gt. qxmin(lh) ) then
      if ( ipconc .ge. 5 ) then

        xv(mgs,lh) = rho0(mgs)*qx(mgs,lh)/(xdn(mgs,lh)*Max(1.0e-9,cx(mgs,lh)))
        xmas(mgs,lh) = xv(mgs,lh)*xdn(mgs,lh)

        IF ( xv(mgs,lh) .lt. xvmn(lh) .or. xv(mgs,lh) .gt. xvmx(lh) ) THEN
          xv(mgs,lh) = Min( xvmx(lh), Max( xvmn(lh),xv(mgs,lh) ) )
          xmas(mgs,lh) = xv(mgs,lh)*xdn(mgs,lh)
          cx(mgs,lh) = rho0(mgs)*qx(mgs,lh)/(xmas(mgs,lh))
        ENDIF

         xdia(mgs,lh,3) = (xv(mgs,lh)*6.*pii)**(1./3.) ! mwfac*xdia(mgs,lh,1) ! (xv(mgs,lh)*cwc0*6.0)**(1./3.)
         IF ( dmuh == 1.0 ) THEN
           xdia(mgs,lh,1) = cwch(mgs)*xdia(mgs,lh,3)
         ELSE
           xdia(mgs,lh,1) = (xv(mgs,lh)*cwch(mgs))**(1./3.)
         ENDIF

      ELSE
      xdia(mgs,lh,1) =  &
     &  (qx(mgs,lh)*rho0(mgs)/(pi*xdn(mgs,lh)*cno(lh)))**(0.25) 
      cx(mgs,lh) = cno(lh)*xdia(mgs,lh,1)
      xv(mgs,lh) = Max(xvmn(lh), rho0(mgs)*qx(mgs,lh)/(xdn(mgs,lh)*cx(mgs,lh)) )
      xdia(mgs,lh,3) = (xv(mgs,lh)*6./pi)**(1./3.) 
      end if
      else
      xdia(mgs,lh,1) = 1.e-9
      xdia(mgs,lh,3) = 1.e-9
      end if
      xdia(mgs,lh,2) = xdia(mgs,lh,1)**2
!      hwdia3(mgs) = xdia(mgs,lh,2)*xdia(mgs,lh,1)
!      xmas(mgs,lh) = xdn(mgs,lh)*(pi/6.)*hwdia3(mgs)
      end do
      
      ENDIF

!
! ################################################################
!
!  HAIL
!

      IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lhl) .gt. qxmin(lhl) ) then
      if ( ipconc .ge. 5 ) then

        xv(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/(xdn(mgs,lhl)*Max(1.0e-9,cx(mgs,lhl)))
        xmas(mgs,lhl) = xv(mgs,lhl)*xdn(mgs,lhl)
!        write(0,*) 'setvt: xv = ',xv(mgs,lhl),xdn(mgs,lhl),cx(mgs,lhl),xmas(mgs,lhl),qx(mgs,lhl)

        IF ( xv(mgs,lhl) .lt. xvmn(lhl) .or. xv(mgs,lhl) .gt. xvmx(lhl) ) THEN
          xv(mgs,lhl) = Min( xvmx(lhl), Max( xvmn(lhl),xv(mgs,lhl) ) )
          xmas(mgs,lhl) = xv(mgs,lhl)*xdn(mgs,lhl)
          cx(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/(xmas(mgs,lhl))
        ENDIF

        xdia(mgs,lhl,3) = (xv(mgs,lhl)*6./pi)**(1./3.) ! mwfac*xdia(mgs,lh,1) ! (xv(mgs,lh)*cwc0*6.0)**(1./3.)
         IF ( dmuhl == 1.0 ) THEN
           xdia(mgs,lhl,1) = cwchl(mgs)*xdia(mgs,lhl,3)
         ELSE
           xdia(mgs,lhl,1) = (xv(mgs,lhl)*cwchl(mgs))**(1./3.)
         ENDIF
        
!        write(0,*) 'setvt: xv = ',xv(mgs,lhl),xdn(mgs,lhl),cx(mgs,lhl),xdia(mgs,lhl,3)
      ELSE
      xdia(mgs,lhl,1) = &
     &  (qx(mgs,lhl)*rho0(mgs)/(pi*xdn(mgs,lhl)*cno(lhl)))**(0.25) 
      cx(mgs,lhl) = cno(lhl)*xdia(mgs,lhl,1)
      xv(mgs,lhl) = Max(xvmn(lhl), rho0(mgs)*qx(mgs,lhl)/(xdn(mgs,lhl)*cx(mgs,lhl)) )
      xdia(mgs,lhl,3) = (xv(mgs,lhl)*6./pi)**(1./3.) 
      end if
      else
      xdia(mgs,lhl,1) = 1.e-9
      xdia(mgs,lhl,3) = 1.e-9
      end if
      xdia(mgs,lhl,2) = xdia(mgs,lhl,1)**2
!      hwdia3(mgs) = xdia(mgs,lh,2)*xdia(mgs,lh,1)
!      xmas(mgs,lh) = xdn(mgs,lh)*(pi/6.)*hwdia3(mgs)
      end do
      
      ENDIF
!      
!
!
!  Set terminal velocities...
!    also set drag coefficients (moved to start of subroutine)
!
!      cdx(lr) = 0.60
!      cdx(lh) = 0.45
!      cdx(lhl) = 0.45
!      cdx(lf) = 0.45
!      cdx(lgh) = 0.60
!      cdx(lgm) = 0.80
!      cdx(lgl) = 0.80
!      cdx(lir) = 2.00
!
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set terminal velocities'
!
!
! ################################################################
!
!  RAIN
!
      IF ( ildo == 0 .or. ildo == lr ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then
      IF ( ipconc .lt. 3 ) THEN
        vtxbar(mgs,lr,1) = rainfallfac*(ar*gf4br/6.0)*(xdia(mgs,lr,1)**br)*rhovt(mgs)
!        write(91,*) 'vtxbar: ',vtxbar(mgs,lr,1),mgs,gf4br,xdia(mgs,lr,1),rhovt(mgs)
      ELSE
        
        IF ( imurain == 1 ) THEN ! DSD of Diameter
        
        ! using functional form of  arx*(1 - Exp(-frx*diameter) ), with arx =       arx = 10.
        !  and frx = 516.575 ! raind fit parameters for arx*(1 - Exp(-fx*d)), where d is rain diameter in meters.

        
          alp = alpha(mgs,lr)
          
          vtxbar(mgs,lr,1) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 4.0) ) ! mass weighted
          
          IF ( infdo .ge. 1 .and. rssflg == 1 ) THEN
            vtxbar(mgs,lr,2) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 1.0) ) ! number weighted
          ELSE
            vtxbar(mgs,lr,2) = vtxbar(mgs,lr,1)
          ENDIF
          
          IF ( infdo .ge. 2 .and. rssflg == 1 ) THEN
            vtxbar(mgs,lr,3) = rhovt(mgs)*arx*(1.0 - (1.0 + frx*xdia(mgs,lr,1))**(-alp - 7.0) ) ! z-weighted
          ELSE
            vtxbar(mgs,lr,3) = vtxbar(mgs,lr,1)
          ENDIF
          
!          write(91,*) 'setvt: alp,vn,vm,vz = ',alp,vtxbar(mgs,lr,2), vtxbar(mgs,lr,1), vtxbar(mgs,lr,3),alpha(mgs,lr)

        ELSEIF ( imurain == 3 ) THEN ! DSD of Volume
        
        IF ( lzr < 1 ) THEN ! not 3-moment rain
        rwdia = Min( xdia(mgs,lr,1), 8.0e-3 )
        
         vtxbar(mgs,lr,1) = rhovt(mgs)*6.0*pii*( 0.04771 + 3788.0*rwdia -  &
     &        1.105e6*rwdia**2 + 1.412e8*rwdia**3 - 6.527e9*rwdia**4)
        
        IF ( infdo .ge. 1 ) THEN
         vtxbar(mgs,lr,2) = (0.09112 + 2714.0*rwdia - 4.872e5*rwdia**2 +  &
     &            4.495e7*rwdia**3 - 1.626e9*rwdia**4)*rhovt(mgs)
        ENDIF
        
        IF ( infdo .ge. 2 ) THEN ! Z-weighted fall speed
        vtxbar(mgs,lr,3)  = rhovt(mgs)*(  &
     &       0.0911229 +                  &
     &  9246.494*(rwdia) -               &
     &  3.2839926e6*(rwdia**2) +          &
     &  4.944093e8*(rwdia**3) -          &
     &  2.631718e10*(rwdia**4) )
        ENDIF
        
        ELSE ! 3-moment rain, gamma-volume

        vr = xv(mgs,lr)
        rnux = alpha(mgs,lr)
        
        IF ( infdo .ge. 1 .and. rssflg == 1) THEN ! number-weighted; DTD: added size-sorting flag
        vtxbar(mgs,lr,2) = rhovt(mgs)*                             &
     &     (((1. + rnux)/vr)**(-1.333333)*                         &
     &    (0.0911229*((1. + rnux)/vr)**1.333333*Gamma(1. + rnux) + &
     &      (5430.3131*(1. + rnux)*Gamma(4./3. + rnux))/           &
     &       vr - 1.0732802e6*((1. + rnux)/vr)**0.6666667*         &
     &       Gamma(1.666667 + rnux) +                              &
     &      8.584110982429507e7*((1. + rnux)/vr)**(1./3.)*         &
     &       Gamma(2. + rnux) -                                    &
     &      2.3303765697228556e9*Gamma(7./3. + rnux)))/            &
     &  Gamma(1. + rnux)
        ENDIF

!  mass-weighted
       vtxbar(mgs,lr,1)  = rhovt(mgs)*                                                 &
     &   (0.0911229*(1 + rnux)**1.3333333333333333*Gamma(2. + rnux) +                  &
     &    5430.313059683277*(1 + rnux)*vr**0.3333333333333333*                         &
     &     Gamma(2.333333333333333 + rnux) -                                           &
     &    1.0732802065650471e6*(1 + rnux)**0.6666666666666666*vr**0.6666666666666666*  &
     &     Gamma(2.6666666666666667 + rnux) +                                          &
     &    8.584110982429507e7*(1 + rnux)**0.3333333333333333*vr*Gamma(3 + rnux) -      &
     &    2.3303765697228556e9*vr**1.3333333333333333*                                 &
     &     Gamma(3.333333333333333 + rnux))/                                           &
     &  ((1 + rnux)**2.333333333333333*Gamma(1 + rnux)) 
     
        IF(infdo .ge. 1 .and. rssflg == 0) THEN ! No size-sorting, set N-weighted fall speed to mass-weighted
          vtxbar(mgs,lr,2) = vtxbar(mgs,lr,1)
        ENDIF     
      
        IF ( infdo .ge. 2 .and. rssflg == 1) THEN ! Z-weighted fall speed
        vtxbar(mgs,lr,3)  =   rhovt(mgs)*                                          &
     &  ((1. + rnux)*(0.0911229*(1 + rnux)**1.3333333333333333*Gamma(3. + rnux) +  &
     &      5430.313059683277*(1 + rnux)*vr**0.3333333333333333*                   &
     &       Gamma(3.3333333333333335 + rnux) -                                    &
     &      1.0732802065650471e6*(1 + rnux)**0.6666666666666666*                   &
     &       vr**0.6666666666666666*Gamma(3.6666666666666665 + rnux) +             &
     &      8.5841109824295e7*(1 + rnux)**0.3333333333333333*vr*Gamma(4. + rnux) - &
     &      2.3303765697228556e9*vr**1.3333333333333333*                           &
     &       Gamma(4.333333333333333 + rnux)))/                                    &
     &  ((1 + rnux)**3.3333333333333335*(2 + rnux)*Gamma(1 + rnux))
        
!         write(0,*) 'setvt: mgs,lzr,infdo = ',mgs,lzr,infdo
!         write(0,*) 'vt1,2,3 = ',vtxbar(mgs,lr,1),vtxbar(mgs,lr,2),vtxbar(mgs,lr,3)
        
        ELSEIF (infdo .ge. 2) THEN ! No size-sorting, set Z-weighted fall speed to mass-weighted
          vtxbar(mgs,lr,3) = vtxbar(mgs,lr,1)
        ENDIF
        
        
        ENDIF
       ENDIF ! imurain

!        IF ( rwrad*mwfac .gt. 6.0e-4  ) THEN
!          vtxbar(mgs,lr,1) = 20.1*Sqrt(100.*rwrad*mwfac)*rhovt(mgs)
!        ELSE
!          vtxbar(mgs,lr,1) = 80.0e2*rwrad*rhovt(mgs)*mwfac
!        ENDIF
!        IF ( rwrad .gt. 6.0e-4  ) THEN
!          vtxbar(mgs,lr,2) = 20.1*Sqrt(100.*rwrad)*rhovt(mgs)
!        ELSE
!          vtxbar(mgs,lr,2) = 80.0e2*rwrad*rhovt(mgs)
!        ENDIF
      ENDIF ! ipconc
      else  ! qr < qrmin
      vtxbar(mgs,lr,1) = 0.0
      vtxbar(mgs,lr,2) = 0.0
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set rain vt'
      
      ENDIF
!
! ################################################################
!
!  SNOW !Zrnic et al. (1993)
!
      IF ( ls .gt. 1 .and. ( ildo == 0 .or. ildo == ls ) ) THEN
      do mgs = 1,ngscnt
      if ( qx(mgs,ls) .gt. qxmin(ls) ) then
        IF ( ipconc .ge. 4 ) THEN
         if ( mixedphase .and. qsvtmod ) then
         else
          IF ( isnowfall == 1 ) THEN
           ! original (Zrnic et al. 1993)
           vtxbar(mgs,ls,1) = 5.72462*rhovt(mgs)*(xv(mgs,ls))**(1./12.)
          ELSEIF ( isnowfall == 2 ) THEN
          ! Ferrier:
            vtxbar(mgs,ls,1) = 11.9495*rhovt(mgs)*(xv(mgs,ls))**(0.14)
          ENDIF
          
          IF(sssflg == 1) THEN
            IF ( isnowfall == 1 ) THEN
              vtxbar(mgs,ls,2) = 4.04091*rhovt(mgs)*(xv(mgs,ls))**(1./12.)
            ELSEIF ( isnowfall == 2 ) THEN
            ! Ferrier:
              vtxbar(mgs,ls,2) = 7.02909*rhovt(mgs)*(xv(mgs,ls))**(0.14) ! bug fix 11/15/2015: was rewriting to mass fall speed vtxbar(mgs,ls,1)
            ENDIF
          ELSE
            vtxbar(mgs,ls,2) = vtxbar(mgs,ls,1)
          ENDIF
          vtxbar(mgs,ls,3) = vtxbar(mgs,ls,1)
         endif
        ELSE
         vtxbar(mgs,ls,1) = (cs*gf4ds/6.0)*(xdia(mgs,ls,1)**ds)*rhovt(mgs)
         vtxbar(mgs,ls,2) = vtxbar(mgs,ls,1)
        ENDIF
      else
      vtxbar(mgs,ls,1) = 0.0
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set snow vt'
      
      ENDIF ! ls .gt. 1
!
!
! ################################################################
!
!  GRAUPEL !Wisner et al. (1972)
!
      IF ( lh .gt. 1 .and. ( ildo == 0 .or. ildo == lh ) ) THEN
      
      do mgs = 1,ngscnt
      vtxbar(mgs,lh,1) = 0.0
      if ( qx(mgs,lh) .gt. qxmin(lh) ) then
       IF ( icdx .eq. 1 ) THEN
         cd = cdx(lh)
       ELSEIF ( icdx .eq. 2 ) THEN
!         cd = Max(0.6, Min(1.0, 0.6 + 0.4*(xdnmx(lh) - xdn(mgs,lh))/(xdnmx(lh)-xdnmn(lh)) ) )
!         cd = Max(0.6, Min(1.0, 0.6 + 0.4*(900.0 - xdn(mgs,lh))/(900. - 300.) ) )
         cd = Max(0.45, Min(1.0, 0.45 + 0.35*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )
!         cd = Max(0.55, Min(1.0, 0.55 + 0.25*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )
       ELSEIF ( icdx .eq. 3 ) THEN
!         cd = Max(0.45, Min(1.0, 0.45 + 0.55*(800.0 - Max( 300., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 300.) ) )
         cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( hdnmn, Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 170.0) ) )
       ELSEIF ( icdx .eq. 4 ) THEN
         cd = Max(cdhmin, Min(cdhmax, cdhmin + (cdhmax-cdhmin)* &
     &        (cdhdnmax - Max( cdhdnmin, Min( cdhdnmax, xdn(mgs,lh) ) ) )/(cdhdnmax - cdhdnmin) ) )
       ELSEIF ( icdx .eq. 5 ) THEN
         cd = cdx(lh)*(xdn(mgs,lh)/rho_qh)**(2./3.)
       ELSEIF ( icdx .eq. 6 ) THEN ! Milbrandt and Morrison (2013)
         indxr = Int( (xdn(mgs,lh)-50.)/100. ) + 1
         indxr = Min( ngdnmm, Max(1,indxr) )
         
         
         delrho = Max( 0.0, 0.01*(xdn(mgs,lh) - mmgraupvt(indxr,1)) )
         IF ( indxr < ngdnmm ) THEN
          
          axh(mgs) = mmgraupvt(indxr,2) + delrho*(mmgraupvt(indxr+1,2) - mmgraupvt(indxr,2) )
          bxh(mgs) = mmgraupvt(indxr,3) + delrho*(mmgraupvt(indxr+1,3) - mmgraupvt(indxr,3) )

          
         ELSE
          axh(mgs) = mmgraupvt(indxr,2)
          bxh(mgs) = mmgraupvt(indxr,3)
         ENDIF
         
         aax = axh(mgs)
         bbx = bxh(mgs)
         
       ENDIF
       
      IF ( alpha(mgs,lh) .eq. 0.0 .and. icdx > 0 .and. icdx /= 6 ) THEN
      vtxbar(mgs,lh,1) = (gf4p5/6.0)*  &
     &  Sqrt( (xdn(mgs,lh)*xdia(mgs,lh,1)*4.0*gr) /  &
     &    (3.0*cd*rho0(mgs)) )
      ELSE
        IF ( icdx /= 6 ) bbx = bx(lh)
        tmp = 4. + alpha(mgs,lh) + bbx
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 4. + alpha(mgs,lh)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
        
!        aax = Max( 1.0, Min(2.0, (xdn(mgs,lh)/400.) ) )
!        vtxbar(mgs,lh,1) =  rhovt(mgs)*aax*ax(lh)*(xdia(mgs,lh,1)**bx(lh)*x)/y
        
        IF ( icdx > 0 .and. icdx /= 6) THEN
          aax = Sqrt(4.0*xdn(mgs,lh)*gr/(3.0*cd*rho00))
          vtxbar(mgs,lh,1) =  rhovt(mgs)*aax* Sqrt(xdia(mgs,lh,1)) * x/y
          axh(mgs) = aax
          bxh(mgs) = bbx
        ELSEIF (icdx == 6 ) THEN
          vtxbar(mgs,lh,1) =  rhovt(mgs)*aax* xdia(mgs,lh,1)**bbx * x/y
        ELSE
          axh(mgs) = ax(lh)
          bxh(mgs) = bx(lh)
          vtxbar(mgs,lh,1) =  rhovt(mgs)*ax(lh)*(xdia(mgs,lh,1)**bx(lh)*x)/y          
        ENDIF

!     &    Gamma(4.0 + dnu(lh) + 0.6))/Gamma(4. + dnu(lh))
      ENDIF

      IF ( lwsm6 .and. ipconc == 0 ) THEN
!         vtxbar(mgs,lh,1) = (330.*gf4ds/6.0)*(xdia(mgs,ls,1)**ds)*rhovt(mgs)
         vtxbar(mgs,lh,1) = (330.*gf4br/6.0)*(xdia(mgs,lh,1)**br)*rhovt(mgs)
      ENDIF
      
      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt'
      
      ENDIF ! lh .gt. 1
!
!
! ################################################################
!
!  HAIL
!
      IF ( lhl .gt. 1 .and. ( ildo == 0 .or. ildo == lhl ) ) THEN
      
      do mgs = 1,ngscnt
      vtxbar(mgs,lhl,1) = 0.0
      if ( qx(mgs,lhl) .gt. qxmin(lhl) ) then

       IF ( icdxhl .eq. 1 ) THEN
         cd = cdx(lhl)
       ELSEIF ( icdxhl .eq. 3 ) THEN
!         cd = Max(0.45, Min(1.0, 0.45 + 0.55*(800.0 - Max( 300., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 300.) ) )
         cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( hldnmn, Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 170.0) ) )
       ELSEIF ( icdxhl .eq. 4 ) THEN
         cd = Max(cdhlmin, Min(cdhlmax, cdhlmin + (cdhlmax-cdhlmin)*  &
     &       (cdhldnmax - Max( cdhldnmin, Min( cdhldnmax, xdn(mgs,lhl) ) ) )/(cdhldnmax - cdhldnmin) ) )
       ELSEIF ( icdxhl .eq. 5 ) THEN
         cd = cdx(lh)*(xdn(mgs,lhl)/rho_qh)**(2./3.)
       ELSEIF ( icdxhl .eq. 6 ) THEN ! Milbrandt and Morrison (2013)
         indxr = Int( (xdn(mgs,lhl)-50.)/100. ) + 1
         indxr = Min( ngdnmm, Max(1,indxr) )
         
         
         delrho = Max( 0.0, 0.01*(xdn(mgs,lhl) - mmgraupvt(indxr,1)) )
         IF ( indxr < ngdnmm ) THEN
          
          axhl(mgs) = mmgraupvt(indxr,2) + delrho*(mmgraupvt(indxr+1,2) - mmgraupvt(indxr,2) )
          bxhl(mgs) = mmgraupvt(indxr,3) + delrho*(mmgraupvt(indxr+1,3) - mmgraupvt(indxr,3) )

          
         ELSE
          axhl(mgs) = mmgraupvt(indxr,2)
          bxhl(mgs) = mmgraupvt(indxr,3)
         ENDIF
         
         aax = axhl(mgs)
         bbx = bxhl(mgs)
         
       ELSE
!         cd = Max(0.6, Min(1.0, 0.6 + 0.4*(900.0 - xdn(mgs,lhl))/(900. - 300.) ) )
!        cd = Max(0.5, Min(0.8, 0.5 + 0.3*(xdnmx(lhl) - xdn(mgs,lhl))/(xdnmx(lhl)-xdnmn(lhl)) ) )
         cd = Max(0.45, Min(0.6, 0.45 + 0.15*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 500.) ) )
       ENDIF

      IF ( alpha(mgs,lhl) .eq. 0.0 .and. icdxhl > 0 .and. icdxhl /= 6) THEN
      vtxbar(mgs,lhl,1) = (gf4p5/6.0)* &
     &  Sqrt( (xdn(mgs,lhl)*xdia(mgs,lhl,1)*4.0*gr) / &
     &    (3.0*cd*rho0(mgs)) )
      ELSE
        IF ( icdxhl /= 6 ) bbx = bx(lhl)
        tmp = 4. + alpha(mgs,lhl) + bbx
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 4. + alpha(mgs,lhl)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        IF ( icdxhl > 0 .and. icdxhl /= 6) THEN
          aax = Sqrt(4.0*xdn(mgs,lhl)*gr/(3.0*cd*rho00))
          vtxbar(mgs,lhl,1) =  rhovt(mgs)*aax* Sqrt(xdia(mgs,lhl,1)) * x/y
          axhl(mgs) = aax
          bxhl(mgs) = bbx
        ELSEIF ( icdxhl == 6 ) THEN
          vtxbar(mgs,lhl,1) =  rhovt(mgs)*aax* (xdia(mgs,lhl,1))**bbx * x/y
        ELSE
          axhl(mgs) = ax(lhl)
          bxhl(mgs) = bx(lhl)
         vtxbar(mgs,lhl,1) =  rhovt(mgs)*(ax(lhl)*xdia(mgs,lhl,1)**bx(lhl)*x)/y
        ENDIF
        
!     &    Gamma(4.0 + dnu(lh) + 0.6))/Gamma(4. + dnu(lh))
      ENDIF


      end if
      end do
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt'
      
      ENDIF ! lhl .gt. 1


      IF ( infdo .ge. 1 ) THEN

!      DO il = lc,lhab
!      IF ( il .ne. lr ) THEN
        DO mgs = 1,ngscnt
          vtxbar(mgs,lc,2) = vtxbar(mgs,lc,1)
        IF ( li .gt. 1 ) THEN
!          vtxbar(mgs,li,2) = rhovt(mgs)*49420.*1.25447*xdia(mgs,li,1)**(1.415) ! n-wgt (Ferrier 94)
!          vtxbar(mgs,li,2) = vtxbar(mgs,li,1)

! test print stuff...
!          IF ( xdia(mgs,li,1) .gt. 200.e-6 ) THEN
!            tmp = (xv(mgs,li)*cwc0)**(1./3.)
!            x = rhovt(mgs)*49420.*40.0005/5.40662*tmp**(1.415)
!            y = rhovt(mgs)*49420.*1.25447*tmp**(1.415)
!            write(6,*) 'Ice fall: ',vtxbar(mgs,li,1),x,y,tmp,xdia(mgs,li,1)
!          ENDIF
        ENDIF
!          vtxbar(mgs,ls,2) = vtxbar(mgs,ls,1)
        ENDDO

        IF ( lg .gt. lr ) THEN

        DO il = lg,lhab
         IF ( ildo == 0 .or. ildo == il ) THEN

            DO mgs = 1,ngscnt
             IF ( qx(mgs,il) .gt. qxmin(il) ) THEN
              IF ( (il .eq. lh .and. hssflg == 1) .or. ( lhl .gt. 1 .and. il .eq. lhl .and. hlssflg == 1) ) THEN ! DTD: added flag for size-sorting
              
              ! DTD: allow for setting of number-weighted and z-weighted fall speeds to the mass-weighted value,
              ! effectively turning off size-sorting

              IF ( il .eq. lh ) THEN ! {
             
               IF ( icdx .eq. 1 ) THEN
                 cd = cdx(lh)
               ELSEIF ( icdx .eq. 2 ) THEN
!                 cd = Max(0.6, Min(1.0, 0.6 + 0.4*(xdnmx(lh) - xdn(mgs,lh))/(xdnmx(lh)-xdnmn(lh)) ) )
!                 cd = Max(0.6, Min(1.0, 0.6 + 0.4*(900.0 - xdn(mgs,lh))/(900. - 300.) ) )
                 cd = Max(0.45, Min(1.0, 0.45 + 0.35*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )
!                 cd = Max(0.55, Min(1.0, 0.55 + 0.25*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 500.) ) )
               ELSEIF ( icdx .eq. 3 ) THEN
!                 cd = Max(0.45, Min(1.0, 0.45 + 0.55*(800.0 - Max( 170.0, Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 170.0) ) )
                 cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( hdnmn, Min( 800.0, xdn(mgs,lh) ) ) )/(800. - 170.0) ) )
               ELSEIF ( icdx .eq. 4 ) THEN
                 cd = Max(cdhmin, Min(cdhmax, cdhmin + (cdhmax-cdhmin)* &
     &            (cdhdnmax - Max( cdhdnmin, Min( cdhdnmax, xdn(mgs,lh) ) ) )/(cdhdnmax - cdhdnmin) ) )
               ELSEIF ( icdx .eq. 5 ) THEN
                 cd = cdx(lh)*(xdn(mgs,lh)/rho_qh)**(2./3.)
               ELSEIF ( icdx .eq. 6 ) THEN ! Milbrandt and Morrison (2013)
                  aax = axh(mgs)
                  bbx = bxh(mgs)
               ENDIF
               
              ELSEIF ( lhl .gt. 1 .and. il .eq. lhl ) THEN
             
               IF ( icdxhl .eq. 1 ) THEN
                 cd = cdx(lhl)
               ELSEIF ( icdxhl .eq. 3 ) THEN
!               cd = Max(0.45, Min(1.0, 0.45 + 0.55*(800.0 - Max( 300., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 300.) ) )
                cd = Max(0.45, Min(1.2, 0.45 + 0.55*(800.0 - Max( hldnmn, Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 170.0) ) )
               ELSEIF ( icdxhl .eq. 4 ) THEN
                cd = Max(cdhlmin, Min(cdhlmax, cdhlmin + (cdhlmax-cdhlmin)*  &
     &               (cdhldnmax - Max( cdhldnmin, Min( cdhldnmax, xdn(mgs,lhl) ) ) )/(cdhldnmax - cdhldnmin) ) )
               ELSEIF ( icdxhl == 5 ) THEN
!                cd = Max(0.6, Min(1.0, 0.6 + 0.4*(900.0 - xdn(mgs,lhl))/(900. - 300.) ) )
!                cd = Max(0.5, Min(0.8, 0.5 + 0.3*(xdnmx(lhl) - xdn(mgs,lhl))/(xdnmx(lhl)-xdnmn(lhl)) ) )
                 cd = Max(0.45, Min(0.6, 0.45 + 0.15*(800.0 - Max( 500., Min( 800.0, xdn(mgs,lhl) ) ) )/(800. - 500.) ) )
               ELSEIF ( icdxhl .eq. 6 ) THEN ! Milbrandt and Morrison (2013)
                  aax = axhl(mgs)
                  bbx = bxhl(mgs)
               ENDIF
               
              ENDIF ! }

               IF ( alpha(mgs,il) .eq. 0. .and. infdo .lt. 2 .and.   &
               ( ( il==lh .and. icdx > 0 .and. icdx /= 6) .or. ( il==lhl .and. icdxhl > 0 .and. icdxhl /= 6 ) ) ) THEN ! {
                 vtxbar(mgs,il,2) =   &
     &              Sqrt( (xdn(mgs,il)*xdia(mgs,il,1)*pi*gr) / &
     &                (3.0*cd*rho0(mgs)) )

               ELSE
               IF ( il == lh  .and. icdx   /= 6 ) bbx = bx(il)
               IF ( il == lhl .and. icdxhl /= 6 ) bbx = bx(il)
               tmp = 1. + alpha(mgs,il) + bbx
               i = Int(dgami*(tmp))
               del = tmp - dgam*i
               x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
  
               tmp = 1. + alpha(mgs,il)
               i = Int(dgami*(tmp))
               del = tmp - dgam*i
               y = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

                 IF ( il .eq. lh  .or. il .eq. lhl) THEN ! {
                   IF ( ( il==lh .and. icdx > 0 ) ) THEN
                     IF ( icdx /= 6 ) THEN
                      aax = Sqrt(4.0*xdn(mgs,il)*gr/(3.0*cd*rho00))
                      vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**0.5 * x/y
                     ELSE !  (icdx == 6 ) THEN
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**bbx * x/y
                     ENDIF
!                   ELSE
!                     aax = ax(il)
!                     vtxbar(mgs,il,2) =  rhovt(mgs)*ax(il)*(xdia(mgs,il,1)**bx(il)*x)/y
!                   ENDIF

                   ELSEIF ( ( il==lhl .and. icdxhl > 0 ) ) THEN
                     IF ( icdxhl /= 6 ) THEN
                       aax = Sqrt(4.0*xdn(mgs,il)*gr/(3.0*cd*rho00))
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**0.5 * x/y
                     ELSE ! ( icdxhl == 6 )
                       vtxbar(mgs,il,2) =  rhovt(mgs)*aax* xdia(mgs,il,1)**bbx * x/y
                     ENDIF
                   ELSE
                     aax = ax(il)
                     vtxbar(mgs,il,2) =  rhovt(mgs)*ax(il)*(xdia(mgs,il,1)**bx(il)*x)/y
                   ENDIF

!                  vtxbar(mgs,il,2) =  &
!     &               rhovt(mgs)*(xdn(mgs,il)/400.)*(75.715*xdia(mgs,il,1)**0.6* &
!     &               x)/y
!                  vtxbar(mgs,il,2) =  &
!     &               rhovt(mgs)*(xdn(mgs,il)/400.)*(ax(il)*xdia(mgs,il,1)**bx(il)* &
!     &               x)/y
                  IF ( infdo .ge. 2 ) THEN ! Z-weighted
                   vtxbar(mgs,il,3) = rhovt(mgs)*                 &
     &                (aax*(1.0/xdia(mgs,il,1) )**(- bbx)*  &
     &                 Gamma(7.0 + alpha(mgs,il) + bbx))/Gamma(7. + alpha(mgs,il))
!     &                (aax*(1.0/xdia(mgs,il,1) )**(- bx(il))*  &
!     &                 Gamma(7.0 + alpha(mgs,il) + bx(il)))/Gamma(7. + alpha(mgs,il))
                  ENDIF

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt3'

                 ELSE ! hail
                  vtxbar(mgs,il,2) =  &
     &               rhovt(mgs)*(ax(il)*xdia(mgs,il,1)**bx(il)* &
     &               x)/y

                 IF ( infdo .ge. 2 ) THEN ! Z-weighted
                  vtxbar(mgs,il,3) = rhovt(mgs)*                 &
     &              (aax*(1.0/xdia(mgs,il,1) )**(- bbx)*  &
     &               Gamma(7.0 + alpha(mgs,il) + bbx))/Gamma(7. + alpha(mgs,il))
!     &              (ax(il)*(1.0/xdia(mgs,il,1) )**(- bx(il))*  &
!     &               Gamma(7.0 + alpha(mgs,il) + bx(il)))/Gamma(7. + alpha(mgs,il))
                  ENDIF

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set hail vt4'

                 ENDIF ! }
!     &             Gamma(1.0 + dnu(il) + 0.6)/Gamma(1. + dnu(il))
               ENDIF ! }

!              IF ( infdo .ge. 2 ) THEN ! Z-weighted
!               vtxbar(mgs,il,3) = rhovt(mgs)*                 &
!     &            (ax(il)*(1.0/xdia(mgs,il,1) )**(- bx(il))*  &
!     &             Gamma(7.0 + alpha(mgs,il) + bx(il)))/Gamma(7. + alpha(mgs,il))
!              ENDIF

!               IF ( lhl .gt. 1 .and. il .eq. lhl ) THEN
!                write(0,*) 'setvt: ',qx(mgs,il),xdia(mgs,il,1),xdia(mgs,il,3),dnu(il),ax(il),bx(il)
!               ENDIF
             ELSEIF ( (il .eq. lh .and. hssflg == 0) .or. ( lhl .gt. 1 .and. il .eq. lhl .and. hlssflg == 0) ) THEN ! no size-sorting for graupel or hail
              vtxbar(mgs,il,2) = vtxbar(mgs,il,1)
              vtxbar(mgs,il,3) = vtxbar(mgs,il,1)
             ELSE ! not lh or lhl
              vtxbar(mgs,il,2) = &
     &            Sqrt( (xdn(mgs,il)*xdia(mgs,il,1)*pi*gr) /  &
     &              (3.0*cdx(il)*rho0(mgs)) )
              vtxbar(mgs,il,3) = vtxbar(mgs,il,1)

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt5'


              ENDIF
             ELSE ! qx < qxmin
              vtxbar(mgs,il,2) = 0.0

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt6'

             ENDIF
           ENDDO ! mgs

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt7'

        ENDIF
        ENDDO ! il

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt8'

        ENDIF ! lg .gt. 1 
        
!      ENDIF
!      ENDDO

      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: Set graupel vt9'

!       DO mgs = 1,ngscnt
!        IF ( qx(mgs,lr) > qxmin(lr) ) THEN
!         write(0,*) 'setvt2: mgs,lzr,infdo = ',mgs,lzr,infdo
!         write(0,*) 'vt1,2,3 = ',vtxbar(mgs,lr,1),vtxbar(mgs,lr,2),vtxbar(mgs,lr,3)
!        ENDIF
!       ENDDO

      ENDIF ! infdo .ge. 1 
      
      if ( ndebug1 .gt. 0 ) write(0,*) 'SETVTZ: END OF ROUTINE'

!############ SETVTZ ############################

      RETURN
      END SUBROUTINE setvtz
!--------------------------------------------------------------------------

!
! ##############################################################################

!
!  subroutine to calculate fall speeds of hydrometeors
!

      subroutine ziegfall1d(nx,ny,nz,nor,norz,na,dtp,jgs,ixcol, &
     &  xvt, rhovtzx,                                           &
     &  an,dn,ipconc0,t0,t7,cwmasn,cwmasx,       &
     &  cwradn,                                   &
     &  qxmin,xdnmx,xdnmn,cdx,cno,xdn0,xvmn,xvmx,  &
     &  ngs,qx,qxw,cx,xv,vtxbar,xmas,xdn,xdia,vx,alpha,zx,igs,kgs, &
     &  rho0,temcg,temg,rhovt,cwnc,cinc,fadvisc,cwdia,cipmas,cnina,cimas, &
     &  cnostmp,                     &
     &  infdo,ildo,timesetvt)

! 12.16.2005: .F version use in transitional SWM model
!
! 10.10.2003: Added cimn and cimx to setting for cci and cip.
!
! TO DO LIST:
!
! need to set up values for:
!     :  cipdia,cidia,cwdia,cwmas,vtwbar,
!     :  rho0,temcg,cip,cci
!
! and need to put fallspeed values in cwvt etc.
!
      
      implicit none
      integer ng1
      parameter(ng1 = 1)
      
      integer, intent(in) :: ixcol ! which column to return
      integer, intent(in) :: ildo
      
      integer nx,ny,nz,nor,norz,ngt,jgs,na
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na)
      real dn(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real t0(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real t7(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)
      real dtp,dtz1
      
      real :: rhovtzx(nz,nx)
      
      integer ndebugzf
      parameter (ndebugzf = 0)

      integer ix,jy,kz,i,j,k,il
      integer infdo
!
!
      real xvt(nz+1,nx,3,lc:lhab) ! 1=mass-weighted, 2=number-weighted

      real qxmin(lc:lhab)
      real xdn0(lc:lhab)
      real xvmn(lc:lhab), xvmx(lc:lhab)
      double precision,optional :: timesetvt

      integer :: ngs
      integer :: ngscnt,mgs,ipconc0
!      parameter ( ngs=200 )
      
      real ::  qx(ngs,lv:lhab) 
      real ::  qxw(ngs,ls:lhab) 
      real ::  cx(ngs,lc:lhab) 
      real ::  xv(ngs,lc:lhab) 
      real ::  vtxbar(ngs,lc:lhab,3) 
      real ::  xmas(ngs,lc:lhab) 
      real ::  xdn(ngs,lc:lhab) 
      real ::  xdia(ngs,lc:lhab,3) 
      real ::  vx(ngs,li:lhab) 
      real ::  alpha(ngs,lc:lhab) 
      real ::  zx(ngs,lr:lhab) 

      real xdnmx(lc:lhab), xdnmn(lc:lhab)
      real axh(ngs),bxh(ngs),axhl(ngs),bxhl(ngs)

!
!   drag coefficients
!
      real cdx(lc:lhab)
!
! Fixed intercept values for single moment scheme
!
      real cno(lc:lhab)
      
      real cwccn0,cwmasn,cwmasx,cwradn
!      real cwc0

      integer nxmpb,nzmpb,nxz,numgs,inumgs
      integer kstag
      parameter (kstag=1)

      integer igs(ngs),kgs(ngs)
      
      real rho0(ngs),temcg(ngs)

      real temg(ngs)
      
      real rhovt(ngs)
      
      real cwnc(ngs),cinc(ngs)
      real fadvisc(ngs),cwdia(ngs),cipmas(ngs)
      
!      real cimasn,cimasx,
      real :: cnina(ngs),cimas(ngs)
      
      real :: cnostmp(ngs)

!      real pii
!
!
!  general constants for microphysics
!

! 
! Miscellaneous
!
      
      logical flag
      logical ldoliq
      
    
      real chw, qr, z, rd, alp, z1, g1, vr, nrx, tmp
      
      real vtmax
      real xvbarmax
      
      integer l1, l2
      
      double precision :: dpt1, dpt2


!-----------------------------------------------------------------------------
! MPI LOCAL VARIABLES 

      integer :: ixb, jyb, kzb
      integer :: ixe, jye, kze

      logical :: debug_mpi = .false.


      if (ndebugzf .gt. 0 ) write(0,*) "ZIEGFALL: ENTERED SUBROUTINE"

! #####################################################################
! BEGIN EXECUTABLE
! #####################################################################
!

!  constants
!

      ldoliq = .false.
      IF ( ls .gt. 1 ) THEN
      DO il = ls,lhab
        ldoliq = ldoliq .or. ( lliq(il) .gt. 1 )
      ENDDO
      ENDIF
      
!      poo = 1.0e+05
!      cp608 = 0.608
!      cp = 1004.0
!      cv = 717.0
!      dnz00 = 1.225
!      rho00 = 1.225
!      cs = 4.83607122
!      ds = 0.25
!  new values for  cs and ds
!      cs = 12.42
!      ds = 0.42
!      pi = 4.0*atan(1.0)
!      pii = piinv ! 1./pi
!      pid4 = pi/4.0 
!      qccrit = 2.0e-03
!      qscrit = 6.0e-04
!      cwc0 = pii
      
!
!
!  general constants for microphysics
!
      
!
!  ci constants in mks units
!
!      cimasn = 6.88e-13 
!      cimasx = 1.0e-8
!
!  Set terminal velocities...
!    also set drag coefficients
!
      jy = jgs
      nxmpb = ixcol
      nzmpb = 1
      nxz = 1*nz
!      ngs = nz
      numgs = 1

      IF ( ildo == 0 ) THEN
        l1 = lc
        l2 = lhab
      ELSE
        l1 = ildo
        l2 = ildo
      ENDIF


      do inumgs = 1,numgs
       ngscnt = 0


       do kz = nzmpb,nz
        do ix = ixcol,ixcol
        flag = .false.

        
        DO il = l1,l2
          flag =  flag .or. ( an(ix,jy,kz,il)  .gt. qxmin(il) ) 
        ENDDO

        if ( flag ) then
! load temp quantities

        ngscnt = ngscnt + 1
        igs(ngscnt) = ix
        kgs(ngscnt) = kz
        if ( ngscnt .eq. ngs ) goto 1100
        end if
!#ifndef MPI
        end do !!ix
!#endif
        nxmpb = 1
       end do !! kz

!      if ( jy .eq. (ny-jstag) ) iend = 1

 1100 continue

      if ( ngscnt .eq. 0 ) go to 9998
!
!  set temporaries for microphysics variables
!


!
!  Reconstruct various quantities 
!
      do mgs = 1,ngscnt

       rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
       rhovt(mgs) = rhovtzx(kgs(mgs),ixcol) !  Sqrt(rho00/rho0(mgs))
       temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
       temcg(mgs) = temg(mgs) - tfr

        
!
      end do
!
! only need fadvisc for 
      IF ( lc .gt. 1 .and. (ildo == 0 .or. ildo == lc ) ) then
        do mgs = 1,ngscnt
         fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* &
     &   (temg(mgs)/296.0)**(1.5)
        end do
      ENDIF

      IF ( ipconc .eq. 0 ) THEN
      do mgs = 1,ngscnt
      cnina(mgs) = t7(igs(mgs),jgs,kgs(mgs))
      end do
      ENDIF


      IF ( ildo > 0 ) THEN
        vtxbar(:,ildo,:) = 0.0
      ELSE
        vtxbar(:,:,:) = 0.0
      ENDIF
      
!      do mgs = 1,ngscnt
!        qx(mgs,lv) = max(an(igs(mgs),jy,kgs(mgs),lv), 0.0) 
!      ENDDO
      DO il = l1,l2
      do mgs = 1,ngscnt
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0) 
      ENDDO
      end do
      
      cnostmp(:) = cno(ls)
      IF ( ipconc < 1 .and. lwsm6 .and. (ildo == 0 .or. ildo == ls )) THEN
        DO mgs = 1,ngscnt
          tmp = Min( 0.0, temcg(mgs) )
          cnostmp(mgs) = Min( 2.e8, 2.e6*exp(0.12*tmp) )
        ENDDO
      ENDIF


!
!  set concentrations
!
      cx(:,:) = 0.0
      
      if ( ipconc .ge. 1 .and. li .gt. 1 .and. (ildo == 0 .or. ildo == li ) ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
       end do
      end if
      if ( ipconc .ge. 2 .and. lc .gt. 1 .and. (ildo == 0 .or. ildo == lc ) ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
!        cx(mgs,lc) = Min( ccwmx, cx(mgs,lc) )
       end do
      end if
      if ( ipconc .ge. 3 .and. lr .gt. 1 .and. (ildo == 0 .or. ildo == lr ) ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)
!        IF ( qx(mgs,lr) .le. qxmin(lr) ) THEN
!        ELSE
!          cx(mgs,lr) = Max( 0.0, cx(mgs,lr) )
!        ENDIF
       end do
      end if
      if ( ipconc .ge. 4  .and. ls .gt. 1 .and. (ildo == 0 .or. ildo == ls ) ) then
       do mgs = 1,ngscnt
        cx(mgs,ls) = Max(an(igs(mgs),jy,kgs(mgs),lns), 0.0)
!        IF ( qx(mgs,ls) .le. qxmin(ls) ) THEN
!        ELSE
!          cx(mgs,ls) = Max( 0.0, cx(mgs,ls) )
!        ENDIF
       end do
      end if

      if ( ipconc .ge. 5  .and. lh .gt. 1 .and. (ildo == 0 .or. ildo == lh ) ) then
       do mgs = 1,ngscnt

        cx(mgs,lh) = Max(an(igs(mgs),jy,kgs(mgs),lnh), 0.0)
!        IF ( qx(mgs,lh) .le. qxmin(lh) ) THEN
!        ELSE
!          cx(mgs,lh) = Max( 0.0, cx(mgs,lh) )
!        ENDIF

       end do
      ENDIF

      if ( ipconc .ge. 5  .and. lhl .gt. 1 .and. (ildo == 0 .or. ildo == lhl ) ) then
       do mgs = 1,ngscnt

        cx(mgs,lhl) = Max(an(igs(mgs),jy,kgs(mgs),lnhl), 0.0)
!        IF ( qx(mgs,lhl) .le. qxmin(lhl) ) THEN
!          cx(mgs,lhl) = 0.0
!        ELSEIF ( cx(mgs,lhl) .eq. 0.0 .and. qx(mgs,lhl) .lt. 3.0*qxmin(lhl) ) THEN
!          qx(mgs,lhl) = 0.0
!        ELSE
!          cx(mgs,lhl) = Max( 0.0, cx(mgs,lhl) )
!        ENDIF

       end do
      end if
       
      do mgs = 1,ngscnt
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)
!        IF ( ls .gt. 1 .and. lvs .eq. 0 ) xdn(mgs,ls) = xdn0(ls)
!        IF ( lh .gt. 1 .and. lvh .eq. 0 ) xdn(mgs,lh) = xdn0(lh)
        IF ( li .gt. 1 )  xdn(mgs,li) = xdn0(li)
        IF ( ls .gt. 1 )  xdn(mgs,ls) = xdn0(ls)
        IF ( lh .gt. 1 )  xdn(mgs,lh) = xdn0(lh)
        IF ( lhl .gt. 1 ) xdn(mgs,lhl) = xdn0(lhl)
      end do

!
! Set mean particle volume
!
      IF ( ldovol .and. (ildo == 0 .or. ildo >= li ) ) THEN
      
      vx(:,:) = 0.0
      
       DO il = l1,l2
        
        IF ( lvol(il) .ge. 1 ) THEN
        
          DO mgs = 1,ngscnt
            vx(mgs,il) = Max(an(igs(mgs),jy,kgs(mgs),lvol(il)), 0.0)
            IF ( vx(mgs,il) .gt. rho0(mgs)*qxmin(il)*1.e-3 .and. qx(mgs,il) .gt. qxmin(il) ) THEN
              xdn(mgs,il) = Min( xdnmx(il), Max( xdnmn(il), rho0(mgs)*qx(mgs,il)/vx(mgs,il) ) )
            ENDIF
          ENDDO
          
        ENDIF
      
       ENDDO
      
      ENDIF

      DO il = lg,lhab
      DO mgs = 1,ngscnt
        alpha(mgs,il) = dnu(il)
      ENDDO
      ENDDO
      
      IF ( imurain == 1 ) THEN
        alpha(:,lr) = alphar
      ELSEIF ( imurain == 3 ) THEN
        alpha(:,lr) = xnu(lr)
      ENDIF
       






!
!  Set density
!
      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: call setvtz'
!
      
      call setvtz(ngscnt,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp,   &
     &                 xmas,vtxbar,xdn,xvmn,xvmx,xv,cdx,        &
     &                 ipconc,ndebugzf,ngs,nz,kgs,fadvisc, &
     &                 cwmasn,cwmasx,cwradn,cnina,cimn,cimx,    &
     &                 itype1,itype2,temcg,infdo,alpha,ildo,axh,bxh,axhl,bxhl)



!
! put fall speeds into the x-z arrays
!
      DO il = l1,l2
      do mgs = 1,ngscnt
       
       vtmax = 150.0

       
       IF ( vtxbar(mgs,il,2) .gt. vtxbar(mgs,il,1)  .or. &
     &      ( vtxbar(mgs,il,1) .gt. vtxbar(mgs,il,3) .and. vtxbar(mgs,il,3) > 0.0) ) THEN
          
          
          
          vtxbar(mgs,il,1) = Max( vtxbar(mgs,il,1), vtxbar(mgs,il,2) )
          vtxbar(mgs,il,3) = Max( vtxbar(mgs,il,3), vtxbar(mgs,il,1) )
          
       ENDIF

       
       IF ( vtxbar(mgs,il,1) .gt. vtmax .or. vtxbar(mgs,il,2) .gt. vtmax .or. &
     &      vtxbar(mgs,il,3) .gt. vtmax ) THEN
       
        vtxbar(mgs,il,1) = Min(vtmax,vtxbar(mgs,il,1) )
        vtxbar(mgs,il,2) = Min(vtmax,vtxbar(mgs,il,2) )
        vtxbar(mgs,il,3) = Min(vtmax,vtxbar(mgs,il,3) )
        
!        call commasmpi_abort()
       ENDIF


       xvt(kgs(mgs),igs(mgs),1,il) = vtxbar(mgs,il,1)
       xvt(kgs(mgs),igs(mgs),2,il) = vtxbar(mgs,il,2)
       IF ( infdo .ge. 2 ) THEN
       xvt(kgs(mgs),igs(mgs),3,il) = vtxbar(mgs,il,3)
       ELSE
       xvt(kgs(mgs),igs(mgs),3,il) = 0.0
       ENDIF

!       xvt(kgs(mgs),igs(mgs),2,il) = xvt(kgs(mgs),igs(mgs),1,il)

      enddo
      ENDDO


      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: COPIED FALL SPEEDS'



 9998 continue

      if (ndebugzf .gt. 0 ) write(0,*)  'ZIEGFALL: DONE WITH LOOP'

      if ( kz .gt. nz-1 ) then
        go to 1200
      else
        nzmpb = kz 
      end if

      if (ndebugzf .gt. 0 ) write(0,*) 'ZIEGFALL: SET NZMPB'

      end do !! inumgs

      if (ndebugzf .gt. 0 ) write(0,*) 'ZIEGFALL: SET NXMPB'

 1200 continue


!       ENDDO ! ix
!      ENDDO ! kz


      if (ndebugzf .gt. 0 ) write(0,*) "ZIEGFALL: EXITING SUBROUTINE"


      RETURN
      END subroutine ziegfall1d

! #####################################################################
! #####################################################################


! #####################################################################
! #####################################################################

! ##############################################################################
      subroutine radardd02(nx,ny,nz,nor,na,an,temk,         &
     &    dbz,db,nzdbz,cnoh0t,hwdn1t,ipconc, iunit)
!
! 11.13.2005: Changed values of indices for reordering of lip
!
! 07.13.2005: Fixed an error where cnoh was being used for graupel and frozen drops
!
! 01.24.2005: add ice crystal reflectivity using parameterization of
!             Heymsfield (JAS, 1977).  Could also try Ferrier for this, too.
!
!  09.28.2002 Test alterations for dry ice following Ferrier (1994)
!      for equivalent melted diameter reflectivity.
!      Converted to Fortran by ERM.
!      
!Date: Tue, 21 Nov 2000 10:13:36 -0600 (CST)
!From: Matthew Gilmore <gilmore@hesston.met.tamu.edu>
!
!PRO RF_SPEC ; Computes Radar Reflectivity
!COMMON MAINB, data, x1d, y1d, z1d, iconst, rconst, labels, nx, ny, nz, dshft
!
!;MODIFICATION HISTORY
!; 5/99  -Svelta Veleva introduces variable dielf (const_ki_x) as a (weak)
!;   function of density.  This leads to slight modification of dielf such
!;   that the snow reflectivity is slightly increased - not a big effect.
!;   This is believed to be more accurate than assuming the dielectric
!;   constant for snow is the same as for hail in previous versions.
!
!;On 6/13/99 I added the VIL computation (k=0 in vil array)
!;On 6/15/99 I removed the number concentration dependencies as a function
!;           of temperature (only use for ferrier!)
!;On 6/15/99 I added the Composite reflectivity (k=1 in VIL array)
!;On 6/15/99 I added the Severe Hail Index computation (k=2 in vil array)
!;
!; 6/99 - Veleva and Seo argue that since graupel is more similar to
!;   snow (in number conc and size density) than it is to hail, we
!;   should not weight wetted graupel with the .95 exponent correction
!;   factor as in the case of hail.  An if-statement checks the size
!;   density for wet hail/graupel and treats them appropriately.
!;
!; 6/22/99 - Added function to compute height of max rf and 40 dbz echo top
!;           Also added vilqr which is the model vertical integrated liquid only
!;           using qr.  Will need to check...doesn't seem consistent with vilZ
!;


      implicit none
      
      character(LEN=15), parameter :: microp = 'ZVD'
      integer nx,ny,nz,nor,na,ngt
      integer nzdbz    !  how many levels actually to process
      
      integer ng1,n10
      integer iunit
      integer, parameter :: printyn = 0

      parameter( ng1 = 1 )
      
      real cnoh0t,hwdn1t
      integer ipconc
      real vr


      integer imapz,mzdist
      
      integer vzflag
      integer, parameter :: norz = 3
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,na)
      real db(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)  ! air density
!      real gt(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor,ngt)
      real temk(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)  ! air temperature (kelvin)
      real dbz(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-nor+ng1:nz+nor)   ! reflectivity
      real gz(-nor+1:nz+nor) ! ,z1d(-nor+1:nz+nor,4)
      
!      real g,rgas,eta,inveta
      real cr1, cr2 ,  hwdnsq,swdnsq
      real rwdnsq, dhmin, qrmin, qsmin, qhmin, qhlmin, tfr, tfrh, zrc
      real reflectmin,  kw_sq
      real const_ki_sn, const_ki_h, ki_sq_sn
      real ki_sq_h, dielf_sn, dielf_h
      real pi
      logical ltest

!  Other data arrays
       real gtmp     (nx,nz)
       real dtmp     (nx,nz)
       real tmp

       real*8 dtmps, dtmpr, dtmph, dtmphl, g1, zx, ze, x

       integer i,j,k,ix,jy,kz,ihcnt

        real*8 xcnoh, xcnos, dadh, dads, zhdryc, zsdryc, zhwetc,zswetc
        real*8 dadr
        real dbzmax,dbzmin
        parameter ( dbzmin = 0 )

      real cnow,cnoi,cnoip,cnoir,cnor,cnos
      real cnogl,cnogm,cnogh,cnof,cnoh,cnohl

      real swdn, rwdn ,hwdn,gldn,gmdn,ghdn,fwdn,hldn
      real swdn0

      real rwdnmx,cwdnmx,cidnmx,xidnmx,swdnmx,gldnmx,gmdnmx
      real ghdnmx,fwdnmx,hwdnmx,hldnmx
      real rwdnmn,cwdnmn,cidnmn,xidnmn,swdnmn,gldnmn,gmdnmn
      real ghdnmn,fwdnmn,hwdnmn,hldnmn
 
      real gldnsq,gmdnsq,ghdnsq,fwdnsq,hldnsq

      real dadgl,dadgm,dadgh,dadhl,dadf
      real zgldryc,zglwetc,zgmdryc, zgmwetc,zghdryc,zghwetc
      real zhldryc,zhlwetc,zfdryc,zfwetc

      real dielf_gl,dielf_gm,dielf_gh,dielf_hl,dielf_fw
      
      integer imx,jmx,kmx
      
      real swdia,gldia,gmdia,ghdia,fwdia,hwdia,hldia
      
      real csw,cgl,cgm,cgh,cfw,chw,chl
      real xvs,xvgl,xvgm,xvgh,xvf,xvh,xvhl
      
      real cwc0
      integer izieg
      integer ice10
      real rhos
      parameter ( rhos = 0.1 )
      
      real qxw,qxw1    ! temp value for liquid water on ice mixing ratio
      real :: dnsnow
      real qh

      real, parameter :: cwmasn = 5.23e-13   ! minimum mass, defined by radius of 5.0e-6
      real, parameter :: cwmasx = 5.25e-10   ! maximum mass, defined by radius of 50.0e-6
      real, parameter :: cwradn = 5.0e-6     ! minimum radius

      real cwnccn(nz)
      
      real :: vzsnow, vzrain, vzgraupel, vzhail
      real :: ksq
      real :: dtp


! #########################################################################      

      vzflag = 0
      
      izieg = 0
      ice10 = 0
!      g=9.806                 ! g: gravity constant
!      rgas=287.04             ! rgas: gas constant for dry air
!      rcp=rgas/cp             ! rcp: gamma constant
!      eta=0.622
!      inveta = 1./eta
!      rcpinv = 1./rcp
!      cpr=cp/rgas
!      cvr=cv/rgas
      pi = 4.0*ATan(1.)
      cwc0 = piinv ! 1./pi ! 6.0/pi
      
      cnoh = cnoh0t
      hwdn = hwdn1t

      rwdn = 1000.0
      swdn = 100.0

      qrmin = 1.0e-05
      qsmin = 1.0e-06
      qhmin = 1.0e-05

!
!  default slope intercepts
!
      cnow  = 1.0e+08
      cnoi  = 1.0e+08
      cnoip = 1.0e+08 
      cnoir = 1.0e+08 
      cnor  = 8.0e+06 
      cnos  = 8.0e+06 
      cnogl = 4.0e+05 
      cnogm = 4.0e+05 
      cnogh = 4.0e+05 
      cnof  = 4.0e+05
      cnohl = 1.0e+03


      imx = 1
      jmx = 1
      kmx = 1
      i = 1


       IF ( microp(1:4) .eq. 'ZIEG' ) THEN !  na .ge. 14 .and. ipconc .ge. 3 ) THEN 

!        write(0,*)  'Set reflectivity for ZIEG'
         izieg = 1

         hwdn = hwdn1t ! 500.


         cnor  = cno(lr)
         cnos  = cno(ls)
         cnoh  = cno(lh)
         qrmin = qxmin(lr)
         qsmin = qxmin(ls)
         qhmin = qxmin(lh)
         IF ( lhl .gt. 1 ) THEN
            cnohl  = cno(lhl)
            qhlmin = qxmin(lhl)
         ENDIF

       ELSEIF ( microp(1:3) .eq. 'ZVD' ) THEN !  na .ge. 14 .and. ipconc .ge. 3 ) THEN 

         izieg = 1
         
         swdn0 = swdn

         cnor  = cno(lr)
         cnos  = cno(ls)
         cnoh  = cno(lh)
         
         qrmin = qxmin(lr)
         qsmin = qxmin(ls)
         qhmin = qxmin(lh)
         IF ( lhl .gt. 1 ) THEN
            cnohl  = cno(lhl)
            qhlmin = qxmin(lhl)
         ENDIF
!         write(*,*) 'radardbz: ',db(1,1,1),temk(1,1,1),an(1,1,1,lr),an(1,1,1,ls),an(1,1,1,lh)


        ENDIF


!      cdx(lr) = 0.60
!      
!      IF ( lh > 1 ) THEN
!      cdx(lh) = 0.8 ! 1.0 ! 0.45
!      cdx(ls) = 2.00
!      ENDIF
!
!      IF ( lhl .gt. 1 ) cdx(lhl) = 0.45
!
!      xvmn(lc) = xvcmn
!      xvmn(lr) = xvrmn
!
!      xvmx(lc) = xvcmx
!      xvmx(lr) = xvrmx
!
!      IF ( lh > 1 ) THEN
!      xvmn(ls) = xvsmn
!      xvmn(lh) = xvhmn
!      xvmx(ls) = xvsmx
!      xvmx(lh) = xvhmx
!      ENDIF
!
!      IF ( lhl .gt. 1 ) THEN
!      xvmn(lhl) = xvhlmn
!      xvmx(lhl) = xvhlmx
!      ENDIF
!
!      xdnmx(lr) = 1000.0
!      xdnmx(lc) = 1000.0
!      IF ( lh > 1 ) THEN
!      xdnmx(li) =  917.0
!      xdnmx(ls) =  300.0
!      xdnmx(lh) =  900.0
!      ENDIF
!      IF ( lhl .gt. 1 ) xdnmx(lhl) = 900.0
!!
!      xdnmn(:) = 900.0
!      
!      xdnmn(lr) = 1000.0
!      xdnmn(lc) = 1000.0
!      IF ( lh > 1 ) THEN
!      xdnmn(li) =  100.0
!      xdnmn(ls) =  100.0
!      xdnmn(lh) =  hdnmn
!      ENDIF
!      IF ( lhl .gt. 1 ) xdnmn(lhl) = 500.0
!
!      xdn0(:) = 900.0
!      
!      xdn0(lc) = 1000.0
!      xdn0(lr) = 1000.0
!      IF ( lh > 1 ) THEN
!      xdn0(li) = 900.0
!      xdn0(ls) = 100.0 ! 100.0
!      xdn0(lh) = hwdn1t ! (0.5)*(xdnmn(lh)+xdnmx(lh))
!      ENDIF
!      IF ( lhl .gt. 1 ) xdn0(lhl) = 800.0

!
!  slope intercepts
!
!      cnow  = 1.0e+08
!      cnoi  = 1.0e+08
!      cnoip = 1.0e+08 
!      cnoir = 1.0e+08 
!      cnor  = 8.0e+06 
!      cnos  = 8.0e+06 
!      cnogl = 4.0e+05 
!      cnogm = 4.0e+05 
!      cnogh = 4.0e+05 
!      cnof  = 4.0e+05
!c      cnoh  = 4.0e+04
!      cnohl = 1.0e+03
!
!
!  density maximums and minimums
!
      rwdnmx = 1000.0
      cwdnmx = 1000.0
      cidnmx =  917.0
      xidnmx =  917.0
      swdnmx =  200.0
      gldnmx =  400.0
      gmdnmx =  600.0
      ghdnmx =  800.0
      fwdnmx =  900.0
      hwdnmx =  900.0
      hldnmx =  900.0
!
      rwdnmn = 1000.0
      cwdnmn = 1000.0
      xidnmn =  001.0
      cidnmn =  001.0
      swdnmn =  001.0
      gldnmn =  200.0
      gmdnmn =  400.0
      ghdnmn =  600.0
      fwdnmn =  700.0
      hwdnmn =  700.0
      hldnmn =  900.0

      
      gldn = (0.5)*(gldnmn+gldnmx)  ! 300.
      gmdn = (0.5)*(gmdnmn+gmdnmx)  ! 500.
      ghdn = (0.5)*(ghdnmn+ghdnmx)  ! 700.
      fwdn = (0.5)*(fwdnmn+fwdnmx)  ! 800.
      hldn = (0.5)*(hldnmn+hldnmx)  ! 900.


      cr1  = 7.2e+20
      cr2  = 7.295e+19
      hwdnsq = hwdn**2
      swdnsq = swdn**2
      rwdnsq = rwdn**2

      gldnsq = gldn**2
      gmdnsq = gmdn**2
      ghdnsq = ghdn**2
      fwdnsq = fwdn**2
      hldnsq = hldn**2
      
      dhmin = 0.005
      tfr   = 273.16
      tfrh  = tfr - 8.0
      zrc   = cr1*cnor
      reflectmin = 0.0
      kw_sq = 0.93
      dbzmax = dbzmin
      
      ihcnt=0

            
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Dielectric Factor  - Formulas implemented by Svetla Veleva
!                       following Battan, "Radar Meteorology" - p. 40
!  The result of these calculations is that the dielf numerator (ki_sq) without
!  the density ratio is  .2116 for hail if using 917 density and .25 for
!  snow if using 220 density.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      const_ki_sn = 0.5 - (0.5-0.46)/(917.-220.)*(swdn-220.)
      const_ki_h  = 0.5 - (0.5-0.46)/(917.-220.)*(hwdn-220.)
      ki_sq_sn = (swdnsq/rwdnsq) * const_ki_sn**2
      ki_sq_h  = (hwdnsq/rwdnsq) * const_ki_h**2
      dielf_sn = ki_sq_sn / kw_sq
      dielf_h  = ki_sq_h  / kw_sq
            
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Use the next line if you want to hardwire dielf for dry hail for both dry
!  snow and dry hail.
!  This would be equivalent to what Straka had originally. (i.e, .21/.93)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      dielf_sn = (swdnsq/rwdnsq)*.21/ kw_sq
      dielf_h  = (hwdnsq/rwdnsq)*.21/ kw_sq

      dielf_gl  = (gldnsq/rwdnsq)*.21/ kw_sq
      dielf_gm  = (gmdnsq/rwdnsq)*.21/ kw_sq
      dielf_gh  = (ghdnsq/rwdnsq)*.21/ kw_sq
      dielf_hl  = (hldnsq/rwdnsq)*.21/ kw_sq
      dielf_fw  = (fwdnsq/rwdnsq)*.21/ kw_sq

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Notes on dielectric factors  - from Eun-Kyoung Seo
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! constants for both snow and hail would be (x=s,h).....
!       xwdnsq/rwdnsq *0.21/kw_sq   ! Straka/Smith - the original
!       xwdnsq/rwdnsq *0.224        ! Ferrier - for particle sizes in equiv. drop diam
!       xwdnsq/rwdnsq *0.176/kw_sq  ! =0.189 in Smith - for particle sizes in equiv 
!                       ice spheres
!       xwdnsq/rwdnsq *0.208/kw_sq  ! Smith '84 - for particle sizes in equiv melted drop diameter
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! VIL algorithm constants
!      Ztop = 10.**(56./10)           !56 dbz is the max rf used by WATADS in cell vil


! Hail detection algorithm constants
!      ZL = 40.
!      ZU = 50.
!      Ho = 3400.  !WATADS Defaults
!      Hm20 = 6200.      !WATADS Defaults

!      DO kz = 1,Min(nzdbz,nz-1)

      DO jy=1,1

        DO kz = 1,nz
         
          DO ix=1,nx
            dbz(ix,jy,kz) = 0.0
                      
          vzsnow = 0.0
          vzrain = 0.0
          vzgraupel = 0.0
          vzhail = 0.0
          
          dtmph = 0.0
          dtmps = 0.0
          dtmphl = 0.0
          dtmpr = 0.0
           dadr = (db(ix,jy,kz)/(pi*rwdn*cnor))**(0.25)
!-----------------------------------------------------------------------
! Compute Rain Radar Reflectivity
!-----------------------------------------------------------------------
           
           dtmp(ix,kz) = 0.0
           gtmp(ix,kz) = 0.0
           IF ( an(ix,jy,kz,lr) .ge. qrmin ) THEN
             IF ( ipconc .le. 2 ) THEN
               gtmp(ix,kz) = dadr*an(ix,jy,kz,lr)**(0.25)
               dtmp(ix,kz) = zrc*gtmp(ix,kz)**7
             ELSEIF ( an(ix,jy,kz,lnr) .gt. 1.e-3 ) THEN
               IF ( imurain == 3 ) THEN
                 vr = db(ix,jy,kz)*an(ix,jy,kz,lr)/(1000.*an(ix,jy,kz,lnr))
                 dtmp(ix,kz) = 3.6e18*(rnu+2.)*an(ix,jy,kz,lnr)*vr**2/(rnu+1.)
               ELSE ! imurain == 1
                g1 = (6.0 + alphar)*(5.0 + alphar)*(4.0 + alphar)/((3.0 + alphar)*(2.0 + alphar)*(1.0 + alphar))
                zx = g1*(db(ix,jy,kz)*an(ix,jy,kz,lr))**2/an(ix,jy,kz,lnr)
                ze =1.e18*zx*(6./(pi*1000.))**2 ! note: using 1000. here for water density
                dtmp(ix,kz) = ze
               ENDIF
             ENDIF
             dtmpr = dtmp(ix,kz)
           ENDIF
           
!-----------------------------------------------------------------------
! Compute snow and graupel reflectivity
!
! Lou modified to look at parcel temperature rather than base state
!-----------------------------------------------------------------------

          IF( lhab .gt. lr ) THEN

!    qs2d   = reform(data[*,*,k,10],[nx*ny])
!    qh2d   = reform(data[*,*,k,11],[nx*ny])

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Only use the following lines if running Straka's GEMS microphysics
!  (Sam 1-d version modified by L Wicker does not use this)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    ;xcnoh    = cnoh*exp(-0.025*(temp-tfr))
!    ;xcnos    = cnos*exp(-0.038*(temp-tfr))
!    ;good = where(temp GT tfr, n_elements)
!    ;IF n_elements NE 0 THEN xcnoh(good) = cnoh*exp(-0.075*(temp(good)-tfr))
!    ;IF n_elements NE 0 THEN xcnos(good) = cnos*exp(-0.088*(temp(good)-tfr))

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Only use the following lines if running Ferrier micro with No=No(T)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!    ;  NOSE = -.15
!    ;  NOGE =  .0
!    ;  xcnoh = cnoh*(1.>exp(NOGE*(temp-tfr)) )
!    ;  xcnos = cnos*(1.>exp(NOSE*(temp-tfr)) )

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! Use the following lines if Nos and Noh are constant
!  (As in Svetla's version of Ferrier, GCE Tao, and SAM 1-d)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        xcnoh    = cnoh
        xcnos    = cnos

!
! Temporary fix for predicted number concentration -- need a 
! more appropriate reflectivity equation!
!
!        IF ( an(ix,jy,kz,lns) .lt. 0.1 ) THEN
!         swdia = (xvrmn*cwc0)**(1./3.)
!         xcnos = an(ix,jy,kz,ls)*db(ix,jy,kz)/(xvrmn*swdn*swdia)
!        ELSE
!      ! changed back to diameter of mean volume!!!
!         swdia =
!     >  (an(ix,jy,kz,ls)*db(ix,jy,kz)
!     > /(pi*swdn*an(ix,jy,kz,lns)))**(1./3.)
!
!        xcnos = an(ix,jy,kz,lns)/swdia
!        ENDIF

        IF ( ls .gt. 1 ) THEN ! {
        
        IF ( lvs .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvs) .gt. 0.0 ) THEN
            swdn = db(ix,jy,kz)*an(ix,jy,kz,ls)/an(ix,jy,kz,lvs)
            swdn = Min( 300., Max( 100., swdn ) )
          ELSE 
            swdn = swdn0
          ENDIF
        
        ENDIF 
        
        IF ( ipconc .ge. 5 ) THEN ! {

        xvs = db(ix,jy,kz)*an(ix,jy,kz,ls)/  &
     &      (swdn*Max(1.0e-3,an(ix,jy,kz,lns)))
        IF ( xvs .lt. xvsmn .or. xvs .gt. xvsmx ) THEN
          xvs = Min( xvsmx, Max( xvsmn,xvs ) )
          csw = db(ix,jy,kz)*an(ix,jy,kz,ls)/(xvs*swdn)
        ENDIF

         swdia = (xvs*cwc0)**(1./3.)
         xcnos = an(ix,jy,kz,ls)*db(ix,jy,kz)/(xvs*swdn*swdia)
         
         ENDIF ! }
         ENDIF  ! }

!        IF ( an(ix,jy,kz,lnh) .lt. 0.1 ) THEN
!         hwdia = (xvrmn*cwc0)**(1./3.)
!         xcnoh = an(ix,jy,kz,lh)*db(ix,jy,kz)/(xvrmn*hwdn*hwdia)
!        ELSE
!      ! changed back to diameter of mean volume!!!
!         hwdia =
!     >  (an(ix,jy,kz,lh)*db(ix,jy,kz)
!     > /(pi*hwdn*an(ix,jy,kz,lnh)))**(1./3.)
!        
!         xcnoh = an(ix,jy,kz,lnh)/hwdia
!        ENDIF

        IF ( lh .gt. 1 ) THEN ! {

        IF ( lvh .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
            hwdn = db(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
            hwdn = Min( 900., Max( hdnmn, hwdn ) )
          ELSE 
            hwdn = 500. ! hwdn1t
          ENDIF
        ELSE
          hwdn = hwdn1t
        ENDIF 
        
        IF ( ipconc .ge. 5 ) THEN ! {

        xvh = db(ix,jy,kz)*an(ix,jy,kz,lh)/       &
     &      (hwdn*Max(1.0e-3,an(ix,jy,kz,lnh)))
        IF ( xvh .lt. xvhmn .or. xvh .gt. xvhmx ) THEN
          xvh = Min( xvhmx, Max( xvhmn,xvh ) )
          chw = db(ix,jy,kz)*an(ix,jy,kz,lh)/(xvh*hwdn)
        ENDIF

         hwdia = (xvh*cwc0)**(1./3.)
         xcnoh = an(ix,jy,kz,lh)*db(ix,jy,kz)/(xvh*hwdn*hwdia)
         
        ENDIF ! } ipconc .ge. 5
 
        ENDIF ! }

        dadh = 0.0
        dadhl = 0.0
        dads = 0.0
        IF ( xcnoh .gt. 0.0 ) THEN 
          dadh = ( db(ix,jy,kz) /(pi*hwdn*xcnoh) )**(.25)
          zhdryc = 0.224*cr2*(db(ix,jy,kz)/rwdn)**2/xcnoh ! dielf_h*cr1*xcnoh          ! SV - equiv formula as before but
                                        ! ratio of densities included in
                                        ! dielf_h rather than here following
                                        ! Battan.
        ELSE
          dadh = 0.0
          zhdryc = 0.0
        ENDIF
        
        IF ( xcnos .gt. 0.0 ) THEN
          dads = ( db(ix,jy,kz) /(pi*swdn*xcnos) )**(.25)
          zsdryc = 0.224*cr2*(db(ix,jy,kz)/rwdn)**2/xcnos ! dielf_sn*cr1*xcnos         ! SV - similar change as above
        ELSE
          dads = 0.0
          zsdryc = 0.0
        ENDIF
        zhwetc = zhdryc ! cr1*xcnoh      !Hail/graupel version with .95 power bug removed
        zswetc = zsdryc ! cr1*xcnos
!           
! snow contribution
!
          IF ( ls .gt. 1 ) THEN
          
          gtmp(ix,kz) = 0.0 
          qxw = 0.0 
          qxw1 = 0.0
          dtmps = 0.0
           IF ( an(ix,jy,kz,ls) .ge. qsmin ) THEN !{
            IF ( ipconc .ge. 4 ) THEN  ! (Ferrier 94) !{

             if (lsw .gt. 1) THEN 
               qxw = an(ix,jy,kz,lsw)
               qxw1 = 0.0
             ELSEIF ( iusewetsnow == 1 .and. temk(ix,jy,kz) .gt. tfr+1. .and. an(ix,jy,kz,ls) > an(ix,jy,kz,lr) &
     &              .and. an(ix,jy,kz,lr) > qsmin) THEN
               qxw = Min(0.5*an(ix,jy,kz,ls), an(ix,jy,kz,lr))
               qxw1 = qxw
             ENDIF

             vr = xvs ! db(ix,jy,kz)*an(ix,jy,kz,lr)/(1000.*an(ix,jy,kz,lnr))
!             gtmp(ix,kz) = 3.6e18*(0.243*rhos**2/0.93)*(snu+2.)*an(ix,jy,kz,lns)*vr**2/(snu+1.)
             
             ksq = 0.189 ! Smith (1984, JAMC) for equiv. ice sphere
             IF ( an(ix,jy,kz,lns) .gt. 1.e-7 ) THEN
               IF ( qxw > qsmin ) THEN ! old version
!                gtmp(ix,kz) = 3.6e18*(snu+2.)*( 0.224*an(ix,jy,kz,ls) + 0.776*qxw)*an(ix,jy,kz,ls)/ &
!     &              (an(ix,jy,kz,lns)*(snu+1.)*rwdn**2)*db(ix,jy,kz)**2
                gtmp(ix,kz) = 3.6e18*(snu+2.)*( 0.224*(an(ix,jy,kz,ls)+qxw1) + 0.776*qxw)*(an(ix,jy,kz,ls)+qxw1)/ &
     &              (an(ix,jy,kz,lns)*(snu+1.)*rwdn**2)*db(ix,jy,kz)**2

               ELSE ! new form using a mass relationship m = p d^2 (instead of d^3 -- Cox 1988 QJRMS) so that density depends on size
                    ! p = 0.106214 for m = p v^(2/3)
                 dnsnow = 0.346159*sqrt(an(ix,jy,kz,lns)/(an(ix,jy,kz,ls)*db(ix,jy,kz)) )
                 IF ( .true. .or. dnsnow < 900. ) THEN
                  gtmp(ix,kz) = 1.e18*323.3226* 0.106214**2*(ksq*an(ix,jy,kz,ls) + (1.-ksq)*qxw)*an(ix,jy,kz,ls)*db(ix,jy,kz)**2*gsnow73/    &
     &                   (an(ix,jy,kz,lns)*(917.)**2* gsnow1*(1.0+snu)**(4./3.))
                 ELSE ! otherwise small enough to assume ice spheres?
                  gtmp(ix,kz) = (36./pi**2) * 1.e18*(snu+2.)*( 0.224*(an(ix,jy,kz,ls)+qxw1) + 0.776*qxw)*(an(ix,jy,kz,ls)+qxw1)/ &
     &              (an(ix,jy,kz,lns)*(snu+1.)*rwdn**2)*db(ix,jy,kz)**2
                 ENDIF
               ENDIF
             
             ENDIF
             
!             tmp = Min(1.0,1.e3*(an(ix,jy,kz,ls))*db(ix,jy,kz))
!             gtmp(ix,kz) = Max( 1.0*gtmp(ix,kz), 750.0*(tmp)**1.98)
             dtmps = gtmp(ix,kz)
             dtmp(ix,kz) = dtmp(ix,kz) + gtmp(ix,kz)
            ELSE
             gtmp(ix,kz) = dads*an(ix,jy,kz,ls)**(0.25)
             
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN !{
             dtmps = zsdryc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +          &
     &                   zsdryc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             ELSE
               dtmp(ix,kz) = dtmp(ix,kz) +          &
     &                  zswetc*an(ix,jy,kz,ls)**2/gtmp(ix,kz)
             ENDIF
             ENDIF !}
            ENDIF !}
           
           ENDIF !}
           
           ENDIF


!
! ice crystal contribution (Heymsfield, 1977, JAS)
!
         IF ( li .gt. 1 .and. idbzci .ne. 0 ) THEN
          
          gtmp(ix,kz) = 0.0 
           IF ( an(ix,jy,kz,li) .ge. 0.1e-3 ) THEN
             gtmp(ix,kz) = Min(1.0,1.e3*(an(ix,jy,kz,li))*db(ix,jy,kz))
             dtmp(ix,kz) = dtmp(ix,kz) + 750.0*(gtmp(ix,kz))**1.98
           ENDIF
           
          ENDIF
          
!           
! graupel/hail contribution
!
         IF ( lh .gt. 1 ) THEN ! {
           gtmp(ix,kz) = 0.0 
           dtmph = 0.0
           qxw = 0.0

          IF ( izieg .ge. 1 .and. ipconc .ge. 5 ) THEN

           ltest = .false.
           
           IF ( ltest .or. (an(ix,jy,kz,lh) .ge. qhmin .and. an(ix,jy,kz,lnh) .gt. 1.e-6 )) THEN
            
            IF ( lvh .gt. 1 ) THEN
             
             IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
               hwdn = db(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
               hwdn = Min( 900., Max( 100., hwdn ) )
              ELSE 
               hwdn = 500. ! hwdn1t
              ENDIF

             ENDIF

             chw = an(ix,jy,kz,lnh)
            IF ( chw .gt. 0.0 ) THEN                                         ! (Ferrier 94)
             xvh = db(ix,jy,kz)*an(ix,jy,kz,lh)/(hwdn*Max(1.0e-3,chw))
             IF ( xvh .lt. xvhmn .or. xvh .gt. xvhmx ) THEN
              xvh = Min( xvhmx, Max( xvhmn,xvh ) )
              chw = db(ix,jy,kz)*an(ix,jy,kz,lh)/(xvh*hwdn)
             ENDIF
             
             qh = an(ix,jy,kz,lh)
             
             IF ( lhw .gt. 1 ) THEN
               IF ( iusewetgraupel .eq. 1 ) THEN
                  qxw = an(ix,jy,kz,lhw)
               ELSEIF ( iusewetgraupel .eq. 2 ) THEN
                  IF ( hwdn .lt. 300. ) THEN
                    qxw = an(ix,jy,kz,lhw)
                  ENDIF
               ENDIF
             ELSEIF ( iusewetgraupel .eq. 3 ) THEN
                  IF ( hwdn .lt. 300. .and. temk(ix,jy,kz) > tfr .and. an(ix,jy,kz,lr) > qhmin ) THEN
                    qxw = Min( an(ix,jy,kz,lh), an(ix,jy,kz,lr))
                    qh = qh + qxw
                  ENDIF
             ELSEIF ( iusewetgraupel == 4 .and. temk(ix,jy,kz) .gt. tfr+0.25 .and. an(ix,jy,kz,lh) > an(ix,jy,kz,lr) &
     &              .and. an(ix,jy,kz,lr) > qhmin) THEN
               qxw = Min(0.5*an(ix,jy,kz,lh), an(ix,jy,kz,lr))
               qh = qh + qxw

             ENDIF
             
             IF ( lzh .gt. 1 ) THEN
             ELSE
             g1 = (6.0 + alphah)*(5.0 + alphah)*(4.0 + alphah)/((3.0 + alphah)*(2.0 + alphah)*(1.0 + alphah))
!             zx = g1*(db(ix,jy,kz)*an(ix,jy,kz,lh))**2/chw
!             ze = 0.224*1.e18*zx*(6./(pi*1000.))**2
             zx = g1*db(ix,jy,kz)**2*( 0.224*qh + 0.776*qxw)*qh/chw
             ze =1.e18*zx*(6./(pi*1000.))**2
             dtmp(ix,kz) = dtmp(ix,kz) + ze
             dtmph = ze
             ENDIF
             
            ENDIF
             
        !     IF ( an(ix,jy,kz,lh) .gt. 1.0e-3 ) write(0,*)  'Graupel Z : ',dtmph,ze
           ENDIF
          
          ELSE
          
          dtmph = 0.0
          
           IF ( an(ix,jy,kz,lh) .ge. qhmin ) THEN
             gtmp(ix,kz) = dadh*an(ix,jy,kz,lh)**(0.25)
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN
             dtmph =  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)
             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)
             ELSE
!               IF ( hwdn .gt. 700.0 ) THEN
                 dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhdryc*an(ix,jy,kz,lh)**2/gtmp(ix,kz)
! 
!     &                               (zhwetc*gtmp(ix,kz)**7)**0.95
!               ELSE
!                 dtmp(ix,kz) = dtmp(ix,kz) + zhwetc*gtmp(ix,kz)**7
!               ENDIF
             ENDIF
             ENDIF
           ENDIF
          
         
          
          ENDIF
 

          ENDIF ! }
          
          ENDIF ! na .gt. 5

        
        IF ( izieg .ge. 1 .and. lhl .gt. 1 ) THEN

        hldn = 900.0
        gtmp(ix,kz) = 0.0
        dtmphl = 0.0
        qxw = 0.0
        

        IF ( lvhl .gt. 1 ) THEN
          IF ( an(ix,jy,kz,lvhl) .gt. 0.0 ) THEN
            hldn = db(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
            hldn = Min( 900., Max( 300., hldn ) )
          ELSE 
            hldn = 900. 
          ENDIF
        ELSE
          hldn = rho_qhl
        ENDIF 


        IF ( ipconc .ge. 5 ) THEN

           ltest = .false.

          IF ( ltest .or. ( an(ix,jy,kz,lhl) .ge. qhlmin .and. an(ix,jy,kz,lnhl) .gt. 0.) ) THEN !{
            chl = an(ix,jy,kz,lnhl)
            IF ( chl .gt. 0.0 ) THEN !{
             xvhl = db(ix,jy,kz)*an(ix,jy,kz,lhl)/         &
     &        (hldn*Max(1.0e-9,an(ix,jy,kz,lnhl)))
            IF ( xvhl .lt. xvhlmn .or. xvhl .gt. xvhlmx ) THEN ! {
              xvhl = Min( xvhlmx, Max( xvhlmn,xvhl ) )
              chl = db(ix,jy,kz)*an(ix,jy,kz,lhl)/(xvhl*hldn)
              an(ix,jy,kz,lnhl) = chl
            ENDIF ! }

             IF ( lhlw .gt. 1 ) THEN
               IF ( iusewethail .eq. 1 ) THEN
                  qxw = an(ix,jy,kz,lhlw)
               ELSEIF ( iusewethail .eq. 2 ) THEN
                  IF ( hldn .lt. 300. ) THEN
                    qxw = an(ix,jy,kz,lhlw)
                  ENDIF
               ENDIF
             ENDIF
            
             IF ( lzhl .gt. 1 ) THEN !{
             ELSE !}

             g1 = (6.0 + alphahl)*(5.0 + alphahl)*(4.0 + alphahl)/((3.0 + alphahl)*(2.0 + alphahl)*(1.0 + alphahl))
             zx = g1*db(ix,jy,kz)**2*( 0.224*an(ix,jy,kz,lhl) + 0.776*qxw)*an(ix,jy,kz,lhl)/chl
!             zx = g1*(db(ix,jy,kz)*an(ix,jy,kz,lhl))**2/chl
             ze = 1.e18*zx*(6./(pi*1000.))**2 ! 3/28/2016 removed extra factor of 0.224
             dtmp(ix,kz) = dtmp(ix,kz) + ze
             dtmphl = ze
             
             ENDIF !}
            ENDIF!}
        !     IF ( an(ix,jy,kz,lh) .gt. 1.0e-3 ) write(0,*)  'Graupel Z : ',dtmph,ze
           ENDIF

          
          ELSE
          
          
           IF ( an(ix,jy,kz,lhl) .ge. qhlmin ) THEN ! {
            dadhl = ( db(ix,jy,kz) /(pi*hldn*cnohl) )**(.25)
             gtmp(ix,kz) = dadhl*an(ix,jy,kz,lhl)**(0.25)
             IF ( gtmp(ix,kz) .gt. 0.0 ) THEN ! {

              zhldryc = 0.224*cr2*( db(ix,jy,kz)/rwdn)**2/cnohl 

             dtmphl =  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)

             IF ( temk(ix,jy,kz) .lt. tfr ) THEN
               dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)
             ELSE
!               IF ( hwdn .gt. 700.0 ) THEN
                 dtmp(ix,kz) = dtmp(ix,kz) +                   &
     &                  zhldryc*an(ix,jy,kz,lhl)**2/gtmp(ix,kz)
! 
!     :                               (zhwetc*gtmp(ix,kz)**7)**0.95
!               ELSE
!                 dtmp(ix,kz) = dtmp(ix,kz) + zhwetc*gtmp(ix,kz)**7
!               ENDIF
             ENDIF
             ENDIF ! }
           
           ENDIF ! }
          
         ENDIF ! ipconc .ge. 5


        ENDIF ! izieg .ge. 1 .and. lhl .gt. 1 

          
           
          IF ( dtmp(ix,kz) .gt. 0.0 ) THEN
            dbz(ix,jy,kz) = Max(dbzmin, 10.0*Log10(dtmp(ix,kz)) )
            
            IF ( dbz(ix,jy,kz) .gt. dbzmax ) THEN
              dbzmax = Max(dbzmax,dbz(ix,jy,kz))
              imx = ix
              jmx = jy
              kmx = kz
            ENDIF
          ELSE 
             dbz(ix,jy,kz) = dbzmin
             IF ( lh > 1 .and. lhl > 1) THEN
               IF ( an(ix,jy,kz,lh) > 1.0e-3 ) THEN
                 write(0,*) 'radardbz: qr,qh,qhl = ',an(ix,jy,kz,lr), an(ix,jy,kz,lh),an(ix,jy,kz,lhl)
                 write(0,*) 'radardbz: dtmps,dtmph,dadh,dadhl,dtmphl = ',dtmps,dtmph,dadh,dadhl,dtmphl
                 
                 IF ( lzh>1 .and. lzhl>1 ) write(0,*) 'radardbz: zh, zhl = ',an(ix,jy,kz,lzh),an(ix,jy,kz,lzhl)
               ENDIF
             ENDIF
          ENDIF

!         IF ( an(ix,jy,kz,lh) .gt. 1.e-4 .and. 
!     &        dbz(ix,jy,kz) .le. 0.0 ) THEN
!          write(0,*) 'dbz = ',dbz(ix,jy,kz)
!          write(0,*) 'Hail intercept: ',xcnoh,ix,kz
!          write(0,*) 'Hail,snow q: ',an(ix,jy,kz,lh),an(ix,jy,kz,ls)
!          write(0,*) 'Hail,snow c: ',an(ix,jy,kz,lnh),an(ix,jy,kz,lns)
!          write(0,*) 'dtmps,dtmph = ',dtmps,dtmph
!         ENDIF
        IF ( .not. dtmp(ix,kz) .lt. 1.e30 .or. dbz(ix,jy,kz) > 190.0 ) THEN
!        IF ( ix == 31 .and. kz == 20 .and. jy == 23 ) THEN
!          write(0,*) 'my_rank = ',my_rank
          write(0,*) 'ix,jy,kz = ',ix,jy,kz
          write(0,*) 'dbz = ',dbz(ix,jy,kz)
          write(0,*) 'db, zhdryc = ',db(ix,jy,kz),zhdryc
          write(0,*) 'Hail intercept: ',xcnoh,ix,kz
          write(0,*) 'Hail,snow q: ',an(ix,jy,kz,lh),an(ix,jy,kz,ls)
          write(0,*) 'graupel density hwdn = ',hwdn
          write(0,*) 'rain q: ',an(ix,jy,kz,lr)
          write(0,*) 'ice q: ',an(ix,jy,kz,li)
          IF ( lhl .gt. 1 ) write(0,*) 'Hail (lhl): ',an(ix,jy,kz,lhl)
          IF (ipconc .ge. 3 ) write(0,*) 'rain c: ',an(ix,jy,kz,lnr)
          IF ( lzr > 1 ) write(0,*) 'rain Z: ',an(ix,jy,kz,lzr)
          IF ( ipconc .ge. 5 ) THEN
          write(0,*) 'Hail,snow c: ',an(ix,jy,kz,lnh),an(ix,jy,kz,lns)
          IF ( lhl .gt. 1 ) write(0,*) 'Hail (lnhl): ',an(ix,jy,kz,lnhl)
          IF ( lzhl .gt. 1 ) THEN 
            write(0,*) 'Hail (lzhl): ',an(ix,jy,kz,lzhl)
            write(0,*) 'chl,xvhl,dhl = ',chl,xvhl,(xvhl*6./3.14159)**(1./3.)
            write(0,*) 'xvhlmn,xvhlmx = ',xvhlmn,xvhlmx
          ENDIF
          ENDIF
          write(0,*) 'chw,xvh = ', chw,xvh
          write(0,*) 'dtmps,dtmph,dadh,dadhl,dtmphl = ',dtmps,dtmph,dadh,dadhl,dtmphl
          write(0,*) 'dtmpr = ',dtmpr
          write(0,*) 'gtmp = ',gtmp(ix,kz),dtmp(ix,kz)
          IF ( .not. (dbz(ix,jy,kz) .gt. -100 .and. dbz(ix,jy,kz) .lt. 200 ) ) THEN
            write(0,*) 'dbz out of bounds! STOP!'
!            STOP
          ENDIF
         ENDIF

           
          ENDDO ! ix
         ENDDO ! kz
      ENDDO ! jy
            
      
      
      
!      write(0,*)  'na,lr = ',na,lr
      IF ( printyn .eq. 1 ) THEN
!      IF ( dbzmax .gt. dbzmin ) THEN
        write(iunit,*) 'maxdbz,ijk = ',dbzmax,imx,jmx,kmx
        write(iunit,*) 'qrw = ',an(imx,jmx,kmx,lr)
        
        IF ( lh .gt. 1 ) THEN
          write(iunit,*) 'qi  = ',an(imx,jmx,kmx,li)
          write(iunit,*) 'qsw = ',an(imx,jmx,kmx,ls)
          write(iunit,*) 'qhw = ',an(imx,jmx,kmx,lh)
          IF ( lhl .gt. 1 ) write(iunit,*) 'qhl = ',an(imx,jmx,kmx,lhl)
        ENDIF

      
      ENDIF
      
      
      RETURN
      END subroutine radardd02
      

! ##############################################################################
! ##############################################################################


! #####################################################################
! #####################################################################
!
! Subroutine for explicit cloud condensation and droplet nucleation
!
   SUBROUTINE NUCOND    &
     &  (nx,ny,nz,na,jyslab & 
     &  ,nor,norz,dtp,nxi & 
     &  ,dz3d & 
     &  ,t0,t9 & 
     &  ,an,dn,p2 & 
     &  ,pn,w & 
     &  ,axtra,io_flag &
     &  ,ssfilt,t00,t77,flag_qndrop  &
     & )


   implicit none

      integer :: nx,ny,nz,na,nxi
      integer :: nor,norz, jyslab ! ,nht,ngt,igsr
      real    :: dtp  ! time step
      logical :: flag_qndrop

      integer, parameter :: ng1 = 1


!
! external temporary arrays
!
      real t00(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t77(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real t0(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t1(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t3(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t4(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t5(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t6(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t7(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real t8(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real t9(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      

      real p2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)  ! perturbation Pi
      real pn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      real an(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real w(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
!      real qv(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      real ssfilt(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)
      

      real pb(-norz+ng1:nz+norz)
      real pinit(-norz+ng1:nz+norz)

      real dz3d(-nor+1:nx+nor,-nor+1:ny+nor,-norz+1:nz+norz)

      
    ! local


      real axtra(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz,nxtra)
      logical :: io_flag
      
      real :: dv

! 
!  declarations microphysics and for gather/scatter
!
      integer nxmpb,nzmpb,nxz
      integer mgs,ngs,numgs,inumgs
      parameter (ngs=500)
      integer ngscnt,igs(ngs),kgs(ngs)
      integer kgsp(ngs),kgsm(ngs)
      integer nsvcnt
      
      integer ix,kz,i,n, kp1
      integer :: jy, jgs
      integer ixb,ixe,jyb,jye,kzb,kze
    
      integer itile,jtile,ktile
      integer ixend,jyend,kzend,kzbeg
      integer nxend,nyend,nzend,nzbeg

!
! Variables for Ziegler warm rain microphysics
!      


      real ccnc(ngs), ccna(ngs), cnuc(ngs), cwnccn(ngs)
      real sscb  ! 'cloud base' SS threshold
      parameter ( sscb = 2.0 )
      integer idecss  ! flag to turn on (=1) decay of ssmax when no cloud or ice crystals
      parameter ( idecss = 1 )
      integer iba ! flag to do condensation/nucleation in 1st or 2nd loop
                  ! =0 to use ad to calculate SS
                  ! =1 to use an at end of main jy loop to calculate SS
      parameter (iba = 1)
      integer ifilt   ! =1 to filter ssat, =0 to set ssfilt=ssat
      parameter ( ifilt = 0 ) 
      real temp1,temp2 ! ,ssold
      real ssmax(ngs)       ! maximum SS experienced by a parcel
      real ssmx
      real dnnet,dqnet
!      real cnu,rnu,snu,cinu
!      parameter ( cnu = 0.0, rnu = -0.8, snu = -0.8, cinu = 0.0 )
      real ventrx(ngs)
      real ventrxn(ngs)
      real volb, t2s
      real, parameter :: aa1 = 9.44e15, aa2 = 5.78e3  ! a1 in Ziegler

      real ec0, ex1, ft, rhoinv(ngs)
      
      real chw, g1, rd1

      real ac1,bc, taus, c1,d1,e1,f1,p380,tmp,tmp2 ! , sstdy, super
      real tmpmx, fw
      real x,y,del,r,alpr
      double precision :: vent1,vent2
      real g1palp
      real bs
      real v1, v2
      real d1r, d1i, d1s, e1i
      integer nc ! condensation step
      real dtcon,dtcon1,dtcon2 ! condensation time step (dtcon*nc = dtp)
      real delta
      integer ltemq1,ltemq1m ! ,ltemq1m2
      real dqv,qv1,ss1,ss2,qvs1,dqvs,dtemp,dt1   ! temporaries for condensation

      real ssi1, ssi2, dqvi, dqvis, dqvii,qis1
      real dqvr, dqc, dqr, dqi, dqs
      real qv1m,qvs1m,ss1m,ssi1m,qis1m
      real cwmastmp 
      real  dcloud,dcloud2 ! ,as, bs
      real cn(ngs) 

      integer ltemq
      
      integer il

      real  es(ngs) ! ss(ngs),
!      real  eis(ngs)
      real ssf(ngs),ssfkp1(ngs),ssfkm1(ngs),ssat0(ngs)
      real ssfjp1(ngs),ssfjm1(ngs)
      real ssfip1(ngs),ssfim1(ngs)

      real supcb, supmx
      parameter (supcb=0.5,supmx=238.0)
      real r2dxm, r2dym, r2dzm
      real dssdz, dssdy, dssdx
!      real tqvcon
      real epsi,d
      parameter (epsi = 0.622, d = 0.266)
      real r1,qevap ! ,slv
      
      real vr,nrx,qr,z1,z2,rdi,alp,xnutmp,xnuc
      real ctmp, ccwtmp
      real f5, qvs0  ! Kessler condensation factor
      real    :: t0p1, t0p3
      real qvex
      
!      real, dimension(ngs) :: temp, tempc, elv, elf, els, pqs, theta, temg, temcg
      real dqvcnd(ngs),dqwv(ngs),dqcw(ngs),dqci(ngs)
      real temp(ngs),tempc(ngs)
      real temg(ngs),temcg(ngs),theta(ngs),qvap(ngs) ! ,tembzg(ngs)
      real temgx(ngs),temcgx(ngs)
      real qvs(ngs),qis(ngs),qss(ngs),pqs(ngs)
      real felv(ngs),felf(ngs),fels(ngs)
      real felvcp(ngs),felvpi(ngs)
      real gamw(ngs),gams(ngs)   !   qciavl(ngs),
      real tsqr(ngs),ssi(ngs),ssw(ngs)
      real cc3(ngs),cqv1(ngs),cqv2(ngs)
      real qcwtmp(ngs),qtmp

      real fvent(ngs) !,fraci(ngs),fracl(ngs)
      real fwvdf(ngs),ftka(ngs),fthdf(ngs)
      real fadvisc(ngs),fakvisc(ngs)
      real fci(ngs),fcw(ngs)
      real fschm(ngs),fpndl(ngs)

      real pres(ngs),pipert(ngs)
      real pk(ngs)
      real rho0(ngs),pi0(ngs)
      real rhovt(ngs)
      real thetap(ngs),theta0(ngs),qwvp(ngs),qv0(ngs)
      real thsave(ngs)
      real qss0(ngs)
      real fcqv1(ngs)
      real wvel(ngs),wvelkm1(ngs)

      real wvdf(ngs),tka(ngs)
      real advisc(ngs)

      real rwvent(ngs)
      

      real :: qx(ngs,lv:lhab)
      real :: cx(ngs,lc:lhab)
      real :: xv(ngs,lc:lhab)
      real :: xmas(ngs,lc:lhab)
      real :: xdn(ngs,lc:lhab)
      real :: xdia(ngs,lc:lhab,3)
      real :: alpha(ngs,lc:lhab)
      real :: zx(ngs,lr:lhab)


      logical zerocx(lc:lqmx)

      integer, parameter :: iunit = 0
      
      real :: frac, hwdn, tmpg
      
      real :: cvm,cpm,rmm

      real, parameter :: rovcp = rd/cp
      real, parameter ::      cpv = 1885.0       ! specific heat of water vapor at constant pressure
      
      integer :: kstag
      
      integer :: count
      

! -------------------------------------------------------------------------------
      itile = nxi
      jtile = ny
      ktile = nz
      ixend = nxi
      jyend = ny
      kzend = nz
      nxend = nxi + 1
      nyend = ny + 1
      nzend = nz
      kzbeg = 1
      nzbeg = 1

      f5 = 237.3 * 17.27 * 2.5e6 / cp ! combined constants for rain condensation (Soong and Ogura 73)

       jy = 1
       kstag = 0
       pb(:) = 0.0
       pinit(:) = 0.0
      
      IF ( ipconc <= 1 .or. isedonly == 2 ) GOTO 2200

!
!  Ziegler nucleation 
!

!      ssfilt(:,:,:) = 0.0
      ssmx = 0
      count = 0

      do kz = 1,nz-kstag
        do ix = 1,nxi

         temp1 = an(ix,jy,kz,lt)*t77(ix,jy,kz)
          t0(ix,jy,kz) = temp1
          ltemq = Int( (temp1-163.15)/fqsat+1.5 )
         ltemq = Min( nqsat, Max(1,ltemq) )

          c1 = t00(ix,jy,kz)*tabqvs(ltemq)

          IF ( c1 > 0. ) THEN
            ssfilt(ix,jy,kz) = 100.*(an(ix,jy,kz,lv)/c1 - 1.0)  ! from "new" values
          ENDIF

        ENDDO
      ENDDO


!
!     jy = 1 ! working on a 2d slab
!!  VERY IMPORTANT:  SET jgs = jy

      jgs = jy

!
!..Gather microphysics
!
      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: Gather stage'

      nxmpb = 1
      nzmpb = 1
      nxz = nxi*nz
      numgs = nxz/ngs + 1


      do 2000 inumgs = 1,numgs

      ngscnt = 0


      kzb = nzmpb
      kze = nz-kstag
 !     if (kzbeg .le. nzmpb .and. kzend .gt. nzmpb) kzb = nzmpb

      ixb = nxmpb
      ixe = itile
!      if (ixbeg .le. nxmpb .and. ixend .gt. nxmpb) ixb = nxmpb

      do kz = kzb,kze
      do ix = nxmpb,nxi

      pqs(1) = 380.0/(pn(ix,jy,kz) + pb(kz))
      theta(1) = an(ix,jy,kz,lt)
      temg(1) = t0(ix,jy,kz)

      temcg(1) = temg(1) - tfr
      ltemq = (temg(1)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(1) = pqs(1)*tabqvs(ltemq)
      qis(1) = pqs(1)*tabqis(ltemq)

      qss(1) = qvs(1)


      if ( temg(1) .lt. tfr ) then
      end if
!
      if ( (temg(1) .gt. tfrh ) .and.  &
     &   ( an(ix,jy,kz,lv)  .gt. qss(1) .or. &
     &     an(ix,jy,kz,lc)  .gt. qxmin(lc)   .or.  &
     &     ( an(ix,jy,kz,lr)  .gt. qxmin(lr) .and. rcond == 2 )  &
     &     )) then
      ngscnt = ngscnt + 1
      igs(ngscnt) = ix
      kgs(ngscnt) = kz
      if ( ngscnt .eq. ngs ) goto 2100
      end if

      end do  !ix

      nxmpb = 1
      end do  !kz
!      if ( jy .eq. (ny-jstag) ) iend = 1
 2100 continue

      if ( ngscnt .eq. 0 ) go to 29998

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: dbg = 8'
      
!      write(0,*) 'NUCOND: dbg = 8, ngscnt,ssmx = ',ngscnt,ssmx

      
      qx(:,:) = 0.0
      cx(:,:) = 0.0

      xv(:,:) = 0.0
      xmas(:,:) = 0.0

      IF ( imurain == 1 ) THEN
        alpha(:,lr) = alphar
      ELSEIF ( imurain == 3 ) THEN
        alpha(:,lr) = xnu(lr)
      ENDIF

!
!  define temporaries for state variables to be used in calculations
!
      DO mgs = 1,ngscnt
      qx(mgs,lv) = an(igs(mgs),jy,kgs(mgs),lv)
       DO il = lc,lhab
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0)
       ENDDO

       qcwtmp(mgs) = qx(mgs,lc)


      theta0(mgs) = an(igs(mgs),jy,kgs(mgs),lt) !
      thetap(mgs) = 0.0
      theta(mgs) = an(igs(mgs),jy,kgs(mgs),lt)
      qv0(mgs) =  qx(mgs,lv)
      qwvp(mgs) = qx(mgs,lv) - qv0(mgs)

       pres(mgs) = pn(igs(mgs),jy,kgs(mgs)) + pb(kgs(mgs))
       pipert(mgs) = p2(igs(mgs),jy,kgs(mgs))
       rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
       rhoinv(mgs) = 1.0/rho0(mgs)
       rhovt(mgs) = Sqrt(rho00/rho0(mgs))
       pi0(mgs) = p2(igs(mgs),jy,kgs(mgs)) + pinit(kgs(mgs))
       temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
!       pk(mgs) = t77(igs(mgs),jy,kgs(mgs)) ! ( pres(mgs) / poo ) ** cap
       pk(mgs)   = p2(igs(mgs),jy,kgs(mgs)) + pinit(kgs(mgs)) ! t77(igs(mgs),jy,kgs(mgs))
       temcg(mgs) = temg(mgs) - tfr
       qss0(mgs) = (380.0)/(pres(mgs))
       pqs(mgs) = (380.0)/(pres(mgs))
       ltemq = (temg(mgs)-163.15)/fqsat+1.5
       ltemq = Min( nqsat, Max(1,ltemq) )
       qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
       qis(mgs) = pqs(mgs)*tabqis(ltemq)
!
        qvap(mgs) = max( (qwvp(mgs) + qv0(mgs)), 0.0 )
        es(mgs) = 6.1078e2*tabqvs(ltemq)
        qss(mgs) = qvs(mgs)


        temgx(mgs) = min(temg(mgs),313.15)
        temgx(mgs) = max(temgx(mgs),233.15)
        felv(mgs) = 2500837.367 * (273.15/temgx(mgs))**((0.167)+(3.67e-4)*temgx(mgs))
!
        IF ( eqtset <= 1 ) THEN
          felvcp(mgs) = felv(mgs)*cpi
        ELSE ! equation set 2 in cm1
          tmp = qx(mgs,li)+qx(mgs,ls)+qx(mgs,lh)
          IF ( lhl > 1 ) tmp = tmp + qx(mgs,lhl)
          cvm = cv+cvv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)
          cpm = cp+cpv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)
          rmm=rd+rw*qx(mgs,lv)
          
          IF ( eqtset == 2 ) THEN

           felvcp(mgs) = (felv(mgs)-rw*temg(mgs))/cvm

          ELSE
            felvcp(mgs) = (felv(mgs)*cv/(cp) - rw*temg(mgs)*(1.0-rovcp*cpm/rmm))/cvm
            felvpi(mgs) = pi0(mgs)*rovcp*(felv(mgs)/(temg(mgs)) - rw*cpm/rmm)/cvm
          ENDIF

        ENDIF

        temcgx(mgs) = min(temg(mgs),273.15)
        temcgx(mgs) = max(temcgx(mgs),223.15)
        temcgx(mgs) = temcgx(mgs)-273.15
        felf(mgs) = 333690.6098 + (2030.61425)*temcgx(mgs) - (10.46708312)*temcgx(mgs)**2
!
        fels(mgs) = felv(mgs) + felf(mgs)
        fcqv1(mgs) = 4098.0258*felv(mgs)*cpi

      wvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)* &
     &  (101325.0/(pb(kgs(mgs)) + pn(igs(mgs),jgs,kgs(mgs))))                            ! diffusivity of water vapor, Hall and Pruppacher (76)
      advisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* &
     &  (temg(mgs)/296.0)**(1.5)                         ! dynamic viscosity (SMT; see Beard & Pruppacher 71)
      tka(mgs) = tka0*advisc(mgs)/advisc1                 ! thermal conductivity


      ENDDO



!
! load concentrations
!
      if ( ipconc .ge. 1 ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
       end do
      end if
      if ( ipconc .ge. 2 ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
        cwnccn(mgs) = cwccn*rho0(mgs)/rho00
        cn(mgs) = 0.0
        IF ( lccn .gt. 1 ) THEN
          ccnc(mgs) = an(igs(mgs),jy,kgs(mgs),lccn)
        ELSE
          ccnc(mgs) = cwnccn(mgs)
        ENDIF
        IF ( lccna > 1 ) THEN
          ccna(mgs) = an(igs(mgs),jy,kgs(mgs),lccna)
        ELSE
          IF ( lccn > 1 ) THEN
            ccna(mgs) = cwnccn(mgs) - ccnc(mgs)
          ELSE
            ccna(mgs) = cx(mgs,lc) ! approximation of number of activated ccn
          ENDIF
        ENDIF
       end do
      end if
      if ( ipconc .ge. 3 ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)
       end do
      end if

!        cnuc(1:ngscnt) = cwccn*rho0(mgs)/rho00*(1. - renucfrac) + ccnc(1:ngscnt)*renucfrac
       DO mgs = 1,ngscnt
        cnuc(mgs) = Max(ccnc(mgs),cwnccn(mgs))*(1. - renucfrac) + ccnc(mgs)*renucfrac
       ENDDO

!  Set density
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_DR: Set density'

      do mgs = 1,ngscnt
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)
      end do

      ventrx(:) = ventr
      ventrxn(:) = ventrn
      


!       write(0,*) 'NUCOND: Set ssf variables, ssmxinit =',ssmxinit
      ssmx = 0.0
      DO mgs = 1,ngscnt
      
      kp1 = Min(nz, kgs(mgs)+1 )
      wvel(mgs) = (0.5)*(w(igs(mgs),jgs,kp1) & 
     &                  +w(igs(mgs),jgs,kgs(mgs)))
      wvelkm1(mgs) = (0.5)*(w(igs(mgs),jgs,kgs(mgs)) & 
     &                  +w(igs(mgs),jgs,Max(1,kgs(mgs)-1)))

      ssat0(mgs)  = ssfilt(igs(mgs),jgs,kgs(mgs))
      ssf(mgs)    = ssfilt(igs(mgs),jgs,kgs(mgs))
!      ssmx = Max( ssmx, ssf(mgs) )

      
      ssfkp1(mgs) = ssfilt(igs(mgs),jgs,Min(nz-1,kgs(mgs)+1))
      ssfkm1(mgs) = ssfilt(igs(mgs),jgs,Max(1,kgs(mgs)-1))


      ENDDO



!
!  cloud water variables
!

      if ( ndebug .gt. 0 )write(0,*) 'ICEZVD_DR: Set cloud water variables'

      do mgs = 1,ngscnt
      xv(mgs,lc) = 0.0
      IF ( ipconc .ge. 2 .and. cx(mgs,lc) .gt. 1.0e6 ) THEN
        xmas(mgs,lc) = &
     &    min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),cwmasn),cwmasx )
        xv(mgs,lc) = xmas(mgs,lc)/xdn(mgs,lc)
      ELSE
       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 0.01 ) THEN
        xmas(mgs,lc) = &
     &     min( max(qx(mgs,lc)*rho0(mgs)/cx(mgs,lc),xdn(mgs,lc)*xvmn(lc)), &
     &      xdn(mgs,lc)*xvmx(lc) )

        cx(mgs,lc) = qx(mgs,lc)*rho0(mgs)/xmas(mgs,lc)

       ELSEIF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .le. 0.01 ) THEN
        xmas(mgs,lc) = xdn(mgs,lc)*4.*pi/3.*(5.0e-6)**3
        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc)

       ELSE
        xmas(mgs,lc) = cwmasn
       ENDIF
      ENDIF
      xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**c1f3


      end do
!
! rain
!
      do mgs = 1,ngscnt
      if ( qx(mgs,lr) .gt. qxmin(lr) ) then

      if ( ipconc .ge. 3 ) then
        xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*Max(1.0e-9,cx(mgs,lr)))
!      parameter( xvmn(lr)=2.8866e-13, xvmx(lr)=4.1887e-9 )  ! mks
        IF ( xv(mgs,lr) .gt. xvmx(lr) ) THEN
          xv(mgs,lr) = xvmx(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmx(lr)*xdn(mgs,lr))
        ELSEIF ( xv(mgs,lr) .lt. xvmn(lr) ) THEN
          xv(mgs,lr) = xvmn(lr)
          cx(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xvmn(lr)*xdn(mgs,lr))
        ENDIF

        xmas(mgs,lr) = xv(mgs,lr)*xdn(mgs,lr)
        xdia(mgs,lr,3) = (xmas(mgs,lr)*cwc1)**(1./3.) ! xdia(mgs,lr,1)
        IF ( imurain == 3 ) THEN
!          xdia(mgs,lr,1) = (6.*pii*xv(mgs,lr)/(alpha(mgs,lr)+1.))**(1./3.)
          xdia(mgs,lr,1) = xdia(mgs,lr,3) ! formulae for Ziegler (1985) use mean volume diameter, not lambda**(-1)
        ELSE ! imurain == 1, Characteristic diameter (1/lambda)
          xdia(mgs,lr,1) = (6.*piinv*xv(mgs,lr)/((alpha(mgs,lr)+3.)*(alpha(mgs,lr)+2.)*(alpha(mgs,lr)+1.)))**(1./3.)
        ENDIF
!        rwrad(mgs) = 0.5*xdia(mgs,lr,1)

! Inverse exponential version:
!        xdia(mgs,lr,1) =
!     >  (qx(mgs,lr)*rho0(mgs)
!     > /(pi*xdn(mgs,lr)*cx(mgs,lr)))**(0.333333)
      ELSE
        xdia(mgs,lr,1) = &
     &  (qx(mgs,lr)*rho0(mgs)/(pi*xdn(mgs,lr)*cno(lr)))**(0.25)
      end if
      else
        xdia(mgs,lr,1) = 1.e-9
!        rwrad(mgs) = 0.5*xdia(mgs,lr,1)
      end if

      end do


!
!  Ventilation coefficients

      do mgs = 1,ngscnt


      fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))* & 
     &  (temg(mgs)/296.0)**(1.5)

      fakvisc(mgs) = fadvisc(mgs)*rhoinv(mgs)

      fwvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)* & 
     &  (101325.0/(pres(mgs)))
      
      fschm(mgs) = (fakvisc(mgs)/fwvdf(mgs))

      fvent(mgs) = (fschm(mgs)**(1./3.)) * (fakvisc(mgs)**(-0.5))

      end do
!
!
!  Ziegler nucleation 
!
!
! cloud evaporation, condensation, and nucleation
!  sqsat -> qss(mgs)

      DO mgs=1,ngscnt
        dcloud = 0.0
        IF ( temg(mgs) .le. tfrh ) THEN
         CYCLE
        ENDIF

      IF( ssat0(mgs) .GT. 0. .OR. ssf(mgs) .GT. 0. ) GO TO 620
!6/4      IF( qvap(mgs) .EQ. qss(mgs) ) GO TO 631
!
!.... EVAPORATION. QV IS LESS THAN qss(mgs).
!.... EVAPORATE CLOUD FIRST
!
      IF ( qx(mgs,lc) .LE. 0. ) GO TO 631
!.... CLOUD EVAPORATION.
! convert input 'cp' to cgs
      R1=1./(1. + caw*(273.15 - cbw)*qss(mgs)*felv(mgs)/ &
     &            (cp*(temg(mgs) - cbw)**2))
      QEVAP= Min( qx(mgs,lc), R1*(qss(mgs)-qvap(mgs)) )


      IF ( qx(mgs,lc) .LT. QEVAP ) THEN ! GO TO 63
        qwvp(mgs) = qwvp(mgs) + qx(mgs,lc)
        thetap(mgs) = thetap(mgs) - felv(mgs)*qx(mgs,lc)/(cp*pi0(mgs))
        IF ( io_flag .and. nxtra > 1 ) THEN
           axtra(igs(mgs),jy,kgs(mgs),1) = -qx(mgs,lc)/dtp
        ENDIF
        qx(mgs,lc) = 0.
        cx(mgs,lc) = 0.
      ELSE
        qwvp(mgs) = qwvp(mgs) + QEVAP
        qx(mgs,lc) = qx(mgs,lc) - QEVAP
        IF ( qx(mgs,lc) .le. 0. ) cx(mgs,lc) = 0.
        thetap(mgs) = thetap(mgs) - felv(mgs)*QEVAP/(CP*pi0(mgs))
        IF ( io_flag .and. nxtra > 1 ) THEN
           axtra(igs(mgs),jy,kgs(mgs),1) = -QEVAP/dtp
        ENDIF

      ENDIF

      GO TO 631


  620 CONTINUE

!.... CLOUD CONDENSATION

        IF ( qx(mgs,lc) .GT. qxmin(lc) .and. cx(mgs,lc) .ge. 1. ) THEN



!       ac1 =  xdn(mgs,lc)*elv(kgs(mgs))**2*epsi/
!     :        (tka(kgs(mgs))*rw*temg(mgs)**2)
! took out xdn factor because it cancels later...
       ac1 =  felv(mgs)**2/(tka(mgs)*rw*temg(mgs)**2)


!       bc = xdn(mgs,lc)*rw*temg(mgs)/
!     :       (epsi*wvdf(kgs(mgs))*es(mgs))
! took out xdn factor because it cancels later...
       bc =   rw*temg(mgs)/(wvdf(mgs)*es(mgs))

!       bs = rho0(mgs)*((rd*temg(mgs)/(epsi*es(mgs)))+
!     :             (epsi*elv(kgs(mgs))**2/(pres(mgs)*temg(mgs)*cp)))

!       taus = Min(dtp, xdn(mgs,lc)*rho0(mgs)*(ac1+bc)/
!     :        (4*pi*0.89298*BS*0.5*xdia(mgs,lc,1)*cx(mgs,lc)*xdn(mgs,lc)))

!
      IF ( ssf(mgs) .gt. 0.0 .or. ssat0(mgs) .gt. 0.0 ) THEN
       IF ( ny .le. 2 ) THEN
!        write(0,*)  'undershoot: ',ssf(mgs),
!     :   ( (qx(mgs,lv) - dcloud)/c1 - 1.0)*100.
       ENDIF



       IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN

         IF ( xdia(mgs,lc,1) .le. 0.0 ) THEN
          xmas(mgs,lc) = cwmasn
          xdia(mgs,lc,1) = (xmas(mgs,lc)*cwc1)**c1f3
         ENDIF
        d1 = (1./(ac1 + bc))*4.0*pi*ventc &
     &        *0.5*xdia(mgs,lc,1)*cx(mgs,lc)*rhoinv(mgs)

       ELSE
         d1 = 0.0
       ENDIF

       IF ( rcond .eq. 2 .and. qx(mgs,lr) .gt. qxmin(lr) .and. cx(mgs,lr) > 1.e-9 ) THEN
          IF ( imurain == 3 ) THEN
           IF ( izwisventr == 1 ) THEN
            rwvent(mgs) = ventrx(mgs)*(1.6 + 124.9*(1.e-3*rho0(mgs)*qx(mgs,lr))**.2046)
           ELSE ! izwisventr = 2
!  Following Wisner et al. (1972) but using gamma of volume. Note that Ferrier's rain fall speed does not integrate with gamma of volume, so using Vr = ar*d^br
          rwvent(mgs) =   &
     &  (0.78*ventrx(mgs) + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
           ENDIF

          ELSE ! imurain == 1

           IF ( iferwisventr == 1 ) THEN
             alpr = Min(alpharmax,alpha(mgs,lr) )
!             alpr = alpha(mgs,lr)
             x =  1. + alpr

              tmp = 1 + alpr
              i = Int(dgami*(tmp))
              del = tmp - dgam*i
              g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

              tmp = 2.5 + alpr + 0.5*bx(lr)
              i = Int(dgami*(tmp))
              del = tmp - dgam*i
              y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp ! ratio of gamma functions

         vent1 = dble(xdia(mgs,lr,1))**(-2. - alpr)
         vent2 = dble(1./xdia(mgs,lr,1) + 0.5*fx(lr))**dble(2.5+alpr+0.5*bx(lr))
        
        
        rwvent(mgs) =    &
     &    0.78*x +    &
     &    0.308*fvent(mgs)*y*   &
     &            Sqrt(ax(lr)*rhovt(mgs))*(vent1/vent2)

           ELSEIF ( iferwisventr == 2 ) THEN
          
!  Following Wisner et al. (1972) but using gamma of volume. Note that Ferrier's rain fall speed does not integrate with gamma of volume, so using Vr = ar*d^br
            x =  1. + alpha(mgs,lr)

            rwvent(mgs) =   &
     &        (0.78*x + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &         *Sqrt((ar*rhovt(mgs)))   &
     &         *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )

          
          ENDIF ! iferwisventr
          
       ENDIF ! imurain

       d1r = (1./(ac1 + bc))*4.0*pi*rwvent(mgs) & 
     &        *0.5*xdia(mgs,lr,1)*cx(mgs,lr)*rhoinv(mgs)
       ELSE
       d1r = 0.0
       ENDIF
       
       
       e1  = felvcp(mgs)/(pi0(mgs))
       f1 = pk(mgs) ! (pres(mgs)/poo)**cap

!
!  fifth trial to see what happens:
!
       ltemq = (temg(mgs)-163.15)/fqsat+1.5
       ltemq = Min( nqsat, Max(1,ltemq) )
       ltemq1 = ltemq
       temp1 = temg(mgs)
       p380 = 380.0/pres(mgs)

!       taus = Max( 0.05*dtp, Min(taus, 0.25*dtp ) )
!       nc = NInt(dtp/Min(1.0,0.5*taus))
!       dtcon = dtp/float(nc)
       ss1 = qx(mgs,lv)/qvs(mgs)
       ss2 = ss1
       temp2 = temp1
       qv1 = qx(mgs,lv)
       qvs1 = qvs(mgs)
       qis1 = qis(mgs)
       dt1 = 0.0


!          dtcon = Max(dtcon,0.2)
!          nc = Nint(dtp/dtcon)

       ltemq1 = ltemq
! want to start out with a small time step to handle the steep slope
! and fast changes, then can switch to a larger step (dtcon2) for the
! rest of the big time step.
! base the initial time step (dtcon1) on the slope (delta)
       IF ( Abs(ss1 - 1.0) .gt. 1.e-5 ) THEN
         delta = 0.5*(qv1-qvs1)/(d1*(ss1 - 1.0))
       ELSE
         delta = 0.1*dtp
       ENDIF
! delta is the extrapolated time to get halfway from qv1 to qvs1
! want at least 5 time steps to the halfway point, so multiply by 0.2
! for the initial time step
       dtcon1 = Min(0.05,0.2*delta)
       nc = Max(5,2*NInt( (dtp-4.0*dtcon1)/delta))
       dtcon2 = (dtp-4.0*dtcon1)/nc

       n = 1
       dt1 = 0.0
       nc = 0
       dqc = 0.0
       dqr = 0.0
       dqi = 0.0
       dqs = 0.0
       dqvii = 0.0
       dqvis = 0.0

       RK2c: DO WHILE ( dt1 .lt. dtp )
          nc = 0
          IF ( n .le. 4 ) THEN
            dtcon = dtcon1
          ELSE
            dtcon = dtcon2
          ENDIF
 609       dqv  = -(ss1 - 1.)*d1*dtcon
           dqvr = -(ss1 - 1.)*d1r*dtcon
            dtemp = -0.5*e1*f1*(dqv + dqvr)
!          write(0,*) 'RK2c dqv1 = ',dqv
! calculate midpoint values:
           ltemq1m = ltemq1 + Nint(dtemp*fqsat + 0.5)
           IF ( ltemq1m .lt. 1 .or. ltemq1m .gt. nqsat ) THEN
             write(0,*) 'STOP in nucond line 1192 '
             write(0,*) ' ltemq1m,icond = ',ltemq1m,icond
             write(0,*) ' dtemp,e1,f1,dqv,dqvr = ', dtemp,e1,f1,dqv,dqvr
             write(0,*) ' d1,d1r,dtcon,ss1 = ',d1,d1r,dtcon,ss1
             write(0,*) ' dqc, dqr = ',dqc,dqr
             write(0,*) ' qv,qc,qr = ',qx(mgs,lv)*1000.,qx(mgs,lc)*1000.,qx(mgs,lr)*1000.
             write(0,*) ' i, j, k = ',igs(mgs),jy,kgs(mgs)
             write(0,*) ' dtcon1,dtcon2,delta = ',dtcon1,dtcon2,delta
             write(0,*) ' nc,dtp = ',nc,dtp
             write(0,*) ' rwvent,xdia,crw,ccw = ', rwvent(mgs),xdia(mgs,lr,1),cx(mgs,lr),cx(mgs,lc)
             write(0,*) ' fvent,alphar = ',fvent(mgs),alpha(mgs,lr)
             write(0,*) ' xvr,xmasr,xdnr,cwc1 = ',xv(mgs,lr),xmas(mgs,lr),xdn(mgs,lr),cwc1
           ENDIF
            dqvs = dtemp*p380*dtabqvs(ltemq1m)
            qv1m = qv1 + dqv + dqvr
!          qv1mr = qv1r + dqvr

            qvs1m = qvs1 + dqvs
            ss1m = qv1m/qvs1m

    ! check for undersaturation when no ice is present, if so, then reduce time step
          IF ( ss1m .lt. 1.  .and. (dqvii + dqvis) .eq. 0.0 ) THEN
            dtcon = (0.5*dtcon)
            IF ( dtcon .ge. dtcon1 ) THEN
             GOTO 609
            ELSE
             EXIT
            ENDIF
          ENDIF
! calculate full step:
          dqv  = -(ss1m - 1.)*d1*dtcon
          dqvr = -(ss1m - 1.)*d1r*dtcon


!          write(0,*) 'RK2a dqv1m = ',dqv
          dtemp = -e1*f1*(dqv + dqvr)
          ltemq1 = ltemq1 + Nint(dtemp*fqsat + 0.5)
           IF ( ltemq1 .lt. 1 .or. ltemq1 .gt. nqsat ) THEN
             write(0,*) 'STOP in nucond line 1230 '
             write(0,*) ' ltemq1m,icond = ',ltemq1m,icond
             write(0,*) ' dtemp,e1,dqv,dqvr = ', dtemp,e1,dqv,dqvr
           ENDIF
          dqvs = dtemp*p380*dtabqvs(ltemq1)

          qv1 = qv1 + dqv + dqvr

          dqc = dqc - dqv
          dqr = dqr - dqvr

          qvs1 = qvs1 + dqvs
          ss1 = qv1/qvs1
          temp1 = temp1 + dtemp
          IF ( temp2 .eq. temp1 .or. ss2 .eq. ss1 .or.  &
     &           ss1 .eq. 1.00 .or.  &
     &      ( n .gt. 10 .and. ss1 .lt. 1.0005 ) ) THEN
!           write(0,*) 'RK2c break'
           EXIT
          ELSE
           ss2 = ss1
           temp2 = temp1
           dt1 = dt1 + dtcon
           n = n + 1
          ENDIF
       ENDDO RK2c


        dcloud = dqc ! qx(mgs,lv) - qv1
        thetap(mgs) = thetap(mgs) + e1*(DCLOUD + dqr)
        IF ( eqtset > 2 ) THEN
           pipert(mgs) = pipert(mgs) + felvpi(mgs)*(DCLOUD + dqr)
        ENDIF
        IF ( io_flag .and. nxtra > 1 ) THEN
           axtra(igs(mgs),jy,kgs(mgs),1) = DCLOUD/dtp
           axtra(igs(mgs),jy,kgs(mgs),2) = axtra(igs(mgs),jy,kgs(mgs),2) + dqr/dtp
        ENDIF
        qwvp(mgs) = qwvp(mgs) - (DCLOUD + dqr)
        qx(mgs,lc) = qx(mgs,lc) + DCLOUD
        qx(mgs,lr) = qx(mgs,lr) + dqr
!        t9(igs(mgs),jy,kgs(mgs)) = t9(igs(mgs),jy,kgs(mgs)) + (DCLOUD + dqr)/dtp*felv(mgs)/(cp*pi0(mgs)) !* &
!!     &                 dx*dy*dz3d(igs(mgs),jy,kgs(mgs))


        theta(mgs) = thetap(mgs) + theta0(mgs)
        temg(mgs) = theta(mgs)*f1
        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )
        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
!        es(mgs) = 6.1078e2*tabqvs(ltemq)

!

      ENDIF  ! dcloud .gt. 0.


      ELSE  ! qc .le. qxmin(lc)

!        IF ( ssf(mgs) .gt. 0.0 .and. .not. flag_qndrop ) THEN ! flag_qndrop turns off primary nucleation when using wrf-chem with progn=1
        IF ( ssf(mgs) .gt. 0.0 ) THEN ! .and.  ssmax(mgs) .lt. sscb ) THEN ! except that wrf-chem does not seem to initialize qc for activated aerosols, so keep this, after all

          IF ( iqcinit == 1 ) THEN

         qvs0   = 380.*exp(17.27*(temg(mgs)-273.)/(temg(mgs)- 36.))/pk(mgs)

         dcloud = Max(0.0, (qx(mgs,lv)-qvs0) / (1.+qvs0*f5/(temg(mgs)-36.)**2) )

          ELSEIF ( iqcinit == 3 ) THEN
              R1=1./(1. + caw*(273.15 - cbw)*qss(mgs)*felvcp(mgs)/ & 
     &             ((temg(mgs) - cbw)**2))
            DCLOUD=R1*(qvap(mgs) - qvs(mgs))  ! KW model adjustment; 
                              ! this will put mass into qc if qv > sqsat exists
          
          ELSEIF ( iqcinit == 2 ) THEN
!              R1=1./(1. + caw*(273.15 - cbw)*qss(mgs)*felv(mgs)/
!     :             (cp*(temg(mgs) - cbw)**2))
!            DCLOUD=R1*(qvap(mgs) - qvs(mgs))  ! KW model adjustment; 
                              ! this will put mass into qc if qv > sqsat exists
         ssmx = ssmxinit

          IF ( ssf(mgs) > ssmx ) THEN
           CALL QVEXCESS(ngs,mgs,qwvp,qv0,qx(1,lc),pres,thetap,theta0,dcloud, & 
     &      pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ssmx,pk,ngscnt)
          ELSE
            dcloud = 0.0
          ENDIF
         ENDIF
        ELSE
            dcloud = 0.0
        ENDIF

        thetap(mgs) = thetap(mgs) + felvcp(mgs)*DCLOUD/(pi0(mgs))
        qwvp(mgs) = qwvp(mgs) - DCLOUD
        qx(mgs,lc) = qx(mgs,lc) + DCLOUD
        IF ( io_flag .and. nxtra > 1 ) THEN
           axtra(igs(mgs),jy,kgs(mgs),1) = DCLOUD/dtp
        ENDIF
        theta(mgs) = thetap(mgs) + theta0(mgs)
        temg(mgs) = theta(mgs)*pk(mgs) !( pres(mgs) / poo ) ** cap
!        temg(mgs) = theta2temp( theta(mgs), pres(mgs) )
        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )
        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
!        es(mgs) = 6.1078e2*tabqvs(ltemq)

!.... S. TWOMEY (1959)
! Note: get here if there is no previous cloud water and w > 0.
      cn(mgs) = 0.0
      
      IF ( ncdebug .ge. 1 ) THEN
        write(iunit,*) 'at 613: ',qx(mgs,lc),cx(mgs,lc),wvel(mgs),ssmax(mgs),kgs(mgs)
      ENDIF
      
      IF (  .not. flag_qndrop ) THEN ! { only calculate mass change when using wrf-chem

      
!      IF ( ssmax(mgs) .lt. sscb .and. qx(mgs,lc) .gt. qxmin(lc)) THEN
      IF ( dcloud .gt. qxmin(lc) .and. wvel(mgs) > 0.0) THEN
!       CN(mgs) =   CCNE*wvel(mgs)**cnexp ! *Min(1.0,1./dtp) ! 0.3465
       CN(mgs) =   CCNE0*cnuc(mgs)**(2./(2.+cck))*wvel(mgs)**cnexp ! *Min(1.0,1./dtp) ! 0.3465
        IF ( ny .le. 2 .and. cn(mgs) .gt. 0.0    &
     &                    .and. ncdebug .ge. 1 ) THEN 
          write(iunit,*) 'CN: ',cn(mgs)*1.e-6, cx(mgs,lc)*1.e-6, qx(mgs,lc)*1.e3,   &
     &       wvel(mgs), dcloud*1.e3
          IF ( cn(mgs) .gt. 1.0 ) write(iunit,*) 'cwrad = ',   &
     &       1.e6*(rho0(mgs)*qx(mgs,lc)/cn(mgs)*cwc1)**c1f3,   &
     &   igs(mgs),kgs(mgs),temcg(mgs),    &
     &   1.e3*an(igs(mgs),jgs,kgs(mgs)-1,lc)
        ENDIF
        IF ( iccwflg .eq. 1 ) THEN
          cn(mgs) = Min(cwccn*rho0(mgs)/rho00, Max(cn(mgs),   &
     &       rho0(mgs)*qx(mgs,lc)/(xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3)))
        ENDIF
      ELSE
       cn(mgs) = 0.0
       dcloud = 0.0
!          cn(mgs) = Min(cwccn,    &
!     &       rho0(mgs)*dcloud/(xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3) )
      ENDIF

      IF ( cn(mgs) .gt. 0.0 ) THEN
       IF ( cn(mgs) .gt. ccnc(mgs) ) THEN
         cn(mgs) = ccnc(mgs)
!         ccnc(mgs) = 0.0
       ENDIF
!      cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
      ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
      ccna(mgs) = ccna(mgs) + cn(mgs)
      ENDIF

!       write(91,*) 'nuc1: cn, ix, kz = ',cn(mgs),igs(mgs),kgs(mgs),wvel(mgs),cnexp,ccnc(mgs)

      IF( CN(mgs) .GT. cx(mgs,lc) ) cx(mgs,lc) = CN(mgs)
      IF( cx(mgs,lc) .GT. 0. .AND. qx(mgs,lc) .le. qxmin(lc) ) THEN
        cx(mgs,lc) = 0.
      ELSE
        cx(mgs,lc) = Min(cx(mgs,lc),rho0(mgs)*Max(0.0,qx(mgs,lc))/cwmasn)
      ENDIF
      
      ENDIF ! }.not. flag_qndrop

        GOTO 613
        
        END IF ! qc .gt. 0.

!        ES=EES(PIB(K)*PT)
!        SQSAT=EPSI*ES/(PB(K)*1000.-ES)

!.... CLOUD NUCLEATION
!      T=PIB(K)*PT
!      ES=1.E3*PB(K)*QV/EPSI

      IF ( wvel(mgs) .le. 0. ) GO TO 616
      IF ( cx(mgs,lc) .le. 0. )  GO TO 613                             !TWOMEY (1959) Nucleation
      IF ( kzbeg-1+kgs(mgs) .GT. 1 .and. qx(mgs,lc) .le. qxmin(lc)) GO TO 613  !TWOMEY (1959) Nucleation
      IF ( kzbeg-1+kgs(mgs) .eq. 1 .and. wvel(mgs) .gt. 0. ) GO TO 613         !TWOMEY (1959) Nucleation
!.... ATTEMPT ZIEGLER CLOUD NUCLEATION IN CLOUD INTERIOR UNLESS...
  616 IF ( ssf(mgs) .LE. SUPCB .AND. wvel(mgs) .GT. 0. ) GO TO 631 !... weakly saturated updraft
      IF ( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 .AND.  &
     &    (ssfkp1(mgs) .GE. SUPMX .OR. &
     &     ssf(mgs)    .GE. SUPMX .OR. &
     &     ssfkm1(mgs) .GE. SUPMX)) GO TO 631                      !... too much vapour
      IF (ssf(mgs) .LT. 1.E-10 .OR. ssf(mgs) .GE. SUPMX) GO TO 631 !... at the extremes for ss

!
! get here if ( qc > 0 and ss > supcb) or (w < 0)
!

      if (ndebug .gt. 0) write(0,*) "ICEZVD_DR: Entered Ziegler Cloud Nucleation" !mpidebug

      DSSDZ=0.
      r2dzm=0.50/dz3d(igs(mgs),jy,kgs(mgs))
      IF ( irenuc >= 0 .and. .not. flag_qndrop) THEN ! turn off nucleation when flag_qndrop (using WRF-CHEM for activation)

      IF ( irenuc /= 2 ) THEN !{

        IF ( kzend == nzend ) THEN
          t0p3 = t0(igs(mgs),jgs,Min(kze,kgs(mgs)+3))
          t0p1 = t0(igs(mgs),jgs,Min(kze,kgs(mgs)+1))
        ELSE
          t0p3 = t0(igs(mgs),jgs,kgs(mgs)+3)
          t0p1 = t0(igs(mgs),jgs,kgs(mgs)+1)
        ENDIF

      IF ( ( ssf(mgs) .gt. ssmax(mgs) .or.  irenuc .eq. 1 ) &
     &   .and.  ( ( lccn .lt. 1 .and.  &
     &            cx(mgs,lc) .lt. cwccn*(Min(1.0,rho0(mgs)))) .or. &
     &    ( lccn .gt. 1 .and. ccnc(mgs) .gt. 0. )   ) &
     &    ) THEN
      IF( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 &
     &  .and. ssf(mgs) .gt. 0.0 &
     &  .and. ssfkp1(mgs) .LT. SUPMX .and. ssfkp1(mgs) .ge. 0.0  &
     &  .AND. ssfkm1(mgs) .LT. SUPMX .AND. ssfkm1(mgs) .ge. 0.0  &
     &  .AND. ssfkp1(mgs) .gt. ssfkm1(mgs)  &
     &  .and. t0p3 .gt. 233.2) THEN
          DSSDZ = (ssfkp1(mgs) - ssfkm1(mgs))*R2DZM
!
! otherwise check for cloud base condition with updraft:
!
        ELSEIF( kzbeg-1+kgs(mgs) .GT. 1 .AND. kzbeg-1+kgs(mgs) .LT. nzend-1 &
!        IF( kgs(mgs) .GT. 1 .AND. kgs(mgs) .LT. NZ-1 &
     &  .and. ssf(mgs) .gt. 0.0  .and. wvel(mgs) .gt. 0.0 &
     &  .and. ssfkp1(mgs) .gt. 0.0   &
     &  .AND. ssfkm1(mgs) .le. 0.0 .and. wvelkm1(mgs) .gt. 0.0 &
     &  .AND. ssf(mgs) .gt. ssfkm1(mgs)  &
     &  .and. t0p1 .gt. 233.2) THEN
         DSSDZ = 2.*(ssf(mgs) - ssfkm1(mgs))*R2DZM  ! 1-sided difference
        ENDIF

       ENDIF
!
!CLZ  IF(wijk.LE.0.) CN=CCN*ssfilt(ix,jy,kz)**CCK
! note: CCN -> cwccn, DELT -> dtp
      c1 = Max(0.0, rho0(mgs)*(qx(mgs,lv) - qss(mgs))/ &
     &        (xdn(mgs,lc)*(4.*pi/3.)*(4.e-6)**3))
      IF ( lccn .lt. 1 ) THEN
       CN(mgs) = cwccn*rho0(mgs)/rho00*CCK*ssf(mgs)**CCKM*dtp*   &
     & Max(0.0,    &
     &         (wvel(mgs)*DSSDZ) )      ! probably the vertical gradient dominates
      ELSE
      CN(mgs) =  &
     &    Min(ccnc(mgs), cnuc(mgs)*CCK*ssf(mgs)**CCKM*dtp*   &
     & Max(0.0,    &
     &         ( wvel(mgs)*DSSDZ) )  )
!      IF ( cn(mgs) .gt. 0 ) ccnc(mgs) = ccnc(mgs) - cn(mgs)
      ENDIF

      IF ( cn(mgs) .gt. 0.0 ) THEN
       IF ( ccnc(mgs) .lt. 5.e7 .and. cn(mgs) .ge. 5.e7 ) THEN
          cn(mgs) = 5.e7
          ccnc(mgs) = 0.0
       ELSEIF ( cn(mgs) .gt. ccnc(mgs) ) THEN
         cn(mgs) = ccnc(mgs)
         ccnc(mgs) = 0.0
       ENDIF
      cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
      ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
      ENDIF

      ELSEIF ( irenuc == 2 ) THEN !} { 
      ! simple Twomey scheme
!      if (ndebug .gt. 0) write(0,*) 'ICEZVD_DR:  Cloud reNucleation, wvel = ',wvel(mgs)
       CN(mgs) =   CCNE0*cnuc(mgs)**(2./(2.+cck))*Max(0.0,wvel(mgs))**cnexp ! *Min(1.0,1./dtp) ! 0.3465
!      ccne = ccnefac*1.e6*(1.e-6*Abs(cwccn))**(2./(2.+cck))
!!!       CN(mgs) = Max( 0.0, CN(mgs) - ccna(mgs) ) ! this was from
               ! Philips, Donner et al. 2007, but results in too much limitation of
               ! nucleation
       CN(mgs) = Min(cn(mgs), ccnc(mgs))
       cn(mgs) = Min(cn(mgs), 0.5*dqc/cwmasn) ! limit the nucleation mass to half of the condensation mass
       
       cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
       
       ccnc(mgs) = Max(0.0, ccnc(mgs) - cn(mgs))
       
      ENDIF ! }

      ccna(mgs) = ccna(mgs) + cn(mgs)

      ENDIF ! irenuc >= 0 .and. .not. flag_qndrop

      IF( cx(mgs,lc) .GT. 0. .AND. qx(mgs,lc) .LE. qxmin(lc)) cx(mgs,lc)=0.
      GO TO 631
!.... NUCLEATION ON CLOUD INFLOW BOUNDARY POINT

  613 CONTINUE

  631  CONTINUE

!
! Check for supersaturation greater than ssmx and adjust down
!
       ssmx = 1.9
       qv1 = qv0(mgs) + qwvp(mgs)
       qvs1 = qvs(mgs)
       
!       IF ( flag_qndrop .and. do_satadj_for_wrfchem ) ssmx = 1.04 ! set lower threshold for progn=1 when using WRF-CHEM

       IF ( qv1 .gt. (ssmx*qvs1) ) THEN
! use line below to disable saturation adjustment when flag_qndrop is true
!       IF ( qv1 .gt. (ssmx*qvs1) .and. .not. flag_qndrop ) THEN
        
         ss1 = qv1/qvs1

        ssmx = 100.*(ssmx - 1.0)
        
        qvex = 0.0

        CALL QVEXCESS(ngs,mgs,qwvp,qv0,qx(1,lc),pres,thetap,theta0,qvex,   &
     &    pi0,tabqvs,nqsat,fqsat,cbw,fcqv1,felvcp,ssmx,pk,ngscnt)



        IF ( qvex .gt. 0.0 ) THEN
        thetap(mgs) = thetap(mgs) + felvcp(mgs)*qvex/(pi0(mgs))
        IF ( io_flag .and. nxtra > 1 ) THEN
           axtra(igs(mgs),jy,kgs(mgs),1) = axtra(igs(mgs),jy,kgs(mgs),1) + qvex/dtp
        ENDIF
        qwvp(mgs) = qwvp(mgs) - qvex
        qx(mgs,lc) = qx(mgs,lc) + qvex
        IF ( .not. flag_qndrop) THEN
        cn(mgs) = Min( Max(ccnc(mgs),cwnccn(mgs)), rho0(mgs)*qvex/Max( cwmasn5, xmas(mgs,lc) )  )
        ccnc(mgs) = Max( 0.0, ccnc(mgs) - cn(mgs) )
        cx(mgs,lc) = cx(mgs,lc) + cn(mgs)
        ENDIF
        
!        write(iunit,*) 'theta = ',theta0(mgs) + thetap(mgs)

!        temg(mgs) = theta(mgs)*( pres(mgs) / poo ) ** cap

        ENDIF

       
       ENDIF

!
! Calculate droplet volume and check if it is within bounds.
!  Adjust if necessary
!  
!      if (ndebug .gt. 0) write(0,*) "ICEZVD_DR: check droplet volume" 


!      cx(mgs,lc) = Min( cwnccn(mgs), cx(mgs,lc) )
      IF( cx(mgs,lc) > cxmin .AND. qx(mgs,lc) .GT. qxmin(lc)) THEN
!        SVC(mgs) = rho0(mgs)*qx(mgs,lc)/(cx(mgs,lc)*xdn(mgs,lc))
        xmas(mgs,lc) = rho0(mgs)*qx(mgs,lc)/(cx(mgs,lc))
        
       IF (  xmas(mgs,lc) < cwmasn .or.  xmas(mgs,lc) > cwmasx ) THEN
        xmas(mgs,lc) = Min( xmas(mgs,lc), cwmasx )
        xmas(mgs,lc) = Max( xmas(mgs,lc), cwmasn )
        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc)
       ENDIF
      ENDIF


!      IF( cx(mgs,lc) .GT. 10.e6 .AND. qx(mgs,lc) .GT. qxmin(lc) ) GO TO 681
!        ccwtmp = cx(mgs,lc)
!        cwmastmp = xmas(mgs,lc)
!       xmas(mgs,lc) = Max(xmas(mgs,lc), cwmasn)
!       IF (qx(mgs,lc) .GT. qxmin(lc) .AND. cx(mgs,lc) .le. 0.) THEN
!          cx(mgs,lc) = Min(0.5*cwccn,rho0(mgs)*qx(mgs,lc)/xmas(mgs,lc))
!          xmas(mgs,lc) = rho0(mgs)*qx(mgs,lc)/cx(mgs,lc)
!       ENDIF
!      IF (cx(mgs,lc) .GT. 0. .AND. qx(mgs,lc) .GT. qxmin(lc))    &
!     &        xmas(mgs,lc) = rho0(mgs)*qx(mgs,lc)/cx(mgs,lc)
!      IF (qx(mgs,lc) .GT. qxmin(lc) .AND. xmas(mgs,lc) .LT. cwmasn)    &
!     &          xmas(mgs,lc) = cwmasn
!      IF (qx(mgs,lc) .GT. qxmin(lc) .AND. xmas(mgs,lc) .GT. cwmasx)    &
!     &    xmas(mgs,lc) = cwmasx
!      IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN
!        cx(mgs,lc) = rho0(mgs)*qx(mgs,lc)/Max(cwmasn,xmas(mgs,lc))
!      ENDIF
!        
!
! 681  CONTINUE

        
      IF ( ipconc .ge. 3 .and. rcond == 2 ) THEN

        
        IF (cx(mgs,lr) .GT. 0. .AND. qx(mgs,lr) .GT. qxmin(lr))    &
     &       xv(mgs,lr)=rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*cx(mgs,lr))
        IF (xv(mgs,lr) .GT. xvmx(lr)) xv(mgs,lr) = xvmx(lr)
        IF (xv(mgs,lr) .LT. xvmn(lr)) xv(mgs,lr) = xvmn(lr)

      ENDIF



      ENDDO ! mgs


! ################################################################
      DO mgs=1,ngscnt
      IF ( ssf(mgs) .gt. ssmax(mgs)    &
     &  .and. ( idecss .eq. 0 .or. qx(mgs,lc) .gt. qxmin(lc)) ) THEN
        ssmax(mgs) = ssf(mgs)
      ENDIF
      ENDDO
!

      do mgs = 1,ngscnt
      an(igs(mgs),jy,kgs(mgs),lt) = theta0(mgs) + thetap(mgs)
      an(igs(mgs),jy,kgs(mgs),lv) =  qv0(mgs) + qwvp(mgs)
!      tmp3d(igs(mgs),jy,kgs(mgs)) = tmp3d(igs(mgs),jy,kgs(mgs)) + t9(igs(mgs),jy,kgs(mgs)) !  pi0(mgs) ! wvdf(mgs) ! ssf(mgs) ! cn(mgs)
!
      IF ( eqtset > 2 ) THEN
        p2(igs(mgs),jy,kgs(mgs)) = pipert(mgs)
      ENDIF

       if ( ido(lc) .eq. 1 )  then
        an(igs(mgs),jy,kgs(mgs),lc) = qx(mgs,lc) +    &
     &    min( an(igs(mgs),jy,kgs(mgs),lc), 0.0 )
!        qx(mgs,lc) = an(igs(mgs),jy,kgs(mgs),lc)
       end if
!

       if ( ido(lr) .eq. 1 .and. rcond == 2 )  then
        an(igs(mgs),jy,kgs(mgs),lr) = qx(mgs,lr) +    &
     &    min( an(igs(mgs),jy,kgs(mgs),lr), 0.0 )
!        qx(mgs,lr) = an(igs(mgs),jy,kgs(mgs),lr)
       end if



       IF (  ipconc .ge. 2 ) THEN
        an(igs(mgs),jy,kgs(mgs),lnc) = Max(cx(mgs,lc) , 0.0)
        IF ( lccn .gt. 1 ) THEN
          an(igs(mgs),jy,kgs(mgs),lccn) = Max(0.0,  ccnc(mgs) )
        ENDIF
       ENDIF
       IF (  ipconc .ge. 3 .and. rcond == 2 ) THEN
        an(igs(mgs),jy,kgs(mgs),lnr) = Max(cx(mgs,lr) , 0.0)
       ENDIF
      end do


29998 continue


      if ( kz .gt. nz-1 .and. ix .ge. nxi) then
        if ( ix .ge. nxi ) then
         go to 2200 ! exit gather scatter
        else
         nzmpb = kz
        endif
      else
        nzmpb = kz
      end if

      if ( ix .ge. nxi ) then
        nxmpb = 1
        nzmpb = kz+1
      else
       nxmpb = ix+1
      end if

 2000 continue ! inumgs
 2200 continue
!
!  end of gather scatter (for this jy slice)


!#ifdef COMMAS
!    GOTO 9999
!#endif

! Redistribute inappreciable cloud particles and charge
!
! Redistribution everywhere in the domain...
!
      frac = 1.0 ! 0.25 ! 1.0 ! 0.2
!
!  alternate test version for ipconc .ge. 3
!  just vaporize stuff to prevent noise in the number concentrations


      do kz = 1,nz
!      do jy = 1,1
      do ix = 1,nxi
      
      zerocx(:) = .false.
      DO il = lc,lhab
       IF ( iresetmoments == 1 .or. iresetmoments == il ) THEN
        IF ( ln(il) > 1 ) zerocx(il) = ( an(ix,jy,kz,ln(il)) < cxmin )
        IF ( lz(il) > 1 ) zerocx(il) = ( zerocx(il) .or. an(ix,jy,kz,lz(il)) < zxmin )
       ELSE
        IF ( il == lc ) THEN
          IF ( ln(il) > 1 ) zerocx(il) = ( an(ix,jy,kz,ln(il)) <= 0 ) .and. .not. flag_qndrop ! don't reset if progn=1 (WRF-CHEM)
        ELSE
         IF ( ln(il) > 1 ) zerocx(il) = ( an(ix,jy,kz,ln(il)) <= 0 )
        ENDIF
       ENDIF
      ENDDO

      IF ( lhl .gt. 1 ) THEN
      
      
      if ( an(ix,jy,kz,lhl) .lt. frac*qxmin(lhl) .or. zerocx(lhl) ) then

!        IF ( an(ix,jy,kz,lhl) .gt. 0 ) THEN
          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lhl)
          an(ix,jy,kz,lhl) = 0.0
!        ENDIF

        IF ( ipconc .ge. 5 ) THEN ! .and. an(ix,jy,kz,lnh) .gt. 0.0 ) THEN
          an(ix,jy,kz,lnhl) = 0.0
        ENDIF

        IF ( lvhl .gt. 1 ) THEN
           an(ix,jy,kz,lvhl) = 0.0
        ENDIF

        IF ( lhlw .gt. 1 ) THEN
           an(ix,jy,kz,lhlw) = 0.0
        ENDIF
      
        IF ( lzhl .gt. 1 ) THEN
           an(ix,jy,kz,lzhl) = 0.0
        ENDIF

      ELSE
       IF ( lvol(lhl) .gt. 1 ) THEN  ! check density
        IF ( an(ix,jy,kz,lvhl) .gt. 0.0 ) THEN
         tmp = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
        ELSE ! in case volume is zero but mass is above threshold (should not happen, of course)
          tmp = rho_qhl
          an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
        ENDIF

        IF (  tmp .lt. xdnmn(lhl) ) THEN
          tmp = Max( xdnmn(lhl), tmp )
          an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
        ENDIF

        IF ( tmp .gt. xdnmx(lhl) .and. lhlw .le. 0 ) THEN ! no liquid allowed on hail
          tmp = Min( xdnmx(lhl), tmp )
          an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
        ELSEIF ( tmp .gt. xdnmx(lhl) .and. lhlw .gt. 1 ) THEN  ! allow for liquid on hail
          fw = an(ix,jy,kz,lhlw)/an(ix,jy,kz,lhl)
!          tmpmx = xdnmx(lhl) + fw*(xdnmx(lr) - xdnmx(lhl)) ! maximum possible average density
                                                           ! it is not exactly linear, but approx. is close enough for this
!          tmpmx = 1./( (1. - fw)/900. + fw/1000. ) is exact max, where 900 is xdnmx

          tmpmx = xdnmx(lhl)/( 1. - fw*(1. - xdnmx(lhl)/xdnmx(lr) )) 

          IF ( tmp .gt. tmpmx  ) THEN
            an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmpmx
          ENDIF

!          IF ( tmp .gt. xdnmx(lhl) .and. an(ix,jy,kz,lhlw) .lt. qxmin(lhl) ) THEN
!            tmp = Min( xdnmx(lhl), tmp )
!            an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
!          ELSEIF ( tmp .gt. xdnmx(lr) ) THEN
!            tmp =  xdnmx(lr)
!            an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
!          ENDIF
        ENDIF

        IF ( lhlw .gt. 1 ) THEN ! check if basically pure water
          IF ( an(ix,jy,kz,lhlw) .gt. 0.98*an(ix,jy,kz,lhl) ) THEN
           tmp = xdnmx(lr)
           an(ix,jy,kz,lvhl) = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/tmp
          ENDIF
        ENDIF
        
       ENDIF
       
       
!  CHECK INTERCEPT
       IF ( ipconc == 5 .and.  an(ix,jy,kz,lhl) .gt. qxmin(lhl) .and.  alphahl .le. 0.1 .and. lnhl .gt. 1 .and. lzhl == 0 ) THEN
       
         IF ( lvhl .gt. 1 ) THEN
           hwdn = dn(ix,jy,kz)*an(ix,jy,kz,lhl)/an(ix,jy,kz,lvhl)
         ELSE
           hwdn = xdn0(lhl)
         ENDIF
           tmp = (hwdn*an(ix,jy,kz,lnhl))/(dn(ix,jy,kz)*an(ix,jy,kz,lhl))
           tmpg = an(ix,jy,kz,lnhl)*(tmp*(3.14159))**(1./3.)
           IF ( tmpg .lt. cnohlmn ) THEN
             tmp = ( (hwdn)/(dn(ix,jy,kz)*an(ix,jy,kz,lhl))*(3.14159))**(1./3.)
              an(ix,jy,kz,lnhl) = (cnohlmn/tmp)**(3./4.)
           ENDIF
       
       ENDIF
!      ELSE  ! check mean size here?

      end if



      ENDIF !lhl


      if ( an(ix,jy,kz,lh) .lt. frac*qxmin(lh) .or. zerocx(lh) ) then

!        IF ( an(ix,jy,kz,lh) .gt. 0 ) THEN
          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lh)
          an(ix,jy,kz,lh) = 0.0
!        ENDIF

        IF ( ipconc .ge. 5 ) THEN ! .and. an(ix,jy,kz,lnh) .gt. 0.0 ) THEN
          an(ix,jy,kz,lnh) = 0.0
        ENDIF

        IF ( lvh .gt. 1 ) THEN
           an(ix,jy,kz,lvh) = 0.0
        ENDIF
      
        IF ( lhw .gt. 1 ) THEN
           an(ix,jy,kz,lhw) = 0.0
        ENDIF
      
        IF ( lzh .gt. 1 ) THEN
           an(ix,jy,kz,lzh) = 0.0
        ENDIF

      ELSE
       IF ( lvol(lh) .gt. 1 ) THEN  ! check density
        IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
         tmp = dn(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
        ELSE
         tmp = rho_qh
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ENDIF

        IF (  tmp .lt. xdnmn(lh) ) THEN
          tmp = Max( xdnmn(lh), tmp )
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ENDIF

        IF ( tmp .gt. xdnmx(lh) .and. lhw .le. 0 ) THEN ! no liquid allowed on graupel
          tmp = Min( xdnmx(lh), tmp )
          an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
        ELSEIF ( tmp .gt. xdnmx(lh) .and. lhw .gt. 1 ) THEN  ! allow for liquid on graupel
          fw = an(ix,jy,kz,lhw)/an(ix,jy,kz,lh)
!          tmpmx = xdnmx(lh) + fw*(xdnmx(lr) - xdnmx(lh)) ! maximum possible average density
                                                           ! it is not exactly linear, but approx. is close enough for this
!          tmpmx = 1./( (1. - fw)/900. + fw/1000. ) is exact max, where 900 is xdnmx
          tmpmx = xdnmx(lh)/( 1. - fw*(1. - xdnmx(lh)/xdnmx(lr) )) 

          IF ( tmp .gt. tmpmx  ) THEN
            an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmpmx
          ENDIF

!          IF ( tmp .gt. xdnmx(lh) .and. an(ix,jy,kz,lhw) .lt. qxmin(lh) ) THEN
!            tmp = Min( xdnmx(lh), tmp )
!            an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
!          ELSEIF ( tmp .gt. xdnmx(lr) ) THEN
!            tmp =  xdnmx(lr)
!            an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
!          ENDIF

        ENDIF

        IF ( lhw .gt. 1 ) THEN ! check if basically pure water
          IF ( an(ix,jy,kz,lhw) .gt. 0.98*an(ix,jy,kz,lh) ) THEN
           tmp = xdnmx(lr)
           an(ix,jy,kz,lvh) = dn(ix,jy,kz)*an(ix,jy,kz,lh)/tmp
          ENDIF
        ENDIF
        
       ENDIF

!  CHECK INTERCEPT
       IF ( ipconc == 5 .and.  an(ix,jy,kz,lh) .gt. qxmin(lh) .and.  alphah .le. 0.1 .and. lnh .gt. 1 .and. lzh == 0 ) THEN
       
         IF ( lvh .gt. 1 ) THEN
           IF ( an(ix,jy,kz,lvh) .gt. 0.0 ) THEN
             hwdn = dn(ix,jy,kz)*an(ix,jy,kz,lh)/an(ix,jy,kz,lvh)
           ELSE
             hwdn = xdn0(lh)
           ENDIF
           hwdn = Max( xdnmn(lh), hwdn )
         ELSE
           hwdn = xdn0(lh)
         ENDIF
           tmp = (hwdn*an(ix,jy,kz,lnh))/(dn(ix,jy,kz)*an(ix,jy,kz,lh))
           tmpg = an(ix,jy,kz,lnh)*(tmp*(3.14159))**(1./3.)
           IF ( tmpg .lt. cnohmn ) THEN
!           tmpg = an(ix,jy,kz,lnh)*( (hwdn*an(ix,jy,kz,lnh))/(dn(ix,jy,kz)*an(ix,jy,kz,lh))*(3.14159))**(1./3.)
!           tmpg = an(ix,jy,kz,lnh)**(4./3.)*( (hwdn)/(dn(ix,jy,kz)*an(ix,jy,kz,lh))*(3.14159))**(1./3.)
             tmp = ( (hwdn)/(dn(ix,jy,kz)*an(ix,jy,kz,lh))*(3.14159))**(1./3.)
              an(ix,jy,kz,lnh) = (cnohmn/tmp)**(3./4.)
           ENDIF
       
       ENDIF
        
      end if


      if ( an(ix,jy,kz,ls) .lt.  frac*qxmin(ls)  .or. zerocx(ls)  & ! .or.  an(ix,jy,kz,lns) .lt. 0.1 ! .and.
     &         ) then
      IF ( t0(ix,jy,kz) .lt. 273.15 ) THEN
!        IF ( an(ix,jy,kz,ls) .gt. 0 ) THEN
          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,ls)
          an(ix,jy,kz,ls) = 0.0
!        ENDIF
      
        IF ( ipconc .ge. 4 ) THEN ! .and. an(ix,jy,kz,lns) .gt. 0.0  ) THEN ! 
!          an(ix,jy,kz,lni) = an(ix,jy,kz,lni) + an(ix,jy,kz,lns)
          an(ix,jy,kz,lns) = 0.0
        ENDIF
        
        IF ( lvs .gt. 1 ) THEN
           an(ix,jy,kz,lvs) = 0.0
        ENDIF

        IF ( lsw .gt. 1 ) THEN
           an(ix,jy,kz,lsw) = 0.0
        ENDIF

      ELSE
!        IF ( an(ix,jy,kz,ls) .gt. 0 ) THEN
          an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,ls)
          an(ix,jy,kz,ls) = 0.0
!        ENDIF

        IF ( lvs .gt. 1 ) THEN
           an(ix,jy,kz,lvs) = 0.0
        ENDIF

        IF ( lsw .gt. 1 ) THEN
           an(ix,jy,kz,lsw) = 0.0
        ENDIF

        IF ( ipconc .ge. 4 ) THEN ! .and. an(ix,jy,kz,lns) .gt. 0.0  ) THEN ! 
!          an(ix,jy,kz,lnr) = an(ix,jy,kz,lnr) + an(ix,jy,kz,lns)
          an(ix,jy,kz,lns) = 0.0
        ENDIF

      ENDIF
      

      ELSEIF ( lvol(ls) .gt. 1 ) THEN  ! check density
        IF ( an(ix,jy,kz,lvs) .gt. 0.0 ) THEN
          tmp = dn(ix,jy,kz)*an(ix,jy,kz,ls)/an(ix,jy,kz,lvs)
          IF ( tmp .gt. xdnmx(ls) .or. tmp .lt. xdnmn(ls) ) THEN
            tmp = Min( xdnmx(ls), Max( xdnmn(ls), tmp ) )
            an(ix,jy,kz,lvs) = dn(ix,jy,kz)*an(ix,jy,kz,ls)/tmp
          ENDIF
        ELSE
          tmp = rho_qs
          an(ix,jy,kz,lvs) = dn(ix,jy,kz)*an(ix,jy,kz,ls)/tmp
        ENDIF


      end if


      if ( an(ix,jy,kz,lr) .lt. frac*qxmin(lr)  .or. zerocx(lr)  &
     &  ) then
        an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lr)
        an(ix,jy,kz,lr) = 0.0
        IF ( ipconc .ge. 3 ) THEN
!          an(ix,jy,kz,lnc) = an(ix,jy,kz,lnc) + an(ix,jy,kz,lnr)
          an(ix,jy,kz,lnr) = 0.0
        ENDIF
        
      end if

!
!  for qci
!
      IF ( an(ix,jy,kz,li) .le. frac*qxmin(li) .or. zerocx(li)   & ! .or.  an(ix,jy,kz,lni) .lt. 0.1
     &    ) THEN
      an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,li)
      an(ix,jy,kz,li)= 0.0
       IF ( ipconc .ge. 1 ) THEN
         an(ix,jy,kz,lni) = 0.0
       ENDIF
      ENDIF

!
!  for qis
!
      IF ( lis > 1 ) THEN
      IF ( an(ix,jy,kz,lis) .le. frac*qxmin(lis) .or. zerocx(lis)   & ! .or.  an(ix,jy,kz,lni) .lt. 0.1
     &    ) THEN
      an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lis)
      an(ix,jy,kz,lis)= 0.0
       IF ( ipconc .ge. 1 ) THEN
         an(ix,jy,kz,lnis) = 0.0
       ENDIF
      ENDIF
      ENDIF

!
!  for qcw
!

      IF ( an(ix,jy,kz,lc) .le. frac*qxmin(lc) .or. zerocx(lc)   &
     &       ) THEN
      an(ix,jy,kz,lv) = an(ix,jy,kz,lv) + an(ix,jy,kz,lc)
      an(ix,jy,kz,lc)= 0.0
       IF ( ipconc .ge. 2 ) THEN
        IF ( lccn .gt. 1 ) THEN
         an(ix,jy,kz,lccn) =     &
     &       an(ix,jy,kz,lccn) + Max(0.0,an(ix,jy,kz,lnc))
        ENDIF
         an(ix,jy,kz,lnc) = 0.0

       ENDIF

      ENDIF

      end do
!      end do
      end do
      
      
      IF ( ndebug .ge. 1 ) write(6,*) 'END OF ICEZVD_DR'
!
!
   
   
   9999 RETURN
   
   END SUBROUTINE NUCOND


! #####################################################################
! #####################################################################


!
! Things to do:
!
!  Test using exponential formulation for rain fall speed.  If there is little change
!  from the quadratic, it would be less complicated to use.
!
!  Contact nucleation needs to be fixed up to be similar to Cotton et al. 1986 and Meyers et al 1992.
!
! The following are done?
!
!  Fix Rain evaporation for gamma function (ipconc >= 3)
!
!  convert cloud ice to snow as in Ferrier 1994 (change only mass in cloud ice),
!    then can try turning off direct conversion from cloud ice to graupel and rimed ice
!
!  look at an iterative check on overdepletion;  need to be careful with two-moment
!
!  check ice supersaturation in two-moment.  Getting enough deposition, or need 
!      to do sat adj. when cloud droplets are all gone?
!
!  
!
! new comment
!
! Fix use of gt for SWM IN FALLOUT ROUTINES
!
!  How to remove hl for ipconc=5?  Need to preprocess?
!
!   When the charging rates are moved to a subroutine, need to move the
!   call to be after the wet growth calculations -- or at least the 
!   splashing stuff.  Think about this....
!
!  Think about what to do with cracif
!
!    Replace qv0 with qx(mgs,lv)? No. qv0 is base val
!
! Need to look at limiting supersaturation to 1 or so by nucleation/condensation
!
!  put in temperature-dependent function for homogeneous freezing
!
!c--------------------------------------------------------------------------
!
!
!--------------------------------------------------------------------------
!

      subroutine nssl_2mom_gs   &
     &  (nx,ny,nz,na,jyslab  &
     &  ,nor,norz          &
     &  ,dtp,gz       &
     &  ,t0,t1,t2,t3,t4,t5,t6,t7,t8,t9      &
     &  ,an,dn,p2                  &
     &  ,pn,w,iunit                   &
     &  ,t00,t77,                             &
     &   ventr,ventc,c1sw,jgs,ido,    &
     &   xdnmx,xdnmn,               &
!     &   ln,ipc,lvol,lz,lliq,   &
     &   cdx,                              &
     &   xdn0,tmp3d,tkediss  &
     & ,timevtcalc,axtra,io_flag  &
     & ,rainprod2d, evapprod2d &
     & ,elec,its,ids,ide,jds,jde &
     & )


!
!--------------------------------------------------------------------------
!                                
!     Ziegler 1985 parameterized microphysics (also Zrnic et al. 1993)
!     1)  cloud water
!     2)  rain
!     3)  column ice 
!     6)  snow
!     11) graupel/hail
!
!--------------------------------------------------------------------------
!
! Notes:
!
!  4/27/2009: allows for liquid water to be advected on snow and graupel particles using flag "mixedphase"
!
!  3/14/2007: (APS) added qproc temp to make microphysic process timeseries
!
!  10/17/2006: added flag (iehw) to select how to calculate ehw
!
!  10/5/2006: switched chacr to integrated version rather than assuming that average rain
!             drop mass does not change.  This acts to reduce rain size somewhat via graupel
!             collection.
!             Use Mason data for ehw, with scaling toward ehw=1 as air density decreases.
!
!  10/3/2006: Turned off Meyers nucleation for T > -5 (can turn on with imeyers5 flag)
!             Turned off contact nucleation in updrafts
!
!  7/24/2006:  Turned on Meyers nucleation for -5 < T < 0
!
!  5/12/2006:  Converted qsacw/csacw and qsaci/csaci to Z93
!
!  5/12/2006:  Put a threshold on Bigg rain freezing.  If the frozen drops
!              have an average volume less than xvhmn, then the drops are put
!              into snow instead of graupel/hail.
!
!              Fixed bug when vapor deposition was limited.
!
!  5/13/2006:  Note that qhacr has a large effect, but Z85 did not include it.
!              Turned off qsacr (set to zero).
!
!  9/14/2007: erm: recalculate vx(lh) after setting xdn(lh) in case xdn was out of allowed range.
!             added parameter rimc3 for minimum rime density.  Default value set at 170. kg/m**3
!             instead of previous use of 100.  (Farley, 1987)
!
!--------------------------------------------------------------------------
!
!  general declarations
!
!--------------------------------------------------------------------------
!
!
!


      implicit none
!
!      integer icond 
!      parameter ( icond = 2 )

      integer, parameter :: ng1 = 1

      integer nx,ny,nz,na,nba,nv
      integer nor,norz,istag,jstag,kstag ! ,nht,ngt,igsr
      integer iwrite
      real dtp,dx,dy,dz

      logical, intent(in) :: io_flag

      integer itile,jtile,ktile
      integer ixbeg,jybeg
      integer ixend,jyend,kzend,kzbeg
      integer nxend,nyend,nzend,nzbeg
      integer :: my_rank = 0
      integer, parameter :: myprock = 1, nprock = 1
      real rainprod2d(-nor+1:nx+nor,-norz+ng1:nz+norz)
      real evapprod2d(-nor+1:nx+nor,-norz+ng1:nz+norz)


      real tkediss(-nor+1:nx+nor,-norz+ng1:nz+norz)
      real axtra(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz,nxtra)

      real :: galpharaut
      real :: xvbarmax
      
      integer jyslab,its,ids,ide,jds,jde ! domain boundaries
      integer, intent(in) :: iunit !,iunit0
      real qvex
      integer iraincv, icgxconv
      parameter ( iraincv = 1, icgxconv = 1)
      real ffrz

      real qcitmp,cirdiatmp ! ,qiptmp,qirtmp
      real ccwtmp,ccitmp ! ,ciptmp,cirtmp
      real cpqc,cpci ! ,cpip,cpir
      real cpqc0,cpci0 ! ,cpip0,cpir0
      real scfac ! ,cpip1
      
      double precision dp1
      
      double precision frac, frach, xvfrz
      
      double precision :: timevtcalc
      double precision :: dpt1,dpt2
            
      
      logical, parameter :: usegamxinf = .false.
      logical, parameter :: usegamxinf2 = .false.
      logical, parameter :: usegamxinf3 = .false.
!      real rar  ! rime accretion rate as calculated from qxacw


! a few vars for time-split fallout      
      real vtmax
      integer n,ndfall
      
      double precision chgneg,chgpos,sctot
      
      real temgtmp

      real pb(-norz+ng1:nz+norz)
      real pinit(-norz+ng1:nz+norz)

      real gz(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz) ! dz
      
      real qimax,xni0,roqi0


      real dv

      real dtptmp
      integer itest,nidx,id1,jd1,kd1
      parameter (itest=1)
      parameter (nidx=10)
      parameter (id1=1,jd1=1,kd1=1)
      integer ierr
      integer iend

      integer ix,kz, il, ic, ir, icp1, irp1, ip1,jp1,kp1
      integer :: jy
      integer i,j,k,i1
      integer kzb,kze
      real slope1, slope2
      real x1, x2, x3
      real eps,eps2
      parameter (eps=1.e-20,eps2=1.e-5)
!
!  Other elec. vars
!
      real  temele
      real  trev
      
      logical ldovol, ishail, ltest
!
!
!  wind indicies
!
      integer mu,mv,mw
      parameter (mu=1,mv=2,mw=3)
!
!  conversion parameters
!
      integer mqcw,mqxw,mtem,mrho,mtim
      parameter (mqcw=21,mqxw=21,mtem=21,mrho=5,mtim=6)

      real xftim,xftimi,yftim, xftem,yftem, xfqcw,yfqcw, xfqxw,yfqxw
      parameter (xftim=0.05,xftimi = 1./xftim,yftim=1.)
      parameter (xftem=0.5,yftem=1.)
      parameter (xfqcw=2000.,yfqcw=1.)
      parameter (xfqxw=2000.,yfqxw=1.)
      real dtfac
      parameter ( dtfac = 1.0 )
      integer ido(lc:lqmx)

!      integer iexy(lc:lqmx,lc:lqmx)
!      integer ieswi, ieswir, ieswip, ieswc, ieswr
!      integer ieglsw, iegli, ieglir, ieglip, ieglc, ieglr
!      integer iegmsw, iegmi, iegmir, iegmip, iegmc, iegmr
!      integer ieghsw, ieghi, ieghir, ieghip, ieghc, ieghr
!      integer iefwsw, iefwi, iefwir, iefwip, iefwc, iefwr
!      integer iehwsw, iehwi, iehwir, iehwip, iehwc, iehwr
!      integer iehlsw, iehli, iehlir, iehlip, iehlc, iehlr
!      real delqnsa, delqxsa, delqnsb, delqxsb, delqnia, delqxia
!      real delqnra, delqxra

       real delqnxa(lc:lqmx)
       real delqxxa(lc:lqmx)
!
! external temporary arrays
!
      real t00(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t77(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)

      real t0(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t1(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t2(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t3(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t4(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t5(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t6(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t7(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t8(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)
      real t9(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)

      real p2(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)  ! perturbation Pi
      real pn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)
      real an(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz,na)
      real dn(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)
      real w(-nor+1:nx+nor,-nor+1:ny+nor,-norz+ng1:nz+norz)

      real tmp3d(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz)

! 
!  declarations microphyscs and for gather/scatter
!
      integer nxmpb,nzmpb,nxz
      integer jgs,mgs,ngs,numgs
      parameter (ngs=500) !500)
      integer, parameter :: ngsz = 500
      integer ntt
      parameter (ntt=300)

      real dvmgs(ngs)
      
      integer ngscnt,igs(ngs),kgs(ngs)
      integer kgsp(ngs),kgsm(ngs),kgsm2(ngs)
      integer ncuse
      parameter (ncuse=0)
      integer il0(ngs),il5(ngs),il2(ngs),il3(ngs)
!      integer il1m(ngs),il2m(ngs),il3m(ngs),il4m(ngs),il5m(ngs)
!
      real tdtol,temsav,tfrcbw,tfrcbi
      real, parameter :: thnuc = 235.15
!
!  Ice Multiplication Arrays.
!
      real  fimt1(ngs),fimta(ngs),fimt2(ngs) !,qmul1(ngs),qmul2(ngs)
      real xcwmas
!
!
! Variables for Ziegler warm rain microphysics
!      


      real ccnc(ngs),ccin(ngs),cina(ngs),ccna(ngs)
      real cwnccn(ngs)
      real sscb  ! 'cloud base' SS threshold
      parameter ( sscb = 2.0 )
      integer idecss  ! flag to turn on (=1) decay of ssmax when no cloud or ice crystals
      parameter ( idecss = 1 )
      integer iba ! flag to do condensation/nucleation in 1st or 2nd loop
                  ! =0 to use ad to calculate SS
                  ! =1 to use an at end of main jy loop to calculate SS
      parameter (iba = 1)
      integer ifilt   ! =1 to filter ssat, =0 to set ssfilt=ssat
      parameter ( ifilt = 0 ) 
      real temp1,temp2 ! ,ssold
      real :: mwat, mice, dice, mwshed, fwmax, fw, mwcrit, massfactor, tmpdiam
      real, parameter :: shedalp = 3.  ! set 3 for maximum mass diameter (same as area-weighted diameter), 4 for mass-weighted diameter
      real ssmax(ngs)       ! maximum SS experienced by a parcel
      real ssmx
      real dnnet,dqnet
!      real cnu,rnu,snu,cinu
!      parameter ( cnu = 0.0, rnu = -0.8, snu = -0.8, cinu = 0.0 )
      real bfnu, bfnu0, bfnu1
      parameter ( bfnu0 = (rnu + 2.0)/(rnu + 1.0)  )
      real ventr, ventc
      real volb, aa1, aa2
      double precision t2s, xdp
      double precision xl2p(ngs),rb(ngs)
      parameter ( aa1 = 9.44e15, aa2 = 5.78e3 ) ! a1 in Ziegler
! snow parameters:
      real cexs, cecs
      parameter ( cexs = 0.1, cecs = 0.5 )
      real rvt      ! ratio of collection kernels (Zrnic et al, 1993)
      parameter ( rvt = 0.104 )
      real kfrag    ! rate coefficent for collisional splintering (Schuur & Rutledge 00b)
      parameter ( kfrag = 1.0e-6 )
      real mfrag    ! assumed ice fragment mass for collisional splintering (Schuur & Rutledge 00b)
      parameter ( mfrag = 1.0e-10)
      double precision cautn(ngs), rh(ngs), nh(ngs)
      real ex1, ft, rhoinv(ngs)
      double precision ec0(ngs)
      
      real ac1,bc, taus, c1,d1,e1,f1,p380,tmp,tmp1,tmp2,tmp3,tmp4,tmp5 ! , sstdy, super
      real ratio, delx, dely
      real dbigg,volt
      real chgtmp,fac
      real x,y,y2,del,r,rtmp,alpr
      double precision :: vent1,vent2
      real g1palp,g4palp
      real fqt !charge separation as fn of temperature from Dong and Hallett 1992
      real bs
      real v1, v2
      real d1r, d1i, d1s, e1i
      real c1sw   ! integration factor for snow melting with snu = -0.8
      real, parameter :: vr1mm = 5.23599e-10 ! volume of 1mm diameter sphere (m**3)
      real, parameter :: vr3mm = 5.23599e-10*(3.0/1.)**3   ! volume of a 3 mm diameter sphere (m**3) (Rasmussen et al. 1984b, JAS)
      real, parameter :: vr4p5mm = 5.23599e-10*(4.5/1.)**3 ! volume of 4.5mm diameter sphere (m**3) (Rasmussen et al. 1984b, JAS)
      real vmlt,vshd, vshdgs(ngs,lh:lhab), maxmassfac(lc:lhab)
      real rhosm
      parameter ( rhosm = 500. )
      integer nc ! condensation step
      real dtcon,dtcon1,dtcon2 ! condensation time step (dtcon*nc = dtp)
      real delta
      integer ltemq1,ltemq1m ! ,ltemq1m2
      real dqv,qv1,ss1,ss2,qvs1,dqvs,dtemp,dt1   ! temporaries for condensation
      real ssi1, ssi2, dqvi, dqvis, dqvii,qis1
      real dqvr, dqc, dqr, dqi, dqs
      real qv1m,qvs1m,ss1m,ssi1m,qis1m
      real cwmastmp
      real  dcloud,dcloud2 ! ,as, bs
      real cn(ngs)
      double precision xvc, xvr
      real mwfac
!      real  es(ngs) ! ss(ngs),
!      real  eis(ngs)

      real rwmasn,rwmasx

      real vgra,vfrz
      parameter ( vgra = 0.523599*(1.0e-3)**3 )
     
      real epsi,d
      parameter (epsi = 0.622, d = 0.266)
      real r1,qevap ! ,slv
      
      real vr,nrx,chw,g1,qr,z,z1,rdi,alp,xnutmp,xnuc,g1r,rd1,rdia
      
      real, parameter :: rhofrz = 900.   ! density of graupel from newly-frozen rain
      real, parameter :: rimedens = 500. ! default rime density

!      real svc(ngs)  !  droplet volume
!
!  contact freezing nucleation
!
      real raero,kaero !assumd aerosol radius, thermal conductivity
      parameter ( raero = 3.e-7, kaero = 5.39e-3 )
      real kb   ! Boltzman constant  J K-1
      parameter (kb = 1.3807e-23)
      
      real knud(ngs),knuda(ngs) !knudsen number and correction factor
      real gtp(ngs)  !G(T,p) = 1/(a' + b')  Cotton 72b
      real dfar(ngs) !aerosol diffusivity
      real fn1(ngs),fn2(ngs),fnft(ngs)
      
      real ccia(ngs)
      real ctfzbd(ngs),ctfzth(ngs),ctfzdi(ngs)
!
!  misc
!
      real ni,nr,d0
      real dqvcnd(ngs),dqwv(ngs),dqcw(ngs),dqci(ngs)
      real tempc(ngs)
      real temg(ngs),temcg(ngs),theta(ngs),qvap(ngs) 
      real temgkm1(ngs), temgkm2(ngs)
      real temgx(ngs),temcgx(ngs)
      real qvs(ngs),qis(ngs),qss(ngs),pqs(ngs)
      real elv(ngs),elf(ngs),els(ngs)
      real tsqr(ngs),ssi(ngs),ssw(ngs)
      real qcwtmp(ngs),qtmp,qtot(ngs) 
      real qcond(ngs)
      real ctmp, sctmp
      real cimasn,cimasx,ccimx
      real pid4
      real cs,ds,gf7,gf6,gf5,gf4,gf3,gf2,gf1
      real gf73rds, gf83rds
      real gamice73fac, gamsnow73fac
      real gf43rds, gf53rds
      real gamma
      real aradcw,bradcw,cradcw,dradcw,cwrad,rwrad,rwradmn
      parameter ( rwradmn = 50.e-6 )
      real dh0
      
      real clionpmx,clionnmx
      parameter (clionpmx=1.e9,clionnmx=1.e9) ! Takahashi 84
!
!  other arrays

      real fwet1(ngs),fwet2(ngs)   
      real fmlt1(ngs),fmlt2(ngs)  
      real fvds(ngs),fvce(ngs),fiinit(ngs) 
      real fvent(ngs),fraci(ngs),fracl(ngs)
!
      real fai(ngs),fav(ngs),fbi(ngs),fbv(ngs)
      real felv(ngs),fels(ngs),felf(ngs)
      real felvcp(ngs),felscp(ngs),felfcp(ngs)
      real felvpi(ngs),felspi(ngs),felfpi(ngs)
      real felvs(ngs),felss(ngs)      !   ,felfs(ngs)
      real fwvdf(ngs),ftka(ngs),fthdf(ngs)
      real fadvisc(ngs),fakvisc(ngs)
      real fci(ngs),fcw(ngs)
      real fschm(ngs),fpndl(ngs)
      real fgamw(ngs),fgams(ngs)
      real fcqv1(ngs),fcqv2(ngs),fcc3(ngs) 
      
      real cvm,cpm,rmm

      real, parameter :: rovcp = rd/cp
      real, parameter ::      cpv = 1885.0       ! specific heat of water vapor at constant pressure
!
      real fcci(ngs), fcip(ngs)
!
      real :: sfm1(ngs),sfm2(ngs)
      real :: gfm1(ngs),gfm2(ngs)
      real :: hfm1(ngs),hfm2(ngs)

      logical :: wetsfc(ngs),wetsfchl(ngs)
      logical :: wetgrowth(ngs), wetgrowthhl(ngs)

       real qitmp(ngs),qistmp(ngs)
       
      real rzxh(ngs), rzxhl(ngs), rzxhlh(ngs)
      real rzxs(ngs)
      real axh(ngs),bxh(ngs),axhl(ngs),bxhl(ngs)
      real vt2ave(ngs)

      real ::  qx(ngs,lv:lhab)
      real ::  qxw(ngs,ls:lhab)
      real ::  cx(ngs,lc:lhab)
      real ::  cxmxd(ngs,lc:lhab)
      real ::  qxmxd(ngs,lv:lhab)
      real ::  scx(ngs,lc:lhab)
      real ::  xv(ngs,lc:lhab)
      real ::  vtxbar(ngs,lc:lhab,3)
      real ::  xmas(ngs,lc:lhab)
      real ::  xdn(ngs,lc:lhab)
      real ::  xdia(ngs,lc:lhab,3)
      real ::  rarx(ngs,ls:lhab)
      real ::  vx(ngs,li:lhab)
      real ::  rimdn(ngs,li:lhab)
      real ::  raindn(ngs,li:lhab)
      real ::  alpha(ngs,lc:lhab)
      real ::  dab0lh(ngs,lc:lhab,lr:lhab)
      real ::  dab1lh(ngs,lc:lhab,lr:lhab)
      
      real :: galphrout
      
      real ventrx(ngs)
      real ventrxn(ngs)
      real g1shr, alphashr
      real g1mlr, alphamlr
      real massfacshr, massfacmlr
      
      real :: qhgt8mm ! ice mass greater than 8mm
      real :: qhgt10mm ! mass greater than 10mm
      real :: qhgt20mm ! mass greater than 20mm
      real :: fwmhtmp 
      real, parameter :: d1t = (6.0 * 0.268e-3/(917.* pi))**(1./3.)
      real, parameter :: srasheym = 0.1389
!
      real swvent(ngs),hwvent(ngs),rwvent(ngs),hlvent(ngs),hwventy(ngs),hlventy(ngs)
      integer, parameter :: ndiam = 10
      integer :: numdiam
      real hwvent0(ndiam+3),hlvent0 ! 0 to d1
      real hwvent1,hlvent1 ! d1 to infinity
      real hwvent2,hlvent2 ! d2 to infinity
      real gama0,gamb0
      real gama1,gamb1
      real gama2,gamb2
      real, parameter :: mltdiam1 = 9.0e-3, mltdiam2 = 19.0e-3, mltdiam3 = 200.0e-3, mltdiam05 = 4.5e-3
      real :: mltdiam(ndiam+3)
      real mltmass1inv,mltmass2inv, mltmass1cgs, mltmass2cgs
      real qhmlr0, qhmlr05, qhmlr1, qhmlr2, qhmlr12
      real qhlmlr0, qhlmlr05, qhlmlr1, qhlmlr2, qhlmlr12
      real qxd1, cxd1 ! mass and number up to mltdiam1
      real qxd05, cxd05 ! mass and number up to mltdiam1/2
      
      real :: qxd(ndiam+3), cxd(ndiam+3), qhml(ndiam+3), qhml0(ndiam+3)
      real :: dqxd(ndiam+3), dcxd(ndiam+3), dqhml(ndiam+3)
      
      
      real civent(ngs)
      real isvent(ngs)
!
      real xmascw(ngs)
      real xdnmx(lc:lhab), xdnmn(lc:lhab)
      real dnmx
      real :: xdiamxmas(ngs,lc:lhab)
!
      real cilen(ngs) ! ,ciplen(ngs)
!
!
      real rwcap(ngs),swcap(ngs)
      real hwcap(ngs)
      real hlcap(ngs)
      real cicap(ngs)
      real iscap(ngs)

      real qvimxd(ngs)
      real qimxd(ngs),qismxd(ngs),qcmxd(ngs),qrmxd(ngs),qsmxd(ngs),qhmxd(ngs),qhlmxd(ngs)
      real cimxd(ngs),ccmxd(ngs),crmxd(ngs),csmxd(ngs),chmxd(ngs)
      real cionpmxd(ngs),cionnmxd(ngs)
      real clionpmxd(ngs),clionnmxd(ngs)


      real elec(-nor+ng1:nx+nor,-nor+ng1:ny+nor,-norz+ng1:nz+norz) ! Ez (elecsave)

!
!
      ! Hallett-Mossop arrays
      real chmul1(ngs),chlmul1(ngs),csmul1(ngs),csmul(ngs)
      real qhmul1(ngs),qhlmul1(ngs),qsmul1(ngs),qsmul(ngs)
      
      ! splinters from drop freezing
      real csplinter(ngs),qsplinter(ngs)
      real csplinter2(ngs),qsplinter2(ngs)
!
!
!  concentration arrays...
!
      real :: chlcnh(ngs), vhlcnh(ngs), vhlcnhl(ngs)
      real cracif(ngs), ciacrf(ngs)
      real cracr(ngs)

!
      real ciint(ngs), crfrz(ngs), crfrzf(ngs), crfrzs(ngs)
      real cicint(ngs)
      real cipint(ngs)
      real ciacw(ngs), cwacii(ngs) 
      real ciacr(ngs), craci(ngs)
      real csacw(ngs)
      real csacr(ngs)
      real csaci(ngs),   csacs(ngs)
      real cracw(ngs) 
      real chacw(ngs), chacr(ngs)
      real :: chlacw(ngs) ! = 0.0
      real chaci(ngs), chacs(ngs)
!
      real :: chlacr(ngs)
      real :: chlaci(ngs), chlacs(ngs)
      real crcnw(ngs) 
      real cidpv(ngs),cisbv(ngs)
      real cisdpv(ngs),cissbv(ngs)
      real cimlr(ngs),cismlr(ngs)

      real chlsbv(ngs), chldpv(ngs)
      real chlmlr(ngs), chlmlrr(ngs) 
      real chlshr(ngs), chlshrr(ngs)

      real chdpv(ngs),chsbv(ngs)
      real chmlr(ngs),chcev(ngs)
      real chmlrr(ngs)
      real chshr(ngs), chshrr(ngs)

      real csdpv(ngs),cssbv(ngs)
      real csmlr(ngs),cscev(ngs)
      real csshr(ngs)

      real crcev(ngs)
      real crshr(ngs)
!
!
! arrays for w-ac-x ;  x-ac-w
!
!
!
      real qrcnw(ngs), qwcnr(ngs)
      real zrcnw(ngs),zracr(ngs),zracw(ngs),zrcev(ngs)


      real qracw(ngs) ! qwacr(ngs),
      real qiacw(ngs) !, qwaci(ngs)

      real qsacw(ngs) ! ,qwacs(ngs),
      real qhacw(ngs) ! qwach(ngs),
      real :: qhlacw(ngs) ! = 0.0
      real vhacw(ngs), vsacw(ngs), vhlacw(ngs), vhlacr(ngs)

!
      real qsacws(ngs)

!
!  arrays for x-ac-r and r-ac-x; 
!
      real qsacr(ngs),qracs(ngs)
      real qhacr(ngs),qhacrmlr(ngs) ! ,qrach(ngs)
      real vhacr(ngs), zhacr(ngs), zhacrf(ngs), zrach(ngs), zrachl(ngs)
      real qiacr(ngs),qraci(ngs)
      
      real ziacr(ngs)

      real qracif(ngs),qiacrf(ngs),qiacrs(ngs),ciacrs(ngs)

      real :: qhlacr(ngs),qhlacrmlr(ngs) ! = 0.0
      real qsacrs(ngs) !,qracss(ngs)
!
!  ice - ice interactions
!
      real qsaci(ngs)
      real qhaci(ngs)
      real qhacs(ngs)

      real :: csaci0(ngs) ! collision rate only
      real :: chaci0(ngs) ! collision rate only
      real :: chacs0(ngs) ! collision rate only
      real :: chlaci0(ngs) ! = 0.0
      real :: chlacs0(ngs) ! = 0.0

      real :: qsaci0(ngs) ! collision rate only
      real :: qhaci0(ngs) ! collision rate only
      real :: qhacs0(ngs) ! collision rate only
      real :: qhlaci0(ngs) ! = 0.0
      real :: qhlacs0(ngs) ! = 0.0

      real :: qhlaci(ngs) ! = 0.0
      real :: qhlacs(ngs) ! = 0.0
!
!  conversions
!
      real qrfrz(ngs) ! , qirirhr(ngs)
      real zrfrz(ngs), zrfrzf(ngs), zrfrzs(ngs)
      real ziacrf(ngs), zhcnsh(ngs), zhcnih(ngs)
      real zhacw(ngs), zhacs(ngs), zhaci(ngs)
      real zhmlr(ngs), zhdsv(ngs), zhsbv(ngs), zhlcnh(ngs), zhshr(ngs)
      real zhmlrtmp,zhmlr0inf,zhlmlr0inf
      real zhmlrr(ngs),zhlmlrr(ngs),zhshrr(ngs),zhlshrr(ngs)
      real zsmlr(ngs), zsmlrr(ngs), zsshr(ngs)
      real zhcns(ngs), zhcni(ngs)
      real zhwdn(ngs) ! change in Z due to density changes
      real zhldn(ngs) ! change in Z due to density changes

      real zhlacw(ngs), zhlacs(ngs), zhlacr(ngs)
      real zhlmlr(ngs), zhldsv(ngs), zhlsbv(ngs), zhlshr(ngs)

      
      real vrfrzf(ngs), viacrf(ngs)
      real qrfrzs(ngs), qrfrzf(ngs)
      real qwfrz(ngs), qwctfz(ngs)
      real cwfrz(ngs), cwctfz(ngs)
      real qwfrzis(ngs), qwctfzis(ngs) ! droplet freezing to ice spheres
      real cwfrzis(ngs), cwctfzis(ngs)
      real qwfrzc(ngs), qwctfzc(ngs) ! droplet freezing to columns
      real cwfrzc(ngs), cwctfzc(ngs)
      real qwfrzp(ngs), qwctfzp(ngs) ! droplet freezing to plates
      real cwfrzp(ngs), cwctfzp(ngs)
      real xcolmn(ngs), xplate(ngs)
      real ciihr(ngs), qiihr(ngs)
      real cicichr(ngs), qicichr(ngs)
      real cipiphr(ngs), qipiphr(ngs)
      real qscni(ngs), cscni(ngs), cscnis(ngs)
      real qscnvi(ngs), cscnvi(ngs), cscnvis(ngs)
      real qhcns(ngs), chcns(ngs), chcnsh(ngs), vhcns(ngs)
      real qscnh(ngs), cscnh(ngs), vscnh(ngs)
      real qhcni(ngs), chcni(ngs), chcnih(ngs), vhcni(ngs)
      real qiint(ngs),qipipnt(ngs),qicicnt(ngs)
      real cninm(ngs),cnina(ngs),cninp(ngs),wvel(ngs),wvelkm1(ngs)
      real tke(ngs)
      real uvel(ngs),vvel(ngs)
!
      real qidpv(ngs),qisbv(ngs) ! qicnv(ngs),qievv(ngs),
      real qimlr(ngs),qidsv(ngs),qisdsv(ngs),qidsvp(ngs) ! ,qicev(ngs)
      real qismlr(ngs)

!
      real qfdpv(ngs),qfsbv(ngs) ! qfcnv(ngs),qfevv(ngs),
      real qfmlr(ngs),qfdsv(ngs) ! ,qfcev(ngs)
      real qfwet(ngs),qfdry(ngs),qfshr(ngs)
      real qfshrp(ngs)
!
      real :: qhldpv(ngs), qhlsbv(ngs) ! qhlcnv(ngs),qhlevv(ngs),
      real :: qhlmlr(ngs), qhldsv(ngs) 
      real :: qhlwet(ngs), qhldry(ngs), qhlshr(ngs) 
!
      real :: qrfz(ngs),qsfz(ngs),qhfz(ngs),qhlfz(ngs)
!
      real qhdpv(ngs),qhsbv(ngs) ! qhcnv(ngs),qhevv(ngs),
      real qhmlr(ngs),qhdsv(ngs),qhcev(ngs),qhcndv(ngs),qhevv(ngs)
      real qhmlrlg(ngs),qhlmlrlg(ngs) ! melting from the larger diameters
      real qhlcev(ngs), chlcev(ngs)
      real qhwet(ngs),qhdry(ngs),qhshr(ngs)
      real qhshrp(ngs)
      real qhshh(ngs) !accreted water that remains on graupel
      real qhmlh(ngs) !melt water that remains on graupel
      real qhfzh(ngs) !water that freezes on mixed-phase graupel
      real qhlfzhl(ngs) !water that freezes on mixed-phase hail

      real vhfzh(ngs) ! change in volume from water that freezes on mixed-phase graupel
      real vhlfzhl(ngs) !  change in volume from water that freezes on mixed-phase hail

      real vhshdr(ngs) !accreted water that leaves on graupel (mixedphase)
      real vhlshdr(ngs) !accreted water that leaves on hail (mixedphase)
      real vhmlr(ngs) !melt water that leaves graupel (single phase)
      real vhlmlr(ngs) !melt water that leaves hail (single phase)
      real vhsoak(ngs) !  aquired water that seeps into graupel.
      real vhlsoak(ngs) !  aquired water that seeps into hail.
!
      real qsdpv(ngs),qssbv(ngs) ! qscnv(ngs),qsevv(ngs),
      real qsmlr(ngs),qsdsv(ngs),qscev(ngs),qscndv(ngs),qsevv(ngs)
      real qswet(ngs),qsdry(ngs),qsshr(ngs)
      real qsshrp(ngs)
      real qsfzs(ngs)
!
!
      real qipdpv(ngs),qipsbv(ngs)
      real qipmlr(ngs),qipdsv(ngs)
!
      real qirdpv(ngs),qirsbv(ngs)
      real qirmlr(ngs),qirdsv(ngs),qirmlw(ngs)
!
      real qgldpv(ngs),qglsbv(ngs)
      real qglmlr(ngs),qgldsv(ngs)
      real qglwet(ngs),qgldry(ngs),qglshr(ngs)
      real qglshrp(ngs)
!
      real qgmdpv(ngs),qgmsbv(ngs)
      real qgmmlr(ngs),qgmdsv(ngs)
      real qgmwet(ngs),qgmdry(ngs),qgmshr(ngs)
      real qgmshrp(ngs)
      real qghdpv(ngs),qghsbv(ngs)
      real qghmlr(ngs),qghdsv(ngs) 
      real qghwet(ngs),qghdry(ngs),qghshr(ngs)
      real qghshrp(ngs)
!
      real qrztot(ngs),qrzmax(ngs),qrzfac(ngs)
      real qrcev(ngs)
      real qrshr(ngs)
      real fsw(ngs),fhw(ngs),fhlw(ngs) !liquid water fractions
      real qhcnf(ngs) 
      real :: qhlcnh(ngs) ! = 0.0
      real qhcngh(ngs),qhcngm(ngs),qhcngl(ngs)
      
      real :: qhcnhl(ngs), chcnhl(ngs), zhcnhl(ngs), vhcnhl(ngs) ! conversion of low-density hail back to graupel

      real eiw(ngs),eii(ngs),eiri(ngs),eipir(ngs),eisw(ngs)
      real erw(ngs),esw(ngs),eglw(ngs),eghw(ngs),efw(ngs)
      real ehxw(ngs),ehlw(ngs),egmw(ngs),ehw(ngs)
      real err(ngs),esr(ngs),eglr(ngs),eghr(ngs),efr(ngs)
      real ehxr(ngs),ehlr(ngs),egmr(ngs) 
      real eri(ngs),esi(ngs),egli(ngs),eghi(ngs),efi(ngs)
      real ehxi(ngs),ehli(ngs),egmi(ngs),ehi(ngs) 
      real ers(ngs),ess(ngs),egls(ngs),eghs(ngs),efs(ngs),ehs(ngs)
      real ehscnv(ngs)
      real ehxs(ngs),ehls(ngs),egms(ngs),egmip(ngs) 

      real ehsclsn(ngs),ehiclsn(ngs)
      real ehlsclsn(ngs),ehliclsn(ngs)
      real esiclsn(ngs)

      real :: ehs_collsn = 0.5, ehi_collsn = 1.0
      real :: ehls_collsn = 1.0, ehli_collsn = 1.0
      real :: esi_collsn = 1.0
      
      real ew(8,6)
      real cwr(8,2)  ! radius and inverse of interval
      data cwr / 2.0, 3.0, 4.0, 6.0,  8.0,  10.0, 15.0,  20.0 , & ! radius
     &           1.0, 1.0, 0.5, 0.5,  0.5,   0.2,  0.2,  1.  /   ! inverse of interval
      integer icwr(ngs), igwr(ngs), irwr(ngs), ihlr(ngs)
      real grad(6,2) ! graupel radius and inverse of interval
      data grad / 100., 200., 300., 400., 600., 1000.,   &
     &            1.e-2,1.e-2,1.e-2,5.e-3,2.5e-3, 1.    /
!droplet radius: 2     3     4     6     8    10    15    20
      data ew /0.03, 0.07, 0.17, 0.41, 0.58, 0.69, 0.82, 0.88,  & ! 100
!     :         0.07, 0.13, 0.27, 0.48, 0.65, 0.73, 0.84, 0.91,  ! 150
     &         0.10, 0.20, 0.34, 0.58, 0.70, 0.78, 0.88, 0.92,  & ! 200
     &         0.15, 0.31, 0.44, 0.65, 0.75, 0.83, 0.96, 0.91,  & ! 300
     &         0.17, 0.37, 0.50, 0.70, 0.81, 0.87, 0.93, 0.96,  & ! 400
     &         0.17, 0.40, 0.54, 0.71, 0.83, 0.88, 0.94, 0.98,  & ! 600
     &         0.15, 0.37, 0.52, 0.74, 0.82, 0.88, 0.94, 0.98 / ! 1000
!     :         0.11, 0.34, 0.49, 0.71, 0.83, 0.88, 0.94, 0.95 / ! 1400


      real da0lr(ngs)
      real da0lh(ngs)
      real da0lhl(ngs)
      
      real va0 (lc:lqmx)          ! collection coefficients from Seifert 2005
      real vab0(lc:lqmx,lc:lqmx)  ! collection coefficients from Seifert 2005
      real vab1(lc:lqmx,lc:lqmx)  ! collection coefficients from Seifert 2005
      real va1 (lc:lqmx)          ! collection coefficients from Seifert 2005
      real ehip(ngs),ehlip(ngs),ehlir(ngs)
      real erir(ngs),esir(ngs),eglir(ngs),egmir(ngs),eghir(ngs)
      real efir(ngs),ehir(ngs),eirw(ngs),eirir(ngs),ehr(ngs)
      real erip(ngs),esip(ngs),eglip(ngs),eghip(ngs)
      real efip(ngs),eipi(ngs),eipw(ngs),eipip(ngs)
!
!  arrays for production terms
!
      real ptotal(ngs) ! , pqtot(ngs)
!
      real pqcwi(ngs),pqcii(ngs),pqrwi(ngs),pqisi(ngs)
      real pqswi(ngs),pqhwi(ngs),pqwvi(ngs)
      real pqgli(ngs),pqghi(ngs),pqfwi(ngs)
      real pqgmi(ngs),pqhli(ngs) ! ,pqhxi(ngs)
      real pqiri(ngs),pqipi(ngs) ! pqwai(ngs),
      real pqlwsi(ngs),pqlwhi(ngs),pqlwhli(ngs)
      
      real pvhwi(ngs), pvhwd(ngs)
      real pvhli(ngs), pvhld(ngs)
      real pvswi(ngs), pvswd(ngs)
!
      real pqcwd(ngs),pqcid(ngs),pqrwd(ngs),pqisd(ngs)
      real pqswd(ngs),pqhwd(ngs),pqwvd(ngs)
      real pqgld(ngs),pqghd(ngs),pqfwd(ngs)
      real pqgmd(ngs),pqhld(ngs) ! ,pqhxd(ngs)
      real pqird(ngs),pqipd(ngs) ! pqwad(ngs),
      real pqlwsd(ngs),pqlwhd(ngs),pqlwhld(ngs)
!
!      real pqxii(ngs,nhab),pqxid(ngs,nhab)
!
      real  pctot(ngs)
      real  pcipi(ngs), pcipd(ngs)
      real  pciri(ngs), pcird(ngs)
      real  pccwi(ngs), pccwd(ngs)
      real  pccii(ngs), pccid(ngs)
      real  pcisi(ngs), pcisd(ngs)
      real  pccin(ngs)
      real  pcrwi(ngs), pcrwd(ngs)
      real  pcswi(ngs), pcswd(ngs)
      real  pchwi(ngs), pchwd(ngs)
      real  pchli(ngs), pchld(ngs)
      real  pcfwi(ngs), pcfwd(ngs)
      real  pcgli(ngs), pcgld(ngs)
      real  pcgmi(ngs), pcgmd(ngs)
      real  pcghi(ngs), pcghd(ngs)

      real  pzrwi(ngs), pzrwd(ngs)
      real  pzhwi(ngs), pzhwd(ngs)
      real  pzhli(ngs), pzhld(ngs)
      real  pzswi(ngs), pzswd(ngs)

!
!  other arrays
!
      real dqisdt(ngs) !,advisc(ngs) !dqwsdt(ngs), ,schm(ngs),pndl(ngs)

      real qss0(ngs)

      real qsacip(ngs)
      real pres(ngs),pipert(ngs)
      real pk(ngs)
      real rho0(ngs),pi0(ngs)
      real rhovt(ngs),sqrtrhovt
      real thetap(ngs),theta0(ngs),qwvp(ngs),qv0(ngs)
      real thsave(ngs)
      real ptwfzi(ngs),ptimlw(ngs)
      real psub(ngs),pvap(ngs),pfrz(ngs),ptem(ngs),pmlt(ngs),pevap(ngs),pdep(ngs)
      
      real cnostmp(ngs)   ! for diagnosed snow intercept
!
!  iholef = 1 to do hole filling technique version 1
!  which uses all hydrometerors to do hole filling of all hydrometeors
!  iholef = 2 to do hole filling technique version 2
!  which uses an individual hydrometeror species to do hole
!  filling of a species of a hydrometeor
!
!  iholen = interval that hole filling is done
!
      integer  iholef
      integer  iholen
      parameter (iholef = 1)
      parameter (iholen = 1)
      real  cqtotn,cqtotn1
      real  cctotn
      real  citotn
      real  crtotn
      real  cstotn
      real  cvtotn
      real  cftotn
      real  cgltotn
      real  cghtotn
      real  chtotn
      real  cqtotp,cqtotp1
      real  cctotp
      real  citotp
      real  ciptotp
      real  crtotp
      real  cstotp
      real  cvtotp
      real  cftotp
      real  chltotp
      real  cgltotp
      real  cgmtotp
      real  cghtotp
      real  chtotp
      real  cqfac
      real  ccfac
      real  cifac
      real  cipfac
      real  crfac
      real  csfac
      real  cvfac
      real  cffac
      real  cglfac
      real  cghfac
      real  chfac
      
      real ssifac, qvapor
!
!   Miscellaneous variables
!
      integer ireadqf,lrho,lqsw,lqgl,lqgm ,lqgh 
      integer lqrw
      real vt
      real arg  ! gamma is a function
      real erbnd1, fdgt1, costhe1
      real qeps
      real dyi2,dzi2,cp608,bta1,cnit,dragh,dnz00,pii
      real qccrit,gf4br,gf4ds,gf4p5, gf3ds, gf1ds,gr

      
      real xdn0(lc:lhab)
      real xdn_new,drhodt
      
      integer l ,ltemq,inumgs, idelq

      real brz,arz,temq

      real ssival,tqvcon
      real cdx(lc:lhab)
      real cnox
      real cval,aval,eval,fval,gval ,qsign,ftelwc,qconkq
      real qconm,qconn,cfce15,gf8,gf4i,gf3p5,gf1a,gf1p5,qdiff,argrcnw
      real c4,bradp,bl2,bt2,dtrh,hrifac, hdia0,hdia1,civenta,civentb
      real civentc,civentd,civente,civentf,civentg,cireyn,xcivent
      real cipventa,cipventb,cipventc,cipventd,cipreyn,cirventa
      real cirventb
      integer igmrwa,igmrwb,igmswa, igmswb,igmfwa,igmfwb,igmhwa,igmhwb
      real rwventa ,rwventb,swventa,swventb,fwventa,fwventb,fwventc
      real hwventa,hwventb
      real    hwventc, hlventa, hlventb,  hlventc
      real  glventa, glventb, glventc
      real   gmventa, gmventb,  gmventc, ghventa, ghventb, ghventc
      real  dzfacp,  dzfacm,  cmassin,  cwdiar 
      real  rimmas, rhobar
      real   argtim, argqcw, argqxw, argtem
      real   frcswsw, frcswgl, frcswgm, frcswgh, frcswfw, frcswsw1
      real   frcglgl, frcglgm, frcglgh,  frcglfw, frcglgl1
      real   frcgmgl, frcgmgm, frcgmgh,  frcgmfw, frcgmgm1
      real   frcghgl, frcghgm, frcghgh,  frcghfw,  frcghgh1
      real   frcfwgl, frcfwgm, frcfwgh, frcfwfw,  frcfwfw1
      real   frcswrsw, frcswrgl,  frcswrgm,  frcswrgh, frcswrfw
      real   frcswrsw1
      real   frcrswsw, frcrswgl, frcrswgm, frcrswgh, frcrswfw
      real  frcrswsw1
      real  frcglrgl, frcglrgm, frcglrgh,  frcglrfw, frcglrgl1
      real  frcrglgl
      real  frcrglgm,  frcrglgh, frcrglfw, frcrglgl1
      real  frcgmrgl, frcgmrgm, frcgmrgh, frcgmrfw,  frcgmrgm1
      real  frcrgmgl, frcrgmgm,  frcrgmgh, frcrgmfw, frcrgmgm1
      real  sum,  qweps,  gf2a, gf4a, dqldt, dqidt, dqdt
      real frcghrgl, frcghrgm, frcghrgh, frcghrfw, frcghrgh1, frcrghgl
      real frcrghgm, frcrghgh,  frcrghfw, frcrghgh1
      real    a1,a2,a3,a4,a5,a6
      real   gamss
      real cdw, cdi, denom1, denom2, delqci1, delqip1
      real cirtotn,  ciptotn, cgmtotn, chltotn,  cirtotp
      real  cgmfac, chlfac,  cirfac
      integer igmhla, igmhlb, igmgla, igmglb, igmgma,  igmgmb
      integer igmgha, igmghb
      integer idqis, item, itim0 
      integer  iqgl, iqgm, iqgh, iqrw, iqsw 
      integer  itertd, ia
      
      integer :: infdo
      
      real tau, ewtmp
      
      integer cntnic_noliq
      real     q_noliqmn, q_noliqmx
      real     scsacimn, scsacimx
      
      real :: dtpinv
      
!   arrays for temporary bin space

      real :: qhmlrtmp,qhmlrtmp2, chmlrtmp, chmlrtmpd1inf, chlmlrtmp, zhlmlrtmp, zhlmlrrtmp, qvs0,tmpcmlt

      real :: term1,term2,term3,term4
      real :: qaacw ! combined qsacw-qhacw for WSM6 variation



!
! ####################################################################
!
!  Start routine
!
! ####################################################################



!

       pb(:) = 0.0
       pinit(:) = 0.0
      itile = nx
      jtile = ny
      ktile = nz
      ixend = nx
      jyend = ny
      kzend = nz
      nxend = nx + 1
      nyend = ny + 1
      nzend = nz
      kzbeg = 1
      nzbeg = 1

      istag = 0
      jstag = 0
      kstag = 1


!
!  slope intercepts
!

      IF ( ngs .lt. nz ) THEN
!       write(0,*) 'Error in ICEZVD: Must have ngs .ge. nz!'
!       STOP
      ENDIF

      cntnic_noliq = 0
      q_noliqmn = 0.0
      q_noliqmx = 0.0
      scsacimn = 0.0
      scsacimx = 0.0

      ldovol = .false.

      DO il = lc,lhab
        ldovol = ldovol .or. ( lvol(il) .gt. 1 )
      ENDDO


!      DO il = lc,lhab
!        write(iunit,*) 'delqnxa(',il,') = ',delqnxa(il)
!      ENDDO
      
!
!  density maximums and minimums
!

!
!  Set terminal velocities...
!    also set drag coefficients
!

      dtpinv = 1.d0/dtp

!

!
!  electricity constants
!
!  mixing ratio epsilon
!
      qeps  = 1.0e-20

!  rebound efficiency (erbnd)
!
!
!
!  constants
!

      cp608 = 0.608
      aradcw = -0.27544
      bradcw = 0.26249e+06
      cradcw = -1.8896e+10
      dradcw = 4.4626e+14
      bta1 = 0.6
      cnit = 1.0e-02
      dragh = 0.60
      dnz00 = 1.225
!      cs = 4.83607122
!      ds = 0.25
!  new values for  cs and ds
      cs = 12.42
      ds = 0.42
      pii = piinv ! 1./pi
      pid4 = pi/4.0 
!      qscrit = 6.0e-04
      gf1 = 1.0 ! gamma(1.0)
      gf1p5 = 0.8862269255  ! gamma(1.5)
      gf2 = 1.0 ! gamma(2.0)
      gf3 = 2.0 ! gamma(3.0)
      gf3p5 = 3.32335097 ! gamma(3.5)
      gf4 = 6.00 ! gamma(4.0)
      gf5 = 24.0 ! gamma(5.0)
      gf6 = 120.0 ! gamma(6.0)
      gf7 = 720.0 ! gamma(7.0)
      gf4br = 17.837861981813607 ! gamma(4.0+br)
      gf4ds = 10.41688578110938 ! gamma(4.0+ds)
      gf4p5 = 11.63172839656745 ! gamma(4.0+0.5)
      gf3ds = 3.0458730354120997 ! gamma(3.0+ds)
      gf1ds = 0.8863557896089221 ! gamma(1.0+ds)
      gr = 9.8
      gf43rds = 0.8929795116 ! gamma(4./3.)
      gf53rds = 0.9027452930 ! gamma(5./3.)
      gf73rds = 1.190639349 ! gamma(7./3.)
      gf83rds = 1.504575488 ! gamma(8./3.)
      
      gamice73fac =  (gamma(7./3. + cinu))**3/ (gamma(1. + cinu)**3 * (1. + cinu)**4)
      gamsnow73fac =  (gamma(7./3. + snu))**3/ (gamma(1. + snu)**3 * (1. + snu)**4)
!
!  constants
!
!
!  general constants for microphysics
!
      brz = 100.0
      arz = 0.66
      
      bfnu1 = (4. + alphar)*(5. + alphar)*(6. + alphar)/ &
     &       ((1. + alphar)*(2. + alphar)*(3. + alphar))

       galpharaut = (6.+alpharaut)*(5.+alpharaut)*(4.+alpharaut)/ &
     &             ((3.+alpharaut)*(2.+alpharaut)*(1.+alpharaut))
      
      vfrz = 0.523599*(dfrz)**3 
      vmlt = Min(xvmx(lr), 0.523599*(dmlt)**3 )
      vshd = Min(xvmx(lr), 0.523599*(dshd)**3 )

     

      tdtol = 1.0e-05
      tfrcbw = tfr - cbw
      tfrcbi = tfr - cbi
!
!
! #ifdef COMMAS
!      print*,'ventr,ventc = ',ventr,ventc

!
!  Set up look up tables for supersaturation w.r.t. liq and ice
!
!VD$L SKIP
!      do l = 1,nqsat
!      temq = 163.15 + (l-1)*fqsat
!      tabqvs(l) = exp(caw*(temq-273.15)/(temq-cbw))
!      tabqis(l) = exp(cai*(temq-273.15)/(temq-cbi))
!      end do

      mltmass1inv = 1.0/( 1000.0*(4.0*pi/3.0)*((0.01*0.5*takshedsize1)**3) ) ! for drops melting from ice with diameter > 1.9cm
      mltmass2inv = 1.0/( 1000.0*(4.0*pi/3.0)*((0.01*0.5*takshedsize2)**3) ) ! for drops melting from ice with 0.9cm < d < 1.9cm
      mltmass1cgs =  1.0*(4.0*pi/3.0)*((0.5*takshedsize1)**3) 
      mltmass2cgs =  1.0*(4.0*pi/3.0)*((0.5*takshedsize2)**3) 
      
!      real, parameter :: mltdiam1 = 9.0e-3, mltdiam2 = 19.0e-3, mltdiam05 = 4.5e-3

      IF ( .false. ) THEN
        numdiam = 1 ! must have numdiam < ndiam because numdiam+1 holds values for the interval of mltdiam(numdiam) to mltdiam(ndiam+1)
        mltdiam(1) = 4.5e-3
      ELSEIF ( .true. ) THEN
        numdiam = 2 ! must have numdiam < ndiam because numdiam+1 holds values for the interval of mltdiam(numdiam) to mltdiam(ndiam+1)
        mltdiam(1) = 1.5e-3
        mltdiam(2) = 4.5e-3
      ELSE
        numdiam = 5 ! must have numdiam < ndiam because numdiam+1 holds values for the interval of mltdiam(numdiam) to mltdiam(ndiam+1)
        mltdiam(1) = 0.5e-3
        mltdiam(2) = 1.0e-3
        mltdiam(3) = 2.0e-3
        mltdiam(4) = 4.0e-3
        mltdiam(5) = 6.0e-3
      ENDIF


      mltdiam(ndiam+1) = mltdiam1 !  9.0e-3
      mltdiam(ndiam+2) = mltdiam2 ! 19.0e-3
      mltdiam(ndiam+3) = mltdiam3 !100.0e-3


!
!  cw constants in mks units
!
!      cwmasn = 4.25e-15  ! radius of 1.0e-6
      mwfac = 6.0**(1./3.)
      IF ( ipconc .ge. 2 ) THEN
!        cwmasn = xvmn(lc)*1000.
!        cwradn = 1.0e-6
!        cwmasx = xvmx(lc)*1000.
      ENDIF
        rwmasn = xvmn(lr)*1000.
        rwmasx = xvmx(lr)*1000.

!
!  ci constants in mks units
!
      cimasn = Min(cimas0, cimas1) ! 12 microns for  0.1871*(xmas(mgs,li)**(0.3429))
      cimasx = 1.0e-8   ! 338 microns
      ccimx = 5000.0e3   ! max of 5000 per liter

!
!  constants for paramerization
!
!
!  set save counter (number of saves):  nsvcnt
!
!      nsvcnt = 0
      iend = 0


!      timetd1 = etime(tarray)
!      timetd1 = tarray(1)

!
!***********************************************************
!  start jy loop
!***********************************************************
!

!      do 9999 jy = 1,ny-jstag
!
!  VERY IMPORTANT:  SET jy = jgs
!
      jy = jgs
     
     
!      t1(:,:,:) = 0
!      t2(:,:,:) = 0
!      t3(:,:,:) = 0
!      t4(:,:,:) = 0
!      t5(:,:,:) = 0
!      t6(:,:,:) = 0
!      t8(:,:,:) = 0
      
      IF ( ipconc < 2 ) THEN ! Make a copy of cloud droplet mixing ratio to use for homogeneous freezing
        DO kz = 1,nz
         DO ix = 1,itile
           t9(ix,jy,kz) = an(ix,jy,kz,lc)
         ENDDO
        ENDDO
      ENDIF
      
!
!..Gather microphysics  
!
      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: ENTER GATHER STAGE'


      
      nxmpb = 1
      nzmpb = 1
      nxz = itile*nz
      numgs = nxz/ngs + 1
!      write(0,*) 'ICEZVD_GS: ENTER GATHER STAGE: nx,nz,nxz,numgs,ngs = ',nx,nz,nxz,numgs,ngs

      do 1000 inumgs = 1,numgs
      ngscnt = 0
      
      do kz = nzmpb,nz
      do ix = nxmpb,itile

      pqs(1) = t00(ix,jy,kz)
!      pqs(kz) = t00(ix,jy,kz)

      theta(1) = an(ix,jy,kz,lt)
      temg(1) = t0(ix,jy,kz)
      temcg(1) = temg(1) - tfr
      tqvcon = temg(1)-cbw
      ltemq = (temg(1)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(1) = pqs(1)*tabqvs(ltemq)
      qis(1) = pqs(1)*tabqis(ltemq)

      qss(1) = qvs(1)

!      IF ( jy .eq. 1 .and. ix .eq. 24 ) THEN
!       write(91,*) 'kz,qv,th: ',kz,an(ix,jy,kz,lv),an(ix,jy,kz,lt),pqs(kz),tabqvs(ltemq),qvs(kz)
!      ENDIF

      if ( temg(1) .lt. tfr ) then
!      if( qcw(kz) .le. qxmin(lc) .and. qci(kz) .gt. qxmin(li))
!     >  qss(kz) = qis(kz)
!      if( qcw(kz) .gt. qxmin(lc) .and. qci(kz) .gt. qxmin(li))
!     >   qss(kz) = (qcw(kz)*qvs(kz) + qci(kz)*qis(kz)) /
!     >   (qcw(kz) + qci(kz))
      qss(1) = qis(1)
      else
!       IF ( an(ix,jy,kz,lv)  .gt. qss(kz) ) THEN
!       write(iunit,*) 'qss exceeded at ',ix,jy,kz,qss(kz),an(ix,jy,kz,lv),temg(kz)
!       write(iunit,*) 'other temg = ',theta(kz)*(pinit(kz)+p2(ix,jy,kz))
!       ENDIF
      end if
!
      ishail = .false.
      IF ( lhl > 1 ) THEN
        IF ( an(ix,jy,kz,lhl)  .gt. qxmin(lhl) ) ishail = .true.
      ENDIF
      
      if ( an(ix,jy,kz,lv)  .gt. qss(1) .or.   &
     &     an(ix,jy,kz,lc)  .gt. qxmin(lc)   .or.    &
     &     an(ix,jy,kz,li)  .gt. qxmin(li)   .or.   &
     &     an(ix,jy,kz,lr)  .gt. qxmin(lr)   .or.   &
     &     an(ix,jy,kz,ls)  .gt. qxmin(ls)   .or.   &
     &     an(ix,jy,kz,lh)  .gt. qxmin(lh)   .or.  ishail ) then
      ngscnt = ngscnt + 1
      igs(ngscnt) = ix
      kgs(ngscnt) = kz
      if ( ngscnt .eq. ngs ) goto 1100
      end if
      enddo !ix
      nxmpb = 1
      enddo !kz
 1100 continue

      if ( ngscnt .eq. 0 ) go to 9998

      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: dbg = 5'

!      write(0,*) 'allocating qc

      
      xv(:,:) = 0.0
      xmas(:,:) = 0.0
      vtxbar(:,:,:) = 0.0
      xdia(:,:,:) = 0.0
      raindn(:,:) = 900.
      cx(:,:) = 0.0
      alpha(:,:) = 0.0
      DO il = li,lhab
        DO mgs = 1,ngscnt
          rimdn(mgs,il)  = rimedens ! xdn0(il)
        ENDDO
      ENDDO
!
!  define temporaries for state variables to be used in calculations
!
      do mgs = 1,ngscnt
      kgsm(mgs) = max(kgs(mgs)-1,1)
      kgsp(mgs) = min(kgs(mgs)+1,nz-1)
      kgsm2(mgs) = Max(kgs(mgs)-2,1)
      theta0(mgs) = an(igs(mgs),jy,kgs(mgs),lt)
      thetap(mgs) = an(igs(mgs),jy,kgs(mgs),lt) - theta0(mgs)
      theta(mgs) = an(igs(mgs),jy,kgs(mgs),lt)
      qv0(mgs) = an(igs(mgs),jy,kgs(mgs),lv)
      qwvp(mgs) = an(igs(mgs),jy,kgs(mgs),lv)  - qv0(mgs) ! qv0(mgs) is full qv, so qwvp starts as zero!

      pres(mgs) = pn(igs(mgs),jy,kgs(mgs)) + pb(kgs(mgs))
      pipert(mgs) = p2(igs(mgs),jy,kgs(mgs))
      rho0(mgs) = dn(igs(mgs),jy,kgs(mgs))
      rhoinv(mgs) = 1.0/rho0(mgs)
      rhovt(mgs) = Sqrt(rho00/rho0(mgs))
      pi0(mgs) = p2(igs(mgs),jy,kgs(mgs)) + pinit(kgs(mgs))
      temg(mgs) = t0(igs(mgs),jy,kgs(mgs))
      temgkm1(mgs) = t0(igs(mgs),jy,kgsm(mgs))
      temgkm2(mgs) = t0(igs(mgs),jy,kgsm2(mgs))
      pk(mgs)   = p2(igs(mgs),jy,kgs(mgs)) + pinit(kgs(mgs)) ! t77(igs(mgs),jy,kgs(mgs))
      temcg(mgs) = temg(mgs) - tfr
      qss0(mgs) = (380.0)/(pres(mgs))
      pqs(mgs) = (380.0)/(pres(mgs))
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)
!      es(mgs)  = 6.1078e2*tabqvs(ltemq)
!      eis(mgs) = 6.1078e2*tabqis(ltemq)
      cnostmp(mgs) = cno(ls)
!

      il5(mgs) = 0
      if ( temg(mgs) .lt. tfr ) then
      il5(mgs) = 1
      end if
      enddo !mgs
      
      IF ( ipconc < 1 .and. lwsm6 ) THEN
        DO mgs = 1,ngscnt
          tmp = Min( 0.0, temcg(mgs) )
          cnostmp(mgs) = Min( 2.e8, 2.e6*exp(0.12*tmp) )
        ENDDO
      ENDIF


!
! zero arrays that are used but not otherwise set (tm)
!
      do mgs = 1,ngscnt
         qhshr(mgs) = 0.0 
       end do
!
!  set temporaries for microphysics variables
!
      DO il = lv,lhab
      do mgs = 1,ngscnt
        qx(mgs,il) = max(an(igs(mgs),jy,kgs(mgs),il), 0.0) 
      ENDDO
      end do

      qxw(:,:) = 0.0



        scx(:,:) = 0.0
!
!  set shape parameters
!
      IF ( imurain == 1 ) THEN
        alpha(:,lr) = alphar
      ELSEIF ( imurain == 3 ) THEN
        alpha(:,lr) = xnu(lr)
      ENDIF
      
      alpha(:,li) = xnu(li)

      IF ( imusnow == 1 ) THEN
        alpha(:,ls) = alphas
      ELSEIF ( imusnow == 3 ) THEN
        alpha(:,ls) = xnu(ls)
      ENDIF
      
      DO il = lc,lhab
      do mgs = 1,ngscnt
        IF ( il .ge. lg ) alpha(mgs,il) = dnu(il)
        DO ic = lr,lhab
        dab0lh(mgs,il,ic) = dab0(ic,il)
        dab1lh(mgs,il,ic) = dab1(ic,il)
        ENDDO
      ENDDO
      end do
      
      
!      DO mgs = 1,ngscnt
        da0lh(:) = da0(lh)
        da0lr(:) = da0(lr)
        IF ( lzh < 1 .or. lzhl < 1 ) THEN
          rzxhlh(:) = rzhl/rz
        ELSEIF ( lzh > 1 .and. lzhl > 1 ) THEN
          rzxhlh(:) = 1.
        ENDIF
        IF ( lzr > 1 ) THEN
          rzxh(:) = 1.
          rzxhl(:) = 1.
        ELSE
          rzxh(:) = rz
          rzxhl(:) = rzhl
        ENDIF
        
        IF ( imurain == 1 .and. imusnow == 3 ) THEN
          rzxs(:) = rzs
        ELSEIF ( imurain == imusnow ) THEN
          rzxs(:) = 1.
        ENDIF
 !     ENDDO
      
      IF ( lhl .gt. 1 ) THEN
      DO mgs = 1,ngscnt
        da0lhl(mgs) = da0(lhl)
      ENDDO
      ENDIF
      
      ventrx(:) = ventr
      ventrxn(:) = ventrn

!
!  set concentrations
!
!      ssmax = 0.0
      
      
      
      if ( ipconc .ge. 1 ) then
       do mgs = 1,ngscnt
        cx(mgs,li) = Max(an(igs(mgs),jy,kgs(mgs),lni), 0.0)
        IF ( lcina .gt. 1 ) THEN
         cina(mgs) = an(igs(mgs),jy,kgs(mgs),lcina)
        ELSE
         cina(mgs) = cx(mgs,li)
        ENDIF
        IF ( lcin > 1 ) THEN
         ccin(mgs) = an(igs(mgs),jy,kgs(mgs),lcin)
        ENDIF
       end do
      end if
      if ( ipconc .ge. 2 ) then
       do mgs = 1,ngscnt
        cx(mgs,lc) = Max(an(igs(mgs),jy,kgs(mgs),lnc), 0.0)
!        cx(mgs,lc) = Min( ccwmx, cx(mgs,lc) )
        IF ( lss > 1 ) THEN
        ssmax(mgs) = an(igs(mgs),jy,kgs(mgs),lss)
        ENDIF
        IF ( lccn .gt. 1 ) THEN
         ccnc(mgs) = an(igs(mgs),jy,kgs(mgs),lccn)
        ELSE
         ccnc(mgs) = 0.0
        ENDIF
        IF ( lccna .gt. 1 ) THEN
         ccna(mgs) = an(igs(mgs),jy,kgs(mgs),lccna)
        ELSE
         ccna(mgs) = cx(mgs,lc)
        ENDIF
       end do
!       ELSE
!       cx(mgs,lc) = Abs(ccn)
      end if
      if ( ipconc .ge. 3 ) then
       do mgs = 1,ngscnt
        cx(mgs,lr) = Max(an(igs(mgs),jy,kgs(mgs),lnr), 0.0)
        IF ( qx(mgs,lr) .le. qxmin(lr) ) THEN
!          cx(mgs,lr) = 0.0
        ELSEIF ( cx(mgs,lr) .eq. 0.0 .and. qx(mgs,lr) .lt. 3.0*qxmin(lr) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lr)
          qx(mgs,lr) = 0.0
        ELSE
          cx(mgs,lr) = Max( 1.e-9, cx(mgs,lr) )
        ENDIF
       end do
      end if
      if ( ipconc .ge. 4 ) then
       do mgs = 1,ngscnt
        cx(mgs,ls) = Max(an(igs(mgs),jy,kgs(mgs),lns), 0.0)
        IF ( qx(mgs,ls) .le. qxmin(ls) ) THEN
!          cx(mgs,ls) = 0.0
        ELSEIF ( cx(mgs,ls) .eq. 0.0 .and. qx(mgs,ls) .lt. 3.0*qxmin(ls) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,ls)
          qx(mgs,ls) = 0.0
        ELSE
          cx(mgs,ls) = Max( 1.e-9, cx(mgs,ls) )

         IF ( ilimit .ge. ipc(ls) ) THEN
            tmp = (xdn0(ls)*cx(mgs,ls))/(rho0(mgs)*qx(mgs,ls))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,ls)*(tmp2)
         IF ( cnox .gt. 3.0*cno(ls) ) THEN
           cx(mgs,ls) = 3.0*cno(ls)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if
      if ( ipconc .ge. 5 ) then
       do mgs = 1,ngscnt

        cx(mgs,lh) = Max(an(igs(mgs),jy,kgs(mgs),lnh), 0.0)
        IF ( qx(mgs,lh) .le. qxmin(lh) ) THEN
!          cx(mgs,lh) = 0.0
        ELSEIF ( cx(mgs,lh) .eq. 0.0 .and. qx(mgs,lh) .lt. 3.0*qxmin(lh) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lh) 
          qx(mgs,lh) = 0.0
        ELSE
          cx(mgs,lh) = Max( 1.e-9, cx(mgs,lh) )
         IF ( ilimit .ge. ipc(lh) ) THEN
            tmp = (xdn0(lh)*cx(mgs,lh))/(rho0(mgs)*qx(mgs,lh))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,lh)*(tmp2)
         IF ( cnox .gt. 3.0*cno(lh) ) THEN
           cx(mgs,lh) = 3.0*cno(lh)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if

      if ( lhl .gt. 1 .and. ipconc .ge. 5 ) then
       do mgs = 1,ngscnt

        cx(mgs,lhl) = Max(an(igs(mgs),jy,kgs(mgs),lnhl), 0.0)
        IF ( qx(mgs,lhl) .le. qxmin(lhl) ) THEN
          cx(mgs,lhl) = 0.0
        ELSEIF ( cx(mgs,lhl) .eq. 0.0 .and. qx(mgs,lhl) .lt. 3.0*qxmin(lhl) ) THEN
          qx(mgs,lv) = qx(mgs,lv) + qx(mgs,lhl) 
          qx(mgs,lhl) = 0.0
        ELSE
          cx(mgs,lhl) = Max( 1.e-9, cx(mgs,lhl) )
         IF ( ilimit .ge. ipc(lhl) ) THEN
            tmp = (xdn0(lhl)*cx(mgs,lhl))/(rho0(mgs)*qx(mgs,lhl))
            tmp2 = (tmp*(3.14159))**(1./3.)
            cnox = cx(mgs,lhl)*(tmp2)
         IF ( cnox .gt. 3.0*cno(lhl) ) THEN
           cx(mgs,lhl) = 3.0*cno(lhl)/tmp2
         ENDIF
         ENDIF
        ENDIF
       end do
      end if

!
! Set mean particle volume
!
      IF ( ldovol ) THEN
      
      vx(:,:) = 0.0
      
       DO il = li,lhab
        
        IF ( lvol(il) .ge. 1 ) THEN
        
          DO mgs = 1,ngscnt
            vx(mgs,il) = Max(an(igs(mgs),jy,kgs(mgs),lvol(il)), 0.0)
          ENDDO

        ENDIF

       ENDDO

      ENDIF





!
!  set factors
!
      do mgs = 1,ngscnt
!
      ssi(mgs) = qx(mgs,lv)/qis(mgs)
      ssw(mgs) = qx(mgs,lv)/qvs(mgs)
!
      tsqr(mgs) = temg(mgs)**2
!
      temgx(mgs) = min(temg(mgs),313.15)
      temgx(mgs) = max(temgx(mgs),233.15)
      felv(mgs) = 2500837.367 * (273.15/temgx(mgs))**((0.167)+(3.67e-4)*temgx(mgs))
!
      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),223.15)
      temcgx(mgs) = temcgx(mgs)-273.15

! felf = latent heat of fusion, fels = LH of sublimation, felv = LH of vaporization
      felf(mgs) = 333690.6098 + (2030.61425)*temcgx(mgs) - (10.46708312)*temcgx(mgs)**2
!
      fels(mgs) = felv(mgs) + felf(mgs)
!
      felvs(mgs) = felv(mgs)*felv(mgs)
      felss(mgs) = fels(mgs)*fels(mgs)
      
        IF ( eqtset <= 1 ) THEN
          felvcp(mgs) = felv(mgs)*cpi
          felscp(mgs) = fels(mgs)*cpi
          felfcp(mgs) = felf(mgs)*cpi
        ELSE
          
          ! equations from appendix in Bryan and Morrison (2012, MWR)
          ! note that rw is Rv in the paper, and rd is R.
          
          tmp = qx(mgs,li)+qx(mgs,ls)+qx(mgs,lh)
          IF ( lhl > 1 ) tmp = tmp + qx(mgs,lhl)
          cvm = cv+cvv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)

          IF ( eqtset == 2 ) THEN ! compact form from treating dT/dt = theta*d(pi)/dt + pi*d(theta)dt and then applied to theta assuming constant pi
          felvcp(mgs) = (felv(mgs)-rw*temg(mgs))/cvm
          felscp(mgs) = (fels(mgs)-rw*temg(mgs))/cvm
          felfcp(mgs) = felf(mgs)/cvm
          
          ELSE
           ! equivalent version that applies separate updates of latent heating to theta and pi, when both are returned.

          cpm = cp+cpv*qx(mgs,lv)+cpl*(qx(mgs,lc)+qx(mgs,lr))   &
                                  +cpigb*(tmp)
          rmm=rd+rw*qx(mgs,lv)
          
          felvcp(mgs) = (felv(mgs)*cv/(cp) - rw*temg(mgs)*(1.0-rovcp*cpm/rmm))/cvm
          felscp(mgs) = (fels(mgs)*cv/(cp) - rw*temg(mgs)*(1.0-rovcp*cpm/rmm))/cvm
          felfcp(mgs) = felf(mgs)*cv/(cp*cvm)

          felvpi(mgs) = pi0(mgs)*rovcp*(felv(mgs)/(temg(mgs)) - rw*cpm/rmm)/cvm
          felspi(mgs) = pi0(mgs)*rovcp*(fels(mgs)/(temg(mgs)) - rw*cpm/rmm)/cvm 
          felfpi(mgs) = pi0(mgs)*rovcp*(felf(mgs)/(cvm*temg(mgs)))
          
          ENDIF

        ENDIF
!
      fgamw(mgs) = felvcp(mgs)/pi0(mgs)
      fgams(mgs) = felscp(mgs)/pi0(mgs)
!
      fcqv1(mgs) = 4098.0258*pi0(mgs)*fgamw(mgs)
      fcqv2(mgs) = 5807.6953*pi0(mgs)*fgams(mgs)
      fcc3(mgs) = felfcp(mgs)/pi0(mgs)
!
!  fwvdf = water vapor diffusivity
      fwvdf(mgs) = (2.11e-05)*((temg(mgs)/tfr)**1.94)*(101325.0/(pres(mgs)))
!
! fadvisc = kinematic viscosity
      fadvisc(mgs) = advisc0*(416.16/(temg(mgs)+120.0))*(temg(mgs)/296.0)**(1.5) ! dynamic visc.
!
      fakvisc(mgs) = fadvisc(mgs)*rhoinv(mgs) ! divide by rho_air to get kinematic visc. (note the 'k' vs. 'd')
!
      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),233.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fci(mgs) = (2.118636 + 0.007371*(temcgx(mgs)))*(1.0e+03)
!
      if ( temg(mgs) .lt. 273.15 ) then
      temcgx(mgs) = min(temg(mgs),273.15)
      temcgx(mgs) = max(temcgx(mgs),233.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fcw(mgs) = 4203.1548  + (1.30572e-2)*((temcgx(mgs)-35.)**2)   &
     &                 + (1.60056e-5)*((temcgx(mgs)-35.)**4)
      end if
      if ( temg(mgs) .ge. 273.15 ) then
      temcgx(mgs) = min(temg(mgs),308.15)
      temcgx(mgs) = max(temcgx(mgs),273.15)
      temcgx(mgs) = temcgx(mgs)-273.15
      fcw(mgs) = 4243.1688  + (3.47104e-1)*(temcgx(mgs)**2)
      end if
!
      ftka(mgs) = tka0*fadvisc(mgs)/advisc1  ! thermal conductivity: proportional to dynamic viscosity
      fthdf(mgs) = ftka(mgs)*cpi*rhoinv(mgs)
!
      fschm(mgs) = (fakvisc(mgs)/fwvdf(mgs))  ! Schmidt number
      fpndl(mgs) = (fakvisc(mgs)/fthdf(mgs))  ! Prandl number (not used)
!
      fai(mgs) = (fels(mgs)**2)/(ftka(mgs)*rw*temg(mgs)**2)
      fbi(mgs) = (1.0/(rho0(mgs)*fwvdf(mgs)*qis(mgs)))
      fav(mgs) = (felv(mgs)**2)/(ftka(mgs)*rw*temg(mgs)**2)
      fbv(mgs) = (1.0/(rho0(mgs)*fwvdf(mgs)*qvs(mgs)))
!
      end do
!
!
!   ice habit fractions
!
!
!
!  Set density
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set density'
!

      do mgs = 1,ngscnt
        xdn(mgs,li) = xdn0(li)
        xdn(mgs,lc) = xdn0(lc)
        xdn(mgs,lr) = xdn0(lr)
        xdn(mgs,ls) = xdn0(ls)
        xdn(mgs,lh) = xdn0(lh)
        IF ( lvol(ls) .gt. 1 ) THEN
         IF ( vx(mgs,ls) .gt. 0.0 .and. qx(mgs,ls) .gt. qxmin(ls) ) THEN
           xdn(mgs,ls) = Min( xdnmx(ls), Max( xdnmn(ls), rho0(mgs)*qx(mgs,ls)/vx(mgs,ls) ) )
         ENDIF
        ENDIF

        IF ( lvol(lh) .gt. 1 ) THEN
         IF ( vx(mgs,lh) .gt. 0.0 .and. qx(mgs,lh) .gt. qxmin(lh) ) THEN
           IF ( mixedphase ) THEN
           ELSE
             dnmx = xdnmx(lh)
           ENDIF
           xdn(mgs,lh) = Min( dnmx, Max( xdnmn(lh), rho0(mgs)*qx(mgs,lh)/vx(mgs,lh) ) )
           vx(mgs,lh) = rho0(mgs)*qx(mgs,lh)/xdn(mgs,lh)
         ENDIF
        ENDIF

        IF ( lhl .gt. 1 ) THEN

          xdn(mgs,lhl) = xdn0(lhl)

          IF ( lvol(lhl) .gt. 1 ) THEN
           IF ( vx(mgs,lhl) .gt. 0.0 .and. qx(mgs,lhl) .gt. qxmin(lhl) ) THEN

           IF ( mixedphase .and. lhlw > 1 ) THEN
           ELSE
             dnmx = xdnmx(lhl)
           ENDIF

             xdn(mgs,lhl) = Min( dnmx, Max( xdnmn(lhl), rho0(mgs)*qx(mgs,lhl)/vx(mgs,lhl) ) )
             vx(mgs,lhl) = rho0(mgs)*qx(mgs,lhl)/xdn(mgs,lhl)
           ENDIF
          ENDIF

        ENDIF

! adjust density for wet snow and graupel (Ferrier 94)
! (aps): for the time being, do not adjust density until we keep track of fully melted snow/graupel
!
!        IF (mixedphase) THEN
          IF (qsdenmod) THEN
           IF(fsw(mgs) .gt. 0.01) THEN
            xdn(mgs,ls) = (1.-fsw(mgs))*rho_qs + fsw(mgs)*rho_qr        !Ferrier: 100./(1.-fsw(mgs))
            IF(fsw(mgs) .eq. 1.) xdn(mgs,ls) = rho_qr   ! fsw = 1 means it's liquid water, yo!
           ENDIF
          ENDIF

          IF (qhdenmod) THEN
!          IF(fhw(mgs) .gt. 0.01) THEN
!           IF(fhw(mgs) .lt. 1.) xdn(mgs,lh) = rho_qh / (1. - fhw(mgs))       !Ferrier: 400./(1.-fsw(mgs))
!           IF(fhw(mgs) .eq. 1.) xdn(mgs,lh) = rho_qr   ! fhw = 1 means it's liquid water, yo!
!          ENDIF
          ENDIF
!        ENDIF

      end do


       IF ( imurain == 3 ) THEN
         IF ( lzr > 1 ) THEN
           alphashr = 0.0
           alphamlr = -2.0/3.0
         ELSE
           alphashr = xnu(lr)
           alphamlr = xnu(lr)
         ENDIF
!         massfacshr = ( (2. + 3.*(1. +alphashr) )/( 3.*(1. + alphashr) ) )**(1./3.) ! this is the diameter factor
!         massfacmlr = ( (2. + 3.*(1. +alphamlr) )/( 3.*(1. + alphamlr) ) )**(1./3.)
         massfacshr = ( (2. + 3.*(1. +alphashr) )**3/( 3.*(1. + alphashr) ) )  ! this is the mass or volume factor
         massfacmlr = ( (2. + 3.*(1. +alphamlr) )**3/( 3.*(1. + alphamlr) ) )
       ELSEIF ( imurain == 1 ) THEN
         IF ( lzr > 1 ) THEN
           alphashr = 4.0
           alphamlr = 4.0
         ELSE
           alphashr = alphar
           alphamlr = alphar
         ENDIF
!         massfacshr = (3.0 + alphashr)*((3.+alphashr)*(2.+alphashr)*(1. + alphashr) )**(-1./3.) ! this is the diameter factor
!         massfacmlr = (3.0 + alphamlr)*((3.+alphamlr)*(2.+alphamlr)*(1. + alphamlr) )**(-1./3.)
         massfacshr = (3.0 + alphashr)**3/((3.+alphashr)*(2.+alphashr)*(1. + alphashr) ) ! this is the mass or volume factor
         massfacmlr = (3.0 + alphamlr)**3/((3.+alphamlr)*(2.+alphamlr)*(1. + alphamlr) )
       ENDIF
       

!
!  set some values for ice nucleation
!
      do mgs = 1,ngscnt
      kp1 = Min(nz, kgs(mgs)+1 )
      wvel(mgs) = (0.5)*(w(igs(mgs),jgs,kp1)   &
     &                  +w(igs(mgs),jgs,kgs(mgs)))

      
        wvelkm1(mgs) = (0.5)*(w(igs(mgs),jgs,kgs(mgs))   &
     &                    +w(igs(mgs),jgs,kgsm(mgs)))
      cninm(mgs) = t7(igs(mgs),jgs,kgsm(mgs))
      cnina(mgs) = t7(igs(mgs),jgs,kgs(mgs))
      cninp(mgs) = t7(igs(mgs),jgs,kgsp(mgs))
      end do

!
!  Set a couple of cloud variables...
!

!      SUBROUTINE setvt(ngscnt,qx,qxmin,cx,rho0,rhovt,xdia,cno,
!     :                 xmas,xdn,xvmn,xvmx,xv,cdx,
!     :                 ipconc,ndebug)
!      SUBROUTINE setvtz(ngscnt,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno, &
!     &                 xmas,vtxbar,xdn,xvmn,xvmx,xv,cdx,            &
!     &                 ipconc1,ndebug1,ngs,nz,kgs,cwnccn,fadvisc,   &
!     &                 cwmasn,cwmasx,cwradn,cnina,cimna,cimxa,      &
!     &                 itype1a,itype2a,temcg,infdo,alpha)


      infdo = 0
      IF ( io_flag .and. nxtra > 1 ) infdo = 1

      call setvtz(ngscnt,qx,qxmin,qxw,cx,rho0,rhovt,xdia,cno,cnostmp,   &
     &                 xmas,vtxbar,xdn,xvmn,xvmx,xv,cdx,   &
     &                 ipconc,ndebug,ngs,nz,kgs,fadvisc,   &
     &                 cwmasn,cwmasx,cwradn,cnina,cimn,cimx,   &
     &                 itype1,itype2,temcg,infdo,alpha,0,axh,bxh,axhl,bxhl)


       IF ( lwsm6 .and. ipconc == 0 ) THEN
         tmp = Max(qxmin(lh), qxmin(ls))
         DO mgs = 1,ngscnt
           sum = qx(mgs,lh) + qx(mgs,ls)
           IF ( sum > tmp ) THEN
             vt2ave(mgs) = (qx(mgs,lh)*vtxbar(mgs,lh,1) + qx(mgs,ls)*vtxbar(mgs,ls,1))/sum
           ELSE
             vt2ave(mgs) = 0.0
           ENDIF
         ENDDO
       ENDIF


!
!  Set number concentrations (need xdia from setvt)
!
      if ( ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set concentration'
      IF ( ipconc .lt. 1 ) THEN
         cina(1:ngscnt) = cx(1:ngscnt,li)
      ENDIF
      if ( ipconc .lt. 5 ) then
      do mgs = 1,ngscnt


      IF ( ipconc .lt. 3 ) THEN
!      cx(mgs,lr) = 0.0
      if ( qx(mgs,lr) .gt. qxmin(lh) )  then
!      cx(mgs,lr) = cno(lr)*xdia(mgs,lr,1)
!      xv(mgs,lr) = rho0(mgs)*qx(mgs,lr)/(xdn(mgs,lr)*cx(mgs,lr))
      end if
      ENDIF

      IF ( ipconc .lt. 4 ) THEN
!      tmp = cx(mgs,ls)
!      cx(mgs,ls) = 0.0
      if ( qx(mgs,ls) .gt. qxmin(ls) )  then
!      cx(mgs,ls) = cno(ls)*xdia(mgs,ls,1)
!      xv(mgs,ls) = rho0(mgs)*qx(mgs,ls)/(xdn(mgs,ls)*cx(mgs,ls))
      end if
      ENDIF ! ( ipconc .lt. 4 )

      IF ( ipconc .lt. 5 ) THEN


!      cx(mgs,lh) = 0.0
      if ( qx(mgs,lh) .gt. qxmin(lh) )  then
!      cx(mgs,lh) = cno(lh)*xdia(mgs,lh,1)
!      xv(mgs,lh) = Max(xvmn(lh), rho0(mgs)*qx(mgs,lh)/(xdn(mgs,lh)*cx(mgs,lh)) )
!      xdia(mgs,lh,3) = (xv(mgs,lh)*6./pi)**(1./3.) 
      end if

      ENDIF ! ( ipconc .lt. 5 )

      end do
      end if
      
      IF ( ipconc .ge. 2 ) THEN
      DO mgs = 1,ngscnt
        rb(mgs) = 0.5*xdia(mgs,lc,1)*((1./(1.+cnu)))**(1./6.)
        xl2p(mgs) = Max(0.0d0, 2.7e-2*xdn(mgs,lc)*cx(mgs,lc)*xv(mgs,lc)*   &
     &           ((0.5e20*rb(mgs)**3*xdia(mgs,lc,1))-0.4) )
        IF ( rb(mgs) .gt. 3.51e-6 ) THEN
!          rh(mgs) = Max( 0.5d0*xdia(mgs,lc,1), 6.3d-4/(1.d6*(rb(mgs) - 3.5d-6)) )
          rh(mgs) = Max( 41.d-6, 6.3d-4/(1.d6*(rb(mgs) - 3.5d-6)) )
        ELSE
          rh(mgs) = 41.d-6
        ENDIF
        IF ( xl2p(mgs) .gt. 0.0 ) THEN
          nh(mgs) = 4.2d9*xl2p(mgs)
        ELSE
          nh(mgs) = 1.e30
        ENDIF
      ENDDO
      ENDIF
      
!
!
!              
!
!  maximum depletion tendency by any one source
!
!
      if( ndebug .ge. 0 ) THEN
!mpi!        write(iunit,*) 'Set depletion max/min1'
      endif
      do mgs = 1,ngscnt
      qvimxd(mgs) = 0.70*(qx(mgs,lv)-qis(mgs))/dtp ! depletion by all vap. dep to ice.
      qvimxd(mgs) = max(qvimxd(mgs), 0.0)
!      qimxd(mgs)  = 0.20*qx(mgs,li)/dtp
!      qcmxd(mgs)  = 0.20*qx(mgs,lc)/dtp
!      qrmxd(mgs)  = 0.20*qx(mgs,lr)/dtp
!      qsmxd(mgs)  = 0.20*qx(mgs,ls)/dtp
!      qhmxd(mgs)  = 0.20*qx(mgs,lh)/dtp

      frac = 0.1d0
      qimxd(mgs)  = frac*qx(mgs,li)/dtp
      qcmxd(mgs)  = frac*qx(mgs,lc)/dtp
      qrmxd(mgs)  = frac*qx(mgs,lr)/dtp
      qsmxd(mgs)  = frac*qx(mgs,ls)/dtp
      qhmxd(mgs)  = frac*qx(mgs,lh)/dtp
      IF ( lhl > 1 ) qhlmxd(mgs)  = frac*qx(mgs,lhl)/dtp
      end do
!
      if( ndebug .ge. 0 ) THEN
!mpi!        write(0,*) 'Set depletion max/min2'
      endif

      do mgs = 1,ngscnt
!
      if ( qx(mgs,lc) .le. qxmin(lc) ) then
      ccmxd(mgs)  = 0.20*cx(mgs,lc)/dtp
      else
      IF ( ipconc .ge. 2 ) THEN
        ccmxd(mgs)  = frac*cx(mgs,lc)/dtp
      ELSE
        ccmxd(mgs)  = frac*qx(mgs,lc)/(xmas(mgs,lc)*rho0(mgs)*dtp)
      ENDIF
      end if
!
      if ( qx(mgs,li) .le. qxmin(li) ) then
      cimxd(mgs)  = frac*cx(mgs,li)/dtp
      else
      IF ( ipconc .ge. 1 ) THEN
        cimxd(mgs)  = frac*cx(mgs,li)/dtp
      ELSE
        cimxd(mgs)  = frac*qx(mgs,li)/(xmas(mgs,li)*rho0(mgs)*dtp)
      ENDIF
      end if
!
!
      crmxd(mgs)  = 0.10*cx(mgs,lr)/dtp
      csmxd(mgs)  = frac*cx(mgs,ls)/dtp
      chmxd(mgs)  = frac*cx(mgs,lh)/dtp

      ccmxd(mgs)  = frac*cx(mgs,lc)/dtp
      cimxd(mgs)  = frac*cx(mgs,li)/dtp
      crmxd(mgs)  = frac*cx(mgs,lr)/dtp
      csmxd(mgs)  = frac*cx(mgs,ls)/dtp
      chmxd(mgs)  = frac*cx(mgs,lh)/dtp

      qxmxd(mgs,lv) = Max(0.0, 0.1*(qx(mgs,lv) - qvs(mgs))/dtp)

      DO il = lc,lhab
       qxmxd(mgs,il) = frac*qx(mgs,il)/dtp
       cxmxd(mgs,il) = frac*cx(mgs,il)/dtp
      ENDDO

      end do





    ! default factors between mean volume and maximum mass volume
      maxmassfac(lc) = ( (2. + 3.*(1. + xnu(lc)) )**3/( 3.*(1. + xnu(lc)) ) )
      maxmassfac(li) = ( (2. + 3.*(1. + xnu(li)) )**3/( 3.*(1. + xnu(li)) ) )

      IF ( imurain == 3 ) THEN
        maxmassfac(lr) = ( (2. + 3.*(1. + xnu(lr)) )**3/( 3.*(1. + xnu(lr)) ) )
      ELSE
        maxmassfac(lr) =  (3.0 + alphar)**3/    &
     &                  ((3.+alphar)*(2.+alphar)*(1. + alphar) )
      ENDIF

      IF ( imusnow == 3 ) THEN
        maxmassfac(ls) = ( (2. + 3.*(1. + alphas) )**3/( 3.*(1. + alphas) ) )
      ELSE
        maxmassfac(ls) =  (3.0 + alphas)**3/    &
     &                  ((3.+alphas)*(2.+alphas)*(1. + alphas) )
      ENDIF
      
        maxmassfac(lh) =  (3.0 + alphah)**3/    &
     &                  ((3.+alphah)*(2.+alphah)*(1. + alphah) )

       IF ( lhl > 1 ) THEN
        maxmassfac(lhl) =  (3.0 + alphahl)**3/    &
     &                  ((3.+alphahl)*(2.+alphahl)*(1. + alphahl) )
       ENDIF
      


       DO mgs = 1,ngscnt
          DO il = lh,lhab ! graupel and hail only
            
            vshdgs(mgs,il) = vshd ! base value
            
            IF ( qx(mgs,il) > qxmin(il) ) THEN
              
              tmpdiam = (shedalp+alpha(mgs,il))*xdia(mgs,il,1)*( xdn(mgs,il)/917. )**(1./3.) ! erm added density factor for equiv. solid ice sphere 10.12.2015
              
              IF ( tmpdiam > sheddiam0 ) THEN
                vshdgs(mgs,il) = 0.523599*(1.5e-3)**3/massfacshr ! 1.5mm drops from very large ice
              ELSEIF ( tmpdiam > sheddiam ) THEN ! intermediate size
                vshdgs(mgs,il) = 0.523599*(3.0e-3)**3/massfacshr ! 3.0mm drops from medium-large ice
              ELSE
!                vshdgs(mgs,il) = Min( xvmx(lr), xv(mgs,il)*xdn(mgs,il)*0.001 ) ! size of drop from melted mean ice particle
                vshdgs(mgs,il) = Min( xvmx(lr), 6./pi*xdn(mgs,il)*0.001*tmpdiam**3 )/massfacshr ! size of drop from melted mean ice particle
              ENDIF
            ENDIF
          ENDDO
       ENDDO

!
!
!  microphysics source terms (1/s) for mixing ratios 
!
!
!
!  Collection efficiencies:
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: Set collection efficiencies'
!
      do mgs = 1,ngscnt
!
!
!
      erw(mgs) = 0.0
      esw(mgs) = 0.0
      ehw(mgs) = 0.0
      ehlw(mgs) = 0.0
!      ehxw(mgs) = 0.0
!
      err(mgs) = 0.0
      esr(mgs) = 0.0
      il2(mgs) = 0
      il3(mgs) = 0
      ehr(mgs) = 0.0
      ehlr(mgs) = 0.0
!      ehxr(mgs) = 0.0
!
      eri(mgs) = 0.0
      esi(mgs) = 0.0
      ehi(mgs) = 0.0 ! used as sticking efficiency, so collection efficiency is ehi*ehiclsn
      ehli(mgs) = 0.0 ! used as sticking efficiency, so collection efficiency is ehli*ehliclsn
!      ehxi(mgs) = 0.0
!
      ers(mgs) = 0.0
      ess(mgs) = 0.0
      ehs(mgs) = 0.0 ! used as sticking efficiency, so collection efficiency is ehs*ehsclsn
      ehls(mgs) = 0.0 ! used as sticking efficiency, so collection efficiency is ehls*ehlsclsn
      ehscnv(mgs) = 0.0
!      ehxs(mgs) = 0.0
!
      eiw(mgs) = 0.0
      eii(mgs) = 0.0

      ehsclsn(mgs) = 0.0
      ehiclsn(mgs) = 0.0
      ehlsclsn(mgs) = 0.0
      ehliclsn(mgs) = 0.0
      esiclsn(mgs) = 0.0


      icwr(mgs) = 1
      IF ( qx(mgs,lc) .gt. qxmin(lc) ) THEN
       cwrad = 0.5*xdia(mgs,lc,1)
      DO il = 1,8
         IF ( cwrad .ge. 1.e-6*cwr(il,1) ) icwr(mgs) = il
      ENDDO
      ENDIF


      irwr(mgs) = 1
      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
         rwrad = 0.5*xdia(mgs,lr,3)  ! changed to mean volume diameter (10/6/06)
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) irwr(mgs) = il
      ENDDO
      ENDIF


      igwr(mgs) = 1
!      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
!         rwrad = 0.5*xdia(mgs,lr,1)
! setting erw = 1 always, so now use igwr for graupel
      IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
         rwrad = 0.5*xdia(mgs,lh,3)  ! changed to mean volume diameter (10/6/06)
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) igwr(mgs) = il
      ENDDO
      ENDIF

      IF ( lhl .gt. 1 ) THEN ! hail is turned on
      ihlr(mgs) = 1
      IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
         rwrad = 0.5*xdia(mgs,lhl,3)  ! changed to mean volume diameter (10/6/06)
      DO il = 1,6
         IF ( rwrad .ge. 1.e-6*grad(il,1) ) ihlr(mgs) = il
      ENDDO
      ENDIF
      ENDIF

!
!
!  Ice-Ice: Collection (cxc) efficiencies
!
!
      if ( qx(mgs,li) .gt. qxmin(li) ) then
!      IF ( ipconc .ge. 14 ) THEN
!       eii(mgs)=0.1*exp(0.1*temcg(mgs))
!       if ( temg(mgs) .lt. 243.15 .and. qx(mgs,lc) .gt. 1.e-6 ) then
!        eii(mgs)=0.1
!       end if
!      
!      ELSE
        eii(mgs) = exp(0.025*Min(temcg(mgs),0.0))  ! alpha1 from LFO83 (21)
!      ENDIF
      if ( temg(mgs) .gt. 273.15 ) eii(mgs) = 1.0
      end if
!
!
!
!  Ice-cloud water: Collection (cxc) efficiencies
!
!
      eiw(mgs) = 0.0
      if ( qx(mgs,li).gt.qxmin(li) .and. qx(mgs,lc).gt.qxmin(lc) ) then
      if (xdia(mgs,lc,1).gt.15.0e-06 .and. xdia(mgs,li,1).gt.30.0e-06) then
! erm 5/10/2007 test following change:
!      if (xdia(mgs,lc,1).gt.12.0e-06 .and. xdia(mgs,li,1).gt.50.0e-06) then
      eiw(mgs) = 0.5
      end if
      if ( temg(mgs) .ge. 273.15 ) eiw(mgs) = 0.0
      end if

!
!
!
!  Rain: Collection (cxc) efficiencies
!
!
      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lc).gt.qxmin(lc) ) then

       IF ( lnr .gt. 1 ) THEN
       erw(mgs) = 1.0

       ELSE

!      cwrad = 0.5*xdia(mgs,lc,1)
!      erw(mgs) =
!     >  min((aradcw + cwrad*(bradcw + cwrad*
!     <  (cradcw + cwrad*(dradcw)))), 1.0)
!       IF ( xdia(mgs,lc,1) .lt. 2.4e-06 .or. xdia(mgs,lr,1) .le. 50.0e-6 ) THEN
!          erw(mgs)=0.0
!       ENDIF
!       erw(mgs) = ew(icwr(mgs),igwr(mgs))
! interpolate along droplet radius
       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = irwr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,3)
       rwrad = 0.5*xdia(mgs,lr,3)
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)

!       write(iunit,*) 'slop1: ',slope1,slope2,ew(ic,ir),cwr(ic,2)

       x1 = ew(ic,  ir) + slope1*Max(0.0, (cwrad - cwr(ic,1)) )
       x2 = ew(icp1,ir) + slope2*Max(0.0, (cwrad - cwr(ic,1)) )

       slope1 = (x2 - x1)*grad(ir,2)

       erw(mgs) = Max(0.0, x1 + slope1*Max(0.0, (rwrad - grad(ir,1)) ))

!       write(iunit,*) 'erw: ',erw(mgs),1.e6*cwrad,1.e6*rwrad,ic,ir,x1,x2
!       write(iunit,*)

       erw(mgs) = Max(0.0, erw(mgs) )
       IF ( rwrad .lt. 50.e-6 ) THEN
         erw(mgs) = 0.0
       ELSEIF (  rwrad .lt. 100.e-6 ) THEN  ! linear change from zero at 50 to erw at 100 microns
         erw(mgs) = erw(mgs)*(rwrad - 50.e-6)/50.e-6
       ENDIF

       ENDIF
      end if
      IF ( cx(mgs,lc) .le. 0.0 ) erw(mgs) = 0.0
!
      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lr).gt.qxmin(lr) ) then
      err(mgs)=1.0
      end if
!
      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,ls).gt.qxmin(ls) ) then
      ers(mgs)=1.0
      end if
!
      if ( qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,li).gt.qxmin(li) ) then
!        IF ( vtxbar(mgs,lr,1) .gt. vtxbar(mgs,li,1) .and.
!     :       xdia(mgs,lr,3) .gt. 200.e-6 .and. xdia(mgs,li,3) .gt. 100.e-6 ) THEN
         eri(mgs) = eri0
!      cwrad = 0.5*xdia(mgs,li,3)
!      eri(mgs) =
!     >  1.0*min((aradcw + cwrad*(bradcw + cwrad*
!     <  (cradcw + cwrad*(dradcw)))), 1.0)
!         ENDIF
!       if ( xdia(mgs,li,1) .lt. 10.e-6 ) eri(mgs)=0.0
       if ( xdia(mgs,li,3) .lt. 40.e-6 ) eri(mgs)=0.0
      end if
!
!
!  Snow aggregates: Collection (cxc) efficiencies
!
! Modified by ERM with a linear function for small droplets and large
! snow agg. based numerical data from Wang and Ji (1992) in P&K 1997, which
! allows collection of very small droplets, albeit at low efficiency.  But slow
! fall speeds of snow make up for the efficiency.
!
      esw(mgs) = 0.0
      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,lc).gt.qxmin(lc) ) then
        esw(mgs) = 0.5
        if ( xdia(mgs,lc,1) .gt. 15.e-6 .and. xdia(mgs,ls,1) .gt. 100.e-6) then
          esw(mgs) = 0.5
        ELSEIF ( xdia(mgs,ls,1) .ge. 500.e-6 ) THEN
          esw(mgs) = Min(0.5, 0.05 + (0.8-0.05)/(40.e-6)*xdia(mgs,lc,1) )
        ENDIF
      end if
!
      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,lr).gt.qxmin(lr)  &
     &     .and. temg(mgs) .lt. tfr - 1.   &
     &                               ) then
      esr(mgs)=Exp(-(40.e-6)**3/xv(mgs,lr))*Exp(-40.e-6/xdia(mgs,ls,1))
      IF ( qx(mgs,ls) < 1.e-4 .and. qx(mgs,lr) < 1.e-4 ) il2(mgs) = 1
      end if
      
      IF ( ipconc < 3 .and. temg(mgs) < tfr .and. qx(mgs,lr).gt.qxmin(lr) .and. qx(mgs,lr) < 1.e-4 ) THEN
        il3(mgs) = 1
      ENDIF
!
!      if ( qx(mgs,ls).gt.qxmin(ls) ) then
      if ( temcg(mgs) < 0.0 ) then
            
      IF ( ipconc .lt. 4 .or. temcg(mgs) < esstem1 ) THEN
        ess(mgs) = 0.0
!        ess(mgs)=0.1*exp(0.1*min(temcg(mgs),0.0))
!        ess(mgs)=min(0.1,ess(mgs))
      ELSE
        IF ( temcg(mgs) > esstem1 .and. temcg(mgs) < esstem2 ) THEN  ! only nonzero for T > -25
        ess(mgs) = ess0*Exp(ess1*(esstem2) )*(temcg(mgs) - esstem1)/5. ! linear ramp up from zero at -25 to value at -20
        ELSEIF ( temcg(mgs) >= esstem2 ) THEN
        ess(mgs) = ess0*Exp(ess1*Min( temcg(mgs), 0.0 ) )
        ENDIF
      ENDIF
      end if
!
      if ( qx(mgs,ls).gt.qxmin(ls) .and. qx(mgs,li).gt.qxmin(li) ) then
       esiclsn(mgs) = esi_collsn
!      IF ( ipconc .lt. 4 ) THEN
      IF ( ipconc < 1 .and. lwsm6 ) THEN
        esi(mgs) = exp(0.7*min(temcg(mgs),0.0))
      ELSE
        esi(mgs) = esi0*exp(0.1*min(temcg(mgs),0.0))
        esi(mgs) = Min(0.1,esi(mgs))
      ENDIF
      IF ( ipconc .le. 3 ) THEN
       esi(mgs) =  exp(0.025*min(temcg(mgs),0.0)) ! LFO
!       esi(mgs) =  Min(0.5, exp(0.025*min(temcg(mgs),0.0)) ) ! LFO
!       esi(mgs)=0.5*exp(0.1*min(temcg(mgs),0.0))  ! 10ice
      ENDIF
!      ELSE ! zrnic/ziegler 1993
!      esi(mgs)= 0.1 ! 0.5*exp(0.1*min(temcg(mgs),0.0))
!      ENDIF
      if ( temg(mgs) .gt. 273.15 ) esi(mgs) = 0.0
      end if
!
!
!
!
!  Graupel: Collection (cxc) efficiencies
!
!
       xmascw(mgs) = xmas(mgs,lc)
      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,lc).gt.qxmin(lc) ) then
       ehw(mgs) = 1.0
       IF ( iehw .eq. 0 ) THEN
       ehw(mgs) = ehw0  ! default value is 1.0
       ELSEIF ( iehw .eq. 1 .or. iehw .eq. 10 ) THEN
      cwrad = 0.5*xdia(mgs,lc,1)
      ehw(mgs) = Min( ehw0,    &
     &  ewfac*min((aradcw + cwrad*(bradcw + cwrad*   &
     &  (cradcw + cwrad*(dradcw)))), 1.0) )
      
       ELSEIF ( iehw .eq. 2 .or. iehw .eq. 10 ) THEN
       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = igwr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,1)
       rwrad = 0.5*xdia(mgs,lh,3)  ! changed to mean volume diameter
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)
 
!        write(iunit,*) 'slop1: ',slope1,slope2,ew(ic,ir),cwr(ic,2)

       x1 = ew(ic,  ir) + slope1*Max(0.0, (cwrad - cwr(ic,1)) )
       x2 = ew(icp1,ir) + slope2*Max(0.0, (cwrad - cwr(ic,1)) )
       
       slope1 = (x2 - x1)*grad(ir,2)
       
       tmp = Max( 0.0, Min( 1.0, x1 + slope1*Max(0.0, (rwrad - grad(ir,1)) ) ) )
       ehw(mgs) = Min( ehw(mgs), tmp )

!       write(iunit,*) 'ehw: ',ehw(mgs),1.e6*cwrad,1.e6*rwrad,ic,ir,x1,x2
!       write(iunit,*)

!       ehw(mgs) = Max( 0.2, ehw(mgs) )
!  assume that ehw = 1 for zero air resistance (rho0 = 0.0) and extrapolate toward that
!      ehw(mgs) = ehw(mgs) + (ehw(mgs) - 1.0)*(rho0(mgs) - rho00)/rho00
!      ehw(mgs) = ehw(mgs) + (1.0 - ehw(mgs))*((Max(0.0,rho00 - rho0(mgs)))/rho00)**2

       ELSEIF ( iehw .eq. 3 .or. iehw .eq. 10 ) THEN ! use fraction of droplets greater than dmincw diameter
         tmp = Exp(- (dmincw/xdia(mgs,lc,1))**3)
         xmascw(mgs) = xmas(mgs,lc) + xdn0(lc)*(pi*dmincw**3/6.0) ! this is the average mass of the droplets with d > dmincw
         ehw(mgs) = Min( ehw(mgs), tmp )
       ELSEIF ( iehw .eq. 4 .or. iehw .eq. 10 ) THEN ! Cober and List 1993
         tmp =  &
     &   2.0*xdn(mgs,lc)*vtxbar(mgs,lh,1)*(0.5*xdia(mgs,lc,1))**2 &
     &  /(9.0*fadvisc(mgs)*0.5*xdia(mgs,lh,3))
         tmp = Max( 1.5, Min(10.0, tmp) )
         ehw(mgs) = Min( ehw(mgs), 0.55*Log10(2.51*tmp) )
       ENDIF
      if ( xdia(mgs,lc,1) .lt. 2.4e-06 ) ehw(mgs)=0.0

       ehw(mgs) = Min( ehw0, ehw(mgs) )
       
       IF ( ibfc == -1 .and. temcg(mgs) < -41.0 ) THEN
        ehw(mgs) = 0.0
       ENDIF 

      end if
!
      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,lr).gt.qxmin(lr)    &
!     &     .and. temg(mgs) .lt. tfr    &
     &                               ) then
!      ehr(mgs) = Exp(-(40.e-6)**3/xv(mgs,lr))*Exp(-40.e-6/xdia(mgs,lh,1))
      ehr(mgs) = 1.0
       ehr(mgs) = Min( ehr0, ehr(mgs) )
      end if
!
      IF ( qx(mgs,ls).gt.qxmin(ls) ) THEN
        IF ( ipconc .ge. 4 ) THEN
        ehscnv(mgs) = ehs0*exp(ehs1*min(temcg(mgs),0.0)) ! for 2-moment, used as default for ehs and ehls. Otherwise not used for snow->graupel conversion
        ELSE
        ehscnv(mgs) = exp(0.09*min(temcg(mgs),0.0))
        ENDIF
        if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,lc) > qxmin(lc)  ) then
          ehsclsn(mgs) = ehs_collsn
          IF ( xdia(mgs,ls,3) < 40.e-6 ) THEN
            ehsclsn(mgs) = 0.0
          ELSEIF ( xdia(mgs,ls,3) < 150.e-6 ) THEN
            ehsclsn(mgs) =  ehs_collsn*(xdia(mgs,ls,3) - 40.e-6)/(150.e-6 - 40.e-6)
          ELSE
            ehsclsn(mgs) = ehs_collsn
          ENDIF
!          ehs(mgs) = ehscnv(mgs)*Min(1.0, Max(0., xdn(mgs,lh) - xdnmn(lh)*1.2)/xdnmn(lh)  ) ! shut off qhacs as graupel goes to lowest density
          ehs(mgs) = ehscnv(mgs)*Min(1.0, Max(0.0,xdn(mgs,lh) - 300.)/300.  ) ! shut off qhacs as graupel goes to low density
          ehs(mgs) = Min(ehs(mgs),ehsmax)
        end if
      ENDIF
!
      if ( qx(mgs,lh).gt.qxmin(lh) .and. qx(mgs,li).gt.qxmin(li) ) then
      ehiclsn(mgs) = ehi_collsn
      ehi(mgs)=eii0*exp(eii1*min(temcg(mgs),0.0))
      ehi(mgs) = Min( ehimax, Max( ehi(mgs), ehimin ) )
      if ( temg(mgs) .gt. 273.15 .or. ( qx(mgs,lc) < qxmin(lc)) ) ehi(mgs) = 0.0
      end if


!
!
!  Hail: Collection (cxc) efficiencies
!
!
      IF ( lhl .gt. 1 ) THEN

      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,lc).gt.qxmin(lc) ) then
       IF ( iehw == 3 ) iehlw = 3
       IF ( iehw == 4 ) iehlw = 4
       ehlw(mgs) = ehlw0
       IF ( iehlw .eq. 0 ) THEN
       ehlw(mgs) = ehlw0  ! default value is 1.0
       ELSEIF ( iehlw .eq. 1 .or. iehlw .eq. 10 ) THEN
      cwrad = 0.5*xdia(mgs,lc,1)
      ehlw(mgs) = Min( ehlw0,    &
     &  ewfac*min((aradcw + cwrad*(bradcw + cwrad*   &
     &  (cradcw + cwrad*(dradcw)))), 1.0) )
      
       ELSEIF ( iehlw .eq. 2 .or. iehlw .eq. 10 ) THEN
       ic = icwr(mgs)
       icp1 = Min( 8, ic+1 )
       ir = ihlr(mgs)
       irp1 = Min( 6, ir+1 )
       cwrad = 0.5*xdia(mgs,lc,1)
       rwrad = 0.5*xdia(mgs,lhl,3)  ! changed to mean volume diameter
       
       slope1 = (ew(icp1, ir  ) - ew(ic,ir  ))*cwr(ic,2)
       slope2 = (ew(icp1, irp1) - ew(ic,irp1))*cwr(ic,2)
       
       x1 = ew(ic,  ir) + slope1*(cwrad - cwr(ic,1))
       x2 = ew(icp1,ir) + slope2*(cwrad - cwr(ic,1))
       
       slope1 = (x2 - x1)*grad(ir,2)
       
       tmp = Max( 0.0, Min( 1.0, x1 + slope1*(rwrad - grad(ir,1)) ) )
         ehlw(mgs) = Min( ehlw(mgs), tmp )
       ehlw(mgs) = Min( ehlw0, ehlw(mgs) )
!       ehw(mgs) = Max( 0.2, ehw(mgs) )
!  assume that ehw = 1 for zero air resistance (rho0 = 0.0) and extrapolate toward that
!      ehw(mgs) = ehw(mgs) + (ehw(mgs) - 1.0)*(rho0(mgs) - rho00)/rho00
!      ehlw(mgs) = ehlw(mgs) + (1.0 - ehlw(mgs))*((Max(0.0,rho00 - rho0(mgs)))/rho00)**2

       ELSEIF ( iehlw .eq. 3 .or. iehlw .eq. 10 ) THEN ! use fraction of droplets greater than 15 micron diameter
         tmp = Exp(- (dmincw/xdia(mgs,lc,1))**3)
         ehlw(mgs) = Min( ehlw(mgs), tmp )
       ELSEIF ( iehlw .eq. 4 .or. iehlw .eq. 10 ) THEN ! Cober and List 1993
         tmp =  &
     &   2.0*xdn(mgs,lc)*vtxbar(mgs,lhl,1)*(0.5*xdia(mgs,lc,1))**2 &
     &  /(9.0*fadvisc(mgs)*0.5*xdia(mgs,lhl,3))
         tmp = Max( 1.5, Min(10.0, tmp) )
         ehlw(mgs) = Min( ehlw(mgs), 0.55*Log10(2.51*tmp) )
       ENDIF
      if ( xdia(mgs,lc,1) .lt. 2.4e-06 ) ehlw(mgs)=0.0
       ehlw(mgs) = Min( ehlw0, ehlw(mgs) )

       IF ( ibfc == -1 .and. temcg(mgs) < -41.0 ) THEN 
        ehlw(mgs) = 0.0
       ENDIF 

      end if
!
      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,lr).gt.qxmin(lr)    &
!     &     .and. temg(mgs) .lt. tfr    &
     &                               ) then
        ehlr(mgs) = 1.0
       ehlr(mgs) = Min( ehlr0, ehlr(mgs) )
      end if
!
      IF ( qx(mgs,ls).gt.qxmin(ls) ) THEN
        if ( qx(mgs,lhl).gt.qxmin(lhl)  ) then
          ehlsclsn(mgs) = ehls_collsn
          ehls(mgs) = ehscnv(mgs)
          ehls(mgs) = Min(ehls(mgs),ehsmax)
        end if
      ENDIF
!
      if ( qx(mgs,lhl).gt.qxmin(lhl) .and. qx(mgs,li).gt.qxmin(li) ) then
      ehliclsn(mgs) = ehli_collsn
      ehli(mgs)=eii0hl*exp(eii1hl*min(temcg(mgs),0.0))
      ehli(mgs) = Min( ehimax, Max( ehli(mgs), ehimin ) )
      if ( temg(mgs) .gt. 273.15 .or. ( qx(mgs,lc) < qxmin(lc)) ) ehli(mgs) = 0.0
      end if


      ENDIF ! lhl .gt. 1

      ENDDO  ! mgs loop for collection efficiencies

!
!
!
!  Set flags for plates vs. columns
!
!
      do mgs = 1,ngscnt
!
      xplate(mgs) = 0.0
      xcolmn(mgs) = 1.0
!
!      if ( temcg(mgs) .lt. 0. .and. temcg(mgs) .ge. -4. ) then
!      xplate(mgs) = 1.0
!      xcolmn(mgs) = 0.0
!      end if
!c
!      if ( temcg(mgs) .lt. -4. .and. temcg(mgs) .ge. -9. ) then
!      xplate(mgs) = 0.0
!      xcolmn(mgs) = 1.0
!      end if
!c
!      if ( temcg(mgs) .lt. -9. .and. temcg(mgs) .ge. -22.5 ) then
!      xplate(mgs) = 1.0
!      xcolmn(mgs) = 0.0
!      end if
!c
!      if ( temcg(mgs) .lt. -22.5 .and. temcg(mgs) .ge. -90. ) then
!      xplate(mgs) = 0.0
!      xcolmn(mgs) = 1.0
!      end if
!
      end do
!
!
!
!  Collection growth equations....
!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: rain collects xxxxx'
!
      do mgs = 1,ngscnt
      qracw(mgs) =  0.0
      IF ( qx(mgs,lr) .gt. qxmin(lr) .and. erw(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .lt. 3 ) THEN
       IF ( erw(mgs) .gt. 0.0 .and. qx(mgs,lr) .gt. 1.e-7 ) THEN
       vt = (ar*(xdia(mgs,lc,1)**br))*rhovt(mgs)
       qracw(mgs) =    &
     &   (0.25)*pi*erw(mgs)*qx(mgs,lc)*cx(mgs,lr) &
!     >  *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,lc,1))   &
     &  *Max(0.0, vtxbar(mgs,lr,1)-vt)   &
     &  *(  gf3*xdia(mgs,lr,2)    &
     &    + 2.0*gf2*xdia(mgs,lr,1)*xdia(mgs,lc,1)    &
     &    + gf1*xdia(mgs,lc,2) )
!       qracw(mgs) = 0.0
!      write(iunit,*) 'qracw,cx =',qracw(mgs),1.e6*xdia(mgs,lr,1),erw(mgs)
!      write(iunit,*) 'qracw,cx =',qracw(mgs),cx(mgs,lc),kgs(mgs),cx(mgs,lr),1.e6*xdia(mgs,lr,1),vtxbar(mgs,lr,1),vt
!      write(iunit,*) 'vtr: ',vtxbar(mgs,lr,1), ar*gf4br/6.0*xdia(mgs,lr,1)**br, rhovt(mgs),
!     :         ar*gf4br/6.0*xdia(mgs,lr,1)**br * rhovt(mgs)
       ENDIF
      ELSE

      IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN 
       rwrad = 0.5*xdia(mgs,lr,3)
        IF ( rwrad .gt. rh(mgs) ) THEN ! .or. cx(mgs,lr) .gt. nh(mgs) ) THEN
         IF ( rwrad .gt. rwradmn ) THEN
!      DM1CCC=A2*XNC*XNR*XVC*(((CNU+2.)/(CNU+1.))*XVC+XVR)       ! (A12)
!     NOTE: Result is independent of imurain, assumes mucloud = 3
           qracw(mgs) = erw(mgs)*aa2*cx(mgs,lr)*cx(mgs,lc)*xmas(mgs,lc)*   &
     &        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,lr))/rho0(mgs) !*rhoinv(mgs)
         ELSE

          IF ( imurain == 3 ) THEN

!      DM1CCC=A1*XNC*XNR*(((CNU+3.)*(CNU+2.)/(CNU+1.)**2)*XVC**3+ ! (A14)
!     1 ((RNU+2.)/(RNU+1.))*XVC*XVR**2)

!           qracw(mgs) = aa1*cx(mgs,lr)*cx(mgs,lc)*xdn(mgs,lc)*   &
!     &        ((cnu + 3.)*(cnu + 2.)*xv(mgs,lc)**3/(cnu + 1.)**2 +    &
!     &         (alpha(mgs,lr) + 2.)*xv(mgs,lc)*xv(mgs,lr)**2/(alpha(mgs,lr) + 1.))/rho0(mgs) !*rhoinv(mgs)
! save multiplies by converting cx*xdn*xv/rho0 to qx
           qracw(mgs) = aa1*cx(mgs,lr)*qx(mgs,lc)*   &
     &        ((cnu + 3.)*(cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.)**2 +    &
     &         (alpha(mgs,lr) + 2.)*xv(mgs,lr)**2/(alpha(mgs,lr) + 1.)) 
           
           ELSE ! imurain == 1

           qracw(mgs) = aa1*cx(mgs,lr)*qx(mgs,lc)*   &
     &        ((cnu + 3.)*(cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.)**2 +    &
     &         (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)*xv(mgs,lr)**2/ &
     &          ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.))) 
           
           ENDIF
           
         ENDIF
        ENDIF
        ENDIF
       ENDIF
!       qracw(mgs) = Min(qracw(mgs), qx(mgs,lc))
       qracw(mgs) = Min(qracw(mgs), qcmxd(mgs))
       ENDIF
      end do
!
      do mgs = 1,ngscnt
      qraci(mgs) = 0.0
      craci(mgs) = 0.0
      IF ( eri(mgs) .gt. 0.0 .and. iacr .ge. 1 .and. xdia(mgs,lr,3) .gt. 2.*rwradmn ) THEN
        IF ( ipconc .ge. 3 ) THEN

           tmp = eri(mgs)*aa2*cx(mgs,lr)*cx(mgs,li)*   &
     &        ((cinu + 2.)*xv(mgs,li)/(cinu + 1.) + xv(mgs,lr))

        qraci(mgs) = Min( qxmxd(mgs,li), tmp*xmas(mgs,li)*rhoinv(mgs) )
        craci(mgs) = Min( cxmxd(mgs,li), tmp )

!       vt = Sqrt((vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))**2 +
!     :            0.04*vtxbar(mgs,lr,1)*vtxbar(mgs,li,1) )
!
!          qraci(mgs) = 0.25*pi*eri(mgs)*cx(mgs,lr)*qx(mgs,li)*vt*
!     :         (  da0(lr)*xdia(mgs,lr,3)**2 +
!     :            dab1(lr,li)*xdia(mgs,lr,3)*xdia(mgs,li,3) +
!     :            da1(li)*xdia(mgs,li,3)**2 )
!
!
!       vt = Sqrt((vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))**2 +
!     :            0.04*vtxbar(mgs,lr,1)*vtxbar(mgs,li,1) )
!
!          craci(mgs) = 0.25*pi*eri(mgs)*cx(mgs,lr)*cx(mgs,li)*vt*
!     :         (  da0(lr)*xdia(mgs,lr,3)**2 +
!     :            dab0(lr,li)*xdia(mgs,lr,3)*xdia(mgs,li,3) +
!     :            da0(li)*xdia(mgs,li,3)**2 )
!
!          qraci(mgs) = Min( qraci(mgs), qxmxd(mgs,li) )
!          craci(mgs) = Min( craci(mgs), cxmxd(mgs,li) )

        ELSE
          qraci(mgs) =    &
     &     min(   &
     &     (0.25)*pi*eri(mgs)*qx(mgs,li)*cx(mgs,lr)   &
     &    *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))   &
     &    *(  gf3*xdia(mgs,lr,2)    &
     &      + 2.0*gf2*xdia(mgs,lr,1)*xdia(mgs,li,1)    &
     &      + gf1*xdia(mgs,li,2) )     &
     &    , qimxd(mgs))
        ENDIF
      if ( temg(mgs) .gt. 268.15 ) then
      qraci(mgs) = 0.0
      end if
      ENDIF
      end do
!
      do mgs = 1,ngscnt
      qracs(mgs) =  0.0
      IF ( ers(mgs) .gt. 0.0 .and. ipconc < 3 ) THEN
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,ls,1)
       ENDIF
      qracs(mgs) =      &
     &   min(     &
     &   ((0.25)*pi/gf4)*ers(mgs)*qx(mgs,ls)*cx(mgs,lr)     &
     &  *abs(vtxbar(mgs,lr,1)-vt)     &
     &  *(  gf6*gf1*xdia(mgs,ls,2)     &
     &    + 2.0*gf5*gf2*xdia(mgs,ls,1)*xdia(mgs,lr,1)      &
     &    + gf4*gf3*xdia(mgs,lr,2) )      &
     &  , qsmxd(mgs))
      ENDIF
      end do

!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: snow collects xxxxx'
!
      do mgs = 1,ngscnt
      qsacw(mgs) =  0.0
      csacw(mgs) =  0.0
      vsacw(mgs) =  0.0
      IF ( esw(mgs) .gt. 0.0 ) THEN

       IF ( ipconc .ge. 4 ) THEN
!      QSACC=CECS*RVT*A2*XNC*XNS*XVC*ROS*
!     *    (((CNU+2.)/(CNU+1.))*XVC+XVS)/RO

!        tmp = esw(mgs)*rvt*aa2*cx(mgs,ls)*cx(mgs,lc)*
!     :        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,ls))
        tmp = 1.0*rvt*aa2*cx(mgs,ls)*cx(mgs,lc)*   &
     &        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,ls))

        qsacw(mgs) = Min( qxmxd(mgs,lc), tmp*xmas(mgs,lc)*rhoinv(mgs) )
        csacw(mgs) = Min( cxmxd(mgs,lc), tmp )

          IF ( lvol(ls) .gt. 1 ) THEN
             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,ls) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,ls,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
             rimdn(mgs,ls) = Min( Max( rimc3, rimdn(mgs,ls) ), rimc4 )
             ELSE
             rimdn(mgs,ls) = 1000.
             ENDIF

           vsacw(mgs) = rho0(mgs)*qsacw(mgs)/rimdn(mgs,ls)

          ENDIF


!        qsacw(mgs) = cecs*aa2*cx(mgs,ls)*cx(mgs,lc)*xmas(mgs,lc)*
!     :        ((cnu + 2.)*xv(mgs,lc)/(cnu + 1.) + xv(mgs,ls))*rhoinv(mgs)
       ELSE
!      qsacw(mgs) =
!     >   min(
!     >   ((0.25)*pi)*esw(mgs)*qx(mgs,lc)*cx(mgs,ls)
!     >  *abs(vtxbar(mgs,ls,1)-vtxbar(mgs,lc,1))
!     >  *(  gf3*xdia(mgs,ls,2)
!     >    + 2.0*gf2*xdia(mgs,ls,1)*xdia(mgs,lc,1)
!     >    + gf1*xdia(mgs,lc,2) )
!     <  , qcmxd(mgs))

            vt = abs(vtxbar(mgs,ls,1)-vtxbar(mgs,lc,1))

          qsacw(mgs) = 0.25*pi*esw(mgs)*cx(mgs,ls)*qx(mgs,lc)*vt*   &
     &         (  da0(ls)*xdia(mgs,ls,3)**2 +     &
     &            dab1(ls,lc)*xdia(mgs,ls,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )
        qsacw(mgs) = Min( qsacw(mgs), qxmxd(mgs,ls) )
        csacw(mgs) = rho0(mgs)*qsacw(mgs)/xmas(mgs,lc)
       ENDIF
      ENDIF
      end do
!
!
      do mgs = 1,ngscnt
      qsaci(mgs) = 0.0
      csaci(mgs) = 0.0
      csaci0(mgs) = 0.0
      IF ( ipconc .ge. 4 ) THEN
      IF ( esi(mgs) .gt. 0.0 .or. ( ipelec > 0 .and. esiclsn(mgs) > 0.0 )) THEN
!      QSCOI=CEXS*RVT*A2*XNCI*XNS*XVCI*ROS*
!     *  (((CINU+2.)/(CINU+1.))*VCIP+XVS)/RO

        tmp = esiclsn(mgs)*rvt*aa2*cx(mgs,ls)*cx(mgs,li)*   &
     &        ((cinu + 2.)*xv(mgs,li)/(cinu + 1.) + xv(mgs,ls))

        qsaci(mgs) = Min( qxmxd(mgs,li), esi(mgs)*tmp*xmas(mgs,li)*rhoinv(mgs) )
        csaci0(mgs) = tmp
        csaci(mgs) = Min(cxmxd(mgs,li), esi(mgs)*tmp )

!      qsaci(mgs) =
!     >   min(
!     >   ((0.25)*pi)*esi(mgs)*qx(mgs,li)*cx(mgs,ls)
!     >  *abs(vtxbar(mgs,ls,1)-vtxbar(mgs,li,1))
!     >  *(  gf3*xdia(mgs,ls,2)
!     >    + 2.0*gf2*xdia(mgs,ls,1)*xdia(mgs,li,1)
!     >    + gf1*xdia(mgs,li,2) )
!     <  , qimxd(mgs))
      ENDIF
      ELSE ! 
      IF ( esi(mgs) .gt. 0.0 ) THEN
         qsaci(mgs) =    &
     &   min(   &
     &   ((0.25)*pi)*esi(mgs)*qx(mgs,li)*cx(mgs,ls)   &
     &  *abs(vtxbar(mgs,ls,1)-vtxbar(mgs,li,1))   &
     &  *(  gf3*xdia(mgs,ls,2)    &
     &    + 2.0*gf2*xdia(mgs,ls,1)*xdia(mgs,li,1)    &
     &    + gf1*xdia(mgs,li,2) )     &
     &  , qimxd(mgs))
      ENDIF
      ENDIF
      end do
!
!
!
      do mgs = 1,ngscnt
      qsacr(mgs) = 0.0
      qsacrs(mgs) = 0.0
      csacr(mgs) = 0.0
      IF ( esr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN
!       vt = Sqrt((vtxbar(mgs,ls,1)-vtxbar(mgs,lr,1))**2 + 
!     :            0.04*vtxbar(mgs,ls,1)*vtxbar(mgs,lr,1) )
!       qsacr(mgs) = esr(mgs)*cx(mgs,ls)*vt*
!     :     qx(mgs,lr)*0.25*pi*
!     :      (3.02787*xdia(mgs,lr,2) + 
!     :       3.30669*xdia(mgs,ls,1)*xdia(mgs,lr,1) + 
!     :       2.*xdia(mgs,ls,2))
!        qsacr(mgs) = Min( qsacr(mgs), qrmxd(mgs) )
!        csacr(mgs) = qsacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
!        csacr(mgs) = min(csacr(mgs),crmxd(mgs))
      ELSE
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,ls,1)
       ENDIF
       
       qsacr(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*esr(mgs)*qx(mgs,lr)*cx(mgs,ls)   &
     &  *abs(vtxbar(mgs,lr,1)-vt)   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,ls,1)    &
     &    + gf4*gf3*xdia(mgs,ls,2) )    &
     &  , qrmxd(mgs))
      ENDIF
      ENDIF
      end do
!
!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: graupel collects xxxxx'
!
      do mgs = 1,ngscnt
      qhacw(mgs) = 0.0
      rarx(mgs,lh) = 0.0
      vhacw(mgs) = 0.0
      vhsoak(mgs) = 0.0
      zhacw(mgs) = 0.0
      
      IF ( .false. ) THEN
        vtmax = (gz(igs(mgs),jgs,kgs(mgs))/dtp)
        vtxbar(mgs,lh,1) = Min( vtmax, vtxbar(mgs,lh,1))
        vtxbar(mgs,lh,2) = Min( vtmax, vtxbar(mgs,lh,2))
        vtxbar(mgs,lh,3) = Min( vtmax, vtxbar(mgs,lh,3))
      ENDIF
      IF ( ehw(mgs) .gt. 0.0 ) THEN

        IF ( ipconc .ge. 2 ) THEN

        IF ( .false. ) THEN  
        qhacw(mgs) = (ehw(mgs)*qx(mgs,lc)*cx(mgs,lh)*pi*   &
     &    abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))*   &
     &    (2.0*xdia(mgs,lh,1)*(xdia(mgs,lh,1) +    &
     &         xdia(mgs,lc,1)*gf73rds) +    &
     &      xdia(mgs,lc,2)*gf83rds))/4.     
     
         ELSE  ! using Seifert coefficients
            vt = abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1)) 

          qhacw(mgs) = 0.25*pi*ehw(mgs)*cx(mgs,lh)*qx(mgs,lc)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,lc,lh)*xdia(mgs,lh,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 ) 
         
         ENDIF
          qhacw(mgs) = Min( qhacw(mgs), 0.5*qx(mgs,lc)/dtp )
        
         IF ( lzh .gt. 1 ) THEN
          tmp = qx(mgs,lh)/cx(mgs,lh)
          
!!          g1 = (6.0 + alpha(mgs,lh))*(5.0 + alpha(mgs,lh))*(4.0 + alpha(mgs,lh))/
!!     :         ((3.0 + alpha(mgs,lh))*(2.0 + alpha(mgs,lh))*(1.0 + alpha(mgs,lh)))
!          alp = Max( 1.0, alpha(mgs,lh)+1. )
!          g1 = (6.0 + alp)*(5.0 + alp)*(4.0 + alp)/
!     :         ((3.0 + alp)*(2.0 + alp)*(1.0 + alp))
!          zhacw(mgs) =  g1*(6.*rho0(mgs)/(pi*1000.))**2*( 2.*( qx(mgs,lh)/cx(mgs,lh)) * qhacw(mgs) )
         ENDIF
        
        ELSE
         qhacw(mgs) =    &
     &   min(   &
     &   ((0.25)*pi)*ehw(mgs)*qx(mgs,lc)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf3*xdia(mgs,lh,2)    &
     &    + 2.0*gf2*xdia(mgs,lh,1)*xdia(mgs,lc,1)    &
     &    + gf1*xdia(mgs,lc,2) )     &
     &    , 0.5*qx(mgs,lc)/dtp)
!     <  , qxmxd(mgs,lc))
!     <  , qcmxd(mgs))
       
       
         IF ( lwsm6 .and. qsacw(mgs) > 0.0 .and.  qhacw(mgs) > 0.0) THEN
           qaacw = ( qx(mgs,ls)*qsacw(mgs) + qx(mgs,lh)*qhacw(mgs) )/(qx(mgs,ls) + qx(mgs,lh))
!           qaacw = Min( qaacw, 0.5*(qsacw(mgs) + qhacw(mgs) ) )
           qsacw(mgs) = qaacw
           qhacw(mgs) = qaacw
         ENDIF
         
       ENDIF

          IF ( lvol(lh) .gt. 1 .or. lhl .gt. 1 ) THEN ! calculate rime density for graupel volume and/or for graupel conversion to hail
             
             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,lh) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,lh,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
!             rimdn(mgs,lh) = Min( Max( hdnmn, rimc3, rimdn(mgs,lh) ), rimc4 )
             rimdn(mgs,lh) = Min( Max( rimc3, rimdn(mgs,lh) ), rimc4 )
             ELSE
             rimdn(mgs,lh) = 1000.
             ENDIF
             
             IF ( lvol(lh) > 1 ) vhacw(mgs) = rho0(mgs)*qhacw(mgs)/rimdn(mgs,lh)

          ENDIF
      
        IF ( qx(mgs,lh) .gt. qxmin(lh) .and. ipelec .ge. 1 ) THEN
         rarx(mgs,lh) =     &
     &    qhacw(mgs)*1.0e3*rho0(mgs)/((pi/2.0)*xdia(mgs,lh,2)*cx(mgs,lh))
        ENDIF
      
      ENDIF  
      end do   
!
!
      do mgs = 1,ngscnt
      qhaci(mgs) = 0.0
      qhaci0(mgs) = 0.0
      IF ( ehi(mgs) .gt. 0.0 ) THEN
       IF (  ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,li,1) )

          qhaci0(mgs) = 0.25*pi*ehiclsn(mgs)*cx(mgs,lh)*qx(mgs,li)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,li,lh)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da1(li)*xdia(mgs,li,3)**2 ) 
          qhaci(mgs) = Min( ehi(mgs)*qhaci0(mgs), qimxd(mgs) )
       ELSE
        qhaci(mgs) =    &
     &  min(   &
     &  ((0.25)*pi)*ehi(mgs)*ehiclsn(mgs)*qx(mgs,li)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))   &
     &  *(  gf3*xdia(mgs,lh,2)    &
     &    + 2.0*gf2*xdia(mgs,lh,1)*xdia(mgs,li,1)    &
     &    + gf1*xdia(mgs,li,2) )     &
     &  , qimxd(mgs))
       ENDIF
      ENDIF
      end do   
!
!
      do mgs = 1,ngscnt
      qhacs(mgs) = 0.0
      qhacs0(mgs) = 0.0
      IF ( ehs(mgs) .gt. 0.0 ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,ls,1) )

          qhacs0(mgs) = 0.25*pi*ehsclsn(mgs)*cx(mgs,lh)*qx(mgs,ls)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,ls,lh)*xdia(mgs,lh,3)*xdia(mgs,ls,3) +    &
     &            da1(ls)*xdia(mgs,ls,3)**2 ) 
      
          qhacs(mgs) = Min( ehs(mgs)*qhacs0(mgs), qsmxd(mgs) )

       ELSE
         qhacs(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*ehs(mgs)*ehsclsn(mgs)*qx(mgs,ls)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))   &
     &  *(  gf6*gf1*xdia(mgs,ls,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,ls,1)*xdia(mgs,lh,1)   &
     &    + gf4*gf3*xdia(mgs,lh,2) )   &
     &  , qsmxd(mgs))
        ENDIF
      ENDIF
      end do   
!
      do mgs = 1,ngscnt
      qhacr(mgs) = 0.0
      qhacrmlr(mgs) = 0.0
      vhacr(mgs) = 0.0
      chacr(mgs) = 0.0
      zhacr(mgs) = 0.0
      IF ( temg(mgs) .gt. tfr ) raindn(mgs,lh) = 1000.0

      IF ( ehr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,lr,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,lr,1) )
!       qhacr(mgs) = ehr(mgs)*cx(mgs,lh)*vt*
!     :     qx(mgs,lr)*0.25*pi*
!     :      (3.02787*xdia(mgs,lr,2) + 
!     :       3.30669*xdia(mgs,lh,1)*xdia(mgs,lr,1) + 
!     :       2.*xdia(mgs,lh,2))
     
       qhacr(mgs) = 0.25*pi*ehr(mgs)*cx(mgs,lh)*qx(mgs,lr)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab1lh(mgs,lr,lh)*xdia(mgs,lh,3)*xdia(mgs,lr,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 )
!       IF ( qhacr(mgs) .gt. 0. .or. tmp .gt. 0.0 ) write(0,*) 'qhacr= ',qhacr(mgs),tmp
!!        qhacr(mgs) = Min( qhacr(mgs), qrmxd(mgs) )
!!        chacr(mgs) = qhacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
!!        chacr(mgs) = min(chacr(mgs),crmxd(mgs))

        qhacr(mgs) = Min( qhacr(mgs), qxmxd(mgs,lr) )

        IF ( temg(mgs) > tfr ) THEN
          qhacrmlr(mgs) = qhacr(mgs)
          qhacr(mgs) = 0.0
        ELSE
!        chacr(mgs) = Min( qhacr(mgs)*rho0(mgs)/xmas(mgs,lr), cxmxd(mgs,lr) )

!       chacr(mgs) = ehr(mgs)*cx(mgs,lh)*vt*
!     :     cx(mgs,lr)*0.25*pi*
!     :      (0.69874*xdia(mgs,lr,2) +
!     :       1.24001*xdia(mgs,lh,1)*xdia(mgs,lr,1) +
!     :       2.*xdia(mgs,lh,2))

!        chacr(mgs) = 0.25*pi*ehr(mgs)*cx(mgs,lh)*cx(mgs,lr)*vt*
!     :         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +
!     :            dab0lh(mgs,lr)*xdia(mgs,lh,3)*xdia(mgs,lr,3) +
!     :            da0(lr)*xdia(mgs,lr,3)**2 )

!       IF ( qhacr(mgs) .gt. 0. .or. tmp .gt. 0.0 ) write(0,*) 'chacr= ',chacr(mgs),tmp

        chacr(mgs) = qhacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
        chacr(mgs) = min(chacr(mgs),crmxd(mgs))

      IF ( lzh .gt. 1 ) THEN
          tmp = qx(mgs,lh)/cx(mgs,lh)

!          g1 = (6.0 + alpha(mgs,lh))*(5.0 + alpha(mgs,lh))*(4.0 + alpha(mgs,lh))/
!     :         ((3.0 + alpha(mgs,lh))*(2.0 + alpha(mgs,lh))*(1.0 + alpha(mgs,lh)))
!          alp = Max( 1.0, alpha(mgs,lh)+1. )
!          g1 = (6.0 + alp)*(5.0 + alp)*(4.0 + alp)/
!     :         ((3.0 + alp)*(2.0 + alp)*(1.0 + alp))
!        zhacr(mgs) =  g1*(6.*rho0(mgs)/(pi*1000.))**2*( 2.*( tmp ) * qhacr(mgs) - tmp**2 * chacr(mgs) )
!        zhacr(mgs) =  g1*(6.*rho0(mgs)/(pi*xdn(mgs,lh)))**2*( 2.*( tmp ) * qhacr(mgs) )
      ENDIF
      ENDIF ! temg > tfr
      
      ELSE
       IF ( lwsm6 .and. ipconc == 0 ) THEN
         vt = vt2ave(mgs)
       ELSE
         vt = vtxbar(mgs,lh,1)
       ENDIF

      qhacr(mgs) =   &
     &   min(   &
     &   ((0.25)*pi/gf4)*ehr(mgs)*qx(mgs,lr)*cx(mgs,lh)   &
     &  *abs(vt-vtxbar(mgs,lr,1))   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)   &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,lh,1)   &
     &    + gf4*gf3*xdia(mgs,lh,2) )   &
     &  , qrmxd(mgs))
      
        IF ( temg(mgs) > tfr ) THEN
          qhacrmlr(mgs) = qhacr(mgs)
          qhacr(mgs) = 0.0
        ENDIF
      
      ENDIF
        IF ( lvol(lh) .gt. 1 ) THEN
         vhacr(mgs) = rho0(mgs)*qhacr(mgs)/raindn(mgs,lh)
        ENDIF
      ENDIF
      end do

!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: hail collects xxxxx'
!

      do mgs = 1,ngscnt
      qhlacw(mgs) = 0.0
      vhlacw(mgs) = 0.0
      vhlsoak(mgs) = 0.0
      IF ( lhl > 1 .and. .true.) THEN
        vtmax = (gz(igs(mgs),jgs,kgs(mgs))/dtp)
        vtxbar(mgs,lhl,1) = Min( vtmax, vtxbar(mgs,lhl,1))
        vtxbar(mgs,lhl,2) = Min( vtmax, vtxbar(mgs,lhl,2))
        vtxbar(mgs,lhl,3) = Min( vtmax, vtxbar(mgs,lhl,3))
      ENDIF

      IF ( lhl > 0 ) THEN
      rarx(mgs,lhl) = 0.0
      ENDIF

      IF ( lhl .gt. 1 .and. ehlw(mgs) .gt. 0.0 ) THEN


!        IF ( ipconc .ge. 2 ) THEN

            vt = abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,lc,1))

          qhlacw(mgs) = 0.25*pi*ehlw(mgs)*cx(mgs,lhl)*qx(mgs,lc)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,lc,lhl)*xdia(mgs,lhl,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )


          qhlacw(mgs) = Min( qhlacw(mgs), 0.5*qx(mgs,lc)/dtp )

          IF ( lvol(lhl) .gt. 1 ) THEN

             IF ( temg(mgs) .lt. 273.15) THEN
             rimdn(mgs,lhl) = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,lhl,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
             rimdn(mgs,lhl) = Min( Max( hldnmn, rimc3, rimdn(mgs,lhl) ), rimc4 )
             ELSE
             rimdn(mgs,lhl) = 1000.
             ENDIF

             vhlacw(mgs) = rho0(mgs)*qhlacw(mgs)/rimdn(mgs,lhl)

          ENDIF


        IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. ipelec .ge. 1 ) THEN
         rarx(mgs,lhl) =     &
     &    qhlacw(mgs)*1.0e3*rho0(mgs)/((pi/2.0)*xdia(mgs,lhl,2)*cx(mgs,lhl))
        ENDIF

      ENDIF
      end do

      qhlaci(:) = 0.0
      qhlaci0(:) = 0.0
      IF ( lhl .gt. 1  ) THEN
      do mgs = 1,ngscnt
      IF ( ehli(mgs) .gt. 0.0 ) THEN
       IF (  ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,li,1) )

          qhlaci0(mgs) = 0.25*pi*ehliclsn(mgs)*cx(mgs,lhl)*qx(mgs,li)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,li,lhl)*xdia(mgs,lhl,3)*xdia(mgs,li,3) +    &
     &            da1(li)*xdia(mgs,li,3)**2 )
        ! qhlaci(mgs) = Min( qhlaci(mgs), qimxd(mgs) )
          qhlaci(mgs) = Min( ehli(mgs)*qhlaci0(mgs), qimxd(mgs) )
       ENDIF
      ENDIF
      end do
      ENDIF
!
      qhlacs(:) = 0.0
      qhlacs0(:) = 0.0
      IF ( lhl .gt. 1 ) THEN
      do mgs = 1,ngscnt
      IF ( ehls(mgs) .gt. 0.0) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,ls,1) )

          qhlacs0(mgs) = 0.25*pi*ehlsclsn(mgs)*cx(mgs,lhl)*qx(mgs,ls)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,ls,lhl)*xdia(mgs,lhl,3)*xdia(mgs,ls,3) +    &
     &            da1(ls)*xdia(mgs,ls,3)**2 )

          qhlacs(mgs) = Min( ehls(mgs)*qhlacs0(mgs), qsmxd(mgs) )
        ENDIF
      ENDIF
      end do
      ENDIF


      do mgs = 1,ngscnt
      qhlacr(mgs) = 0.0
      qhlacrmlr(mgs) = 0.0
      chlacr(mgs) = 0.0
      vhlacr(mgs) = 0.0
      IF ( lhl .gt. 1 .and. temg(mgs) .gt. tfr ) raindn(mgs,lhl) = 1000.0

      IF ( lhl .gt. 1 .and. ehlr(mgs) .gt. 0.0 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,lr,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,lr,1) )

       qhlacr(mgs) = 0.25*pi*ehlr(mgs)*cx(mgs,lhl)*qx(mgs,lr)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab1lh(mgs,lr,lhl)*xdia(mgs,lhl,3)*xdia(mgs,lr,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 )
!       IF ( qhacr(mgs) .gt. 0. .or. tmp .gt. 0.0 ) write(0,*) 'qhacr= ',qhacr(mgs),tmp
!!        qhacr(mgs) = Min( qhacr(mgs), qrmxd(mgs) )
!!        chacr(mgs) = qhacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
!!        chacr(mgs) = min(chacr(mgs),crmxd(mgs))

        qhlacr(mgs) = Min( qhlacr(mgs), qxmxd(mgs,lr) )

     
        qhlacrmlr(mgs) = qhlacr(mgs)
        IF ( temg(mgs) > tfr ) THEN
        qhlacr(mgs) = 0.0
        ELSE
        chlacr(mgs) = 0.25*pi*ehlr(mgs)*cx(mgs,lhl)*cx(mgs,lr)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,lr)*xdia(mgs,lhl,3)*xdia(mgs,lr,3) +    &
     &            da0(lr)*xdia(mgs,lr,3)**2 )

        chlacr(mgs) = min(chlacr(mgs),crmxd(mgs))

        IF ( lvol(lhl) .gt. 1 ) THEN
         vhlacr(mgs) = rho0(mgs)*qhlacr(mgs)/raindn(mgs,lhl)
        ENDIF
        ENDIF
      ENDIF
      ENDIF
      end do



!
!
!
!
!      if (ndebug .gt. 0 ) write(0,*) 'Collection: Cloud collects xxxxx'

      if (ndebug .gt. 0 ) write(0,*) 'Collection: cloud ice collects xxxx2'
!
      do mgs = 1,ngscnt
      qiacw(mgs) = 0.0
      IF ( eiw(mgs) .gt. 0.0 ) THEN

       vt = Sqrt((vtxbar(mgs,li,1)-vtxbar(mgs,lc,1))**2 +    &
     &            0.04*vtxbar(mgs,li,1)*vtxbar(mgs,lc,1) )

          qiacw(mgs) = 0.25*pi*eiw(mgs)*cx(mgs,li)*qx(mgs,lc)*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab1(li,lc)*xdia(mgs,li,3)*xdia(mgs,lc,3) +    &
     &            da1(lc)*xdia(mgs,lc,3)**2 )

       qiacw(mgs) = Min( qiacw(mgs), qxmxd(mgs,lc) )
      ENDIF
      end do


!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: cloud ice collects xxxx8'
!
      do mgs = 1,ngscnt
      qiacr(mgs) = 0.0
      qiacrf(mgs) = 0.0
      qiacrs(mgs) = 0.0
      ciacrs(mgs) = 0.0
      ciacr(mgs) = 0.0
      ciacrf(mgs) = 0.0
      viacrf(mgs) = 0.0
      csplinter(mgs) = 0.0
      qsplinter(mgs) = 0.0
      csplinter2(mgs) = 0.0
      qsplinter2(mgs) = 0.0
      IF ( iacr .ge. 1 .and. eri(mgs) .gt. 0.0    &
     &     .and. temg(mgs) .le. 270.15 ) THEN
      IF ( ipconc .ge. 3 ) THEN
       ni = 0.0
         IF ( xdia(mgs,li,1) .ge. 10.e-6 ) THEN
          ni = ni + cx(mgs,li)*Exp(- (40.e-6/xdia(mgs,li,1))**3 )
         ENDIF
       IF ( imurain == 1 ) THEN ! gamma of diameter
           IF ( iacrsize /= 4 ) THEN
           IF ( iacrsize .eq. 1 ) THEN
             ratio = 500.e-6/xdia(mgs,lr,1)
           ELSEIF ( iacrsize .eq. 2 ) THEN
             ratio = 300.e-6/xdia(mgs,lr,1)
           ELSEIF ( iacrsize .eq. 3 ) THEN
             ratio = 40.e-6/xdia(mgs,lr,1)
           ELSEIF ( iacrsize .eq. 5 ) THEN
             ratio = 150.e-6/xdia(mgs,lr,1)
           ENDIF
           i = Min(nqiacrratio,Int(ratio*dqiacrratioinv))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr)))*dqiacralphainv)
           delx = ratio - float(i)*dqiacrratio
           dely = alpha(mgs,lr) - float(j)*dqiacralpha
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           ! interpolate along x, i.e., ratio
           tmp1 = ciacrratio(i,j) + delx*dqiacrratioinv*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*dqiacrratioinv*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))
           
           ! interpolate along alpha
           
           nr = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*cx(mgs,lr)
           
           ! interpolate along x, i.e., ratio; 
           tmp1 = qiacrratio(i,j) + delx*dqiacrratioinv*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*dqiacrratioinv*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           ! interpolate along alpha; 
           
           qr = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*qx(mgs,lr)
           
           ELSE ! iacrsize == 4 : use all
             nr = cx(mgs,lr)
             qr = qx(mgs,lr)
           ENDIF

          vt = Sqrt((vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))**2 +     &
     &            0.04*vtxbar(mgs,lr,1)*vtxbar(mgs,li,1) )

          qiacr(mgs) = 0.25*pi*eri(mgs)*ni*qr*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab1lh(mgs,lr,li)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da1(lr)*xdia(mgs,lr,3)**2 ) 
          
          qiacr(mgs) = Min( qrmxd(mgs), qiacr(mgs) )
          

          ciacr(mgs) = 0.25*pi*eri(mgs)*ni*nr*vt*   &
     &         (  da0(li)*xdia(mgs,li,3)**2 +     &
     &            dab0lh(mgs,lr,li)*xdia(mgs,lr,3)*xdia(mgs,li,3) +    &
     &            da0(lr)*xdia(mgs,lr,3)**2 ) 

          ciacr(mgs) = Min( crmxd(mgs), ciacr(mgs) )
          
!          write(iunit,*) 'qiacr: ',cx(mgs,lr),nr,qx(mgs,lr),qr,qiacr(mgs),ciacr(mgs)
!          write(iunit,*) 'xdia r li = ',xdia(mgs,lr,3),xdia(mgs,li,3),xdia(mgs,lr,1),xdia(mgs,li,1)
!          write(iunit,*) 'i,j,ratio = ',i,j,ciacrratio(i,j),qiacrratio(i,j)
!          write(iunit,*) 'ni,ci = ',ni,cx(mgs,li),qx(mgs,li)

       ELSEIF ( imurain == 3 ) THEN ! gamma of volume
!   Set nr to the number of drops greater than 40 microns.
         arg = 1000.*xdia(mgs,lr,3)
!         nr = cx(mgs,lr)*gaml02( arg )
!        IF ( iacr .eq. 1 ) THEN
         IF ( ipconc .ge. 3 ) THEN
           IF ( iacrsize .eq. 1 ) THEN
            nr = cx(mgs,lr)*gaml02d500( arg )  ! number greater than 500 microns in diameter
           ELSEIF ( iacrsize .eq. 2 .or. iacrsize .eq. 5 ) THEN
            nr = cx(mgs,lr)*gaml02d300( arg )  ! number greater than 300 microns in diameter
           ELSEIF ( iacrsize .eq. 3 ) THEN
            nr = cx(mgs,lr)*gaml02( arg ) ! number greater than 40 microns in diameter
           ELSEIF ( iacrsize .eq. 4 ) THEN
            nr = cx(mgs,lr) ! all raindrops
           ENDIF
         ELSE
         nr = cx(mgs,lr)*gaml02( arg )
         ENDIF
!        ELSEIF ( iacr .eq. 2 ) THEN
!         nr = cx(mgs,lr)*gaml02d300( arg )  ! number greater than 300 microns in diameter
!        ENDIF
       IF ( ni .gt. 0.0 .and. nr .gt. 0.0 ) THEN
       d0 = xdia(mgs,lr,3)
       qiacr(mgs) = xdn(mgs,lr)*rhoinv(mgs)*   &
     &     (0.217239*(0.522295*(d0**5) +    &
     &      49711.81*(d0**6) -    &
     &      1.673016e7*(d0**7)+    &
     &      2.404471e9*(d0**8) -    &
     &      1.22872e11*(d0**9))*ni*nr)
      qiacr(mgs) = Min( qrmxd(mgs), qiacr(mgs) )
      ciacr(mgs) =   &
     &   (0.217239*(0.2301947*(d0**2) +    &
     &      15823.76*(d0**3) -    &
     &      4.167685e6*(d0**4) +    &
     &      4.920215e8*(d0**5) -    &
     &      2.133344e10*(d0**6))*ni*nr)
      ciacr(mgs) = Min( crmxd(mgs), ciacr(mgs) )
!      ciacr(mgs) = qiacr(mgs)*cx(mgs,lr)/qx(mgs,lr)
      ENDIF
      ENDIF
       IF ( iacr .eq. 1 .or. iacr .eq. 3 ) THEN
         ciacrf(mgs) = Min(ciacr(mgs), qiacr(mgs)/(1.0*vr1mm*1000.0)*rho0(mgs) ) ! *rzxh(mgs)
       ELSEIF ( iacr .eq. 2 ) THEN
         ciacrf(mgs) = ciacr(mgs) ! *rzxh(mgs)
       ELSEIF ( iacr .eq. 4 ) THEN
         ciacrf(mgs) = Min(ciacr(mgs), qiacr(mgs)/(1.0*vfrz*1000.0)*rho0(mgs) ) ! *rzxh(mgs)
       ELSEIF ( iacr .eq. 5 ) THEN
         ciacrf(mgs) = ciacr(mgs)*rzxh(mgs)
       ENDIF 
!      crfrzf(mgs) = Min(crfrz(mgs), qrfrz(mgs)/(bfnu*27.0*vr1mm*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
       ENDIF
      
      
      ELSE ! single-moment rain
      qiacr(mgs) =    &
     &  min(        &
     &   ((0.25/gf4)*pi)*eri(mgs)*cx(mgs,li)*qx(mgs,lr)   &
     &  *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,li,1))   &
     &  *(  gf6*gf1*xdia(mgs,lr,2)    &
     &    + 2.0*gf5*gf2*xdia(mgs,lr,1)*xdia(mgs,li,1)    &
     &    + gf4*gf3*xdia(mgs,li,2) )     &
     &  , qrmxd(mgs))
      ENDIF
!      if ( temg(mgs) .gt. 268.15 ) then
!      qiacr(mgs) = 0.0
!      ciacr(mgs) = 0.0
!      end if

      IF ( ipconc .ge. 1 ) THEN
        IF ( nsplinter .ge. 0 ) THEN
          csplinter(mgs) = nsplinter*ciacr(mgs)
        ELSE
          csplinter(mgs) = -nsplinter*ciacrf(mgs)
        ENDIF
        qsplinter(mgs) = Min(0.1*qiacr(mgs), csplinter(mgs)*splintermass/rho0(mgs) ) ! makes splinters smaller if too much mass is taken from graupel
      ENDIF
      
      frach = 1.0
           IF ( ibiggsnow == 2 .or. ibiggsnow == 3 ) THEN
           IF ( ciacr(mgs) > qxmin(lh) ) THEN
           xvfrz = rho0(mgs)*qiacr(mgs)/(ciacr(mgs)*900.) ! mean volume of frozen drops; 900. for frozen drop density
           frach = 0.5 *(1. +  Tanh(0.2e12 *( xvfrz - 1.15*xvmn(lh))))

             qiacrs(mgs) = (1.-frach)*qiacr(mgs)
             ciacrs(mgs) = (1.-frach)*ciacr(mgs) ! *rzxh(mgs)
           
           ENDIF
           ENDIF

      qiacrf(mgs) = frach*qiacr(mgs)
      ciacrf(mgs) = frach*ciacrf(mgs)

      IF ( lvol(lh) > 1 ) THEN
         viacrf(mgs) = rho0(mgs)*qiacrf(mgs)/rhofrz
      ENDIF
      
      end do
!
!
!
!

! snow aggregation here
      if ( ipconc .ge. 4 ) then !
      do mgs = 1,ngscnt
      csacs(mgs) = 0.0
      IF ( qx(mgs,ls) > qxmin(ls) .and. ess(mgs) .gt. 0.0 .and. xv(mgs,ls) < 0.25*xvmx(ls)*Max(1.,100./Min(100.,xdn(mgs,ls)))  ) THEN
      csacs(mgs) = rvt*aa2*ess(mgs)*cx(mgs,ls)**2*xv(mgs,ls) *Min(1.,xdn(mgs,ls)/100. ) ! Min func tries to recalibrate for low diagnosed density 
      csacs(mgs) = min(csacs(mgs),csmxd(mgs))
      ENDIF
      end do
      end if
!
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 11'
      if ( ipconc .ge. 2 .or. ipelec .ge. 9 ) then
      do mgs = 1,ngscnt
      ciacw(mgs) = 0.0
      IF ( eiw(mgs) .gt. 0.0 ) THEN

        ciacw(mgs) = qiacw(mgs)*rho0(mgs)/xmas(mgs,lc)
      ciacw(mgs) = min(ciacw(mgs),ccmxd(mgs))
      ENDIF
      end do

      end if

      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 18'
      if ( ipconc .ge. 2 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
       cracw(mgs) = 0.0
       cracr(mgs) = 0.0
       ec0(mgs) = 1.e9
      IF ( qx(mgs,lc) .gt. qxmin(lc) .and. qx(mgs,lr) .gt. qxmin(lr)    &
     &      .and. qracw(mgs) .gt. 0.0 ) THEN

       IF ( ipconc .lt. 3 ) THEN
        IF ( erw(mgs) .gt. 0.0 ) THEN
        cracw(mgs) =   &
     &   ((0.25)*pi)*erw(mgs)*cx(mgs,lc)*cx(mgs,lr)   &
     &  *abs(vtxbar(mgs,lr,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf1*xdia(mgs,lc,2)   &
     &    + 2.0*gf2*xdia(mgs,lc,1)*xdia(mgs,lr,1)   &
     &    + gf3*xdia(mgs,lr,2) )
        ENDIF
       ELSE ! IF ( ipconc .ge. 3 .and. 
        IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN  !{
        IF ( 0.5*xdia(mgs,lr,3) .gt. rh(mgs) ) THEN ! { .or. cx(mgs,lr) .gt. nh(mgs) 
!        IF ( qx(mgs,lc) .gt. qxmin(lc) .and. qx(mgs,lr) .gt. qxmin(lr) ) THEN
          IF ( 0.5*xdia(mgs,lr,3) .gt. rwradmn ) THEN ! r > 50.e-6 
!          DM0CCC=A2*XNC*XNR*(XVC+XVR)                               ! (A11)
!         NOTE: murain drops out, so same result for imurain = 1 and 3
            cracw(mgs) = aa2*cx(mgs,lr)*cx(mgs,lc)*(xv(mgs,lc) + xv(mgs,lr))
          ELSE
            IF ( imurain == 3 ) THEN
!          DM0CCC=A1*XNC*XNR*(((CNU+2.)/(CNU+1.))*XVC**2+((RNU+2.)/(RNU+1.))*XVR**2) ! (A13)
            cracw(mgs) = aa1*cx(mgs,lr)*cx(mgs,lc)*   &
     &          ((cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.) +    &
     &          (alpha(mgs,lr) + 2.)*xv(mgs,lr)**2/(alpha(mgs,lr) + 1.))
            ELSE ! imurain == 1 USE CP00 for rain DSD in diameter
            cracw(mgs) = aa1*cx(mgs,lr)*cx(mgs,lc)*   &
     &          ((cnu + 2.)*xv(mgs,lc)**2/(cnu + 1.) +    &
     &          (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)*xv(mgs,lr)**2/  &
     &             ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.)) )
            ENDIF ! imurain
          ENDIF
        ENDIF ! } rh
        ENDIF ! } dmrauto
       ENDIF ! ipconc
      ENDIF ! qc > qcmin & qr > qrmin
        
! Rain self collection (cracr) and break-up (factor of ec0)
!
!       
        ec0(mgs) = 2.e9
        IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
        rwrad = 0.5*xdia(mgs,lr,3)
        IF ( xdia(mgs,lr,3) .gt. 2.0e-3 ) THEN
          ec0(mgs) = 0.0
          cracr(mgs) = 0.0
        ELSE
         IF ( dmrauto <= 0 .or.  rho0(mgs)*qx(mgs,lr) > 1.2*xl2p(mgs) ) THEN 
          IF ( xdia(mgs,lr,3) .lt. 6.1e-4 ) THEN
            ec0(mgs) = 1.0
          ELSE
            ec0(mgs) = Exp(-50.0*(50.0*(xdia(mgs,lr,3) - 6.0e-4)))
          ENDIF
          

          IF ( rwrad .ge. 50.e-6 ) THEN
              cracr(mgs) = ec0(mgs)*aa2*cx(mgs,lr)**2*xv(mgs,lr)
          ELSE
            IF ( imurain == 3 ) THEN
             cracr(mgs) = ec0(mgs)*aa1*(cx(mgs,lr)*xv(mgs,lr))**2*   &
     &                   (alpha(mgs,lr) + 2.)/(alpha(mgs,lr) + 1.)
            ELSE ! imurain == 1
             cracr(mgs) = ec0(mgs)*aa1*(cx(mgs,lr)*xv(mgs,lr))**2*   &
     &                   (alpha(mgs,lr) + 6.)*(alpha(mgs,lr) + 5.)*(alpha(mgs,lr) + 4.)/ &
     &                  ((alpha(mgs,lr) + 3.)*(alpha(mgs,lr) + 2.)*(alpha(mgs,lr) + 1.))
              
            ENDIF
          ENDIF
!          cracr(mgs) = Min(cracr(mgs),crmxd(mgs))
         ENDIF
        ENDIF
        ENDIF

!      cracw(mgs) = min(cracw(mgs),ccmxd(mgs))
      end do
      end if
!
!
!
!  Graupel
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22ii'
      chacw(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt

      IF ( ipconc .ge. 5 ) THEN
       IF ( qhacw(mgs) .gt. 0.0 .and. xmas(mgs,lc) .gt. 0.0 ) THEN

!  This is the explict version of chacw, which turns out to be very close to the
!  approximation that the droplet size does not change, to within a few percent.
!  This may _not_ be the case for cnu other than zero!
!          chacw(mgs) = (ehw(mgs)*cx(mgs,lc)*cx(mgs,lh)*(pi/4.)*
!     :    abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))*
!     :    (2.0*xdia(mgs,lh,1)*(xdia(mgs,lh,1) +
!     :         xdia(mgs,lc,1)*gf43rds) +
!     :      xdia(mgs,lc,2)*gf53rds))

!          chacw(mgs) = Min( chacw(mgs), 0.6*cx(mgs,lc)/dtp )

!        chacw(mgs) = qhacw(mgs)*rho0(mgs)/xmas(mgs,lc)
        chacw(mgs) = qhacw(mgs)*rho0(mgs)/xmascw(mgs)
!        chacw(mgs) = min(chacw(mgs),cxmxd(mgs,lc))
        chacw(mgs) = Min( chacw(mgs), 0.5*cx(mgs,lc)/dtp )
       ELSE
        qhacw(mgs) = 0.0
       ENDIF
      ELSE
      chacw(mgs) =   &
     &   ((0.25)*pi)*ehw(mgs)*cx(mgs,lc)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,lc,1))   &
     &  *(  gf1*xdia(mgs,lc,2)   &
     &    + 2.0*gf2*xdia(mgs,lc,1)*xdia(mgs,lh,1)   &
     &    + gf3*xdia(mgs,lh,2) )
      chacw(mgs) = min(chacw(mgs),0.5*cx(mgs,lc)/dtp)
!      chacw(mgs) = min(chacw(mgs),cxmxd(mgs,lc))
!      chacw(mgs) = min(chacw(mgs),ccmxd(mgs))
      ENDIF
      end do
      end if
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22kk'
      chaci(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( ehi(mgs) .gt. 0.0 .or. ( ehiclsn(mgs) > 0.0 .and. ipelec > 0 )) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,li,1) )

          chaci0(mgs) = 0.25*pi*ehiclsn(mgs)*cx(mgs,lh)*cx(mgs,li)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab0lh(mgs,li,lh)*xdia(mgs,lh,3)*xdia(mgs,li,3) +    &
     &            da0(li)*xdia(mgs,li,3)**2 )

       ELSE
        chaci0(mgs) =   &
     &   ((0.25)*pi)*ehiclsn(mgs)*cx(mgs,li)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,li,1))   &
     &  *(  gf1*xdia(mgs,li,2)   &
     &    + 2.0*gf2*xdia(mgs,li,1)*xdia(mgs,lh,1)   &
     &    + gf3*xdia(mgs,lh,2) )
        ENDIF

        chaci(mgs) = min(ehi(mgs)*chaci0(mgs),cimxd(mgs))
       ENDIF
      end do
      end if
!
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22nn'
      chacs(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( ehs(mgs) .gt. 0 ) THEN
       IF ( ipconc .ge. 5 .or. ( ehsclsn(mgs) > 0.0 .and. ipelec > 0 ) ) THEN

       vt = Sqrt((vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lh,1)*vtxbar(mgs,ls,1) )

          chacs0(mgs) = 0.25*pi*ehsclsn(mgs)*cx(mgs,lh)*cx(mgs,ls)*vt*   &
     &         (  da0lh(mgs)*xdia(mgs,lh,3)**2 +     &
     &            dab0lh(mgs,ls,lh)*xdia(mgs,lh,3)*xdia(mgs,ls,3) +    &
     &            da0(ls)*xdia(mgs,ls,3)**2 )

       ELSE
      chacs0(mgs) =   &
     &   ((0.25)*pi)*ehsclsn(mgs)*cx(mgs,ls)*cx(mgs,lh)   &
     &  *abs(vtxbar(mgs,lh,1)-vtxbar(mgs,ls,1))   &
     &  *(  gf3*gf1*xdia(mgs,ls,2)   &
     &    + 2.0*gf2*gf2*xdia(mgs,ls,1)*xdia(mgs,lh,1)   &
     &    + gf1*gf3*xdia(mgs,lh,2) )
      ENDIF
      chacs(mgs) = min(ehs(mgs)*chacs0(mgs),csmxd(mgs))
      ENDIF
      end do
      end if


!
!
!  Hail
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22ii'
      chlacw(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt

      IF ( lhl .gt. 1 .and. ipconc .ge. 5 ) THEN
       IF ( qhlacw(mgs) .gt. 0.0 .and. xmas(mgs,lc) .gt. 0.0 ) THEN

!  This is the explict version of chacw, which turns out to be very close to the
!  approximation that the droplet size does not change, to within a few percent.
!  This may _not_ be the case for cnu other than zero!
!          chlacw(mgs) = (ehlw(mgs)*cx(mgs,lc)*cx(mgs,lhl)*(pi/4.)*
!     :    abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,lc,1))*
!     :    (2.0*xdia(mgs,lhl,1)*(xdia(mgs,lhl,1) +
!     :         xdia(mgs,lc,1)*gf43rds) +
!     :      xdia(mgs,lc,2)*gf53rds))

!          chlacw(mgs) = Min( chlacw(mgs), 0.6*cx(mgs,lc)/dtp )

!        chlacw(mgs) = qhlacw(mgs)*rho0(mgs)/xmas(mgs,lc)
        chlacw(mgs) = qhlacw(mgs)*rho0(mgs)/xmascw(mgs)
!        chlacw(mgs) = min(chlacw(mgs),cxmxd(mgs,lc))
        chlacw(mgs) = Min( chlacw(mgs), 0.5*cx(mgs,lc)/dtp )
       ELSE
        qhlacw(mgs) = 0.0
       ENDIF
!      ELSE
!      chlacw(mgs) =
!     >   ((0.25)*pi)*ehlw(mgs)*cx(mgs,lc)*cx(mgs,lhl)
!     >  *abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,lc,1))
!     >  *(  gf1*xdia(mgs,lc,2)
!     >    + 2.0*gf2*xdia(mgs,lc,1)*xdia(mgs,lhl,1)
!     >    + gf3*xdia(mgs,lhl,2) )
!      chlacw(mgs) = min(chlacw(mgs),0.5*cx(mgs,lc)/dtp)
!      chlacw(mgs) = min(chlacw(mgs),cxmxd(mgs,lc))
!      chlacw(mgs) = min(chlacw(mgs),ccmxd(mgs))
      ENDIF
      end do
      end if
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22kk'
      chlaci(:) = 0.0
      chlaci0(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( lhl .gt. 1 .and. ( ehli(mgs) .gt. 0.0 .or. (ipelec > 0 .and. ehliclsn(mgs) > 0.0) )  ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,li,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,li,1) )

          chlaci0(mgs) = 0.25*pi*ehliclsn(mgs)*cx(mgs,lhl)*cx(mgs,li)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,li)*xdia(mgs,lhl,3)*xdia(mgs,li,3) +    &
     &            da0(li)*xdia(mgs,li,3)**2 )

!       ELSE
!        chlaci(mgs) =
!     >   ((0.25)*pi)*ehli(mgs)*cx(mgs,li)*cx(mgs,lhl)
!     >  *abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,li,1))
!     >  *(  gf1*xdia(mgs,li,2)
!     >    + 2.0*gf2*xdia(mgs,li,1)*xdia(mgs,lhl,1)
!     >    + gf3*xdia(mgs,lhl,2) )
        ENDIF

        chlaci(mgs) = min(ehli(mgs)*chlaci0(mgs),cimxd(mgs))
       ENDIF
      end do
      end if
!
!
      if (ndebug .gt. 0 ) write(0,*) 'ICEZVD_GS: conc 22jj'
      chlacs(:) = 0.0
      chlacs0(:) = 0.0
      if ( ipconc .ge. 1 .or. ipelec .ge. 1 ) then
      do mgs = 1,ngscnt
      IF ( lhl .gt. 1 .and. ( ehls(mgs) .gt. 0.0 .or. (ipelec > 0 .and. ehlsclsn(mgs) > 0.0) ) ) THEN
       IF ( ipconc .ge. 5 ) THEN

       vt = Sqrt((vtxbar(mgs,lhl,1)-vtxbar(mgs,ls,1))**2 +    &
     &            0.04*vtxbar(mgs,lhl,1)*vtxbar(mgs,ls,1) )

          chlacs0(mgs) = 0.25*pi*ehlsclsn(mgs)*cx(mgs,lhl)*cx(mgs,ls)*vt*   &
     &         (  da0lhl(mgs)*xdia(mgs,lhl,3)**2 +     &
     &            dab0(lhl,ls)*xdia(mgs,lhl,3)*xdia(mgs,ls,3) +    &
     &            da0(ls)*xdia(mgs,ls,3)**2 )

!       ELSE
!      chlacs(mgs) =
!     >   ((0.25)*pi)*ehls(mgs)*cx(mgs,ls)*cx(mgs,lhl)
!     >  *abs(vtxbar(mgs,lhl,1)-vtxbar(mgs,ls,1))
!     >  *(  gf3*gf1*xdia(mgs,ls,2)
!     >    + 2.0*gf2*gf2*xdia(mgs,ls,1)*xdia(mgs,lhl,1)
!     >    + gf1*gf3*xdia(mgs,lhl,2) )
      ENDIF
      chlacs(mgs) = min(ehls(mgs)*chlacs0(mgs),csmxd(mgs))
      ENDIF
      end do
      end if

!
! Ziegler (1985) autoconversion
!
!
      IF ( ipconc .ge. 2 .and. ircnw /= -1) THEN ! DTD: added flag for autoconversion.  If -1, turns off autoconversion
      if (ndebug .gt. 0 ) write(0,*) 'conc 26a'
      
      DO mgs = 1,ngscnt
        zrcnw(mgs) = 0.0
        qrcnw(mgs) = 0.0
        crcnw(mgs) = 0.0
        cautn(mgs) = 0.0
      ENDDO
      
      DO mgs = 1,ngscnt
!      qracw(mgs) = 0.0
!      cracw(mgs) = 0.0
       IF ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 1000. .and. temg(mgs) .gt. tfrh+4.) THEN
       ! .and. w(igs(mgs),jgs,kgs(mgs)) > 5.0) THEN ! DTD: added w threshold for testing                                                                                                            
         volb = xv(mgs,lc)*(1./(1.+CNU))**(1./2.)
         cautn(mgs) = Min(ccmxd(mgs),   &
     &      ((CNU+2.)/(CNU+1.))*aa1*cx(mgs,lc)**2*xv(mgs,lc)**2)
         cautn(mgs) = Max( 0.0d0, cautn(mgs) )
         IF ( rb(mgs) .le. 7.51d-6 ) THEN
           t2s = 1.d30
!           cautn(mgs) = 0.0
         ELSE
!         XL2P=2.7E-2*XNC*XVC*((1.E12*RB**3*RC)-0.4)
         
!        T2S=3.72E-3/(((1.E4*RB)-7.5)*XNC*XVC) 
!           t2s = 3.72E-3/(((1.e6*rb)-7.5)*cx(mgs,lc)*xv(mgs,lc))
!           t2s = 3.72/(((1.e6*rb(mgs))-7.5)*rho0(mgs)*qx(mgs,lc))
           t2s = 3.72/(1.e6*(rb(mgs)-7.500d-6)*rho0(mgs)*qx(mgs,lc))

           qrcnw(mgs) = Max( 0.0d0, xl2p(mgs)/(t2s*rho0(mgs)) )
           crcnw(mgs) = Max( 0.0d0, Min(3.5e9*xl2p(mgs)/t2s,0.5*cautn(mgs)) )
           
           IF ( dmrauto == 0 ) THEN
             IF ( qx(mgs,lr)*rho0(mgs) > 1.2*xl2p(mgs) .and. cx(mgs,lr) > cxmin ) THEN ! Cohard and Pinty (2000a) switch over from (18) to (19)
               crcnw(mgs) = cx(mgs,lr)/qx(mgs,lr)*qrcnw(mgs)
             ENDIF
           ELSEIF ( dmrauto == 1  .and. cx(mgs,lr) > cxmin) THEN
             IF ( qx(mgs,lr) > qxmin(lr) ) THEN
               tmp = qrcnw(mgs)*cx(mgs,lr)/qx(mgs,lr)
               crcnw(mgs) = Min(tmp,crcnw(mgs) )
             ENDIF
           ELSEIF ( dmrauto == 2  .and. cx(mgs,lr) > cxmin) THEN
               tmp = crcnw(mgs)
               tmp2 = qrcnw(mgs)*cx(mgs,lr)/qx(mgs,lr)
               ! try mass-weighted average of old and new Dmr
               crcnw(mgs) = (tmp*qrcnw(mgs)+tmp2*qx(mgs,lr))/(qrcnw(mgs)+qx(mgs,lr))
           ELSEIF ( dmrauto == 3  .and. cx(mgs,lr) > cxmin) THEN ! adapted from MY/CP code
              tmp = Max( 2.d0*rh(mgs), dble( xdia(mgs,lr,3) ) )
              crcnw(mgs) = rho0(mgs)*qrcnw(mgs)/(pi/6.*1000.*tmp**3)
           ENDIF
           
           IF ( crcnw(mgs) < 1.e-30 ) qrcnw(mgs) = 0.0

!           IF (  crcnw(mgs) .gt. cautn(mgs) .and. crcnw(mgs) .gt. 1.0 )
!     :          THEN
!             write(0,*)  'crcnw,cautn ',crcnw(mgs)/cautn(mgs),
!     :          crcnw(mgs),cautn(mgs),igs(mgs),kgs(mgs),t2s,qx(mgs,lr)
!             write(0,*)  '            ',qx(mgs,lc),cx(mgs,lc),0.5e6*xdia(mgs,lc,1)
!             write(0,*)  '            ',rho0(mgs)*qrcnw(mgs)/crcnw(mgs),
!     :         1.e6*(( 3/(4.*pi))*rho0(mgs)*qrcnw(mgs)/
!     :       (crcnw(mgs)*xdn(mgs,lr)))**(1./3.),rh(mgs)*1.e6,rwrad(mgs)
!           ELSEIF ( crcnw(mgs) .gt. 1.0 .and. cautn(mgs) .gt. 0.) THEN
!             write(0,*)  'crcnw,cautn ',crcnw(mgs)/cautn(mgs),
!     :          crcnw(mgs),cautn(mgs),igs(mgs),kgs(mgs),t2s
!             write(0,*)  '            ',rho0(mgs)*qrcnw(mgs)/crcnw(mgs),
!     :  1.e6*(( 3*pi/4.)*rho0(mgs)*qrcnw(mgs)/
!     :   (crcnw(mgs)*xdn(mgs,lr)))**(1./3.)
!           ENDIF
!           crcnw(mgs) = Min(cautn(mgs),3.5e9*xl2p(mgs)/t2s)

!           IF ( qrcnw(mgs) .gt. 0.3e-2 ) THEN
!            write(0,*)  'QRCNW'
!            write(0,*)  qrcnw(mgs),crcnw(mgs),cautn(mgs)
!            write(0,*)  xl2p,t2s,rho0(mgs),xv(mgs,lc),cx(mgs,lc),qx(mgs,lc)
!            write(0,*)  rb,0.5*xdia(mgs,lc,1),mgs,igs(mgs),kgs(mgs)
!           ENDIF
!           qrcnw(mgs) = Min(qrcnw(mgs),qcmxd(mgs))
         ENDIF


       ENDIF
      ENDDO



      ELSE

!
!  Berry 1968 auto conversion for rain (Orville & Kopp 1977)
!
!
      if ( ircnw .eq. 4 ) then
      do mgs = 1,ngscnt
!      sconvmix(lcw,mgs) = 0.0
      qrcnw(mgs) =  0.0
      qdiff = max((qx(mgs,lc)-qminrncw),0.0)
      if ( qdiff .gt. 0.0 .and. xdia(mgs,lc,1) .gt. 20.0e-6 ) then
      argrcnw =   &
     &  ((1.2e-4)+(1.596e-12)*(cx(mgs,lc)*1.0e-6)   &
     &  /(cwdisp*qdiff*1.0e-3*rho0(mgs)))
      qrcnw(mgs) = (rho0(mgs)*1e-3)*(qdiff**2)/argrcnw
!      sconvmix(lcw,mgs) = max(sconvmix(lcw,mgs),0.0)
      qrcnw(mgs) = (max(qrcnw(mgs),0.0))
      end if
      end do

      ENDIF
!
!
!
!  Berry 1968 auto conversion for rain (Ferrier 1994)
!
!
      if ( ircnw .eq. 5 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      qrcnw(mgs) =  0.0
      qccrit = (pi/6.)*(cx(mgs,lc)*cwdiap**3)*xdn(mgs,lc)/rho0(mgs)
      qdiff = max((qx(mgs,lc)-qccrit),0.)
      if ( qdiff .gt. 0.0 .and. cx(mgs,lc) .gt. 1.0 ) then
      argrcnw = &
!     >  ((1.2e-4)+(1.596e-12)*cx(mgs,lc)/(cwdisp*rho0(mgs)*qdiff))   &
     &  ((1.2e-4)+(1.596e-12)*cx(mgs,lc)*1.0e-3/(cwdisp*rho0(mgs)*qdiff))
      qrcnw(mgs) = &
!     >  timflg(mgs)*rho0(mgs)*(qdiff**2)/argrcnw   &
     &  1.0e-3*rho0(mgs)*(qdiff**2)/argrcnw
      qrcnw(mgs) = Min(qxmxd(mgs,lc), (max(qrcnw(mgs),0.0)) )

!      write(iunit,*) 'qrcnw,cx =',qrcnw(mgs),cx(mgs,lc),mgs,1.e3*qx(mgs,lc),cno(lr)
      end if
      end do
      end if

!
!
!  kessler auto conversion for rain.
!
      if ( ircnw .eq. 2 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      qrcnw(mgs) = (0.001)*max((qx(mgs,lc)-qminrncw),0.0)
      end do
      end if
!
!  c4 = pi/6
!  c1 = 0.12-0.32 for colorado storms...typically 0.3-0.4
!  berry reinhart type conversion (proctor 1988)
!
      if ( ircnw .eq. 1 ) then
      do mgs = 1,ngscnt
      qrcnw(mgs) = 0.0
      c1 = 0.2
      c4 = pi/(6.0)
      bradp =    &
     & (1.e+06) * ((c1/(0.38))**(1./3.)) * (xdia(mgs,lc,1)*(0.5))
      bl2 =   &
     & (0.027) * ((100.0)*(bradp**3)*(xdia(mgs,lc,1)*(0.5)) - (0.4))
      bt2 = (bradp -7.5) / (3.72)
      qrcnw(mgs) = 0.0
      if ( bl2 .gt. 0.0 .and. bt2 .gt. 0.0 ) then
      qrcnw(mgs) = bl2 * bt2 * rho0(mgs)   &
     &  * qx(mgs,lc) * qx(mgs,lc)
      end if
      end do
      end if



      ENDIF  !  ( ipconc .ge. 2 )

!
!
!
!  Bigg Freezing of Rain
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 27a'
      qrfrz(:) = 0.0
      qrfrzs(:) = 0.0
      qrfrzf(:) = 0.0
      vrfrzf(:) = 0.0
      crfrz(:) = 0.0
      crfrzs(:) = 0.0
      crfrzf(:) = 0.0
      zrfrz(:)  = 0.0
      zrfrzs(:)  = 0.0
      zrfrzf(:)  = 0.0
      qwcnr(:) = 0.0
      
      IF ( .not. ( ipconc == 0 .and. lwsm6 ) ) THEN
      
      do mgs = 1,ngscnt 
      if ( qx(mgs,lr) .gt. qxmin(lr) .and. temcg(mgs) .lt. -5. .and. ibiggopt > 0 ) then
!      brz = 100.0
!      arz = 0.66
       IF ( ipconc .lt. 3 ) THEN
       qrfrz(mgs) =    &
     &  min(   &
     &  (20.0)*(pi**2)*brz*(xdn(mgs,lr)/rho0(mgs))   &
     &   *cx(mgs,lr)*(xdia(mgs,lr,1)**6)   &
     &   *(exp(max(-arz*temcg(mgs), 0.0))-1.0)   &
     &  , qrmxd(mgs))
        qrfrzf(mgs) = qrfrz(mgs)

!       ELSEIF ( ipconc .ge. 3 .and. xv(mgs,lr) .gt. 1.1*xvmn(lr) ) THEN
       ELSEIF ( ipconc .ge. 3 ) THEN
!         tmp = brz*cx(mgs,lr)*(Exp(Max( -arz*temcg(mgs), 0.0 )) - 1.0)
!         crfrz(mgs) = xv(mgs,lr)*tmp

         frach = 1.0d0
         
!         IF ( ibiggopt == 2 .and. imurain == 1 .and. lzr < 1 ) THEN ! lzr check because results are weird for 3-moment
         IF ( ibiggopt == 2 .and. imurain == 1 ) THEN !
         ! integrate from Bigg diameter (for given supercooling Ts) to infinity
           
           volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 !  Ts == -temcg ; volt comes from the fit in Fig. 1 in Bigg 1953 
                                               ! for mean temperature for freezing: -ln (V) = a*Ts - b
                                               ! volt is given in cm**3, so convert to m**3
           dbigg = (6./pi* volt )**(1./3.) 
           
           ! perhaps should also test that W > V_t_dbigg, i.e., that drops the size of dbigg are being lifted and cooled. 
           
             ratio = Min(maxratiolu, dbigg/xdia(mgs,lr,1) )
           
           i = Min(nqiacrratio,Int(ratio*dqiacrratioinv))
!           j = Int(Max(0.0,Min(15.,alpha(mgs,lr))))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr)))*dqiacralphainv)
           delx = ratio - float(i)*dqiacrratio
           dely = alpha(mgs,lr) - float(j)*dqiacralpha
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           ! interpolate along x, i.e., ratio; 
           tmp1 = ciacrratio(i,j) + delx*dqiacrratioinv*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*dqiacrratioinv*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))
           
           ! interpolate along alpha; 
           
           crfrz(mgs) = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*cx(mgs,lr)/dtp
           crfrzf(mgs) = crfrz(mgs)
           ! interpolate along x, i.e., ratio; 
           tmp1 = qiacrratio(i,j) + delx*dqiacrratioinv*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*dqiacrratioinv*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           ! interpolate along alpha; 
           
           qrfrz(mgs) = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*qx(mgs,lr)/dtp
           qrfrzf(mgs) = qrfrz(mgs)
           
           
           IF ( dbigg < Max(dfrz,dhmn) .and. ( ibiggsnow == 1 .or. ibiggsnow == 3 ) ) THEN ! { convert some to snow or ice crystals
            ! temporarily store qrfrz and crfrz in snow terms and caclulate new crfrzf, qrfrzf, and zrfrzf. Leave crfrz etc. alone!
            crfrzs(mgs) = crfrz(mgs)
            qrfrzs(mgs) = qrfrz(mgs)
            
           ! recalculate using dhmn for ratio
           ratio = Min( maxratiolu, Max(dfrz,dhmn)/xdia(mgs,lr,1) )
           
           i = Min(nqiacrratio,Int(ratio*dqiacrratioinv))
!           j = Int(Max(0.0,Min(15.,alpha(mgs,lr))))
           j = Int(Max(0.0,Min(15.,alpha(mgs,lr)))*dqiacralphainv)
           delx = ratio - float(i)*dqiacrratio
           dely = alpha(mgs,lr) - float(j)*dqiacralpha
           ip1 = Min( i+1, nqiacrratio )
           jp1 = Min( j+1, nqiacralpha )

           ! interpolate along x, i.e., ratio; 
           tmp1 = ciacrratio(i,j) + delx*dqiacrratioinv*(ciacrratio(ip1,j) - ciacrratio(i,j))
           tmp2 = ciacrratio(i,jp1) + delx*dqiacrratioinv*(ciacrratio(ip1,jp1) - ciacrratio(i,jp1))


           ! interpolate along alpha; 
           
           crfrzf(mgs) = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*cx(mgs,lr)/dtp
           
           ! interpolate along x, i.e., ratio; 
           tmp1 = qiacrratio(i,j) + delx*dqiacrratioinv*(qiacrratio(ip1,j) - qiacrratio(i,j))
           tmp2 = qiacrratio(i,jp1) + delx*dqiacrratioinv*(qiacrratio(ip1,jp1) - qiacrratio(i,jp1))
           
           ! interpolate along alpha; 
           
           qrfrzf(mgs) = (tmp1 + dely*dqiacralphainv*(tmp2 - tmp1))*qx(mgs,lr)/dtp

           ! now subtract off the difference
            crfrzs(mgs) = crfrzs(mgs) - crfrzf(mgs)
            qrfrzs(mgs) = qrfrzs(mgs) - qrfrzf(mgs)

           
           ELSE
            crfrzs(mgs) = 0.0
            qrfrzs(mgs) = 0.0
            zrfrzs(mgs) = 0.0
           ENDIF ! }
           
           IF ( (qrfrz(mgs))*dtp > qx(mgs,lr) ) THEN
             fac = ( qrfrz(mgs) )*dtp/qx(mgs,lr)
             qrfrz(mgs) = fac*qrfrz(mgs)
             qrfrzs(mgs) = fac*qrfrzs(mgs)
             qrfrzf(mgs) = fac*qrfrzf(mgs)
             crfrz(mgs) = fac*crfrz(mgs)
             crfrzs(mgs) = fac*crfrzs(mgs)
             crfrzf(mgs) = fac*crfrzf(mgs)
             IF ( lzr > 1 ) THEN
               zrfrz(mgs) = fac*zrfrz(mgs)
               zrfrzf(mgs) = fac*zrfrzf(mgs)
             ENDIF
           ENDIF
!           IF ( (crfrzs(mgs) + crfrz(mgs))*dtp > cx(mgs,lr) ) THEN
!             fac = ( crfrzs(mgs) + crfrz(mgs) )*dtp/cx(mgs,lr)
!             crfrz(mgs) = fac*crfrz(mgs)
!             crfrzs(mgs) = fac*crfrzs(mgs)
!           ENDIF
           
!           qrfrzf(mgs) = qrfrz(mgs)
!           crfrzf(mgs) = crfrz(mgs)
           
   !        qrfrz(mgs) = qrfrzf(mgs) + qrfrzs(mgs)
   !        crfrz(mgs) = crfrzf(mgs) + crfrzs(mgs)

           
         ELSE ! ibiggopt == 1 
         
         tmp = xv(mgs,lr)*brz*cx(mgs,lr)*(Exp(Max( -arz*temcg(mgs), 0.0 )) - 1.0)
         IF ( .false. .and. tmp .gt. cxmxd(mgs,lr) ) THEN ! {
!           write(iunit,*) 'Bigg Freezing problem!',mgs,igs(mgs),kgs(mgs)
!           write(iunit,*)  'tmp, cx(lr), xv = ',tmp, cx(mgs,lr), xv(mgs,lr), (Exp(Max( -arz*temcg(mgs), 0.0 )) - 1.0)
!           write(iunit,*)  'qr,temcg = ',qx(mgs,lr)*1000.,temcg(mgs)
           crfrz(mgs) = cxmxd(mgs,lr) ! cx(mgs,lr)/dtp
           qrfrz(mgs) = qxmxd(mgs,lr) ! qx(mgs,lr)/dtp
!           STOP
         ELSE ! } {
         crfrz(mgs) = tmp
 !        crfrzfmx = cx(mgs,lr)*Exp(-4./3.*pi*(40.e-6)**3/xv(mgs,lr))
 !        IF ( crfrz(mgs) .gt. crfrzmx ) THEN
 !          crfrz(mgs) = crfrzmx
 !          qrfrz(mgs) = bfnu*xmas(mgs,lr)*rhoinv(mgs)*crfrzmx
 !          qwcnr(mgs) = cx(mgs,lr) - crfrzmx
 !        ELSE
         IF ( lzr < 1 ) THEN
           IF ( imurain == 3 ) THEN
             bfnu = bfnu0
           ELSE !imurain == 1
             bfnu = bfnu1
           ENDIF
         ELSE
 !         bfnu = 1.0 ! (alpha(mgs,lr)+2.0)/(alpha(mgs,lr)+1.)
           IF ( imurain == 3 ) THEN
             bfnu = (alpha(mgs,lr)+2.0)/(alpha(mgs,lr)+1.)
           ELSE !imurain == 1
!             bfnu = bfnu1
            bfnu = (4. + alpha(mgs,lr))*(5. + alpha(mgs,lr))*(6. + alpha(mgs,lr))/  &
     &            ((1. + alpha(mgs,lr))*(2. + alpha(mgs,lr))*(3. + alpha(mgs,lr)))
!            bfnu = 1.
           ENDIF
         ENDIF 
         qrfrz(mgs) = bfnu*xmas(mgs,lr)*rhoinv(mgs)*crfrz(mgs)

         qrfrz(mgs) = Min( qrfrz(mgs), 1.*qx(mgs,lr)/dtp ) ! qxmxd(mgs,lr) 
         crfrz(mgs) = Min( crfrz(mgs), 1.*cx(mgs,lr)/dtp ) !cxmxd(mgs,lr) 
         qrfrz(mgs) = Min( qrfrz(mgs), qx(mgs,lr) )
         qrfrzf(mgs) = qrfrz(mgs)
         ENDIF !}

         
         
         
         IF ( crfrz(mgs) .gt. qxmin(lh) ) THEN !{
!          IF ( xdia(mgs,lr,1) .lt. 200.e-6 ) THEN
!           IF ( xv(mgs,lr) .lt. xvmn(lh) ) THEN
           
           IF ( (ibiggsnow == 1 .or. ibiggsnow == 3 ) .and. ibiggopt /= 2 ) THEN
           xvfrz = rho0(mgs)*qrfrz(mgs)/(crfrz(mgs)*900.) ! mean volume of frozen drops; 900. for frozen drop density
           frach = 0.5 *(1. +  Tanh(0.2e12 *( xvfrz - 1.15*xvmn(lh))))

             qrfrzs(mgs) = (1.-frach)*qrfrz(mgs)
             crfrzs(mgs) = (1.-frach)*crfrz(mgs) ! *rzxh(mgs)
!             qrfrzf(mgs) = frach*qrfrz(mgs)
           
           ENDIF
           
           IF ( ipconc .ge. 14 .and. 1.e-3*rho0(mgs)*qrfrz(mgs)/crfrz(mgs) .lt. xvmn(lh) ) THEN
             qrfrzs(mgs) = qrfrz(mgs)
             crfrzs(mgs) = crfrz(mgs) ! *rzxh(mgs)
           ELSE
!           crfrz(mgs) = Min( crfrz(mgs), 0.1*cx(mgs,lr)/dtp ) ! cxmxd(mgs,lr) 
!           qrfrz(mgs) = Min( qrfrz(mgs), 0.1*qx(mgs,lr)/dtp ) ! qxmxd(mgs,lr) 
             qrfrzf(mgs) = frach*qrfrz(mgs)
!             crfrzf(mgs) = Min( qrfrz(mgs)*rho0(mgs)/(xdn(mgs,lh)*vgra), crfrz(mgs) )
            IF ( ibfr .le. 1 ) THEN
             crfrzf(mgs) = frach*Min(crfrz(mgs), qrfrz(mgs)/(bfnu*1.0*vr1mm*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
            ELSEIF ( ibfr .eq. 5 ) THEN
             crfrzf(mgs) = frach*Min(crfrz(mgs), qrfrz(mgs)/(bfnu*vfrz*1000.0)*rho0(mgs) )*rzxh(mgs)  !*crfrz(mgs)
            ELSEIF ( ibfr .eq. 2 ) THEN
             crfrzf(mgs) = frach*Min(crfrz(mgs), qrfrz(mgs)/(bfnu*vfrz*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
            ELSEIF ( ibfr .eq. 6 ) THEN
             crfrzf(mgs) = frach*Max(crfrz(mgs), qrfrz(mgs)/(bfnu*9.*xv(mgs,lr)*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
            ELSE
             crfrzf(mgs) = frach*crfrz(mgs)
            ENDIF 
!             crfrzf(mgs) = Min(crfrz(mgs), qrfrz(mgs)/(bfnu*xvmn(lh)*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
!            IF ( lz(lr) > 1 .and. lz(lh) > 1 ) THEN
!              crfrzf(mgs) = crfrz(mgs)
!            ENDIF
            
           ENDIF
!         crfrz(mgs) = Min( cxmxd(mgs,lr), rho0(mgs)*qrfrz(mgs)/xmas(mgs,lr) )
         ELSE
          crfrz(mgs) = 0.0
          qrfrz(mgs) = 0.0
         ENDIF !}

         ENDIF ! ibiggopt

          IF ( lvol(lh) .gt. 1 ) THEN
           vrfrzf(mgs) = rho0(mgs)*qrfrzf(mgs)/rhofrz
          ENDIF

        
        IF ( nsplinter .ne. 0 ) THEN
          IF ( nsplinter .gt. 0 ) THEN
            tmp = nsplinter*crfrz(mgs)
          ELSE
            tmp = -nsplinter*crfrzf(mgs)
          ENDIF
          csplinter2(mgs) = tmp
          qsplinter2(mgs) = Min(0.1*qrfrz(mgs), tmp*splintermass/rho0(mgs) ) ! makes splinters smaller if too much mass is taken from graupel

!          csplinter(mgs) = csplinter(mgs) + tmp
!          qsplinter(mgs) = qsplinter(mgs) + Min(0.1*qrfrz(mgs), tmp*splintermass/rho0(mgs) ) ! makes splinters smaller if too much mass is taken from graupel
        ENDIF
!         IF ( temcg(mgs) .lt. -31.0 ) THEN
!           qrfrz(mgs) = qx(mgs,lr)/dtp + qrcnw(mgs)
!           qrfrzf(mgs) = qrfrz(mgs)
!           crfrz(mgs) = cx(mgs,lr)/dtp + crcnw(mgs)
!           crfrzf(mgs) = Min(crfrz(mgs), qrfrz(mgs)/(bfnu*1.0*vr1mm*1000.0)*rho0(mgs) ) ! rzxh(mgs)*crfrz(mgs)
!         ENDIF
!         qrfrz(mgs) = 6.0*xdn(mgs,lr)*xv(mgs,lr)**2*tmp*rhoinv(mgs)
!         qrfrz(mgs) = Min( qrfrz(mgs), ffrz*qrmxd(mgs) )
!         crfrz(mgs) = Min( crmxd(mgs), ffrz*crfrz(mgs))
!         crfrz(mgs) = Min(crmxd(mgs),qrfrz(mgs)*rho0(mgs)/xmas(mgs,lr))
       ENDIF
!      if ( temg(mgs) .gt. 268.15 ) then
      else
!      end if
      end if
      end do
      
      ENDIF
!
!  Homogeneous freezing of cloud drops to ice crystals
!  following Bigg (1953) and Ferrier (1994).
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 25b'
      do mgs = 1,ngscnt
      qwfrz(mgs) = 0.0
      cwfrz(mgs) = 0.0
      qwfrzc(mgs) = 0.0
      cwfrzc(mgs) = 0.0
      qwfrzp(mgs) = 0.0
      cwfrzp(mgs) = 0.0
      IF ( ibfc .ge. 1 .and. temg(mgs) < 268.15 ) THEN
!      if ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. 1.  .and.   &
!     &     .not. (ipconc .ge. 2 .and. xdia(mgs,lc,1) .lt. 10.e-6) ) then
      if ( qx(mgs,lc) .gt. qxmin(lc) .and. cx(mgs,lc) .gt. cxmin ) THEN
      IF ( ipconc < 2 ) THEN
      qwfrz(mgs) = ((2.0)*(brz)/(xdn(mgs,lc)*cx(mgs,lc)))   &
     &  *(exp(max(-arz*temcg(mgs), 0.0))-1.0)   &
     &  *rho0(mgs)*(qx(mgs,lc)**2)
      qwfrz(mgs) = max(qwfrz(mgs), 0.0)
      qwfrz(mgs) = min(qwfrz(mgs),qcmxd(mgs))
         cwfrz(mgs) = qwfrz(mgs)*rho0(mgs)/xmas(mgs,li)
       ELSEIF ( ipconc .ge. 2 ) THEN
         IF ( xdia(mgs,lc,3) > 0.e-6 ) THEN
          volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 !  Ts == -temcg ; volt comes from the fit in Fig. 1 in Bigg 1953 
                                               ! for mean temperature for freezing: -ln (V) = a*Ts - b
                                               ! volt is given in cm**3, so factor of 1.e-6 to convert to m**3
!           dbigg = (6./pi* volt )**(1./3.) 

         cwfrz(mgs) = cx(mgs,lc)*Exp(-volt/xv(mgs,lc))/dtp ! number of droplets with volume greater than volt
!turn off limit so that all can freeze at low temp
!!!       cwfrz(mgs) = Min(cwfrz(mgs),ccmxd(mgs))

         qwfrz(mgs) = cwfrz(mgs)*xdn0(lc)*rhoinv(mgs)*(volt + xv(mgs,lc))
!         cwfrz(mgs) = cx(mgs,lc)*qwfrz(mgs)/qx(mgs,lc) ! reset number frozen to same fraction as mass. This makes 
                                                       ! sure that cwfrz and qwfrz are consistent and prevents 
                                                       ! spurious creation of ice crystals.
!         IF ( temg(mgs) < tfrh - 3 ) THEN
!          cwfrz(mgs) = cx(mgs,lc)
!          qwfrz(mgs) = qx(mgs,lc)
!         ENDIF
!         IF ( qwfrz(mgs) > 0.5*qx(mgs,lc) ) THEN
!           write(0,*) 'Problem with qwfrz(mgs): qwfrz,temcg,volt,xv,cx = ',qwfrz(mgs),qx(mgs,lc),temcg(mgs),volt,xv(mgs,lc),cx(mgs,lc),cwfrz(mgs)
!           STOP
!         ENDIF
!turn off limit so that all can freeze at low temp
!!!         qwfrz(mgs) = Min( qwfrz(mgs), qxmxd(mgs,lc) )
         ENDIF
       ENDIF
      if ( temg(mgs) .gt. 268.15 ) then
      qwfrz(mgs) = 0.0
      cwfrz(mgs) = 0.0
      end if
      end if
      ENDIF
!
        if ( xplate(mgs) .eq. 1 ) then
          qwfrzp(mgs) = qwfrz(mgs)
          cwfrzp(mgs) = cwfrz(mgs)
        end if
!  
        if ( xcolmn(mgs) .eq. 1 ) then
          qwfrzc(mgs) = qwfrz(mgs)
          cwfrzc(mgs) = cwfrz(mgs)
        end if
      
!
!     qwfrzp(mgs) = 0.0
!     qwfrzc(mgs) = qwfrz(mgs)
!
      end do
!
!
!  Contact freezing nucleation:  factor is to convert from L-1
!  T < -2C:  via Meyers et al. JAM July, 1992 (31, 708-721)
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 25a'
      do mgs = 1,ngscnt

       ccia(mgs) = 0.0

       cwctfz(mgs) = 0.0
       qwctfz(mgs) = 0.0
       ctfzbd(mgs) = 0.0
       ctfzth(mgs) = 0.0
       ctfzdi(mgs) = 0.0

       cwctfzc(mgs) = 0.0
       qwctfzc(mgs) = 0.0
       cwctfzp(mgs) = 0.0
       qwctfzp(mgs) = 0.0
       IF ( icfn .ge. 1 ) THEN

       IF ( temg(mgs) .lt. 271.15  .and. qx(mgs,lc) .gt. qxmin(lc)) THEN

!       find available # of ice nuclei & limit value to max depletion of cloud water

        IF ( icfn .ge. 2 ) THEN
         ccia(mgs) = exp( 4.11 - (0.262)*temcg(mgs) )  ! in m-3, see Walko et al. 1995
         !ccia(mgs) = Min(cwctfz(mgs), ccmxd(mgs) )

!       now find how many of these collect cloud water to form IN
!       Cotton et al 1986

         knud(mgs) = 2.28e-5 * temg(mgs) / ( pres(mgs)*raero ) !Walko et al. 1995
         knuda(mgs) = 1.257 + 0.4*exp(-1.1/knud(mgs))          !Pruppacher & Klett 1997 eqn 11-16
         gtp(mgs) = 1. / ( fai(mgs) + fbi(mgs) )               !Byers 65 / Cotton 72b
         dfar(mgs) = kb*temg(mgs)*(1.+knuda(mgs)*knud(mgs))/(6.*pi*fadvisc(mgs)*raero) !P&K 1997 eqn 11-15
         fn1(mgs) = 2.*pi*xdia(mgs,lc,1)*cx(mgs,lc)*ccia(mgs)
         fn2(mgs) = -gtp(mgs)*(ssw(mgs)-1.)*felv(mgs)/pres(mgs)
         fnft(mgs) = 0.4*(1.+1.45*knud(mgs)+0.4*knud(mgs)*exp(-1./knud(mgs)))*(ftka(mgs)+2.5*knud(mgs)*kaero)      &
     &              / ( (1.+3.*knud(mgs))*(2*ftka(mgs)+5.*knud(mgs)*kaero+kaero) )


!      Brownian diffusion
         ctfzbd(mgs) = fn1(mgs)*dfar(mgs)

!      Thermophoretic contact nucleation
         ctfzth(mgs) = fn1(mgs)*fn2(mgs)*fnft(mgs)/rho0(mgs)

!      Diffusiophoretic contact nucleation
         ctfzdi(mgs) = fn1(mgs)*fn2(mgs)*rw*temg(mgs)/(felv(mgs)*rho0(mgs))

         cwctfz(mgs) = max( ctfzbd(mgs) + ctfzth(mgs) + ctfzdi(mgs) , 0.)

!      Sum of the contact nucleation processes
!         IF ( cx(mgs,lc) .gt. 50.e6) write(6,*) 'ctfzbd,etc = ',ctfzbd(mgs),ctfzth(mgs),ctfzdi(mgs)
!         IF ( wvel(mgs) .lt. -0.05 ) write(6,*) 'ctfzbd,etc = ',ctfzbd(mgs),ctfzth(mgs),ctfzdi(mgs),cx(mgs,lc)*1e-6,wvel(mgs)
!         IF ( ssw(mgs) .lt. 1.0 .and. cx(mgs,lc) .gt. 1.e6 .and. cwctfz(mgs) .gt. 1. ) THEN
!          write(6,*) 'ctfzbd,etc = ',ctfzbd(mgs),ctfzth(mgs),ctfzdi(mgs),cx(mgs,lc)*1e-6,wvel(mgs),fn1(mgs),fn2(mgs)
!          write(6,*) 'more = ',nstep,ssw(mgs),dfar(mgs),gtp(mgs),felv(mgs),pres(mgs)
!         ENDIF

        ELSEIF ( icfn .eq. 1 ) THEN
         IF ( wvel(mgs) .lt. -0.05 ) THEN ! older kludgy version
           cwctfz(mgs) = cfnfac*exp( (-2.80) - (0.262)*temcg(mgs) )
           cwctfz(mgs) = Min((1.0e3)*cwctfz(mgs), ccmxd(mgs) )  !convert to m-3
         ENDIF
        ENDIF   ! icfn

        IF ( ipconc .ge. 2 ) THEN
         cwctfz(mgs) = Min( cwctfz(mgs)/dtp, ccmxd(mgs) )
         qwctfz(mgs) = xmas(mgs,lc)*cwctfz(mgs)/rho0(mgs)
        ELSE
         qwctfz(mgs) = (cimasn)*cwctfz(mgs)/(dtp*rho0(mgs))
         qwctfz(mgs) = max(qwctfz(mgs), 0.0)
         qwctfz(mgs) = min(qwctfz(mgs),qcmxd(mgs))
        ENDIF

!
        if ( xplate(mgs) .eq. 1 ) then
         qwctfzp(mgs) = qwctfz(mgs)
         cwctfzp(mgs) = cwctfz(mgs)
        end if
!
        if ( xcolmn(mgs) .eq. 1 ) then
         qwctfzc(mgs) = qwctfz(mgs)
         cwctfzc(mgs) = cwctfz(mgs)
        end if
       
!
!     qwctfzc(mgs) = qwctfz(mgs)
!     qwctfzp(mgs) = 0.0
!
       end if

       ENDIF ! icfn

      end do
!
!
!
! Hobbs-Rangno ice enhancement (Ferrier, 1994)
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 23a'
      dtrh = 300.0
      hrifac = (1.e-3)*((0.044)*(0.01**3))
      do mgs = 1,ngscnt
      ciihr(mgs) = 0.0
      qiihr(mgs) = 0.0
      cicichr(mgs) = 0.0
      qicichr(mgs) = 0.0
      cipiphr(mgs) = 0.0
      qipiphr(mgs) = 0.0
      IF ( ihrn .ge. 1 ) THEN
      if ( qx(mgs,lc) .gt. qxmin(lc) ) then
      if ( temg(mgs) .lt. 273.15 ) then
!      write(iunit,'(3(1x,i3),3(1x,1pe12.5))')
!     : igs(mgs),jgs,kgs(mgs),cx(mgs,lc),rho0(mgs),qx(mgs,lc)
!      write(iunit,'(1pe15.6)')
!     :  log(cx(mgs,lc)*(1.e-6)/(3.0)),
!     :  ((1.e-3)*rho0(mgs)*qx(mgs,lc)),
!     :  (cx(mgs,lc)*(1.e-6)),
!     : ((1.e-3)*rho0(mgs)*qx(mgs,lc))/(cx(mgs,lc)*(1.e-6)),
!     : (alog(cx(mgs,lc)*(1.e-6)/(3.0)) *
!     >  ((1.e-3)*rho0(mgs)*qx(mgs,lc))/(cx(mgs,lc)*(1.e-6)))

      IF ( Log(cx(mgs,lc)*(1.e-6)/(3.0)) .gt. 0.0 ) THEN
      ciihr(mgs) = ((1.69e17)/dtrh)   &
     & *(log(cx(mgs,lc)*(1.e-6)/(3.0)) *   &
     &  ((1.e-3)*rho0(mgs)*qx(mgs,lc))/(cx(mgs,lc)*(1.e-6)))**(7./3.)
      ciihr(mgs) = ciihr(mgs)*(1.0e6)
      qiihr(mgs) = hrifac*ciihr(mgs)/rho0(mgs)
      qiihr(mgs) = max(qiihr(mgs), 0.0)
      qiihr(mgs) = min(qiihr(mgs),qcmxd(mgs))
      ENDIF
!
      if ( xplate(mgs) .eq. 1 ) then
      qipiphr(mgs) = qiihr(mgs)
      cipiphr(mgs) = ciihr(mgs)
      end if
!
      if ( xcolmn(mgs) .eq. 1 ) then
      qicichr(mgs) = qiihr(mgs)
      cicichr(mgs) = ciihr(mgs)
      end if
!
!     qipiphr(mgs) = 0.0
!     qicichr(mgs) = qiihr(mgs)
!
      end if
      end if
      ENDIF ! ihrn
      end do
!
!
!
!  simple frozen rain to hail conversion.  All of the
!  frozen rain larger than 5.0e-3 m in diameter are converted
!  to hail.  This is done by considering the equation for
!  frozen rain mixing ratio:
!
!
!  qfw = [ cno(lf) * pi * fwdn / (6 rhoair) ]
!
!         /inf
!      *  |     fwdia*3 exp(-dia/fwdia) d(dia)
!         /Do
!
!  The amount to be reclassified as hail is the integral above from
!  Do to inf where Do is 5.0e-3 m.
!
!
!  qfauh = [ cno(lf) * pi * fwdn / (6 rhoair) ]
!
!


      hdia0 = 300.0e-6
      do mgs = 1,ngscnt
      qscnvi(mgs) = 0.0
      cscnvi(mgs) = 0.0
      cscnvis(mgs) = 0.0
!      IF ( .false. ) THEN
!      IF ( temg(mgs) .lt. tfr .and. ssi(mgs) .gt. 1.01 .and. qx(mgs,li) .gt. qxmin(li) ) THEN
      IF ( temg(mgs) .lt. tfr .and. qx(mgs,li) .gt. qxmin(li) ) THEN
        IF ( ipconc .ge. 4 .and. .false. ) THEN
         if ( cx(mgs,li) .gt. 10. .and. xdia(mgs,li,1) .gt. 50.e-6 ) then !{
         cirdiatmp =   &
     &  (qx(mgs,li)*rho0(mgs)   &
     & /(pi*xdn(mgs,li)*cx(mgs,li)))**(1./3.)
          IF ( cirdiatmp .gt. 100.e-6 ) THEN !{
          qscnvi(mgs) =   &
     &  ((pi*xdn(mgs,li)*cx(mgs,li)) / (6.0*rho0(mgs)*dtp))   &
     & *exp(-hdia0/cirdiatmp)   &
     & *( (hdia0**3) + 3.0*(hdia0**2)*cirdiatmp   &
     &  + 6.0*(hdia0)*(cirdiatmp**2) + 6.0*(cirdiatmp**3) )
      qscnvi(mgs) =   &
     &  min(qscnvi(mgs),qimxd(mgs))
          IF ( ipconc .ge. 4 ) THEN
            cscnvi(mgs) = Min( cimxd(mgs), cx(mgs,li)*Exp(-hdia0/cirdiatmp))
          ENDIF
         ENDIF  ! }
        end if ! }

       ELSEIF ( ipconc .lt. 4 ) THEN

        qscnvi(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
        qscnvi(mgs) = min(qscnvi(mgs),qxmxd(mgs,li))
        cscnvi(mgs) = qscnvi(mgs)*rho0(mgs)/xmas(mgs,li)
        cscnvis(mgs) = 0.5*cscnvi(mgs)

       ENDIF
      ENDIF
!      ENDIF
      end do

!
!  Ventilation coeficients
!
      do mgs = 1,ngscnt
      fvent(mgs) = (fschm(mgs)**(1./3.)) * (fakvisc(mgs)**(-0.5))
      end do
!
!
      if ( ndebug .gt. 0 ) write(0,*) 'civent'
!
      civenta = 1.258e4
      civentb = 2.331
      civentc = 5.662e4
      civentd = 2.373
      civente = 0.8241
      civentf = -0.042
      civentg = 1.70

      do mgs = 1,ngscnt
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN
      IF ( qx(mgs,li) .gt. qxmin(li) ) THEN
      cireyn =   &
     &  (civenta*xdia(mgs,li,1)**civentb   &
     &  +civentc*xdia(mgs,li,1)**civentd)   &
     &  /   &
     &  (civente*xdia(mgs,li,1)**civentf+civentg)
      xcivent = (fschm(mgs)**(1./3.))*((cireyn/fakvisc(mgs))**0.5)
      if ( xcivent .lt. 1.0 ) then
      civent(mgs) = 1.0 + 0.14*xcivent**2
      end if
      if ( xcivent .ge. 1.0 ) then
      civent(mgs) = 0.86 + 0.28*xcivent
      end if
      ELSE
       civent(mgs) = 0.0
      ENDIF


      ENDIF ! icond .eq. 1
      end do

!
!
      igmrwa = 100.0*2.0
      igmrwb = 100.*((5.0+br)/2.0)
      rwventa = (0.78)*gmoi(igmrwa)  ! 0.78
      rwventb = (0.308)*gmoi(igmrwb) ! 0.562825
      do mgs = 1,ngscnt
      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN
        IF ( ipconc .ge. 3 ) THEN
          IF ( imurain == 3 ) THEN
           IF ( izwisventr == 1 ) THEN
            rwvent(mgs) = ventrx(mgs)*(1.6 + 124.9*(1.e-3*rho0(mgs)*qx(mgs,lr))**.2046)
           ELSE ! izwisventr = 2
!  Following Wisner et al. (1972) but using gamma of volume. Note that Ferrier's rain fall speed does not integrate with gamma of volume, so using Vr = ar*d^br
          rwvent(mgs) =   &
     &  (0.78*ventrx(mgs) + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
           ENDIF

          ELSE ! imurain == 1
       ! linear interpolation of complete gamma function
!        tmp = 2. + alpha(mgs,lr)
!        i = Int(dgami*(tmp))
!        del = tmp - dgam*i
!        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        IF ( iferwisventr == 1 ) THEN

        alpr = Min(alpharmax,alpha(mgs,lr) )

        x =  1. + alpha(mgs,lr)

        IF ( lzr > 1 ) THEN ! 3 moment
        ELSE
         y = ventrxn(mgs)
        ENDIF

         vent1 = dble(xdia(mgs,lr,1))**(-2. - alpr)
         vent2 = dble(1./xdia(mgs,lr,1) + 0.5*fx(lr))**dble(2.5+alpr+0.5*bx(lr))
        
        
        rwvent(mgs) =    &
     &    0.78*x +    &
     &    0.308*fvent(mgs)*y*   &
     &            Sqrt(ax(lr)*rhovt(mgs))*(vent1/vent2)
       
        ELSEIF ( iferwisventr == 2 ) THEN
          
!  Following Wisner et al. (1972) but using gamma of volume. Note that Ferrier's rain fall speed does not integrate with gamma of volume, so using Vr = ar*d^br
         x =  1. + alpha(mgs,lr)

           rwvent(mgs) =   &
     &  (0.78*x + 0.308*ventrxn(mgs)*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )

          
          ENDIF ! iferwisventr
          
          ENDIF ! imurain
        ELSE
         rwvent(mgs) =   &
     &  (rwventa + rwventb*fvent(mgs)   &
     &   *Sqrt((ar*rhovt(mgs)))   &
     &    *(xdia(mgs,lr,1)**((1.0+br)/2.0)) )
        ENDIF
      ELSE
       rwvent(mgs) = 0.0
      ENDIF
      end do
!
      igmswa = 100.0*2.0
      igmswb = 100.*((5.0+ds)/2.0)
      swventa = (0.78)*gmoi(igmswa)
      swventb = (0.308)*gmoi(igmswb)
      do mgs = 1,ngscnt
      IF ( qx(mgs,ls) .gt. qxmin(ls) ) THEN
      IF ( ipconc .ge. 4 ) THEN
      swvent(mgs) = 0.65 + 0.44*fvent(mgs)*Sqrt(vtxbar(mgs,ls,1)*xdia(mgs,ls,1))
      ELSE
! 10-ice version:
       swvent(mgs) =   &
     &  (swventa + swventb*fvent(mgs)   &
     &   *Sqrt((cs*rhovt(mgs)))   &
     &   *(xdia(mgs,ls,1)**((1.0+ds)/2.0)) )
      ENDIF
      ELSE
      swvent(mgs) = 0.0
      ENDIF
      end do
!
!

      igmhwa = 100.0*2.0
      igmhwb = 100.0*2.75
      hwventa = (0.78)*gmoi(igmhwa)
      hwventb = (0.308)*gmoi(igmhwb)
      hwventc = (4.0*gr/(3.0*cdx(lh)))**(0.25)
      do mgs = 1,ngscnt
      IF ( qx(mgs,lh) .gt. qxmin(lh) ) THEN
       IF ( .false. .or. alpha(mgs,lh) .eq. 0.0 ) THEN
        hwvent(mgs) =   &
     &  ( hwventa + hwventb*hwventc*fvent(mgs)   &
     &    *((xdn(mgs,lh)/rho0(mgs))**(0.25))   &
     &    *(xdia(mgs,lh,1)**(0.75)))
       ELSE ! Ferrier 1994, eq. B.36
       ! linear interpolation of complete gamma function
!        tmp = 2. + alpha(mgs,lh)
!        i = Int(dgami*(tmp))
!        del = tmp - dgam*i
!        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami
        
! note that hwvent includes a division by Gamma(1+alpha), so Gamma(2+alpha)/Gamma(1+alpha) = 1 + alpha
! and g1palp = Gamma(1+alpha) divides into y
        x =  1. + alpha(mgs,lh)

        tmp = 1 + alpha(mgs,lh)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 2.5 + alpha(mgs,lh) + 0.5*bxh(mgs)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp
        
        
        hwventy(mgs) = 0.308*fvent(mgs)*(xdia(mgs,lh,1)**(0.5 + 0.5*bxh(mgs)))*Sqrt(axh(mgs)*rhovt(mgs)) 
        hwvent(mgs) =    &
     &  ( 0.78*x +  y*hwventy(mgs) ) !   &
!     &    0.308*fvent(mgs)*y*(xdia(mgs,lh,1)**(0.5 + 0.5*bxh(mgs)))*   &
!     &            Sqrt(axh(mgs)*rhovt(mgs)) )
       
       ENDIF
      ELSE
      hwvent(mgs) = 0.0
      hwventy(mgs) = 0.0
      ENDIF
      end do
      
      hlvent(:) = 0.0
      hlventy(:) = 0.0

      IF ( lhl .gt. 1 ) THEN
      igmhwa = 100.0*2.0
      igmhwb = 100.0*2.75
      hwventa = (0.78)*gmoi(igmhwa)
      hwventb = (0.308)*gmoi(igmhwb)
      hwventc = (4.0*gr/(3.0*cdx(lhl)))**(0.25)
      do mgs = 1,ngscnt
      IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN

       IF ( .false. .or. alpha(mgs,lhl) .eq. 0.0 ) THEN
        hlvent(mgs) =   &
     &  ( hwventa + hwventb*hwventc*fvent(mgs)   &
     &    *((xdn(mgs,lhl)/rho0(mgs))**(0.25))   &
     &    *(xdia(mgs,lhl,1)**(0.75)))
       ELSE ! Ferrier 1994, eq. B.36
       ! linear interpolation of complete gamma function
!        tmp = 2. + alpha(mgs,lhl)
!        i = Int(dgami*(tmp))
!        del = tmp - dgam*i
!        x = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

! note that hlvent includes a division by Gamma(1+alpha), so x = Gamma(2+alpha)/Gamma(1+alpha) = 1 + alpha
! and g1palp = Gamma(1+alpha) divides into y

        x =  1. + alpha(mgs,lhl)

        tmp = 1 + alpha(mgs,lhl)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

        tmp = 2.5 + alpha(mgs,lhl) + 0.5*bxhl(mgs)
        i = Int(dgami*(tmp))
        del = tmp - dgam*i
        y = (gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami)/g1palp ! ratio of gamma functions

        hlventy(mgs) = 0.308*fvent(mgs)*(xdia(mgs,lhl,1)**(0.5 + 0.5*bxhl(mgs)))*Sqrt(axhl(mgs)*rhovt(mgs)) 
        
        hlvent(mgs) =  0.78*x + y*hlventy(mgs)  !   &
!     &    0.308*fvent(mgs)*y*(xdia(mgs,lhl,1)**(0.5 + 0.5*bxhl(mgs)))*   &
!     &            Sqrt(axhl(mgs)*rhovt(mgs)))
!     :            Sqrt(xdn(mgs,lhl)*ax(lhl)*rhovt(mgs)/rg0))/tmp

        ENDIF
       ENDIF
      end do
      ENDIF

!
!
!
!  Wet growth constants
!
      do mgs = 1,ngscnt
      fwet1(mgs) =   &
     & (2.0*pi)*   &
     & ( felv(mgs)*fwvdf(mgs)*rho0(mgs)*(qss0(mgs)-qx(mgs,lv))   &
     &  -ftka(mgs)*temcg(mgs) )   &
     & / ( rho0(mgs)*(felf(mgs)+fcw(mgs)*temcg(mgs)) )
      fwet2(mgs) =   &
     &  (1.0)-fci(mgs)*temcg(mgs)   &
     & / ( felf(mgs)+fcw(mgs)*temcg(mgs) )
      end do
!
!  Melting constants
!
      do mgs = 1,ngscnt
      fmlt1(mgs) = (2.0*pi)*   &
     &  ( felv(mgs)*fwvdf(mgs)*(qss0(mgs)-qx(mgs,lv))   &
     &   -ftka(mgs)*temcg(mgs)/rho0(mgs) )    &
     &  / (felf(mgs))
      fmlt2(mgs) = -fcw(mgs)*temcg(mgs)/felf(mgs)
      end do
!
!  Vapor Deposition constants
!
      do mgs = 1,ngscnt
      fvds(mgs) =    &
     &  (4.0*pi/rho0(mgs))*(ssi(mgs)-1.0)*   &
     &  (1.0/(fai(mgs)+fbi(mgs)))
      end do
      do mgs = 1,ngscnt
      fvce(mgs) =    &
     &  (4.0*pi/rho0(mgs))*(ssw(mgs)-1.0)*   &
     &  (1.0/(fav(mgs)+fbv(mgs)))
      end do

!
!  deposition, sublimation, and melting of snow, graupel and hail
!
      qsmlr(:) = 0.0
      qimlr(:) = 0.0 ! this is not used. qi melts to qc way down in the code.
      qhmlr(:) = 0.0
      qhlmlr(:) = 0.0
      qhmlrlg(:) = 0.0
      qhlmlrlg(:) = 0.0
      qhfzh(:) = 0.0
      qhlfzhl(:) = 0.0
      vhfzh(:) = 0.0
      vhlfzhl(:) = 0.0
      qsfzs(:) = 0.0
      zsmlr(:) = 0.0
      zhmlr(:) = 0.0
      zhmlrr(:) = 0.0
      zhshr(:) = 0.0
      zhlmlr(:) = 0.0
      zhlshr(:) = 0.0

      zhshrr(:) = 0.0
      zhlmlrr(:) = 0.0
      zhlshrr(:) = 0.0

      csmlr(:) = 0.0
      chmlr(:) = 0.0
      chmlrr(:) = 0.0
      chlmlr(:) = 0.0
      chlmlrr(:) = 0.0

      if ( .not. mixedphase ) then !{
      do mgs = 1,ngscnt
!
      IF ( temg(mgs) .gt. tfr ) THEN
      
      IF (  qx(mgs,ls) .gt. qxmin(ls) ) THEN
      qsmlr(mgs) =   &
     &   min(   &
     &  (c1sw*fmlt1(mgs)*cx(mgs,ls)*swvent(mgs)*xdia(mgs,ls,1) ) & ! /rhosm    &
     &   , 0.0 )
      ENDIF
      
!       IF ( qx(mgs,ls) .gt. 0.1e-4 ) write(0,*) 'qsmlr: ',qsmlr(mgs),qx(mgs,ls),cx(mgs,ls),fmlt1(mgs),
!     :        temcg(mgs),swvent(mgs),xdia(mgs,ls,1),qss0(mgs)-qx(mgs,lv)
!      ELSE
!       qsmlr(mgs) = 0.0
!      ENDIF
! 10ice version:
!     >   min(
!     >  (fmlt1(mgs)*cx(mgs,ls)*swvent(mgs)*xdia(mgs,ls,1) +
!     >   fmlt2(mgs)*(qsacr(mgs)+qsacw(mgs)) )
!     <   , 0.0 )

      IF (  qx(mgs,lh) .gt. qxmin(lh) ) THEN

      IF ( ibinhmlr == 0 .or. lzh < 1 ) THEN
       qhmlr(mgs) =   &
     &   min(   &
     &  fmlt1(mgs)*cx(mgs,lh)*hwvent(mgs)*xdia(mgs,lh,1)   &
     &  + fmlt2(mgs)*(qhacrmlr(mgs)+qhacw(mgs))    &
     &   , 0.0 )
       ELSEIF ( ibinhmlr == 1 ) THEN ! use incomplete gamma functions to approximate the bin results

         write(0,*) 'ibinhmlr = 1 not available for 2-moment'
         STOP
         
       ELSEIF ( ibinhmlr == 2 ) THEN

       ENDIF
       
       
       IF ( ivhmltsoak > 0 .and. qhmlr(mgs) < 0.0 .and. lvol(lh) > 1 .and. xdn(mgs,lh) .lt. xdnmx(lh) ) THEN
         ! act as if 100% of the meltwater were soaked into the graupel
           v1 = (1. - xdn(mgs,lh)/xdnmx(lh))*(vx(mgs,lh) + rho0(mgs)*qhmlr(mgs)/xdn(mgs,lh) )/(dtp) ! volume available for filling
           v2 = -1.0*rho0(mgs)*qhmlr(mgs)/xdnmx(lh)  ! volume of melted ice if it were refrozen in the matrix
           
           vhsoak(mgs) = Min(v1,v2)
           
       ENDIF

      ENDIF !  qx(mgs,lh) .gt. qxmin(lh)

      
      IF ( lhl .gt. 1  .and. lhlw < 1 ) THEN

       IF ( qx(mgs,lhl) .gt. qxmin(lhl) ) THEN
         IF ( ibinhlmlr == 0  .or. lzhl < 1) THEN
       qhlmlr(mgs) =   &
     &   min(   &
     &  fmlt1(mgs)*cx(mgs,lhl)*hlvent(mgs)*xdia(mgs,lhl,1)   &
     &  + fmlt2(mgs)*(qhlacrmlr(mgs)+qhlacw(mgs))    &
     &   , 0.0 )

       ELSEIF ( ibinhlmlr == 1 ) THEN ! use incomplete gamma functions to approximate the bin results


       ELSEIF ( ibinhlmlr == -1 ) THEN ! OLD VERSION use incomplete gamma functions to approximate the bin results

        ENDIF ! ibinhlmlr


       IF ( ivhmltsoak > 0 .and.  qhlmlr(mgs) < 0.0 .and. lvol(lhl) > 1 .and. xdn(mgs,lhl) .lt. xdnmx(lhl) ) THEN
         ! act as if 50% of the meltwater were soaked into the graupel
           v1 = (1. - xdn(mgs,lhl)/xdnmx(lhl))*(vx(mgs,lhl) + rho0(mgs)*qhlmlr(mgs)/xdn(mgs,lhl) )/(dtp) ! volume available for filling
           v2 = -1.0*rho0(mgs)*qhlmlr(mgs)/xdnmx(lhl)  ! volume of melted ice if it were refrozen in the matrix
           
           vhlsoak(mgs) = Min(v1,v2)
           
       ENDIF
        
        ENDIF
       ENDIF

      ENDIF
      
!
!      qimlr(mgs)  = max( qimlr(mgs), -qimxd(mgs) ) 
!      qsmlr(mgs)  = max( qsmlr(mgs),  -qsmxd(mgs) ) 
! erm 5/10/2007 changed to next line:
      if ( .not. mixedphase ) qsmlr(mgs)  = max( qsmlr(mgs),  Min( -qsmxd(mgs), -0.7*qx(mgs,ls)*dtpinv ) ) 
      if ( .not. mixedphase ) qhmlr(mgs)  = max( qhmlr(mgs),  Min( -qhmxd(mgs), -0.5*qx(mgs,lh)*dtpinv ) ) 
!      qhmlr(mgs)  = max( max( qhmlr(mgs),  -qhmxd(mgs) ) , -0.5*qx(mgs,lh)/dtp ) !limits to 1/2 qh or max depletion
      qhmlh(mgs)  = 0.


      ! Rasmussen and Heymsfield say melt water remains on graupel up to 9 mm before shedding


      IF ( lhl .gt. 1 .and. lhlw < 1 ) qhlmlr(mgs)  = max( qhlmlr(mgs),  Min( -qxmxd(mgs,lhl), -0.5*qx(mgs,lhl)/dtp ) )

!
      end do

      endif  ! } not mixedphase
!
      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      cimlr(mgs)  = (cx(mgs,li)/(qx(mgs,li)+1.e-20))*qimlr(mgs)
      IF ( .not. mixedphase ) THEN !{
        IF ( xdia(mgs,ls,1) .gt. 1.e-6 .and. -qsmlr(mgs) .ge. 0.5*qxmin(ls) .and. ipconc .ge. 4 ) THEN 
!         csmlr(mgs)  = rho0(mgs)*qsmlr(mgs)/(xv(mgs,ls)*rhosm)
         csmlr(mgs)  = (cx(mgs,ls)/(qx(mgs,ls)))*qsmlr(mgs)
        ELSEIF ( qx(mgs,ls) > qxmin(ls) ) THEN
         csmlr(mgs)  = (cx(mgs,ls)/(qx(mgs,ls)))*qsmlr(mgs)
        ENDIF

!        IF ( xdia(mgs,lh,1) .gt. 1.e-6 .and. Abs(qhmlr(mgs)) .ge. qxmin(lh) ) THEN
!          chmlr(mgs) = rho0(mgs)*qhmlr(mgs)/(pi*xdn(mgs,lh)*xdia(mgs,lh,1)**3)  ! out of hail
!          chmlr(mgs) = Max( chmlr(mgs), -chmxd(mgs) )
!        ELSE
         IF ( ibinhmlr == 0 ) THEN
           chmlr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhmlr(mgs)
           IF ( imltshddmr == 3 .and. qhmlr(mgs) < -qxmin(lh) ) THEN
            !  tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
            !  
            !  IF ( tmpdiam > sheddiam ) THEN ! let size get smaller until it reaches sheddiam
            !   chmlr(mgs) = 0.0
            !  ENDIF
            
            ! test to remove the part of the melting associated with large ice particles so they get smaller

            tmp = 1. + alpha(mgs,lh)
            i = Int(dgami*(tmp))
            del = tmp - dgam*i
            g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

            ratio = Min( maxratiolu,  mltdiam1/xdia(mgs,lh,1) )

            x =  gamxinfdp(2. + alpha(mgs,lh), ratio)/g1palp
            y =  gamxinfdp(2.5 + alpha(mgs,lh) + 0.5*bxh(mgs), ratio)/g1palp

            hwvent1 =  0.78*x + y*hwventy(mgs) 

            qhlmlr1 = min( fmlt1(mgs)*cx(mgs,lh)*hwvent1*xdia(mgs,lh,1), 0.0 )

            chmlr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*(qhmlr(mgs) - qhlmlr1)
           
           
           ENDIF
!           IF ( igs(mgs) == 40 ) THEN
!             write(0,*) 'is this running? chmlr = ',kgs(mgs), chmlr(mgs)
!           ENDIF
         ENDIF
!        ENDIF



     IF ( chmlr(mgs) < 0.0 .and. ibinhmlr < 1) THEN ! { already done if ibinhmlr > 0
      
      IF ( ibinhmlr == 0 ) THEN
      IF ( ihmlt .eq. 1 ) THEN
        chmlrr(mgs)  = Min( chmlr(mgs), rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*vmlt) ) ! into rain 
      ELSEIF ( ihmlt .eq. 2 ) THEN
        IF ( xv(mgs,lh) .gt. 0.0 .and. chmlr(mgs) .lt. 0.0 ) THEN
!        chmlrr(mgs) = Min( chmlr(mgs), rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lh)*xv(mgs,lh)) ) ! into rain 
! guess what, this is the same as chmlr: rho0*qhmlr/xmas(lh) --> cx/qx = rho0/xmas
          IF(imltshddmr == 1) THEN
            ! DTD: If Dmg < sheddiam, then assume complete melting into
            ! maximal raindrop.  Between sheddiam and sheddiam0 mm, linearly ramp down to a 3 mm shed drop
            tmp = -rho0(mgs)*qhmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lh)*xv(mgs,lh))) ! Min of Maximum raindrop size/mean hail size
            tmp2 = -rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*vr3mm) ! conc. change for a 3 mm mean drop diameter
            
            chmlrr(mgs) = tmp*(sheddiam0-xdia(mgs,lh,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lh,3)-sheddiam)/(sheddiam0-sheddiam)  ! old version
            chmlrr(mgs) = -Max(tmp,Min(tmp2,chmlrr(mgs)))
          ELSEIF ( imltshddmr == 2 .or. imltshddmr == 3 ) THEN
            ! 8/26/2015 ERM updated to use shedalp and tmpdiam
            ! tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
            chmlrr(mgs) =  rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*vshdgs(mgs,lh))  ! into rain 
          ELSE ! Old method
            chmlrr(mgs) =  rho0(mgs)*qhmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lh)*xv(mgs,lh)))  ! into rain 
          ENDIF
        ELSE
        chmlrr(mgs) = chmlr(mgs)
        ENDIF
      ELSEIF ( ihmlt .eq. 0 ) THEN
        chmlrr(mgs) = chmlr(mgs)
      ENDIF

      ELSE ! ibinhmlr < 0? Already have an outer IF test for ibinhmlr < 1
        chmlrr(mgs)  = Min( chmlrr(mgs), rho0(mgs)*qhmlr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) ! into rain 
      ENDIF
      
      ENDIF ! } ( chmlr(mgs) < 0.0 .and. ibinhmlr < 1)

      IF ( lhl .gt. 1 .and. lhlw < 1 .and. .not. mixedphase .and. qhlmlr(mgs) < 0.0 ) THEN ! {
      
      IF ( ibinhlmlr == 0 ) THEN
!      IF ( xdia(mgs,lhl,1) .gt. 1.e-6 .and. Abs(qhlmlr(mgs)) .ge. qxmin(lhl) ) THEN
!      chlmlr(mgs) = rho0(mgs)*qhlmlr(mgs)/(pi*xdn(mgs,lhl)*xdia(mgs,lhl,1)**3)  ! out of hail
!      chlmlr(mgs) = Max( chlmlr(mgs), -cxmxd(mgs,lhl) )
!      ELSE
      chlmlr(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlmlr(mgs)
           IF ( imltshddmr == 3 .and. qhlmlr(mgs) < -qxmin(lhl) ) THEN
!           IF ( .false. .and. imltshddmr == 3  ) THEN
!              tmpdiam = (shedalp+alpha(mgs,lhl))*xdia(mgs,lhl,1)
!              
!              IF ( tmpdiam > sheddiam ) THEN ! let size get smaller until it reaches sheddiam
!                chlmlr(mgs) = 0.0
!              ENDIF
 
            ! test to remove the part of the melting associated with large ice particles so they get smaller
!
            tmp = 1. + alpha(mgs,lhl)
            i = Int(dgami*(tmp))
            del = tmp - dgam*i
            g1palp = gmoi(i) + (gmoi(i+1) - gmoi(i))*del*dgami

            ratio = Min( maxratiolu,  mltdiam1/xdia(mgs,lhl,1) )

            x =  gamxinfdp(2. + alpha(mgs,lhl), ratio)/g1palp
            y =  gamxinfdp(2.5 + alpha(mgs,lhl) + 0.5*bxhl(mgs), ratio)/g1palp

            hwvent1 =  0.78*x + y*hlventy(mgs) 

            qhlmlr1 = min( fmlt1(mgs)*cx(mgs,lhl)*hwvent1*xdia(mgs,lhl,1), 0.0 )

            chlmlr(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*Min(0.0, qhlmlr(mgs) - qhlmlr1)

            ENDIF
!      ENDIF
      ENDIF
      
      IF ( ibinhlmlr == 0 ) THEN !{
      IF ( ihmlt .eq. 1 ) THEN
        chlmlrr(mgs)  = Min( chlmlr(mgs), rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*vmlt) ) ! into rain 
      ELSEIF ( ihmlt .eq. 2 ) THEN
        IF ( xv(mgs,lhl) .gt. 0.0 .and. chlmlr(mgs) .lt. 0.0 ) THEN
!        chlmlrr(mgs) = rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl)))  ! into rain 
!        chlmlrr(mgs) = Min( chlmlr(mgs), rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lhl)*xv(mgs,lhl)) ) ! into rain 
          IF(imltshddmr == 1 ) THEN
            tmp = -rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl))) ! Min of Maximum raindrop size/mean hail size
            tmp2 = -rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*vr3mm) ! conc. change for a 3 mm mean drop diameter
            chlmlrr(mgs) = tmp*(20.e-3-xdia(mgs,lhl,3))/(20.e-3-sheddiam)+tmp2*(xdia(mgs,lhl,3)-sheddiam)/(20.e-3-sheddiam)
            chlmlrr(mgs) = -Max(tmp,Min(tmp2,chlmlrr(mgs)))
          ELSEIF ( imltshddmr == 2 .or. imltshddmr == 3 ) THEN
            ! 8/26/2015 ERM updated to use shedalp and tmpdiam
            ! tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
            chlmlrr(mgs) =  rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*vshdgs(mgs,lhl))  ! into rain 
          ELSE
            chlmlrr(mgs) = rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl)))  ! into rain 
          ENDIF
        ELSE
        chlmlrr(mgs) = chlmlr(mgs)
        ENDIF
      ELSEIF ( ihmlt .eq. 0 ) THEN
        chlmlrr(mgs) = chlmlr(mgs)
      ENDIF

      ELSE ! } { ibinhlmlr > 0
        chlmlrr(mgs)  = Min( chlmlrr(mgs), rho0(mgs)*qhlmlr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) ! into rain 
      ENDIF !}
        
      ENDIF ! }

      ENDIF ! }.not. mixedphase 

! 10ice versions:
!      chmlr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhmlr(mgs)
!      chmlrr(mgs) = chmlr(mgs)
      end do
      end if

!
!  deposition/sublimation of ice
!
      DO mgs = 1,ngscnt

      rwcap(mgs) = (0.5)*xdia(mgs,lr,1)
      swcap(mgs) = (0.5)*xdia(mgs,ls,1)
      hwcap(mgs) = (0.5)*xdia(mgs,lh,1)
      IF ( lhl .gt. 1 ) hlcap(mgs) = (0.5)*xdia(mgs,lhl,1)

      if ( qx(mgs,li).gt.qxmin(li) .and. xdia(mgs,li,1) .gt. 0.0 ) then
!
! from Cotton, 1972 (Part II)
!
        cilen(mgs)   = 0.4764*(xdia(mgs,li,1))**(0.958)
        cval = xdia(mgs,li,1)
        aval = cilen(mgs)
        eval = Sqrt(1.0-(aval**2)/(cval**2))
        fval = min(0.99,eval)
        gval = alog( abs( (1.+fval)/(1.-fval) ) )
        cicap(mgs) = cval*fval / gval
      ELSE
       cicap(mgs) = 0.0
      end if
      ENDDO
!
!
      qhldsv(:) = 0.0

      do mgs = 1,ngscnt
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN
        qidsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,li)*civent(mgs)*cicap(mgs)
        qsdsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,ls)*swvent(mgs)*swcap(mgs)
!        IF ( ny .eq. 2 .and. igs(mgs) .eq. 302 .and. temg(mgs) .le. tfrh+10 .and. qx(mgs,lv) .gt. qis(mgs)
!     :       .and. qx(mgs,li) .gt. qxmin(li) ) THEN
!         write(0,*) 'qidsv = ',nstep,kgs(mgs),qidsv(mgs),temg(mgs)-tfrh,100.*(qx(mgs,lv)/qis(mgs) - 1.),1.e6*xdia(mgs,li,1),
!     :            fvds(mgs),civent(mgs),cicap(mgs)
!        ENDIF
      ELSE
        qidsv(mgs) = 0.0
        qsdsv(mgs) = 0.0
      ENDIF
        qhdsv(mgs) =   &
     &    fvds(mgs)*cx(mgs,lh)*hwvent(mgs)*hwcap(mgs)*depfac

        IF ( lhl .gt. 1 ) qhldsv(mgs) = fvds(mgs)*cx(mgs,lhl)*hlvent(mgs)*hlcap(mgs)*depfac
!
!
      end do
!
      do mgs = 1,ngscnt
      qisbv(mgs) = 0.0
      qssbv(mgs) = 0.0
      qidpv(mgs) = 0.0
      qsdpv(mgs) = 0.0
      IF ( icond .eq. 1 .or. temg(mgs) .le. tfrh    &
     &      .or. (qx(mgs,lr) .le. qxmin(lr) .and. qx(mgs,lc) .le. qxmin(lc)) ) THEN
!        qisbv(mgs) = max( min(qidsv(mgs), 0.0), -qimxd(mgs) )
!        qssbv(mgs) = max( min(qsdsv(mgs), 0.0), -qsmxd(mgs) )
! erm 5/10/2007:
        qisbv(mgs) = max( min(qidsv(mgs), 0.0),  Min( -qimxd(mgs), -0.7*qx(mgs,li)/dtp ) )
        qssbv(mgs) = max( min(qsdsv(mgs), 0.0),  Min( -qsmxd(mgs), -0.7*qx(mgs,ls)/dtp ) )
        qidpv(mgs) = Max(qidsv(mgs), 0.0)
        qsdpv(mgs) = Max(qsdsv(mgs), 0.0)
      ELSE
        qisbv(mgs) = 0.0
        qssbv(mgs) = 0.0
        qidpv(mgs) = 0.0
        qsdpv(mgs) = 0.0
      ENDIF

      qhsbv(mgs) = max( min(qhdsv(mgs), 0.0), -qhmxd(mgs) )


      qhdpv(mgs) = Max(qhdsv(mgs), 0.0)

      qhlsbv(mgs) = 0.0
      qhldpv(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN
        qhlsbv(mgs) = max( min(qhldsv(mgs), 0.0), -qxmxd(mgs,lhl) )
        qhldpv(mgs) = Max(qhldsv(mgs), 0.0)
      ENDIF
      
      temp1 = qidpv(mgs) + qsdpv(mgs) + qhdpv(mgs) + qhldpv(mgs)

      IF ( temp1 .gt. qvimxd(mgs) ) THEN

      frac = qvimxd(mgs)/temp1

      qidpv(mgs) = frac*qidpv(mgs)
      qsdpv(mgs) = frac*qsdpv(mgs)
      qhdpv(mgs) = frac*qhdpv(mgs)
      qhldpv(mgs) = frac*qhldpv(mgs)

!        IF ( ny .eq. 2 .and. igs(mgs) .eq. 302 .and. temg(mgs) .le. tfrh+10 .and. qx(mgs,lv) .gt. qis(mgs)
!     :       .and. qx(mgs,li) .gt. qxmin(li) ) THEN
!         write(0,*) 'qidpv,frac = ',kgs(mgs),qidpv(mgs),frac
!        ENDIF

      ENDIF

      end do
!
!
      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      cssbv(mgs)  = (cx(mgs,ls)/(qx(mgs,ls)+1.e-20))*qssbv(mgs)
      cisbv(mgs)  = (cx(mgs,li)/(qx(mgs,li)+1.e-20))*qisbv(mgs)
      chsbv(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhsbv(mgs)
      IF ( lhl .gt. 1 ) chlsbv(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlsbv(mgs)
      csdpv(mgs)  = 0.0 ! (cx(mgs,ls)/(qx(mgs,ls)+1.e-20))*qsdpv(mgs)
      cidpv(mgs) =  0.0 ! (cx(mgs,li)/(qx(mgs,li)+1.e-20))*qidpv(mgs)
      cisdpv(mgs) = 0.0
      chdpv(mgs)  = 0.0 ! (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhdpv(mgs)
      chldpv(mgs) = 0.0
      end do
      end if

!
!  Aggregation or size conversion of small crystals to snow
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 29a'
      do mgs = 1,ngscnt
      qscni(mgs) =  0.0
      cscni(mgs) = 0.0
      cscnis(mgs) = 0.0
      if ( ipconc .ge. 4 .and. iscni .ge. 1 .and. qx(mgs,li) .gt. qxmin(li) ) then
        IF ( iscni .eq. 1 ) THEN
         qscni(mgs) =    &
     &      pi*rho0(mgs)*((0.25)/(6.0))   &
     &      *eii(mgs)*(qx(mgs,li)**2)*(xdia(mgs,li,2))   &
     &      *vtxbar(mgs,li,1)/xmas(mgs,li)
         cscni(mgs) = Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/xmas(mgs,li))
         cscnis(mgs) = 0.5*cscni(mgs)
        ELSEIF ( iscni .eq. 2 .or. iscni .eq. 4 .or. iscni .eq. 5 ) THEN  ! Zeigler 1985/Zrnic 1993, sort of
          IF ( iscni .ne. 5 .and. qidpv(mgs) .gt. 0.0 .and.  xdia(mgs,li,3) .ge. 100.e-6 ) THEN
          ! convert larger crystals to snow
!            IF ( xdia(mgs,ls,3) .gt. xdia(mgs,li,3) ) THEN
!              qscni(mgs) = Max(0.1,xdia(mgs,li,3)/xdia(mgs,ls,3))*qidpv(mgs)
! erm 9/5/08 changed max to min
              qscni(mgs) = Min(0.5, xdia(mgs,li,3)/200.e-6)*qidpv(mgs)
!            ELSE
!              qscni(mgs) = 0.1*qidpv(mgs)
!            ENDIF
            cscni(mgs) = 0.5*Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/Max(xdn(mgs,ls)*xvsmn,xmas(mgs,li)))
!            cscni(mgs) = Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/xmas(mgs,li) )
!            IF ( xdia(mgs,ls,3) .le. 200.e-6 ) THEN
              cscnis(mgs) = cscni(mgs)
!            ELSE
!              cscnis(mgs) = 0.0
!            ENDIF
          ENDIF

           IF ( iscni .ne. 4 ) THEN
           ! crystal aggregation to become snow
! erm 9/5/08 commented second line and added xv to 1st line (zrnic et al 1993)
             tmp = ess(mgs)*rvt*aa2*cx(mgs,li)*cx(mgs,li)*xv(mgs,li)
!     :         ((cinu + 2.)*xv(mgs,li)/(cinu + 1.) + xv(mgs,li))

!           csacs(mgs) = rvt*aa2*ess(mgs)*cx(mgs,ls)**2*xv(mgs,ls)

             qscni(mgs) = qscni(mgs) + Min( qxmxd(mgs,li), 2.0*tmp*xmas(mgs,li)*rhoinv(mgs) )
             cscni(mgs) = cscni(mgs) + Min( cxmxd(mgs,li), 2.0*tmp )
             cscnis(mgs) = cscnis(mgs) + Min( cxmxd(mgs,li), tmp )
           ENDIF
        ELSEIF ( iscni .eq. 3 ) THEN ! LFO
           qscni(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
           qscni(mgs) = min(qscni(mgs),qxmxd(mgs,li))
           cscni(mgs) = qscni(mgs)*rho0(mgs)/xmas(mgs,li)
           cscnis(mgs) = 0.5*cscni(mgs)
!           write(iunit,*) 'qscni, qi = ',qscni(mgs),qx(mgs,li),igs(mgs),kgs(mgs)
        ENDIF

      ELSEIF ( ipconc < 4 ) THEN ! LFO
           IF ( lwsm6 ) THEN
             qimax = rhoinv(mgs)*roqimax
             qscni(mgs) = Min(0.9*qx(mgs,li), Max( 0., (qx(mgs,li) - qimax)*dtpinv ) )
           ELSE
             qscni(mgs) = 0.001*eii(mgs)*max((qx(mgs,li)-1.e-3),0.0)
             qscni(mgs) = min(qscni(mgs),qxmxd(mgs,li))
           ENDIF
      else ! 10-ice version
      if ( iscni > 0 .and. qx(mgs,li) .gt. qxmin(li) ) then
          qscni(mgs) =    &
     &    pi*rho0(mgs)*((0.25)/(6.0))   &
     &    *eii(mgs)*(qx(mgs,li)**2)*(xdia(mgs,li,2))   &
     &    *vtxbar(mgs,li,1)/xmas(mgs,li)
         cscni(mgs) = Min(cimxd(mgs),qscni(mgs)*rho0(mgs)/xmas(mgs,li))
        end if

      end if
      end do

!
!
!  compute dry growth rate of snow, graupel, and hail
!
      do mgs = 1,ngscnt
!
      qsdry(mgs)  = qsacr(mgs)    + qsacw(mgs)   &
     &            + qsaci(mgs)
!
      qhdry(mgs)  = qhaci(mgs)    + qhacs(mgs)   &
     &            + qhacr(mgs)   &
     &            + qhacw(mgs)
!
      qhldry(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN
      qhldry(mgs)  = qhlaci(mgs)    + qhlacs(mgs)   &
     &               + qhlacr(mgs)   &
     &               + qhlacw(mgs)
      ENDIF
      end do
!
!  set wet growth and shedding
!
      do mgs = 1,ngscnt
      
      IF ( temg(mgs) < tfr ) THEN
!
!      qswet(mgs) =
!     >  ( xdia(mgs,ls,1)*swvent(mgs)*cx(mgs,ls)*fwet1(mgs)
!     >  + fwet2(mgs)*(qsaci(mgs)+qsacir(mgs)
!     >               +qsacip(mgs)) )
!      qswet(mgs) = max( 0.0, qswet(mgs))
!
!      IF ( dnu(lh) .ne. 0. ) THEN
!        qhwet(mgs) = qhdry(mgs)
!      ELSE
        qhwet(mgs) =   &
     &    ( xdia(mgs,lh,1)*hwvent(mgs)*cx(mgs,lh)*fwet1(mgs)   &
     &   + fwet2(mgs)*(qhaci(mgs) + qhacs(mgs)) )
       qhwet(mgs) = max( 0.0, qhwet(mgs))
!      ENDIF

       qhlwet(mgs) = 0.0
       IF ( lhl .gt. 1 ) THEN
       qhlwet(mgs) =   &
     &    ( xdia(mgs,lhl,1)*hlvent(mgs)*cx(mgs,lhl)*fwet1(mgs)   &
     &   + fwet2(mgs)*(qhlaci(mgs) + qhlacs(mgs)) )
       qhlwet(mgs) = max( 0.0, qhlwet(mgs))
       ENDIF
       
       ELSE
       
        qhwet(mgs) = qhdry(mgs)
        qhlwet(mgs) = qhldry(mgs)
        
       ENDIF
!
!      qhlwet(mgs) = qhldry(mgs)

      end do
!
! shedding rate
!
      qsshr(:)  =  0.0
      qhshr(:)  =  0.0
      qhlshr(:) =  0.0
      qhshh(:)  =  0.0
      csshr(:)  =  0.0
      chshr(:)  =  0.0
      chlshr(:)  =  0.0
      chshrr(:)  =  0.0
      chlshrr(:)  =  0.0
      vhshdr(:)  = 0.0
      vhlshdr(:)  = 0.0
      wetsfc(:)  = .false.
      wetgrowth(:)  = .false.
      wetsfchl(:)  = .false.
      wetgrowthhl(:)  = .false.


      do mgs = 1,ngscnt
!
!
!
      qhshr(mgs)  = Min( 0.0, qhwet(mgs) - qhdry(mgs) )  ! water that freezes should never be more than what sheds
      


      qhlshr(mgs)  =  Min( 0.0, qhlwet(mgs) - qhldry(mgs) )

!
! limit wet growth to only higher density particles
!
      qsshr(mgs)  =  0.0
!
!
!  no shedding for temperatures < 243.15 
!
      if ( temg(mgs) .lt. 243.15 ) then
       qsshr(mgs)  =  0.0
       qhshr(mgs)  =  0.0
       qhlshr(mgs) =  0.0
       vhshdr(mgs)  = 0.0
       vhlshdr(mgs)  = 0.0
       wetsfc(mgs) = .false.
       wetgrowth(mgs) = .false.
       wetsfchl(mgs) = .false.
       wetgrowthhl(mgs) = .false.
      end if
!
!  shed all at temperatures > 273.15
!
      if ( temg(mgs) .gt. tfr ) then

       qsshr(mgs)   = -qsdry(mgs)
       qhlshr(mgs)  = -qhldry(mgs)

       qhshr(mgs)  = -qhdry(mgs)
       vhshdr(mgs)  = -vhacw(mgs) - vhacr(mgs)
       vhlshdr(mgs)  = -vhlacw(mgs)
       qhwet(mgs)  = 0.0
       qhlwet(mgs) = 0.0
      end if
!
!      if (qhshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr  ) THEN
        wetsfc(mgs) =  (qhshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) .or. ( qhmlr(mgs) < -qxmin(lh) .and.  temg(mgs) > tfr )
        wetgrowth(mgs) = (qhshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr )
!      ENDIF

      if (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) THEN
        wetsfchl(mgs) = (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr ) .or. ( qhlmlr(mgs) < -qxmin(lhl) .and.  temg(mgs) > tfr )
        wetgrowthhl(mgs) = (qhlshr(mgs) .lt. 0.0 .and. temg(mgs) < tfr )
      ENDIF

      end do
!
      if ( ipconc .ge. 1 ) then
      do mgs = 1,ngscnt
      csshr(mgs)  = 0.0 ! (cx(mgs,ls)/(qx(mgs,ls)+1.e-20))*Min(0.0,qsshr(mgs))
      ! why is there a number loss for graupel for shedding? NEED TO CHECK THIS
       ! chshr(mgs)  = (cx(mgs,lh)/(qx(mgs,lh)+1.e-20))*qhshr(mgs)
       ! IF ( temg(mgs) < tfr ) chshr(mgs) = 0.0 ! no change to graupel number concentration for wet-growth shedding
       
       chshr(mgs) = 0.0 ! no change to graupel number concentration for wet-growth shedding
       
      !   tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
        ! Base the drop size on the shedding regime
            ! 8/26/2015 ERM updated to use shedalp and tmpdiam
            ! tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
            chshrr(mgs) =  rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*vshdgs(mgs,lh))  ! into rain 
      
      IF ( .false. ) THEN
      IF ( temg(mgs) < tfr ) THEN
         chshrr(mgs) = Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn0(lr)*vshd) ) ! maximum of dshd from shedding
      ELSE
        IF(imltshddmr > 0) THEN
          ! DTD: If Dmg < sheddiam, then assume complete melting into
          ! maximal raindrop.  Between sheddiam and sheddiam0, linearly ramp down to a 3 mm shed drop
          tmp = -Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) ! limit to maximum size allowed for rain
          tmp2 = -rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*vr3mm) ! conc. change for a 3 mm mean drop diameter
          chshrr(mgs) = tmp*(sheddiam0-xdia(mgs,lh,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lh,3)-sheddiam)/(sheddiam0-sheddiam)
          chshrr(mgs) = -Max(tmp,Min(tmp2,chshrr(mgs)))
        ELSE
         chshrr(mgs) = Min( chshr(mgs), rho0(mgs)*qhshr(mgs)/(xdn(mgs,lr)*Min(vr4p5mm,xvmx(lr))) ) ! limit to maximum size allowed for rain or 4.5mm diameter, whichever is smaller
!        chlmlrr(mgs) = rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl)))  ! into rain 
        ENDIF
      ENDIF
      ENDIF
      
      
      chlshr(mgs) = 0.0
      chlshrr(mgs) = 0.0
      IF ( lhl .gt. 1 ) THEN 
!         chlshr(mgs)  = (cx(mgs,lhl)/(qx(mgs,lhl)+1.e-20))*qhlshr(mgs)


       chlshr(mgs) = 0.0 ! no change to hail number concentration for wet-growth shedding
       
      !   tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
        ! Base the drop size on the shedding regime
            ! 8/26/2015 ERM updated to use shedalp and tmpdiam
            ! tmpdiam = (shedalp+alpha(mgs,lh))*xdia(mgs,lh,1)
            chlshrr(mgs) =  rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*vshdgs(mgs,lhl))  ! into rain 


      IF ( .false. ) THEN
        IF ( temg(mgs) < tfr ) THEN
          chlshrr(mgs) = Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn0(lr)*vshd) ) ! maximum of dshd from shedding
!         chlshrr(mgs) = Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn0(lr)*vr1mm) ) ! maximum of 1mm drops from shedding
        ELSE
          IF(imltshddmr > 0) THEN
            ! DTD: If Dmg < sheddiam, then assume complete melting into
            ! maximal raindrop.  Between sheddiam and sheddiam0, linearly ramp down to a 3 mm shed drop
            tmp = -Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*xvmx(lr)) ) ! limit to maximum size allowed for rain
            tmp2 = -rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*vr3mm) ! conc. change for a 3 mm mean drop diameter
            chlshrr(mgs) = tmp*(sheddiam0-xdia(mgs,lhl,3))/(sheddiam0-sheddiam)+tmp2*(xdia(mgs,lhl,3)-sheddiam)/(sheddiam0-sheddiam)
            chlshrr(mgs) = -Max(tmp,Min(tmp2,chlshrr(mgs)))
          ELSE
           chlshrr(mgs) = Min( chlshr(mgs), rho0(mgs)*qhlshr(mgs)/(xdn(mgs,lr)*Min(vr4p5mm,xvmx(lr))) ) ! limit to 4.5mm diameter or maximum size allowed for rain, whichever is smaller
!        chlmlrr(mgs) = rho0(mgs)*qhlmlr(mgs)/(Min(xdn(mgs,lr)*xvmx(lr), xdn(mgs,lhl)*xv(mgs,lhl)))  ! into rain 
          ENDIF
        ENDIF
      ENDIF

      ENDIF ! ( lhl > 1 )
      
      end do
      end if



!
!  final decisions
!
      do mgs = 1,ngscnt
!
!  Snow
!
      if ( qsshr(mgs) .lt. 0.0 ) then
      qsdpv(mgs) = 0.0
      qssbv(mgs) = 0.0
      else
      qsshr(mgs) = 0.0
      end if
!
!     if ( qsdry(mgs) .lt. qswet(mgs) ) then
!     qswet(mgs) = 0.0
!     else
!     qsdry(mgs) = 0.0
!     end if
!

! zero the shedding rates when wet snow/graupel included.
! shedding of wet snow/graupel is calculated after summing other sources/sinks.
      if (mixedphase) then
        qsshr(mgs) = 0.0
        qhshr(mgs) = 0.0
        csshr(mgs) = 0.0
        chshr(mgs) = 0.0
        chshrr(mgs) = 0.0
        vhshdr(mgs) = 0.0
        IF ( lhlw > 1 ) THEN
          qhlshr(mgs) = 0.0
          vhlshdr(mgs) = 0.0
          chlshr(mgs) = 0.0
          chlshrr(mgs) = 0.0
        ENDIF
      end if

!  graupel
!
!
      if ( wetgrowth(mgs) .or. (mixedphase .and. fhw(mgs) .gt. 0.05 .and. temg(mgs) .gt. 243.15) ) then
      

! soaking (when not advected liquid water film with graupel)

        IF ( lvol(lh) .gt. 1 .and. .not. mixedphase) THEN
        ! rescale volumes to maximum density
         rimdn(mgs,lh) = xdnmx(lh)
         raindn(mgs,lh) = xdnmx(lh)
         vhacw(mgs) = qhacw(mgs)*rho0(mgs)/rimdn(mgs,lh)
         vhacr(mgs) = qhacr(mgs)*rho0(mgs)/raindn(mgs,lh)
!        IF ( lvol(lh) .gt. 1 .and. wetgrowth(mgs) ) THEN
         IF ( xdn(mgs,lh) .lt. xdnmx(lh) ) THEN
         ! soak some liquid into the graupel
!           v1 = xdnmx(lh)*vx(mgs,lh)/(xdn(mgs,lh)*dtp) ! volume available for filling
           v1 = (1. - xdn(mgs,lh)/xdnmx(lh))*vx(mgs,lh)/(dtp) ! volume available for filling
!            tmp = (vx(mgs,lh)/rho0(mgs))*(xdnmx(lh) - xdn(mgs,lh)) ! max mixing ratio of liquid water that can be added
           v2 = rho0(mgs)*qhwet(mgs)/xdnmx(lh)  ! volume of frozen accretion
           
           vhsoak(mgs) = Min(v1,v2)
           
         ENDIF

         vhshdr(mgs) = Min(0.0, rho0(mgs)*qhwet(mgs)/xdnmx(lh) - vhacw(mgs) - vhacr(mgs) )
         
        ELSEIF ( lvol(lh) .gt. 1  .and. mixedphase ) THEN
!         vhacw(mgs) = rho0(mgs)*qhacw(mgs)/xdn0(lr)
!         vhacr(mgs) = rho0(mgs)*qhacr(mgs)/xdn0(lr)
        ENDIF
        

      qhdpv(mgs) = 0.0
!      qhsbv(mgs) = 0.0
      chdpv(mgs) = 0.0
!      chsbv(mgs) = 0.0

! collection efficiency modification

      IF ( ehi(mgs) .gt. 0.0 ) THEN
        qhaci(mgs) = Min(qimxd(mgs),qhaci0(mgs))  ! effectively sets collection eff to 1
        chaci(mgs) = Min(cimxd(mgs),chaci0(mgs))  ! effectively sets collection eff to 1
      ENDIF
      IF ( ehs(mgs) .gt. 0.0 ) THEN
!        qhacs(mgs) = Min(qsmxd(mgs),qhacs(mgs)/ehs(mgs))  ! effectively sets collection eff to 1
        qhacs(mgs) = Min(qsmxd(mgs),qhacs0(mgs)) !/ehs(mgs)                   ! divide out the collection efficiency
        chacs(mgs) = Min(csmxd(mgs),chacs0(mgs)) !/ehs(mgs)                   ! divide out the collection efficiency
        ehs(mgs) = ehsmax ! 1.0 ! min(ehsfrac*ehs(mgs),ehsmax)            ! modify it
        qhacs(mgs) = Min(qsmxd(mgs),qhacs(mgs))   ! plug it back in
      ENDIF

! be sure to catch particles with wet surfaces but not in wet growth to turn off Hallett-Mossop
      wetsfc(mgs) = .true.

      else
!        qhshr(mgs) = 0.0
      end if
!
!
!  hail
!
!      if ( lhl .gt. 1 .and. qhlshr(mgs) .lt. 0.0 ) then
      if ( wetgrowthhl(mgs) .or. (mixedphase .and. fhlw(mgs) .gt. 0.05 .and. temg(mgs) .gt. 243.15) ) then
!      if ( wetgrowthhl(mgs) ) then
       

      qhldpv(mgs) = 0.0
!      qhlsbv(mgs) = 0.0
      chldpv(mgs) = 0.0
!      chlsbv(mgs) = 0.0




        IF ( lvol(lhl) .gt. 1  .and. .not. mixedphase ) THEN
!        IF ( lvol(lhl) .gt. 1 .and. wetgrowthhl(mgs) ) THEN

         rimdn(mgs,lhl) = xdnmx(lhl) 
         raindn(mgs,lhl) = xdnmx(lhl) 
         vhlacw(mgs) = qhlacw(mgs)*rho0(mgs)/rimdn(mgs,lhl)
         vhlacr(mgs) = qhlacr(mgs)*rho0(mgs)/raindn(mgs,lhl)

         IF ( xdn(mgs,lhl) .lt. xdnmx(lhl) ) THEN
         ! soak some liquid into the hail
!           v1 = xdnmx(lhl)*vx(mgs,lhl)/(xdn(mgs,lhl)*dtp) ! volume available for filling
           v1 = (1. - xdn(mgs,lhl)/xdnmx(lhl))*vx(mgs,lhl)/(dtp) ! volume available for filling
!            tmp = (vx(mgs,lhl)/rho0(mgs))*(xdnmx(lhl) - xdn(mgs,lhl)) ! max mixing ratio of liquid water that can be added
           v2 = rho0(mgs)*qhlwet(mgs)/xdnmx(lhl)  ! volume of frozen accretion
           IF ( v1 > v2 ) THEN ! all the frozen stuff fits in
             vhlsoak(mgs) = v2
           ELSE  ! fill up the available space
             vhlsoak(mgs) = v1
           ENDIF
!           vhlacw(mgs) = 0.0
!           vhlacr(mgs) = Max( 0.0, v2 - v1 )
         ELSE
           vhlsoak(mgs) = 0.0
!           vhlacw(mgs) = 0.0
!           vhlacr(mgs) = rho0(mgs)*qhlwet(mgs)/raindn(mgs,lhl)
         
         ENDIF

         vhlshdr(mgs) = Min(0.0, rho0(mgs)*qhlwet(mgs)/xdnmx(lhl) - vhlacw(mgs) - vhlacr(mgs) )


        ELSEIF ( lvol(lhl) .gt. 1  .and. mixedphase ) THEN
!         vhlacw(mgs) = rho0(mgs)*qhlacw(mgs)/xdn0(lr)
!         vhlacr(mgs) = rho0(mgs)*qhlacr(mgs)/xdn0(lr)
        ENDIF

      IF ( ehli(mgs) .gt. 0.0 ) THEN
        qhlaci(mgs) = Min(qimxd(mgs),qhlaci0(mgs))  ! effectively sets collection eff to 1
        chlaci(mgs) = Min(cimxd(mgs),chlaci0(mgs))  ! effectively sets collection eff to 1
      ENDIF

!      IF ( ehls(mgs) .gt. 0.0 ) THEN
!        qhlacs(mgs) = Min(qsmxd(mgs),qhlacs(mgs)/ehls(mgs))
!      ENDIF
      IF ( ehls(mgs) .gt. 0.0 ) THEN
        qhlacs(mgs) = Min(qsmxd(mgs),qhlacs0(mgs)) !/ehls(mgs)                   ! divide out the collection efficiency
        chlacs(mgs) = Min(csmxd(mgs),chlacs0(mgs)) !/ehls(mgs)                   ! divide out the collection efficiency
        ehls(mgs) = ehsmax ! 1.0 ! min(ehsfrac*ehs(mgs),ehsmax)            ! modify it
!        qhlacs(mgs) = Min(qsmxd(mgs),qhlacs(mgs))   ! plug it back in
      ENDIF

      
!      qhlwet(mgs) = 1.0

! be sure to catch particles with wet surfaces but not in wet growth to turn off Hallett-Mossop
      wetsfchl(mgs) = .true.


      else
!      qhlshr(mgs) = 0.0
!      qhlwet(mgs) = 0.0
      end if


      end do
!
! Ice -> graupel conversion
!
      DO mgs = 1,ngscnt
      
      qhcni(mgs) = 0.0
      chcni(mgs) = 0.0
      chcnih(mgs) = 0.0
      vhcni(mgs) = 0.0
      
      IF ( iglcnvi .ge. 1 ) THEN
      IF ( temg(mgs) .lt. 273.0 .and. qiacw(mgs) - qidpv(mgs) .gt. 0.0 ) THEN
      
        
        tmp = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,li,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
        tmp = Min( Max( rimc3, tmp ), 900.0 )
        
        !  Assume that half the volume of the embryo is rime with density 'tmp'
        !  m = rhoi*(V/2) + rhorime*(V/2) = (rhoi + rhorime)*V/2
        !  V = 2*m/(rhoi + rhorime)
        
!        write(0,*)  'rime dens = ',tmp
        
        IF ( tmp .ge. 200.0 .or. iglcnvi >= 2 ) THEN
          r = Max( 0.5*(xdn(mgs,li) + tmp), xdnmn(lh) )
!          r = Max( r, 400. )
          qhcni(mgs) = (qiacw(mgs) - qidpv(mgs)) ! *float(iglcnvi)
          chcni(mgs) = cx(mgs,li)*qhcni(mgs)/qx(mgs,li)
!          chcnih(mgs) = rho0(mgs)*qhcni(mgs)/(1.6e-10)
          chcnih(mgs) = Min(chcni(mgs), rho0(mgs)*qhcni(mgs)/(r*xvmn(lh)) )
!          vhcni(mgs) = rho0(mgs)*2.0*qhcni(mgs)/(xdn(mgs,li) + tmp)
          vhcni(mgs) = rho0(mgs)*qhcni(mgs)/r
        ENDIF
      
      ELSEIF ( iglcnvi == 3 ) THEN

       IF ( temg(mgs) .lt. 273.0 .and. qiacw(mgs)*dtp > 2.*qxmin(lh) .and. gamice73fac*xmas(mgs,li) > xdnmn(lh)*xvmn(lh) ) THEN
      
        
        tmp = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,li,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
        tmp = Min( Max( rimc3, tmp ), 900.0 )
        
        !  Assume that half the volume of the embryo is rime with density 'tmp'
        !  m = rhoi*(V/2) + rhorime*(V/2) = (rhoi + rhorime)*V/2
        !  V = 2*m/(rhoi + rhorime)
        
!        write(0,*)  'rime dens = ',tmp
        ! convert to particles with the mass of the mass-weighted diameter
      !  massofmwr = gamice73fac*xmas(mgs,li)
        
        IF ( tmp .ge. xdnmn(lh)  ) THEN
          r = Max( 0.5*(xdn(mgs,li) + tmp), xdnmn(lh) )
!          r = Max( r, 400. )
          qhcni(mgs) = 0.5*qiacw(mgs)
          chcni(mgs) = qhcni(mgs)/(gamice73fac*xmas(mgs,li))
          chcnih(mgs) = Min(chcni(mgs), rho0(mgs)*qhcni(mgs)/(r*xvmn(lh)) )
!          vhcni(mgs) = rho0(mgs)*2.0*qhcni(mgs)/(xdn(mgs,li) + tmp)
          vhcni(mgs) = rho0(mgs)*qhcni(mgs)/r
        ENDIF
      
      ENDIF

      
      ENDIF
      ENDIF
      
      
      ENDDO
      
      
      qhlcnh(:) = 0.0
      chlcnh(:) = 0.0
      vhlcnh(:) = 0.0
      vhlcnhl(:) = 0.0
      zhlcnh(:) = 0.0

      qhcnhl(:) = 0.0
      chcnhl(:) = 0.0
      vhcnhl(:) = 0.0
      zhcnhl(:) = 0.0
      

      IF ( lhl .gt. 1  ) THEN
      
      IF ( ihlcnh == 1 ) THEN

!
!  Graupel (h) conversion to hail (hl) based on Milbrandt and Yau 2005b
!
      DO mgs = 1,ngscnt

!        IF ( lhl .gt. 1 .and. ipconc .ge. 5 .and. qx(mgs,lh) .gt. 1.0e-3 .and.
!     :        xdn(mgs,lh) .gt. 750. .and. qhshr(mgs) .lt. 0.0 .and.
!     :        xdia(mgs,lh,3) .gt. 1.e-3 ) THEN
        IF (  wetgrowth(mgs) .and. (xdn(mgs,lh) .gt. hldnmn .or. lvh < 1 ) .and.  & ! correct this when hail gets turned on
!        IF (  ( qhshr(mgs) .lt. 0.0 .or. rimdn(mgs,lh) .gt. 800. ) .and.   &
     &        rimdn(mgs,lh) .gt. 800. .and.   &
     &        xdia(mgs,lh,3) .gt. hlcnhdia .and. qx(mgs,lh) .gt. hlcnhqmin ) THEN
!     :        xdia(mgs,lh,3) .gt. 2.e-3 .and. qx(mgs,lh) .gt. 1.0e-3 ) THEN ! 0823.2008 erm test
!        IF ( xdia(mgs,lh,3) .gt. 1.e-3 ) THEN
        IF ( qhacw(mgs) .gt. 0.0 .and. qhacw(mgs) .gt. qhaci(mgs) .and. temg(mgs) .le. tfr-2.0 ) THEN
        ! dh0 is the diameter dividing wet growth from dry growth (Ziegler 1985), modified by MY05
!          dh0 = 0.01*(exp(temcg(mgs)/(1.1e4*(qx(mgs,lc)+qx(mgs,lr)) - 
!     :           1.3e3*qx(mgs,li) + 1.0e-3 ) ) - 1.0)
          x = (1.1e4*(rho0(mgs)*qx(mgs,lc)) - 1.3e3*rho0(mgs)*qx(mgs,li) + 1.0e-3 )
          IF ( x > 1.e-20 ) THEN
          arg = Min(70.0, (-temcg(mgs)/x )) ! prevent overflow of the exp function in 32 bit
          dh0 = 0.01*(exp(arg) - 1.0)
          ELSE
           dh0 = 1.e30
          ENDIF
!          dh0 = Max( dh0, 5.e-3 )
          
!         IF ( dh0 .gt. 0.0 ) write(0,*) 'dh0 = ',dh0
!         IF ( dh0 .gt. 1.0e-4 ) THEN
         IF ( xdia(mgs,lh,3)/dh0 .gt. 0.1 ) THEN 
!         IF ( xdia(mgs,lh,3) .lt. 20.*dh0 .and. dh0 .lt. 2.0*xdia(mgs,lh,3) ) THEN 
           tmp = qhacw(mgs) + qhacr(mgs) + qhaci(mgs) + qhacs(mgs)
!           qtmp = Min( 1.0, xdia(mgs,lh,3)/(2.0*dh0) )*(tmp)
           qtmp = Min( 100.0, xdia(mgs,lh,3)/(2.0*dh0) )*(tmp)
           IF ( .false. .and. qx(mgs,lhl) + qtmp*dtp .lt. 0.5e-3 ) THEN
             hdia1 = Max(dh0, xdia(mgs,lh,3) )
            qtmp = qtmp + Min(qxmxd(mgs,lh), Max( 0.0,   &
     &      ((pi*xdn(mgs,lh)*cx(mgs,lh)) / (6.0*rho0(mgs)*dtp))   &
     &      *exp(-hdia1/xdia(mgs,lh,1))   &
     &      *( (hdia1**3) + 3.0*(hdia1**2)*xdia(mgs,lh,1)   &
     &      + 6.0*(hdia1)*(xdia(mgs,lh,1)**2) + 6.0*(xdia(mgs,lh,1)**3) ) ) )

!c           qtmp = Min( qxmxd(mgs,lh), qtmp )
!c           tmp = tmp + Min( 0.5e-3/dtp, qtmp )
           ENDIF
!           write(0,*) 'dh0 = ',dh0,tmp,qx(mgs,lh)*1000.
!           qhlcnh(mgs) = Min( 0.5*(qx(mgs,lh))+tmp, xdia(mgs,lh,3)/(2.0*dh0)*(tmp) )
!           qhlcnh(mgs) = Min(  qxmxd(mgs,lh), xdia(mgs,lh,3)/(2.0*dh0)*(tmp) )
           qhlcnh(mgs) = Min(  qxmxd(mgs,lh), qtmp )
           
           IF ( ipconc .ge. 5 ) THEN
!           dh0 = Max( xdia(mgs,lh,3), Min( dh0, 5.e-3 ) ) ! don't create hail greater than 5mm diam. unless the graupel is larger
           dh0 = Min( dh0, 10.e-3 ) ! don't create hail greater than 10mm diam., which is the max graupel size
!           IF ( qx(mgs,lhl) > 0.1e-3 ) dh0 = xdia(mgs,lhl,3) ! when enough hail is established, don't dilute the size
           chlcnh(mgs) = Min( cxmxd(mgs,lh), rho0(mgs)*qhlcnh(mgs)/(pi*xdn(mgs,lh)*dh0**3/6.0) )
!           chlcnh(mgs) = Min( chlcnh(mgs), (1./8.)*rho0(mgs)*qhlcnh(mgs)/(xdn(mgs,lh)*xv(mgs,lh)) )
!           chlcnh(mgs) = Min( chlcnh(mgs), (1./2.)*rho0(mgs)*qhlcnh(mgs)/(xdn(mgs,lh)*xv(mgs,lh)) )
           r = rho0(mgs)*qhlcnh(mgs)/(xdn(mgs,lh)*xv(mgs,lh))  ! number of graupel particles at mean volume diameter
!           chlcnh(mgs) = Min( Max( 1./8.*r , chlcnh(mgs)), r )
!           chlcnh(mgs) = Min( chlcnh(mgs), r )
           chlcnh(mgs) = Max( chlcnh(mgs), r )
!           chlcnh(mgs) =  r 
           ENDIF
           
           vhlcnh(mgs) = rho0(mgs)*qhlcnh(mgs)/xdn(mgs,lh)
           vhlcnhl(mgs) = rho0(mgs)*qhlcnh(mgs)/Max(xdnmn(lhl), xdn(mgs,lh))
!           write(0,*) 'qhlcnh = ',qhlcnh(mgs)*1000.,chlcnh(mgs)
          ENDIF
!         write(0,*) 'graupel to hail conversion not complete! STOP!'
!         STOP
        ENDIF
        ENDIF
      
      ENDDO
      
      ELSEIF ( ihlcnh == 2 ) THEN ! 10-ice type conversion 

!
! Staka and Mansell (2005) type conversion -- assuming alphah = 0 for now!
!
!      hldia1 is set in micro_module and namelist
      do mgs = 1,ngscnt
!      qhlcnh(mgs) = 0.0
!      chlcnh(mgs) = 0.0
      if ( wetgrowth(mgs) .and. temg(mgs) .lt. tfr-5. .and. qx(mgs,lh) > qxmin(lh) ) then
      if ( qhacw(mgs).gt.1.e-6 .and. xdn(mgs,lh) > 700. ) then
      qhlcnh(mgs) =                                                   &
        ((pi*xdn(mgs,lh)*cx(mgs,lh)) / (6.0*rho0(mgs)*dtp))           &
       *exp(-hldia1/xdia(mgs,lh,1))                                    &
       *( (hldia1**3) + 3.0*(hldia1**2)*xdia(mgs,lh,1)                  &
        + 6.0*(hldia1)*(xdia(mgs,lh,1)**2) + 6.0*(xdia(mgs,lh,1)**3) )
      qhlcnh(mgs) =   min(qhlcnh(mgs),qhmxd(mgs))
      IF ( ipconc .ge. 5 ) THEN
        chlcnh(mgs) = Min( cxmxd(mgs,lh), cx(mgs,lh)*Exp(-hldia1/xdia(mgs,lh,1)))
!        chlcnh(mgs) = Min( cxmxd(mgs,lh), rho0(mgs)*qhlcnh(mgs)/(2.0*xmas(mgs,lh) ))
      ENDIF
           vhlcnh(mgs) = rho0(mgs)*qhlcnh(mgs)/xdn(mgs,lh)
           vhlcnhl(mgs) = rho0(mgs)*qhlcnh(mgs)/Max(xdnmn(lhl), xdn(mgs,lh))
      end if
      end if
      end do
      
      ENDIF
      
     ! convert low-density hail to graupel
      IF ( icvhl2h >= 1 ) THEN
      DO mgs = 1,ngscnt
        IF (  qx(mgs,lhl) > qxmin(lhl) .and. xdn(mgs,lhl) < 0.5*(xdnmn(lhl) + xdnmx(lhl)) ) THEN
          tmp = Min(0.95, 1. - 0.5*(1. + tanh(0.125*(xdn(mgs,lhl) - 1.01*xdnmn(lhl) )) ))
          qhcnhl(mgs) = tmp*qx(mgs,lhl)/dtp
          chcnhl(mgs) = cx(mgs,lhl)*qhcnhl(mgs)/qx(mgs,lhl)
          vhcnhl(mgs) = vx(mgs,lhl)*qhcnhl(mgs)/qx(mgs,lhl)
          
        ENDIF
      ENDDO
      
      ENDIF
      
      
      ENDIF ! lhl > 1


!
! Ziegler snow conversion to graupel
!
      DO mgs = 1,ngscnt

      qhcns(mgs) = 0.0
      chcns(mgs) = 0.0
      chcnsh(mgs) = 0.0
      vhcns(mgs) = 0.0

      qscnh(mgs) = 0.0
      cscnh(mgs) = 0.0
      vscnh(mgs) = 0.0

      IF ( ipconc .ge. 5 ) THEN

        ! test attempt at converting graupel to snow when not riming but growing by deposition
        IF ( temg(mgs) < tfr .and. qx(mgs,lh) .gt. qxmin(lh) .and. qhdpv(mgs) > qxmin(lh)*dtpinv  &
     &       .and. qhacw(mgs) < qxmin(lh)*dtpinv ) THEN
          IF ( xdn(mgs,lh) < 290. ) THEN
!          qscnh(mgs) = 2.*qhdpv(mgs)
!          cscnh(mgs) = cx(mgs,lh)*qscnh(mgs)/qx(mgs,lh)
!          vscnh(mgs) = rho0(mgs)*qscnh(mgs)/xdn(mgs,lh)
          ENDIF
        ENDIF


        IF ( qx(mgs,ls) .gt. qxmin(ls) .and. qsacw(mgs) .gt. 0.0 ) THEN

!      DATA VGRA/1.413E-2/  ! this is the volume (cm**3) of a 3mm diam. sphere
!    vgra = 1.4137e-8 m**3

!      DNNET=DNCNV-DNAGG
!      DQNET=QXCON+QSACC+SDEP
!
!      DNSCNV=EXP(-(ROS*XNS*VGRA/(RO*QI)))*((1.-(XNS*VGRA*ROS/
!     / (RO*QI)))*DNNET + (XNS**2*VGRA*ROS/(RO*QI**2))*DQNET)
!      IF(DNSCNV.LT.0.) DNSCNV=0.
!
!      QIHC=(ROS*VGRA/RO)*DNSCNV
!
!      QH=QH+DT*QIHC
!      QI=QI-DT*QIHC
!      XNH=XNH+DT*DNSCNV
!      XNS=XNS-DT*DNSCNV

        IF ( iglcnvs .eq. 1 ) THEN  ! Zrnic, Ziegler et al (1993)

        dnnet = cscnvis(mgs) + cscnis(mgs) - csacs(mgs)
        dqnet = qscnvi(mgs) + qscni(mgs) + qsacw(mgs) + qsdpv(mgs) + qssbv(mgs)

        a3 = 1./(rho0(mgs)*qx(mgs,ls))
        a1 = Exp( - xdn(mgs,ls)*cx(mgs,ls)*vgra*a3 )  ! EXP(-(ROS*XNS*VGRA/(RO*QI)))
! (1.-(XNS*VGRA*ROS/(RO*QI)))*DNNET
        a2 =  (1.-(cx(mgs,ls)*vgra*xdn(mgs,ls)*a3))*dnnet
! (XNS**2*VGRA*ROS/(RO*QI**2))*DQNET
        a4 = cx(mgs,ls)**2*vgra*xdn(mgs,ls)*a3/qx(mgs,ls)*dqnet

        chcns(mgs) = Max( 0.0, a1*(a2 + a4) )
        chcns(mgs) = Min( chcns(mgs), cxmxd(mgs,ls) )
        chcnsh(mgs) = chcns(mgs)

        qhcns(mgs) = Min( xdn(mgs,ls)*vgra*rhoinv(mgs)*chcns(mgs), qxmxd(mgs,ls) )
        vhcns(mgs) = rho0(mgs)*qhcns(mgs)/Max(xdn(mgs,ls),xdnmn(lh))
!        vhcns(mgs) = rho0(mgs)*qhcns(mgs)/Max(xdn(mgs,ls),400.)

        ELSEIF ( iglcnvs .ge. 2  ) THEN  ! treat like ice crystals, i.e., check for rime density (ERM)

          IF ( temg(mgs) .lt. 273.0 .and. ( qsacw(mgs) - qsdpv(mgs) .gt. 0.0 .or. &
              ( iglcnvs >= 3 .and. qsacw(mgs) > 2.*qxmin(lh) .and. gamsnow73fac*xmas(mgs,ls) > xdnmn(lh)*xvmn(lh)  ) ) ) THEN


        tmp = rimc1*(-((0.5)*(1.e+06)*xdia(mgs,lc,1))   &
     &                *((0.60)*vtxbar(mgs,ls,1))   &
     &                /(temg(mgs)-273.15))**(rimc2)
        tmp = Min( Max( rimc3, tmp ), 900.0 )

        !  Assume that half the volume of the embryo is rime with density 'tmp'
        !  m = rhoi*(V/2) + rhorime*(V/2) = (rhoi + rhorime)*V/2
        !  V = 2*m/(rhoi + rhorime)

!        write(0,*)  'rime dens = ',tmp

        IF ( iglcnvs == 2 ) THEN
        IF ( tmp .ge. 200.0  ) THEN
          r = Max( 0.5*(xdn(mgs,ls) + tmp), xdnmn(lh) )
!          r = Max( r, 400. )
          qhcns(mgs) = (qsacw(mgs) - qsdpv(mgs))
          chcns(mgs) = cx(mgs,ls)*qhcns(mgs)/qx(mgs,ls)
!          chcnih(mgs) = rho0(mgs)*qhcni(mgs)/(1.6e-10)
          chcnsh(mgs) = Min(chcns(mgs), rho0(mgs)*qhcns(mgs)/(r*xvmn(lh)) )
!          vhcni(mgs) = rho0(mgs)*2.0*qhcni(mgs)/(xdn(mgs,li) + tmp)
          vhcns(mgs) = rho0(mgs)*qhcns(mgs)/r
        ENDIF
        
        ELSEIF ( iglcnvs == 3 ) THEN
 
         ! convert to particles with the mass of the mass-weighted diameter
      !  massofmwr = gamice73fac*xmas(mgs,li)
        
        IF ( tmp .ge. xdnmn(lh)  ) THEN
          r = Max( 0.5*(xdn(mgs,ls) + tmp), xdnmn(lh) )
!          r = Max( r, 400. )
          qhcns(mgs) = 0.5*qsacw(mgs)
          chcns(mgs) = qhcns(mgs)/(gamsnow73fac*xmas(mgs,ls))
          chcnsh(mgs) = Min(chcns(mgs), rho0(mgs)*qhcns(mgs)/(r*xvmn(lh)) )
          vhcns(mgs) = rho0(mgs)*qhcns(mgs)/r
        ENDIF

        ENDIF

      ENDIF

        ENDIF


        ENDIF

       ELSE ! single moment lfo

        qhcns(mgs) = 0.001*ehscnv(mgs)*max((qx(mgs,ls)-6.e-4),0.0)
        qhcns(mgs) = min(qhcns(mgs),qxmxd(mgs,ls))
        IF ( lvol(lh) .ge. 1 ) vhcns(mgs) = rho0(mgs)*qhcns(mgs)/Max(xdn(mgs,ls),400.)

       ENDIF
      ENDDO
!
!
!  heat budget for rain---not all rain that collects ice can freeze
!
!
!
      if ( irwfrz .gt. 0 .and. .not. mixedphase) then
!
      do mgs = 1,ngscnt
!
!  compute total rain that freeze when it interacts with cloud ice
!
      qrztot(mgs) = qrfrz(mgs) + qiacr(mgs) + qsacr(mgs)
!
!  compute the maximum amount of rain that can freeze
!  Used to limit freezing to 4*qrmxd, but now allow all rain to freeze if possible
!
      qrzmax(mgs) =   &
     &  ( xdia(mgs,lr,1)*rwvent(mgs)*cx(mgs,lr)*fwet1(mgs) )
      qrzmax(mgs) = max(qrzmax(mgs), 0.0)
      qrzmax(mgs) = min(qrztot(mgs), qrzmax(mgs))
      qrzmax(mgs) = min(qx(mgs,lr)/dtp, qrzmax(mgs))

      IF ( temcg(mgs) < -30. ) THEN ! allow all to freeze if T < -30 because fwet becomes invalid (negative)
        qrzmax(mgs) = qx(mgs,lr)/dtp
      ENDIF
!      qrzmax(mgs) = min(4.*qrmxd(mgs), qrzmax(mgs))
!
!  compute the correction factor
!
!      IF ( qrztot(mgs) .gt. qxmin(lr) ) THEN
      IF ( qrztot(mgs) .gt. qrzmax(mgs) .and. qrztot(mgs) .gt. qxmin(lr) ) THEN
        qrzfac(mgs) = qrzmax(mgs)/(qrztot(mgs))
      ELSE
        qrzfac(mgs) = 1.0
      ENDIF
      qrzfac(mgs) = min(1.0, qrzfac(mgs))
!
      end do
!
!
! now correct the above sources
!
!
      do mgs = 1,ngscnt
      if ( temg(mgs) .le. 273.15 .and. qrzfac(mgs) .lt. 1.0 ) then
      qrfrz(mgs)   = qrzfac(mgs)*qrfrz(mgs)
      qrfrzs(mgs)  = qrzfac(mgs)*qrfrzs(mgs)
      qrfrzf(mgs)  = qrzfac(mgs)*qrfrzf(mgs)
      qiacr(mgs)   = qrzfac(mgs)*qiacr(mgs)
      qsacr(mgs)   = qrzfac(mgs)*qsacr(mgs)
      qiacrf(mgs)  = qrzfac(mgs)*qiacrf(mgs)
      qiacrs(mgs)  = qrzfac(mgs)*qiacrs(mgs)
      crfrz(mgs)   = qrzfac(mgs)*crfrz(mgs)
      crfrzf(mgs)  = qrzfac(mgs)*crfrzf(mgs)
      crfrzs(mgs)  = qrzfac(mgs)*crfrzs(mgs)
      ciacr(mgs)   = qrzfac(mgs)*ciacr(mgs)
      ciacrf(mgs)  = qrzfac(mgs)*ciacrf(mgs)
      ciacrs(mgs)  = qrzfac(mgs)*ciacrs(mgs)

      
       vrfrzf(mgs)  = qrzfac(mgs)*vrfrzf(mgs)
       viacrf(mgs)  = qrzfac(mgs)*viacrf(mgs)
      end if
      end do
!
!
!
      end if
!
!
!
!  evaporation of rain
!
!
!
      qrcev(:) = 0.0
      crcev(:) = 0.0


      do mgs = 1,ngscnt
!
      IF ( qx(mgs,lr) .gt. qxmin(lr) ) THEN

      qrcev(mgs) =   &
     &  fvce(mgs)*cx(mgs,lr)*rwvent(mgs)*rwcap(mgs)*evapfac
! this line to allow condensation on rain:
      IF ( rcond .eq. 1 ) THEN
        qrcev(mgs) = min(qrcev(mgs), qxmxd(mgs,lv))
! this line to have evaporation only:
      ELSE
        qrcev(mgs) = min(qrcev(mgs), 0.0)
      ENDIF

      qrcev(mgs) = max(qrcev(mgs), -qrmxd(mgs))
!      if ( temg(mgs) .lt. 273.15 ) qrcev(mgs) = 0.0
      IF ( qrcev(mgs) .lt. 0. .and. lnr > 1 ) THEN
!        qrcev(mgs) =   -qrmxd(mgs)
!        crcev(mgs) = (rho0(mgs)/(xmas(mgs,lr)+1.e-20))*qrcev(mgs)
      crcev(mgs) = (cx(mgs,lr)/(qx(mgs,lr)))*qrcev(mgs)
      ELSE
         crcev(mgs) = 0.0
      ENDIF
!      if ( temg(mgs) .lt. 273.15 ) crcev(mgs) = 0.0
!
      ENDIF

      end do
!
! evaporation/condensation of wet graupel and snow
!
      qscev(:) = 0.0
      cscev(:) = 0.0
      qhcev(:) = 0.0
      chcev(:) = 0.0
      qhlcev(:) = 0.0
      chlcev(:) = 0.0

!
!
!
!  ICE MULTIPLICATION: Two modes (rimpa, and rimpb)
!  (following Cotton et al. 1986)
!
 
      chmul1(:) =  0.0
      chlmul1(:) =  0.0
      csmul1(:) = 0.0
!
      qhmul1(:) =  0.0
      qhlmul1(:) =  0.0
      qsmul1(:) =  0.0

      do mgs = 1,ngscnt
 
       ltest =  qx(mgs,lh) .gt. qxmin(lh)
       IF ( lhl > 1 )  ltest =  ltest .or. qx(mgs,lhl) .gt. qxmin(lhl)
       
      IF ( (itype1 .ge. 1 .or. itype2 .ge. 1 )   &
     &              .and. qx(mgs,lc) .gt. qxmin(lc)) THEN
      if ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 271.15 ) then
       IF ( ipconc .ge. 2 ) THEN
        IF ( xv(mgs,lc) .gt. 0.0     &
     &     .and.  ltest &
!     .and. itype2 .ge. 2    &
     &       ) THEN
!
!  Ziegler et al. 1986 Hallett-Mossop process.  VSTAR = 7.23e-15 (vol of 12micron radius)
!
         ex1 = (1./250.)*Exp(-7.23e-15/xv(mgs,lc))
       IF ( itype2 .le. 2 ) THEN
         ft = Max(0.0,Min(1.0,-0.11*temcg(mgs)**2 - 1.1*temcg(mgs)-1.7))
       ELSE
        IF ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 267.15 ) THEN
          ft = 0.5
        ELSEIF (temg(mgs) .ge. 267.15 .and. temg(mgs) .le. 269.15 ) THEN
          ft = 1.0
        ELSEIF (temg(mgs) .ge. 269.15 .and. temg(mgs) .le. 271.15 ) THEN
          ft = 0.5
        ELSE 
          ft = 0.0
        ENDIF
       ENDIF
!        rhoinv = 1./rho0(mgs)
!        DNSTAR = ex1*cglacw(mgs)
        
       IF ( ft > 0.0 ) THEN
        
        IF ( itype2 > 0 ) THEN
         IF ( qx(mgs,lh) .gt. qxmin(lh) .and. (.not. wetsfc(mgs))  ) THEN
          chmul1(mgs) = ft*ex1*chacw(mgs)
!          chmul1(mgs) = Min( ft*ex1*chacw(mgs), ft*(30.*1.e+06)*rho0(mgs)*qhacw(mgs) ) ! 1.e+6 converts kg to mg; Saunders & Hosseini (2001) average of about 30 crystals per mg
          qhmul1(mgs) = cimas0*chmul1(mgs)*rhoinv(mgs)
         ENDIF
         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs))  ) THEN
            chlmul1(mgs) = (ft*ex1*chlacw(mgs))
            qhlmul1(mgs) = cimas0*chlmul1(mgs)*rhoinv(mgs)
           ENDIF
         ENDIF
        ENDIF ! itype2

        IF ( itype1 > 0 ) THEN
         IF ( qx(mgs,lh) .gt. qxmin(lh) .and. (.not. wetsfc(mgs))  ) THEN
          tmp = ft*(3.5e+08)*rho0(mgs)*qhacw(mgs)
          chmul1(mgs) = chmul1(mgs) + tmp
          qhmul1(mgs) = qhmul1(mgs) + cimas0*tmp*rhoinv(mgs)
         ENDIF
         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs)) ) THEN
            tmp = ft*(3.5e+08)*rho0(mgs)*qhlacw(mgs)
            chlmul1(mgs) = chlmul1(mgs) + tmp
            qhlmul1(mgs) = qhlmul1(mgs) + cimas0*tmp*rhoinv(mgs)
           ENDIF
         ENDIF
        ENDIF ! itype1
        
        ENDIF ! ft

        ENDIF ! xv(mgs,lc) .gt. 0.0 .and.

       ELSE ! ipconc .lt. 2
!
!  define the temperature function
!
      fimt1(mgs) = 0.0
!
! Cotton et al. (1986) version
!
      if ( temg(mgs) .ge. 268.15 .and. temg(mgs) .le. 270.15 ) then
        fimt1(mgs) = 1.0 -(temg(mgs)-268.15)/2.0
      elseif (temg(mgs) .le. 268.15 .and. temg(mgs) .ge. 265.15 ) then
        fimt1(mgs) = 1.0 +(temg(mgs)-268.15)/3.0
      ELSE 
        fimt1(mgs) = 0.0
      end if
!
! Ferrier (1994) version
!
      if ( temg(mgs) .ge. 265.15 .and. temg(mgs) .le. 267.15 ) then
        fimt1(mgs) = 0.5
      elseif (temg(mgs) .ge. 267.15 .and. temg(mgs) .le. 269.15 ) then
        fimt1(mgs) = 1.0
      elseif (temg(mgs) .ge. 269.15 .and. temg(mgs) .le. 271.15 ) then
        fimt1(mgs) = 0.5
      ELSE 
        fimt1(mgs) = 0.0
      end if
!
!
!   type I:  350 splinters are formed for every 1e-3 grams of cloud
!            water accreted by graupel/hail (note converted to MKS units)
!            3.5e+8 has units of 1/kg
!
      IF ( itype1 .ge. 1 ) THEN
       fimta(mgs) = (3.5e+08)*rho0(mgs)
      ELSE
       fimta(mgs) = 0.0
      ENDIF

!
!
!   type II:  1 splinter formed for every 250 cloud droplets larger than
!             24 micons in diameter (12 microns in radius) accreted by
!             graupel/hail
!
!
      fimt2(mgs) = 0.0
      xcwmas = xmas(mgs,lc) * 1000.
!
      IF ( itype2 .ge. 1 ) THEN
      if ( xcwmas.lt.1.26e-9 ) then
        fimt2(mgs) = 0.0
      end if
      if ( xcwmas .le. 3.55e-9 .and. xcwmas .ge. 1.26e-9 ) then
        fimt2(mgs) = (2.27)*alog(xcwmas) + 13.39
      end if
      if ( xcwmas .gt. 3.55e-9 ) then
        fimt2(mgs) = 1.0
      end if

      fimt2(mgs) = min(fimt2(mgs),1.0)
      fimt2(mgs) = max(fimt2(mgs),0.0)
      
      ENDIF
!
!     qhmul2 = 0.0
!     qsmul2 = 0.0
!
!     qhmul2 =
!    >  (4.0e-03)*fimt1(mgs)*fimt2(mgs)*qhacw(mgs)
!     qsmul2 =
!    >  (4.0e-03)*fimt1(mgs)*fimt2(mgs)*qsacw(mgs)
!
!      cimas0 = (1.0e-12)
!      cimas0 = 2.5e-10
      IF ( .not. wetsfc(mgs) ) THEN
      chmul1(mgs) =  fimt1(mgs)*(fimta(mgs) +   &
     &                           (4.0e-03)*fimt2(mgs))*qhacw(mgs)
      ENDIF
!
      qhmul1(mgs) =  chmul1(mgs)*(cimas0/rho0(mgs))


         IF ( lhl .gt. 1 ) THEN
           IF ( qx(mgs,lhl) .gt. qxmin(lhl) .and. (.not. wetsfchl(mgs)) ) THEN
            tmp = fimt1(mgs)*(fimta(mgs) +   &
     &                           (4.0e-03)*fimt2(mgs))*qhlacw(mgs)
            chlmul1(mgs) =  tmp
            qhlmul1(mgs) = cimas0*tmp*rhoinv(mgs)
           ENDIF
         ENDIF

!      qsmul1(mgs) =  csmul1(mgs)*(cimas0/rho0(mgs))
!
      ENDIF ! ( ipconc .ge. 2 )
      
      end if ! (in temperature range)
      
      ENDIF ! ( itype1 .eq. 1 .or. itype2 .eq. 1)
!
      end do
!
!
!
!     end if
!
!     end do
!
!
! ICE MULTIPLICATION FROM SNOW
!   Lo and Passarelli 82 / Willis and Heymsfield 89 / Schuur and Rutledge 00b
!   using kfrag as fragmentation rate (s-1) / 500 microns as char mean diam for max snow mix ratio
!
      csmul(:) = 0.0
      qsmul(:) = 0.0
      
      IF ( isnwfrac /= 0 ) THEN
      do mgs = 1,ngscnt
       IF (temg(mgs) .gt. 265.0) THEN !{
        if (xdia(mgs,ls,1) .gt. 100.e-6 .and. xdia(mgs,ls,1) .lt. 2.0e-3) then  ! equiv diameter 100microns to 2mm

        tmp = rhoinv(mgs)*pi*xdn(mgs,ls)*cx(mgs,ls)*(500.e-6)**3
        qsmul(mgs) = Max( kfrag*( qx(mgs,ls) - tmp ) , 0.0 )

        qsmul(mgs) = Min( qxmxd(mgs,li), qsmul(mgs) )
        csmul(mgs) = Min( cxmxd(mgs,li), rho0(mgs)*qsmul(mgs)/mfrag )

        endif
       ENDIF !}
      enddo
      ENDIF

!
!  frozen rain-rain interaction....
!
!
!
!
!  rain-ice interaction
!
!
      do mgs = 1,ngscnt
      qracif(mgs) = qraci(mgs)
      cracif(mgs) = craci(mgs)
!      ciacrf(mgs) = ciacr(mgs)
      end do
!
! 
!  vapor to pristine ice crystals   UP
!
!
!
!  compute the nucleation rate
!
!     do mgs = 1,ngscnt
!     idqis = 0
!     if ( ssi(mgs) .gt. 1.0 ) idqis = 1
!     fiinit(mgs) = (felv(mgs)**2)/(cp*rw)
!     dqisdt(mgs) = (qx(mgs,lv)-qis(mgs))/
!    >  (1.0 + fiinit(mgs)*qis(mgs)/tsqr(mgs))
!     qidsvp(mgs) = dqisdt(mgs)
!     cnnt = min(cnit*exp(-temcg(mgs)*bta1),1.0e+09)
!     qiint(mgs) = 
!    >  il5(mgs)*idqis*(1.0/dtp)
!    <  *min((6.88e-13)*cnnt/rho0(mgs), 0.25*dqisdt(mgs)) 
!     end do
!
!  Meyers et al. (1992; JAS) and Ferrier (1994) primary ice nucleation
!
      cmassin = cimasn  ! 6.88e-13
      do mgs = 1,ngscnt
      qiint(mgs) = 0.0
      ciint(mgs) = 0.0
      qicicnt(mgs) = 0.0
      cicint(mgs) = 0.0
      qipipnt(mgs) = 0.0
      cipint(mgs) = 0.0
      ccitmp = 0.0
      IF ( icenucopt == 1 .or. icenucopt == -10 .or. icenucopt == -11 ) THEN
      if ( ( temg(mgs) .lt. 268.15 .or.  &
!     : ( imeyers5 .and. temg(mgs) .lt.  273.0) ) .and.    &
     & ( imeyers5 .and. temg(mgs) .lt.  272.0 .and. temgkm2(mgs) .lt. tfr) ) .and.    &
     &    ciintmx .gt. (cx(mgs,li)+ccitmp)  &
!     :    .and. cninm(mgs) .gt. 0.   &
     &     ) then
      fiinit(mgs) = (felv(mgs)**2)/(cp*rw)
      dqisdt(mgs) = (qx(mgs,lv)-qis(mgs))/   &
     &  (1.0 + fiinit(mgs)*qis(mgs)/tsqr(mgs))
!      qidsvp(mgs) = dqisdt(mgs)
      idqis = 0
      if ( ssi(mgs) .gt. 1.0 ) THEN
      idqis = 1 
      dzfacp = max( float(kgsp(mgs)-kgs(mgs)), 0.0 )
      dzfacm = max( float(kgs(mgs)-kgsm(mgs)), 0.0 )
      qiint(mgs) =   &
     &  idqis*il5(mgs)   &
     &  *(cmassin/rho0(mgs))   &
     &  *max(0.0,wvel(mgs))   &
     &  *max((cninp(mgs)-cninm(mgs)),0.0)/gz(igs(mgs),jgs,kgs(mgs))   &
     &  /((dzfacp+dzfacm))

      qiint(mgs) = min(qiint(mgs), max(0.25*dqisdt(mgs),0.0)) 
      ciint(mgs) = qiint(mgs)*rho0(mgs)/cmassin
!
! limit new crystals so it does not increase the current concentration
!  above ciintmx 20,000 per liter (2.e7 per m**3)
!
!      ciintmx = 1.e9
!      ciintmx = 1.e9
      IF ( icenucopt /= -10 ) THEN
      
        IF ( lcin > 1 ) THEN
          ciint(mgs) = Min(ciint(mgs), ccin(mgs))
          ccin(mgs) = ccin(mgs) - ciint(mgs)
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)
        ELSEIF ( lcina > 1 ) THEN
          ciint(mgs) = Max(0.0, Min( ciint(mgs), Min( cnina(mgs), ciintmx ) - cina(mgs) ))
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)
      
        ELSEIF ( icenucopt == 1 .and. ciint(mgs) .gt. Max(0.0, ciintmx - cx(mgs,li) - ccitmp )*dtpinv  ) THEN
          ciint(mgs) = Max(0.0, ciintmx - (cx(mgs,li)) )*dtpinv 
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)

        ELSEIF ( icenucopt == -11 .and. dtp*ciint(mgs) .gt. ( cnina(mgs) - (cx(mgs,li) - ccitmp))) THEN
          ciint(mgs) = Max(0.0,  cnina(mgs) - (cx(mgs,li)+ccitmp)*dtpinv )
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)

        ENDIF
      ENDIF
      
      end if
      endif

      ELSEIF ( icenucopt == 2 .or. icenucopt == -1 .or. icenucopt == -2 ) THEN
      
        IF ( ( temg(mgs) .lt. 268.15 .and. ssw(mgs) > 1.0 ) .or. ssi(mgs) > 1.25 ) THEN
          IF ( lcin > 1 ) THEN
           ciint(mgs) = Min(cnina(mgs), ccin(mgs))
           ciint(mgs) = Min( ciint(mgs), Max(0.0, ciintmx - (cx(mgs,li) - ccitmp) ) ) ! do not initiate ice beyond concentration of ciintmx
           ccin(mgs) = ccin(mgs) - ciint(mgs)
           ciint(mgs) = ciint(mgs)*dtpinv ! convert total initiation to a rate
          ELSE
           ciint(mgs) = Max( 0.0, cnina(mgs) - cina(mgs) )*dtpinv
          ENDIF
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)

          fiinit(mgs) = (felv(mgs)**2)/(cp*rw)
          dqisdt(mgs) = (qx(mgs,lv)-qis(mgs))/(1.0 + fiinit(mgs)*qis(mgs)/tsqr(mgs))
          qiint(mgs) = min(qiint(mgs), max(0.25*dqisdt(mgs),0.0))
          ciint(mgs) = qiint(mgs)*rho0(mgs)/cmassin
        ENDIF
      
      
      
      ELSEIF ( icenucopt == 3 ) THEN
        IF (  temg(mgs) .lt. 268.15 ) THEN
          IF ( lcin > 1 ) THEN
           ciint(mgs) = Min(cnina(mgs), ccin(mgs))
           ciint(mgs) = Min( ciint(mgs), Max(0.0, ciintmx - (cx(mgs,li) + ccitmp) ) ) ! do not initiate ice beyond concentration of ciintmx
           ccin(mgs) = ccin(mgs) - ciint(mgs)
           ciint(mgs) = ciint(mgs)*dtpinv ! convert total initiation to a rate
          ELSE
           ciint(mgs) = Max( 0.0, cnina(mgs) - cina(mgs) )*dtpinv
          ENDIF
          qiint(mgs) = ciint(mgs)*cmassin/rho0(mgs)
        ENDIF

      ENDIF
!
      if ( xplate(mgs) .eq. 1 ) then
      qipipnt(mgs) = qiint(mgs)
      cipint(mgs) = ciint(mgs)
      end if
!
      if ( xcolmn(mgs) .eq. 1 ) then
      qicicnt(mgs) = qiint(mgs)
      cicint(mgs) = ciint(mgs)
      end if
!
!     qipipnt(mgs) = 0.0
!     qicicnt(mgs) = qiint(mgs)
!
      end do
!
! 

!
!  vapor to cloud droplets   UP
!
      if (ndebug .gt. 0 ) write(0,*) 'dbg = 8'
!
!
      if (ndebug .gt. 0 ) write(0,*) 'Collection: set 3-component'
!
!  time for riming....
!
!     rimtim = 240.0
!     dtrim = rimtim
!     xacrtim  = 120.0
!     tranfr = 0.50
!     tranfw = 0.50
!
!  coefficients for riming
!
!     rimc1 = 300.00
!     rimc2 = 0.44
!
! 
!  zero som arrays
!
!
      do mgs = 1,ngscnt
      qrshr(mgs) = 0.0
      qsshrp(mgs) = 0.0
      qhshrp(mgs) = 0.0
      end do
!
!
!  first sum all of the shed rain
!
!
      do mgs = 1,ngscnt
      qrshr(mgs) = qsshr(mgs) + qhshr(mgs) + qhlshr(mgs)
      crshr(mgs) = chshrr(mgs)/rzxh(mgs) + chlshrr(mgs)/rzxhl(mgs)
      IF ( ipconc .ge. 3 ) THEN
!       crshr(mgs) = Max(crshr(mgs), rho0(mgs)*qrshr(mgs)/(xdn(mgs,lr)*vr1mm) )
      ENDIF
      end do 
!
!
!

!
!
!
!
      IF ( ipconc .ge. 1 ) THEN
!
!
!  concentration production terms
!
!  YYY
!
!
!       DO mgs = 1,ngscnt
       pccwi(:) = 0.0
       pccwd(:) = 0.0
       pccii(:) = 0.0
       pccin(:) = 0.0
       pccid(:) = 0.0
       pcisi(:) = 0.0
       pcisd(:) = 0.0
       pcrwi(:) = 0.0
       pcrwd(:) = 0.0
       pcswi(:) = 0.0
       pcswd(:) = 0.0
       pchwi(:) = 0.0
       pchwd(:) = 0.0
       pchli(:) = 0.0
       pchld(:) = 0.0
!       ENDDO
!
!  Cloud ice
!
!      IF ( ipconc .ge. 1 ) THEN

      IF ( warmonly < 0.5 ) THEN
      do mgs = 1,ngscnt
      pccii(mgs) =   &
     &   il5(mgs)*cicint(mgs)*(1. - ffrzs)   &
     &  +il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs)   &
     &  +cicichr(mgs))*(1. - ffrzs)   &
     &  +chmul1(mgs)   &
     &  +chlmul1(mgs)    &
     &  + csplinter(mgs) + csplinter2(mgs)   &
     &  +csmul(mgs)
!     >  + nsplinter*(crfrzf(mgs) + crfrz(mgs))
      pccid(mgs) =   &
     &   il5(mgs)*(-cscni(mgs) - cscnvi(mgs) & ! - cwaci(mgs)   &
     &  -craci(mgs)    &
     &  -csaci(mgs)   &
     &  -chaci(mgs) - chlaci(mgs)   &
     &  -chcni(mgs))   &
     &  +il5(mgs)*cisbv(mgs)   &
     &  -(1.-il5(mgs))*cimlr(mgs)

      pccin(mgs) = ciint(mgs)
      

      end do
      ELSEIF ( warmonly < 0.8 ) THEN
      do mgs = 1,ngscnt
      
!      qiint(mgs) = 0.0
!      cicint(mgs) = 0.0
!      qicicnt(mgs) = 0.0
      
      pccii(mgs) =   &
     &   il5(mgs)*cicint(mgs)*(1. - ffrzs)   &
     &  +il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs)   &
     &  +cicichr(mgs))*(1. - ffrzs)   &
     &  +chmul1(mgs)   &
     &  +chlmul1(mgs)    &
     &  + csplinter(mgs) + csplinter2(mgs)   &
     &  +csmul(mgs)
      pccid(mgs) =   &
!     &   il5(mgs)*(-cscni(mgs) - cscnvi(mgs) & ! - cwaci(mgs)   &
!     &  -craci(mgs)    &
!     &  -csaci(mgs)   &
!     &  -chaci(mgs) - chlaci(mgs)   &
!     &  -chcni(mgs))   &
     &  +il5(mgs)*cisbv(mgs)   &
     &  -(1.-il5(mgs))*cimlr(mgs)

      pccin(mgs) = ciint(mgs)

      end do
      ENDIF ! warmonly

      
!      ENDIF ! ( ipconc .ge. 1 )
!
!  Cloud water
!
      IF ( ipconc .ge. 2 ) THEN
      
      do mgs = 1,ngscnt
      pccwi(mgs) =  (0.0) ! + (1-il5(mgs))*(-cirmlw(mgs))
      
      IF ( warmonly < 0.5 ) THEN
      pccwd(mgs) =    &
     &  - cautn(mgs) +   &
     &  il5(mgs)*(-ciacw(mgs)-cwfrzp(mgs)-cwctfzp(mgs)   &
     &  -cwfrzc(mgs)-cwctfzc(mgs)   &
     &   )   &
     &  -cracw(mgs) -csacw(mgs)  -chacw(mgs) - chlacw(mgs)
      ELSEIF ( warmonly < 0.8 ) THEN
      pccwd(mgs) =    &
     &  - cautn(mgs) +   &
     &  il5(mgs)*(  &
     & -ciacw(mgs)-cwfrzp(mgs)-cwctfzp(mgs)   &
     &  -cwfrzc(mgs)-cwctfzc(mgs)   &
     &   )   &
     &  -cracw(mgs) -chacw(mgs) -chlacw(mgs) 
      ELSE
      
!       tmp3d(igs(mgs),jy,kgs(mgs)) = crcnw(mgs)

!       cracw(mgs) = 0.0 ! turn off accretion
!       qracw(mgs) = 0.0
!       crcev(mgs) = 0.0 ! turn off evap
!       qrcev(mgs) = 0.0 ! turn off evap
!       cracr(mgs) = 0.0 ! turn off self collection
       
       
!       cautn(mgs) = 0.0 
!       crcnw(mgs) = 0.0
!       qrcnw(mgs) = 0.0

      pccwd(mgs) =    &
     &  - cautn(mgs) -cracw(mgs)
      ENDIF


      IF ( -pccwd(mgs)*dtp .gt. cx(mgs,lc) ) THEN
!       write(0,*) 'OUCH! pccwd(mgs)*dtp .gt. ccw(mgs) ',pccwd(mgs),cx(mgs,lc)
!       write(0,*) 'qc = ',qx(mgs,lc)
!       write(0,*) -ciacw(mgs)-cwfrzp(mgs)-cwctfzp(mgs)-cwfrzc(mgs)-cwctfzc(mgs)
!       write(0,*)  -cracw(mgs) -csacw(mgs)  -chacw(mgs)
!       write(0,*) - cautn(mgs)

       frac = -cx(mgs,lc)/(pccwd(mgs)*dtp)
       pccwd(mgs) = -cx(mgs,lc)/dtp

        ciacw(mgs)   = frac*ciacw(mgs)
        cwfrzp(mgs)  = frac*cwfrzp(mgs)
        cwctfzp(mgs) = frac*cwctfzp(mgs)
        cwfrzc(mgs)  = frac*cwfrzc(mgs)
        cwctfzc(mgs) = frac*cwctfzc(mgs)
        cwctfz(mgs) = frac*cwctfz(mgs)
        cracw(mgs)   = frac*cracw(mgs)
        csacw(mgs)   = frac*csacw(mgs)
        chacw(mgs)   = frac*chacw(mgs)
        cautn(mgs)   = frac*cautn(mgs)
       
        pccii(mgs) = pccii(mgs) - (1.-frac)*il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs))
        IF ( lhl .gt. 1 ) chlacw(mgs)   = frac*chlacw(mgs)

!       STOP
      ENDIF

      end do

      ENDIF ! ipconc

!
!  Rain
!
      IF ( ipconc .ge. 3 ) THEN

      do mgs = 1,ngscnt

      IF ( warmonly < 0.5 ) THEN
      pcrwi(mgs) = &
!     >   cracw(mgs) +    &
     &   crcnw(mgs)   &
     &  +(1-il5(mgs))*(   &
     &    -chmlrr(mgs)/rzxh(mgs)   &
     &    -chlmlrr(mgs)/rzxhl(mgs)   &
     &    -csmlr(mgs)/rzxs(mgs)     &
     &   - cimlr(mgs) )   &
     &  -crshr(mgs)             !null at this point when wet snow/graupel included
      pcrwd(mgs) =   &
     &   il5(mgs)*(-ciacr(mgs) - crfrz(mgs) ) & ! - cipacr(mgs))
!     >  -csacr(mgs)   &
     &  - chacr(mgs) - chlacr(mgs)   &
     &  +crcev(mgs)   &
     &  - cracr(mgs)
!     >  -il5(mgs)*ciracr(mgs)
      ELSEIF ( warmonly < 0.8 ) THEN
       pcrwi(mgs) = &
     &   crcnw(mgs)   &
     &  +(1-il5(mgs))*(   &
     &    -chmlrr(mgs)/rzxh(mgs)    &
     &    -chlmlrr(mgs)/rzxhl(mgs)   &
     &    -csmlr(mgs)     &
     &   - cimlr(mgs) )   &
     &  -crshr(mgs)             !null at this point when wet snow/graupel included
      pcrwd(mgs) =   &
     &   il5(mgs)*( - crfrz(mgs) ) & ! - cipacr(mgs))
     &  - chacr(mgs)    &
     &  - chlacr(mgs)    &
     &  +crcev(mgs)   &
     &  - cracr(mgs)
      ELSE
      pcrwi(mgs) =   &
     &   crcnw(mgs)
      pcrwd(mgs) =   &
     &  +crcev(mgs)   &
     &  - cracr(mgs)

!        tmp3d(igs(mgs),jy,kgs(mgs)) = vtxbar(mgs,lr,1) ! crcnw(mgs) ! (pcrwi(mgs) + pcrwd(mgs))
!        pcrwi(mgs) = 0.0
!        pcrwd(mgs) = 0.0
!        qrcnw(mgs) = 0.0

      ENDIF


      frac = 0.0
      IF ( -pcrwd(mgs)*dtp .gt. cx(mgs,lr) ) THEN
!       write(0,*) 'OUCH! pcrwd(mgs)*dtp .gt. crw(mgs) ',pcrwd(mgs)*dtp,cx(mgs,lr),mgs,igs(mgs),kgs(mgs)
!       write(0,*) -ciacr(mgs)
!       write(0,*) -crfrz(mgs)
!       write(0,*) -chacr(mgs)
!       write(0,*)  crcev(mgs)
!       write(0,*)  -cracr(mgs)

       frac =  -cx(mgs,lr)/(pcrwd(mgs)*dtp)
       pcrwd(mgs) = -cx(mgs,lr)/dtp

        ciacr(mgs) = frac*ciacr(mgs)
        ciacrf(mgs) = frac*ciacrf(mgs)
        ciacrs(mgs) = frac*ciacrs(mgs)
        crfrz(mgs) = frac*crfrz(mgs)
        crfrzf(mgs) = frac*crfrzf(mgs)
        crfrzs(mgs) = frac*crfrzs(mgs)
        chacr(mgs) = frac*chacr(mgs)
        crcev(mgs) = frac*crcev(mgs)
        cracr(mgs) = frac*cracr(mgs)

!       STOP
      ENDIF

      end do

      ENDIF


      IF ( warmonly < 0.5 ) THEN

!
!  Snow
!
      IF ( ipconc .ge. 4 ) THEN !

      do mgs = 1,ngscnt
      pcswi(mgs) =   &
     &   il5(mgs)*(cscnis(mgs) + cscnvis(mgs) )    &
     &  + il5(mgs)*cicint(mgs)*ffrzs                &
     &  + ifrzs*crfrzs(mgs) &
     &  + ifrzs*ciacrs(mgs) &
     &  +il5(mgs)*(cwfrzc(mgs)+cwctfzc(mgs)   &
     &  +cicichr(mgs))*ffrzs   &
     &  + cscnh(mgs)
      pcswd(mgs) = &
!     :  cracs(mgs)     &
     &  -chacs(mgs) - chlacs(mgs)   &
     &  -chcns(mgs)   &
     &  +(1-il5(mgs))*csmlr(mgs) + csshr(mgs) & ! + csshrp(mgs)
!     >  +il5(mgs)*(cssbv(mgs))   &
     &   + cssbv(mgs)   &
     &  - csacs(mgs)

      frac = 0.0
      IF ( imixedphase == 0 ) THEN
        IF ( cx(mgs,ls) + dtp*(pcswi(mgs) + pcswd(mgs)) < 0.0 ) THEN
         frac = (-cx(mgs,ls) + pcswi(mgs)*dtp)/(pcswd(mgs)*dtp)
         
           pqswd(mgs) = frac*pqswd(mgs)
           
           chacs(mgs)  = frac*chacs(mgs) 
           chlacs(mgs) = frac*chlacs(mgs)
           chcns(mgs)  = frac*chcns(mgs) 
           csmlr(mgs)  = frac*csmlr(mgs) 
           csshr(mgs)  = frac*csshr(mgs) 
           cssbv(mgs)  = frac*cssbv(mgs) 
           csacs(mgs)  = frac*csacs(mgs)
      
        ENDIF
      ENDIF


      pccii(mgs) =  pccii(mgs) &
     &  + (1. - ifrzs)*crfrzs(mgs) &
     &  + (1. - ifrzs)*ciacrs(mgs)

      end do

      ENDIF

!
!  Graupel
!
      IF ( ipconc .ge. 5 ) THEN !
      do mgs = 1,ngscnt
      pchwi(mgs) =   &
     &  +ifrzg*(crfrzf(mgs)   &
     & +il5(mgs)*(ciacrf(mgs) ))    &
     & + chcnsh(mgs) + chcnih(mgs) + chcnhl(mgs)

      pchwd(mgs) =   &
     &  (1-il5(mgs))*chmlr(mgs) &
!     >  + il5(mgs)*chsbv(mgs)   &
     &  + chsbv(mgs)   &
     &  - il5(mgs)*chlcnh(mgs) &
     &  - cscnh(mgs)
      end do
!

!
!  Hail
!
      IF ( lhl .gt. 1 ) THEN !
      do mgs = 1,ngscnt
      pchli(mgs) = (1.0-ifrzg)*(crfrzf(mgs) +il5(mgs)*(ciacrf(mgs) ))  &
     & + chlcnh(mgs) *rzxhlh(mgs)

      pchld(mgs) =   &
     &  (1-il5(mgs))*chlmlr(mgs)   &
!     >  + il5(mgs)*chlsbv(mgs)   &
     &  + chlsbv(mgs) - chcnhl(mgs)
      
!      IF ( pchli(mgs) .ne. 0. .or. pchld(mgs) .ne. 0 ) THEN
!       write(0,*) 'dr: pchli,pchld = ', pchli(mgs),pchld(mgs), igs(mgs),kgs(mgs)
!      ENDIF
      end do
      
      ENDIF
!

      ENDIF ! (ipconc .ge. 5 )

      ELSEIF ( warmonly < 0.8 ) THEN

!
!  Graupel
!
      IF ( ipconc .ge. 5 ) THEN !
      do mgs = 1,ngscnt
      pchwi(mgs) =   &
     &  +ifrzg*(crfrzf(mgs) )

      pchwd(mgs) =   &
     &  (1-il5(mgs))*chmlr(mgs) &
     &  - il5(mgs)*chlcnh(mgs)
      end do
!
!  Hail
!
      IF ( lhl .gt. 1 ) THEN !
      do mgs = 1,ngscnt
      pchli(mgs) = (1.0-ifrzg)*(crfrzf(mgs) +il5(mgs)*(ciacrf(mgs) ))  &
     & + chlcnh(mgs) *rzxhl(mgs)/rzxh(mgs)

      pchld(mgs) =   &
     &  (1-il5(mgs))*chlmlr(mgs) !  &
!     >  + il5(mgs)*chlsbv(mgs)   &
!     &  + chlsbv(mgs)

!      IF ( pchli(mgs) .ne. 0. .or. pchld(mgs) .ne. 0 ) THEN
!       write(0,*) 'dr: pchli,pchld = ', pchli(mgs),pchld(mgs), igs(mgs),kgs(mgs)
!      ENDIF
      end do

      ENDIF

      ENDIF ! ipconc >= 5

      ENDIF ! warmonly

!

!
!  Balance and checks for continuity.....within machine precision...
!
      do mgs = 1,ngscnt
      pctot(mgs)   = pccwi(mgs) +pccwd(mgs) +   &
     &               pccii(mgs) +pccid(mgs) +   &
     &               pcrwi(mgs) +pcrwd(mgs) +   &
     &               pcswi(mgs) +pcswd(mgs) +   &
     &               pchwi(mgs) +pchwd(mgs) +   &
     &               pchli(mgs) +pchld(mgs)
      end do
!
!
      ENDIF ! ( ipconc .ge. 1 )
!
!
!
!
!
!  GOGO
!  production terms for mass
!
!
       pqwvi(:) = 0.0
       pqwvd(:) = 0.0
       pqcwi(:) = 0.0
       pqcwd(:) = 0.0
       pqcii(:) = 0.0
       pqcid(:) = 0.0
       pqrwi(:) = 0.0
       pqrwd(:) = 0.0
       pqswi(:) = 0.0
       pqswd(:) = 0.0
       pqhwi(:) = 0.0
       pqhwd(:) = 0.0
       pqhli(:) = 0.0
       pqhld(:) = 0.0
       pqlwsi(:) = 0.0
       pqlwsd(:) = 0.0
       pqlwhi(:) = 0.0
       pqlwhd(:) = 0.0
       pqlwhli(:) = 0.0
       pqlwhld(:) = 0.0
!
!  Vapor
!
      IF ( warmonly < 0.5 ) THEN
      do mgs = 1,ngscnt
      
! NOTE: ANY CHANGES HERE ALSO NEED TO GO INTO THE RESUM FARTHER DOWN!
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))   &
     &  -Min(0.0, qhcev(mgs))   &
     &  -Min(0.0, qhlcev(mgs))   &
     &  -Min(0.0, qscev(mgs))   &
!     >  +il5(mgs)*(-qhsbv(mgs) - qhlsbv(mgs) )   &
     &  -qhsbv(mgs) - qhlsbv(mgs)   &
     &  -qssbv(mgs)    &
     &  -il5(mgs)*qisbv(mgs)
      
      pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))   &
     &  -Max(0.0, qhcev(mgs))   &
     &  -Max(0.0, qhlcev(mgs))   &
     &  -Max(0.0, qscev(mgs))   &
     &  +il5(mgs)*(-qiint(mgs)   &
     &  -qhdpv(mgs) -qsdpv(mgs) - qhldpv(mgs))   &
     &  -il5(mgs)*qidpv(mgs)  
      
      end do

      ELSEIF ( warmonly < 0.8 ) THEN
      do mgs = 1,ngscnt
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs)) &
     &  -il5(mgs)*qisbv(mgs)
      pqwvd(mgs) =     &
     &  +il5(mgs)*(-qiint(mgs)   &
!     &  -qhdpv(mgs) ) & !- qhldpv(mgs))   &
     &  -qhdpv(mgs) - qhldpv(mgs))   &
!     &  -qhdpv(mgs) -qsdpv(mgs) - qhldpv(mgs))   &
     &  -Max(0.0, qrcev(mgs))     &
     &  -il5(mgs)*qidpv(mgs)  
      end do

      ELSE
      do mgs = 1,ngscnt
      pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))
      pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))
      end do

      ENDIF ! warmonly
!
!  Cloud water
!
      do mgs = 1,ngscnt

      pqcwi(mgs) =  (0.0) + qwcnr(mgs)

      IF ( warmonly < 0.5 ) THEN
      pqcwd(mgs) =    &
     &  il5(mgs)*(-qiacw(mgs)-qwfrz(mgs)-qwctfz(mgs))   &
     &  -il5(mgs)*(qicichr(mgs))   &
     &  -qracw(mgs) -qsacw(mgs) -qrcnw(mgs) -qhacw(mgs) - qhlacw(mgs)  !&
!     &  -il5(mgs)*(qwfrzp(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pqcwd(mgs) =    &
     &  il5(mgs)*(-qiacw(mgs)-qwfrz(mgs)-qwctfz(mgs))   &
     &  -il5(mgs)*(qicichr(mgs))   &
     &  -qracw(mgs) -qrcnw(mgs) -qhacw(mgs) -qhlacw(mgs)
      ELSE
      pqcwd(mgs) =    &
     &  -qracw(mgs) - qrcnw(mgs)
      ENDIF

      IF ( pqcwd(mgs) .lt. 0.0 .and. -pqcwd(mgs)*dtp .gt. qx(mgs,lc) ) THEN

       frac = -Max(0.0,qx(mgs,lc))/(pqcwd(mgs)*dtp)
       pqcwd(mgs) = -qx(mgs,lc)/dtp

        qiacw(mgs)   = frac*qiacw(mgs)
!        qwfrzp(mgs)  = frac*qwfrzp(mgs)
!        qwctfzp(mgs) = frac*qwctfzp(mgs)
        qwfrzc(mgs)  = frac*qwfrzc(mgs)
        qwfrzis(mgs)  = frac*qwfrzis(mgs)
        qwfrz(mgs)  = frac*qwfrz(mgs)
        qwctfzc(mgs) = frac*qwctfzc(mgs)
        qwctfzis(mgs) = frac*qwctfzis(mgs)
        qwctfz(mgs) = frac*qwctfz(mgs)
        qracw(mgs)   = frac*qracw(mgs)
        qsacw(mgs)   = frac*qsacw(mgs)
        qhacw(mgs)   = frac*qhacw(mgs)
        vhacw(mgs)   = frac*vhacw(mgs)
        qrcnw(mgs)   = frac*qrcnw(mgs)
        qwfrzp(mgs)  = frac*qwfrzp(mgs)
        IF ( lhl .gt. 1 ) THEN
          qhlacw(mgs)   = frac*qhlacw(mgs)
          vhlacw(mgs)   = frac*vhlacw(mgs)
        ENDIF
!        IF ( lzh .gt. 1 ) zhacw(mgs) = frac*zhacw(mgs)

!       STOP
      ENDIF
      

      end do
!
!  Cloud ice
!
      IF ( warmonly < 0.5 ) THEN

      do mgs = 1,ngscnt
      pqcii(mgs) =     &
     &   il5(mgs)*qicicnt(mgs)*(1. - ffrzs)    &
     &  +il5(mgs)*qidpv(mgs)    &
     &  +il5(mgs)*qiacw(mgs)   & 
     &  +il5(mgs)*(qwfrzc(mgs)+qwctfzc(mgs))*(1. - ffrzs)   &
     &  +il5(mgs)*(qicichr(mgs))*(1. - ffrzs)   &
     &  +qsmul(mgs)               &
     &  +qhmul1(mgs) + qhlmul1(mgs)   &
     & + qsplinter(mgs) + qsplinter2(mgs)
!     > + cimas0*nsplinter*(crfrzf(mgs) + crfrz(mgs))/rho0(mgs)

      pqcid(mgs) =     &
     &   il5(mgs)*(-qscni(mgs) - qscnvi(mgs)    & ! -qwaci(mgs)    &
     &  -qraci(mgs)    &
     &  -qsaci(mgs) )   &
     &  -qhaci(mgs)   &
     &  -qhlaci(mgs)    &
     &  +il5(mgs)*qisbv(mgs)    &
     &  +(1.-il5(mgs))*qimlr(mgs)   &
     &  - qhcni(mgs)
      end do

      
      ELSEIF ( warmonly < 0.8 ) THEN

      do mgs = 1,ngscnt
      pqcii(mgs) =     &
     &   il5(mgs)*qicicnt(mgs)*(1. - ffrzs)    &
     &  +il5(mgs)*(qwfrzc(mgs)+qwctfzc(mgs))*(1. - ffrzs)   &
     &  +il5(mgs)*(qicichr(mgs))*(1. - ffrzs)   &
!     &  +il5(mgs)*(qicichr(mgs))   &
!     &  +qsmul(mgs)               &
     &  +qhmul1(mgs) + qhlmul1(mgs)   &
     & + qsplinter(mgs) + qsplinter2(mgs) &
     &  +il5(mgs)*qidpv(mgs)    &
     &  +il5(mgs)*qiacw(mgs)  ! & ! (qiacwi(mgs)+qwacii(mgs))   &
!     &  +il5(mgs)*(qwfrzc(mgs)+qwctfzc(mgs))   &
!     &  +il5(mgs)*(qicichr(mgs))   &
!     &  +qsmul(mgs)               &
!     &  +qhmul1(mgs) + qhlmul1(mgs)   &
!     & + qsplinter(mgs) + qsplinter2(mgs)

      pqcid(mgs) =     &
!     &   il5(mgs)*(-qscni(mgs) - qscnvi(mgs)    & ! -qwaci(mgs)    &
!     &  -qraci(mgs)    &
!     &  -qsaci(mgs) )   &
!     &  -qhaci(mgs)   &
!     &  -qhlaci(mgs)    &
     &  +il5(mgs)*qisbv(mgs)    &
     &  +(1.-il5(mgs))*qimlr(mgs)  ! &
!     &  - qhcni(mgs)
      end do

      ENDIF
!
!  Rain
!

      do mgs = 1,ngscnt
      IF ( warmonly < 0.5 ) THEN
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))   &
     &  +(1-il5(mgs))*(   &
     &    -qhmlr(mgs)                 &            !null at this point when wet snow/graupel included
     &    -qsmlr(mgs)  - qhlmlr(mgs)     &
     &    -qimlr(mgs))   &
     &    -qsshr(mgs)       &                      !null at this point when wet snow/graupel included
     &    -qhshr(mgs)       &                      !null at this point when wet snow/graupel included
     &    -qhlshr(mgs)
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qiacr(mgs)-qrfrz(mgs))    &
     &  - qsacr(mgs) - qhacr(mgs) - qhlacr(mgs) - qwcnr(mgs)   &
     &  + Min(0.0,qrcev(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))   &
     &  +(1-il5(mgs))*(   &
     &    -qhmlr(mgs)                 &            !null at this point when wet snow/graupel included
     &    -qhshr(mgs)                 &           !null at this point when wet snow/graupel included
     &    -qhlmlr(mgs)                 &            !null at this point when wet snow/graupel included
     &    -qhlshr(mgs) )                           !null at this point when wet snow/graupel included
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qrfrz(mgs))    &
     &   - qhacr(mgs)    &
     &   - qhlacr(mgs)    &
     &  + Min(0.0,qrcev(mgs))
      ELSE
      pqrwi(mgs) =     &
     &   qracw(mgs) + qrcnw(mgs) + Max(0.0, qrcev(mgs))
      pqrwd(mgs) =  Min(0.0,qrcev(mgs))
      ENDIF ! warmonly


 !      IF ( pqrwd(mgs) .lt. 0.0 .and. -(pqrwd(mgs) + pqrwi(mgs))*dtp .gt. qx(mgs,lr)  ) THEN
      IF ( pqrwd(mgs) .lt. 0.0 .and. -(pqrwd(mgs) + pqrwi(mgs))*dtp .gt. qx(mgs,lr)  ) THEN

       frac = (-qx(mgs,lr) + pqrwi(mgs)*dtp)/(pqrwd(mgs)*dtp)
!       pqrwd(mgs) = -qx(mgs,lr)/dtp  + pqrwi(mgs)

       pqwvi(mgs) = pqwvi(mgs)    &
     &  + Min(0.0, qrcev(mgs))   &
     &  - frac*Min(0.0, qrcev(mgs))
       pqwvd(mgs) =  pqwvd(mgs)   &
     &  + Max(0.0, qrcev(mgs))   &
     &  - frac*Max(0.0, qrcev(mgs))

       qiacr(mgs)  = frac*qiacr(mgs)
       qiacrf(mgs) = frac*qiacrf(mgs)
       qiacrs(mgs) = frac*qiacrs(mgs)
       viacrf(mgs) = frac*viacrf(mgs)
       qrfrz(mgs)  = frac*qrfrz(mgs) 
       qrfrzs(mgs) = frac*qrfrzs(mgs) 
       qrfrzf(mgs) = frac*qrfrzf(mgs)
       vrfrzf(mgs) = frac*vrfrzf(mgs)
       qsacr(mgs)  = frac*qsacr(mgs)
       qhacr(mgs)  = frac*qhacr(mgs)
       vhacr(mgs)  = frac*vhacr(mgs)
       qrcev(mgs)  = frac*qrcev(mgs)
       qhlacr(mgs) = frac*qhlacr(mgs)
       vhlacr(mgs) = frac*vhlacr(mgs)
!       qhcev(mgs)  = frac*qhcev(mgs)


      IF ( warmonly < 0.5 ) THEN
       pqrwd(mgs) =     &
     &  il5(mgs)*(-qiacr(mgs)-qrfrz(mgs) - qsacr(mgs))    &
     &  - qhacr(mgs) - qhlacr(mgs) - qwcnr(mgs)   &
     &  + Min(0.0,qrcev(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pqrwd(mgs) =     &
     &  il5(mgs)*(-qrfrz(mgs))    &
     &   - qhacr(mgs)    &
     &   - qhlacr(mgs)    &
     &  + Min(0.0,qrcev(mgs))
      ELSE
       pqrwd(mgs) =  Min(0.0,qrcev(mgs))
      ENDIF ! warmonly

!
! Resum for vapor since qrcev has changed
!
      IF ( qrcev(mgs) .ne. 0.0 ) THEN
       pqwvi(mgs) =    &
     &  -Min(0.0, qrcev(mgs))   &
     &  -Min(0.0, qhcev(mgs))   &
     &  -Min(0.0, qhlcev(mgs))   &
     &  -Min(0.0, qscev(mgs))   &
!     >  +il5(mgs)*(-qhsbv(mgs)  - qhlsbv(mgs) )   &
     &  -qhsbv(mgs)  - qhlsbv(mgs)   &
     &  -qssbv(mgs)    &
     &  -il5(mgs)*qisbv(mgs) 
     
       pqwvd(mgs) =     &
     &  -Max(0.0, qrcev(mgs))   &
     &  -Max(0.0, qhcev(mgs))   &
     &  -Max(0.0, qhlcev(mgs))   &
     &  -Max(0.0, qscev(mgs))   &
     &  +il5(mgs)*(-qiint(mgs)   &
     &  -qhdpv(mgs) -qsdpv(mgs) - qhldpv(mgs))   &
     &  -il5(mgs)*qidpv(mgs)  

       ENDIF


!       STOP
      ENDIF
      end do

      IF ( warmonly < 0.5 ) THEN

!
!  Snow
!
      do mgs = 1,ngscnt
      pqswi(mgs) =     &
     &   il5(mgs)*(qscni(mgs)+qsaci(mgs)+qsdpv(mgs)   &
     &   + qscnvi(mgs)                        &
     &   + ifrzs*(qiacrs(mgs) + qrfrzs(mgs))  &
     &   + il5(mgs)*( qwfrzc(mgs) + qwctfzc(mgs) + qicichr(mgs) )*ffrzs   &
     &   + il2(mgs)*qsacr(mgs))   &
     &   + il5(mgs)*qicicnt(mgs)*ffrzs        &
     &   + il3(mgs)*(qiacrf(mgs)+qracif(mgs)) &
     &   + Max(0.0, qscev(mgs))   &
     &   + qsacw(mgs) + qscnh(mgs)
      pqswd(mgs) =    &
!     >  -qfacs(mgs) ! -qwacs(mgs)   &
     &  -qracs(mgs)*(1-il2(mgs)) -qhacs(mgs) - qhlacs(mgs)   &
     &  -qhcns(mgs)   &
     &  +(1-il5(mgs))*qsmlr(mgs) + qsshr(mgs)    &    !null at this point when wet snow included
!     >  +il5(mgs)*(qssbv(mgs))   &
     &  + (qssbv(mgs))   &
     &  + Min(0.0, qscev(mgs))  &
     &  -qsmul(mgs)
      
      
      IF ( imixedphase == 0 .and. pqswd(mgs) .lt. 0.0  ) THEN
        IF ( qx(mgs,ls) + dtp*(pqswi(mgs) + pqswd(mgs)) < 0.0 ) THEN
         frac = (-qx(mgs,ls) + pqswi(mgs)*dtp)/(pqswd(mgs)*dtp)
         
           pqswd(mgs) = frac*pqswd(mgs)
           
           qracs(mgs)  = frac*qracs(mgs) ! only used for single moment at this time
           qhacs(mgs)  = frac*qhacs(mgs) 
           qhlacs(mgs) = frac*qhlacs(mgs)
           qhcns(mgs)  = frac*qhcns(mgs) 
           qsmlr(mgs)  = frac*qsmlr(mgs) 
           qsshr(mgs)  = frac*qsshr(mgs) 
           qssbv(mgs)  = frac*qssbv(mgs) 
           qsmul(mgs)  = frac*qsmul(mgs) 
           IF ( qscev(mgs) < 0.0 ) qscev(mgs) = frac*qscev(mgs)

        ENDIF
      ENDIF
      
      pqcii(mgs) =  pqcii(mgs) &
     &  + (1. - ifrzs)*qrfrzs(mgs) &
     &  + (1. - ifrzs)*qiacrs(mgs)
      
      end do 
      
!
!  Graupel
!
      do mgs = 1,ngscnt
      pqhwi(mgs) =    &
     &  +il5(mgs)*ifrzg*(qrfrzf(mgs)  + (1-il3(mgs))*(qiacrf(mgs)+qracif(mgs)))   &
     &  + (1-il2(mgs))*(qracs(mgs) + qsacr(mgs))  &
     &  +il5(mgs)*(qhdpv(mgs))   &
     &  +Max(0.0, qhcev(mgs))   &
     &  +qhacr(mgs)+qhacw(mgs)   &
     &  +qhacs(mgs)+qhaci(mgs)   &
     &  + qhcns(mgs) + qhcni(mgs) + qhcnhl(mgs)
      pqhwd(mgs) =     &
     &   qhshr(mgs)                &    !null at this point when wet graupel included
     &  +(1-il5(mgs))*qhmlr(mgs)   &    !null at this point when wet graupel included
!     >  +il5(mgs)*qhsbv(mgs)   &
     &  + qhsbv(mgs)   &
     &  + Min(0.0, qhcev(mgs))   &
     &  -qhmul1(mgs) - qhlcnh(mgs) - qscnh(mgs)  &
     &  - qsplinter(mgs) - qsplinter2(mgs)
!     > - cimas0*nsplinter*(crfrzf(mgs) + crfrz(mgs))/rho0(mgs)
      end do

!
!  Hail
!
      IF ( lhl .gt. 1 ) THEN

      do mgs = 1,ngscnt
      pqhli(mgs) =    &
     &  +il5(mgs)*(qhldpv(mgs) + (1.0-ifrzg)*(qiacrf(mgs)+qrfrzf(mgs)  + qracif(mgs)))   &
     &  +Max(0.0, qhlcev(mgs))   &
     &  +qhlacr(mgs)+qhlacw(mgs)   &
     &  +qhlacs(mgs)+qhlaci(mgs)   &
     &  + qhlcnh(mgs)
      pqhld(mgs) =     &
     &   qhlshr(mgs)    &
     &  +(1-il5(mgs))*qhlmlr(mgs)    &
!     >  +il5(mgs)*qhlsbv(mgs)   &
     &  + qhlsbv(mgs)   &
     &  + Min(0.0, qhlcev(mgs))   &
     &  -qhlmul1(mgs) - qhcnhl(mgs)
      end do
      
      ENDIF ! lhl

      ELSEIF ( warmonly < 0.8 ) THEN
!
!  Graupel
!
      do mgs = 1,ngscnt
      pqhwi(mgs) =    &
     &  +il5(mgs)*ifrzg*(qrfrzf(mgs) )   &
     &  +il5(mgs)*(qhdpv(mgs))   &
     &  +qhacr(mgs)+qhacw(mgs)   
      pqhwd(mgs) =     &
     &   qhshr(mgs)                &    !null at this point when wet graupel included
     &  - qhlcnh(mgs)   &
     &  - qhmul1(mgs)   &
     &  - qsplinter(mgs) - qsplinter2(mgs) &
     &  +(1-il5(mgs))*qhmlr(mgs)        !null at this point when wet graupel included
       end do

!
!  Hail
!
      IF ( lhl .gt. 1 ) THEN

      do mgs = 1,ngscnt
      pqhli(mgs) =    &
     &  +il5(mgs)*(qhldpv(mgs) ) & ! + (1.0-ifrzg)*(qiacrf(mgs)+qrfrzf(mgs)  + qracif(mgs)))   &
     &  +qhlacr(mgs)+qhlacw(mgs)   &
!     &  +qhlacs(mgs)+qhlaci(mgs)   &
     &  + qhlcnh(mgs)
      pqhld(mgs) =     &
     &   qhlshr(mgs)    &
     &  +(1-il5(mgs))*qhlmlr(mgs)    &
!     >  +il5(mgs)*qhlsbv(mgs)   &
     &  + qhlsbv(mgs)   &
     &  -qhlmul1(mgs) - qhcnhl(mgs)

      end do

      ENDIF ! lhl

      ENDIF ! warmonly

!
!  Liquid water on snow and graupel
!

      vhmlr(:) = 0.0
      vhlmlr(:) = 0.0
      vhfzh(:) = 0.0
      vhlfzhl(:) = 0.0

      IF ( mixedphase ) THEN
      ELSE ! set arrays for non-mixedphase graupel
      
!        vhshdr(:) = 0.0
        vhmlr(:) = qhmlr(:) ! not actually volume, but treated as q in rate equation
!        vhsoak(:) = 0.0

!        vhlshdr(:) = 0.0
        vhlmlr(:) = qhlmlr(:) ! not actually volume, but treated as q in rate equation
!        vhlmlr(:) = rho0(:)*qhlmlr(:)/xdn(:,lhl) 
!        vhlsoak(:) = 0.0
      
      ENDIF  ! mixedphase



!
!  Snow volume
!
      IF ( lvol(ls) .gt. 1 ) THEN
      do mgs = 1,ngscnt
!      pvswi(mgs) = rho0(mgs)*( pqswi(mgs) )/xdn0(ls)

      pvswi(mgs) = rho0(mgs)*(    &
!aps     >   il5*qsfzs(mgs)/xdn(mgs,ls)   &
!aps     >  -il5*qsfzs(mgs)/xdn(mgs,lr)   &
     &  +il5(mgs)*(qscni(mgs)+qsaci(mgs)+qsdpv(mgs)   &
     &   + qscnvi(mgs) + (1. - ifrzs)*qiacrs(mgs) &
     &   + (1. - ifrzs)*qrfrzs(mgs)  &
     &  )/xdn0(ls)   &
     &    + (qsacr(mgs))/rimdn(mgs,ls) ) + vsacw(mgs)
!     >   + (qsacw(mgs) + qsacr(mgs))/rimdn(mgs,ls) )
      pvswd(mgs) = rho0(mgs)*( pqswd(mgs) )/xdn0(ls)  &
!     >  -qhacs(mgs)
!     >  -qhcns(mgs)
!     >  +(1-il5(mgs))*qsmlr(mgs) + qsshr(mgs)
!     >  +il5(mgs)*(qssbv(mgs))
     &   -rho0(mgs)*qsmul(mgs)/xdn0(ls)
!aps     >   +rho0(mgs)*(1-il5(mgs))*(
!aps     >             qsmlr(mgs)/xdn(mgs,ls)
!aps     >    +(qscev-qsmlr(mgs))/xdn(mgs,lr) )
      end do

!aps      IF (mixedphase) THEN
!aps        pvswd(mgs) = pvswd(mgs)
!aps     >   + rho0(mgs)*qsshr(mgs)/xdn(mgs,lr)
!aps      ENDIF

      ENDIF
!
!  Graupel volume
!
      IF ( lvol(lh) .gt. 1 ) THEN
      DO mgs = 1,ngscnt
!      pvhwi(mgs) = rho0(mgs)*( (pqhwi(mgs) )/xdn0(lh) )

!      pvhwi(mgs) = rho0(mgs)*( (pqhwi(mgs) - il5(mgs)*qrfrzf(mgs) )/xdn0(lh) !
!     :  +  il5(mgs)*qrfrzf(mgs)/rhofrz )

      pvhwi(mgs) = rho0(mgs)*(   &
     &  +il5(mgs)*( qracif(mgs))/rhofrz   &
!erm     >  + il5(mgs)*qhfzh(mgs)/rhofrz !aps: or use xdnmx(lh)?   &
     &  + (  il5(mgs)*qhdpv(mgs)   &
     &     + qhacs(mgs) + qhaci(mgs) )/xdnmn(lh) )   &
     &  +   rho0(mgs)*Max(0.0, qhcev(mgs))/1000.   & ! only used in mixed phase: evaporation of liquid water coating
!     >     + qhacs(mgs) + qhaci(mgs) )/xdn0(ls) )   &
     &  + vhcns(mgs)   &
     &  + vhacr(mgs) + vhacw(mgs)  + vhfzh(mgs)   & ! qhacw(mgs)/rimdn(mgs,lh)
!     >  + vhfrh(mgs)   &
     &  + vhcni(mgs) + ifrzg*(viacrf(mgs) + vrfrzf(mgs))
!     >  +qhacr(mgs)/raindn(mgs,lh) + qhacw(mgs)/rimdn(mgs,lh)
      
!      pvhwd(mgs) = rho0(mgs)*(pqhwd(mgs) )/xdn0(lh)

      pvhwd(mgs) = rho0(mgs)*(   &
!     >   qhshr(mgs)/xdn0(lr)   &
!     >  - il5(mgs)*qhfzh(mgs)/xdn(mgs,lr)   &
     &  +( (1-il5(mgs))*vhmlr(mgs)    &
!     >     +il5(mgs)*qhsbv(mgs)   &
     &     + qhsbv(mgs)   &
     &     + Min(0.0, qhcev(mgs))   &
     &     -qhmul1(mgs) )/xdn(mgs,lh) )   &
     &  - vhlcnh(mgs) + vhshdr(mgs) - vhsoak(mgs) - vscnh(mgs)

!      IF (mixedphase) THEN
!       pvhwd(mgs) = pvhwd(mgs) 
!     >  + rho0(mgs)*qhshr(mgs)/xdn(mgs,lh) !xdn(mgs,lr)
!      ENDIF

      IF ( .false. .and. ny .eq. 2 .and. kgs(mgs) .eq. 9 .and. igs(mgs) .eq. 19 ) THEN

      write(iunit,*)
      write(iunit,*)   'Graupel at ',igs(mgs),kgs(mgs)
!
      write(iunit,*)   il5(mgs)*qrfrzf(mgs), qrfrzf(mgs) - qrfrz(mgs)
      write(iunit,*)   il5(mgs)*qiacrf(mgs)
      write(iunit,*)   il5(mgs)*qracif(mgs)
      write(iunit,*)   'qhcns',qhcns(mgs)
      write(iunit,*)   'qhcni',qhcni(mgs)
      write(iunit,*)   il5(mgs)*(qhdpv(mgs))
      write(iunit,*)   'qhacr ',qhacr(mgs)
      write(iunit,*)   'qhacw', qhacw(mgs)
      write(iunit,*)   'qhacs', qhacs(mgs)
      write(iunit,*)   'qhaci', qhaci(mgs)
      write(iunit,*)   'pqhwi = ',pqhwi(mgs)
      write(iunit,*)
      write(iunit,*) 'qhcev',qhcev(mgs)
      write(iunit,*)
      write(iunit,*)   'qhshr',qhshr(mgs)
      write(iunit,*)  'qhmlr', (1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)   'qhsbv', qhsbv(mgs)
      write(iunit,*)   'qhlcnh',-qhlcnh(mgs)
      write(iunit,*)   'qhmul1',-qhmul1(mgs)
      write(iunit,*)   'pqhwd = ', pqhwd(mgs)
      write(iunit,*)
      write(iunit,*)  'Volume'
      write(iunit,*)
      write(iunit,*)  'pvhwi',pvhwi(mgs)
      write(iunit,*)   'vhcns', vhcns(mgs)
      write(iunit,*)  'vhacr,vhacw',vhacr(mgs), vhacw(mgs) ! qhacw(mgs)/rimdn(mgs,lh)
      write(iunit,*)  'vhcni',vhcni(mgs)
      write(iunit,*)
      write(iunit,*)  'pvhwd',pvhwd(mgs)
      write(iunit,*)  'vhlcnh,vhshdr,vhsoak ', vhlcnh(mgs),  vhshdr(mgs), vhsoak(mgs)
      write(iunit,*)  'vhmlr', vhmlr(mgs)
      write(iunit,*)
!      write(iunit,*)
!      write(iunit,*)
!      write(iunit,*)
      write(iunit,*)  'Concentration'
      write(iunit,*)   pchwi(mgs),pchwd(mgs)
      write(iunit,*)  crfrzf(mgs)
      write(iunit,*)  chcns(mgs)
      write(iunit,*)  ciacrf(mgs)


      ENDIF


      ENDDO

      ENDIF
!
!
!

!
!  Hail volume
!
      IF ( lhl .gt. 1 ) THEN
      IF ( lvol(lhl) .gt. 1 ) THEN
      DO mgs = 1,ngscnt

      pvhli(mgs) = rho0(mgs)*(   &
     &  + (  il5(mgs)*qhldpv(mgs)   &
!     &  +    Max(0.0, qhlcev(mgs))   &
!     &     + qhlacs(mgs) + qhlaci(mgs) )/xdnmn(lhl) )   & ! xdn0(ls) )   &
     &     + qhlacs(mgs) + qhlaci(mgs) )/xdnmn(lh) )   &  ! yes, this is 'lh' on purpose
     &  +   rho0(mgs)*Max(0.0, qhlcev(mgs))/1000.   &
     &  + vhlcnhl(mgs) + (1.0-ifrzg)*(viacrf(mgs) + vrfrzf(mgs))  & 
     &  + vhlacr(mgs) + vhlacw(mgs) + vhlfzhl(mgs) ! qhlacw(mgs)/rimdn(mgs,lhl)
      
      pvhld(mgs) = rho0(mgs)*(   &
     &  +(  qhlsbv(mgs)   &
     &     + Min(0.0, qhlcev(mgs))   &
     &     -qhlmul1(mgs) )/xdn(mgs,lhl) ) &
!     &   + vhlmlr(mgs)                    &
     &   + rho0(mgs)*(1-il5(mgs))*vhlmlr(mgs)/xdn(mgs,lhl)  &
     &   + vhlshdr(mgs) - vhlsoak(mgs)


      ENDDO
      
      ENDIF
      ENDIF


      if ( ndebug .ge. 1 ) then
      do mgs = 1,ngscnt
!
      ptotal(mgs) = 0.
      ptotal(mgs) = ptotal(mgs)     &
     &  + pqwvi(mgs) + pqwvd(mgs)   &
     &  + pqcwi(mgs) + pqcwd(mgs)   &
     &  + pqcii(mgs) + pqcid(mgs)   &
     &  + pqrwi(mgs) + pqrwd(mgs)   &
     &  + pqswi(mgs) + pqswd(mgs)   &
     &  + pqhwi(mgs) + pqhwd(mgs)   &
     &  + pqhli(mgs) + pqhld(mgs)
!
      
      
      ENDDO
      
      do mgs = 1,ngscnt

      if ( ( (ndebug .ge. 0  ) .and. abs(ptotal(mgs)) .gt. eqtot )   &
!      if ( (  abs(ptotal(mgs)) .gt. eqtot )
!     :    .or. pqswi(mgs)*dtp .gt. 1.e-3
!     :    .or. pqhwi(mgs)*dtp .gt. 1.e-3
!     :     .or. dtp*(pqrwi(mgs)+pqrwd(mgs)) .gt. 10.0e-3
!     :     .or. dtp*(pccii(mgs)+pccid(mgs)) .gt. 1.e7
!     :     .or. dtp*(pcipi(mgs)+pcipd(mgs)) .gt. 1.e7    &
     &  .or.  .not. (ptotal(mgs) .lt. 1.0 .and.  ptotal(mgs) .gt. -1.0)   & ! this line is basically checking for NaNs
     &              ) then
      write(iunit,*) 'YIKES! ','ptotal1',mgs,igs(mgs),jgs,   &
     &       kgs(mgs),ptotal(mgs)

      write(iunit,*) 't7: ', t7(igs(mgs),jgs,kgs(mgs))
      write(iunit,*)  'cci,ccw,crw,rdia: ',cx(mgs,li),cx(mgs,lc),cx(mgs,lr),0.5*xdia(mgs,lr,1)
      write(iunit,*)  'qc,qi,qr : ',qx(mgs,lc),qx(mgs,li),qx(mgs,lr)
      write(iunit,*)  'rmas, qrcalc : ',xmas(mgs,lr),xmas(mgs,lr)*cx(mgs,lr)/rho0(mgs)
      write(iunit,*)  'vti,vtc,eiw,vtr: ',vtxbar(mgs,li,1),vtxbar(mgs,lc,1),eiw(mgs),vtxbar(mgs,lr,1)
      write(iunit,*)  'cidia,cwdia,qcmxd: ', xdia(mgs,li,1),xdia(mgs,lc,1),qcmxd(mgs)
      write(iunit,*)  'snow: ',qx(mgs,ls),cx(mgs,ls),swvent(mgs),vtxbar(mgs,ls,1),xdia(mgs,ls,1)
      write(iunit,*)  'graupel: ',qx(mgs,lh),cx(mgs,lh),hwvent(mgs),vtxbar(mgs,lh,1),xdia(mgs,lh,1)
      IF ( lhl .gt. 1 ) write(iunit,*)  'hail: ',qx(mgs,lhl),cx(mgs,lhl),hlvent(mgs),vtxbar(mgs,lhl,1),xdia(mgs,lhl,1)


      write(iunit,*)  'li: ',xdia(mgs,li,1),xdia(mgs,li,2),xmas(mgs,li),qx(mgs,li),   &
     &         vtxbar(mgs,li,1)


      write(iunit,*)  'rain cx,xv : ',cx(mgs,lr),xv(mgs,lr)
      write(iunit,*)  'temcg = ', temcg(mgs)

      write(iunit,*) 'v ', pqwvi(mgs) ,pqwvd(mgs)
      write(iunit,*) 'c ', pqcwi(mgs) ,pqcwd(mgs)
      write(iunit,*) 'ci', pqcii(mgs) ,pqcid(mgs)
      write(iunit,*) 'r ', pqrwi(mgs) ,pqrwd(mgs)
      write(iunit,*) 's ', pqswi(mgs) ,pqswd(mgs)
      write(iunit,*) 'h ', pqhwi(mgs) ,pqhwd(mgs)
      write(iunit,*) 'hl', pqhli(mgs) ,pqhld(mgs)
       tmp =  pqwvi(mgs) + pqwvd(mgs)   &
     &  + pqcwi(mgs) + pqcwd(mgs)   &
     &  + pqcii(mgs) + pqcid(mgs)   &
     &  + pqrwi(mgs) + pqrwd(mgs)   &
     &  + pqswi(mgs) + pqswd(mgs)   &
     &  + pqhwi(mgs) + pqhwd(mgs)   &
     &  + pqhli(mgs) + pqhld(mgs)

      write(iunit,*) 'total = ',tmp
      write(iunit,*) 'END OF OUTPUT OF SOURCE AND SINK'

!
!  print production terms
!
      write(iunit,*)
      write(iunit,*)   'Vapor'
!
      write(iunit,*)   -Min(0.0,qrcev(mgs))
      write(iunit,*)   -il5(mgs)*qhsbv(mgs)
      write(iunit,*)   -il5(mgs)*qhlsbv(mgs)
      write(iunit,*)   -il5(mgs)*qssbv(mgs)
      write(iunit,*)   -il5(mgs)*qisbv(mgs)
      write(iunit,*)    'pqwvi= ', pqwvi(mgs)
      write(iunit,*)   -Max(0.0,qrcev(mgs))
      write(iunit,*)   -Max(0.0,qhcev(mgs))
      write(iunit,*)   -Max(0.0,qhlcev(mgs))
      write(iunit,*)   -Max(0.0,qscev(mgs))
      write(iunit,*)   -il5(mgs)*qiint(mgs)
      write(iunit,*)   -il5(mgs)*qhdpv(mgs)
      write(iunit,*)   -il5(mgs)*qhldpv(mgs)
      write(iunit,*)   -il5(mgs)*qsdpv(mgs)
      write(iunit,*)   -il5(mgs)*qidpv(mgs)
      write(iunit,*)    'pqwvd = ', pqwvd(mgs)
!
      write(iunit,*)
      write(iunit,*)   'Cloud ice'
!
      write(iunit,*)   il5(mgs)*qicicnt(mgs)
      write(iunit,*)   il5(mgs)*qidpv(mgs)
      write(iunit,*)   il5(mgs)*qiacw(mgs)
      write(iunit,*)   il5(mgs)*qwfrzc(mgs)
      write(iunit,*)   il5(mgs)*qwctfzc(mgs)
      write(iunit,*)   il5(mgs)*qicichr(mgs)
      write(iunit,*)   qhmul1(mgs)
      write(iunit,*)   qhlmul1(mgs)
      write(iunit,*)   'pqcii = ', pqcii(mgs)
      write(iunit,*)   -il5(mgs)*qscni(mgs)
      write(iunit,*)   -il5(mgs)*qscnvi(mgs)
      write(iunit,*)   -il5(mgs)*qraci(mgs)
      write(iunit,*)   -il5(mgs)*qsaci(mgs)
      write(iunit,*)   -il5(mgs)*qhaci(mgs)
      write(iunit,*)   -il5(mgs)*qhlaci(mgs)
      write(iunit,*)   il5(mgs)*qisbv(mgs)
      write(iunit,*)   (1.-il5(mgs))*qimlr(mgs)
      write(iunit,*)   -il5(mgs)*qhcni(mgs)
      write(iunit,*)   'pqcid = ', pqcid(mgs)
      write(iunit,*)   ' Conc:'
      write(iunit,*)   pccii(mgs),pccid(mgs)
      write(iunit,*)   il5(mgs),cicint(mgs)
      write(iunit,*)   cwacii(mgs),cwfrzc(mgs),cwctfzc(mgs)
      write(iunit,*)   cicichr(mgs)
      write(iunit,*)   chmul1(mgs)
      write(iunit,*)   chlmul1(mgs)
      write(iunit,*)   csmul(mgs)
!
!
!
!
      write(iunit,*)
      write(iunit,*)   'Cloud water'
!
      write(iunit,*)   'pqcwi =', pqcwi(mgs)
      write(iunit,*)   -il5(mgs)*qiacw(mgs)
      write(iunit,*)   -il5(mgs)*qwfrzc(mgs)
      write(iunit,*)   -il5(mgs)*qwctfzc(mgs)
      write(iunit,*)   -il5(mgs)*qwctfzis(mgs)
!      write(iunit,*)   -il5(mgs)*qwfrzp(mgs)
!      write(iunit,*)   -il5(mgs)*qwctfzp(mgs)
      write(iunit,*)   -il5(mgs)*qiihr(mgs)
      write(iunit,*)   -il5(mgs)*qicichr(mgs)
      write(iunit,*)   -il5(mgs)*qipiphr(mgs)
      write(iunit,*)   -qracw(mgs)
      write(iunit,*)   -qsacw(mgs)
      write(iunit,*)   -qrcnw(mgs)
      write(iunit,*)   -qhacw(mgs)
      write(iunit,*)   -qhlacw(mgs)
      write(iunit,*)   'pqcwd = ', pqcwd(mgs)


      write(iunit,*)
      write(iunit,*)  'Concentration:'
      write(iunit,*)   -cautn(mgs)
      write(iunit,*)   -cracw(mgs)
      write(iunit,*)   -csacw(mgs)
      write(iunit,*)   -chacw(mgs)
      write(iunit,*)  -ciacw(mgs)
      write(iunit,*)  -cwfrzp(mgs)
      write(iunit,*)  -cwctfzp(mgs)
      write(iunit,*)  -cwfrzc(mgs)
      write(iunit,*)  -cwctfzc(mgs)
      write(iunit,*)   pccwd(mgs)
!
      write(iunit,*)
      write(iunit,*)      'Rain '
!
      write(iunit,*)      qracw(mgs)
      write(iunit,*)      qrcnw(mgs)
      write(iunit,*)      Max(0.0, qrcev(mgs))
      write(iunit,*)       -(1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qhlmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qsmlr(mgs)
      write(iunit,*)       -(1-il5(mgs))*qimlr(mgs)
      write(iunit,*)       -qrshr(mgs)
      write(iunit,*)       'pqrwi = ', pqrwi(mgs)    
      write(iunit,*)        -qsshr(mgs)     
      write(iunit,*)        -qhshr(mgs)     
      write(iunit,*)        -qhlshr(mgs)
      write(iunit,*)        -il5(mgs)*qiacr(mgs),qiacr(mgs), qiacrf(mgs)
      write(iunit,*)        -il5(mgs)*qrfrz(mgs)
      write(iunit,*)        -qsacr(mgs)
      write(iunit,*)        -qhacr(mgs)
      write(iunit,*)        -qhlacr(mgs)
      write(iunit,*)        qrcev(mgs)
      write(iunit,*)       'pqrwd = ', pqrwd(mgs) 
      write(iunit,*)       'fhw, fhlw = ',fhw(mgs),fhlw(mgs)
      write(iunit,*)        'qrzfac = ', qrzfac(mgs)
!
      
      write(iunit,*)
      write(iunit,*)  'Rain concentration'
      write(iunit,*)  pcrwi(mgs) 
      write(iunit,*)    crcnw(mgs)
      write(iunit,*)    1-il5(mgs)
      write(iunit,*)   -chmlr(mgs),-csmlr(mgs)
      write(iunit,*)     -crshr(mgs)
      write(iunit,*)  pcrwd(mgs) 
      write(iunit,*)    il5(mgs)
      write(iunit,*)   -ciacr(mgs),-crfrz(mgs) 
      write(iunit,*)   -csacr(mgs),-chacr(mgs)
      write(iunit,*)   +crcev(mgs)
      write(iunit,*)   cracr(mgs)
!      write(iunit,*)   -il5(mgs)*ciracr(mgs)


      write(iunit,*)
      write(iunit,*)   'Snow'
!
      write(iunit,*)        il5(mgs)*qscni(mgs), qscnvi(mgs)
      write(iunit,*)        il5(mgs)*qsaci(mgs)
      write(iunit,*)        il5(mgs)*qrfrzs(mgs)
      write(iunit,*)        il5(mgs)*qiacrs(mgs),il3(mgs)*(qiacrf(mgs)+qracif(mgs)),il3(mgs),qiacrf(mgs),qracif(mgs)
      write(iunit,*)        il5(mgs)*qsdpv(mgs), qscev(mgs)
      write(iunit,*)        qsacw(mgs)
      write(iunit,*)        qsacr(mgs), qscnh(mgs)
       write(iunit,*)        'pqswi = ',pqswi(mgs)
      write(iunit,*)        -qhcns(mgs)
      write(iunit,*)        -qracs(mgs)
      write(iunit,*)        -qhacs(mgs)
      write(iunit,*)        -qhlacs(mgs)
      write(iunit,*)       (1-il5(mgs))*qsmlr(mgs)
      write(iunit,*)       qsshr(mgs)
!      write(iunit,*)       qsshrp(mgs)
      write(iunit,*)       il5(mgs)*(qssbv(mgs))
      write(iunit,*)       'pqswd = ', pqswd(mgs)
      write(iunit,*)   -qracs(mgs)*(1-il2(mgs)) , qhacs(mgs) , qhlacs(mgs)   
      write(iunit,*)   -qhcns(mgs)   
      write(iunit,*)   +(1-il5(mgs))*qsmlr(mgs) , qsshr(mgs)        !null at this point when wet snow included
      write(iunit,*)   (qssbv(mgs))   
      write(iunit,*)   Min(0.0, qscev(mgs))  
      write(iunit,*)   -qsmul(mgs)
!
!
      write(iunit,*)
      write(iunit,*)   'Graupel'
!
      write(iunit,*)   il5(mgs)*qrfrzf(mgs), qrfrzf(mgs) - qrfrz(mgs)
      write(iunit,*)   il5(mgs)*qiacrf(mgs)
      write(iunit,*)   il5(mgs)*qracif(mgs)
      write(iunit,*)   qhcns(mgs)
      write(iunit,*)   qhcni(mgs)
      write(iunit,*)   il5(mgs)*(qhdpv(mgs))
      write(iunit,*)   qhacr(mgs)
      write(iunit,*)   qhacw(mgs)
      write(iunit,*)   qhacs(mgs)
      write(iunit,*)   qhaci(mgs)
      write(iunit,*)   'pqhwi = ',pqhwi(mgs)
      write(iunit,*)
      write(iunit,*)   qhshr(mgs)
      write(iunit,*)   (1-il5(mgs))*qhmlr(mgs)
      write(iunit,*)   il5(mgs),qhsbv(mgs)
      write(iunit,*)   -qhlcnh(mgs)
      write(iunit,*)   -qhmul1(mgs)
      write(iunit,*)   'pqhwd = ', pqhwd(mgs)
      write(iunit,*)  'Concentration'
      write(iunit,*)   pchwi(mgs),pchwd(mgs)
      write(iunit,*)  crfrzf(mgs)
      write(iunit,*)  chcns(mgs)
      write(iunit,*)  ciacrf(mgs)

!
      write(iunit,*)
      write(iunit,*)   'Hail'
!
      write(iunit,*)   qhlcnh(mgs)
      write(iunit,*)   il5(mgs)*(qhldpv(mgs))
      write(iunit,*)   qhlacr(mgs)
      write(iunit,*)   qhlacw(mgs)
      write(iunit,*)   qhlacs(mgs)
      write(iunit,*)   qhlaci(mgs)
      write(iunit,*)   pqhli(mgs)
      write(iunit,*)
      write(iunit,*)   qhlshr(mgs)
      write(iunit,*)   (1-il5(mgs))*qhlmlr(mgs)
      write(iunit,*)   il5(mgs)*qhlsbv(mgs)
      write(iunit,*)   pqhld(mgs)
      write(iunit,*)  'Concentration'
      write(iunit,*)   pchli(mgs),pchld(mgs)
      write(iunit,*)  chlcnh(mgs)
!
!  Balance and checks for continuity.....within machine precision...
!
!
      write(iunit,*) 'END OF OUTPUT OF SOURCE AND SINK'
      write(iunit,*) 'PTOTAL',ptotal(mgs)
!
      end if ! ptotal out of bounds or NaN
!
      end do
!

      end if ! ( nstep/12*12 .eq. nstep )

!
!  latent heating from phase changes (except qcw, qci cond, and evap)
!
      do mgs = 1,ngscnt
      IF ( warmonly < 0.5 ) THEN
      pfrz(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qsmlr(mgs)+qhlmlr(mgs))   & !+qhmlh(mgs))   &
     &  +il5(mgs)*(qhfzh(mgs)+qsfzs(mgs)+qhlfzhl(mgs))   &
     &  +il5(mgs)*(1-imixedphase)*(   &
     &   qsacw(mgs)+qhacw(mgs) + qhlacw(mgs)   &
     &  +qsacr(mgs)+qhacr(mgs) + qhlacr(mgs)   &
     &  +qsshr(mgs)   &
     &  +qhshr(mgs)   &
     &  +qhlshr(mgs) +qrfrz(mgs)+qiacr(mgs)  &
     &  )  &
     &  +il5(mgs)*(qwfrz(mgs)    &
     &  +qwctfz(mgs)+qiihr(mgs)   &
     &  +qiacw(mgs))
      pmlt(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qsmlr(mgs)+qhlmlr(mgs))    !+qhmlh(mgs))   
      ! NOTE: psub is sum of sublimation and deposition
      psub(mgs) =    &
     &   il5(mgs)*(   &
     &  + qsdpv(mgs) + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs) + qisbv(mgs) )   &
     &   + qssbv(mgs)  + qhsbv(mgs) + qhlsbv(mgs)   &
     &  +il5(mgs)*(qiint(mgs))
      pvap(mgs) =    &
     &   qrcev(mgs) + qhcev(mgs) + qscev(mgs) + qhlcev(mgs)
      pevap(mgs) =    &
     &   Min(0.0,qrcev(mgs)) + Min(0.0,qhcev(mgs)) + Min(0.0,qscev(mgs)) + Min(0.0,qhlcev(mgs))
      ! NOTE: pdep is the deposition part only
      pdep(mgs) =    &
     &   il5(mgs)*(   &
     &  + qsdpv(mgs) + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs)  )   &
     &  +il5(mgs)*(qiint(mgs))
      ELSEIF ( warmonly < 0.8 ) THEN
      pfrz(mgs) =    &
     &  (1-il5(mgs))*   &
     &  (qhmlr(mgs)+qhlmlr(mgs))   & !+qhmlh(mgs))   &
     &  +il5(mgs)*(qhfzh(mgs)+qhlfzhl(mgs))   &
     &  +il5(mgs)*(   &
     &  +qhshr(mgs)   &
     &  +qhlshr(mgs)   &
     &  +qrfrz(mgs)+qwfrz(mgs)   &
     &  +qwctfz(mgs)+qiihr(mgs)   &
     &  +qiacw(mgs)                &
     & +qhacw(mgs) + qhlacw(mgs)   &
     & +qhacr(mgs) + qhlacr(mgs)  ) 
      psub(mgs) =  0.0 +  &
     &   il5(mgs)*(   &
     &  + qhdpv(mgs)   &
     &  + qhldpv(mgs)    &
     &  + qidpv(mgs) + qisbv(mgs) )   &
     &  +il5(mgs)*(qiint(mgs))
      pvap(mgs) =    &
     &   qrcev(mgs) + qhcev(mgs) + qhlcev(mgs) ! + qscev(mgs) 
      ELSE
      pfrz(mgs) = 0.0
      psub(mgs) = 0.0
      pvap(mgs) = qrcev(mgs)
      ENDIF ! warmonly
      ptem(mgs) =    &
     &  (1./pi0(mgs))*   &
     &  (felfcp(mgs)*pfrz(mgs)   &
     &  +felscp(mgs)*psub(mgs)    &
     &  +felvcp(mgs)*pvap(mgs))
      thetap(mgs) = thetap(mgs) + dtp*ptem(mgs)
      IF ( eqtset > 2 ) THEN
        pipert(mgs) = pipert(mgs) + (felfpi(mgs)*pfrz(mgs)   &
     &  +felspi(mgs)*psub(mgs)    &
     &  +felvpi(mgs)*pvap(mgs))*dtp
      ENDIF
      end do




!
!  sum the sources and sinks for qwvp, qcw, qci, qrw, qsw
!
!
      do mgs = 1,ngscnt
      qwvp(mgs) = qwvp(mgs) +        &
     &   dtp*(pqwvi(mgs)+pqwvd(mgs))
      qx(mgs,lc) = qx(mgs,lc) +   &
     &   dtp*(pqcwi(mgs)+pqcwd(mgs))
      qx(mgs,lr) = qx(mgs,lr) +   &
     &   dtp*(pqrwi(mgs)+pqrwd(mgs))
      qx(mgs,li) = qx(mgs,li) +   &
     &   dtp*(pqcii(mgs)+pqcid(mgs))
      qx(mgs,ls) = qx(mgs,ls) +   &
     &   dtp*(pqswi(mgs)+pqswd(mgs))
      qx(mgs,lh) = qx(mgs,lh) +    &
     &   dtp*(pqhwi(mgs)+pqhwd(mgs))
      IF ( lhl .gt. 1 ) THEN
      qx(mgs,lhl) = qx(mgs,lhl) +    &
     &   dtp*(pqhli(mgs)+pqhld(mgs))
      ENDIF


      end do

! sum sources for particle volume

      IF ( ldovol ) THEN

      do mgs = 1,ngscnt

      IF ( lvol(ls) .gt. 1 ) THEN
      vx(mgs,ls) = vx(mgs,ls) +    &
     &   dtp*(pvswi(mgs)+pvswd(mgs))
      ENDIF

      IF ( lvol(lh) .gt. 1 ) THEN
      vx(mgs,lh) = vx(mgs,lh) +    &
     &   dtp*(pvhwi(mgs)+pvhwd(mgs))
!     >   rho0(mgs)*dtp*(pqhwi(mgs)+pqhwd(mgs))/xdn0(lh)
      ENDIF

      IF ( lhl .gt. 1 ) THEN
      IF ( lvol(lhl) .gt. 1 ) THEN
      vx(mgs,lhl) = vx(mgs,lhl) +    &
     &   dtp*(pvhli(mgs)+pvhld(mgs))
!     >   rho0(mgs)*dtp*(pqhwi(mgs)+pqhwd(mgs))/xdn0(lh)
      ENDIF
      ENDIF

      ENDDO

      ENDIF  ! ldovol

!
!
!
! concentrations
!
      if ( ipconc .ge. 1  ) then
      do mgs = 1,ngscnt
      cx(mgs,li) = cx(mgs,li) +   &
     &   dtp*(pccii(mgs)+pccid(mgs)) 
      cina(mgs) = cina(mgs) + pccin(mgs)*dtp
      IF ( ipconc .ge. 2 ) THEN
      cx(mgs,lc) = cx(mgs,lc) +   &
     &   dtp*(pccwi(mgs)+pccwd(mgs))
      ENDIF
      IF ( ipconc .ge. 3 ) THEN
      cx(mgs,lr) = cx(mgs,lr) +   &
     &   dtp*(pcrwi(mgs)+pcrwd(mgs))
      ENDIF
      IF ( ipconc .ge. 4 ) THEN
      cx(mgs,ls) = cx(mgs,ls) +   &
     &   dtp*(pcswi(mgs)+pcswd(mgs))
      ENDIF
      IF ( ipconc .ge. 5 ) THEN
      cx(mgs,lh) = cx(mgs,lh) +    &
     &   dtp*(pchwi(mgs)+pchwd(mgs))
       IF ( lhl .gt. 1 ) THEN
        cx(mgs,lhl) = cx(mgs,lhl) +    &
     &     dtp*(pchli(mgs)+pchld(mgs))
       ENDIF
      ENDIF
      end do
      end if


      IF ( wrfchem_flag > 0 ) THEN
! 20130917 acd_mb_washout start
        DO mgs = 1,ngscnt
         evapprod2d(igs(mgs),kgs(mgs)) = -(qrcev(mgs) + qssbv(mgs)  + qhsbv(mgs) + qhlsbv(mgs)) ! - PRE(K) - EVPMS(K) - EVPMG(K)
         rainprod2d(igs(mgs),kgs(mgs)) = qrcnw(mgs) + qracw(mgs) + qsacw(mgs) + qhacw(mgs) + qhlacw(mgs) + &
                                         qraci(mgs) + qsaci(mgs) + qhaci(mgs) + qhlaci(mgs) + qscni(mgs)     ! PRA(K) + PRC(K) + tqimelt(K)
!         evapprod(k) = - PRE(K) - EPRDS(K) - EPRDG(K) 
!         rainprod(k) = PRA(K) + PRC(K) + PSACWS(K) + PSACWG(K) + PGSACW(K) & 
!                       + PRAI(K) + PRCI(K) + PRACI(K) + PRACIS(K) + &
!                       + PRDS(K) + PRDG(K)
        ENDDO
! 20130917 acd_mb_washout end
      ENDIF
!
!
!
! start saturation adjustment
!
      if (ndebug .gt. 0 ) write(0,*) 'conc 30a'
!      include 'sam.jms.satadj.sgi'
!
!
!
!  Modified Straka adjustment (nearly identical to Tao et al. 1989 MWR)
!
!
!
!  set up temperature and vapor arrays
!
      do mgs = 1,ngscnt
      pqs(mgs) = (380.0)/(pres(mgs))
      theta(mgs) = thetap(mgs) + theta0(mgs)
      qvap(mgs) = max( (qwvp(mgs) + qv0(mgs)), 0.0 )
      temg(mgs) = theta(mgs)*pk(mgs) ! ( pres(mgs) / poo ) ** cap
      end do
!
!  melting of cloud ice
!
      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      ptimlw(mgs) = 0.0
      end do
!
      do mgs = 1,ngscnt
      qitmp(mgs) = qx(mgs,li)
      if( temg(mgs) .gt. tfr .and.   &
     &    qitmp(mgs) .gt. 0.0 ) then
      qx(mgs,lc) = qx(mgs,lc) + qitmp(mgs)
!      pfrz(mgs) = pfrz(mgs) - qitmp(mgs)/dtp
      ptem(mgs) =  ptem(mgs) +   &
     &  (1./pi0(mgs))*   &
     &  felfcp(mgs)*(- qitmp(mgs)/dtp)  
      IF ( eqtset > 2 ) THEN
        pipert(mgs) = pipert(mgs) - (felfpi(mgs)*qitmp(mgs))
      ENDIF
      pmlt(mgs) = pmlt(mgs) - qitmp(mgs)/dtp
      scx(mgs,lc) = scx(mgs,lc) + scx(mgs,li)
      thetap(mgs) = thetap(mgs) -   &
     &  fcc3(mgs)*qitmp(mgs)
      ptimlw(mgs) = -fcc3(mgs)*qitmp(mgs)/dtp
      cx(mgs,lc) = cx(mgs,lc) + cx(mgs,li)
      qx(mgs,li) = 0.0
      cx(mgs,li) = 0.0
      scx(mgs,li) = 0.0
      vx(mgs,li) = 0.0
      qitmp(mgs) = 0.0
      end if
      end do

!
!


!      do mgs = 1,ngscnt
!      qimlw(mgs) = (qcwtmp(mgs)-qx(mgs,lc))/dtp
!      end do
!
!  homogeneous freezing of cloud water
!
      IF ( warmonly < 0.8 ) THEN

      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      ptwfzi(mgs) = 0.0
      end do
!
      do mgs = 1,ngscnt

!      if( temg(mgs) .lt. tfrh ) THEN
!       write(0,*) 'GS: mgs,temp,qc,qi = ',mgs,temg(mgs),temcg(mgs),qx(mgs,lc),qx(mgs,li)
!      ENDIF

      ctmp = 0.0
      frac = 0.0
      qtmp = 0.0
      
!      if( ( temg(mgs) .lt. thnuc + 2. .or. (ibfc == 2 .and. temg(mgs) < thnuc + 10. ) ) .and.    &
!     &  qx(mgs,lc) .gt. qxmin(lc) .and. (ipconc < 2 .or. ibfc == 0 .or. ibfc == 2 )) then
      if( temg(mgs) .lt. thnuc + 0. .and.    &
     &  qx(mgs,lc) .gt. 0.0 .and. (ipconc < 2 .or. ibfc == 0 )) then

      IF ( ibfc /= 2 .or. ipconc < 2 ) THEN
      frac = Max( 0.25, Min( 1., ((thnuc + 1.) - temg(mgs) )/4.0 ) )
      ELSE
          volt = exp( 16.2 + 1.0*temcg(mgs) )* 1.0e-6 !  Ts == -temcg ; volt comes from the fit in Fig. 1 in Bigg 1953 
                                               ! for mean temperature for freezing: -ln (V) = a*Ts - b
                                               ! volt is given in cm**3, so factor of 1.e-6 to convert to m**3
         
         cwfrz(mgs) = cx(mgs,lc)*Exp(-volt/xv(mgs,lc)) ! number of droplets with volume greater than volt

         qtmp = cwfrz(mgs)*xdn0(lc)*rhoinv(mgs)*(volt + xv(mgs,lc))
         frac = qtmp/qx(mgs,lc) ! reset number frozen to same fraction as mass. This makes 
                                                       ! sure that cwfrz and qwfrz are consistent and prevents 
                                                       ! spurious creation of ice crystals.
      
      ENDIF
      qtmp = frac*qx(mgs,lc)

      qx(mgs,li) = qx(mgs,li) + qtmp ! qx(mgs,lc)
      pfrz(mgs) = pfrz(mgs) + qtmp/dtp
      ptem(mgs) =  ptem(mgs) +   &
     &  (1./pi0(mgs))*   &
     &  felfcp(mgs)*(qtmp/dtp)  

      IF ( eqtset > 2 ) THEN
        pipert(mgs) = pipert(mgs) + felfpi(mgs)*qtmp
      ENDIF

!      IF ( lvol(li) .gt. 1 ) vx(mgs,li) = vx(mgs,li) + rho0(mgs)*qx(mgs,lc)/xdn0(li)
      IF ( lvol(li) .gt. 1 ) vx(mgs,li) = vx(mgs,li) + rho0(mgs)*qtmp/xdn0(li)

      IF ( ipconc .ge. 2 ) THEN
        ctmp = frac*cx(mgs,lc)
!        cx(mgs,li) = cx(mgs,li) + cx(mgs,lc)
        cx(mgs,li) = cx(mgs,li) + ctmp
      ELSE ! (ipconc .lt. 2 )
        ctmp = 0.0
        IF ( t9(igs(mgs),jgs,kgs(mgs)-1) .gt. qx(mgs,lc) ) THEN
           qtmp = frac*t9(igs(mgs),jgs,kgs(mgs)-1)  

!           cx(mgs,lc) = cx(mgs,lc)*qx(mgs,lc)*rho0(mgs)/qtmp
           ctmp = cx(mgs,lc)*qx(mgs,lc)*rho0(mgs)/qtmp
        ELSE
           cx(mgs,lc) = Max(0.0,wvel(mgs))*dtp*cwccn   &
     &      /gz(igs(mgs),jgs,kgs(mgs))
          cx(mgs,lc) = cwccn
        ENDIF

       IF ( ipconc .ge. 1 ) cx(mgs,li) = Min(ccimx, cx(mgs,li) + cx(mgs,lc))
      ENDIF

      sctmp = frac*scx(mgs,lc)
!      scx(mgs,li) = scx(mgs,li) + scx(mgs,lc)
      scx(mgs,li) = scx(mgs,li) + sctmp
!      thetap(mgs) = thetap(mgs) + fcc3(mgs)*qx(mgs,lc)
!      ptwfzi(mgs) = fcc3(mgs)*qx(mgs,lc)/dtp
!      qx(mgs,lc) = 0.0
!      cx(mgs,lc) = 0.0
!      scx(mgs,lc) = 0.0
      thetap(mgs) = thetap(mgs) + fcc3(mgs)*qtmp
      ptwfzi(mgs) = fcc3(mgs)*qtmp/dtp
      qx(mgs,lc) = qx(mgs,lc) - qtmp
      cx(mgs,lc) = cx(mgs,lc) - ctmp
      scx(mgs,lc) = scx(mgs,lc) - sctmp
      end if
      end do

      ENDIF ! warmonly
!
!      do mgs = 1,ngscnt
!      qwfzi(mgs) = (qcwtmp(mgs)-qx(mgs,lc))/dtp   ! Not used?? (ERM)
!      end do
!
!  reset temporaries for cloud particles and vapor
!
      qcond(:) = 0.0
      
      IF ( ipconc .le. 1 .and.  lwsm6 ) THEN ! Explicit cloud condensation/evaporation (Rutledge and Hobbs 1983)
       DO mgs = 1,ngscnt

        qcwtmp(mgs) = qx(mgs,lc)
        theta(mgs) = thetap(mgs) + theta0(mgs)
        temgtmp = temg(mgs)
!        temg(mgs) = theta(mgs)*(p2(igs(mgs),jgs,kgs(mgs)) ) ! *pk(mgs) ! ( pres(mgs) / poo ) ** cap
!        temsav = temg(mgs)
!        thsave(mgs) = thetap(mgs)
        temg(mgs) = theta(mgs)*pk(mgs) ! ( pres(mgs) / poo ) ** cap
        temcg(mgs) = temg(mgs) - tfr
        ltemq = (temg(mgs)-163.15)/fqsat+1.5
        ltemq = Min( nqsat, Max(1,ltemq) )

        qvs(mgs) = pqs(mgs)*tabqvs(ltemq)

        IF ( ( qvap(mgs) > qvs(mgs) .or. qx(mgs,lc) > qxmin(lc) ) .and. temg(mgs) > tfrh ) THEN
          tmp = (qvap(mgs) - qvs(mgs))/(1. + qvs(mgs)*felv(mgs)**2/(cp*rw*temg(mgs)**2) )
          qcond(mgs) = Min( Max( 0.0, tmp ), (qvap(mgs)-qvs(mgs)) )
          IF ( qx(mgs,lc) > qxmin(lc) .and. tmp < 0.0 ) THEN ! evaporation
            qcond(mgs) = Max( tmp, -qx(mgs,lc) )
          ENDIF
          qwvp(mgs) = qwvp(mgs) - qcond(mgs)
          qvap(mgs) = qvap(mgs) - qcond(mgs)
          qx(mgs,lc) = Max( 0.0, qx(mgs,lc) + qcond(mgs) )
          thetap(mgs) = thetap(mgs) + felvcp(mgs)*qcond(mgs)/(pi0(mgs))
          
        ENDIF
        
        ENDDO
      
      ENDIF
      
      
      IF ( ipconc .le. 1 .and. .not. lwsm6 ) THEN
!      IF ( ipconc .le. 1  ) THEN
      
      do mgs = 1,ngscnt
      qx(mgs,lv) = max( 0.0, qvap(mgs) )
      qx(mgs,lc) = max( 0.0, qx(mgs,lc) )
      qx(mgs,li) = max( 0.0, qx(mgs,li) )
      qitmp(mgs) = qx(mgs,li)
      end do
!
!
      do mgs = 1,ngscnt
      qcwtmp(mgs) = qx(mgs,lc)
      qitmp(mgs) = qx(mgs,li)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      temgtmp = temg(mgs)
      temg(mgs) = theta(mgs)*(pinit(kgs(mgs)) + p2(igs(mgs),jgs,kgs(mgs)) ) ! *pk(mgs) ! ( pres(mgs) / poo ) ** cap
      temsav = temg(mgs)
      thsave(mgs) = thetap(mgs)
      temcg(mgs) = temg(mgs) - tfr
      tqvcon = temg(mgs)-cbw
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
!      IF ( ltemq .lt. 1 .or. ltemq .gt. nqsat ) THEN
! C$PAR CRITICAL SECTION
!        write(iunit,*) 'out of range ltemq!',temgtmp,temg(mgs),
!     :      thetap(mgs),theta0(mgs),pres(mgs),theta(mgs),
!     :      ltemq,igs(mgs),jy,kgs(mgs)
!        write(iunit,*) an(igs(mgs),jy,kgs(mgs),lt),
!     :   ab(igs(mgs),jy,kgs(mgs),lt),
!     :   t0(igs(mgs),jy,kgs(mgs))
!        write(iunit,*) fcc3(mgs),qx(mgs,lc),qitmp(mgs),dtp,ptem(mgs)
!        STOP
! C$PAR END CRITICAL SECTION
!      END IF
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)
!      qss(kz) = qvs(kz)
!      if ( temg(kz) .lt. tfr ) then
!      if( qcw(kz) .le. qxmin(lc) .and. qci(kz) .gt. qxmin(li))
!     >  qss(kz) = qis(kz)
!      if( qcw(kz) .gt. qxmin(lc) .and. qci(kz) .gt. qxmin(li))
!     >   qss(kz) = (qcw(kz)*qvs(kz) + qci(kz)*qis(kz)) /
!     >   (qcw(kz) + qci(kz))
!      qss(kz) = qis(kz)
!      end if
! dont get enough condensation with qcw .le./.gt. qxmin(lc)
!      if ( temg(mgs) .lt. tfr ) then
!      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )
!     >  qss(mgs) = qvs(mgs)
!      if( qx(mgs,lc) .eq. 0.0 .and. qitmp(mgs) .gt. qxmin(li))
!     >  qss(mgs) = qis(mgs)
!      if( qx(mgs,lc) .gt. 0.0 .and. qitmp(mgs) .gt. qxmin(li))
!     >   qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /
!     >   (qx(mgs,lc) + qitmp(mgs))
!      else
!      qss(mgs) = qvs(mgs)
!      end if
      qss(mgs) = qvs(mgs)
      if ( temg(mgs) .lt. tfr ) then
      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )   &
     &  qss(mgs) = qvs(mgs)
      if( qx(mgs,lc) .le. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &  qss(mgs) = qis(mgs)
      if( qx(mgs,lc) .gt. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &   qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /   &
     &   (qx(mgs,lc) + qitmp(mgs))
      end if
      end do
!
!  iterate  adjustment
!
      do itertd = 1,2
!
      do mgs = 1,ngscnt
!
!  calculate super-saturation
!
      qitmp(mgs) = qx(mgs,li)
      fcci(mgs) = 0.0
      fcip(mgs) = 0.0
      dqcw(mgs) = 0.0
      dqci(mgs) = 0.0
      dqwv(mgs) = ( qx(mgs,lv) - qss(mgs) )
!
!  evaporation and sublimation adjustment
!
      if( dqwv(mgs) .lt. 0. ) then           !  subsaturated
        if( qx(mgs,lc) .gt. -dqwv(mgs) ) then  ! check if qc can make up all of the deficit
          dqcw(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                 !  otherwise make all qc available for evap
          dqcw(mgs) = -qx(mgs,lc)
          dqwv(mgs) = dqwv(mgs) + qx(mgs,lc)
        end if
!
        if( qitmp(mgs) .gt. -dqwv(mgs) ) then  ! check if qi can make up all the deficit
          dqci(mgs) = dqwv(mgs)
          dqwv(mgs) = 0.
        else                                  ! otherwise make all ice available for sublimation
          dqci(mgs) = -qitmp(mgs)
          dqwv(mgs) = dqwv(mgs) + qitmp(mgs)
        end if
!
       qwvp(mgs) = qwvp(mgs) - ( dqcw(mgs) + dqci(mgs) )  ! add to perturbation vapor
!
! This next line removed 3/19/2003 thanks to Adam Houston,
!  who found the bug in the 3-ICE code
!      qwvp(mgs) = max(qwvp(mgs), 0.0)
      qitmp(mgs) = qx(mgs,li)
      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        fcci(mgs) = qx(mgs,li)/(qitmp(mgs))
      ELSE
        fcci(mgs) = 0.0
      ENDIF
      qx(mgs,lc) = qx(mgs,lc) + dqcw(mgs)
      qx(mgs,li) = qx(mgs,li) + dqci(mgs) * fcci(mgs)
      thetap(mgs) = thetap(mgs) +   &
     &  1./pi0(mgs)*   &
     &  (felvcp(mgs)*dqcw(mgs) +felscp(mgs)*dqci(mgs))

      IF ( eqtset > 2 ) THEN
        pipert(mgs) = pipert(mgs)   &
     &  +(felspi(mgs)*dqci(mgs)    &
     &  +felvpi(mgs)*dqcw(mgs))*dtp
      ENDIF

      end if  ! dqwv(mgs) .lt. 0. (end of evap/sublim)
!
! condensation/deposition
!
      IF ( dqwv(mgs) .ge. 0. ) THEN
      
!      write(iunit,*) 'satadj: mgs,iter = ',mgs,itertd,dqwv(mgs),qss(mgs),qx(mgs,lv),qx(mgs,lc)
!
        qitmp(mgs) = qx(mgs,li)
        fracl(mgs) = 1.0
        fraci(mgs) = 0.0
        if ( temg(mgs) .lt. tfr .and. temg(mgs) .gt. thnuc ) then
          fracl(mgs) = max(min(1.,(temg(mgs)-233.15)/(20.)),0.0)
          fraci(mgs) = 1.0-fracl(mgs)
        end if
        if ( temg(mgs) .le. thnuc ) then
           fraci(mgs) = 1.0
           fracl(mgs) = 0.0
         end if
        fraci(mgs) = 1.0-fracl(mgs)
!
       gamss = (felvcp(mgs)*fracl(mgs) + felscp(mgs)*fraci(mgs))   &
     &      / (pi0(mgs))
!
      IF ( temg(mgs) .lt. tfr ) then
        IF (qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) ) then
         dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbw)**2))
        END IF
        IF ( qx(mgs,lc) .eq. 0.0 .and. qitmp(mgs) .gt. qxmin(li) ) then
          dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv2(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbi)**2))
        END IF
        IF ( qx(mgs,lc) .gt. 0.0 .and. qitmp(mgs) .gt. qxmin(li) ) then
         cdw = caw*pi0(mgs)*tfrcbw/((temg(mgs)-cbw)**2)
         cdi = cai*pi0(mgs)*tfrcbi/((temg(mgs)-cbi)**2)
         denom1 = qx(mgs,lc) + qitmp(mgs)
         denom2 = 1.0 + gamss*   &
     &    (qx(mgs,lc)*qvs(mgs)*cdw + qitmp(mgs)*qis(mgs)*cdi) / denom1
         dqvcnd(mgs) =  dqwv(mgs) / denom2
        END IF 

      ENDIF  !  temg(mgs) .lt. tfr
!
      if ( temg(mgs) .ge. tfr ) then
      dqvcnd(mgs) = dqwv(mgs)/(1. + fcqv1(mgs)*qss(mgs)/   &
     &  ((temg(mgs)-cbw)**2))
      end if
!
      delqci1=qx(mgs,li)
!
      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        fcci(mgs) = qx(mgs,li)/(qitmp(mgs))
      ELSE
        fcci(mgs) = 0.0
      ENDIF
!
      dqcw(mgs) = dqvcnd(mgs)*fracl(mgs)
      dqci(mgs) = dqvcnd(mgs)*fraci(mgs)
!
      thetap(mgs) = thetap(mgs) +   &
     &   (felvcp(mgs)*dqcw(mgs) + felscp(mgs)*dqci(mgs))   &
     & / (pi0(mgs))

      IF ( eqtset > 2 ) THEN
        pipert(mgs) = pipert(mgs) + (0   &
     &  +felspi(mgs)*dqci(mgs)    &
     &  +felvpi(mgs)*dqcw(mgs))*dtp
      ENDIF

      qwvp(mgs) = qwvp(mgs) - ( dqvcnd(mgs) )
      qx(mgs,lc) = qx(mgs,lc) + dqcw(mgs)
      IF ( qitmp(mgs) .gt. qxmin(li) ) THEN
        qx(mgs,li) = qx(mgs,li) + dqci(mgs)*fcci(mgs)
        qitmp(mgs) = qx(mgs,li)
      ENDIF
!
!      delqci(mgs) =  dqci(mgs)*fcci(mgs)
!
      END IF !  dqwv(mgs) .ge. 0.
      end do
!
      do mgs = 1,ngscnt
      qitmp(mgs) = qx(mgs,li)
      theta(mgs) = thetap(mgs) + theta0(mgs)
      temg(mgs) = theta(mgs)*pk(mgs) ! ( pres(mgs) / poo ) ** cap
      qvap(mgs) = Max((qwvp(mgs) + qv0(mgs)), 0.0)
      temcg(mgs) = temg(mgs) - tfr
      tqvcon = temg(mgs)-cbw
      ltemq = (temg(mgs)-163.15)/fqsat+1.5
      ltemq = Min( nqsat, Max(1,ltemq) )
      qvs(mgs) = pqs(mgs)*tabqvs(ltemq)
      qis(mgs) = pqs(mgs)*tabqis(ltemq)
      qx(mgs,lc) = max( 0.0, qx(mgs,lc) )
      qitmp(mgs) = max( 0.0, qitmp(mgs) )
      qx(mgs,lv) = max( 0.0, qvap(mgs))
!      if ( temg(mgs) .lt. tfr ) then
!      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )
!     >  qss(mgs) = qvs(mgs)
!c      if( qx(mgs,lc) .le. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))
!      if( qx(mgs,lc) .eq. 0.0 .and. qitmp(mgs) .gt. qxmin(li))
!     >  qss(mgs) = qis(mgs)
!c      if( qx(mgs,lc) .gt. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))
!      if( qx(mgs,lc) .gt. 0.0 .and. qitmp(mgs) .gt. qxmin(li))
!     >  qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /
!     > (qx(mgs,lc) + qitmp(mgs))
!      else
!      qss(mgs) = qvs(mgs)
!      end if
      qss(mgs) = qvs(mgs)
      if ( temg(mgs) .lt. tfr ) then
      if( qx(mgs,lc) .ge. 0.0 .and. qitmp(mgs) .le. qxmin(li) )   &
     &  qss(mgs) = qvs(mgs)
      if( qx(mgs,lc) .le. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &  qss(mgs) = qis(mgs)
      if( qx(mgs,lc) .gt. qxmin(lc) .and. qitmp(mgs) .gt. qxmin(li))   &
     &   qss(mgs) = (qx(mgs,lc)*qvs(mgs) + qitmp(mgs)*qis(mgs)) /   &
     &   (qx(mgs,lc) + qitmp(mgs))
      end if
!      pceds(mgs) = (thetap(mgs) - thsave(mgs))/dtp
!      write(iunit,*) 'satadj2: mgs,iter = ',mgs,itertd,dqwv(mgs),qss(mgs),qx(mgs,lv),qx(mgs,lc)
      end do
!
!  end the saturation adjustment iteration loop
!
      end do

     ENDIF ! ( ipconc .le. 1 )

!
!  spread the growth owing to vapor diffusion onto the
!  ice crystal categories using the
!
!  END OF SATURATION ADJUSTMENT
!

      if (ndebug .gt. 0 ) write(0,*) 'conc 30b'
!
!
!  end of saturation adjustment

!
!
! !DIR$ IVDEP
      do mgs = 1,ngscnt
      t0(igs(mgs),jy,kgs(mgs)) =  temg(mgs)
      end do
!
! Load the save arrays
!



      if (ndebug .gt. 0 ) write(0,*) 'gs 11'

      do mgs = 1,ngscnt
!
      an(igs(mgs),jy,kgs(mgs),lt) =    &
     &  theta0(mgs) + thetap(mgs) 
      an(igs(mgs),jy,kgs(mgs),lv) = qwvp(mgs) + qv0(mgs) !

      IF ( eqtset > 2 ) THEN
        p2(igs(mgs),jy,kgs(mgs)) = pipert(mgs)
      ENDIF
!
      
      DO il = lc,lhab
        IF ( ido(il) .eq. 1 ) THEN
         an(igs(mgs),jy,kgs(mgs),il) = qx(mgs,il) +   &
     &     min( an(igs(mgs),jy,kgs(mgs),il), 0.0 )
         qx(mgs,il) = an(igs(mgs),jy,kgs(mgs),il)
        ENDIF
      ENDDO

      IF ( lcina > 1 ) THEN
        an(igs(mgs),jy,kgs(mgs),lcina) = cina(mgs)
      ENDIF


!
      end do
!

      if ( ipconc .ge. 1 ) then
      DO il = lc,lhab !{

!        write(0,*) 'limiter loop: il,ipc,lz: ',il,ipc(il),lz(il),ipconc

       IF ( ipconc .ge. ipc(il) ) THEN ! {

         IF (  ipconc .ge. 4 .and. ipc(il) .ge. 1 ) THEN ! {

!            write(0,*) 'MY limiter: il,ipc,lz: ',il,ipc(il),lz(il),lr,lzr
!            STOP

          IF ( lz(il) <= 1 .or. ioldlimiter == 1 ) THEN ! { { is a two-moment category so dont worry about reflectivity
          

           DO mgs = 1,ngscnt
            IF ( qx(mgs,il) .le. 0.0 ) THEN
              cx(mgs,il) = 0.0
            ELSE !{
              IF ( cx(mgs,il) .gt. cxmin ) THEN !{
!              xv(mgs,il) = rho0(mgs)*qx(mgs,il)/(xdn(mgs,il)*Max(1.0e-9,cx(mgs,il)))
!              xv(mgs,il) = rho0(mgs)*qx(mgs,il)/(xdn(mgs,il)*Max(cxmin,cx(mgs,il)))
                xv(mgs,il) = rho0(mgs)*qx(mgs,il)/(xdn(mgs,il)*cx(mgs,il))
              
!              IF ( lhl .gt. 1 .and. il .eq. lhl ) THEN
!               write(0,*) 'dr: xv,cx,qx,xdn,ln = ',xv(mgs,il),cx(mgs,il),qx(mgs,il),xdn(mgs,il),ln(il)
!              ENDIF

               ! 8/26/2015 erm: apply imaxdiaopt for 2-moment also
               IF ( imaxdiaopt == 1 .or. il == lc .or. il == li .or. (il == lr .and. imurain == 3) .or. (il == ls .and. imusnow == 3 ) ) THEN
                 xvbarmax = xvmx(il)
               ELSEIF ( imaxdiaopt == 2 ) THEN ! test against maximum mass diameter
                 xvbarmax = xvmx(il) /((3. + alpha(mgs,il))**3/((3. + alpha(mgs,il))*(2. + alpha(mgs,il))*(1. + alpha(mgs,il))))
               ELSEIF ( imaxdiaopt == 3 ) THEN ! test against mass-weighted diameter
                 xvbarmax = xvmx(il) /((4. + alpha(mgs,il))**3/((3. + alpha(mgs,il))*(2. + alpha(mgs,il))*(1. + alpha(mgs,il))))
               ENDIF

               tmp = 1.0
               IF ( il == ls ) THEN
                 xvbarmax = xvbarmax*Max(1.,100./Min(100.,xdn(mgs,ls)))
               ENDIF
               
               IF ( xv(mgs,il) .lt. xvmn(il) .or. xv(mgs,il) .gt. xvbarmax ) THEN
                xv(mgs,il) = Min( xvbarmax, xv(mgs,il) )
                xv(mgs,il) = Max( xvmn(il), xv(mgs,il) )
                cx(mgs,il) = rho0(mgs)*qx(mgs,il)/(xv(mgs,il)*xdn(mgs,il))
               ENDIF
              
             ENDIF !}

!              IF ( lhl .gt. 1 .and. il .eq. lhl ) THEN
!               write(0,*) 'dr: xv,cx,= ',xv(mgs,il),cx(mgs,il)
!              ENDIF

            ENDIF !}
           ENDDO ! mgs
          
          
          ENDIF ! }}
          ENDIF ! }

          DO mgs = 1,ngscnt
            an(igs(mgs),jy,kgs(mgs),ln(il)) = Max(cx(mgs,il), 0.0)
          ENDDO
        ENDIF ! }
      ENDDO ! il }

      IF ( lcin > 1 ) THEN
      do mgs = 1,ngscnt
        an(igs(mgs),jy,kgs(mgs),lcin) = Max(0.0, ccin(mgs))
      end do
      ENDIF

      IF ( ipconc .ge. 2 ) THEN
      do mgs = 1,ngscnt
        IF ( lss > 1 ) THEN
          an(igs(mgs),jy,kgs(mgs),lss) = Max(0.0, ssmax(mgs) )
        ENDIF

        IF ( lccn > 1 ) THEN
          an(igs(mgs),jy,kgs(mgs),lccn) = Max(0.0, ccnc(mgs) )
        ENDIF
      end do
      ENDIF
      
      ELSEIF ( ipconc .eq. 0 .and. lni .gt. 1 ) THEN
      
          DO mgs = 1,ngscnt
            an(igs(mgs),jy,kgs(mgs),lni) = Max(cx(mgs,li), 0.0)
          ENDDO


      end if

      IF ( ldovol ) THEN

       DO il = li,lhab

        IF ( lvol(il) .ge. 1 ) THEN

          DO mgs = 1,ngscnt

           an(igs(mgs),jy,kgs(mgs),lvol(il)) = Max( 0.0, vx(mgs,il) )
          ENDDO
          
        ENDIF
      
       ENDDO
      
      ENDIF
!
!
!
!
!
      if (ndebug .gt. 0 ) write(0,*) 'gs 12'



      if (ndebug .gt. 0 ) write(0,*) 'gs 13'

 9998 continue

      if ( kz .gt. nz-1 .and. ix .ge. itile) then
        if ( ix .ge. itile ) then
         go to 1200 ! exit gather scatter
        else
         nzmpb = kz
        endif
      else
        nzmpb = kz
      end if

      if ( ix .ge. itile ) then
        nxmpb = 1
        nzmpb = kz+1
      else
       nxmpb = ix+1
      end if

 1000 continue
 1200 continue
!
!  end of gather scatter (for this jy slice)
!
!

      return
      end subroutine nssl_2mom_gs
!
!--------------------------------------------------------------------------
!



!
!--------------------------------------------------------------------------
!


END MODULE module_mp_nssl_2mom
