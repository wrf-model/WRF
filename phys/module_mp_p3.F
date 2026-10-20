!
!__________________________________________________________________________________________!
! This module contains the Predicted Particle Property (P3) bulk microphysics scheme.      !
!                                                                                          !
! This code was originally written by H. Morrison, MMM Division, NCAR (Dec 2012).          !
! Modifications were made by J. Milbrandt, RPN, Environment Canada (July 2014).            !
! Subsequent major and minor upgrades have been ongoing.                                   !
!                                                                                          !
! For model-specific aspects/versions, see comments in the interface subroutine(s) in      !
!   this module (mp_p3_wrapper_wrfcm1, mp_p3_wrapper_gem).                                 !
!                                                                                          !
! For details see:                                                                         !
!   Morrison and Milbrandt (2015) [J. Atmos. Sci., 72, 287-311]    - original scheme desc. !
!   Milbrandt and Morrison (2016) [J. Atmos. Sci., 73, 975-995]    - multi-ice-category    !
!   Cholette et al. (2019)        [J. Atmos. Sci., 76, 561-582]    - liquid fraction ice   !
!   Jouan et al. (2020)           [W. Forecasting, 35, 2541-2565]  - cloud fraction        !
!   Milbrandt et al. (2021)       [J. Atmos. Sci., 78, 439-458]    - triple-moment ice     !
!   Cholette et al. (2023)        [J.A.M.E.S, 15(4), e2022MS003328 - trplMomIce + liqFrac  !
!   Morrison et al. (2025)        [J.A.M.E.S, 17, e2024MS004644    - full trplMomIce       !
!                                                                                          !
! For questions or bug reports, please contact:                                            !
!    Hugh Morrison   (morrison@ucar.edu), or                                               !
!    Jason Milbrandt (jason.milbrandt@ec.gc.ca), or                                        !
!    Melissa Cholette (melissa.cholette@ec.gc.ca)                                          !
!                                                                                          !
! For all code updates, including bug-fixes, new developments, and test summaries, see:    !
!    https://github.com/P3-microphysics/P3-microphysics                                    !
!__________________________________________________________________________________________!
!                                                                                          !
! Version:       5.6.2+sbaut                                                               !
! Last updated:  2026 Oct                                                                  !
!__________________________________________________________________________________________!

 MODULE microphy_p3

#ifdef ECCCGEM
 use tdpack, only: foew, foewa, fohrx, foewaf
 use tdpack_const, only: aerk1w
#endif

 implicit none

 private
 public :: p3_main, polysvp1, p3_init
#ifdef ECCCGEM
 public :: mp_p3_wrapper_gem, p3_phybusinit, p3_lwc, p3_iwc
#else
 public :: mp_p3_wrapper_wrfcm1
#endif

 integer, parameter, public :: STATUS_ERROR  = -1
 integer, parameter, public :: STATUS_OK     = 0
 integer, save              :: global_status = STATUS_OK

! ice microphysics lookup table array dimensions
 integer, parameter :: isize        = 50
 integer, parameter :: iisize       = 25
 integer, parameter :: zsize        = 11  ! size of mu_i array in lookup_table (for 3-moment ice)
 integer, parameter :: zqsize       = 80  ! size of Zi,tot/qi,tot for mu_i array in lookup_table (for 3-moment ice)
 integer, parameter :: densize      =  5
 integer, parameter :: rimsize      =  4
 integer, parameter :: liqsize      =  4
 integer, parameter :: rcollsize    = 30
 integer, parameter :: tabsize      = 19  ! number of quantities used from 2-mom lookup table
 integer, parameter :: tabsize_3mom = 29  ! number of quantities used from 3-mom lookup table
 integer, parameter :: colltabsize  =  2  ! number of ice-rain collection  quantities used from lookup table
 integer, parameter :: colltabsize_3mom  =  3  ! number of ice-rain collection  quantities used from 3-mom lookup table
 integer, parameter :: collitabsize =  2  ! number of ice-ice collection  quantities used from lookup table
 integer, parameter :: n_args_r     = 8   ! array size for args_r (passed to functions to access LUTs
 integer, parameter :: n_args_i     = 6   ! array size for args_i (passed to functions to access LUTs

!-- LUT column indices (named parameters replacing magic numbers in proc_from_LUT_* calls)
!
!  LUT1 columns shared by both 2-mom and 3-mom (cols 1-12 identical in both)
 integer, parameter :: LUT1_UNS    =  1  ! number-weighted fallspeed
 integer, parameter :: LUT1_UMS    =  2  ! mass-weighted fallspeed
 integer, parameter :: LUT1_NAGG   =  3  ! ice self-aggregation rate
 integer, parameter :: LUT1_NRWAT  =  4  ! cloud-water collection rate
 integer, parameter :: LUT1_VDEP   =  5  ! vapor deposition/sublimation rate
 integer, parameter :: LUT1_EFF    =  6  ! effective radius
 integer, parameter :: LUT1_QSML   =  7  ! normalized 1/Q_small (lower size limiter)
 integer, parameter :: LUT1_QLRG   =  8  ! normalized 1/Q_large (upper size limiter)
 integer, parameter :: LUT1_REFL   =  9  ! equivalent reflectivity factor
 integer, parameter :: LUT1_VDEP1  = 10  ! melting ventilation coefficient
 integer, parameter :: LUT1_DMM    = 11  ! mass-weighted mean diameter
 integer, parameter :: LUT1_RHOMM  = 12  ! mass-weighted mean bulk density
!
!  LUT1 2-moment specific columns (cols 13-19)
 integer, parameter :: LUT1_2M_LAM  = 13  ! lambda_i PSD slope parameter
 integer, parameter :: LUT1_2M_MU   = 14  ! mu_i PSD shape parameter
 integer, parameter :: LUT1_2M_VDM1 = 15  ! melt-to-rain ventilation term 1
 integer, parameter :: LUT1_2M_VDM2 = 16  ! melt-to-rain ventilation term 2
 integer, parameter :: LUT1_2M_VDM3 = 17  ! melt-on-ice ventilation term 1
 integer, parameter :: LUT1_2M_VDM4 = 18  ! melt-on-ice ventilation term 2
 integer, parameter :: LUT1_2M_SHED = 19  ! shedding rate
!
!  LUT1 3-moment specific columns (cols 13-29; uzs at col 13 shifts all subsequent +1 vs 2-mom)
 integer, parameter :: LUT1_3M_UZS  = 13  ! z-moment-weighted fallspeed (internal)
 integer, parameter :: LUT1_3M_LAM  = 14  ! lambda_i PSD slope parameter
 integer, parameter :: LUT1_3M_MU   = 15  ! mu_i PSD shape parameter
 integer, parameter :: LUT1_3M_VDM1 = 16  ! melt-to-rain ventilation term 1
 integer, parameter :: LUT1_3M_VDM2 = 17  ! melt-to-rain ventilation term 2
 integer, parameter :: LUT1_3M_VDM3 = 18  ! melt-on-ice ventilation term 1
 integer, parameter :: LUT1_3M_VDM4 = 19  ! melt-on-ice ventilation term 2
 integer, parameter :: LUT1_3M_SHED = 20  ! shedding rate
 integer, parameter :: LUT1_3M_M6RI = 21  ! 6th-moment tendency: riming
 integer, parameter :: LUT1_3M_M6D1 = 22  ! 6th-moment tendency: vapor deposition term 1
 integer, parameter :: LUT1_3M_M6D2 = 23  ! 6th-moment tendency: vapor deposition term 2
 integer, parameter :: LUT1_3M_M6L1 = 24  ! 6th-moment tendency: melting term 1
 integer, parameter :: LUT1_3M_M6L2 = 25  ! 6th-moment tendency: melting term 2
 integer, parameter :: LUT1_3M_M6AG = 26  ! 6th-moment tendency: self-collection
 integer, parameter :: LUT1_3M_M6SH = 27  ! 6th-moment tendency: shedding
 integer, parameter :: LUT1_3M_M6S1 = 28  ! 6th-moment tendency: sublimation term 1
 integer, parameter :: LUT1_3M_M6S2 = 29  ! 6th-moment tendency: sublimation term 2
!
!  Ice-rain collection table (itabcoll / itabcoll_3mom)
 integer, parameter :: LUTR_NR = 1  ! log10(ice-rain number collection)
 integer, parameter :: LUTR_QR = 2  ! log10(ice-rain mass collection)
 integer, parameter :: LUTR_M6 = 3  ! 6th-moment tendency: ice-rain collection (3-mom only)
!
!  Ice-ice collision table (itabcolli*; nCat > 1 only)
 integer, parameter :: LUTII_DN = 1  ! number change from ice-ice collision
 integer, parameter :: LUTII_DM = 2  ! mass change from ice-ice collision
!
!  LUT3: mu_i / rho_i diagnostic table (proc_from_LUT_3)
 integer, parameter :: LUT3_MUI  = 1  ! mu_i shape parameter
 integer, parameter :: LUT3_RHOI = 2  ! bulk ice density

 real, parameter    :: real_rcollsize = real(rcollsize)

!ice microphysics lookup table values (allocatable: only one of 2-mom or 3-mom set is allocated at runtime)
 real, allocatable :: itab         (:,:,:,:,:)     !2-mom: (densize,rimsize,liqsize,isize,tabsize)
 real, allocatable :: itab_3mom    (:,:,:,:,:,:)   !3-mom: (zsize,densize,rimsize,liqsize,isize,tabsize_3mom)
 real, allocatable :: itab_3mom_mui(:,:,:,:,:,:)   !3-mom: (zqsize,densize,rimsize,liqsize,isize,2)

!ice lookup table values for ice-rain collision/collection (allocatable: matches 2-mom or 3-mom set)
 real, allocatable :: itabcoll     (:,:,:,:,:,:)   !2-mom: (densize,rimsize,liqsize,isize,rcollsize,colltabsize)
 real, allocatable :: itabcoll_3mom(:,:,:,:,:,:,:) !3-mom: (zsize,densize,rimsize,liqsize,isize,rcollsize,colltabsize_3mom)

!ice-ice collection lookup table values (allocatable: only allocated when nCat > 1;
! the liquid-fraction tables 011..112 additionally require liqfrac = .true.)
! separated into itabcolli001/002/etc. due to max-7-dimension limit on some Fortran compilers
!
! Naming convention itabcolli[a][b][q]:
!   a = Fi,liq of LT2-generator category 1 (0 or 1) = the COLLECTEE at runtime
!       (table dims 1-3: collectee size, rime fraction, rime density)
!   b = Fi,liq of LT2-generator category 2 (0 or 1) = the COLLECTOR at runtime
!       (table dims 4-6: collector size, rime fraction, rime density)
!   q = quantity: 1 = number collected (n_agg), 2 = mass collected (q_agg)
! e.g. itabcolli011 = collectee fully frozen, collector fully liquid, number.
! proc_from_LUT_ii blends b with the collector liq fraction (dum7) and a with the
! collectee liq fraction (dum7c).  The 8-column order in the LT2 file must match
! the read statement in p3_init — see the assembly protocol in the header of
! create_p3_lookupTable_2.f90.
 real, allocatable :: itabcolli001(:,:,:,:,:,:)    !(iisize,rimsize,densize,iisize,rimsize,densize)
 real, allocatable :: itabcolli002(:,:,:,:,:,:)
 real, allocatable :: itabcolli011(:,:,:,:,:,:)
 real, allocatable :: itabcolli012(:,:,:,:,:,:)
 real, allocatable :: itabcolli101(:,:,:,:,:,:)
 real, allocatable :: itabcolli102(:,:,:,:,:,:)
 real, allocatable :: itabcolli111(:,:,:,:,:,:)
 real, allocatable :: itabcolli112(:,:,:,:,:,:)

! integer switch for warm rain autoconversion/accretion schemes
 integer :: autoAccr_param

! number of diagnostic ice-phase hydrometeor types
 integer, public, parameter :: n_qiType = 6

! droplet spectral shape parameter for mass spectra, used for Seifert and Beheng (2001)
! warm rain autoconversion/accretion option only (autoAccr_param = 1)
 real, dimension(16) :: dnu

! lookup table values for rain shape parameter mu_r
 real, dimension(150) :: mu_r_table

! lookup table values for rain number- and mass-weighted fallspeeds and ventilation parameters
 real, dimension(300,10) :: vn_table,vm_table,revap_table

 real, parameter :: mu_i_max = 20.

 ! physical and mathematical constants
 real           :: rhosur,rhosui,ar,br,f1r,f2r,ecr,rhow,kr,kc,bimm,aimm,rin,mi0,trplpt,  &
                   eci,eri,bcn,cpw,e0,cons1,cons2,cons3,cons4,cons5,cons6,cons7,cons8,   &
                   i_rhow,qsmall,nsmall,bsmall,zsmall,zlarge,cp,g,rd,rv,ep_2,i_cp,mw,    &
                   osm,vi,epsm,rhoa,map,ma,rr,bact,i_rm1,i_rm2,sig1,nanew1,f11,f21,sig2, &
                   nanew2,f12,f22,pi,thrd,sxth,piov3,piov6,rho_rimeMin,liqfracsmall,     &
                   rho_rimeMax,i_rho_rimeMax,max_Ni,dbrk,nmltratio,minVIS,               &
                   qsmall_dry1,qsmall_dry2,                                              &
                   maxVIS,mu_i_initial,mu_r_constant,inv_Drmax,Dmin_HM,Dinit_HM,         &
                   nccnst_1,nccnst_2,nccnst_3

 integer :: n_iceCat = -1   !used for GEM interface

 contains

!==================================================================================================!

 subroutine p3_init(lookup_file_dir,nCat,trplMomI,liqfrac,model,stat,abort_on_err,dowr,  &
                    autoAccr_param_in)

!------------------------------------------------------------------------------------------!
! This subroutine initializes all physical constants and parameters needed by the P3       !
! scheme, including reading in two lookup table files and creating a third.                !
! 'P3_INIT' be called at the first model time step, prior to first call to 'P3_MAIN'.      !
!------------------------------------------------------------------------------------------!

#ifdef ECCCGEM
!  use iso_c_binding
 use rpn_comm_itf_mod
#endif

 implicit none

! Passed arguments:
 character(len=*), intent(in)             :: lookup_file_dir    ! directory of the lookup tables (model library)
 integer,          intent(in)             :: nCat               ! number of free ice categories
 logical,          intent(in)             :: trplMomI           ! .T.=3-moment / .F.=2-moment (ice)
 logical,          intent(in)             :: liqfrac            ! .T.=Fi,liq / .F.=no Fi,liq (ice)
 integer,          intent(out), optional  :: stat               ! return status of subprogram
 logical,          intent(in),  optional  :: abort_on_err       ! abort when an error is encountered [.false.]
 character(len=*), intent(in),  optional  :: model              ! driving model
 logical,          intent(in),  optional  :: dowr
 integer,          intent(in),  optional  :: autoAccr_param_in  ! switch for autoc/accr parameterization (passed from driving model)

! Local variables and parameters:
 logical, save                  :: is_init = .false.
 character(len=1024), parameter :: version_p3                    = '5.6.2+sbaut'
 character(len=1024), parameter :: version_intended_table_1_2mom = '6.9-2momI'
 character(len=1024), parameter :: version_intended_table_1_3mom = '6.9-3momI'
 character(len=1024), parameter :: version_intended_table_2      = '6.2'
 character(len=1024), parameter :: version_intended_table_3      = '1.4'

 character(len=1024)            :: version_header_table_1_2mom
 character(len=1024)            :: version_header_table_1_3mom
 character(len=1024)            :: version_header_table_2
 character(len=1024)            :: version_header_table_3
 character(len=1024)            :: lookup_file_1                   !lookup table, main
 character(len=1024)            :: lookup_file_2                   !lookup table for ice-ice interactions (for nCat>1 only)
 character(len=1024)            :: lookup_file_3                   !lookup table for outputing mu_i with 3mom
 character(len=1024)            :: dumstr,read_path
 integer                        :: i,j,ii,jj,kk,jjj,jjj2,jjjj,jjjj2,end_status,zz,procnum,istat,ierr,ll,zq
 real                           :: lamr,mu_r,dum,dm,dum1,dum2,dum3,dum4,dum5,dd,amg,vt,dia
 double precision               :: dp_dum1, dp_dum2, dp_dum3
 logical                        :: err_abort
 logical                        :: owr = .true.

!------------------------------------------------------------------------------------------!

 read_path = lookup_file_dir           ! path for lookup tables from official model library
!read_path = '/MY/LOOKUP_TABLE/PATH'   ! path for lookup tables from user-specified location
!read_path = '/fs/site5/eccc/mrd/rpnarmp/jam003/p3_lookup_tables'

 if (trplMomI) then
   lookup_file_1 = trim(read_path)//'/'//'p3_lookupTable_1.dat-v'//trim(version_intended_table_1_3mom)
 else
   lookup_file_1 = trim(read_path)//'/'//'p3_lookupTable_1.dat-v'//trim(version_intended_table_1_2mom)
 endif
 lookup_file_2 = trim(read_path)//'/'//'p3_lookupTable_2.dat-v'//trim(version_intended_table_2)
 lookup_file_3 = trim(read_path)//'/'//'p3_lookupTable_3.dat-v'//trim(version_intended_table_3)

!------------------------------------------------------------------------------------------!

 if (present(dowr)) owr=dowr

 end_status = STATUS_ERROR
 err_abort = .false.
 if (present(abort_on_err)) err_abort = abort_on_err
 if (is_init) then
    if (present(stat)) stat = STATUS_OK
    return
 endif

 n_iceCat = nCat !used for GEM interface

! mathematical/optimization constants
 pi    = 3.14159265
!pi    = acos(-1.)
 thrd  = 1./3.
 sxth  = 1./6.
 piov3 = pi*thrd
 piov6 = pi*sxth

! maximum ice number concentration (per category)
 max_Ni = 2000.e+3  !(m-3)

! switch for warm-rain (autoconversion/accretion) parameterization
! = 1 Seifert and Beheng 2001
! = 2 Khairoutdinov and Kogan 2000
! = 3 Kogan 2013
 if (present(autoAccr_param_in)) then
    autoAccr_param = autoAccr_param_in
 else
   autoAccr_param = 2
 endif

! parameters for Seifert and Beheng (2001) autoconversion/accretion
 kc     = 9.44e+9
 kr     = 5.78e+3

! specified cloud droplet concentration (m-3; used for 1-moment cloud only)
 nccnst_1 =   80.e+6  ! typical maritime value            (  80 cm-3)
 nccnst_2 =  200.e+6  ! typical mid-latitude conteinental ( 200 cm-3)
 nccnst_3 = 1000.e+6  ! polluted/urban                    (1000 cm-3)

! physical constants
 trplpt = 273.15
 cp     = 1005.
 i_cp = 1./cp
 g      = 9.816
 rd     = 287.15
 rv     = 461.51
 ep_2   = 0.622
 rhosur = 100000./(rd*trplpt)
 rhosui = 60000./(rd*253.15)
 ar     = 841.99667
 br     = 0.8
 f1r    = 0.78
 f2r    = 0.32
 ecr    = 1.
 rhow   = 1000.
 cpw    = 4218.
 i_rhow = 1./rhow  !inverse of (max.) density of liquid water
 mu_r_constant = 0.  !fixed shape parameter for mu_r

 inv_Drmax = 1./0.002  ! inverse of maximum allowed rain number-weighted mean diameter (m-1)

! limits for rime density [kg m-3]
 rho_rimeMin     =  50.
 rho_rimeMax     = 900.
 i_rho_rimeMax = 1./rho_rimeMax

! minium allowable prognostic variables
 qsmall      = 1.e-14
 qsmall_dry1 = 1.e-8
 qsmall_dry2 = 1.e-12
 nsmall      = 1.e-16
 zsmall      = 1.e-35
 bsmall      = qsmall*i_rho_rimeMax

 zlarge      = 1.

 liqfracsmall = 0.01

! Bigg (1953)
!bimm   = 100.
!aimm   = 0.66
! Barklie and Gokhale (1959)
 bimm   = 2.
 aimm   = 0.65
 rin    = 0.1e-6
 mi0    = 4.*piov3*900.*1.e-18

 eci    = 0.5
 eri    = 1.
 bcn    = 2.

! mean size for soft lambda_r limiter [microns]
 dbrk   = 600.e-6
! ratio of rain number produced to ice number loss from melting
! Note: this is not needed with the prognostic qi,liq
 nmltratio = 1.

! mu of initial ice formation by deposition nucleation (or if no ice is present for process group 1)
 mu_i_initial = 10.

! saturation pressure at T = 0 C
 e0    = polysvp1(trplpt,0)

 cons1 = piov6*rhow
 cons2 = 4.*piov3*rhow
 cons3 = 1./(cons2*(25.e-6)**3)
 cons4 = 1./(dbrk**3*pi*rhow)
 cons5 = piov6*bimm
 cons6 = piov6**2*rhow*bimm
 cons7 = 4.*piov3*rhow*(1.e-6)**3
 cons8 = 1./(cons2*(40.e-6)**3)

! aerosol/droplet activation parameters
 mw     = 0.018   ! molecular weight of water [kg/mol]
 osm    = 1.      ! osmotic potential phi_s [ ]
 vi     = 3.      ! number of ions in solution nu
 epsm   = 0.9     ! mass fraction of soluble material [ ]
 rhoa   = 1777.   ! density of (dry) aerosol [kg/m3]
 map    = 0.132   ! molecular weight of aerosol M_s [kg/mol]
 ma     = 0.0284  ! not used
 rr     = 8.3145  ! ! ideal gas constant [J/mol/K]
 bact   = vi*osm*epsm*mw*rhoa/(map*rhow) ! eq 9a of MG07, assumes beta is 0.5
! inv_bact = (map*rhow)/(vi*osm*epsm*mw*rhoa)    *** to replace /bact **

! mode 1
 i_rm1   = 2.e+7           ! inverse of aerosol mean size (m-1)
 sig1    = 2.0             ! aerosol standard deviation
 nanew1  = 300.e6          ! aerosol number mixing ratio (kg-1)
 f11     = 0.5*exp(2.5*(log(sig1))**2)
 f21     = 1. + 0.25*log(sig1)

! note: currently only set for a single mode, droplet activation code needs to
!       be modified to include the second mode
! mode 2
 i_rm2   = 7.6923076e+5    ! inverse of aerosol mean size (m-1)
 sig2    = 2.5             ! aerosol standard deviation
 nanew2  = 0.              ! aerosol number mixing ratio (kg-1)
 f12     = 0.5*exp(2.5*(log(sig2))**2)
 f22     = 1. + 0.25*log(sig2)

!Dmin_HM  = 1000.e-6       ! ice size threshold for rime-splintering (HM)
!Dinit_HM =   10.e-6       ! initial ice diameter for rime splinters

 minVIS  =  1.             ! minimum visibility  (m)
 maxVIS  = 99.e+3          ! maximum visibility  (m)

! parameters for droplet mass spectral shape, used by Seifert and Beheng (2001)
! warm rain scheme only (autoAccr_param = 1)
  dnu(1)  = -0.947
  dnu(2)  = -0.871
  dnu(3)  = -0.783
  dnu(4)  = -0.688
  dnu(5)  = -0.588
  dnu(6)  = -0.486
  dnu(7)  = -0.382
  dnu(8)  = -0.277
  dnu(9)  = -0.171
  dnu(10) = -0.064
  dnu(11) = 0.044
  dnu(12) = 0.152
  dnu(13) = 0.260
  dnu(14) = 0.369
  dnu(15) = 0.478
  dnu(16) = 0.588

!------------------------------------------------------------------------------------------!
! read in ice microphysics table

 procnum = 0

#ifdef ECCCGEM
 call rpn_comm_rank(RPN_COMM_GRID,procnum,istat)
#endif

!-- allocate LUT arrays based on configuration (must run on all PEs before zero-init and broadcast)
 if (trplMomI) then
    allocate(itab_3mom    (zsize,densize,rimsize,liqsize,isize,tabsize_3mom))
    allocate(itab_3mom_mui(zqsize,densize,rimsize,liqsize,isize,2))
    allocate(itabcoll_3mom(zsize,densize,rimsize,liqsize,isize,rcollsize,colltabsize_3mom))
 else
    allocate(itab    (densize,rimsize,liqsize,isize,tabsize))
    allocate(itabcoll(densize,rimsize,liqsize,isize,rcollsize,colltabsize))
 endif
 if (nCat > 1) then
    allocate(itabcolli001(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli002(iisize,rimsize,densize,iisize,rimsize,densize))
  if (liqfrac) then
   !the liqfrac tables are only read, broadcast, and accessed when liqfrac=.true.
    allocate(itabcolli011(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli012(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli101(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli102(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli111(iisize,rimsize,densize,iisize,rimsize,densize))
    allocate(itabcolli112(iisize,rimsize,densize,iisize,rimsize,densize))
  endif
 endif

 if (trplMomI) then
    itabcoll_3mom = 0.
 else
    itabcoll = 0.
 endif
 if (nCat>1) then
  if (liqfrac) then
    itabcolli001 = 0.
    itabcolli002 = 0.
    itabcolli011 = 0.
    itabcolli012 = 0.
    itabcolli101 = 0.
    itabcolli102 = 0.
    itabcolli111 = 0.
    itabcolli112 = 0.
  else
    itabcolli001 = 0.
    itabcolli002 = 0.
  endif
 endif

 IF_PROC0: if (procnum == 0) then

  if(owr) print*
  if(owr) print*, ' P3 microphysics: v',trim(version_p3)
  if(owr) print*, '   P3_INIT (reading/creating lookup tables)'

  TRIPLE_MOMENT_ICE: if (.not. trplMomI) then

    print*, '     Reading table 1 [',trim(version_intended_table_1_2mom),'] ...'

    open(unit=10, file=lookup_file_1, status='old', action='read')

    !-- check that table version is correct:
    !   note:  to override and use a different lookup table, simply comment out the 'return' below
    read(10,*) dumstr,version_header_table_1_2mom
    if (trim(version_intended_table_1_2mom) /= trim(version_header_table_1_2mom)) then
       if(owr) print*
       if(owr) print*, '***********   WARNING in P3_INIT   *************'
       if(owr) print*, ' Loading lookupTable_1: v',trim(version_header_table_1_2mom)
       if(owr) print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_1: ',    &
               trim(version_intended_table_1_2mom)
      !if(owr) print*, '               -- ABORTING -- '
       if(owr) print*, '************************************************'
       if(owr) print*
       global_status = STATUS_ERROR
       if (trim(model) == 'WRF' .or. trim(model) == 'CM1' .or. trim(model) == 'KIN1D') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif

    IF_OK: if (global_status /= STATUS_ERROR) then

     read(10,*)
     do jj = 1,densize
       do ii = 1,rimsize
          do ll = 1,liqsize
            do i = 1,isize
             read(10,*) dum,dum,dum,dum, itab(jj,ii,ll,i, 1),itab(jj,ii,ll,i, 2),                  &
                    itab(jj,ii,ll,i, 3),itab(jj,ii,ll,i, 4),itab(jj,ii,ll,i, 5),                   &
                    itab(jj,ii,ll,i, 6),itab(jj,ii,ll,i, 7),itab(jj,ii,ll,i, 8),                   &
                    itab(jj,ii,ll,i, 9),itab(jj,ii,ll,i,10),itab(jj,ii,ll,i,11),                   &
                    itab(jj,ii,ll,i,12),itab(jj,ii,ll,i,13),itab(jj,ii,ll,i,14),                   &
                    itab(jj,ii,ll,i,15),itab(jj,ii,ll,i,16),itab(jj,ii,ll,i,17),                   &
                    itab(jj,ii,ll,i,18),itab(jj,ii,ll,i,19),dum,dum
            enddo

         !read in table for ice-rain collection
            do i = 1,isize
               do j = 1,rcollsize
!                 read(10,*) dum,dum,dum,dum,dum,dp_dum1,dp_dum2,dum
!                 itabcoll(jj,ii,i,j,1) = sngl(dlog10(max(dp_dum1,1.d-90)))
!                 itabcoll(jj,ii,i,j,2) = sngl(dlog10(max(dp_dum2,1.d-90)))
                read(10,*) dum,dum,dum,dum, dp_dum1,dp_dum2
                itabcoll(jj,ii,ll,i,j,1) = dp_dum1
                itabcoll(jj,ii,ll,i,j,2) = dp_dum2
               enddo
            enddo
         enddo  !ll
       enddo  !ii
     enddo  !jj

    endif IF_OK
    close(10)

    if (global_status == STATUS_ERROR) then
       if (err_abort) then
          print*,'Stopping in P3 init'
          flush(6)
          stop
       endif
       return
    endif

  else ! TRIPLE_MOMENT_ICE  (the following is for trplMomI=.true.)

    print*, '     Reading table 1 [v',trim(version_intended_table_1_3mom),'] ...'

    open(unit=10,file=lookup_file_1,status='old',iostat=ierr,err=101)
 101  if (ierr.ne.0) then
         if(owr) print*,'Error opening 3-moment lookup table file '//lookup_file_1
         if(owr) print*,'Make sure this file is unzipped and then rerun the model.'
         if(owr) print*,' '
         flush(6)
         stop
      end if

    !-- check that table version is correct:
    !   note:  to override and use a different lookup table, simply comment out the 'return' below
    read(10,*) dumstr,version_header_table_1_3mom
    if (trim(version_intended_table_1_3mom) /= trim(version_header_table_1_3mom)) then
       if(owr) print*
       if(owr) print*, '***********   WARNING in P3_INIT   *************'
       if(owr) print*, ' Loading lookupTable_1: v',trim(version_header_table_1_3mom)
       if(owr) print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_1: v',    &
               trim(version_intended_table_1_3mom)
      !if(owr) print*, '               -- ABORTING -- '
       if(owr) print*, '************************************************'
       if(owr) print*
       global_status = STATUS_ERROR
       if (trim(model) == 'WRF' .or. trim(model) == 'CM1' .or. trim(model) == 'KIN1D') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif

    read(10,*)

    do zz = 1,zsize
       do jj = 1,densize
          do ii = 1,rimsize
            do ll = 1,liqsize
              do i = 1,isize
                read(10,*) dum,dum,dum,dum,dum,  itab_3mom(zz,jj,ii,ll,i, 1),itab_3mom(zz,jj,ii,ll,i, 2),     &
                     itab_3mom(zz,jj,ii,ll,i, 3),itab_3mom(zz,jj,ii,ll,i, 4),itab_3mom(zz,jj,ii,ll,i, 5),     &
                     itab_3mom(zz,jj,ii,ll,i, 6),itab_3mom(zz,jj,ii,ll,i, 7),itab_3mom(zz,jj,ii,ll,i, 8),     &
                     itab_3mom(zz,jj,ii,ll,i, 9),itab_3mom(zz,jj,ii,ll,i,10),itab_3mom(zz,jj,ii,ll,i,11),     &
                     itab_3mom(zz,jj,ii,ll,i,12),itab_3mom(zz,jj,ii,ll,i,13),itab_3mom(zz,jj,ii,ll,i,14),     &
                     itab_3mom(zz,jj,ii,ll,i,15),itab_3mom(zz,jj,ii,ll,i,16),itab_3mom(zz,jj,ii,ll,i,17),     &
                     itab_3mom(zz,jj,ii,ll,i,18),itab_3mom(zz,jj,ii,ll,i,19),itab_3mom(zz,jj,ii,ll,i,20),     &
                     itab_3mom(zz,jj,ii,ll,i,21),itab_3mom(zz,jj,ii,ll,i,22),itab_3mom(zz,jj,ii,ll,i,23),     &
                     itab_3mom(zz,jj,ii,ll,i,24),itab_3mom(zz,jj,ii,ll,i,25),itab_3mom(zz,jj,ii,ll,i,26),     &
                     itab_3mom(zz,jj,ii,ll,i,27),itab_3mom(zz,jj,ii,ll,i,28),itab_3mom(zz,jj,ii,ll,i,29),     &
                     dum,dum
               enddo
          !read in table for ice-rain collection
              do i = 1,isize
                 do j = 1,rcollsize
!                  read(10,*) dum,dum,dum,dum,dum,dp_dum1,dp_dum2
                   read(10,*) dum,dum,dum,dum, dp_dum1,dp_dum2,dp_dum3
                   itabcoll_3mom(zz,jj,ii,ll,i,j,1) = dp_dum1
                   itabcoll_3mom(zz,jj,ii,ll,i,j,2) = dp_dum2
                   itabcoll_3mom(zz,jj,ii,ll,i,j,3) = dp_dum3
                 enddo
              enddo
            enddo  !ll
          enddo  !ii
       enddo  !jj
    enddo   !zz

    close(10)

    print*, '     Reading table 3 [v',trim(version_intended_table_3),'] ...'

    open(unit=10,file=lookup_file_3,status='old',iostat=ierr,err=102)
 102  if (ierr.ne.0) then
         if(owr) print*,'Error opening 3-moment lookup table file for mu_i '//lookup_file_3
         if(owr) print*,'Make sure this file is unzipped and then rerun the model.'
         if(owr) print*,' '
         flush(6)
         stop
      end if

    !-- check that table version is correct:
    !   note:  to override and use a different lookup table, simply comment out the 'return' below
    read(10,*) dumstr,version_header_table_3
    if (trim(version_intended_table_3) /= trim(version_header_table_3)) then
       if(owr) print*
       if(owr) print*, '***********   WARNING in P3_INIT   *************'
       if(owr) print*, ' Loading lookupTable_3: v',trim(version_header_table_3)
       if(owr) print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_3: v',    &
               trim(version_intended_table_3)
      !if(owr) print*, '               -- ABORTING -- '
       if(owr) print*, '************************************************'
       if(owr) print*
       global_status = STATUS_ERROR
       if (trim(model) == 'WRF' .or. trim(model) == 'CM1' .or. trim(model) == 'KIN1D') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif

    read(10,*)

    do zq = 1,zqsize
       do jj = 1,densize
          do ii = 1,rimsize
            do ll = 1,liqsize
              do i = 1,isize
                read(10,*) dum,dum,dum,dum,dum,itab_3mom_mui(zq,jj,ii,ll,i,1),&
                     itab_3mom_mui(zq,jj,ii,ll,i,2),dum,dum
              enddo
            enddo  !ll
          enddo  !ii
       enddo  !jj
    enddo   !zq

    close(10)

  endif TRIPLE_MOMENT_ICE

  IF_NCAT: if (nCat>1) then
   ! read in ice-ice collision lookup table  (used for multicategory only)

       if(owr) print*, '     Reading table 2 [v',trim(version_intended_table_2),'] ...'
       open(unit=10,file=lookup_file_2,status='old')

       !--check that table version is correct:
       read(10,*) dumstr,version_header_table_2
       if (trim(version_intended_table_2) /= trim(version_header_table_2)) then
          if(owr) print*
          if(owr) print*, '***********   WARNING in P3_INIT   *************'
          if(owr) print*, ' Loading lookupTable_2 version: ',trim(version_header_table_2)
          if(owr) print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_2: v', &
                  trim(version_intended_table_2)
         !if(owr) print*, '               -- ABORTING -- '
          if(owr) print*, '************************************************'
          if(owr) print*
          global_status = STATUS_ERROR
          if (trim(model)=='WRF' .or. trim(model) == 'CM1' .or. trim(model)=='KIN1D') then
             print*,'Stopping in P3 init'
             stop
          endif
       endif
       IF_OKB: if (global_status /= STATUS_ERROR) then
       read(10,*)

       if (liqfrac) then
        !8-column liqfrac LT2: column-pair order (Fl1,Fl2) = (0,0),(0,1),(1,0),(1,1)
        !must match the assembly protocol in the header of create_p3_lookupTable_2.f90
        !(naming convention: see itabcolli declarations in the module header)
         do i = 1,iisize
            do jjj = 1,rimsize
               do jjjj = 1,densize
                  do ii = 1,iisize
                     do jjj2 = 1,rimsize
                        do jjjj2 = 1,densize
                           read(10,*) dum,dum,dum,dum,dum,dum,                     &
                           itabcolli001(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli002(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli011(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli012(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli101(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli102(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli111(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli112(i,jjj,jjjj,ii,jjj2,jjjj2)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
       else
         do i = 1,iisize
            do jjj = 1,rimsize
               do jjjj = 1,densize
                  do ii = 1,iisize
                     do jjj2 = 1,rimsize
                        do jjjj2 = 1,densize
                           read(10,*) dum,dum,dum,dum,dum,dum,                     &
                           itabcolli001(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                           itabcolli002(i,jjj,jjjj,ii,jjj2,jjjj2)
                        enddo
                     enddo
                  enddo
               enddo
            enddo
         enddo
       endif ! liqfrac

       endif IF_OKB

       close(unit=10)

    endif IF_NCAT

 endif IF_PROC0

#ifdef ECCCGEM
 call rpn_comm_bcast(global_status,1,RPN_COMM_INTEGER,0,RPN_COMM_GRID,istat)
#endif

 if (global_status == STATUS_ERROR) then
    if (err_abort) then
       print*,'Stopping in P3 init'
       flush(6)
       stop
    endif
    return
 endif

#ifdef ECCCGEM
 if (trplMomI) then
    call rpn_comm_bcast(itab_3mom_mui,size(itab_3mom_mui),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itab_3mom,size(itab_3mom),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcoll_3mom,size(itabcoll_3mom),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
 else
    call rpn_comm_bcast(itab,size(itab),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcoll,size(itabcoll),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
 endif

 if (nCat>1) then
  if (liqfrac) then
    call rpn_comm_bcast(itabcolli001,size(itabcolli001),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli002,size(itabcolli002),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli011,size(itabcolli011),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli012,size(itabcolli012),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli101,size(itabcolli101),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli102,size(itabcolli102),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli111,size(itabcolli111),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli112,size(itabcolli112),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
  else
    call rpn_comm_bcast(itabcolli001,size(itabcolli001),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli002,size(itabcolli002),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
  endif
 endif
#endif

!------------------------------------------------------------------------------------------!

! call generate_mur_table(mu_r_table)
! mu_r_table(:) = mu_r_constant

!.......................................................................
! Generate lookup table for rain fallspeed and ventilation parameters
! the lookup table is two dimensional as a function of number-weighted mean size
! proportional to qr/Nr and shape parameter mu_r

 if (procnum == 0) then
    if(owr) then
    print*, '     Generating table for rain parameters ...'
    end if
 endif

 mu_r_loop: do ii = 1,10

   !mu_r = real(ii-1)  ! values of mu
    mu_r = mu_r_constant

! loop over number-weighted mean size
    meansize_loop: do jj = 1,300

       if (jj.le.20) then
          dm = (real(jj)*10.-5.)*1.e-6      ! mean size [m]
       elseif (jj.gt.20) then
          dm = (real(jj-20)*30.+195.)*1.e-6 ! mean size [m]
       endif

       lamr = (mu_r+1)/dm

! do numerical integration over PSD

       dum1 = 0. ! numerator,   number-weighted fallspeed
       dum2 = 0. ! denominator, number-weighted fallspeed
       dum3 = 0. ! numerator,   mass-weighted fallspeed
       dum4 = 0. ! denominator, mass-weighted fallspeed
       dum5 = 0. ! term for ventilation factor in evap
       dd   = 2.

! loop over PSD to numerically integrate number and mass-weighted mean fallspeeds
       do kk = 1,10000

          dia = (real(kk)*dd-dd*0.5)*1.e-6  ! size bin [m]
          amg = piov6*997.*dia**3           ! mass [kg]
          amg = amg*1000.                   ! convert [kg] to [g]

         !get fallspeed as a function of size [m s-1]
          if (dia*1.e+6.le.134.43)      then
            vt = 4.5795e+3*amg**(2.*thrd)
          elseif (dia*1.e+6.lt.1511.64) then
            vt = 4.962e+1*amg**thrd
          elseif (dia*1.e+6.lt.3477.84) then
            vt = 1.732e+1*amg**sxth
          else
            vt = 9.17
          endif

         !note: factor of 4.*mu_r is non-answer changing and only needed to
         !      prevent underflow/overflow errors, same with 3.*mu_r for dum5
          dum1 = dum1 + vt*10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum2 = dum2 + 10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum3 = dum3 + vt*10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum4 = dum4 + 10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum5 = dum5 + (vt*dia)**0.5*10.**((mu_r+1.)*alog10(dia)+3.*mu_r)*exp(-lamr*dia)*dd*1.e-6

       enddo ! kk-loop (over PSD)

       dum2 = max(dum2, 1.e-30)  !to prevent divide-by-zero below
       dum4 = max(dum4, 1.e-30)  !to prevent divide-by-zero below
       dum5 = max(dum5, 1.e-30)  !to prevent log10-of-zero below

       vn_table(jj,ii)    = dum1/dum2
       vm_table(jj,ii)    = dum3/dum4
       revap_table(jj,ii) = 10.**(alog10(dum5)+(mu_r+1.)*alog10(lamr)-(3.*mu_r))

    enddo meansize_loop

 enddo mu_r_loop

!.......................................................................

 if (procnum == 0) then
    if(owr) print*, '   P3_INIT DONE.'
    if(owr) print*
 endif

 end_status = STATUS_OK
 if (present(stat)) stat = end_status
 is_init = .true.

 return

END subroutine p3_init

!==================================================================================================!
#ifndef ECCCGEM

   SUBROUTINE mp_p3_wrapper_wrfcm1( th,qv,qc,qr,qnr,th_old,qv_old,pii,p,dz,w,dt,itimestep,      &
                rainnc,rainncv,sr,snownc,snowncv,                                               &
                ids, ide, jds, jde, kds, kde ,                                                  &
                ims, ime, jms, jme, kms, kme ,                                                  &
                its, ite, jts, jte, kts, kte ,                                                  &
                diag_zdbz, diag_effc, diag_effi_ave, n_iceCat,                                  &
                qit_1, qni_1, qir_1, qib_1, model, n_diag2d, n_diag3d,                          &
                                            diag_vmi_1, diag_dmi_1, diag_rhoi_1, qzi_1, qli_1,  &
                qit_2, qni_2, qir_2, qib_2, diag_vmi_2, diag_dmi_2, diag_rhoi_2, qzi_2, qli_2,  &
                qit_3, qni_3, qir_3, qib_3, diag_vmi_3, diag_dmi_3, diag_rhoi_3, qzi_3, qli_3,  &
                qit_4, qni_4, qir_4, qib_4, diag_vmi_4, diag_dmi_4, diag_rhoi_4, qzi_4, qli_4,  &
                nc, diag2d_01, diag2d_02, diag3d_01, diag3d_02, diag3d_03,                      &
                diag_dhmax_1, diag_dhmax_2, diag_dhmax_3, diag_dhmax_4 )

  !------------------------------------------------------------------------------------------!
  ! This is the main interface for P3 microphysics scheme with the WRF and CM1 models.       !
  !                                                                                          !
  ! It takes 3D arrays (i,j,k) from the driving model and passes 2D slabs (i,k) to the main  !
  ! subroutine ('p3_main') over a j-loop.  For each slab, 'p3_main' updates the prognostic   !
  ! variables (hydrometeor variables, potential temperature, and water vapor).  The wrapper  !
  ! then recontructs the 3D arrays, updates the accumulated precipitation arrays, and        !
  ! initializes diagnostic field arrays, all passed back to the driver model.                !
  !------------------------------------------------------------------------------------------!

  !--- input:

  ! pii       --   Exner function (nondimensional pressure) (currently not used!)
  ! p         --   pressure (Pa)
  ! dz        --   height difference across vertical levels (m)
  ! w         --   vertical air velocity (m/s)
  ! dt        --   time step (s)
  ! itimestep --   integer time step counter
  ! n_iceCat  --   number of ice-phase categories

  !--- input/output:

  ! th        --   theta (K)
  ! qv        --   vapor mass mixing ratio (kg/kg)
  ! qc        --   cloud water mass mixing ratio (kg/kg)
  ! nc        --   cloud droplet number mixing ratio (#/kg)
  ! qr        --   rain mass mixing ratio (kg/kg)
  ! qnr       --   rain number mixing ratio (#/kg)
  ! qit_(x)   --   total mass mixing ratio, ice category x (kg/kg)
  ! qni_(x)   --   number mixing ratio, category x (#/kg)
  ! qir_(x)   --   rime ice mass mixing ratio category 1 (kg/kg)
  ! qib_(x)   --   ice rime volume mixing ratio category 1 (m^-3 kg^-1)

  !--- output:

  ! rainncv        --   one time step accumulated total (solid + liquid) surface precip (mm)
  ! snowncv        --   one time step accumulated surface ice precip (mm)
  ! rainnc         --   accumulated total surface precip (mm)
  ! snownc         --   accumulated surface ice precip (mm)
  ! sr             --   ice to total surface precip ratio
  ! ids...kte      --   integer domain/tile bounds
  ! diag_zdbz      --   reflectivity (dBZ)
  ! diag_effc      --   cloud droplet effective radius (m)
  ! diag_effi_ave  --   ice effective radius (weighted average) (m)
  ! diag_vmi_(x)   --   mass-weighted mean fallspeed, ice category x (m/s)
  ! diag_dmi_(x)   --   mass-weighted mean diameter , ice category x (m)
  ! diag_rhoi_(x)  --   mass-weighted mean density,   ice category x (kg/m3)

  implicit none

  !--- arguments:

   integer, intent(in) ::  ids, ide, jds, jde, kds, kde, ims, ime, jms,                       &
                           jme, kms, kme, its, ite, jts, jte, kts, kte

   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: th,qv,qc,qr,qnr,th_old,qv_old, &
                                                               diag_zdbz,diag_effc,           &
                                                               qit_1,qni_1,qir_1,qib_1
   character(len=16), intent(in) :: model
   integer, intent(in)           :: n_diag2d,n_diag3d

   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: nc
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qzi_1
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qli_1

   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qit_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qni_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qir_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qib_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qzi_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qli_2


   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qit_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qni_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qir_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qib_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qzi_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qli_3

   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qit_4
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qni_4
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qir_4
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qib_4
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qzi_4
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qli_4

   real, dimension(ims:ime, kms:kme, jms:jme), intent(out)             :: diag_effi_ave
   real, dimension(ims:ime, kms:kme, jms:jme), intent(out)             :: diag_vmi_1, diag_dmi_1, diag_rhoi_1
   real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag_vmi_2, diag_dmi_2, diag_rhoi_2
   real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag_vmi_3, diag_dmi_3, diag_rhoi_3
   real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag_vmi_4, diag_dmi_4, diag_rhoi_4
   real, dimension(ims:ime, jms:jme),          intent(out),   optional :: diag2d_01, diag2d_02
   real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag3d_01, diag3d_02, diag3d_03
!  real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag3d_04, diag3d_05, diag3d_06
!  real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag3d_07, diag3d_08, diag3d_09

   real, dimension(ims:ime, kms:kme, jms:jme), intent(out),   optional :: diag_dhmax_1, diag_dhmax_2, diag_dhmax_3, diag_dhmax_4

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in)    :: pii,p,dz,w
   real, dimension(ims:ime, jms:jme),          intent(inout) :: rainnc,rainncv,sr,snownc,snowncv
   real, intent(in)    :: dt
   integer, intent(in) :: itimestep
   integer, intent(in) :: n_iceCat

   !--- local variables/parameters:
   real, dimension(ims:ime, kms:kme) :: nc_loc,ssat
   real, dimension(ims:ime, kms:kme, n_iceCat) :: qitot,qirim,nitot,birim,diag_dmi,diag_vmi,       &
                                                  diag_rhoi,diag_effi
   real, dimension(its:ite, kts:kte, n_iceCat) :: diag_dhmax

   real, dimension(ims:ime, kms:kme,n_iceCat)  :: zitot   ! ice mixing ratio, reflectivity [m6 kg-1]
   real, dimension(ims:ime, kms:kme,n_iceCat)  :: qiliq   ! liquid mixing ratio on ice     [kg kg-1]

   real, dimension(its:ite)                    :: pcprt_liq,pcprt_sol
   real, dimension(its:ite)                    :: prt_drzl,prt_rain,prt_crys,prt_snow,     &
                                                  prt_grpl,prt_pell,prt_hail,prt_sndp,     &
                                                  prt_wsnow   ! precip-type rates (required by type diagnostics; not output)
   real                                        :: dum1,dum2,dum3,dum4,freq3Ddiag
   integer                                     :: i,k,j

   real, dimension(ims:ime,          n_diag2d) :: diag2d         ! user-defined diagnostic fields (2D)
   real, dimension(ims:ime, kms:kme, n_diag3d) :: diag3d         ! user-defined diagnostic fields (3D)

   logical                           :: log_predictNc
   logical                           :: log_3momIce
   logical                           :: log_liqFrac
   logical, parameter                :: log_scpf      = .false.  ! switch for activation of SCPF scheme
   logical, parameter                :: log_debug     = .false.  ! switch for internal real-time debug checking

   real, dimension(its:ite, kts:kte) :: cldfrac                  ! cloud fraction computed by SCPF
   real                              :: scpf_pfrac               ! precipitation fraction factor (SCPF)
   real                              :: scpf_resfact             ! model resolution factor (SCPF)
   real, parameter                   :: clbfact_dep    =  1.     ! calibration factor for deposition
   real, parameter                   :: clbfact_sub    =  1.     ! calibration factor for sublimation
   real, parameter                   :: freq3Ddiag_wrf = 60.     ! frequency (min) for full-column diagnostics
   real, parameter                   :: freq3Ddiag_cm1 =  0.     ! frequency (min) for full-column diagnostics (0: every step)

   !------------------------------------------------------------------------------------------!

   log_predictNc = present(nc)
   log_3momIce   = present(qzi_1)
   log_liqFrac   = present(qli_1)

   if (trim(model)=='WRF') then
      freq3Ddiag = freq3Ddiag_wrf
   elseif (trim(model)=='CM1') then
      freq3Ddiag = freq3Ddiag_cm1
   else
      freq3Ddiag = 0.
   endif

   scpf_pfrac   = 0.  ! SCPF currently not used in WRF/CM1
   scpf_resfact = 0.  ! SCPF currently not used in WRF/CM1

   j_loop: do j = jts,jte      ! j loop (north-south)

      if (log_predictNc) then
         nc_loc(:,:) = nc(:,:,j)
      else
         nc_loc(:,:) = 0.
      endif

      ssat = 0.  ! note: code for prediction of ssat not currently avaiable

    ! contruct full ice arrays (with dimension n_iceCat) from individual ice category arrays:
      qitot(:,:,1) = qit_1(:,:,j)
      qirim(:,:,1) = qir_1(:,:,j)
      nitot(:,:,1) = qni_1(:,:,j)
      birim(:,:,1) = qib_1(:,:,j)
      if (log_3momIce) zitot(:,:,1) = qzi_1(:,:,j)
      if (log_liqFrac) qiliq(:,:,1) = qli_1(:,:,j)

      if (n_iceCat.ge.2) then
         qitot(:,:,2) = qit_2(:,:,j)
         qirim(:,:,2) = qir_2(:,:,j)
         nitot(:,:,2) = qni_2(:,:,j)
         birim(:,:,2) = qib_2(:,:,j)
         if (log_3momIce) zitot(:,:,2) = qzi_2(:,:,j)
         if (log_liqFrac) qiliq(:,:,2) = qli_2(:,:,j)

         if (n_iceCat.ge.3) then
            qitot(:,:,3) = qit_3(:,:,j)
            qirim(:,:,3) = qir_3(:,:,j)
            nitot(:,:,3) = qni_3(:,:,j)
            birim(:,:,3) = qib_3(:,:,j)
            if (log_3momIce) zitot(:,:,3) = qzi_3(:,:,j)
            if (log_liqFrac) qiliq(:,:,3) = qli_3(:,:,j)

            if (n_iceCat.ge.4) then
               qitot(:,:,4) = qit_4(:,:,j)
               qirim(:,:,4) = qir_4(:,:,j)
               nitot(:,:,4) = qni_4(:,:,j)
               birim(:,:,4) = qib_4(:,:,j)
               if (log_3momIce) zitot(:,:,4) = qzi_4(:,:,j)
               if (log_liqFrac) qiliq(:,:,4) = qli_4(:,:,j)
            endif  ! >=4
         endif  ! >=3
      endif  ! >=2

      if (.not. log_3momIce) zitot = 0.  !not used, but avoids passing uninialized values
      if (.not. log_liqFrac) qiliq = 0.  !not used, but avoids passing uninialized values

      call p3_main( qc(its:ite,kts:kte,j),nc_loc(its:ite,kts:kte),qr(its:ite,kts:kte,j),               &
                      qnr(its:ite,kts:kte,j),th_old(its:ite,kts:kte,j),th(its:ite,kts:kte,j),          &
                      qv_old(its:ite,kts:kte,j),qv(its:ite,kts:kte,j),dt,                              &
                      qitot(its:ite,kts:kte,1:n_iceCat),qirim(its:ite,kts:kte,1:n_iceCat),             &
                      qiliq(its:ite,kts:kte,1:n_iceCat),nitot(its:ite,kts:kte,1:n_iceCat),             &
                      birim(its:ite,kts:kte,1:n_iceCat),zitot(its:ite,kts:kte,1:n_iceCat),             &
                      ssat(its:ite,kts:kte),w(its:ite,kts:kte,j),p(its:ite,kts:kte,j),                 &
                      dz(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,             &
                      n_iceCat,diag_zdbz(its:ite,kts:kte,j),diag_effc(its:ite,kts:kte,j),              &
                      diag_effi(its:ite,kts:kte,1:n_iceCat),diag_vmi(its:ite,kts:kte,1:n_iceCat),      &
                      diag_dmi(its:ite,kts:kte,1:n_iceCat),diag_rhoi(its:ite,kts:kte,1:n_iceCat),      &
                      n_diag2d,diag2d(its:ite,1:n_diag2d),n_diag3d,diag3d(its:ite,kts:kte,1:n_diag3d), &
                      log_predictNc,trim(model),clbfact_dep,clbfact_sub,log_debug,log_scpf,            &
                      scpf_pfrac,scpf_resfact,cldfrac,                                                 &
                      log_3momentIce = log_3momIce,                                                    &
                      log_LiquidFrac = log_liqFrac,                                                    &
                      prt_drzl = prt_drzl, prt_rain = prt_rain, prt_crys = prt_crys,                   &
                      prt_snow = prt_snow, prt_grpl = prt_grpl, prt_pell = prt_pell,                   &
                      prt_hail = prt_hail, prt_sndp = prt_sndp, prt_wsnow = prt_wsnow,                 &
                      diag_dhmax     = diag_dhmax,                                                     &
                      freq3Ddiag_in  = freq3Ddiag)

     !surface precipitation output:
      dum1 = 1000.*dt     ! to convert rates from mm/s to mm/time step
      rainncv(its:ite,j) = (pcprt_liq(:) + pcprt_sol(:))*dum1      ! total (liquid + solid) precip "rate" (accumulation per time step)
      snowncv(its:ite,j) = pcprt_sol(:)*dum1                       ! solid (only) precip "rate" (accumulation per time step)
      rainnc(its:ite,j)  = rainnc(its:ite,j) + rainncv(its:ite,j)  ! accumulated (entire integration) total precipitation
      snownc(its:ite,j)  = snownc(its:ite,j) + snowncv(its:ite,j)  ! accumulated (entire integration) solid precipitation
      sr(its:ite,j)      = pcprt_sol(:)/(pcprt_liq(:)+pcprt_sol(:)+1.e-12)         ! solid-to-total ratio

      if (log_predictNc) nc(:,:,j) = nc_loc(:,:)

    !set background effective radii (i.e. with no explicit condensate) to prescribed values:
    !  where (qc(:,:,j) < 1.e-14) diag_effc(:,:,j) = 10.e-6
    !  where (qitot < 1.e-14) diag_effi = 25.e-6

    ! decompose full ice arrays (with dimension n_iceCat) into individual ice category arrays:
      qit_1(:,:,j) = qitot(:,:,1)
      qir_1(:,:,j) = qirim(:,:,1)
      qni_1(:,:,j) = nitot(:,:,1)
      qib_1(:,:,j) = birim(:,:,1)
      diag_vmi_1(:,:,j)  = diag_vmi(:,:,1)
      diag_dmi_1(:,:,j)  = diag_dmi(:,:,1)
      diag_rhoi_1(:,:,j) = diag_rhoi(:,:,1)
      if (log_3momIce) qzi_1(:,:,j) = zitot(:,:,1)
      if (log_liqFrac) qli_1(:,:,j) = qiliq(:,:,1)
      if (present(diag_dhmax_1)) diag_dhmax_1(:,:,j) = diag_dhmax(:,:,1)

      if (n_iceCat.ge.2) then
         qit_2(:,:,j) = qitot(:,:,2)
         qir_2(:,:,j) = qirim(:,:,2)
         qni_2(:,:,j) = nitot(:,:,2)
         qib_2(:,:,j) = birim(:,:,2)
         diag_vmi_2(:,:,j)  = diag_vmi(:,:,2)
         diag_dmi_2(:,:,j)  = diag_dmi(:,:,2)
         diag_rhoi_2(:,:,j) = diag_rhoi(:,:,2)
         if (log_3momIce) qzi_2(:,:,j) = zitot(:,:,2)
         if (log_liqFrac) qli_2(:,:,j) = qiliq(:,:,2)
         if (present(diag_dhmax_2)) diag_dhmax_2(:,:,j) = diag_dhmax(:,:,2)

         if (n_iceCat.ge.3) then
            qit_3(:,:,j) = qitot(:,:,3)
            qir_3(:,:,j) = qirim(:,:,3)
            qni_3(:,:,j) = nitot(:,:,3)
            qib_3(:,:,j) = birim(:,:,3)
            diag_vmi_3(:,:,j)  = diag_vmi(:,:,3)
            diag_dmi_3(:,:,j)  = diag_dmi(:,:,3)
            diag_rhoi_3(:,:,j) = diag_rhoi(:,:,3)
            if (log_3momIce) qzi_3(:,:,j) = zitot(:,:,3)
            if (log_liqFrac) qli_3(:,:,j) = qiliq(:,:,3)
            if (present(diag_dhmax_3)) diag_dhmax_3(:,:,j) = diag_dhmax(:,:,3)

            if (n_iceCat.ge.4) then
               qit_4(:,:,j) = qitot(:,:,4)
               qir_4(:,:,j) = qirim(:,:,4)
               qni_4(:,:,j) = nitot(:,:,4)
               qib_4(:,:,j) = birim(:,:,4)
               diag_vmi_4(:,:,j)  = diag_vmi(:,:,4)
               diag_dmi_4(:,:,j)  = diag_dmi(:,:,4)
               diag_rhoi_4(:,:,j) = diag_rhoi(:,:,4)
               if (log_3momIce) qzi_4(:,:,j) = zitot(:,:,4)
               if (log_liqFrac) qli_4(:,:,j) = qiliq(:,:,4)
               if (present(diag_dhmax_4)) diag_dhmax_4(:,:,j) = diag_dhmax(:,:,4)
            endif  ! >=4
         endif ! >=3
      endif ! >=2


     !Compute single mass-and-projected area-weighted effective radius of ice
      do i=its,ite
         do k=kts,kte

            dum1 = 0.
            dum2 = 0.
            dum3 = 0.
            dum4 = 0.
            diag_effi_ave(i,k,j) = 25.e-6  ! set to default 25 microns

            if (n_iceCat.ge.2) then
               if (qitot(i,k,1).ge.qsmall) dum1 = qitot(i,k,1)/diag_effi(i,k,1)
               if (qitot(i,k,2).ge.qsmall) dum2 = qitot(i,k,2)/diag_effi(i,k,2)
               if (n_iceCat.ge.3) then
                  if (qitot(i,k,3).ge.qsmall) dum3 = qitot(i,k,3)/diag_effi(i,k,3)
                  if (n_iceCat.ge.4) then
                     if (qitot(i,k,4).ge.qsmall) dum4 = qitot(i,k,4)/diag_effi(i,k,4)
                  endif
               endif
            endif

            select case (n_iceCat)
               case (1)
                  diag_effi_ave(i,k,j) = diag_effi(i,k,1)
               case (2)
                  diag_effi_ave(i,k,j) = (qitot(i,k,1)+qitot(i,k,2))/(dum1+dum2)
               case (3)
                  diag_effi_ave(i,k,j) = (qitot(i,k,1)+qitot(i,k,2)+qitot(i,k,3))/(dum1+dum2+dum3)
               case (4)
                  diag_effi_ave(i,k,j) = (qitot(i,k,1)+qitot(i,k,2)+qitot(i,k,3)+qitot(i,k,4))/(dum1+dum2+dum3+dum4)
            end select

         enddo  !k-loop
      enddo   !i-loop


    ! copy generic output arrays (from p3_main) to local arrays (passed back to wrapper)
      if (present(diag2d_01))  diag2d_01(:,j)    = diag2d(:,1)
      if (present(diag2d_02))  diag2d_02(:,j)    = diag2d(:,2)
      if (present(diag3d_01))  diag3d_01(:,:,j)  = diag3d(:,:,1)
      if (present(diag3d_02))  diag3d_02(:,:,j)  = diag3d(:,:,2)
      if (present(diag3d_03))  diag3d_03(:,:,j)  = diag3d(:,:,3)
!       if (present(diag3d_04))  diag3d_04(:,:,j)  = diag3d(:,:,4)
!       if (present(diag3d_05))  diag3d_05(:,:,j)  = diag3d(:,:,5)
!       if (present(diag3d_06))  diag3d_06(:,:,j)  = diag3d(:,:,6)
!       if (present(diag3d_07))  diag3d_07(:,:,j)  = diag3d(:,:,7)
!       if (present(diag3d_08))  diag3d_08(:,:,j)  = diag3d(:,:,8)
!       if (present(diag3d_09))  diag3d_09(:,:,j)  = diag3d(:,:,9)
!       if (present(diag3d_10))  diag3d_10(:,:,j)  = diag3d(:,:,10)

   enddo j_loop

   if (global_status /= STATUS_OK) then
      print*,'Stopping in P3, problem in P3 main'
      stop
   endif

   END SUBROUTINE mp_p3_wrapper_wrfcm1

#endif

!==================================================================================================!
#ifdef ECCCGEM

 function mp_p3_wrapper_gem(ttend,qtend,qctend,qrtend,qitend,                                        &
                              qvap_m,qvap,temp_m,temp,dt,dt_max,ww,psfc,gztherm,gzmom,sigma,kount,   &
                              ni,nk,prt_liq,prt_sol,prt_drzl,prt_rain,prt_crys,prt_snow,             &
                              prt_grpl,prt_pell,prt_hail,prt_sndp,prt_wsnow,diag_Zet,diag_Zec,       &
                              diag_effc,qc_m,qc,nc,qr_m,qr,nr,n_diag_2d,diag_2d,n_diag_3d,diag_3d,   &
                              clbfact_dep,clbfact_sub,debug_on,supdepthr,diag_hcb,diag_hsn,          &
                              diag_vis,diag_vis1,diag_vis2,diag_vis3,diag_slw,                       &
                              scpf_on,scpf_pfrac,scpf_resfact,cldfrac,freq3Ddiag_gem,maxD_hail,      &
                              qi_type_1,qi_type_2,qi_type_3,qi_type_4,qi_type_5,qi_type_6,           &
                              qitot_1m,qitot_1,qirim_1,nitot_1,birim_1,diag_effi_1,zitot_1,qiliq_1,  &
                              qitot_2m,qitot_2,qirim_2,nitot_2,birim_2,diag_effi_2,zitot_2,qiliq_2,  &
                              qitot_3m,qitot_3,qirim_3,nitot_3,birim_3,diag_effi_3,zitot_3,qiliq_3,  &
                              qitot_4m,qitot_4,qirim_4,nitot_4,birim_4,diag_effi_4,zitot_4,qiliq_4)  &
                              result(end_status)

!------------------------------------------------------------------------------------------!
! This wrapper subroutine is the main GEM interface with the P3 microphysics scheme.  It   !
! prepares some necessary fields (converts temperature to potential temperature, etc.),    !
! passes 2D slabs (i,k) to the main microphysics subroutine ('P3_MAIN') -- which updates   !
! the prognostic variables (hydrometeor variables, temperature, and water vapor) and       !
! computes various diagnostics fields (precipitation rates, reflectivity, etc.) -- and     !
! finally converts the updated potential temperature to temperature.                       !
!------------------------------------------------------------------------------------------!

 use phy_status, only: physeterror

 implicit none

!----- input/ouput arguments:  ------------------------------------------------------------!

 integer, intent(in)                    :: ni                    ! number of columns in slab           -
 integer, intent(in)                    :: nk                    ! number of vertical levels           -
!integer, intent(in)                    :: n_iceCat              ! number of ice categories            -
 integer, intent(in)                    :: kount                 ! time step counter                   -
 integer, intent(in)                    :: n_diag_2d             ! number of 2D diagnostic fields      -
 integer, intent(in)                    :: n_diag_3d             ! number of 3D diagnostic fields      -

 real, intent(in)                       :: dt                    ! model time step                     s
 real, intent(in)                       :: dt_max                ! maximum timestep for microphysics   s
 real, intent(in)                       :: clbfact_dep           ! calibration factor for deposition
 real, intent(in)                       :: clbfact_sub           ! calibration factor for sublimation
 real, intent(in)                       :: supdepthr             ! ice supersaturation threshold for deposition

 real, intent(inout), dimension(ni,nk)  :: qc                    ! cloud specific ratio, mass            kg kg-1
 real, intent(inout), dimension(ni,nk)  :: nc                    ! cloud specific ratio, number          #  kg-1
 real, intent(inout), dimension(ni,nk)  :: qr                    ! rain  specific ratio, mass            kg kg-1
 real, intent(inout), dimension(ni,nk)  :: nr                    ! rain  specific ratio, number          #  kg-1
 real, intent(in),    dimension(ni,nk)  :: qc_m                  ! cloud specific ratio, mass t-         kg kg-1
 real, intent(in),    dimension(ni,nk)  :: qr_m                  ! rain  specific ratio, mass t-         kg kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_1           ! ice   specific ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qitot_1m          ! ice   specific ratio, mass (t-)       kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_1           ! ice   specific ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_1           ! ice   specific ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_1           ! ice   specific ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_1       ! ice   effective radius, (cat 1)       m
 real, dimension(:,:), pointer, contiguous  :: zitot_1           ! ice   specific ratio, reflectivity    m^6 kg-1
 real, dimension(:,:), pointer, contiguous  :: qiliq_1           ! ice   specific ratio, mass (liquid)   kg kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_2           ! ice   specific ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qitot_2m          ! ice   specific ratio, mass (t-)       kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_2           ! ice   specific ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_2           ! ice   specific ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_2           ! ice   specific ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_2       ! ice   effective radius, (cat 2)       m
 real, dimension(:,:), pointer, contiguous  :: zitot_2           ! ice   specific ratio, reflectivity    m^6 kg-1
 real, dimension(:,:), pointer, contiguous  :: qiliq_2           ! ice   specific ratio, mass (liquid)   kg kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_3           ! ice   specific ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qitot_3m          ! ice   specific ratio, mass (t-)       kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_3           ! ice   specific ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_3           ! ice   specific ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_3           ! ice   specific ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_3       ! ice   effective radius,  (cat 3)      m
 real, dimension(:,:), pointer, contiguous  :: zitot_3           ! ice   specific ratio, reflectivity    m^6 kg-1
 real, dimension(:,:), pointer, contiguous  :: qiliq_3           ! ice   specific ratio, mass (liquid)   kg kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_4           ! ice   specific ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qitot_4m          ! ice   specific ratio, mass (t-)       kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_4           ! ice   specific ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_4           ! ice   specific ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_4           ! ice   specific ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_4       ! ice   effective radius, (cat 4)       m
 real, dimension(:,:), pointer, contiguous  :: zitot_4           ! ice   specific ratio, reflectivity    m^6 kg-1
 real, dimension(:,:), pointer, contiguous  :: qiliq_4           ! ice   specific ratio, mass (liquid)   kg kg-1

 real, intent(out), dimension(ni,nk)    :: ttend                 ! temperature tendency                K s-1
 real, intent(out), dimension(ni,nk)    :: qtend                 ! moisture tendency                   kg kg-1 s-1
 real, intent(out), dimension(ni,nk)    :: qctend                ! cloud water tendency                kg kg-1 s-1
 real, intent(out), dimension(ni,nk)    :: qrtend                ! cloud water tendency                kg kg-1 s-1
 real, intent(out), dimension(ni,nk)    :: qitend                ! total ice tendency                  kg kg-1 s-1

 real, intent(in),    dimension(ni,nk)  :: qvap_m                ! vapor mixing ratio (previous time)  kg kg-1
 real, intent(inout), dimension(ni,nk)  :: qvap                  ! vapor mixing ratio, mass            kg kg-1
 real, intent(in),    dimension(ni,nk)  :: temp_m                ! temperature (previous time step)    K
 real, intent(inout), dimension(ni,nk)  :: temp                  ! temperature                         K
 real, intent(in),    dimension(ni)     :: psfc                  ! surface air pressure                Pa
 real, intent(in),    dimension(ni,nk)  :: gztherm               ! height AGL of thermodynamic levels  m
 real, intent(in),    dimension(ni,nk)  :: gzmom                 ! height AGL of momentum levels       m
 real, intent(in),    dimension(ni,nk)  :: sigma                 ! sigma = p(k,:)/psfc(:)              -
 real, intent(in),    dimension(ni,nk)  :: ww                    ! vertical motion                     m s-1
 real, intent(out),   dimension(ni)     :: prt_liq               ! precipitation rate, total liquid    m s-1
 real, intent(out),   dimension(ni)     :: prt_sol               ! precipitation rate, total solid     m s-1
 real, intent(out),   dimension(ni)     :: prt_drzl              ! precipitation rate, drizzle         m s-1
 real, intent(out),   dimension(ni)     :: prt_rain              ! precipitation rate, rain            m s-1
 real, intent(out),   dimension(ni)     :: prt_crys              ! precipitation rate, ice cystals     m s-1
 real, intent(out),   dimension(ni)     :: prt_snow              ! precipitation rate, snow            m s-1
 real, intent(out),   dimension(ni)     :: prt_grpl              ! precipitation rate, graupel         m s-1
 real, intent(out),   dimension(ni)     :: prt_pell              ! precipitation rate, ice pellets     m s-1
 real, intent(out),   dimension(ni)     :: prt_hail              ! precipitation rate, hail            m s-1
 real, intent(out),   dimension(ni)     :: prt_wsnow             ! precipitation rate, wet snow        m s-1
 real, intent(out),   dimension(ni)     :: prt_sndp              ! precipitation rate, unmelted snow   m s-1
 real, intent(out),   dimension(ni,nk)  :: diag_Zet              ! equivalent reflectivity, 3D         dBZ
 real, intent(out),   dimension(ni)     :: diag_Zec              ! equivalent reflectivity, col-max    dBZ
 real, intent(out),   dimension(ni,nk)  :: diag_effc             ! effective radius, cloud             m
 real, intent(out),   dimension(ni,n_diag_2d)    :: diag_2d      ! user-defined 2D diagnostic fields
 real, intent(out),   dimension(ni,nk,n_diag_3d) :: diag_3d      ! user-defined 3D diagnostic fields
!real, intent(out),   dimension(ni,nk,n_qiType  ):: qi_type      ! mass mixing ratio, diag ice type    kg kg-1

 real, intent(out),   dimension(ni,nk)  :: qi_type_1             ! small ice crystal mass              kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_2             ! unrimed snow crystal mass           kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_3             ! lightly rimed snow mass             kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_4             ! graupel mass                        kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_5             ! hail mass                           kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_6             ! ice pellet mass                     kg kg-1

 real, intent(out),   dimension(ni,nk)  :: maxD_hail             ! ice, maximum hail size (all cat)    m

 real, intent(out),   dimension(ni)     :: diag_hcb              ! height of cloud base                m
 real, intent(out),   dimension(ni)     :: diag_hsn              ! height of snow level                m
 real, intent(out),   dimension(ni,nk)  :: diag_vis              ! visibility (total)                  m
 real, intent(out),   dimension(ni,nk)  :: diag_vis1             ! visibility through liquid fog       m
 real, intent(out),   dimension(ni,nk)  :: diag_vis2             ! visibility through rain             m
 real, intent(out),   dimension(ni,nk)  :: diag_vis3             ! visibility through snow             m
 real, intent(out),   dimension(ni,nk)  :: diag_slw              ! supercooled LWC                     kg m-3

 logical, intent(in)                    :: debug_on              ! logical switch for internal debug checks
 logical, intent(in)                    :: scpf_on               ! switch for activation of SCPF scheme
 real,    intent(in)                    :: scpf_pfrac            ! precipitation fraction factor (SCPF)
 real,    intent(in)                    :: scpf_resfact          ! model resolution factor (SCPF)
 real,    intent(in)                    :: freq3Ddiag_gem        ! frequency (min) for full-column diagnostics
 real,    intent(out), dimension(ni,nk) :: cldfrac               ! cloud fraction computed by SCPF

!----------------------------------------------------------------------------------------!

!----- local variables and parameters:
 real, dimension(ni,nk,n_iceCat)  :: qitot      ! ice mixing ratio, mass (total)          kg kg-1
 real, dimension(ni,nk,n_iceCat)  :: qirim      ! ice mixing ratio, mass (rime)           kg kg-1
 real, dimension(ni,nk,n_iceCat)  :: qiliq      ! ice mixing ratio, mass (liquid)         kg kg-1
 real, dimension(ni,nk,n_iceCat)  :: nitot      ! ice mixing ratio, number                #  kg-1
 real, dimension(ni,nk,n_iceCat)  :: birim      ! ice mixing ratio, volume                m3 kg-1
 real, dimension(ni,nk,n_iceCat)  :: zitot      ! ice mixing ratio, reflectivity          m6 kg-1
 real, dimension(ni,nk,n_iceCat)  :: diag_effi  ! effective radius, ice                   m
 real, dimension(ni,nk,n_iceCat)  :: diag_vmi   ! mass-weighted fall speed, ice           m s-1  (returned but not used)
 real, dimension(ni,nk,n_iceCat)  :: diag_di    ! mean diameter, ice                      m      (returned but not used)
 real, dimension(ni,nk,n_iceCat)  :: diag_rhoi  ! bulk density, ice                       kg m-3 (returned but not used)
 real, dimension(ni,nk,n_iceCat)  :: diag_dhmax ! maximum hail size, ice                  m

 real, dimension(ni,nk)  :: theta_m             ! potential temperature (previous step)   K
 real, dimension(ni,nk)  :: qvapm               ! qv (previous step)                      kg kg-1
 real, dimension(ni,nk)  :: qvapm1              ! qv (specific previous step)             kg kg-1
 real, dimension(ni,nk)  :: theta               ! potential temperature                   K
 real, dimension(ni,nk)  :: pres                ! pressure                                Pa
 real, dimension(ni,nk)  :: DZ                  ! difference in height between levels     m
 real, dimension(ni,nk)  :: ssat                ! supersaturation
 real, dimension(ni,nk)  :: tmparr_ik           ! temporary array (for optimization)
 real, dimension(ni,nk)  :: qqdelta,ttdelta     ! for sub_stepping
 real, dimension(ni,nk)  :: iwc                 ! total ice water content
 real, dimension(ni,nk)  :: temp0, qvap0, qc0, qr0, iwc0 ! incoming state variables
 real, dimension(ni,nk)  :: totmassm            ! total mass specific/ratio t-            kg kg-1
 real, dimension(ni,nk)  :: totmass             ! total mass specific/ratio t*            kg kg-1
 real, dimension(ni,nk)  :: totmass_mom         ! totmass on momentum levels              kg kg-1
 real, dimension(ni,nk)  :: inv_totmassm        ! total mass specific/ratio t-            kg kg-1
 real, dimension(ni,nk)  :: inv_totmass         ! total mass specific/ratio t*            kg kg-1

 real, dimension(ni,nk,n_qiType) :: qi_type     ! diagnostic precipitation types

 real, dimension(ni)     :: prt_liq_ave,prt_sol_ave,rn1_ave,rn2_ave,sn1_ave, &  ! ave pcp rates over full model timestep
                            sn2_ave,sn3_ave,pe1_ave,pe2_ave,snd_ave,ws_ave
 real                    :: dt_mp                                               ! timestep used by microphsyics (for substepping)
 real                    :: tmp1, idt

 integer                 :: i,k,ktop,kbot,kdir,i_strt,k_strt,i_substep,n_substep,end_status,tmpint1

 logical                 :: log_tmp1,log_tmp2,log_trplMomI,log_liqFrac
 logical, parameter      :: log_predictNc  = .true.     ! temporary; to be put as GEM namelist
 real, parameter         :: SMALL_ICE_MASS = 1e-14      ! threshold for very small specific ice content
!real, parameter         :: freq3Ddiag_gem = 60.        ! frequency (min) for full-column diagnostics

 character(len=16), parameter :: model = 'GEM'

!----------------------------------------------------------------------------------------!

   end_status = STATUS_ERROR

   i_strt = 1  ! beginning index of slab
   k_strt = 1  ! beginning index of column

   ktop  = 1   ! k index of top level
   kbot  = nk  ! k index of bottom level
   kdir  = -1  ! direction of vertical leveling for 1=bottom, nk=top

   log_trplMomI = associated(zitot_1)
   log_liqFrac  = associated(qiliq_1)

   !compute time step and number of steps for substepping
   idt = 1./dt
   n_substep = int((dt-0.1)/max(0.1,dt_max)) + 1
   dt_mp = dt/float(n_substep)

   ! Save initial state for tendency calculation and reset (in specific ratios)
   temp0(:,:) = temp(:,:)
   qvap0(:,:) = qvap(:,:)
   qc0(:,:) = qc(:,:)
   qr0(:,:) = qr(:,:)
   iwc0(:,:) = qitot_1(:,:)
   if (n_iceCat > 1) iwc0(:,:) = iwc0(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) iwc0(:,:) = iwc0(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) iwc0(:,:) = iwc0(:,:) + qitot_4(:,:)

   ! Transform every specific mass to mixing ratio
   ! Total sum at t-
   totmassm(:,:) = qvap_m(:,:)+qr_m(:,:)+qc_m(:,:)+qitot_1m(:,:)
   if (n_iceCat > 1) totmassm(:,:) = totmassm(:,:) + qitot_2m(:,:)
   if (n_iceCat > 2) totmassm(:,:) = totmassm(:,:) + qitot_3m(:,:)
   if (n_iceCat > 3) totmassm(:,:) = totmassm(:,:) + qitot_4m(:,:)
   inv_totmassm(:,:) = 1./(1.-totmassm(:,:))
   ! Total sum at t*
   totmass(:,:) = qvap(:,:)+qr(:,:)+qc(:,:)+qitot_1(:,:)
   if (n_iceCat > 1) totmass(:,:) = totmass(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) totmass(:,:) = totmass(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) totmass(:,:) = totmass(:,:) + qitot_4(:,:)
   inv_totmass(:,:) = 1./(1.-totmass(:,:))
   ! Water vapour:
   qvap(:,:) = qvap(:,:)*inv_totmass(:,:)
   qvapm1(:,:) = qvap_m(:,:)*inv_totmassm(:,:)
   ! Cloud water:
   qc(:,:) = qc(:,:)*inv_totmass(:,:)
   nc(:,:) = nc(:,:)*inv_totmass(:,:)
   ! Rain water:
   qr(:,:) = qr(:,:)*inv_totmass(:,:)
   nr(:,:) = nr(:,:)*inv_totmass(:,:)
   ! Ice:
   qitot_1(:,:) = qitot_1(:,:)*inv_totmass(:,:)
   qirim_1(:,:) = qirim_1(:,:)*inv_totmass(:,:)
   nitot_1(:,:) = nitot_1(:,:)*inv_totmass(:,:)
   birim_1(:,:) = birim_1(:,:)*inv_totmass(:,:)
   if (associated(zitot_1)) zitot_1(:,:) = zitot_1(:,:)*inv_totmass(:,:)
   if (associated(qiliq_1)) qiliq_1(:,:) = qiliq_1(:,:)*inv_totmass(:,:)
   if (n_iceCat >= 2) then
      qitot_2(:,:) = qitot_2(:,:)*inv_totmass(:,:)
      qirim_2(:,:) = qirim_2(:,:)*inv_totmass(:,:)
      nitot_2(:,:) = nitot_2(:,:)*inv_totmass(:,:)
      birim_2(:,:) = birim_2(:,:)*inv_totmass(:,:)
      if (associated(zitot_2)) zitot_2(:,:) = zitot_2(:,:)*inv_totmass(:,:)
      if (associated(qiliq_2)) qiliq_2(:,:) = qiliq_2(:,:)*inv_totmass(:,:)
      if (n_iceCat >= 3) then
         qitot_3(:,:) = qitot_3(:,:)*inv_totmass(:,:)
         qirim_3(:,:) = qirim_3(:,:)*inv_totmass(:,:)
         nitot_3(:,:) = nitot_3(:,:)*inv_totmass(:,:)
         birim_3(:,:) = birim_3(:,:)*inv_totmass(:,:)
         if (associated(zitot_3)) zitot_3(:,:) = zitot_3(:,:)*inv_totmass(:,:)
         if (associated(qiliq_3)) qiliq_3(:,:) = qiliq_3(:,:)*inv_totmass(:,:)
         if (n_iceCat >= 4) then
            qitot_4(:,:) = qitot_4(:,:)*inv_totmass(:,:)
            qirim_4(:,:) = qirim_4(:,:)*inv_totmass(:,:)
            nitot_4(:,:) = nitot_4(:,:)*inv_totmass(:,:)
            birim_4(:,:) = birim_4(:,:)*inv_totmass(:,:)
            if (associated(zitot_4)) zitot_4(:,:) = zitot_4(:,:)*inv_totmass(:,:)
            if (associated(qiliq_4)) qiliq_4(:,:) = qiliq_4(:,:)*inv_totmass(:,:)
         endif
      endif
   endif

   ! All variables are in mixing ratios
   ! External forcings are distributed evenly over steps
   qqdelta = (qvap-qvapm1) / float(n_substep)
   ttdelta = (temp-temp_m) / float(n_substep)
   ! initialise for the 1st substepping
   qvap = qvapm1
   temp = temp_m

  !if (kount == 0) then
   if (.false.) then
      print*,'Microphysics (MP) substepping:'
      print*,'  GEM model time step  : ',dt
      print*,'  MP time step         : ',dt_mp
      print*,'  number of MP substeps: ',n_substep
   endif

 ! note: code for prediction of ssat not currently avaiable, thus array is to 0
   ssat = 0.

  !air pressure:
   do k = kbot,ktop,kdir
      pres(:,k)= psfc(:)*sigma(:,k)
   enddo

  !layer thickness (for sedimentation):
  ! do k = kbot,ktop-kdir,kdir
  !    DZ(:,k) = gztherm(:,k+kdir) - gztherm(:,k)
  ! enddo
  ! DZ(:,ktop) = DZ(:,ktop-kdir)

  !layer thickness (for sedimentation):
  !  note: This is the thickness of the layer "centered" at thermodynamic level k,
  !        computed based on the surrounding momentum levels.
   do k = kbot-1,ktop,kdir
      DZ(:,k) = gzmom(:,k) - gzmom(:,k-kdir)
   enddo
   DZ(:,kbot) = gzmom(:,kbot)

  !construct full ice arrays from individual category arrays:
   qitot(:,:,1) = qitot_1(:,:)
   qirim(:,:,1) = qirim_1(:,:)
   nitot(:,:,1) = nitot_1(:,:)
   birim(:,:,1) = birim_1(:,:)
   diag_effi(:,:,1) = diag_effi_1(:,:)
   if (associated(zitot_1)) zitot(:,:,1) = zitot_1(:,:)
   if (associated(qiliq_1)) qiliq(:,:,1) = qiliq_1(:,:)

   if (n_iceCat >= 2) then
      qitot(:,:,2) = qitot_2(:,:)
      qirim(:,:,2) = qirim_2(:,:)
      nitot(:,:,2) = nitot_2(:,:)
      birim(:,:,2) = birim_2(:,:)
      diag_effi(:,:,2) = diag_effi_2(:,:)
      if (associated(zitot_2)) zitot(:,:,2) = zitot_2(:,:)
      if (associated(qiliq_2)) qiliq(:,:,2) = qiliq_2(:,:)

      if (n_iceCat >= 3) then
         qitot(:,:,3) = qitot_3(:,:)
         qirim(:,:,3) = qirim_3(:,:)
         nitot(:,:,3) = nitot_3(:,:)
         birim(:,:,3) = birim_3(:,:)
         diag_effi(:,:,3) = diag_effi_3(:,:)
         if (associated(zitot_3)) zitot(:,:,3) = zitot_3(:,:)
         if (associated(qiliq_3)) qiliq(:,:,3) = qiliq_3(:,:)

         if (n_iceCat == 4) then
            qitot(:,:,4) = qitot_4(:,:)
            qirim(:,:,4) = qirim_4(:,:)
            nitot(:,:,4) = nitot_4(:,:)
            birim(:,:,4) = birim_4(:,:)
            diag_effi(:,:,4) = diag_effi_4(:,:)
            if (associated(zitot_4)) zitot(:,:,4) = zitot_4(:,:)
            if (associated(qiliq_4)) qiliq(:,:,4) = qiliq_4(:,:)
         endif
      endif
   endif

  !--- substepping microphysics
   if (n_substep > 1) then
      prt_liq_ave(:) = 0.
      prt_sol_ave(:) = 0.
      rn1_ave(:)  = 0.
      rn2_ave(:)  = 0.
      sn1_ave(:)  = 0.
      sn2_ave(:)  = 0.
      sn3_ave(:)  = 0.
      pe1_ave(:)  = 0.
      pe2_ave(:)  = 0.
      ws_ave(:)   = 0.
      snd_ave(:)  = 0.
   endif

   tmparr_ik = (1.e+5/pres)**(rd*i_cp)  !for optimization of calc of theta, temp

   substep_loop: do i_substep = 1, n_substep

     !convert to potential temperature:
     qvapm   = qvap
     qvap    = qvap+qqdelta
     theta_m = temp*tmparr_ik
     temp    = temp+ttdelta
     theta   = temp*tmparr_ik

     if (.not. log_trplMomI) zitot = 0.  !not used, but avoids passing uninialized values
     if (.not. log_liqFrac)  qiliq = 0.  !not used, but avoids passing uninialized values

     call p3_main(qc,nc,qr,nr,theta_m,theta,qvapm,qvap,dt_mp,qitot,qirim,qiliq,nitot,birim,    &
                  zitot,ssat,ww,pres,DZ,kount,prt_liq,prt_sol,i_strt,ni,k_strt,nk,n_iceCat,    &
                  diag_Zet,diag_effc,diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d,   &
                  n_diag_3d,diag_3d,log_predictNc,trim(model),clbfact_dep,clbfact_sub,         &
                  debug_on,scpf_on,scpf_pfrac,scpf_resfact,cldfrac,                            &
                  log_3momentIce = log_trplMomI,                                               &
                  log_LiquidFrac = log_liqFrac,                                                &
!                 nccnst         = nccnst,                                                     &
                  prt_drzl       = prt_drzl,                                                   &
                  prt_rain       = prt_rain,                                                   &
                  prt_crys       = prt_crys,                                                   &
                  prt_snow       = prt_snow,                                                   &
                  prt_grpl       = prt_grpl,                                                   &
                  prt_pell       = prt_pell,                                                   &
                  prt_hail       = prt_hail,                                                   &
                  prt_sndp       = prt_sndp,                                                   &
                  prt_wsnow      = prt_wsnow,                                                  &
                  qi_type        = qi_type,                                                    &
                  diag_vis       = diag_vis,                                                   &
                  diag_vis1      = diag_vis1,                                                  &
                  diag_vis2      = diag_vis2,                                                  &
                  diag_vis3      = diag_vis3,                                                  &
                  diag_dhmax     = diag_dhmax,                                                 &
                  supi_nuc_in    = supdepthr,                                                  &
                  freq3Ddiag_in  = freq3Ddiag_gem)

      if (global_status /= STATUS_OK) return

     !convert back to temperature:
      temp = theta/tmparr_ik    !i.e.: temp = theta*(pres*1.e-5)**(rd*i_cp)

      if (n_substep > 1) then
         prt_liq_ave(:) = prt_liq_ave(:) + prt_liq(:)
         prt_sol_ave(:) = prt_sol_ave(:) + prt_sol(:)
         rn1_ave(:) = rn1_ave(:) + prt_drzl(:)
         rn2_ave(:) = rn2_ave(:) + prt_rain(:)
         sn1_ave(:) = sn1_ave(:) + prt_crys(:)
         sn2_ave(:) = sn2_ave(:) + prt_snow(:)
         sn3_ave(:) = sn3_ave(:) + prt_grpl(:)
         pe1_ave(:) = pe1_ave(:) + prt_pell(:)
         pe2_ave(:) = pe2_ave(:) + prt_hail(:)
         snd_ave(:) = snd_ave(:) + prt_sndp(:)
         ws_ave(:)  = ws_ave(:)  + prt_wsnow(:)
      endif

   enddo substep_loop

   ! Take maximum hail size for all category included
   maxD_hail = maxval(diag_dhmax,3)

   if (n_substep > 1) then
      tmp1 = 1./float(n_substep)
      prt_liq(:)  = prt_liq_ave(:)*tmp1
      prt_sol(:)  = prt_sol_ave(:)*tmp1
      prt_drzl(:) = rn1_ave(:)*tmp1
      prt_rain(:) = rn2_ave(:)*tmp1
      prt_crys(:) = sn1_ave(:)*tmp1
      prt_snow(:) = sn2_ave(:)*tmp1
      prt_grpl(:) = sn3_ave(:)*tmp1
      prt_pell(:) = pe1_ave(:)*tmp1
      prt_hail(:) = pe2_ave(:)*tmp1
      prt_sndp(:) = snd_ave(:)*tmp1
      prt_wsnow(:) = ws_ave(:)*tmp1
   endif

  !===

   diag_effc(:,:) = merge(diag_effc(:,:), 0., qc(:,:) >= SMALL_ICE_MASS)

  !decompose full ice arrays back into individual category arrays:
   qitot_1(:,:) = qitot(:,:,1)
   qirim_1(:,:) = qirim(:,:,1)
   nitot_1(:,:) = nitot(:,:,1)
   birim_1(:,:) = birim(:,:,1)
   if (associated(zitot_1)) zitot_1(:,:) = zitot(:,:,1)
   if (associated(qiliq_1)) qiliq_1(:,:) = qiliq(:,:,1)
   diag_effi_1(:,:) = merge(diag_effi(:,:,1), 0., qitot_1(:,:) >= SMALL_ICE_MASS)

   if (n_iceCat >= 2) then
      qitot_2(:,:) = qitot(:,:,2)
      qirim_2(:,:) = qirim(:,:,2)
      nitot_2(:,:) = nitot(:,:,2)
      birim_2(:,:) = birim(:,:,2)
      if (associated(zitot_2)) zitot_2(:,:) = zitot(:,:,2)
      if (associated(qiliq_2)) qiliq_2(:,:) = qiliq(:,:,2)
      diag_effi_2(:,:) = merge(diag_effi(:,:,2), 0., qitot_2(:,:) >= SMALL_ICE_MASS)

      if (n_iceCat >= 3) then
         qitot_3(:,:) = qitot(:,:,3)
         qirim_3(:,:) = qirim(:,:,3)
         nitot_3(:,:) = nitot(:,:,3)
         birim_3(:,:) = birim(:,:,3)
         if (associated(zitot_3)) zitot_3(:,:) = zitot(:,:,3)
         if (associated(qiliq_3)) qiliq_3(:,:) = qiliq(:,:,3)
         diag_effi_3(:,:) = merge(diag_effi(:,:,3), 0., qitot_3(:,:) >= SMALL_ICE_MASS)

         if (n_iceCat == 4) then
            qitot_4(:,:) = qitot(:,:,4)
            qirim_4(:,:) = qirim(:,:,4)
            nitot_4(:,:) = nitot(:,:,4)
            birim_4(:,:) = birim(:,:,4)
            if (associated(zitot_4)) zitot_4(:,:) = zitot(:,:,4)
            if (associated(qiliq_4)) qiliq_4(:,:) = qiliq(:,:,4)
            diag_effi_4(:,:) = merge(diag_effi(:,:,4), 0., qitot_4(:,:) >= SMALL_ICE_MASS)

         endif
      endif
   endif

  !convert precip rates from volume flux (m s-1) to mass flux (kg m-2 s-1):
  ! (since they are computed back to liq-eqv volume flux in s/r 'ccdiagnostics.F90')
   prt_liq = prt_liq*1000.
   prt_sol = prt_sol*1000.

  !--- diagnostics:
   diag_hcb(:) = -1.
   diag_hsn(:) = -1.

   do i = 1,ni

    !composite (column-maximum) reflectivity:
      diag_Zec(i) = maxval(diag_Zet(i,:))

    !diagnostic heights:
      log_tmp1 = .false.  !cloud base height found
      log_tmp2 = .false.  !snow level height found
      do k = nk,2,-1
        !cloud base height:
         if (qc(i,k)>1.e-6 .and. .not.log_tmp1) then
            diag_hcb(i) = gztherm(i,k)
            log_tmp1 = .true.
         endif
        !snow level height:  (height of lowest level with ice) [for n_iceCat=1 only]
         if (qitot_1(i,k)>1.e-6 .and. .not.log_tmp2) then
            diag_hsn(i) = gztherm(i,k)
            log_tmp2 = .true.
         endif
      enddo

    !supercooled LWC:
      do k = 1,nk
         if (temp(i,k)<trplpt) then
            tmp1 = pres(i,k)/(287.15*temp(i,k))  !air density
            diag_slw(i,k) = tmp1*(qc(i,k)+qr(i,k))
         else
            diag_slw(i,k) = 0.
         endif
      enddo

   enddo  !i-loop

   ! Diagnostic ice particle types:
   if (n_qiType >= 6) then
      qi_type_1 = qi_type(:,:,1)  !small ice crystals
      qi_type_2 = qi_type(:,:,2)  !unrimed snow crystals
      qi_type_3 = qi_type(:,:,3)  !lightly rimed snow
      qi_type_4 = qi_type(:,:,4)  !graupel
      qi_type_5 = qi_type(:,:,5)  !hail
      qi_type_6 = qi_type(:,:,6)  !ice pellets
   else
      call physeterror('microphy_p3::mp_p3_wrapper_gem', &
           'Insufficient size for qi_type')
      return
   endif

   ! Total sum at t+
   totmass(:,:) = qvap(:,:)+qr(:,:)+qc(:,:)+qitot_1(:,:)
   if (n_iceCat > 1) totmass(:,:) = totmass(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) totmass(:,:) = totmass(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) totmass(:,:) = totmass(:,:) + qitot_4(:,:)
   inv_totmass(:,:) = 1./(1.+totmass(:,:))
   ! Water vapour:
   qvap(:,:) = qvap(:,:)*inv_totmass(:,:)
   ! Cloud water:
   qc(:,:) = qc(:,:)*inv_totmass(:,:)
   nc(:,:) = nc(:,:)*inv_totmass(:,:)
   ! Rain water:
   qr(:,:) = qr(:,:)*inv_totmass(:,:)
   nr(:,:) = nr(:,:)*inv_totmass(:,:)
   ! Ice:
   qitot_1(:,:) = qitot_1(:,:)*inv_totmass(:,:)
   qirim_1(:,:) = qirim_1(:,:)*inv_totmass(:,:)
   nitot_1(:,:) = nitot_1(:,:)*inv_totmass(:,:)
   birim_1(:,:) = birim_1(:,:)*inv_totmass(:,:)
   if (associated(zitot_1)) zitot_1(:,:) = zitot_1(:,:)*inv_totmass(:,:)
   if (associated(qiliq_1)) qiliq_1(:,:) = qiliq_1(:,:)*inv_totmass(:,:)
   if (n_iceCat >= 2) then
      qitot_2(:,:) = qitot_2(:,:)*inv_totmass(:,:)
      qirim_2(:,:) = qirim_2(:,:)*inv_totmass(:,:)
      nitot_2(:,:) = nitot_2(:,:)*inv_totmass(:,:)
      birim_2(:,:) = birim_2(:,:)*inv_totmass(:,:)
      if (associated(zitot_2)) zitot_2(:,:) = zitot_2(:,:)*inv_totmass(:,:)
      if (associated(qiliq_2)) qiliq_2(:,:) = qiliq_2(:,:)*inv_totmass(:,:)
      if (n_iceCat >= 3) then
         qitot_3(:,:) = qitot_3(:,:)*inv_totmass(:,:)
         qirim_3(:,:) = qirim_3(:,:)*inv_totmass(:,:)
         nitot_3(:,:) = nitot_3(:,:)*inv_totmass(:,:)
         birim_3(:,:) = birim_3(:,:)*inv_totmass(:,:)
         if (associated(zitot_3)) zitot_3(:,:) = zitot_3(:,:)*inv_totmass(:,:)
         if (associated(qiliq_3)) qiliq_3(:,:) = qiliq_3(:,:)*inv_totmass(:,:)
         if (n_iceCat >= 4) then
            qitot_4(:,:) = qitot_4(:,:)*inv_totmass(:,:)
            qirim_4(:,:) = qirim_4(:,:)*inv_totmass(:,:)
            nitot_4(:,:) = nitot_4(:,:)*inv_totmass(:,:)
            birim_4(:,:) = birim_4(:,:)*inv_totmass(:,:)
            if (associated(zitot_4)) zitot_4(:,:) = zitot_4(:,:)*inv_totmass(:,:)
            if (associated(qiliq_4)) qiliq_4(:,:) = qiliq_4(:,:)*inv_totmass(:,:)
         endif
      endif
   endif

   ! Compute tendencies and reset state
   iwc(:,:) =  qitot_1(:,:)
   if (n_iceCat > 1) iwc(:,:) = iwc(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) iwc(:,:) = iwc(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) iwc(:,:) = iwc(:,:) + qitot_4(:,:)
   ttend(:,:) = (temp(:,:) - temp0(:,:)) * idt
   qtend(:,:) = (qvap(:,:) - qvap0(:,:)) * idt
   qctend(:,:) = (qc(:,:) - qc0(:,:)) * idt
   qrtend(:,:) = (qr(:,:) - qr0(:,:)) * idt
   qitend(:,:) = (iwc(:,:) - iwc0(:,:)) * idt
   temp(:,:) = temp0(:,:)
   qvap(:,:) = qvap0(:,:)
   qc(:,:) = qc0(:,:)
   qr(:,:) = qr0(:,:)

   end_status = STATUS_OK
   return

 end function mp_p3_wrapper_gem

#endif

!==========================================================================================!

 SUBROUTINE compute_SCPF(Qcond,Qprec,Qv,Qsi,Pres,ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,       &
                         SPF_clr,Qv_cld,Qv_clr,cldFrac_on,pfrac,resfact,quick)

!------------------------------------------------------------------------------------------!
! This subroutine computes the cloud and precipitation fractions.  It also provide         !
! in-cloud/clear sky water vapor mixing ratios and the inverse of "cloud" and              !
! precipitation fractions to ease computation in s/r 'p3_main'. It is called 3 times:      !
!                                                                                          !
! 1. Before microphysics source/sink terms and following updates of grid-mean fields       !
! 2. Before sedimentation                                                                  !
! 3. At the end of 'p3_main' (to provide cloud fraction to the driving model               !
!    (e.g. for the radiation scheme, diagnostics, etc.)                                    !
!                                                                                          !
! For details see:  Chosson et al. (2014) [J. Atmos. Sci., 71, 2635-2653]                  !
!                                                                                          !
! NOTES:                                                                                   !
!   'scpf_resfact' is the user-specified scaled horizontal grid spacing, which allows the  !
!   RH threshold to adapt to the model resolution (i.e. to be "scale aware").              !
!   The current recommendation is:  scpf_resfact = sqrt(dx/dx_ref). where dx_ref = 12 km   !
!                                                                                          !
!------------------------------------------------------------------------------------------!
!      Version 1:    April 2016,  Frederick Chosson (ECCC)                                 !
!                    This version is not "scale aware" and RHcrit is from Sundqvist RDPS   !
!                    but without dependency on T (RHcriterion -RHoo- cst in free atm.)     !
!                    This version have a very low optimisation level                       !
!                                                                                          !
!      Version 2:    November 2016, Frederick Chosson (ECCC)                               !
!                    add minimum Cloud and Precipitation Fraction to  1%                   !
!                    add maximum Cloud and Precipitation Fraction to 99%                   !
!                                                                                          !
!      Version 3:    June 2018, Caroline Jouan (ECCC)                                      !
!                    Tests in GEM models                                                   !
!                                                                                          !
!------------------------------------------------------------------------------------------!

 implicit none

!----- input/ouput arguments:  ----------------------------------------------------------!
 real, intent(in),  dimension(:,:) :: Qcond     ! Condensates mix.ratio that goes in the "Cloudy fraction"
 real, intent(in),  dimension(:,:) :: Qprec     ! Condensates mix.ratio that goes in the "Precip fraction"
 real, intent(in),  dimension(:,:) :: Qv        ! Water vapor mix.ratio (grid mean)
 real, intent(in),  dimension(:,:) :: Qsi       ! Saturation Water vapor mix.ratio w.r.t. ice or liq, dep. on T
 real, intent(in),  dimension(:,:) :: pres      ! pressure in Pa
 real, intent(out), dimension(:,:) :: SCF,iSCF  ! Subgrid "cloudy" fraction (fraction where RH>100%) and inverse
 real, intent(out), dimension(:,:) :: SPF,iSPF  ! Subgrid "precip" fraction and inverse
 real, intent(out), dimension(:,:) :: SPF_clr   ! Subgrid "precip" fraction in clear sky (not overlap cloud)
 real, intent(out), dimension(:,:) :: Qv_cld    ! Water vapor mix.ratio     in "cloudy" fraction
 real, intent(out), dimension(:,:) :: Qv_clr    ! Water vapor mix.ratio NOT in "cloudy" fraction
 real, intent(in)                  :: pfrac     ! precipitation fraction factor
 real, intent(in)                  :: resfact   ! model resolution factor
 integer, intent(in)               :: ktop,kbot ! indices of model top and bottom
 integer, intent(in)               :: kdir      ! indice  for direction from bottom to top
 logical, intent(in)               :: quick     ! switch if you only need SCF as output, not the rest (3rd call)
 logical, intent(in)               :: cldFrac_on! switch if you only need SCF or set it to 1.


!----- local variables and parameters: --------------------------------------------------!
 real, dimension(size(Qv,dim=1),size(Qv,dim=2)) :: C  ! Total cloud cover form top to level k
 real, parameter :: SIG_min = 0.7            ! minimum of sigma level below wich RHoo start to increase
 real, parameter :: SIG_max = 0.9            ! maximum of sigma level below wich RHoo stop  to increase
 real, parameter :: xo      = 1.-1.e-6       ! a number very close but less than 1.
 real            :: RHoo_min                 ! minimum of relative humidity criterion for dx around 12km
 real            :: RHoo_max                 ! maximum of relative humidity criterion for dx around 12km
 real            :: slope                    ! scale factor=(RHoo_max-RHoo_min)/(SIG_min-SIG_max)
 real            :: RHoo                     ! Relative humidity criterion above which saturation appears
 real            :: Qtot,DELTA_Qtot          ! Total "cloudy" condensate and the half-width of its PDF
 real            :: D_A_cld2clr              ! Area of cloudy precips. that fall in clear air below
 real            :: D_A_clr2cld              ! Area of clear air precips that fall into cloud below
 real            :: D_C                      ! Area never concerned by precips from top to level k
 real            :: SPF_cld                  ! area of cloudy precips at level k
 real            :: SPF_cld_k_1              ! area of cloudy precips at level k+kdir (just above)
 real            :: sigma                    ! sigma level = P / Psurf with Psurf=P(:,kbot)
 real            :: tmp7                     ! temporary SPF
 integer         :: i,k                      ! loop indices

! Note (OPT): This can be done outside the subroutine to save cost
 compute_cloud_fraction: if (cldFrac_on) then

   ! initialise constants
    RHoo_min = 1.-(1.-0.85 )*resfact         ! minimum of relative humidity criterion for dx ~ 12 km by default
    RHoo_max = 1.-(1.-0.975)*resfact         ! maximum of relative humidity criterion for dx ~ 12 km
    slope    = (RHoo_max-RHoo_min)/(SIG_max-SIG_min)

   ! Initiate Cloud fractions overlaps to zero
    SCF(:,:)    = 0.;      iSCF(:,:)    = 0.;     D_A_cld2clr = 0.
    D_A_clr2cld = 0.;      C(:,:)       = 0.;     D_C         = 0.
    SPF_cld     = 0.;      SPF_clr(:,:) = 0.;     SPF(:,:)    = 0.
    iSPF(:,:)   = 0.;      Qv_cld(:,:)  = 0.;     Qv_clr(:,:) = 0.
    SPF_cld_k_1 = 0.

    Loop_SCPF_k: do k = ktop-kdir,kbot,-kdir
     do i = 1,size(Qv,dim=2)

       sigma = pres(i,k)/pres(i,kbot)                     ! sigma level
       RHoo  = RHoo_min + slope*(sigma-SIG_min )          ! critical relative humidity
       RHoo  = max( RHoo_min, min( RHoo_max, RHoo ) )     ! bounded

       !------------------------------------------------------------
       ! COMPUTE CLOUD FRACTION AND in-FRACTIONS WATER VAPOR CONTENT
       !------------------------------------------------------------
       Qtot       = Qv(i,k)+Qcond(i,k)                            ! Total "cloudy" mean water mixing ratio
       DELTA_Qtot = Qsi(i,k)*(1.-RHoo)                          ! half-width of Qtot subgrid PDF
       SCF(i,k)     = 0.5*(Qtot+DELTA_Qtot-QSI(i,k))/DELTA_Qtot   ! subgrid cloud fraction

       if (SCF(i,k) .lt. 0.01 ) then          ! minimum allowed cloud fraction (below it is clear-sky)
          SCF(i,k)    = 0.                    ! inverse of cloud cover
          iSCF(i,k)   = 0.                    ! inverse of cloud cover
          Qv_cld(i,k) = 0.                    ! water vapour mix. ratio in cloudy part
          Qv_clr(i,k) = Qv(i,k)                 ! water vapour mix. ratio in clear sky part
       elseif (SCF(i,k) .lt. 0.99 ) then
          iSCF(i,k)   = 1./SCF(i,k)             ! beware: Could be big!
          Qv_cld(i,k) = 0.5*(Qtot+DELTA_Qtot+QSI(i,k))-Qcond(i,k)*iSCF(i,k)
          Qv_clr(i,k) = 0.5*(Qtot-DELTA_Qtot+QSI(i,k))
       else ! if SCF >= 0.99
          SCF(i,k)    = 1.
          iSCF(i,k)   = 1.
          Qv_cld(i,k) = Qv(i,k)
          Qv_clr(i,k) = 0.
       endif

       !------------------------------------------------------------
       ! COMPUTE CLOUD AND PRECIPITATION FRACTIONS OVERLAPS
       !------------------------------------------------------------
       if (.not. quick) then

         ! This is the total max-random cloud-cover from top to level k
         C(i,k) = 1.-(1.-C(i,k+kdir))*(1.-max(SCF(i,k),SCF(i,k+kdir)))/(1.-min(SCF(i,k+kdir),xo))
         ! Change in total cloud-cover: this part is never concerned by precips
         D_C = C(i,k)-C(i,k+kdir)
         ! Cloudy precipitation fraction at level k+kdir (level above)
         SPF_cld_k_1 = SPF(i,k+kdir)-SPF_clr(i,k+kdir)
         ! fraction for which cloudy precip. falls into clear air below
         D_A_cld2clr = SPF_cld_k_1 - min(SCF(i,k)-D_C,SPF_cld_k_1)
         ! fraction for which clear-sky precip. falls into cloudy air below
         D_A_clr2cld = max(0., min(SPF_clr(i,k+kdir),SCF(i,k)-D_C-SCF(i,k+kdir)) )
         ! fraction of cloudy precips at level k
         SPF_cld = SPF_cld_k_1 + D_A_clr2cld - D_A_cld2clr
         if (SPF_cld .le. 0.) SPF_cld=SCF(i,k)*Pfrac
         ! fraction of clear-sky precips at level k
         SPF_clr(i,k) = SPF_clr(i,k+kdir) - D_A_clr2cld + D_A_cld2clr
         ! if there is no precips set precips areas to zero
         tmp7 = (SPF_clr(i,k)+SPF_cld)

         if (tmp7.gt.0.) then
           if ((Qprec(i,k)/tmp7<qsmall ) .or. (Qprec(i,k+kdir)*iSPF(i,k+kdir)<qsmall)) then
              SPF_cld    = SCF(i,k+kdir)*Pfrac
              SPF_clr(i,k) = 0.
           endif
         endif

         SPF(i,k) = (SPF_clr(i,k) + SPF_cld)             ! subgrid area of precipitation
         if (SPF(i,k) .ge. 0.01) then
            iSPF(i,k)= 1. / SPF(i,k)                     ! inverse of precip fraction
         else
            if (Qprec(i,k) .ge. qsmall) then
               SPF(i,k)     = max(0.01, SCF(i,k+kdir))   ! in case of slant-wise rain precipitating
               SPF_clr(i,k) = SPF(i,k)                   ! assume at least 1% SPF in clear-sky
               iSPF(i,k)    = 1./SPF(i,k)
            else
               iSPF(i,k)    = 0.
               SPF(i,k)     = 0.
               SPF_clr(i,k) = 0.
            endif
         endif

       endif ! end of IF NOT quick

! Note (BUG): Qcond should be separated into qc and qitot, otherwise
! qc<qsmall and qitot<qsmall but the sum is >= qsmall, which is a problem
       if ((SCF(i,k) .lt. 0.01) .and. (Qcond(i,k) > qsmall) ) then  ! avoid bad clipping
           SCF(i,k)    = max(0.01, SCF(i,k+kdir))                   ! in case of cloudy species precipitating
          iSCF(i,k)    = 1./SCF(i,k)                                ! into unsaturated layer
          Qv_cld(i,k)  = Qv(i,k)
          Qv_clr(i,k)  = Qv(i,k)
          SPF_clr(i,k) = max(SPF(i,k)-SCF(i,k),0.)
       endif

     enddo !i loop
    enddo Loop_SCPF_k

 else  ! compute_cloud_fraction

    SCF  = 1.
    iSCF = 1.
    SPF  = 1.
    iSPF = 1.
    SPF_clr = 0.
    Qv_cld  = Qv
    Qv_clr  = 0.

 endif compute_cloud_fraction

 END SUBROUTINE compute_SCPF

!==========================================================================================!

 SUBROUTINE p3_main(qc,nc,qr,nr,th_old,th,qv_old,qv,dt,qitot,qirim,qiliq,nitot,birim,     &
                    zitot,ssat,uzpl,pres,dzq,it,prt_liq,prt_sol,its,ite,kts,kte,nCat,     &
                    diag_ze,diag_effc,diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,     &
                    diag_2d,n_diag_3d,diag_3d,log_predictNc,model,clbfact_dep,            &
                    clbfact_sub,debug_on,scpf_on,scpf_pfrac,scpf_resfact,SCF_out,         &
                    log_3momentIce,log_LiquidFrac,nccnst_in,prt_drzl,prt_rain,prt_crys,   &
                    prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,prt_wsnow,qi_type,       &
                    diag_vis,diag_vis1,diag_vis2,diag_vis3,diag_dhmax,supi_nuc_in,        &
                    freq3Ddiag_in,timer,timer_description)

!----------------------------------------------------------------------------------------!
!                                                                                        !
! This is the main subroutine for the P3 microphysics scheme.  It is called from the     !
! wrapper subroutine ('MP_P3_WRAPPER_{model}') and is passed i,k slabs of all prognostic !
! variables -- hydrometeor fields, potential temperature, and water vapor mixing ratio.  !
! Microphysical process rates are computed first.  These tendencies are then used to     !
! computed updated values of the prognostic variables.  The hydrometeor variables are    !
! then updated further due to sedimentation.                                             !
!                                                                                        !
! Several diagnostic values are also computed and returned to the wrapper subroutine,    !
! including precipitation rates.                                                         !
!                                                                                        !
!----------------------------------------------------------------------------------------!

 implicit none

!----- Input/ouput arguments:  ----------------------------------------------------------!

 integer, intent(in)                                  :: its,ite    ! array bounds (horizontal)
 integer, intent(in)                                  :: kts,kte    ! array bounds (vertical)
 integer, intent(in)                                  :: nCat       ! number of ice-phase categories
 integer, intent(in)                                  :: n_diag_2d  ! number of 2D diagnostic fields
 integer, intent(in)                                  :: n_diag_3d  ! number of 3D diagnostic fields

 real, intent(inout), dimension(its:ite,kts:kte)      :: qc         ! cloud, mass mixing ratio         kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: nc         ! cloud, number mixing ratio       #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: qr         ! rain, mass mixing ratio          kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: nr         ! rain, number mixing ratio        #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qitot      ! ice, total mass mixing ratio     kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qirim      ! ice, rime mass mixing ratio      kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qiliq      ! ice, liquid mass mixing ratio    kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: nitot      ! ice, total number mixing ratio   #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: birim      ! ice, rime volume mixing ratio    m3 kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: zitot      ! ice, 6th-moment mixing ratio     m6 kg-1

 real, intent(inout), dimension(its:ite,kts:kte)      :: ssat       ! supersaturation (i.e., qv-qvs)   kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: qv         ! water vapor mixing ratio         kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: th         ! potential temperature            K
 real, intent(inout), dimension(its:ite,kts:kte)      :: th_old     ! theta at beginning of time step  K
 real, intent(inout), dimension(its:ite,kts:kte)      :: qv_old     ! qv at beginning of time step     kg kg-1
 real, intent(in),    dimension(its:ite,kts:kte)      :: uzpl       ! vertical air velocity            m s-1
 real, intent(in),    dimension(its:ite,kts:kte)      :: pres       ! pressure                         Pa
 real, intent(in),    dimension(its:ite,kts:kte)      :: dzq        ! vertical grid spacing            m
 real, intent(in)                                     :: dt         ! model time step                  s
 real, intent(in)                                     :: clbfact_dep! calibration factor for deposition
 real, intent(in)                                     :: clbfact_sub! calibration factor for sublimation

 real, intent(out),   dimension(its:ite)              :: prt_liq    ! precipitation rate, liquid (c+r) m s-1
 real, intent(out),   dimension(its:ite)              :: prt_sol    ! precipitation rate, solid        m s-1
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_ze    ! equivalent reflectivity          dBZ
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_effc  ! effective radius, cloud          m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_effi  ! effective radius, ice            m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_vmi   ! mass-weighted fall speed of ice  m s-1
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_di    ! mean diameter of ice             m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_rhoi  ! bulk density of ice              kg m-1

!real, intent(out),   dimension(its:ite,kts:kte,nCat), optional :: diag_Dhm  ! maximum hail diameter   m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis   ! visibility (total)          m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis1  ! visibility through fog      m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis2  ! visibility through rain     m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis3  ! visibility through snow     m
 real, intent(out),   dimension(its:ite,n_diag_2d)         :: diag_2d    ! user-defined 2D diagnostic fields
 real, intent(out),   dimension(its:ite,kts:kte,n_diag_3d) :: diag_3d    ! user-defined 3D diagnostic fields

 integer, intent(in)                                  :: it              ! time step counter (starts at 1 for first step)

 logical, intent(in)                                  :: log_predictNc  ! .T. for two-moment (.F. for one-moment) cloud
 logical, intent(in)                                  :: log_3momentIce ! .T. for three-moment (.F. for two-moment) ice
 logical, intent(in)                                  :: log_LiquidFrac ! .T. for prognostic liquid-fraction
 logical, intent(in)                                  :: debug_on       ! switch for internal debug checks
 character(len=*), intent(in)                         :: model          ! driving model

 logical, intent(in)                                  :: scpf_on       ! Switch to activate SCPF
 real,    intent(in)                                  :: scpf_pfrac    ! precipitation fraction factor (SCPF)
 real,    intent(in)                                  :: scpf_resfact  ! model resolution factor (SCPF)
 real,    intent(out), dimension(its:ite,kts:kte)     :: SCF_out       ! cloud fraction from SCPF

 real, intent(in),  optional                          :: nccnst_in     ! 1-mom cloud concentration     # m-3
 real, intent(in),  optional                          :: supi_nuc_in   ! ice supersat threshold for deposition nucleation
 real, intent(in),  optional                          :: freq3Ddiag_in ! frequency (min) for full-column diagnostics

 real, intent(out), dimension(its:ite), optional      :: prt_drzl      ! precip rate, drizzle          m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_rain      ! precip rate, rain             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_crys      ! precip rate, ice cystals      m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_snow      ! precip rate, snow             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_grpl      ! precip rate, graupel          m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_pell      ! precip rate, ice pellets      m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_hail      ! precip rate, hail             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_sndp      ! precip rate, unmelted snow    m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_wsnow     ! precip rate, very wet snow    m s-1

 real, intent(out), dimension(its:ite,kts:kte,nCat),     optional :: diag_dhmax ! maximum hail size    m
 real, intent(out), dimension(its:ite,kts:kte,n_qiType), optional :: qi_type    ! mass mixing ratio, diagnosed ice type  kg kg-1

 real,              intent(out), dimension(20), optional :: timer             ! local CPU time for code block (timer = timer_end - timer_start)
 character(len=20), intent(out), dimension(20), optional :: timer_description ! description of code block being timed

 !----- Local variables and parameters:  -------------------------------------------------!

 real, dimension(20)              :: timer_start,timer_end

 real, dimension(its:ite,kts:kte) :: mu_r  ! shape parameter of rain
 real, dimension(its:ite,kts:kte) :: t     ! temperature at the beginning of the microhpysics step [K]
 real, dimension(its:ite,kts:kte) :: t_old ! temperature at the beginning of the model time step [K]
 real, dimension(its:ite,nCat)    :: prt_soli ! precipitation rate, solid iice-dep  m s-1

 logical, parameter               :: log_liqsatadj = .false.     ! temporary; to be put as GEM namelist

! 2D size distribution and fallspeed parameters:

 real, dimension(its:ite,kts:kte) :: lamc
 real, dimension(its:ite,kts:kte) :: lamr
 real, dimension(its:ite,kts:kte) :: logn0r
 real, dimension(its:ite,kts:kte) :: mu_c
!real, dimension(its:ite,kts:kte) :: diag_effr   (currently not used)
 real, dimension(its:ite,kts:kte) :: nu
 real, dimension(its:ite,kts:kte) :: cdist
 real, dimension(its:ite,kts:kte) :: cdist1
 real, dimension(its:ite,kts:kte) :: cdistr
 real, dimension(its:ite,kts:kte) :: Vt_qc

! liquid-phase microphysical process rates:
!   all Q process rates have units of kg kg-1 s-1
!   all N process rates have units of # kg-1

 real :: qrcon   ! rain condensation
 real :: qcacc   ! cloud droplet accretion by rain
 real :: qcaut   ! cloud droplet autoconversion to rain
 real :: ncacc   ! change in cloud droplet number from accretion by rain
 real :: ncautc  ! change in cloud droplet number from autoconversion
 real :: ncslf   ! change in cloud droplet number from self-collection
 real :: nrslf   ! change in rain number from self-collection
 real :: ncnuc   ! change in cloud droplet number from activation of CCN
 real :: qccon   ! cloud droplet condensation
 real :: qcnuc   ! activation of cloud droplets from CCN
 real :: qrevp   ! rain evaporation
 real :: qcevp   ! cloud droplet evaporation
 real :: nrevp   ! change in rain number from evaporation
 real :: ncautr  ! change in rain number from autoconversion of cloud water

! ice-phase microphysical process rates:
!  all Q process rates have units of kg kg-1 s-1
!  all N process rates have units of # kg-1

 real, dimension(nCat) :: qccol     ! collection of cloud water by ice
 real, dimension(nCat) :: qwgrth    ! wet growth rate
 real, dimension(nCat) :: qidep     ! vapor deposition
 real, dimension(nCat) :: qrcol     ! collection rain mass by ice
 real, dimension(nCat) :: qinuc     ! deposition/condensation freezing nuc
 real, dimension(nCat) :: nccol     ! change in cloud droplet number from collection by ice
 real, dimension(nCat) :: nrcol     ! change in rain number from collection by ice
 real, dimension(nCat) :: ninuc     ! change in ice number from deposition/cond-freezing nucleation
 real, dimension(nCat) :: qisub     ! sublimation of ice
 real, dimension(nCat) :: qimlt     ! melting of ice
 real, dimension(nCat) :: nimlt     ! melting of ice
 real, dimension(nCat) :: nisub     ! change in ice number from sublimation
 real, dimension(nCat) :: nislf     ! change in ice number from collection within a category
 real, dimension(nCat) :: qchetc    ! contact freezing droplets
 real, dimension(nCat) :: qcheti    ! immersion freezing droplets
 real, dimension(nCat) :: qrhetc    ! contact freezing rain
 real, dimension(nCat) :: qrheti    ! immersion freezing rain
 real, dimension(nCat) :: nchetc    ! contact freezing droplets
 real, dimension(nCat) :: ncheti    ! immersion freezing droplets
 real, dimension(nCat) :: nrhetc    ! contact freezing rain
 real, dimension(nCat) :: nrheti    ! immersion freezing rain
 real, dimension(nCat) :: nrshdr    ! source for rain number from collision of rain/ice above freezing and shedding
 real, dimension(nCat) :: qcshd     ! source for rain mass due to cloud water/ice collision above freezing and shedding or wet growth and shedding
 real, dimension(nCat) :: qrmul     ! change in q, ice multiplication from rime-splitnering of rain (not included in the paper)
 real, dimension(nCat) :: qcmul     ! change in q, ice multiplication from rime-splitnering of cloud (not included in the paper)
 real, dimension(nCat) :: nimul     ! change in Ni, ice multiplication from rime-splintering (not included in the paper)
 real, dimension(nCat) :: ncshdc    ! source for rain number due to cloud water/ice collision above freezing  and shedding (combined with NRSHD in the paper)
 real, dimension(nCat) :: rhorime_c ! density of rime (from cloud)
 real, dimension(nCat) :: zqccol    ! zi change from collection of cloud water by ice
 real, dimension(nCat) :: zidep     ! zi change from vapor deposition
 real, dimension(nCat) :: zisub     ! zi change from sublimation
 real, dimension(nCat) :: zimlt     ! zi change from melting
 real, dimension(nCat) :: zislf     ! zi change from self-collection
 real, dimension(nCat) :: zishd     ! zi change from shedding
 real, dimension(nCat) :: zqrcol    ! zi change from ice-rain collection

 real, dimension(nCat,nCat) :: nicol ! change of N due to ice-ice collision between categories
 real, dimension(nCat,nCat) :: qicol ! change of q due to ice-ice collision between categories

! New process rates with log_LiquidFrac (present(qiliq))
 real, dimension(nCat) :: qrmlt      ! melting of ice going into rain
 real, dimension(nCat) :: qwgrth1    ! wet growth rate (total=rain+cloud)
 real, dimension(nCat) :: qwgrth1c   ! wet growth rate of cloud
 real, dimension(nCat) :: qwgrth1r   ! wet growth rate of rain
 real, dimension(nCat) :: qlshd      ! shedding of mixed-phase ice
 real, dimension(nCat) :: nlshd      ! shedding of mixed-phase ice
 real, dimension(nCat) :: qlcon      ! condensation on mixed-phase ice
 real, dimension(nCat) :: qlevp      ! evaporation of mixed-phase ice
 real, dimension(nCat) :: nlevp      ! evaporation of mixed-phase ice (conc.)
 real, dimension(nCat) :: qifrz      ! refreezing of mixed-phase ice
 real, dimension(nCat) :: qrcoll     ! collection of rain by mixed-phase ice (T>0C)
 real, dimension(nCat) :: nrcoll     ! collection of rain by mixed-phase ice (T>0C)
 real, dimension(nCat) :: qccoll     ! collection of cloud by mixed-phase ice (T>0C)
 real, dimension(nCat) :: nccoll     ! collection of cloud by mixed-phase ice (T>0C)

 logical, dimension(nCat)   :: log_wetgrowth

 real, dimension(nCat) :: Eii_fact,epsi,epsiw
 real :: eii ! temperature dependent aggregation efficiency
 real :: qsmall_dry ! threshold mixing ratio below which all mass is evaporated/sublimated in dry conditions

 real, dimension(its:ite,kts:kte,nCat) :: diam_ice,liq_frac,rime_frac,                   &
                   rimefrac_over_rhorime,arr_lami,arr_mui,rimedensity

 real, dimension(its:ite,kts:kte) :: i_dzq,i_rho,ze_ice,ze_rain,ze_cld,rho,rhofacr,      &
            rhofaci,xxls,xxlv,xlf,qvs,qvi,sup,supi,vtrmi1,tmparr1,massflux_r,i_exn,      &
            SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,prec,acn

 real, dimension(kts:kte) :: V_qr,V_qit,V_nit,V_nr,V_qc,V_nc,V_zit,flux_qit,flux_qx,     &
            flux_nx,flux_nit,flux_qir,flux_bir,flux_zit,flux_qil

 real    :: ssat_cld,ssat_clr,ssat_r,supi_cld,sup_cld,sup_r

 real    :: lammax,lammin,mu,dv,sc,dqsdT,ab,kap,epsr,epsc,xx,aaa,epsilon,sigvl,epsi_tot, &
            aact,sm1,sm2,uu1,uu2,dum,dum1,dum2,dumqv,dumqvs,dums,ratio,qsat0,dum3,dum4,  &
            dum5,dum6,rdumii,rdumjj,dqsidT,abi,dumqvi,rhop,v_impact,ri,iTc,D_c,tmp1,     &
            tmp2,i_dum3,i_dt,i_xx,i_abi,fluxdiv_qit,fluxdiv_nit,fluxdiv_qir,fluxdiv_bir, &
            prt_accum,fluxdiv_qx,fluxdiv_nx,Co_max,dt_sub,fluxdiv_zit,D_new,Q_nuc,N_nuc, &
            deltaD_init,dum1c,dum4c,dum5c,dumt,qcon_satadj,qdep_satadj,sources,sinks,    &
            timeScaleFactor,dt_left,qv_tmp,t_tmp,dum1z,dum7c,dum7,fluxdiv_qil,epsiw_tot, &
            dum8,tmp3,tmp4,nccnst,qevp_satadj,supi_nuc,dumf

 double precision :: tmpdbl1,tmpdbl2,tmpdbl3

 integer :: dumi,i,k,ii,iice,iice_dest,dumj,dumii,dumjj,dumzz,tmpint1,ktop,kbot,kdir,    &
            dumic,dumiic,dumjjc,catcoll,k_qxbot,k_qxtop,dumll,dumllc,dumzq

 logical :: log_nucleationPossible,log_hydrometeorsPresent,log_predictSsat,              &
            log_hmossopOn,log_outputStep,log_outputStep_cm1,log_tmp1,log_tmp2

! quantities related to process rates/parameters, interpolated from lookup tables:

 real    :: f1pr01   ! number-weighted fallspeed
 real    :: f1pr02   ! mass-weighted fallspeed
 real    :: f1pr03   ! ice collection within a category
 real    :: f1pr04   ! collection of cloud water by ice
 real    :: f1pr05   ! melting
 real    :: f1pr06   ! effective radius
 real    :: f1pr07   ! collection of rain number by ice
 real    :: f1pr08   ! collection of rain mass by ice
 real    :: f1pr09   ! inverse normalized qsmall (for lambda limiter)
 real    :: f1pr10   ! inverse normalized qlarge (for lambda limiter)
!real    :: f1pr11   ! not used
!real    :: f1pr12   ! not used
 real    :: f1pr13   ! reflectivity
 real    :: f1pr14   ! melting (ventilation term)
 real    :: f1pr15   ! mass-weighted mean diameter
 real    :: f1pr16   ! mass-weighted mean particle density
 real    :: f1pr17   ! ice-ice category collection change in number
 real    :: f1pr18   ! ice-ice category collection change in mass
 real    :: f1pr19   ! reflectivity-weighted fallspeed
!real    :: f1pr20   ! not used
!real    :: f1pr21   ! not used
 real    :: f1pr22   ! LAMBDA_i (PSD parameter of ice, cat 1)
 real    :: f1pr23   ! MU_i     (PSD parameter of ice, cat 1)

!Only used with the log_LiquidFrac (present(qiliq))
 real    :: f1pr24   ! melting to rain
 real    :: f1pr25   ! melting to rain (ventilation term)
 real    :: f1pr26   ! melting staying on ice
 real    :: f1pr27   ! melting staying on ice (ventilation term)
 real    :: f1pr28   ! shedding of mixed-phase ice

! full 3-moment-ice quantities from lookup table
 real    :: f1pr29   ! zi tendency riming
 real    :: f1pr30   ! zi tendency vapor deposition term 1
 real    :: f1pr31   ! zi tendency vapor deposition term 2
 real    :: f1pr32   ! zi tendency melting term 1 (liquid fraction on only)
 real    :: f1pr33   ! zi tendency melting term 1 (liquid fraction on only)
 real    :: f1pr34   ! zi tendency self-collection
 real    :: f1pr35   ! zi tendency shedding
 real    :: f1pr36   ! zi tendency ice-rain collection
 real    :: f1pr37   ! zi tendency sublimation term 1
 real    :: f1pr38   ! zi tendency sublimation term 1

! for full 3-moment-ice
 real, dimension(nCat) :: epsiz,epsizsb

! quantities related to diagnostic hydrometeor/precipitation types
 real,    parameter                       :: thres_raindrop  = 100.e-6 !size threshold for drizzle vs. rain
 real,    dimension(its:ite,kts:kte)      :: Q_drizzle,Q_rain
 real,    dimension(its:ite,kts:kte,nCat) :: Q_crystals,Q_snow,Q_wsnow,Q_grpl,Q_pellets,Q_hail
 integer                                  :: ktop_typeDiag    !ktop_typeDiag_r,ktop_typeDiag_i
 logical                                  :: log_typeDiags,log_typeDiag_column

 real               :: freq3Ddiag              ! frequency (min) for full-column diagnostics
 real, parameter    :: freq3Ddiag_default = 5. ! value set if not passed in
 real, parameter    :: qsmall_dhmax = 1.e-6    ! min. qitot (kg kg-1) for computing diag_dhmax

! to be added as namelist parameters (future)
 logical, parameter :: debug_ABORT  = .true. !.true. will result in forced abort in s/r 'check_values'
 logical            :: force_abort
 integer            :: location_ind          !return value of location index from sr/ 'check_values'

! added for triple moment ice
 real                  :: mu_i               !shape parameter for ice
 real                  :: rholt3             !mean mass-weighted density from LT3
 real                  :: mu_i_new           !shape parameter for processes that specify mu_i
 real, dimension(nCat) :: dumm0,dumm3,mu_i_s

 integer :: imu
 integer, parameter :: niter_mui    = 5 ! number of iterations for find mu for lookup table
 integer, parameter :: niter_satadj = 5 ! number of iterations for saturation adj. (testing only)

 real    :: dumni,dumqi,dumzi,dumqr,dumbi,dumql,dumden,dmudt,dummu_i,dumnitend,dumqitend,dumzitend
 real    :: G_new,G_rate_tot,dumzi_old
 integer :: iana,nk
 logical, parameter :: log_full3mom = .false.   ! switch to turn on full 3-moment ice

 real,    dimension(n_args_r) :: args_r   ! array of real arguments for functions 'proc_from_LUT_[x]'
 integer, dimension(n_args_i) :: args_i   ! array of integer argument for functions 'proc_from_LUT_[x]'

!-----------------------------------------------------------------------------------!
!  End of variables/parameters declarations
!-----------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------------!
! Note, the array 'diag_3d(ni,nk,n_diag_3d)' provides a placeholder to output 3D diagnostic fields.
! The entire array array is inialized to zero (below).  Code can be added to store desired fields
! by simply adding the appropriate assignment statements.  For example, if one wishs to output the
! rain condensation and evaporation rates, simply add assignments in the appropriate locations.
!  e.g.:
!
!   diag_3d(i,k,1) = qrcon
!   diag_3d(i,k,2) = qrevp
!
! The fields will automatically be passed to the driving model.  In GEM, these arrays can be
! output by adding 'SS01' and 'SS02' to the model output list.
!
! Similarly, 'diag_2d(ni,n_diag_2d) is a placeholder to output 2D diagnostic fields.
!  e.g.:
!
!   diag_2d(i,1) = maxval(qr(i,:))  !column-maximum qr
!-----------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------------!
! The following code blocks can be instered for debugging (all within the main i-loop):
!
!    !-- call to s/r 'check_values' WITHIN k loops:
!     if (debug_on) then
!        tmparr1(i,k) = th(i,k)*(pres(i,k)*1.e-5)**(rd*i_cp)
!        call check_values(qv(i,k:k),tmparr1(i,k:k),qc(i,k:k),nc(i,k:k),qr(i,k:k),nr(i,k:k),     &
!             qitot(i,k:k,:),qirim(i,k:k,:),nitot(i,k:k,:),birim(i,k:k,:),zitot(i,k:k,:),i,it,debug_ABORT,555)
!        if (global_status /= STATUS_OK) return
!     endif
!    !==
!
!    !-- call to s/r 'check_values' OUTSIDE k loops:
!     if (debug_on) then
!        tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
!        call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:),  &
!                          qirim(i,:,:),nitot(i,:,:),birim(i,:,:),zitot(i,:,:),i,it,debug_ABORT,666)
!        if (global_status /= STATUS_OK) return
!     endif
!    !==
!-----------------------------------------------------------------------------------!

#ifdef TIMING_P3
 timer_start = 0.
 timer_end   = 0.
 if (present(timer)) timer = 0.
 if (present(timer_description)) timer_description = ''
#endif

#ifdef TIMING_P3
timer_description(1) = 'full p3_main'
call cpu_time(timer_start(1))
#endif

! set qsmall_dry to determine threshold mixing ratio below which all mass is sublimated/evaporated in dry conditions
! for the "fast" P3 configuration (no liquid fraction, no triple moment, one category), set qsmall_dry to a larger value
! for all other configurations, set qsmall_dry to a smaller value. Using improves the P3 run time by several %
! and impacts the radar reflectivity field by removing areas of small reflectivity,
! but otherwise has no noticeable impact on simulations.
 if (nCat.eq.1.and..not.(log_3momentIce).and..not.(log_LiquidFrac)) then
      qsmall_dry = qsmall_dry1
 else
      qsmall_dry = qsmall_dry2
 endif

 if (present(freq3Ddiag_in)) then
    freq3Ddiag = freq3Ddiag_in
 else
    freq3Ddiag = freq3Ddiag_default
 endif

 tmp1 = uzpl(its,kts)     !avoids compiler warning for unused variable (since code using 'uzpl' is currently commented)

 if (log_3momentIce) then
    mu_i_s(:) = mu_i_initial    ! initialize mu_i
 endif

 ! direction of vertical leveling:
 if (trim(model)=='GEM' .or. trim(model)=='KIN1D') then
    ktop = kts        !k of top level
    kbot = kte        !k of bottom level
    kdir = -1         !(k: 1=top, nk=bottom)
 else
    ktop = kte        !k of top level
    kbot = kts        !k of bottom level
    kdir = 1          !(k: 1=bottom, nk=top)
 endif

 nk = abs(kte-kts)+1

 ! Select fixed number concentration for 1-moment cloud
 !   note: nc(i,k) is the cloud number mixing ratio; for log_predictNc = .F. (i.e. 1-moment cloud)
 !         nc is still used but os updated as nc = nccnst/rho in appropriate locations in this
 !         subroutine.
 if (.not.log_predictNc) then
    if (present(nccnst_in)) then
       nccnst = nccnst_in ! passed in from driving model
    else
      !nccnst = nccnst_1  ! maritime
       nccnst = nccnst_2  ! mid-latitude continental
      !nccnst = nccnst_3  ! polluted/urban
      !nccnst = SPECIFY   ! user-specified (units: # m-3; if unable to specify in driving model)
    endif
 endif

 ! Convert advected (dynamics) variable to zitot (6th moment):
 !   This is done to preserve appropriate ratios between prognostic
 !   moments; for details, see Morrison et al. (2016), MWR
 if (log_3momentIce) then
    where (nitot>0.)
       zitot = zitot**2/nitot
    elsewhere
       zitot = 0.
    endwhere
 endif

! Determine threshold size difference [m] as a function of nCat
! (used for destination category upon ice initiation)
! note -- this code could be moved to 'p3_init'
 select case (nCat)
    case (1)
       deltaD_init = 999.    !not used if n_iceCat=1 (but should be defined)
    case (2)
       deltaD_init = 500.e-6
    case (3)
       deltaD_init = 400.e-6
    case (4)
       deltaD_init = 235.e-6
    case (5)
       deltaD_init = 175.e-6
    case (6:)
       deltaD_init = 150.e-6
 end select

! deltaD_init = 250.e-6   !for testing
! deltaD_init = dummy_in   !temporary; passed in from cld1d

! Note:  Code for prediction of supersaturation is available in current version.
!        In the future 'log_predictSsat' will be a user-defined namelist key.
 log_predictSsat = .false.

 log_typeDiags  = .true.

 i_dzq    = 1./dzq  ! inverse of thickness of layers
 i_dt     = 1./dt   ! inverse of model time step

! Compute time scale factor over which to apply soft rain lambda limiter
! note: '1./max(30.,dt)' = '1.*min(1./30., 1./dt)'
 timeScaleFactor = min(1./120., i_dt)

 prt_liq    = 0.
 prt_sol    = 0.
 prt_soli   = 0.
 massflux_r = 0.
!massflux_i = 0.
 prec       = 0.
 mu_r       = 0.
 diag_ze    = -99.        !not used; avoids possible uninialized value
 tmp1       = 4.19642e-29  !m^3 m-6; corresponds to -99 dbZ/3 (for zero hydrometeors)
 ze_ice     = tmp1
 ze_rain    = tmp1
 ze_cld     = tmp1
 diam_ice   = 0.
 liq_frac   = 0.
 rime_frac  = 0.
 rimefrac_over_rhorime = 0.
 rimedensity = 0.
 diag_effc  = 10.e-6 ! default value
!diag_effr  = 25.e-6 ! default value
 diag_effi  = 25.e-6 ! default value
 diag_vmi   = 0.
 diag_di    = 0.
 diag_rhoi  = 0.
 if (present(diag_dhmax)) diag_dhmax = 0.
 diag_2d    = 0.
 diag_3d    = 0.
 rhorime_c  = 400.
!rhorime_r  = 400.
 f1pr22     = -99.  !to avoid uninialized variable (in case of accidental use)
 f1pr23     = -99.

 if (present(supi_nuc_in)) then
    supi_nuc = supi_nuc_in   !passed in from driving model
 else
    supi_nuc  = 0.05
 endif

 tmparr1 = (pres*1.e-5)**(rd*i_cp)
 i_exn  = 1./tmparr1         !inverse of Exner function array
 t       = th    *tmparr1    !compute temperature from theta (value at beginning of microphysics step)
 t_old   = th_old*tmparr1    !compute temperature from theta (value at beginning of model time step)
 qv      = max(qv,0.)        !clip water vapor to prevent negative values passed in (beginning of microphysics)
!==

!log_hmossopOn  = (nCat.gt.1)      !default: off for nCat=1, off for nCat>1
!log_hmossopOn  = .true.           !switch to have Hallet-Mossop ON
!log_hmossopOn  = .false.          !switch to have Hallet-Mossop OFF

! Note (BUG), I think SCF, SPF,... should be initialize here with scpf_on=.false.

! initialize the qiliq to 0. to allow gereralized use even if liqFrac is not used
 if (.not.log_LiquidFrac) qiliq = 0.

!-----------------------------------------------------------------------------------!
#ifdef TIMING_P3
timer_description(2) = 'i_loop_main'
call cpu_time(timer_start(2))
#endif

! !  i_loop_main: do i = its,ite  ! main i-loop (around the entire scheme)

! !     if (nCat.eq.1) then
! !        !for nCat = 1, rime-splinter is shut off during the summer (dilution of rimed ice sizes
! !        !weakens convection) but on during the winter.  The temperature threshold of +9 C (282 K)
! !        !is used as a proxy for winter/summer
! !        log_hmossopOn = t(i,kbot).lt.282.
! !        Dmin_HM       = 250.e-6
! !        Dinit_HM      =  10.e-6
! !     else
! !        log_hmossopOn = .true.
! !        Dmin_HM       = 1000.e-6
! !        Dinit_HM      =   10.e-6
! !     endif

!     if (debug_on) then
!        location_ind = 100
!        force_abort  =.false.
!        if (log_3momentIce) then
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
!                  Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!        else
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
!                  Qiliq=qiliq(i,:,:))
!        endif
!        if (global_status /= STATUS_OK) return
!     endif


 rho(:,:)   = pres(:,:)/(rd*t(:,:))
 i_rho(:,:) = 1./rho(:,:)

 if (.not.(log_predictNc)) nc(:,:) = nccnst*i_rho(:,:)

 k_loop_1: do k = kbot,ktop,kdir
  do i = its,ite

     !calculate some time-varying atmospheric variables
       xxlv(i,k) = 3.1484e6-2370.*trplpt !t(i,k), use constant Lv
       xxls(i,k) = xxlv(i,k)+0.3337e6
       xlf(i,k)  = xxls(i,k)-xxlv(i,k)
     ! max statement added below for first calculation when t_old is zero before t_old is set at end of p3 main
       qvs(i,k)  = qv_sat(max(t_old(i,k),1.),pres(i,k),0)
       qvi(i,k)  = qv_sat(max(t_old(i,k),1.),pres(i,k),1)

      ! if supersaturation is not predicted or during the first time step, then diagnose from qv and T (qvs)
       if (.not.(log_predictSsat).or.it.le.1) then
          ssat(i,k) = qv_old(i,k)-qvs(i,k)
          sup(i,k)  = qv_old(i,k)/qvs(i,k)-1.
          supi(i,k) = qv_old(i,k)/qvi(i,k)-1.
      ! if supersaturation is predicted then diagnose sup and supi from ssat
       else if ((log_predictSsat).and.it.gt.1) then
          sup(i,k)  = ssat(i,k)/qvs(i,k)
          supi(i,k) = (ssat(i,k)+qvs(i,k)-qvi(i,k))/qvi(i,k)
       endif

       rhofacr(i,k) = (rhosur*i_rho(i,k))**0.54
       rhofaci(i,k) = (rhosui*i_rho(i,k))**0.54
       tmp1         = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)  ! this is mu
       acn(i,k)     = g*rhow/(18.*tmp1)  ! 'a' parameter for droplet fallspeed (Stokes' law)

    !--- apply mass clipping if dry and mass is sufficiently small
    !    (implying all mass is expected to evaporate/sublimate in one time step)

       if (qc(i,k).lt.qsmall .or. (qc(i,k).lt.qsmall_dry .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - i_exn(i,k)*qc(i,k)*xxlv(i,k)*i_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       endif

       if (qr(i,k).lt.qsmall .or. (qr(i,k).lt.qsmall_dry .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - i_exn(i,k)*qr(i,k)*xxlv(i,k)*i_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall .or. (qitot(i,k,iice).lt.qsmall_dry .and.        &
           supi(i,k).lt.-0.1)) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*           &
                                 xxls(i,k)*i_cp
             th(i,k) = th(i,k) - i_exn(i,k)*qiliq(i,k,iice)*xxlv(i,k)*i_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             qiliq(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

          if (log_LiquidFrac .and. qiliq(i,k,iice).ge.0.                                 &
                             .and. qitot(i,k,iice).ge.qsmall) then

             tmp1 = qiliq(i,k,iice)/qitot(i,k,iice)
             if (t(i,k).lt.trplpt .and. tmp1.le.liqfracsmall) then

             !freeze small amount of liquid (qiliq) to rime
                th(i,k) = th(i,k) + i_exn(i,k)*qiliq(i,k,iice)*xlf(i,k)*i_cp
                birim(i,k,iice) = birim(i,k,iice) + qiliq(i,k,iice)*i_rho_rimeMax
                qirim(i,k,iice) = qirim(i,k,iice) + qiliq(i,k,iice)
                qiliq(i,k,iice) = 0.

             elseif (tmp1.gt.(1.-liqfracsmall)) then

             !completely melt all nearly-melted ice
                qr(i,k) = qr(i,k) + qitot(i,k,iice)
                nr(i,k) = nr(i,k) + nitot(i,k,iice)
                th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*        &
                                    xlf(i,k)*i_cp
                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                qiliq(i,k,iice) = 0.
                birim(i,k,iice) = 0.
             endif

          endif

          if (qitot(i,k,iice).ge.qsmall .and. qitot(i,k,iice).lt.qsmall_dry              &
                                        .and. t(i,k).ge.trplpt) then
            !completely melt all tiny quantities of ice if T>0C
             qr(i,k) = qr(i,k) + qitot(i,k,iice)
             nr(i,k) = nr(i,k) + nitot(i,k,iice)
             th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*xlf(i,k)*  &
                                 i_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             qiliq(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

          qiliq(i,k,iice) = max(0., qiliq(i,k,iice))

       enddo  !iice-loop

    !===
  enddo !i loop
 enddo k_loop_1

!zero out zitot if there is no qitot for triple moment
 if (log_3momentIce) where (qitot.lt.qsmall) zitot = 0.

!     if (debug_on) then
!        location_ind = 200
!        force_abort  =.false.
!        tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
!        if (log_3momentIce) then
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                           qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,      &
!                           force_abort,location_ind,Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!        else
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                           qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,      &
!                           force_abort,location_ind,Qiliq=qiliq(i,:,:))
!        endif
!        if (global_status /= STATUS_OK) return
!     endif

   !first call to compute_SCPF
 call compute_SCPF(Qc(:,:)+sum(Qitot(:,:,:),dim=3),Qr(:,:),Qv_old(:,:),Qvi(:,:),           &
                   Pres(:,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,       &
                   SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)

!------------------------------------------------------------------------------------------!
#ifdef TIMING_P3
timer_description(3) = 'k_loop_main_processes'
call cpu_time(timer_start(3))
#endif

 k_loop_main_processes: do k = kbot,ktop,kdir
  do i = its,ite

    log_hydrometeorsPresent = qc(i,k)*iSCF(i,k).ge.qsmall .or. qr(i,k).ge.qsmall .or.    &
                              maxval(qitot(i,k,:)).ge.qsmall

    log_nucleationPossible = ( ((t(i,k).lt.trplpt .and. supi(i,k).ge.-0.05) .or.         &
                                (t(i,k).ge.trplpt .and. sup(i,k) .ge.-0.05 )) .or.       &
                                (scpf_ON .and. SCF(i,k).ge.0.01) )

   ! If there is the possibility of nucleation or droplet activation (i.e., if RH is
   ! relatively high) then calculate microphysical processes even if there is no
   ! existing condensate. Note only theta is updated from clipping and not temp,
   ! though temp is used for subsequent calculations. This change is tiny and
   ! therefore neglected.
   ! Note, the conditions for 'compute_procs' should be reexamined for SCPF.

    compute_procs: if ( (log_hydrometeorsPresent .and. .not.SCPF_on) .or.                &
                         log_nucleationPossible ) then

    ! initialize warm-phase process rates
       qcacc   = 0.;     qrevp   = 0.;     qccon   = 0.
       qcaut   = 0.;     qcevp   = 0.;     qrcon   = 0.
       ncacc   = 0.;     ncnuc   = 0.;     ncslf   = 0.
       ncautc  = 0.;     qcnuc   = 0.;     nrslf   = 0.
       nrevp   = 0.;     ncautr  = 0.

    ! initialize ice-phase  process rates
       qchetc  = 0.;     qisub   = 0.;     nrshdr  = 0.
       qcheti  = 0.;     qrcol   = 0.;     qcshd   = 0.
       qrhetc  = 0.;     qimlt   = 0.;     qccol   = 0.
       qrheti  = 0.;     qinuc   = 0.;     nimlt   = 0.
       nchetc  = 0.;     nccol   = 0.;     ncshdc  = 0.
       ncheti  = 0.;     nrcol   = 0.;     nislf   = 0.
       nrhetc  = 0.;     ninuc   = 0.;     qidep   = 0.
       nrheti  = 0.;     nisub   = 0.;     qwgrth  = 0.
       qrmul   = 0.;     nimul   = 0.;     qicol   = 0.
       nicol   = 0.;     qcmul   = 0.

   ! Liquid fraction microphysical process rates (log_LiquidFrac)
       qrmlt   = 0.;     qifrz    = 0.
       qlshd   = 0.;     nlshd    = 0.;     qlcon   = 0.
       qlevp   = 0.;     nlevp    = 0.;     qrcoll  = 0.
       nrcoll  = 0.;     qccoll   = 0.;     nccoll  = 0.
       qwgrth1 = 0.;     qwgrth1c = 0.;     qwgrth1r = 0.

   ! Full 3-moment rates
       zqccol = 0.;      zidep    = 0.;     zisub   = 0.
       zimlt  = 0.;      zislf    = 0.;     zishd   = 0.
       zqrcol = 0.

       log_wetgrowth = .false.

!----------------------------------------------------------------------
       predict_supersaturation: if (log_predictSsat) then

      ! Adjust cloud water and thermodynamics to prognostic supersaturation
      ! following the method in Grabowski and Morrison (2008).
      ! Note that the effects of vertical motion are assumed to dominate the
      ! production term for supersaturation, and the effects are sub-grid
      ! scale mixing and radiation are not explicitly included.

          dqsdT   = xxlv(i,k)*qvs(i,k)/(rv*t(i,k)*t(i,k))
          ab      = 1. + dqsdT*xxlv(i,k)*i_cp
          epsilon = (qv(i,k)-qvs(i,k)-ssat(i,k))/ab
          epsilon = max(epsilon,-qc(i,k))   ! limit adjustment to available water
        ! do not adjust upward if subsaturated
        ! otherwise this could result in positive adjustment
        ! (spurious generation ofcloud water) in subsaturated conditions
          !if (ssat(i,k).lt.0.) epsilon = min(0.,epsilon)
          epsilon = merge(min(0.,epsilon), epsilon, ssat(i,k).lt.0.)

        ! now do the adjustment
          if (abs(epsilon).ge.1.e-15) then
             qc(i,k)   = qc(i,k)+epsilon
             qv(i,k)   = qv(i,k)-epsilon
             th(i,k)   = th(i,k)+epsilon*i_exn(i,k)*xxlv(i,k)*i_cp
            ! recalculate variables if there was adjustment
             t(i,k)    = th(i,k)*(1.e-5*pres(i,k))**(rd*i_cp)
             qvs(i,k)  = qv_sat(t(i,k),pres(i,k),0)
             qvi(i,k)  = qv_sat(t(i,k),pres(i,k),1)
             sup(i,k)  = qv(i,k)/qvs(i,k)-1.
             supi(i,k) = qv(i,k)/qvi(i,k)-1.
             ssat(i,k) = qv(i,k)-qvs(i,k)
          endif

       endif predict_supersaturation

!----------------------------------------------------------------------

       log_hydrometeorsPresent = qc(i,k)*iSCF(i,k).ge.qsmall .or. qr(i,k).ge.qsmall      &
                                 .or. maxval(qitot(i,k,:)).ge.qsmall

       growth_decay_processes: if (log_hydrometeorsPresent) then
       ! if no hydrometeors present, skip growth/decay processes (for existing hydrometeors)
       ! and go straight to nucleation/activation

         !time/space varying physical variables
          mu     = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)
          dv     = 8.794e-5*t(i,k)**1.81/pres(i,k)
          sc     = mu/(rho(i,k)*dv)
          dum    = 1./(rv*t(i,k)**2)
          dqsdT  = xxlv(i,k)*qvs(i,k)*dum
          dqsidT = xxls(i,k)*qvi(i,k)*dum
          ab     = 1.+dqsdT*xxlv(i,k)*i_cp
          abi    = 1.+dqsidT*xxls(i,k)*i_cp
          kap    = 1.414e+3*mu
         !very simple temperature dependent aggregation efficiency
   !       if (t(i,k).lt.253.15) then
   !          eii = 0.1
   !       else if (t(i,k).ge.253.15.and.t(i,k).lt.268.15) then
   !          eii = 0.1+(t(i,k)-253.15)*0.06     ! linear ramp from 0.1 to 1 between 253.15 and 268.15 K  [note: 0.06 = (1./15.)*0.9]
   !       else if (t(i,k).ge.268.15) then
   !          eii = 1.
   !       endif
          if (t(i,k).lt.253.15) then
             eii = 0.001
          else if (t(i,k).ge.253.15.and.t(i,k).lt.trplpt) then
             eii = 0.001+(t(i,k)-253.15)*(0.3-0.001)*0.05
          else if (t(i,k).ge.trplpt) then
             eii = 0.3
          endif

          call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k),  &
                              cdist(i,k),cdist1(i,k),iSCF(i,k))


          call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),cdistr(i,k),            &
                             logn0r(i,k),iSPF(i,k))

        ! initialize inverse supersaturation relaxation timescale for combined ice categories
          epsi_tot = 0.
          epsiw_tot = 0.

          call impose_max_Ni(nitot(i,k,:),max_Ni,i_rho(i,k))

          iice_loop1: do iice = 1,nCat

             qitot_notsmall_1: if (qitot(i,k,iice).ge.qsmall) then

               !impose lower limits to prevent taking log of # < 0
                nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
                nr(i,k)         = max(nr(i,k),nsmall)

               !compute mean-mass ice diameters (estimated; rigorous approach to be implemented later)
               !dum2 = 500. !ice density
               !diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(nitot(i,k,iice)*dum2*pi))**thrd

               !Note: with scpf_on, no need to compute in-cloud values to access lookup tables since all
               !indices are ratios of mixing ratios, therefore *iSCF is both on num and denom.
               !Also true for the rime density, which is qirim*iSCF/birim*iSCF
               !Also true for dumj and dum3, which calculated using qr/nr

                call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),   &
                          birim(i,k,iice),rhop)

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                          dum7,isize,rimsize,liqsize,densize,qitot(i,k,iice),            &
                          nitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),rhop)

                call find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr(i,k),nr(i,k))

                trplmomice_1: if (.not. log_3momentIce) then

                 ! call to lookup table interpolation subroutines to get process rates
                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum7,0.,0.,0.,0.,      &
                                     dumjj,dumii,dumll,dumi,0,0)

                   f1pr02 = proc_from_LUT_main2mom(LUT1_UMS,args_r,args_i)
                   f1pr03 = proc_from_LUT_main2mom(LUT1_NAGG,args_r,args_i)
                   f1pr04 = proc_from_LUT_main2mom(LUT1_NRWAT,args_r,args_i)
                   f1pr05 = proc_from_LUT_main2mom(LUT1_VDEP,args_r,args_i)
                   f1pr09 = proc_from_LUT_main2mom(LUT1_QSML,args_r,args_i)
                   f1pr10 = proc_from_LUT_main2mom(LUT1_QLRG,args_r,args_i)
                   f1pr14 = proc_from_LUT_main2mom(LUT1_VDEP1,args_r,args_i)
                   f1pr16 = proc_from_LUT_main2mom(LUT1_RHOMM,args_r,args_i)

                   if (log_LiquidFrac) then
                      f1pr24 = proc_from_LUT_main2mom(LUT1_2M_VDM1,args_r,args_i)
                      f1pr25 = proc_from_LUT_main2mom(LUT1_2M_VDM2,args_r,args_i)
                      f1pr26 = proc_from_LUT_main2mom(LUT1_2M_VDM3,args_r,args_i)
                      f1pr27 = proc_from_LUT_main2mom(LUT1_2M_VDM4,args_r,args_i)
                      f1pr28 = proc_from_LUT_main2mom(LUT1_2M_SHED,args_r,args_i)
                   endif

                  ! ice-rain collection processes
                   if (qr(i,k).ge.qsmall) then
                     call args_for_LUT(args_r,args_i,dum1,dum3,dum4,dum5,dum7,0.,0.,0.,  &
                                       dumjj,dumii,dumll,dumj,dumi,0)
                     f1pr07 = proc_from_LUT_ir2mom(LUTR_NR,args_r,args_i)
                     f1pr08 = proc_from_LUT_ir2mom(LUTR_QR,args_r,args_i)
                   else
                      f1pr07 = -99. ! log space
                      f1pr08 = -99. ! log space
                   endif

                else ! trplmomice_1

                   call get_mui_rhoi(mu_i,f1pr16,dum6,dumzz,qitot(i,k,iice),             &
                                     nitot(i,k,iice),zitot(i,k,iice),dum1,dum4,dum5,     &
                                     dum7,dumjj,dumii,dumll,dumi,zsize,zqsize)

                   mu_i_s(iice) = mu_i

                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,    &
                                     dumzz,dumjj,dumii,dumll,dumi,0)

                   f1pr02 = proc_from_LUT_main3mom(LUT1_UMS,args_r,args_i)
                   f1pr03 = proc_from_LUT_main3mom(LUT1_NAGG,args_r,args_i)
                   f1pr04 = proc_from_LUT_main3mom(LUT1_NRWAT,args_r,args_i)
                   f1pr05 = proc_from_LUT_main3mom(LUT1_VDEP,args_r,args_i)
                   f1pr09 = proc_from_LUT_main3mom(LUT1_QSML,args_r,args_i)
                   f1pr10 = proc_from_LUT_main3mom(LUT1_QLRG,args_r,args_i)
                   f1pr14 = proc_from_LUT_main3mom(LUT1_VDEP1,args_r,args_i)

                   if (log_full3mom) then
                      f1pr29 = proc_from_LUT_main3mom(LUT1_3M_M6RI,args_r,args_i)
                      f1pr30 = proc_from_LUT_main3mom(LUT1_3M_M6D1,args_r,args_i)
                      f1pr31 = proc_from_LUT_main3mom(LUT1_3M_M6D2,args_r,args_i)
                      f1pr32 = proc_from_LUT_main3mom(LUT1_3M_M6L1,args_r,args_i)
                      f1pr33 = proc_from_LUT_main3mom(LUT1_3M_M6L2,args_r,args_i)
                      f1pr34 = proc_from_LUT_main3mom(LUT1_3M_M6AG,args_r,args_i)
                      f1pr37 = proc_from_LUT_main3mom(LUT1_3M_M6S1,args_r,args_i)
                      f1pr38 = proc_from_LUT_main3mom(LUT1_3M_M6S2,args_r,args_i)
                   endif

                   if (log_LiquidFrac) then
                      f1pr24 = proc_from_LUT_main3mom(LUT1_3M_VDM1,args_r,args_i)
                      f1pr25 = proc_from_LUT_main3mom(LUT1_3M_VDM2,args_r,args_i)
                      f1pr26 = proc_from_LUT_main3mom(LUT1_3M_VDM3,args_r,args_i)
                      f1pr27 = proc_from_LUT_main3mom(LUT1_3M_VDM4,args_r,args_i)
                      f1pr28 = proc_from_LUT_main3mom(LUT1_3M_SHED,args_r,args_i)
                      f1pr35 = proc_from_LUT_main3mom(LUT1_3M_M6SH,args_r,args_i)
                   endif

             ! ice-rain collection processes
                   if (qr(i,k).ge.qsmall) then
                      call args_for_LUT(args_r,args_i,dum1,dum3,dum4,dum5,dum6,dum7,     &
                                        0.,0.,dumzz,dumjj,dumii,dumll,dumj,dumi)
                      f1pr07 = proc_from_LUT_ir3mom(LUTR_NR,args_r,args_i)
                      f1pr08 = proc_from_LUT_ir3mom(LUTR_QR,args_r,args_i)
                      f1pr36 = proc_from_LUT_ir3mom(LUTR_M6,args_r,args_i)
                   else
                      f1pr07 = -99. ! log space
                      f1pr08 = -99. ! log space
                      f1pr36 = 0.
                   endif

                endif  trplmomice_1

             ! Compute ice diameter (volume equivalent -- for multi-cat)
                diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(nitot(i,k,iice)*f1pr16*pi))**thrd

             ! adjust Ni if needed to make sure mean size is in bounds (i.e. apply lambda limiters)
             !  note: the inv_Qmin (f1pr09) and inv_Qmax (f1pr10) are normalized, thus the
             !  max[min] values of nitot are obtained from multiplying these values by qitot.
                nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
                nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))

                if (log_3momentIce) then
                   call apply_mui_bounds_to_zi(zitot(i,k,iice),qitot(i,k,iice),          &
                                               nitot(i,k,iice),f1pr16)
                endif

   !.......................
   ! diagnose mu tendency from vertical transport and adjustment

   !.......................

             ! Determine additional collection efficiency factor to be applied to ice-ice collection.
             ! The computed values of qicol and nicol are multipiled by Eii_fact to gradually shut off collection
             ! if the ice in iice is highly rimed.
                if (qirim(i,k,iice)>0.) then
                      if ((qitot(i,k,iice)-qiliq(i,k,iice))>0.) then
                         tmp1 = qirim(i,k,iice)/(qitot(i,k,iice)-qiliq(i,k,iice))   !rime mass fraction
                      else
                         tmp1 = 0.   !no frozen mass implies no rime (prevents stale tmp1 read)
                      endif
                   if (tmp1.lt.0.6) then
                      Eii_fact(iice)=1.
                   else if (tmp1.ge.0.6.and.tmp1.lt.0.9) then
                   ! linear ramp from 1 to 0 for Fr between 0.6 and 0.9
                      Eii_fact(iice) = 1.-(tmp1-0.6)/0.3
                   else if (tmp1.ge.0.9) then
                      Eii_fact(iice) = 0.
                   endif
                else
                   Eii_fact(iice) = 1.
                endif

             endif qitot_notsmall_1 ! qitot > qsmall

   !----------------------------------------------------------------------
   ! Begin calculations of microphysical processes

   !......................................................................
   ! ice processes
   !......................................................................

   !.......................
   ! collection of liquid phases (cloud and rain) at T<=0
   !.......................

   ! here we multiply rates by air density, air density fallspeed correction
   ! factor, and collection efficiency since these parameters are not
   ! included in lookup table calculations
   ! for T < trplpt, assume collected cloud water and rain mass is instantly frozen
   ! note 'f1pr' values are normalized, so we need to multiply by N

   ! note with scpf_on, qccol, nccol, qccoll, nccoll are grid-mean
   ! ex: rhofaci is grid-mean, f1pr04 is grid-mean, qc*iSCF is in-cloud,
   ! eci is a constant, rho(i,k) is grid-mean, nitot*iSCF is in-cloud
   ! (qc*iSCF*nitot*iSCF)*SCF = (qc*nitot)*iSCF to obtain grid-mean qccol

             if (qitot(i,k,iice).ge.qsmall .and. qc(i,k)*iSCF(i,k).ge.qsmall .and.       &
                 t(i,k).le.trplpt) then
                tmp1 = eci*rho(i,k)*iSCF(i,k)
                qccol(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*nitot(i,k,iice)*tmp1
                nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*nitot(i,k,iice)*tmp1
                if (log_3momentIce .and. log_full3mom) then
                   zqccol(iice) = rhofaci(i,k)*f1pr29*qc(i,k)*tmp1
                endif
             endif

             if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and.                 &
                 t(i,k).le.trplpt) then
              ! note: f1pr08 and logn0r are already calculated as log_10 (in-precip)
              ! note: (SPF(i,k)-SPF_clr(i,k)) is SPF_cld(k)
                tmp1 = rho(i,k)*rhofaci(i,k)*eri*iSCF(i,k)*(SPF(i,k)-SPF_clr(i,k))
                qrcol(iice) = 10.**(f1pr08+logn0r(i,k))*nitot(i,k,iice)*tmp1
                nrcol(iice) = 10.**(f1pr07+logn0r(i,k))*nitot(i,k,iice)*tmp1
                if (log_3momentIce .and. log_full3mom) then
                   zqrcol(iice) = 10.**(logn0r(i,k))*f1pr36*tmp1
                endif
             endif

   !.......................
   ! collection of liquid phases (cloud and rain) at T>0
   !.......................

             ! for T > trplpt, assume cloud water is collected and shed as rain drops
             liqfrac_1: if (log_LiquidFrac) then

             ! assume cloud water is collected by qiliq
                if (qitot(i,k,iice).ge.qsmall .and. qc(i,k)*iSCF(i,k).ge.qsmall .and.    &
                    t(i,k).gt.trplpt) then
                   tmp1 = eci*rho(i,k)*iSCF(i,k)
                   qccoll(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*nitot(i,k,iice)*tmp1
                   nccoll(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*nitot(i,k,iice)*tmp1
                   if(log_3momentIce .and. log_full3mom) then
                      zqccol(iice) = rhofaci(i,k)*f1pr29*qc(i,k)*tmp1
                   endif
                endif
                ! assume collected rain by qiliq
                if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and.              &
                    t(i,k).gt.trplpt) then
                   ! note: f1pr08 and logn0r are already calculated as log_10
                   tmp1 = rho(i,k)*rhofaci(i,k)*eri*iSCF(i,k)*(SPF(i,k)-SPF_clr(i,k))
                   qrcoll(iice) = 10.**(f1pr08+logn0r(i,k))*nitot(i,k,iice)*tmp1
                   nrcoll(iice) = 10.**(f1pr07+logn0r(i,k))*nitot(i,k,iice)*tmp1
                   if (log_3momentIce .and. log_full3mom) then
                       zqrcol(iice) = 10.**(logn0r(i,k))*f1pr36*tmp1
                   endif
                endif

             else  ! liqfrac_1

             ! assume cloud water is collected and shed as rain drops (original code)
                if (qitot(i,k,iice).ge.qsmall .and. qc(i,k)*iSCF(i,k).ge.qsmall .and.    &
                    t(i,k).gt.trplpt) then
                ! sink for cloud water mass and number, note qcshed is source for rain mass
                   tmp1 = eci*rho(i,k)*nitot(i,k,iice)*iSCF(i,k)
                   qcshd(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*tmp1
                   nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*tmp1
                ! source for rain number, assume 1 mm drops are shed
                   ncshdc(iice) = qcshd(iice)*1.923e+6
                ! note: for full 3-moment, there is no impact of shedding on zitot when liquid fraction is off
                !       because all cloud water collected is instantly shed (not affecting ice particles)
                endif
                ! assume collected rain number is shed as 1 mm drops (original code)
                ! collection of rain above freezing does not impact total rain mass
                if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and.              &
                    t(i,k).gt.trplpt) then
                ! rain number sink due to collection
                   nrcol(iice)  = 10.**(f1pr07 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri* &
                                  nitot(i,k,iice)*iSCF(i,k)*(SPF(i,k)-SPF_clr(i,k))
                ! rain number source due to shedding = collected rain mass/mass of 1 mm drop
                ! for opt comment dum (since it is not used)
                !   dum    = 10.**(f1pr08 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(i,k)*(SPF(i,k)-SPF_clr(i,k))
                ! for now neglect shedding of ice collecting rain above freezing, since snow is
                ! not expected to shed in these conditions (though more hevaily rimed ice would be
                ! expected to lead to shedding)
                !    nrshdr(iice) = dum*1.923e+6   ! 1./5.2e-7, 5.2e-7 is the mass of a 1 mm raindrop
                endif

             endif liqfrac_1
   !...................................
   ! collection between ice categories

            iceice_interaction1:  if (iice.ge.2) then
   !        iceice_interaction1:  if (.false.) then       !for testing (to suppress ice-ice interaction)

            !note:  In this version, lookupTable_2 (LT2, for ice category interactions) is computed for a maximum
            !       mean ice size of Dm_max=2000.e-6 m (the old lambda_i limiter); thus it is compatible with
            !       use of LT1-v5.2_2momI (with Dm_max=2000.e-6) [i.e. for log_3momentIce=.false.] but not with
            !       LT1-v5.3_3momI (with Dm_max=400000.e-6).  This means that this version can still be
            !       run with the 3momI + nCat>1 configuration, but the ice-ice interactions between different
            !       categories (in this 'iceice_interaction1' block) is suppressed.
            !       In a forthcoming version, both LT1-2momI and LT2 (and LT1-3momI) will all be computed
            !       using the unconstrained size limited (i.e. Dm_max=400000.e-6).

                qitot_notsmall: if (qitot(i,k,iice).ge.qsmall) then
                   catcoll_loop: do catcoll = 1,iice-1
                      qitotcatcoll_notsmall: if (qitot(i,k,catcoll).ge.qsmall) then

                     ! first, calculate collection of catcoll category by iice category

                         call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,  &
                              dumjjc,dum1,dum4,dum5,dum7,dum1c,dum4c,dum5c,dum7c,        &
                              iisize,rimsize,densize,qitot(i,k,iice),qitot(i,k,catcoll), &
                               nitot(i,k,iice),nitot(i,k,catcoll),qirim(i,k,iice),       &
                               qirim(i,k,catcoll),birim(i,k,iice),birim(i,k,catcoll),    &
                               qiliq(i,k,iice),qiliq(i,k,catcoll))

                         call args_for_LUT(args_r,args_i,dum1c,dum4c,dum5c,dum7c,dum1,   &
                                   dum4,dum5,dum7,dumjjc,dumiic,dumic,dumjj,dumii,dumi)
                         f1pr17 = proc_from_LUT_ii(LUTII_DN,args_r,args_i)
                         f1pr18 = proc_from_LUT_ii(LUTII_DM,args_r,args_i)

                       ! note: need to multiply by air density, air density fallspeed correction factor,
                       !       and N of the collectee and collector categories for process rates nicol and qicol,
                       !       first index is the collectee, second is the collector
                         nicol(catcoll,iice) = f1pr17*rhofaci(i,k)*rho(i,k)*             &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(i,k)
                         qicol(catcoll,iice) = f1pr18*rhofaci(i,k)*rho(i,k)*             &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(i,k)

                         nicol(catcoll,iice) = eii*Eii_fact(iice)*nicol(catcoll,iice)
                         qicol(catcoll,iice) = eii*Eii_fact(iice)*qicol(catcoll,iice)
                         nicol(catcoll,iice) = min(nicol(catcoll,iice), nitot(i,k,catcoll)*i_dt)
                         qicol(catcoll,iice) = min(qicol(catcoll,iice), qitot(i,k,catcoll)*i_dt)

                     ! second, calculate collection of iice category by catcoll category

                       !needed to force consistency between qirim(catcoll) and birim(catcoll) (not for rhop)
                         call calc_bulkRhoRime(qitot(i,k,catcoll),qirim(i,k,catcoll),    &
                                          qiliq(i,k,catcoll),birim(i,k,catcoll),rhop)

                         call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,  &
                                  dumjjc,dum1,dum4,dum5,dum7,dum1c,dum4c,dum5c,dum7c,    &
                                  iisize,rimsize,densize,qitot(i,k,catcoll),             &
                                  qitot(i,k,iice),nitot(i,k,catcoll),nitot(i,k,iice),    &
                                  qirim(i,k,catcoll),qirim(i,k,iice),birim(i,k,catcoll), &
                                  birim(i,k,iice),qiliq(i,k,catcoll),qiliq(i,k,iice))

                         call args_for_LUT(args_r,args_i,dum1c,dum4c,dum5c,dum7c,dum1,   &
                                   dum4,dum5,dum7,dumjjc,dumiic,dumic,dumjj,dumii,dumi)
                         f1pr17 = proc_from_LUT_ii(LUTII_DN,args_r,args_i)
                         f1pr18 = proc_from_LUT_ii(LUTII_DM,args_r,args_i)

                         nicol(iice,catcoll) = f1pr17*rhofaci(i,k)*rho(i,k)*             &
                                               nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(i,k)
                         qicol(iice,catcoll) = f1pr18*rhofaci(i,k)*rho(i,k)*             &
                                               nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(i,k)

                        ! note: Eii_fact applied to the collector category
                         nicol(iice,catcoll) = eii*Eii_fact(catcoll)*nicol(iice,catcoll)
                         qicol(iice,catcoll) = eii*Eii_fact(catcoll)*qicol(iice,catcoll)
                         nicol(iice,catcoll) = min(nicol(iice,catcoll),nitot(i,k,iice)*i_dt)
                         qicol(iice,catcoll) = min(qicol(iice,catcoll),qitot(i,k,iice)*i_dt)

                      endif qitotcatcoll_notsmall
                   enddo catcoll_loop
                endif qitot_notsmall

             endif iceice_interaction1

   !.............................................
   ! self-collection of ice (in a given category)

       ! here we multiply rates by collection efficiency, air density,
       ! and air density correction factor since these are not included
       ! in the lookup table calculations
       ! note 'f1pr' values are normalized, so we need to multiply by N

             if (qitot(i,k,iice).ge.qsmall) then
                nislf(iice) = f1pr03*rho(i,k)*eii*Eii_fact(iice)*rhofaci(i,k)*           &
                              nitot(i,k,iice)*nitot(i,k,iice)*iSCF(i,k)
                if (log_3momentIce .and. log_full3mom) then
                  ! NOTE: already correct sign from lookup table, thus not multiplied by -1
                   zislf(iice) = f1pr34*rho(i,k)*eii*Eii_fact(iice)*rhofaci(i,k)*        &
                                 nitot(i,k,iice)*iSCF(i,k)
                endif
             endif


   !............................................................
   ! melting

       ! need to add back accelerated melting due to collection of ice mass by rain (pracsw1)
       ! note 'f1pr' values are normalized, so we need to multiply by N

             liqfrac_2: if (log_LiquidFrac) then
             ! some portion of the melted water stays into qiliq --> qimlt(iice) (D>Dth)
             ! the other portion melts into rain --> qrmlt(iice) (D<=Dth)
                if ((qitot(i,k,iice)-qiliq(i,k,iice)).ge.qsmall .and. t(i,k).gt.trplpt) then
                   qsat0 = 0.622*e0/(pres(i,k)-e0)
                   tmp1 = 0.
                   qrmlt(iice) = ((f1pr24+f1pr25*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**   &
                                 0.5)*((t(i,k)-trplpt)*kap-rho(i,k)*xxlv(i,k)*dv*(qsat0- &
                                 Qv_cld(i,k)))*2.*pi/xlf(i,k)+tmp1)*nitot(i,k,iice)
                   qimlt(iice) = ((f1pr26+f1pr27*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**   &
                                 0.5)*((t(i,k)-trplpt)*kap-rho(i,k)*xxlv(i,k)*dv*(qsat0- &
                                 Qv_cld(i,k)))*2.*pi/xlf(i,k)+tmp1)*nitot(i,k,iice)
                   qrmlt(iice) = max(qrmlt(iice),0.)
                   qimlt(iice) = max(qimlt(iice),0.)
                   ! Make sure both terms are bounded (necessary for conservation check)
                   sinks = qimlt(iice)+qrmlt(iice)
                   if (sinks.gt.0. .and. sinks .gt. (qitot(i,k,iice)-qiliq(i,k,iice))*   &
                       i_dt) then
                       ratio = (qitot(i,k,iice)-qiliq(i,k,iice))*i_dt/sinks
                       qrmlt(iice) = qrmlt(iice)*ratio
                       qimlt(iice) = qimlt(iice)*ratio
                   endif
                   nimlt(iice) = qrmlt(iice)*(nitot(i,k,iice)/(qitot(i,k,iice)-          &
                                 qiliq(i,k,iice)))
                   if (log_3momentIce) then
                      zimlt(iice) = -((f1pr24*f1pr32+f1pr25*f1pr33*sc**thrd*             &
                                    (rhofaci(i,k)*rho(i,k)/mu)**0.5)*((t(i,k)-trplpt)*   &
                                    kap-rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(i,k)))*2.*   &
                                    pi/xlf(i,k)+tmp1)
                   endif
                endif

             else   !liqfrac_2

                if (qitot(i,k,iice).ge.qsmall .and. t(i,k).gt.trplpt) then
                   qsat0 = 0.622*e0/(pres(i,k)-e0)
              !  tmp1=cpw/xlf(i,k)*(t(i,k)-trplpt)*(pracsw1+qcshd(iice))
              ! currently enhanced melting from collision is neglected
              ! tmp1=cpw/xlf(i,k)*(t(i,k)-trplpt)*(pracsw1)
                   tmp1 = 0.
              ! qrmlt(iice)=(f1pr05+f1pr14*sc**0.3333*(rhofaci(i,k)*rho(i,k)/mu)**0.5)* &
              !       (t(i,k)-trplpt)*2.*pi*kap/xlf(i,k)+tmp1
              ! include RH dependence
                   qrmlt(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**   &
                                 0.5)*((t(i,k)-trplpt)*kap-rho(i,k)*xxlv(i,k)*dv*(qsat0- &
                                 Qv_cld(i,k)))*2.*pi/xlf(i,k)+tmp1)*nitot(i,k,iice)
                   qrmlt(iice) = max(qrmlt(iice),0.)
                   nimlt(iice) = qrmlt(iice)*(nitot(i,k,iice)/qitot(i,k,iice))
                   if (log_3momentIce .and. log_full3mom)                                &
                      zimlt(iice) = -((f1pr05*f1pr30+f1pr14*f1pr31*sc**thrd*             &
                                    (rhofaci(i,k)*rho(i,k)/mu)**0.5)*((t(i,k)-trplpt)*   &
                                    kap-rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(i,k)))*2.*   &
                                    pi/xlf(i,k)+tmp1)
                endif

             endif liqfrac_2

   !............................................................
   ! calculate wet growth

       ! Musil (1970), JAS, eq. (A9)
       ! note 'f1pr' values are normalized, so we need to multiply by N

             if (qitot(i,k,iice).ge.qsmall .and. (qc(i,k)+qr(i,k)).ge.1.e-6 .and.        &
                 t(i,k).lt.trplpt) then

                qsat0  = 0.622*e0/(pres(i,k)-e0)
                qwgrth(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**     &
                               0.5)*2.*pi*(rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(i,k))-    &
                               (t(i,k)-trplpt)*kap)/(xlf(i,k)+cpw*(t(i,k)-trplpt)))*     &
                               nitot(i,k,iice)
                qwgrth(iice) = max(qwgrth(iice),0.)

                if (log_LiquidFrac) then
                    ! Densification from wet growth turn off as it is now contained into qiliq
                    ! log_wetgrowth(iice) = .false. ! set to false at beginning of subroutine
                    ! NOTE: change variable name from log_wetgrowth to log_densify
                    tmp1 = max(0.,(qccol(iice)+qrcol(iice))-qwgrth(iice))
                    if (tmp1.ge.1.e-10) then
                    !   qwgrth1(iice)  = qrcol(iice)+qccol(iice) ! not used anymore
                       ! note: For full 3-moment ice, do not adjust zqccol and zqrcol from wet growth
                       !       because both ice riming and retention of liquid increase in zitot.
                       !       The difference in density between rime and collected liquid is neglected for zitot.
                       qwgrth1c(iice) = qccol(iice)
                       qwgrth1r(iice) = qrcol(iice)
                       qrcol(iice)    = 0.
                       qccol(iice)    = 0.
                    endif

                else

                   !calculate shedding for wet growth
                   tmp1    = max(0.,(qccol(iice)+qrcol(iice))-qwgrth(iice))
                   if (tmp1.ge.1.e-10) then
                      nrshdr(iice) = nrshdr(iice) + tmp1*1.923e+6   ! 1/5.2e-7, 5.2e-7 is the mass of a 1 mm raindrop
                      if ((qccol(iice)+qrcol(iice)).ge.1.e-10) then
                         tmp2  = 1./(qccol(iice)+qrcol(iice))
                         qcshd(iice) = qcshd(iice) + tmp1*qccol(iice)*tmp2
                         qccol(iice) = qccol(iice) - tmp1*qccol(iice)*tmp2
                         qrcol(iice) = qrcol(iice) - tmp1*qrcol(iice)*tmp2
                        ! adjust zqccol and zqrcol to account for wet growth and resulting shedding of collected liquid
                         if (log_3momentIce .and. log_full3mom) then
                            zqccol(iice) = zqccol(iice) - tmp1*zqccol(iice)*tmp2
                            zqrcol(iice) = zqrcol(iice) - tmp1*zqrcol(iice)*tmp2
                         endif
                     endif
                   ! densify due to wet growth
                     log_wetgrowth(iice) = .true.
                   endif
                endif ! log_LiquidFrac

             endif  !if qitot>qsmall, qc+qr>1.e-6, T<273.


   !-----------------------------
   ! calcualte total inverse ice relaxation timescale combined for all ice categories
   ! note 'f1pr' values are normalized, so we need to multiply by N

   !Note (BUG) insert *iSCF(i,k) because epsi and epsiw needs to be in-cloud (to be done)

           !if (log_LiquidFrac) then
             if (qitot(i,k,iice).ge.qsmall) then
                tmp1 = sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5
                tmp2 = 2.*pi*rho(i,k)*dv
                if ((qiliq(i,k,iice)/qitot(i,k,iice)).lt.0.01) then
                    epsi(iice)  = ((f1pr05+f1pr14*tmp1)*tmp2)*nitot(i,k,iice)
                    epsi_tot    = epsi_tot + epsi(iice)
                    epsiw(iice) = 0.
                else
                    epsiw(iice) = ((f1pr05+f1pr14*tmp1)*tmp2)*nitot(i,k,iice)
                    epsiw_tot   = epsiw_tot + epsiw(iice)
                    epsi(iice)  = 0.
                endif

                if (log_3momentIce) then
                   epsiz(iice)   = (f1pr30+f1pr31*tmp1)*tmp2
                   epsizsb(iice) = (f1pr37+f1pr38*tmp1)*tmp2
                endif

             else
                epsi(iice)    = 0.
                epsiw(iice)   = 0.
                epsiz(iice)   = 0.
                epsizsb(iice) = 0.
             endif

   !............................................................
   ! refreezing of mixed-phase ice particles
   ! only with predicted liquid fraction (log_liqFrac)
   !............................................................
   ! shedding
   ! only with predicted liquid fraction (log_liqFrac)
   ! without log_liqFrac, shedding is included in the wet growth process
   !............................................................

          if (log_LiquidFrac) then

             if (qiliq(i,k,iice).ge.qsmall .and. qitot(i,k,iice).ge.qsmall) then
             ! Refreezing
                if (t(i,k).lt.trplpt) then
                  qsat0  = 0.622*e0/(pres(i,k)-e0)
                  qifrz(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**    &
                                0.5)*2.*pi*(rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(i,k))-   &
                                (t(i,k)-trplpt)*kap)/xlf(i,k))*nitot(i,k,iice)
                  qifrz(iice) = min(max(qifrz(iice),0.),qiliq(i,k,iice)*i_dt)
                endif
             ! Shedding
                tmp1=0.
                if ((qitot(i,k,iice)-qiliq(i,k,iice)).ge.qsmall)                         &
                  tmp1 = qirim(i,k,iice)/(qitot(i,k,iice)-qiliq(i,k,iice))
             ! Shedding
                qlshd(iice) = tmp1*f1pr28*nitot(i,k,iice)*qiliq(i,k,iice)/qitot(i,k,iice)
                qlshd(iice) = min(max(0.,qlshd(iice)),qiliq(i,k,iice)*i_dt)
                nlshd(iice) = qlshd(iice)*1.928e+6
                if (log_3momentIce .and. log_full3mom)                                   &
                 zishd(iice) = -tmp1*f1pr35*qiliq(i,k,iice)/qitot(i,k,iice)
             endif

          endif  ! log_LiquidFrac

   !.........................
   ! calculate rime density

   !     FUTURE:  Add source term for birim (=qccol/rhorime_c) so that all process rates calculations
   !              are done together, before conservation.

        ! NOTE: Tc (ambient) is assumed for the surface temperature.  Technically,
        ! we should diagose graupel surface temperature from heat balance equation.
        ! (but the ambient temperature is a reasonable approximation; tests show
        ! very little sensitivity to different assumed values, Milbrandt and Morrison 2012).

         ! Compute rime density: (based on parameterization of Cober and List, 1993 [JAS])
         ! for simplicty use mass-weighted ice and droplet/rain fallspeeds

           ! if (qitot(i,k,iice).ge.qsmall .and. t(i,k).lt.trplpt) then
           !  NOTE:  condition applicable for cloud only; modify when rain is added back
             if (qccol(iice).ge.qsmall .and. t(i,k).lt.trplpt) then

              ! get mass-weighted mean ice fallspeed
                vtrmi1(i,k) = f1pr02*rhofaci(i,k)
                iTc   = 1./min(-0.001,t(i,k)-trplpt)

             ! cloud:
                if (qc(i,k)*iSCF(i,k).ge.qsmall) then
                 ! droplet fall speed
                 ! (use Stokes' formulation (thus use analytic solution)
                   Vt_qc(i,k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))/(lamc(i,k)**bcn*        &
                                gamma(mu_c(i,k)+4.))
                 ! use mass-weighted mean size
                   D_c = (mu_c(i,k)+4.)/lamc(i,k)
                   V_impact  = abs(vtrmi1(i,k)-Vt_qc(i,k))
                   Ri        = -(0.5e+6*D_c)*V_impact*iTc
   !               Ri        = max(1.,min(Ri,8.))
                   Ri        = max(1.,min(Ri,12.))
                   if (Ri.le.8.) then
                      rhorime_c(iice)  = (0.051 + 0.114*Ri - 0.0055*Ri**2)*1000.
                   else
                   ! for Ri > 8 assume a linear fit between 8 and 12,
                   ! rhorime = 900 kg m-3 at Ri = 12
                   ! this is somewhat ad-hoc but allows a smoother transition
                   ! in rime density up to wet growth
                      rhorime_c(iice)  = 611.+72.25*(Ri-8.)
                   endif

                endif    !if qc*iSCF>qsmall

             ! rain:
               ! assume rime density for rain collecting ice is 900 kg/m3
   !            if (qr(i,k).ge.qsmall) then
   !               D_r = (mu_r(i,k)+1.)/lamr(i,k)
   !               V_impact  = abs(vtrmi1(i,k)-Vt_qr(i,k))
   !               Ri        = -(0.5e+6*D_r)*V_impact*iTc
   !               Ri        = max(1.,min(Ri,8.))
   !               rhorime_r(iice)  = (0.051 + 0.114*Ri - 0.0055*Ri*Ri)*1000.
   !            else
   !               rhorime_r(iice) = 400.
   !            endif

             else
                rhorime_c(iice) = 400.
   !            rhorime_r(iice) = 400.
             endif ! qi > qsmall and T < trplpt

       !--------------------
          enddo iice_loop1
       !--------------------

   !............................................................
   ! contact and immersion freezing droplets

   ! Note (BUG): I think qcheti and ncheti should be multiply by *SCF(i,k) to be grid-mean

   ! contact freezing currently turned off
   !         dum=7.37*t(i,k)/(288.*10.*pres(i,k))/100.
   !         dap=4.*pi*1.38e-23*t(i,k)*(1.+dum/rin)/ &
   !                (6.*pi*rin*mu)
   !         nacnt=exp(-2.80+0.262*(trplpt-t(i,k)))*1000.

          if (qc(i,k)*iSCF(i,k).ge.qsmall .and. t(i,k).le.269.15) then
   !         qchetc(iice) = pi*pi/3.*Dap*Nacnt*rhow*cdist1(i,k)*gamma(mu_c(i,k)+5.)/lamc(i,k)**4
   !         nchetc(iice) = 2.*pi*Dap*Nacnt*cdist1(i,k)*gamma(mu_c(i,k)+2.)/lamc(i,k)
   ! for future: calculate gamma(mu_c+4) in one place since its used multiple times
             dum   = (1./lamc(i,k))**3
   !         qcheti(iice_dest) = cons6*cdist1(i,k)*gamma(7.+pgam(i,k))*exp(aimm*(trplpt-t(i,k)))*dum**2
   !         ncheti(iice_dest) = cons5*cdist1(i,k)*gamma(pgam(i,k)+4.)*exp(aimm*(trplpt-t(i,k)))*dum

   !           Q_nuc = cons6*cdist1(i,k)*gamma(7.+mu_c(i,k))*exp(aimm*(trplpt-t(i,k)))*dum**2
   !           N_nuc = cons5*cdist1(i,k)*gamma(mu_c(i,k)+4.)*exp(aimm*(trplpt-t(i,k)))*dum
   !          tmp1 = cdist1(i,k)*exp(aimm*(trplpt-t(i,k)))
   !          Q_nuc = cons6*gamma(7.+mu_c(i,k))*tmp1*dum**2
   !          N_nuc = cons5*gamma(mu_c(i,k)+4.)*tmp1*dum
              tmpdbl1  = dexp(dble(aimm*(trplpt-t(i,k))))
              tmpdbl2  = dble(dum)
              Q_nuc = cons6*cdist1(i,k)*gamma(7.+mu_c(i,k))*tmpdbl1*tmpdbl2**2
              N_nuc = cons5*cdist1(i,k)*gamma(mu_c(i,k)+4.)*tmpdbl1*tmpdbl2


             if (nCat>1) then
               !determine destination ice-phase category:
                dum1  = 900.     !density of new ice
                D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
                call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,      &
                                        deltaD_init,iice_dest)

                if (global_status /= STATUS_OK) return
             else
                iice_dest = 1
             endif
             qcheti(iice_dest) = Q_nuc
             ncheti(iice_dest) = N_nuc
          endif

   !............................................................
   ! immersion freezing of rain
   ! for future: get rid of log statements below for rain freezing

          if (qr(i,k)*iSPF(i,k).ge.qsmall.and.t(i,k).le.269.15) then

   !         Q_nuc = cons6*exp(log(cdistr(i,k))+log(gamma(7.+mu_r(i,k)))-6.*log(lamr(i,k)))*exp(aimm*(trplpt-t(i,k)))*SPF(i,k)
   !         N_nuc = cons5*exp(log(cdistr(i,k))+log(gamma(mu_r(i,k)+4.))-3.*log(lamr(i,k)))*exp(aimm*(trplpt-t(i,k)))*SPF(i,k)
             tmpdbl1 = dexp(dble(log(cdistr(i,k))+log(gamma(7.+mu_r(i,k)))-6.*log(lamr(i,k))))
             tmpdbl2 = dexp(dble(log(cdistr(i,k))+log(gamma(mu_r(i,k)+4.))-3.*log(lamr(i,k))))
             tmpdbl3 = dexp(dble(aimm*(trplpt-t(i,k))))
             Q_nuc = cons6*sngl(tmpdbl1*tmpdbl3)*SPF(i,k)
             N_nuc = cons5*sngl(tmpdbl2*tmpdbl3)*SPF(i,k)

             if (nCat>1) then
                !determine destination ice-phase category:
                dum1  = 900.     !density of new ice
                D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
                call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,       &
                                  deltaD_init,iice_dest)
                if (global_status /= STATUS_OK) return
              else
                 iice_dest = 1
              endif
              qrheti(iice_dest) = Q_nuc
              nrheti(iice_dest) = N_nuc
          endif


   !......................................
   ! rime splintering (Hallet-Mossop 1974)

   ! Rime splintering occurs from accretion of large drops (>25 microns diameter)
   ! by large, rimed, fully-frozen ice.  For simplicitly it is assumed that all
   ! accreted rain contributes to splintering, but accreted cloud water does not.
   ! It only occurs in the temperature range of -8C < T -3C.

          if (nCat.eq.1) then
          !for nCat = 1, rime-splinter is shut off during the summer (dilution of rimed ice sizes
          !weakens convection) but on during the winter.  The temperature threshold of +9 C (282 K)
          !is used as a proxy for winter/summer
             log_hmossopOn = t(i,kbot).lt.282.
             Dmin_HM       = 250.e-6
             Dinit_HM      =  10.e-6
          else
             log_hmossopOn = .true.
             Dmin_HM       = 1000.e-6
             Dinit_HM      =   10.e-6
          endif

          calc_HM:  if (log_hmossopOn .and. t(i,k).gt.265.15 .and. t(i,k).lt.270.15) then

             if (nCat>1) then
                !determine destination ice-phase category
                D_new = 10.e-6 !assumes ice crystals from rime splintering are tiny
                call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,      &
                                        deltaD_init,iice_dest)
                if (global_status /= STATUS_OK) return
             else
                iice_dest = 1
             endif

             iice_loop_HM:  do iice = 1,nCat

                ice_present:  if (qitot(i,k,iice).ge.qsmall .and. qirim(i,k,iice).ge.qsmall) then

                   tmp1 = qiliq(i,k,iice)/qitot(i,k,iice)                    ! liquid fraction

                   HM_conditions_met: if (diam_ice(i,k,iice).ge.Dmin_HM .and.            &
                                          tmp1.lt.0.1) then

                      if (t(i,k).lt.270.15 .and. t(i,k).gt.268.15) then
                         dum = (270.15-t(i,k))*0.5
                      elseif (t(i,k).le.268.15 .and. t(i,k).ge.265.15) then
                         dum = (t(i,k)-265.15)*thrd
                      endif

                      HM_cloud: if (qccol(iice).gt.0. .and. nCat.eq.1) then
                        !rime splintering from riming of cloud droplets:
                        !  (commented out to exclude rime splintering from accretion of cloud,
                        !   but code is retained in case of possible future use)
                        dum1 = 35.e+4*qccol(iice)*dum*1000. ! 1000 is to convert kg to g
                        dum2 = dum1*piov6*900.*Dinit_HM**3
                        qccol(iice) = qccol(iice)-dum2      ! subtract splintering from rime mass transfer
                        if (qccol(iice) .lt. 0.) then
                           dum2 = qccol(iice) + dum2
                           qccol(iice) = 0.
                        endif
                        qcmul(iice_dest) = qcmul(iice_dest) + dum2
                        nimul(iice_dest) = nimul(iice_dest) + dum1
                      endif HM_cloud


                      HM_rain: if (qrcol(iice).gt.0.) then
                        !rime splintering from riming of rain:
                        dum1 = 35.e+4*qrcol(iice)*dum*1000.  ! 1000 is to convert kg to g
                        dum2 = dum1*piov6*900.*Dinit_HM**3
                        qrcol(iice) = qrcol(iice) - dum2     ! subtract splintering from rime mass transfer
                        if (qrcol(iice) .lt. 0.) then
                           dum2 = qrcol(iice) + dum2
                           qrcol(iice) = 0.
                        endif
                        qrmul(iice_dest) = qrmul(iice_dest) + dum2
                        nimul(iice_dest) = nimul(iice_dest) + dum1
                      endif HM_rain

                   endif HM_conditions_met

                endif ice_present

             enddo iice_loop_HM

          endif calc_HM

   !....................................................
   ! condensation/evaporation and deposition/sublimation
   !   (use semi-analytic formulation)

          !calculate rain evaporation including ventilation
          if (qr(i,k)*iSPF(i,k).ge.qsmall) then
             call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,i_dum3,      &
                                             mu_r(i,k),lamr(i,k))
            !interpolate value at mu_r
             dum1 = revap_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                    (revap_table(dumii+1,dumjj)-revap_table(dumii,dumjj))
            !interoplate value at mu_r+1
             dum2 = revap_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                    (revap_table(dumii+1,dumjj+1)-revap_table(dumii,dumjj+1))
            !final interpolation
             dum  = dum1+(rdumjj-real(dumjj))*(dum2-dum1)

             epsr = 2.*pi*cdistr(i,k)*rho(i,k)*dv*(f1r*gamma(mu_r(i,k)+2.)/(lamr(i,k))   &
                     +f2r*(rho(i,k)/mu)**0.5*sc**thrd*dum)
          else
             epsr = 0.
          endif

          if (qc(i,k)*iSCF(i,k).ge.qsmall) then
             epsc = 2.*pi*rho(i,k)*dv*cdist(i,k)
          else
             epsc = 0.
          endif

          i_abi = 1./abi
          !if (log_LiquidFrac) then
            xx   = epsc + epsr + epsi_tot*(1.+xxls(i,k)*i_cp*dqsdT)*i_abi + epsiw_tot
          !else
          !  if (t(i,k).lt.trplpt) then
          !     xx   = epsc + epsr + epsi_tot*(1.+xxls(i,k)*i_cp*dqsdT)*i_abi
          !  else
          !     xx   = epsc + epsr
          !  endif
          !endif

          dumqvi = qvi(i,k)   !no modification due to latent heating
   !----
   ! !      ! modify due to latent heating from riming rate
   ! !      !   - currently this is done by simple linear interpolation
   ! !      !     between conditions for dry and wet growth --> in wet growth it is assumed
   ! !      !     that particle surface temperature is at 0 C and saturation vapor pressure
   ! !      !     is that with respect to liquid. This simple treatment could be improved in the future.
   ! !        if (qwgrth(iice).ge.1.e-20) then
   ! !           dum = (qccol(iice)+qrcol(iice))/qwgrth(iice)
   ! !        else
   ! !           dum = 0.
   ! !        endif
   ! !        dumqvi = qvi(i,k) + dum*(qvs(i,k)-qvi(i,k))
   ! !        dumqvi = min(qvs(i,k),dumqvi)
   !====


        ! 'A' term including ice (Bergeron process)
        ! note: qv and T tendencies due to mixing and radiation are
        ! currently neglected --> assumed to be much smaller than cooling
        ! due to vertical motion which IS included

        ! The equivalent vertical velocity is set to be consistent with dT/dt
        ! since -g/cp*dum = dT/dt therefore dum = -cp/g*dT/dt
        ! note this formulation for dT/dt is not exact since pressure
        ! may change and t and t_old were both diagnosed using the current pressure
        ! errors from this assumption are small
          dum = -cp/g*(t(i,k)-t_old(i,k))*i_dt

   !       dum = qvs(i,k)*rho(i,k)*g*uzpl(i,k)/max(1.e-3,(pres(i,k)-polysvp1(t(i,k),0)))

          !if (log_LiquidFrac) then
            aaa = (qv(i,k)-qv_old(i,k))*i_dt - dqsdT*(-dum*g*i_cp)-(qvs(i,k)-dumqvi)*    &
                  (1.+xxls(i,k)*i_cp*dqsdT)*i_abi*epsi_tot
          !else
          !  if (t(i,k).lt.trplpt) then
          !     aaa = (qv(i,k)-qv_old(i,k))*i_dt - dqsdT*(-dum*g*i_cp)-(qvs(i,k)-dumqvi)* &
          !           (1.+xxls(i,k)*i_cp*dqsdT)*i_abi*epsi_tot
          !  else
          !     aaa = (qv(i,k)-qv_old(i,k))*i_dt - dqsdT*(-dum*g*i_cp)
          !  endif
          !endif

          xx  = max(1.e-20,xx)   ! set lower bound on xx to prevent division by zero
          i_xx = 1./xx

          ssat_cld  = Qv_cld(i,k) - qvs(i,k) !in-cloud  sub/sur-saturation w.r.t. liq
          ssat_clr  = Qv_clr(i,k) - qvs(i,k) !clear-sky sub/sur-saturation w.r.t. liq
          !mix of in-cloud/clearsky sub/sur-saturation w.r.t. liqfor rain:
          ssat_r    = ssat_cld*(SPF(i,k)-SPF_clr(i,k))+ssat_clr*SPF_clr(i,k)
          sup_r     = ssat_r   /qvs(i,k)
          sup_cld   = ssat_cld /qvs(i,k)   !in-cloud  sub/sur-saturation w.r.t. liq in %
          supi_cld  = Qv_cld(i,k)/qvi(i,k)-1.!in-cloud  sub/sur-saturation w.r.t. ice in %

          if (qc(i,k)*iSCF(i,k).ge.qsmall) qccon = (aaa*epsc*i_xx+(ssat_cld*SCF(i,k)-aaa*i_xx)*i_dt* &
                                          epsc*i_xx* sngl(1.d0-dexp(-dble(xx*dt))) )/ab
          if (qr(i,k).ge.qsmall) qrcon = (aaa*epsr*i_xx+(ssat_r*SPF(i,k)-aaa*i_xx)*i_dt*   &
                                          epsr*i_xx* sngl(1.d0-dexp(-dble(xx*dt))) )/ab

         !evaporate instantly for very small water contents
          qccon = merge(-qc(i,k)*i_dt, qccon, sup_cld.lt.-0.001 .and. qc(i,k).lt.1.e-12)
          qrcon = merge(-qr(i,k)*i_dt, qrcon, sup_r  .lt.-0.001 .and. qr(i,k).lt.1.e-12)

          if (qccon.lt.0.) then
             qcevp = -qccon
             qccon = 0.
          else
             qccon = min(qccon, qv(i,k)*i_dt)
          endif

          if (qrcon.lt.0.) then
             qrevp = -qrcon
             nrevp = qrevp*(nr(i,k)/qr(i,k))
            !nrevp = nrevp*exp(-0.2*mu_r(i,k))  !add mu dependence [Seifert (2008), neglecting size dependence]
             qrcon = 0.
          else
             qrcon = min(qrcon, qv(i,k)*i_dt)
          endif

          iice_loop_depsub:  do iice = 1,nCat

             if (qitot(i,k,iice).ge.qsmall) then

                if (qiliq(i,k,iice)/qitot(i,k,iice).lt.0.01)                             &
                !note: diffusional growth/decay rate: (stored as 'qidep' temporarily;
                !      it may be put to qisub below)
                  qidep(iice) = (aaa*epsi(iice)*i_xx+(ssat_cld*SCF(i,k)-aaa*i_xx)*i_dt*  &
                                epsi(iice)*i_xx*sngl(1.d0-dexp(-dble(xx*dt))) )*i_abi+   &
                                (qvs(i,k)-dumqvi)*epsi(iice)*i_abi
               !for very small ice contents in dry air, sublimate all ice instantly
                if (supi_cld.lt.-0.001 .and. qitot(i,k,iice).lt.1.e-12 .and.             &
                   (qiliq(i,k,iice)/qitot(i,k,iice)).lt.0.01)                            &
                   qidep(iice) = -(qitot(i,k,iice)-qiliq(i,k,iice))*i_dt

             endif

             !note: 'clbfact_dep' and 'clbfact_sub' calibration factors for ice deposition and sublimation
             !   These are adjustable ad hoc factors used to increase or decrease deposition and/or
             !   sublimation rates.  The representation of the ice capacitances are highly simplified
             !   and the appropriate values in the diffusional growth equation are uncertain.

             if (qidep(iice).lt.0.) then
               !note: limit to saturation adjustment (for dep and subl) is applied later
                qisub(iice) = -qidep(iice)
                qisub(iice) = qisub(iice)*clbfact_sub
                qisub(iice) = min(qisub(iice), (qitot(i,k,iice)-qiliq(i,k,iice))*i_dt)
                nisub(iice) = qisub(iice)*(nitot(i,k,iice)/(qitot(i,k,iice)-             &
                              qiliq(i,k,iice)))
                qidep(iice) = 0.
                if (log_3momentIce .and. log_full3mom .and. epsi(iice).gt.0.)            &
                 zisub(iice) = -epsizsb(iice)/epsi(iice)*qisub(iice)
             else
                qidep(iice) = qidep(iice)*clbfact_dep
                qidep(iice) = min(qidep(iice), qv(i,k)*i_dt)
                if (log_3momentIce .and. log_full3mom .and. epsi(iice).gt.0.)            &
                  zidep(iice) = epsiz(iice)/epsi(iice)*qidep(iice)
             endif

             if (qitot(i,k,iice).ge.qsmall) then

                if ((qiliq(i,k,iice)/qitot(i,k,iice)).ge.0.01)                           &
                 ! Condensation/evaporation fo qiliq
                  qlcon(iice) = (aaa*epsiw(iice)*i_xx+(ssat_cld*SCF(i,k)-aaa*i_xx)*i_dt* &
                                 epsiw(iice)*i_xx* sngl(1.d0-dexp(-dble(xx*dt))) )/ab

                if (supi_cld.lt.-0.001 .and. qitot(i,k,iice).lt.1.e-12 .and.             &
                   (qiliq(i,k,iice)/qitot(i,k,iice)).ge.0.01)                            &
                   qlcon(iice) = -qiliq(i,k,iice)*i_dt

                if (qlcon(iice).lt.0.) then
                   qlevp(iice) = -qlcon(iice)
                   qlevp(iice) = min(qlevp(iice),qiliq(i,k,iice)*i_dt)
                   nlevp(iice) = qlevp(iice)*nitot(i,k,iice)/qitot(i,k,iice)
                   qlcon(iice) = 0.
                   if (log_3momentIce.and.epsiw(iice).gt.0.)                             &
                    zisub(iice) = -epsizsb(iice)/epsiw(iice)*qlevp(iice)
                else
                   qlcon(iice) = min(qlcon(iice), qv(i,k)*i_dt)

                   if (log_3momentIce .and. epsiw(iice).gt.0.                            &
                    .and. (qiliq(i,k,iice)/qitot(i,k,iice)).ge.0.01)                     &
                    zidep(iice) = epsiz(iice)/epsiw(iice)*qlcon(iice)
                endif

             endif

          enddo iice_loop_depsub

   !................................................................
   ! autoconversion

          qc_not_small_1: if (qc(i,k)*iSCF(i,k).ge.1.e-8) then

             if (autoAccr_param.eq.1) then
               !Seifert and Beheng (2001)
                dum   = 1.-qc(i,k)*iSCF(i,k)/(qc(i,k)*iSCF(i,k)+qr(i,k)*iSPF(i,k)*       &
                        (SPF(i,k)-SPF_clr(i,k)))
                dum1  = 600.*dum**0.68*(1.-dum**0.68)**3
               ! 1.9230769e+5 = 1/(20 x*), x* = 2.6e-7 g; cgs units, like kc
                qcaut =  kc*1.9230769e+5*(nu(i,k)+2.)*(nu(i,k)+4.)/(nu(i,k)+1.)**2*      &
                         (rho(i,k)*qc(i,k)*iSCF(i,k)*1.e-3)**4/                          &
                         (rho(i,k)*nc(i,k)*iSCF(i,k)*1.e-6)**2*(1.+                      &
                         dum1/(1.-dum)**2)*1000.*i_rho(i,k)*SCF(i,k)
                ncautc = qcaut*7.6923076e+9

             elseif (autoAccr_param.eq.2) then
              !Khroutdinov and Kogan (2000)
                dum   = qc(i,k)*iSCF(i,k)
                qcaut = 1350.*dum**2.47*(nc(i,k)*iSCF(i,k)*1.e-6*rho(i,k))**(-1.79)*SCF(i,k)
               ! note: ncautr is change in Nr; ncautc is change in Nc
                ncautr = qcaut*cons3
                ncautc = qcaut*nc(i,k)/qc(i,k)

             elseif (autoAccr_param.eq.3) then
              !Kogan (2013)
                dum = qc(i,k)*iSCF(i,k)
                qcaut = 7.98e10*dum**4.22*(nc(i,k)*iSCF(i,k)*1.e-6*rho(i,k))**(-3.01)*SCF(i,k)
                ncautr = qcaut*cons8
                ncautc = qcaut*nc(i,k)/qc(i,k)

             endif

             ncautc = merge(0., ncautc, qcaut.eq.0.)
             qcaut  = merge(0., qcaut,  ncautc.eq.0.)

          endif qc_not_small_1

   !............................
   ! self-collection of droplets

          if (qc(i,k)*iSCF(i,k).ge.qsmall) then

             if (autoAccr_param.eq.1) then
              !Seifert and Beheng (2001)
                ncslf = -kc*(1.e-3*rho(i,k)*qc(i,k)*iSCF(i,k))**2*(nu(i,k)+2.)/          &
                        (nu(i,k)+1.)*1.e+6*i_rho(i,k)*SCF(i,k)+ncautc
             elseif (autoAccr_param.eq.2 .or. autoAccr_param.eq.3) then
               !Khroutdinov and Kogan (2000) or Kogan (2013)
                ncslf = 0.
             endif

          endif

   !............................
   ! accretion of cloud by rain

          if (qr(i,k).ge.qsmall .and. qc(i,k)*iSCF(i,k).ge.qsmall) then

             if (autoAccr_param.eq.1) then
              !Seifert and Beheng (2001)
                dum2  = (SPF(i,k)-SPF_clr(i,k)) !in-cloud Precipitation fraction
                dum   = 1.-qc(i,k)*iSCF(i,k)/(qc(i,k)*iSCF(i,k)+qr(i,k)*iSPF(i,k))
                dum1  = (dum/(dum+5.e-4))**4
                qcacc = kr*rho(i,k)*0.001*qc(i,k)*iSCF(i,k)*qr(i,k)*iSPF(i,k)*dum1*dum2
                ncacc = qcacc*rho(i,k)*0.001*(nc(i,k)*rho(i,k)*1.e-6)/(qc(i,k)*rho(i,k)* &  !note: (nc*iSCF)/(qc*iSCF) = nc/qc
                        0.001)*1.e+6*i_rho(i,k)
             elseif (autoAccr_param.eq.2) then
               !Khairoutdinov and Kogan (2000)
                dum2  = (SPF(i,k)-SPF_clr(i,k)) !in-cloud Precipitation fraction
                qcacc = 67.*(qc(i,k)*iSCF(i,k)*qr(i,k)*iSPF(i,k))**1.15 *dum2
                ncacc = qcacc*nc(i,k)/qc(i,k)
             elseif (autoAccr_param.eq.3) then
               !Kogan (2013)
                dum2 = (SPF(i,k)-SPF_clr(i,k)) !in-cloud Precipitation fraction
                qcacc = 8.53*(qc(i,k)*iSCF(i,k))**1.05*(qr(i,k)*iSPF(i,k))**0.98 *dum2
                ncacc = qcacc*nc(i,k)/qc(i,k)
             endif
             ncacc = merge(0., ncacc, qcacc.eq.0.)
             qcacc = merge(0., qcacc, ncacc.eq.0.)

          endif

   !.....................................
   ! self-collection and breakup of rain
   ! (breakup following modified Verlinde and Cotton scheme)

          if (qr(i,k).ge.qsmall) then

           ! include breakup
             dum1 = 280.e-6
             nr(i,k) = max(nr(i,k),nsmall)
           ! use mass-mean diameter (do this by using
           ! the old version of lambda w/o mu dependence)
           ! note there should be a factor of 6^(1/3), but we
           ! want to keep breakup threshold consistent so 'dum'
           ! is expressed in terms of lambda rather than mass-mean D
             dum2 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd
             if (dum2.lt.dum1) then
                dum = 1.
             else if (dum2.ge.dum1) then
                dum = 2.-exp(2300.*(dum2-dum1))
   !            dum = 2.-dexp(dble(2300.*(dum2-dum1)))
             endif

             if (autoAccr_param.eq.1.) then
                nrslf = dum*kr*1.e-3*qr(i,k)*iSPF(i,k)*nr(i,k)*iSPF(i,k)*rho(i,k)*SPF(i,k)
             elseif (autoAccr_param.eq.2) then
                nrslf = dum*5.78*nr(i,k)*iSPF(i,k)*qr(i,k)*iSPF(i,k)*rho(i,k)*SPF(i,k)
             elseif (autoAccr_param.eq.3) then
                nrslf = dum*205.*(qr(i,k)*iSPF(i,k))**1.55*(nr(i,k)*1.e-6*rho(i,k)*      &
                        iSPF(i,k))**0.6*1.e6*i_rho(i,k)*SPF(i,k)   ! 1.e6 converts cm-3 to m-3
             endif

          endif

   !................................................................

       endif growth_decay_processes

!................................................................
! deposition/condensation-freezing nucleation
!   (allow ice nucleation if T < -15 C and > 5% ice supersaturation)

       supi_cld= Qv_cld(i,k)/qvi(i,k)-1.!in-cloud sub/super-saturation w.r.t. ice in %
       sup_cld = Qv_cld(i,k)/qvs(i,k)-1.!in-cloud sub/super-saturation w.r.t. liq in %

       if (t(i,k).lt.258.15 .and. supi_cld.ge.supi_nuc) then
!         dum = exp(-0.639+0.1296*100.*supi(i,k))*1000.*i_rho(i,k)        !Meyers et al. (1992)
          dum = 0.005*exp(0.304*(trplpt-t(i,k)))*1000.*i_rho(i,k)         !Cooper (1986)
          dum = min(dum,100.e3*i_rho(i,k)*SCF(i,k))
          N_nuc = max(0.,(dum-sum(nitot(i,k,:)))*i_dt)

          if (N_nuc.ge.1.e-20) then
             Q_nuc = max(0.,(dum-sum(nitot(i,k,:)))*mi0*i_dt)
             if (nCat>1) then
                !determine destination ice-phase category:
                dum1  = 900.     !density of new ice
                D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
                call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,    &
                                        deltaD_init,iice_dest)
                if (global_status /= STATUS_OK) return
             else
                iice_dest = 1
             endif
             qinuc(iice_dest) = Q_nuc
             ninuc(iice_dest) = N_nuc
          endif
       endif


!.................................................................
! droplet activation

       if (log_predictNc .and. sup_cld.gt.1.e-6) then
       ! 2-moment cloud: alculate droplet activation explicitly from supersaturation
       !   note: also applied at the first time step (number only; mass is already added by sat. adj., below)
          tmp1  = 1./bact**0.5
          sigvl = 0.0761 - 1.55e-4*(t(i,k)-273.15)
          aact  = 2.*mw/(rhow*rr*t(i,k))*sigvl
          sm1   = 2.*tmp1*(aact*thrd*i_rm1)**1.5
          sm2   = 2.*tmp1*(aact*thrd*i_rm2)**1.5
          uu1   = 2.*log(sm1/sup_cld)/(4.242*log(sig1))
          uu2   = 2.*log(sm2/sup_cld)/(4.242*log(sig2))
          tmp1  = nanew1*0.5*(1.-derf(uu1))     ! activated number in kg-1 mode 1
          tmp2  = nanew2*0.5*(1.-derf(uu2))     ! activated number in kg-1 mode 2
          tmp2  = min(nanew1+nanew2, tmp1+tmp2) ! limit value to total aerosol number
          tmp2  = (tmp2-nc(i,k)*iSCF(i,k))*i_dt*SCF(i,k)
          tmp2  = max(0.,tmp2)
          ncnuc = tmp2
        ! exclude mass increase from droplet activation during first time step:
        ! (since this is already accounted for by saturation adjustment below)
          qcnuc = merge(ncnuc*cons7, 0., it.gt.1)

       elseif (.not.(log_predictNc) .and. sup_cld.gt.1.e-6 .and. it.gt.1) then
       ! 1-moment cloud: make sure droplet mass is present if conditions are supersaturated
       !   note: not applied at the first time step, since saturation adjustment is applied at first step
          tmp1   = nccnst*i_rho(i,k)*cons7-qc(i,k)
          tmp1   = max(0.,tmp1*iSCF(i,k))         ! in-cloud value
          dumqvs = qv_sat(t(i,k),pres(i,k),0)
          dqsdT  = xxlv(i,k)*dumqvs/(rv*t(i,k)*t(i,k))
          ab     = 1. + dqsdT*xxlv(i,k)*i_cp
          tmp1   = max(0.,min(tmp1,(Qv_cld(i,k)-dumqvs)/ab))  ! limit overdepletion of supersaturation
          qcnuc  = tmp1*i_dt*SCF(i,k)
          qcnuc  = max(qcnuc,0.)

       endif

!................................................................
! saturation adjustment to get initial cloud water

! This is only called once at the beginning of the simulation
! to remove any supersaturation in the intial conditions

       if (it.le.1) then
          dumt   = th(i,k)*(pres(i,k)*1.e-5)**(rd*i_cp)
          dumqv  = Qv_cld(i,k)
          dumqvs = qv_sat(dumt,pres(i,k),0)
          dums   = dumqv-dumqvs
          qccon  = dums/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*dumt**2))*i_dt*SCF(i,k)
          qccon  = max(0.,qccon)
          if (qccon.le.1.e-7) qccon = 0.
       endif

!.................................................................
! conservation of mass
!
! The microphysical process rates are computed above, based on the environmental conditions.
! The rates are adjusted here (where necessary) such that the sum of the sinks of mass cannot
! be greater than the sum of the sources, thereby resulting in overdepletion.

    !Limit total condensation (incl. activation) and evaporation to saturation adjustment
       dumqvs = qv_sat(t(i,k),pres(i,k),0)
       qcon_satadj = (Qv_cld(i,k)-dumqvs)/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*t(i,k)**2))*    &
                     i_dt*SCF(i,k)
       qevp_satadj  =((Qv_cld(i,k)-dumqvs)*(SPF(i,k)-SPF_clr(i,k))+(Qv_clr(i,k)-dumqvs)* &
                     SPF_clr(i,k))/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*t(i,k)**2))*i_dt

       tmp1 = qccon+qrcon+qcnuc+sum(qlcon)
       if (tmp1>0. .and. qcon_satadj<0.) then
          qccon = 0.
          qrcon = 0.
          qcnuc = 0.
          qlcon = 0.
          ncnuc = 0.
       elseif (tmp1.gt.0. .and. tmp1.gt.qcon_satadj) then
          ratio = max(0.,qcon_satadj)/tmp1
          ratio = min(1.,ratio)
          qccon = qccon*ratio
          qrcon = qrcon*ratio
          qcnuc = qcnuc*ratio
          ncnuc = ncnuc*ratio
          qlcon = qlcon*ratio
       endif

       tmp2 = qcevp+qrevp+sum(qlevp)
       if (tmp2>0. .and. qevp_satadj>0.) then
          qcevp = 0.
          nrevp = 0.
          qlevp = 0.
          nlevp = 0.
       elseif (tmp2.gt.0. .and. tmp2.gt.-qevp_satadj) then
             ratio = max(0.,-qevp_satadj)/(qcevp+qrevp+sum(qlevp))
             ratio = min(1.,ratio)
             qcevp = qcevp*ratio
             qrevp = qrevp*ratio
             nrevp = nrevp*ratio
             qlevp = qlevp*ratio
             nlevp = nlevp*ratio
       endif

    !Limit total deposition (incl. nucleation) and sublimation to saturation adjustment
       qv_tmp = Qv_cld(i,k) + (-qcnuc-qccon-qrcon-sum(qlcon)+qcevp+qrevp+sum(qlevp))*dt       !qv after cond/evap
       t_tmp  = t(i,k) + (qcnuc+qccon+qrcon+sum(qlcon)-qcevp-qrevp-sum(qlevp))*          &    !T after cond/evap
                          xxlv(i,k)*i_cp*dt
       dumqvi = qv_sat(t_tmp,pres(i,k),1)
       qdep_satadj = (qv_tmp-dumqvi)/(1.+xxls(i,k)**2*dumqvi/(cp*rv*t_tmp**2))*i_dt*SCF(i,k)

       tmp1 = sum(qidep)+sum(qinuc)
       if (tmp1>0. .and. qdep_satadj<0.) then
          qidep = 0.
          qinuc = 0.
          ninuc = 0.
       else
          if (tmp1.gt.0. .and. tmp1.gt.qdep_satadj) then
             ratio = max(0.,qdep_satadj)/tmp1
             ratio = min(1.,ratio)
             qidep = qidep*ratio
             qinuc = qinuc*ratio
             ninuc = ninuc*ratio
          endif
          do iice = 1,nCat
             dum = max(qisub(iice),1.e-20)
             qisub(iice)  = qisub(iice)*min(1.,max(0.,-qdep_satadj)/max(sum(qisub),      &
                            1.e-20))  !optimized (avoids IF(qisub.gt.0.) )
             nisub(iice)  = nisub(iice)*min(1.,qisub(iice)/dum)
          enddo
         !qchetc = qchetc*min(1.,qc(i,k)*i_dt/max(sum(qchetc),1.e-20))  !currently not used
         !qrhetc = qrhetc*min(1.,qr(i,k)*i_dt/max(sum(qrhetc),1.e-20))  !currently not used
       endif


! cloud
       sinks   = (qcaut+qcacc+sum(qccol)+qcevp+sum(qchetc)+sum(qcheti)+sum(qcshd)+       &
                 sum(qccoll)+sum(qwgrth1c)+sum(qcmul))*dt
       sources = qc(i,k) + (qccon+qcnuc)*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qcaut  = qcaut*ratio
          qcacc  = qcacc*ratio
          qcevp  = qcevp*ratio
          qccol  = qccol*ratio
          qcheti = qcheti*ratio
          qcshd  = qcshd*ratio
          qcmul  = qcmul*ratio
          qwgrth1c = qwgrth1c*ratio
          qccoll = qccoll*ratio
         !qchetc = qchetc*ratio !currently not used
           ncautc = ncautc*ratio
           ncacc  = ncacc*ratio
           nccol  = nccol*ratio
           ncheti = ncheti*ratio
          !nchetc = nchetc*ratio
           nccoll = nccoll*ratio
       endif

! rain
       sinks   = (qrevp+sum(qrcol)+sum(qrhetc)+sum(qrheti)+sum(qrmul)+sum(qrcoll)+       &
                 sum(qwgrth1r))*dt
       sources = qr(i,k) + (qrcon+qcaut+qcacc+sum(qrmlt)+sum(qcshd)+sum(qlshd))*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qrevp  = qrevp*ratio
          qrcol  = qrcol*ratio
          qrheti = qrheti*ratio
          qrmul  = qrmul*ratio
          qrcoll = qrcoll*ratio
          qwgrth1r = qwgrth1r*ratio
         !qrhetc = qrhetc*ratio !currently not used
          nrevp  = nrevp*ratio
          nrcol  = nrcol*ratio
          nrheti = nrheti*ratio
          nrcoll = nrcoll*ratio
         !qrhetc = qrhetc*ratio
         !nrhetc = nrhetc*ratio
       endif

! ice (total)
       do iice = 1,nCat
          sinks   = (qisub(iice)+qrmlt(iice)+qlevp(iice)+qlshd(iice))*dt
          sources = qitot(i,k,iice) + (qidep(iice)+qinuc(iice)+qrcol(iice)+qccol(iice)+  &
                    qrhetc(iice)+qrheti(iice)+qchetc(iice)+qcheti(iice)+qrmul(iice)+     &
                    qcmul(iice)+qrcoll(iice)+qccoll(iice)+qlcon(iice)+qwgrth1c(iice)+    &
                    qwgrth1r(iice))*dt
          do catcoll = 1,nCat
            !Note: qicol = 0 if iice=catcoll, optimised to not insert an if (catcoll.ne.iice)
            !category interaction leading to source for iice category
             sources = sources + qicol(catcoll,iice)*dt
            !category interaction leading to sink for iice category
             sinks = sinks + qicol(iice,catcoll)*dt
          enddo
          if (sinks.gt.sources .and. sinks.ge.1.e-20) then
             ratio = sources/sinks
             qisub(iice) = qisub(iice)*ratio
             qrmlt(iice) = qrmlt(iice)*ratio
             qlshd(iice) = qlshd(iice)*ratio
             qlevp(iice) = qlevp(iice)*ratio
             nlevp(iice) = nlevp(iice)*ratio
             nisub(iice) = nisub(iice)*ratio
             nimlt(iice) = nimlt(iice)*ratio
             nlshd(iice) = nlshd(iice)*ratio
             do catcoll = 1,nCat
                !Note: qicol = 0 if iice=catcoll, optimised to not insert an if (catcoll.ne.iice)
                qicol(iice,catcoll) = qicol(iice,catcoll)*ratio
                nicol(iice,catcoll) = nicol(iice,catcoll)*ratio
             enddo
          endif
      enddo  !iice-loop

! frozen ice (qitot - qiliq)
!   Prevent the frozen mass from being over-depleted.
!   The three frozen-mass sinks (qisub, qrmlt, qimlt) are each individually capped
!   at their computation sites, but against the same budget. This joint limiter ensures
!   their combined sink cannot exceed the available frozen mass plus frozen sources.
      if (log_LiquidFrac) then
       do iice = 1,nCat
          sinks   = (qisub(iice)+qrmlt(iice)+qimlt(iice))*dt
          sources = (qitot(i,k,iice)-qiliq(i,k,iice)) + (qidep(iice)+qinuc(iice)+        &
                    qrcol(iice)+qccol(iice)+qrhetc(iice)+qrheti(iice)+qchetc(iice)+      &
                    qcheti(iice)+qrmul(iice)+qcmul(iice)+qifrz(iice))*dt
          if (qitot(i,k,iice).ge.qsmall) then
             dumf = 1. - qiliq(i,k,iice)/qitot(i,k,iice)   ! frozen fraction of iice
          else
             dumf = 1.
          endif
          do catcoll = 1,nCat
            !Note: qicol = 0 if iice=catcoll, optimised to not insert an if (catcoll.ne.iice)
            !category interaction leading to source for iice (gains frozen mass of collectee)
             sources = sources + qicol(catcoll,iice)*dt*dumf
            !category interaction leading to sink for iice (loses frozen mass to collector)
             sinks = sinks + qicol(iice,catcoll)*dt*dumf
          enddo
          if (sinks.gt.sources .and. sinks.ge.1.e-20) then
             ratio = sources/sinks
             qisub(iice) = qisub(iice)*ratio
             qrmlt(iice) = qrmlt(iice)*ratio
             qimlt(iice) = qimlt(iice)*ratio
             nisub(iice) = nisub(iice)*ratio
             nimlt(iice) = nimlt(iice)*ratio
             if (log_3momentIce) then
                zisub(iice) = zisub(iice)*ratio
                zimlt(iice) = zimlt(iice)*ratio
             endif
          endif
       enddo  !iice-loop
      endif

! qiliq
      if (log_LiquidFrac) then
       do iice = 1,nCat
          sinks   = (qifrz(iice)+qlshd(iice)+qlevp(iice))*dt
          sources = qiliq(i,k,iice) + (qimlt(iice)+qrcoll(iice)+qccoll(iice)+            &
                    qlcon(iice)+qwgrth1c(iice)+qwgrth1r(iice))*dt
          if (qitot(i,k,iice).ge.qsmall) then
             dum = qiliq(i,k,iice)/qitot(i,k,iice)
          else
             dum = 0.
          endif
          do catcoll = 1,nCat
            !Note: qicol = 0 if iice=catcoll, optimised to not insert an if (catcoll.ne.iice)
            !category interaction leading to source for iice category
             sources = sources + qicol(catcoll,iice)*dt*dum
            !category interaction leading to sink for iice category
             sinks = sinks + qicol(iice,catcoll)*dt*dum
          enddo
          if (sinks.gt.sources .and. sinks.ge.1.e-20) then
             ratio = sources/sinks
             qifrz(iice) = qifrz(iice)*ratio
             qlshd(iice) = qlshd(iice)*ratio
             qlevp(iice) = qlevp(iice)*ratio
             nlevp(iice) = nlevp(iice)*ratio
             nlshd(iice) = nlshd(iice)*ratio
             do catcoll = 1,nCat
                !Note: qicol = 0 if iice=catcoll, optimised to not insert an if (catcoll.ne.iice)
                qicol(iice,catcoll) = qicol(iice,catcoll)*ratio
                nicol(iice,catcoll) = nicol(iice,catcoll)*ratio
             enddo
          endif
       enddo !iice-loop
      endif

! vapor
       sinks   = (qccon+qrcon+qcnuc+sum(qidep)+sum(qinuc)+sum(qlcon))*dt
       sources = qv(i,k) + (qcevp+qrevp+sum(qisub)+sum(qlevp))*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qccon  = qccon*ratio
          qrcon  = qrcon*ratio
          qcnuc  = qcnuc*ratio
          qidep  = qidep*ratio
          qinuc  = qinuc*ratio
          qlcon  = qlcon*ratio
          ninuc  = ninuc*ratio
          ncnuc  = ncnuc*ratio
       endif

!======================================================================================!

!---------------------------------------------------------------------------------
! update prognostic microphysics and thermodynamics variables
!---------------------------------------------------------------------------------

   !-- ice-phase dependent processes:

       iice_loop2: do iice = 1,nCat

       ! compute fractions before update (assumed constant during ice-ice coll.)
          if ((qitot(i,k,iice)-qiliq(i,k,iice)).ge.qsmall .and. qitot(i,k,iice).ge. qsmall) then
             tmp1 = 1./(qitot(i,k,iice)-qiliq(i,k,iice))   ! i.e. 1/(qidep+qirim)  [qidep is implicit]
            !note: rimefrac_over_rhorime = rimefrac/rhorime = (qirim/(qidep+qirim)) / (qirim/birim);
            !      used in birim-tendency calculations below (opimized; the two qirim's cancel)
             rimefrac_over_rhorime(i,k,iice) = birim(i,k,iice)*tmp1
             rime_frac(i,k,iice)             = qirim(i,k,iice)*tmp1
             liq_frac(i,k,iice)              = qiliq(i,k,iice)/qitot(i,k,iice)
          endif

       ! calculate current mu_i (before updated from processes) which is used later to update mu_i
!          if (log_3momentIce) then
!             if (qitot(i,k,iice).ge.qsmall) then
!                tmp1 = qitot(i,k,iice)*6./(f1pr16*pi)  !estimate of 3rd moment
!                mu_i_s(iice) = compute_mu_3mom_1(nitot(i,k,iice),tmp1,zitot(i,k,iice),mu_i_max)  !polynomial approximation
!             else
!                mu_i_s(iice) = mu_i_initial
!             endif
!          endif

       enddo iice_loop2

       iice_loop3: do iice = 1,nCat

          qc(i,k) = qc(i,k) + (-qchetc(iice)-qcheti(iice)-qccol(iice)-qcshd(iice)-       &
                    qccoll(iice)-qwgrth1c(iice)-qcmul(iice))*dt
          nc(i,k) = nc(i,k) + (-nccol(iice)-nchetc(iice)-ncheti(iice)-nccoll(iice))*dt
          qr(i,k) = qr(i,k) + (-qrcol(iice)+qrmlt(iice)-qrhetc(iice)-qrheti(iice)+       &
                    qcshd(iice)-qrmul(iice)-qrcoll(iice)+qlshd(iice)-qwgrth1r(iice))*dt

        ! apply factor to source for rain number from melting of ice, (ad-hoc
        ! but accounts for rapid evaporation of small melting ice particles)
          if (log_LiquidFrac) then
             nr(i,k) = nr(i,k) + (-nrcol(iice)-nrhetc(iice)-nrheti(iice)+nimlt(iice)+    &
                       nrshdr(iice)+ncshdc(iice)-nrcoll(iice)+nlshd(iice))*dt
          else
             nr(i,k) = nr(i,k) + (-nrcol(iice)-nrhetc(iice)-nrheti(iice)+nmltratio*      &
                                 nimlt(iice)+nrshdr(iice)+ncshdc(iice))*dt
          endif

         ! add sink terms, assume density stays constant for sink terms
             birim(i,k,iice) = birim(i,k,iice) - (qisub(iice)+qrmlt(iice)+qimlt(iice))*  &
                               dt*rimefrac_over_rhorime(i,k,iice)
             qirim(i,k,iice) = qirim(i,k,iice) - (qisub(iice)+qrmlt(iice)+qimlt(iice))*  &
                               dt*rime_frac(i,k,iice)
             qiliq(i,k,iice) = qiliq(i,k,iice) + qimlt(iice)*dt
             qitot(i,k,iice) = qitot(i,k,iice) - (qisub(iice)+qrmlt(iice))*dt
         ! endif

          tmp1             = (qrcol(iice)+qccol(iice)+qrhetc(iice)+qrheti(iice)+         &
                            qchetc(iice)+qcheti(iice)+qrmul(iice)+qcmul(iice))*dt
          qitot(i,k,iice) = qitot(i,k,iice) + (qidep(iice)+qinuc(iice)-qlshd(iice)-      &
                            qlevp(iice)+qlcon(iice)+qwgrth1c(iice)+qwgrth1r(iice)+       &
                            qrcoll(iice)+qccoll(iice))*dt + tmp1
          qirim(i,k,iice) = qirim(i,k,iice) + qifrz(iice)*dt + tmp1
          birim(i,k,iice) = birim(i,k,iice) + ((qifrz(iice)+qrcol(iice))*                &
                            i_rho_rimeMax+(qccol(iice)+qcmul(iice))/rhorime_c(iice)+     &
                            (qrhetc(iice)+qrheti(iice)+qchetc(iice)+qcheti(iice)+        &
                            qrmul(iice))*i_rho_rimeMax)*dt
          qiliq(i,k,iice) = qiliq(i,k,iice) + (qrcoll(iice)+qccoll(iice)-qifrz(iice)-    &
                            qlshd(iice)+qlcon(iice)-qlevp(iice)+qwgrth1c(iice)+          &
                            qwgrth1r(iice))*dt
          nitot(i,k,iice) = nitot(i,k,iice) + (ninuc(iice)-nimlt(iice)-nisub(iice)-      &
                            nislf(iice)+nrhetc(iice)+nrheti(iice)+nchetc(iice)+          &
                            ncheti(iice)+nimul(iice)-nlevp(iice))*dt

          if (nCat.gt.1) then
             interactions_loop: do catcoll = 1,nCat
                diff_categories: if (iice.ne.catcoll) then

             ! add ice-ice category interaction collection tendencies
             ! note: nicol is a sink for the collectee category, but NOT a source for collector

             ! now modify rime mass and density, assume collection does not modify rime or liquid mass
             ! fractions or density of the collectee, consistent with the assumption that
             ! these are constant over the PSD
              !source for collector category
                qirim(i,k,iice) = qirim(i,k,iice)+qicol(catcoll,iice)*dt*                &
                                  rime_frac(i,k,catcoll)
                birim(i,k,iice) = birim(i,k,iice)+qicol(catcoll,iice)*dt*                &
                                  rimefrac_over_rhorime(i,k,catcoll)
                qiliq(i,k,iice) = qiliq(i,k,iice)+qicol(catcoll,iice)*dt*                &
                                  liq_frac(i,k,catcoll)
              !sink for collectee category
                qirim(i,k,catcoll) = qirim(i,k,catcoll)-qicol(catcoll,iice)*dt*          &
                                     rime_frac(i,k,catcoll)
                birim(i,k,catcoll) = birim(i,k,catcoll)-qicol(catcoll,iice)*dt*          &
                                     rimefrac_over_rhorime(i,k,catcoll)
                qiliq(i,k,catcoll) = qiliq(i,k,catcoll)-qicol(catcoll,iice)*dt*          &
                                     liq_frac(i,k,catcoll)
                qitot(i,k,catcoll) = qitot(i,k,catcoll) - qicol(catcoll,iice)*dt
                nitot(i,k,catcoll) = nitot(i,k,catcoll) - nicol(catcoll,iice)*dt
                qitot(i,k,iice)    = qitot(i,k,iice)    + qicol(catcoll,iice)*dt

                endif diff_categories
             enddo interactions_loop
          endif

          if (qirim(i,k,iice).lt.0.) then
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

          qiliq(i,k,iice) = max(qiliq(i,k,iice),0.)

          ! densify ice during wet growth (assume total soaking)
            if (log_wetgrowth(iice)) then
               qirim(i,k,iice) = qitot(i,k,iice)
               birim(i,k,iice) = qirim(i,k,iice)*i_rho_rimeMax
            endif
          ! densify rimed ice during melting (tend rime density towards solid ice [917 kg m-3])
            if (.not. log_LiquidFrac .and. qitot(i,k,iice).ge.qsmall .and.               &
             birim(i,k,iice).ge.bsmall .and. qrmlt(iice)>0.) then
               tmp1 = qirim(i,k,iice)/birim(i,k,iice)     ! rho_i before densification
               tmp2 = qitot(i,k,iice) + qrmlt(iice)*dt    ! qitot before melting (but after all other updates)
               birim(i,k,iice) = qirim(i,k,iice)/(tmp1+(917.-tmp1)*qrmlt(iice)*dt/tmp2)
            endif

          qv(i,k) = qv(i,k) + (-qidep(iice)+qisub(iice)-qinuc(iice)-qlcon(iice)+         &
                               qlevp(iice))*dt

        ! Update theta. Note temperature is not updated here even though it is used below for
        ! the homogeneous freezing threshold. This is done for simplicity - the error will be
        ! very small and the homogeneous temp. freezing threshold is approximate anyway.
          th(i,k) = th(i,k) + i_exn(i,k)*((qidep(iice)-qisub(iice)+qinuc(iice))*         &
                              xxls(i,k)*i_cp +(qrcol(iice)+qccol(iice)+qchetc(iice)+     &
                              qcheti(iice)+qrhetc(iice)+qrheti(iice)+qcmul(iice)+        &
                              qrmul(iice)-qrmlt(iice)-qimlt(iice)+qifrz(iice))*          &
                              xlf(i,k)*i_cp+(qlcon(iice)-qlevp(iice))*xxlv(i,k)*i_cp)*dt

       enddo iice_loop3


   !-- warm-phase only processes:
       qc(i,k) = qc(i,k) + (-qcacc-qcaut+qcnuc+qccon-qcevp)*dt
       qr(i,k) = qr(i,k) + (qcacc+qcaut+qrcon-qrevp)*dt
       nc(i,k) = nc(i,k) + (-ncacc-ncautc+ncslf+ncnuc)*dt
       nr(i,k) = nr(i,k) + dt*merge((0.5*ncautc-nrslf-nrevp), (ncautr-nrslf-nrevp),      &
                                    autoAccr_param.eq.1)

       qv(i,k) = qv(i,k) + (-qcnuc-qccon-qrcon+qcevp+qrevp)*dt
       th(i,k) = th(i,k) + i_exn(i,k)*((qcnuc+qccon+qrcon-qcevp-qrevp)*xxlv(i,k)*        &
                 i_cp)*dt

       ! clipping for Filiq > 0.99 (transfer unmelted ice to rain)
       if (log_LiquidFrac) then
         do iice = 1,nCat
            if (qitot(i,k,iice).ge.qsmall) then
               if (qiliq(i,k,iice)/qitot(i,k,iice).gt.0.99) then
                  qr(i,k) = qr(i,k) + qitot(i,k,iice)
                  nr(i,k) = nr(i,k) + nitot(i,k,iice)
                  th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*      &
                                       xlf(i,k)*i_cp
                  qitot(i,k,iice) = 0.
                  nitot(i,k,iice) = 0.
                  qirim(i,k,iice) = 0.
                  qiliq(i,k,iice) = 0.
                  birim(i,k,iice) = 0.
               endif
            endif
          enddo !iice-loop
        endif

     ! clipping for small hydrometeor values
       if (qc(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - i_exn(i,k)*qc(i,k)*xxlv(i,k)*i_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       endif

       if (qr(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - i_exn(i,k)*qr(i,k)*xxlv(i,k)*i_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*          &
                                 xxls(i,k)*i_cp
             th(i,k) = th(i,k) - i_exn(i,k)*qiliq(i,k,iice)*xxlv(i,k)*i_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             qiliq(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif
       enddo !iice-loop

       qv(i,k) = max(0., qv(i,k))
       call impose_max_Ni(nitot(i,k,:),max_Ni,i_rho(i,k))

!---------------------------------------------------------------------------------

       trplmomice_2: if (log_3momentIce) then

          do iice = 1,nCat

! include all processes **except** group 2 processes which are added later below
! thus, all group 2 processes are subtracted from the ice variables below
          dumqi = qitot(i,k,iice) - (qinuc(iice)+qrhetc(iice)+qrheti(iice)+qchetc(iice)+ &
                                    qcheti(iice)+qrmul(iice)+qcmul(iice))*dt
          dumql = qiliq(i,k,iice)

          if ((dumqi-dumql).ge.qsmall) then

             dumni = nitot(i,k,iice) - (ninuc(iice)+nrhetc(iice)+nrheti(iice)+           &
                                       nchetc(iice)+ncheti(iice)+nimul(iice))*dt
             dumzi = zitot(i,k,iice)
             dumqr = qirim(i,k,iice) - (qrhetc(iice)+qrheti(iice)+qchetc(iice)+          &
                                       qcheti(iice)+qrmul(iice)+qcmul(iice))*dt
             dumbi = birim(i,k,iice) - (qrhetc(iice)+qrheti(iice)+qchetc(iice)+          &
                           qcheti(iice)+qrmul(iice)+qcmul(iice))*i_rho_rimeMax*dt

             dumni = max(dumni,nsmall)
             dumzi = max(dumzi,zsmall)

             full3mom_1: if (log_full3mom) then
!.......................
! use full-3 moment method

! NOTE: for ice-ice category collection with nCat > 1, for simplicity it is assumed that
! mu does change change from this process. This is implicitly accounted for in the code below since
! in effect G_rate = 0 for category collection.

                G_rate_tot = zqccol(iice) + zidep(iice) + zisub(iice) + zishd(iice) +    &
                             zimlt(iice) + zislf(iice) + zqrcol(iice)

            ! get indices to calculate updated bulk density
                call calc_bulkRhoRime(dumqi,dumqr,dumql,dumbi,rhop)
                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                     dum7,isize,rimsize,liqsize,densize,dumqi,dumni,dumqr,dumql,rhop)

            ! apply iteration to find updated zitot consistent with updated G and dumqi, dumni, etc.
                dumzi_old = dumzi
                do iana = 1,niter_mui

            ! calculate updated density from LUT3 with updated/iterated value of dumzi (since mu_i is unknown)
                   call get_mui_rhoi(mu_i,rholt3,dum6,dumzz,dumqi,dumni,dumzi,dum1,      &
                                  dum4,dum5,dum7,dumjj,dumii,dumll,dumi,zsize,zqsize)
            ! calculate third moment M3 from updated density
            ! NOTE: M3 is not calculated directly from the lookup table because of large interoplation errors.
            !       It is more accurate to estimate M3 from density and updated Qitot (dumqi).
                   dum3 = 6./(rholt3*pi)*dumqi

                  ! update dummy zi based on updated M3:
                   G_new = G_of_mu(mu_i_s(iice)) + G_rate_tot*dt
                   dumzi = G_new*dum3**2/dumni
                   dumzi = max(dumzi,zsmall)

                   if (abs((dumzi-dumzi_old)/dumzi_old) .lt. 0.01) exit
                   dumzi_old = dumzi

                enddo ! iana iterative loop to estimate updated Zitot

                zitot(i,k,iice) = dumzi

             else  ! full_3mom_1
!..............................
! old (simplified) method with group 1 processes (where mu_i does not change due to these processes)

! Get updated density to estimate M3 from updated Qitot (dumqi)
! Here we know mu_i (mu_i_s) and it does not change from the processes.
! Thus, we can get the updated density using the original lookup table with mu_i known (specified)

                call calc_bulkRhoRime(dumqi,dumqr,dumql,dumbi,rhop)
                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                       dum7,isize,rimsize,liqsize,densize,dumqi,dumni,dumqr,dumql,rhop)
                call find_lookupTable_indices_1c(dumzz,dum6,zsize,mu_i_s(iice))
                call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,       &
                                  dumzz,dumjj,dumii,dumll,dumi,0)
                dumden = proc_from_LUT_main3mom(LUT1_RHOMM,args_r,args_i)

                dum3 = 6./(dumden*pi)*dumqi      !estimate of 3rd moment (new, after group 1 processes only)
                zitot(i,k,iice) = G_of_mu(mu_i_s(iice))*dum3**2/dumni
                zitot(i,k,iice) = max(zsmall,zitot(i,k,iice))

             endif full3mom_1

!......................................................

          endif ! dumqi > qsmall

!...........................................................................................
       !---  Group 2 (initiation processes, where mu_i is specified for the new ice
       !              resulting from that specific process)

        !proceses with rain freezing:
          tmp1 = qrhetc(iice) + qrheti(iice)   !qitot tendency
          tmp2 = nrhetc(iice) + nrheti(iice)   !moment_0 tendency
          call update_zi_proc2(zitot(i,k,iice),tmp2,tmp1,mu_r(i,k),dt)

        !proceses with cloud freezing:
          tmp1 = qchetc(iice) + qcheti(iice)   !qitot tendency
          tmp2 = nchetc(iice) + ncheti(iice)   !moment_0 tendency
          call update_zi_proc2(zitot(i,k,iice),tmp2,tmp1,mu_r(i,k),dt)

        !proceses of deposition nucleation
          tmp1 = qinuc(iice)                   !qitot tendency
          tmp2 = ninuc(iice)                   !moment_0 tendency
          call update_zi_proc2(zitot(i,k,iice),tmp2,tmp1,mu_r(i,k),dt)

        !proceses of ice multiplication
          tmp1 = qrmul(iice)                   !qitot tendency
          tmp2 = nimul(iice)                   !moment_0 tendency
          call update_zi_proc2(zitot(i,k,iice),tmp2,tmp1,mu_r(i,k),dt)

        !proceses of rime splintering of cloud droplets
          tmp1 = qcmul(iice)                   !qitot tendency
          tmp2 = nimul(iice)                   !moment_0 tendency
          call update_zi_proc2(zitot(i,k,iice),tmp2,tmp1,mu_r(i,k),dt)

       !====

! NOTE: Limits on zitot to keep mu_i in bounds are imposed at the start of sedimentation below (for ice)

          enddo ! iice loop

       endif trplmomice_2

!................................................................................

    endif compute_procs

  enddo !i loop
 enddo k_loop_main_processes

#ifdef TIMING_P3
!timer_description(3) = 'k_loop_main (proc)'
call cpu_time(timer_end(3))
#endif

 if (log_3momentIce) where (qitot.lt.qsmall) zitot = 0.

 if (.not.log_predictNc) nc = nccnst*i_rho

    !NOTE: At this point, it is possible to have negative (but small) nc, nr, nitot.  This is not
    !      a problem; those values get clipped to zero or assigned a minumum value in the sedimentation
    !      section, immediately below (if necessary).  Similarly, for 3-moment-ice it is possible at this
    !      point to have zitot=0 but qitot slightly larger than qsmall; in sedimentation non-zero zitot
    !      is computed (if necessary) by applying the constraints on mu_i.

!     if (debug_on) then
!        location_ind = 300
!        force_abort  = debug_ABORT
!        tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
!        if (log_3momentIce) then
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,     &
!                location_ind,Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!        else
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                      qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,           &
!                      force_abort,location_ind,Qiliq=qiliq(i,:,:))
!        endif
!        if (global_status /= STATUS_OK) return
!     endif

   !second call to compute_SCPF
 call compute_SCPF(Qc(:,:)+sum(Qitot(:,:,:),dim=3),Qr(:,:),Qv(:,:),Qvi(:,:),          &
                   Pres(:,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                   SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)

!------------------------------------------------------------------------------------------!
! End of main microphysical processes section
!==========================================================================================!


!==========================================================================================!
#ifdef TIMING_P3
timer_description(6) = 'sedimentation'
call cpu_time(timer_start(6))
#endif

!------------------------------------------------------------------------------------------!
! Sedimentation:

 do i = its,ite

! Cloud:
    if (maxval(qc(i,:)) .ge. qsmall)                                                     &
       call sedimentation_liquid(qc(i,:),nc(i,:),1,iSCF(i,:),prt_liq(i),rho(i,:),        &
                       i_rho(i,:),i_dzq(i,:),dt,ktop,kbot,kdir,acn=acn(i,:),dnu=dnu(:))

! Rain:
    if (maxval(qr(i,:)) .ge. qsmall)                                                     &
       call sedimentation_liquid(qr(i,:),nr(i,:),2,iSPF(i,:),prt_liq(i),rho(i,:),        &
                       i_rho(i,:),i_dzq(i,:),dt,ktop,kbot,kdir,rhofacr=rhofacr(i,:),     &
                       massflux=massflux_r(i,:))

! Ice:
    log_tmp1 = maxval(qitot(i,:,:)) .ge. qsmall

    if (log_tmp1)                                                                        &
       call sedimentation_ice(log_3momentIce,log_LiquidFrac,qitot(i,:,:),qirim(i,:,:),   &
                              qiliq(i,:,:),nitot(i,:,:),birim(i,:,:),zitot(i,:,:),       &
                              prt_sol(i),prt_soli(i,:),rho(i,:),i_rho(i,:),rhofaci(i,:), &
                              i_dzq(i,:),ktop,kbot,kdir,dt)

 enddo !i loop

! note: This debug check is commented since small negative qx,nx values are possible here
!       (but get adjusted below).  If uncommented, caution in interpreting results.
!
   ! if (debug_on) then
   !    location_ind = 600
   !    force_abort  = .false.
   !    tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
   !    if (log_3momentIce) then
   !       call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
   !              qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,         &
   !              Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
   !    else
   !       call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
   !              qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,                      &
   !              location_ind,Qiliq=qiliq(i,:,:))
   !    endif
   !    if (global_status /= STATUS_OK) return
   ! endif


#ifdef TIMING_P3
!timer_description(6) = 'sedimentation'
call cpu_time(timer_end(6))
#endif

! End of sedimentation section
!==========================================================================================!

 if (log_LiquidFrac) call freeze_tiny_liqfrac(qitot,qiliq,qirim,birim,t, th,i_exn,xlf,i_cp)

 if (.not.log_predictNc) nc = nccnst*i_rho

!third and last call to compute_SCPF
 call compute_SCPF(Qc(:,:)+sum(Qitot(:,:,:),dim=3),Qr(:,:),Qv(:,:),Qvi(:,:),             &
                   Pres(:,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,     &
                   SCPF_on,scpf_pfrac,scpf_resfact,quick=.true.)

!.......................................
! homogeneous freezing of cloud and rain

 k_loop_fz:  do k = kbot,ktop,kdir
  do i = its,ite

    freezing_possible: if ( (qc(i,k)*iSCF(i,k).ge.qsmall .or. qr(i,k).ge.qsmall) .and.   &
                            t(i,k).lt.233.15 ) then

       multicat1: if (nCat>1) then

       ! compute mean-mass ice diameters
          diam_ice(i,k,:) = 0.
          do iice = 1,nCat
             if (qitot(i,k,iice).ge.qsmall) then
                nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
                call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),   &
                                      birim(i,k,iice),rhop)
                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                          dum7,isize,rimsize,liqsize,densize,qitot(i,k,iice),            &
                          nitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),rhop)

                if (.not. log_3momentIce) then
                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum7,0.,0.,0.,0.,      &
                                     dumjj,dumii,dumll,dumi,0,0)
                   f1pr16 = proc_from_LUT_main2mom(LUT1_RHOMM,args_r,args_i)
                else
                   call get_mui_rhoi(mu_i,f1pr16,dum6,dumzz,qitot(i,k,iice),             &
                                  nitot(i,k,iice),zitot(i,k,iice),dum1,dum4,dum5,dum7,   &
                                  dumjj,dumii,dumll,dumi,zsize,zqsize)
                endif
                diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(nitot(i,k,iice)*f1pr16*      &
                                       pi))**thrd
             endif
          enddo  !iice loop

       endif multicat1

       qc_not_small_2: if (qc(i,k)*iSCF(i,k).ge.qsmall .and. t(i,k).lt.233.15) then

          Q_nuc = qc(i,k)
          nc(i,k) = max(nc(i,k),nsmall)
          N_nuc = nc(i,k)

          if (nCat>1) then
             !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,         &
                                     deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*i_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         !Z-tendency for triple-moment ice
         !  note:  this could be optimized by moving this conditional block outside of loop k_loop_fz
         !         (would need to save values of iice_dest -- ditto for homo freezing of rain)
          if (log_3momentIce .and. N_nuc.ge.nsmall) then
             tmp1 = Q_nuc*6./(900.*pi)  !estimate of moment_3 tendency
             call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,         &
                                 lamc(i,k),cdist(i,k),cdist1(i,k),iSCF(i,k))
             mu_i_new = mu_c(i,k)
             zitot(i,k,iice_dest) = zitot(i,k,iice_dest) + G_of_mu(mu_i_new)*tmp1**2/    &
                                    N_nuc
          endif ! log_3momentice
         ! update theta. Note temperature is NOT updated here, but currently not used after
          th(i,k) = th(i,k) + i_exn(i,k)*Q_nuc*xlf(i,k)*i_cp
          qc(i,k) = 0.  != qc(i,k) - Q_nuc
          nc(i,k) = 0.  != nc(i,k) - N_nuc

       endif qc_not_small_2

       qr_not_small_2: if (qr(i,k).ge.qsmall .and. t(i,k).lt.233.15) then

          Q_nuc = qr(i,k)
          nr(i,k) = max(nr(i,k),nsmall)
          N_nuc = nr(i,k)
          if (nCat>1) then
             !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(i,k),diam_ice(i,k,:),D_new,       &
                                     deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*i_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         ! z tendency for triple moment ice
          if (log_3momentIce .and. N_nuc.ge.qsmall) then
             tmp1 = Q_nuc*6./(900.*pi)  !estimate of moment_3 tendency
             mu_i_new = mu_r(i,k)
             zitot(i,k,iice_dest) = zitot(i,k,iice_dest)+G_of_mu(mu_i_new)*tmp1**2/N_nuc
          endif ! log_3momentice
         ! update theta. Note temperature is NOT updated here, but currently not used after
          th(i,k) = th(i,k) + i_exn(i,k)*Q_nuc*xlf(i,k)*i_cp
          qr(i,k) = 0.  ! = qr(i,k) - Q_nuc
          nr(i,k) = 0.  ! = nr(i,k) - N_nuc

       endif qr_not_small_2

     endif freezing_possible

  enddo !i loop
 enddo k_loop_fz

!..............................................
! Merge ice categories with similar properties (based on specified similarly condition)

 multicat:  if (nCat.gt.1) then
!multicat:  if (.FALSE.) then       ! *** for testing

   !step 1:  adjustments and calculation of mean diameters
    k_loop_check_before_merge:  do k = kbot,ktop,kdir
     do i = its,ite

          iice_loop_check_before_merge:  do iice = 1,nCat
             qi_not_small_merge:  if (qitot(i,k,iice).ge.qsmall) then

                nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
                call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),   &
                                      birim(i,k,iice),rhop)
                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                          dum7,isize,rimsize,liqsize,densize,qitot(i,k,iice),            &
                          nitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),rhop)

                if (.not. log_3momentIce) then

                  call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum7,0.,0.,0.,0.,       &
                                    dumjj,dumii,dumll,dumi,0,0)
                  f1pr15 = proc_from_LUT_main2mom(LUT1_DMM,args_r,args_i)

                else ! triple moment ice

                   call get_mui_rhoi(mu_i,f1pr16,dum6,dumzz,qitot(i,k,iice),             &
                                  nitot(i,k,iice),zitot(i,k,iice),dum1,dum4,dum5,dum7,   &
                                  dumjj,dumii,dumll,dumi,zsize,zqsize)

                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,    &
                                     dumzz,dumjj,dumii,dumll,dumi,0)
                   f1pr15 = proc_from_LUT_main3mom(LUT1_DMM,args_r,args_i)

                   if (log_3momentIce) then
                      call apply_mui_bounds_to_zi(zitot(i,k,iice),qitot(i,k,iice),       &
                                                  nitot(i,k,iice),f1pr16)
                   endif

                endif

                qiliq(i,k,iice) = merge(0., qiliq(i,k,iice), qiliq(i,k,iice).lt.qsmall)

                diag_di(i,k,iice)   = f1pr15   ! used for merging

             else

                qv(i,k) = qv(i,k) + qitot(i,k,iice)
                th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*       &
                                     xxls(i,k)*i_cp
                th(i,k) = th(i,k) - i_exn(i,k)*qiliq(i,k,iice)*xxlv(i,k)*i_cp
                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                qiliq(i,k,iice) = 0.
                birim(i,k,iice) = 0.
                zitot(i,k,iice) = merge(0., zitot(i,k,iice), log_3momentIce)
                diag_di(i,k,iice) = 0.

             endif qi_not_small_merge
          enddo iice_loop_check_before_merge

     enddo !i loop
    enddo k_loop_check_before_merge

    !step 2:  merge ice with similar properties into one category
    do k = kbot,ktop,kdir
     do i = its,ite
          do iice = nCat,2,-1
             tmp1 = abs(diag_di(i,k,iice)-diag_di(i,k,iice-1))
             if (tmp1.le.deltaD_init .and. qitot(i,k,iice).gt.0. .and.                   &
                 qitot(i,k,iice-1).gt.0.) then
                qitot(i,k,iice-1) = qitot(i,k,iice-1) + qitot(i,k,iice)
                nitot(i,k,iice-1) = nitot(i,k,iice-1) + nitot(i,k,iice)
                qirim(i,k,iice-1) = qirim(i,k,iice-1) + qirim(i,k,iice)
                birim(i,k,iice-1) = birim(i,k,iice-1) + birim(i,k,iice)
                if (log_LiquidFrac) qiliq(i,k,iice-1) = qiliq(i,k,iice-1) + qiliq(i,k,iice)
                if (log_3momentIce) then
                   zitot(i,k,iice-1) = zitot(i,k,iice-1) + zitot(i,k,iice)
                   zitot(i,k,iice) = 0.
                endif
                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
                qiliq(i,k,iice) = 0.
             endif
          enddo !iice loop
     enddo !i loop
    enddo !k loop

 endif multicat

 if (log_LiquidFrac) call freeze_tiny_liqfrac(qitot,qiliq,qirim,birim,t, th,i_exn,xlf,i_cp)

 if (.not.log_predictNc) nc(:,:) = nccnst*i_rho(:,:)

!...................................................
! note: This debug check is commented since small negative qx,nx values are possible here
!       (but get adjusted below).  If uncommented, caution in interpreting results.
!
!    if (debug_on) then
!       location_ind = 700
!       force_abort  = .false.
!       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
!       if (log_3momentIce) then
!          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,         &
!                 Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!       else
!          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,                      &
!                 location_ind,Qiliq=qiliq(i,:,:))
!       endif
!       if (global_status /= STATUS_OK) return
!    endif


!......................................................................................
! final checks to ensure consistency of mass/number and compute output diagnostic fields

 k_loop_final_checks_diags: do k = kbot,ktop,kdir
  do i = its,ite

    ! cloud:
       if (qc(i,k)*iSCF(i,k).ge.qsmall) then
          call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k),  &
                              tmp1,tmp2, iSCF(i,k))
          diag_effc(i,k) = 0.5*(mu_c(i,k)+3.)/lamc(i,k)
          ze_cld(i,k)    = sngl(dble(rho(i,k)*nc(i,k)*(mu_c(i,k)+6.)*(mu_c(i,k)+5.)*     &
                           (mu_c(i,k)+4.)*(mu_c(i,k)+3.)*(mu_c(i,k)+2.)*(mu_c(i,k)+1.))/ &
                           dble(lamc(i,k))**6)
       else
          qv(i,k) = qv(i,k)+qc(i,k)
          th(i,k) = th(i,k)-i_exn(i,k)*qc(i,k)*xxlv(i,k)*i_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       endif

    ! rain:
       if (qr(i,k).ge.qsmall) then

          call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),tmp1,tmp2,1.)

         ! impose size limits for rain with 'soft' lambda limiter
         ! (adjusts over a set timescale rather than within one timestep)
         ! dum2 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd
         ! if (dum2.gt.dbrk) then
         !    dum   = qr(i,k)*cons4
         !   !dum1  = (dum-nr(i,k))/max(60.,dt)  !time scale for adjustment is 60 s
         !    dum1  = (dum-nr(i,k))*timeScaleFactor
         !     nr(i,k) = nr(i,k)+dum1*dt
         ! endif

         !diag_effr(i,k) = 0.5*(mu_r(i,k)+3.)/lamr(i,k)    (currently not used)
         !ze_rain(i,k) = n0r(i,k)*720./lamr(i,k)**3/lamr(i,k)**3/lamr(i,k)  !exponential DSD
          ze_rain(i,k) = rho(i,k)*nr(i,k)*(mu_r(i,k)+6.)*(mu_r(i,k)+5.)*(mu_r(i,k)+4.)*  &
                        (mu_r(i,k)+3.)*(mu_r(i,k)+2.)*(mu_r(i,k)+1.)/lamr(i,k)**6
       else
          qv(i,k) = qv(i,k)+qr(i,k)
          th(i,k) = th(i,k)-i_exn(i,k)*qr(i,k)*xxlv(i,k)*i_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       endif

    ! ice:
       call impose_max_Ni(nitot(i,k,:),max_Ni,i_rho(i,k))

       iice_loop_final_checks_diags:  do iice = 1,nCat

          qi_not_small:  if (qitot(i,k,iice).ge.qsmall) then

             nitot(i,k,iice) = max(nitot(i,k,iice),nsmall) !prevent taking log of # < 0
             nr(i,k)         = max(nr(i,k),nsmall)

             call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),      &
                                   birim(i,k,iice),rhop)

             trplmomice_3: if (.not. log_3momentIce) then

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                          dum7,isize,rimsize,liqsize,densize,qitot(i,k,iice),            &
                          nitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),rhop)

                call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum7,0.,0.,0.,0.,         &
                                  dumjj,dumii,dumll,dumi,0,0)

                f1pr02 = proc_from_LUT_main2mom(LUT1_UMS,args_r,args_i)
                f1pr06 = proc_from_LUT_main2mom(LUT1_EFF,args_r,args_i)
                f1pr09 = proc_from_LUT_main2mom(LUT1_QSML,args_r,args_i)
                f1pr10 = proc_from_LUT_main2mom(LUT1_QLRG,args_r,args_i)
                f1pr13 = proc_from_LUT_main2mom(LUT1_REFL,args_r,args_i)
                f1pr15 = proc_from_LUT_main2mom(LUT1_DMM,args_r,args_i)
                f1pr16 = proc_from_LUT_main2mom(LUT1_RHOMM,args_r,args_i)
                f1pr22 = proc_from_LUT_main2mom(LUT1_2M_LAM,args_r,args_i)
                f1pr23 = proc_from_LUT_main2mom(LUT1_2M_MU,args_r,args_i)

             else ! trplmomice_3

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,  &
                          dum7,isize,rimsize,liqsize,densize,qitot(i,k,iice),            &
                          nitot(i,k,iice),qirim(i,k,iice),qiliq(i,k,iice),rhop)

                call get_mui_rhoi(mu_i,f1pr16,dum6,dumzz,qitot(i,k,iice),                &
                               nitot(i,k,iice),zitot(i,k,iice),dum1,dum4,dum5,dum7,      &
                               dumjj,dumii,dumll,dumi,zsize,zqsize)

                call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,       &
                                  dumzz,dumjj,dumii,dumll,dumi,0)

                f1pr02 = proc_from_LUT_main3mom(LUT1_UMS,args_r,args_i)
                f1pr06 = proc_from_LUT_main3mom(LUT1_EFF,args_r,args_i)
                f1pr09 = proc_from_LUT_main3mom(LUT1_QSML,args_r,args_i)
                f1pr10 = proc_from_LUT_main3mom(LUT1_QLRG,args_r,args_i)
                f1pr13 = proc_from_LUT_main3mom(LUT1_REFL,args_r,args_i)
                f1pr15 = proc_from_LUT_main3mom(LUT1_DMM,args_r,args_i)
                f1pr22 = proc_from_LUT_main3mom(LUT1_3M_LAM,args_r,args_i)
                f1pr23 = proc_from_LUT_main3mom(LUT1_3M_MU,args_r,args_i)

             endif trplmomice_3

          ! impose mean ice size bounds (i.e. apply lambda limiters)
             nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
             nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))

             if (log_3momentIce) call apply_mui_bounds_to_zi(zitot(i,k,iice),            &
                                      qitot(i,k,iice),nitot(i,k,iice),f1pr16)

             qiliq(i,k,iice) = merge(0., qiliq(i,k,iice), qiliq(i,k,iice).lt.qsmall)

             diag_vmi(i,k,iice)  = f1pr02*rhofaci(i,k)
             diag_effi(i,k,iice) = f1pr06  !units are in m
             diag_di(i,k,iice)   = f1pr15
             diag_rhoi(i,k,iice) = f1pr16
             arr_lami(i,k,iice)  = f1pr22  !local use only (not for output)
             arr_mui(i,k,iice)   = f1pr23  !local use only

          ! note: air density factor below converts from m^6/kg to m^6/m^3
          ! also, reflectivity from lookup table is normalized, so need to multiply by N
             ze_ice(i,k) = ze_ice(i,k) + f1pr13*nitot(i,k,iice)*rho(i,k)

          else  !from 'if qi_not_small'

             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - i_exn(i,k)*(qitot(i,k,iice)-qiliq(i,k,iice))*          &
                                 xxls(i,k)*i_cp
             th(i,k) = th(i,k) - i_exn(i,k)*qiliq(i,k,iice)*xxlv(i,k)*i_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             qiliq(i,k,iice) = 0.
             birim(i,k,iice) = 0.
             zitot(i,k,iice) = 0
             diag_di(i,k,iice) = 0.

          endif qi_not_small

       enddo iice_loop_final_checks_diags

       diag_ze(i,k) = 10.*log10((ze_ice(i,k) + ze_rain(i,k) + ze_cld(i,k))*1.e+18)  ! convert to dBZ

     ! if qr is very small then set nr to 0 (needs to be done here after call
     ! to ice lookup table because a minimum nr of nsmall will be set otherwise even if qr=0
       nr(i,k) = merge(0., nr(i,k), qr(i,k).lt.qsmall)

  enddo !i loop
 enddo k_loop_final_checks_diags

!     if (debug_on) then
!        location_ind = 800
!        force_abort  = debug_ABORT
!        if (log_3momentIce) then
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
!                  Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!        else
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,                            &
!                  force_abort,location_ind,Qiliq=qiliq(i,:,:))
!        endif
!        if (global_status /= STATUS_OK) return
!     endif

!.....................................................

 if (log_3momentIce) where (qitot.lt.qsmall) zitot = 0.

 if (log_predictSsat) then
 ! recalculate supersaturation from T and qv
    do k = kbot,ktop,kdir
     do i = its,ite
         t(i,k) = th(i,k)*(1.e-5*pres(i,k))**(rd*i_cp)
         dum    = qv_sat(t(i,k),pres(i,k),0)
         ssat(i,k) = qv(i,k)-dum
     enddo
    enddo
 endif


! calculate 'binary' cloud fraction (0 or 1) (diagnostic only; used in GEM radiation interface)
 if (SCPF_on) then
    SCF_out(:,:) = SCF(:,:)
 else
   SCF_out(:,:) = merge(1., 0., qc(:,:).ge.qsmall)
   SCF_out(:,:) = merge(1., SCF_out(:,:), any(qitot(:,:,: ) >= qsmall, dim=3))
 endif

!     if (debug_on) then
!        location_ind = 900
!        force_abort  = debug_ABORT
!        tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*i_cp)
!        if (log_3momentIce) then
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                      qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,           &
!                      force_abort,location_ind,Zitot=zitot(i,:,:),Qiliq=qiliq(i,:,:))
!        else
!           call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),        &
!                      qitot(i,:,:),qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,           &
!                      force_abort,location_ind,Qiliq=qiliq(i,:,:))
!        endif
!        if (global_status /= STATUS_OK) return
!     endif

   !..............................................


! remove any supersaturation w.r.t to water
 if (log_liqsatadj) then
    do k = kbot,ktop,kdir
     do i = its,ite
         t_tmp   = th(i,k)*(pres(i,k)*1.e-5)**(rd*i_cp)
         qv_tmp  = qv(i,k)
         dumqvs  = qv_sat(t_tmp,pres(i,k),0)
         if (qv_tmp .gt. dumqvs) then
            dum     = (qv_tmp-dumqvs)/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*t_tmp**2))*i_dt
            qc(i,k) = qc(i,k)+dum*dt
            qv(i,k) = qv(i,k)-dum*dt
            th(i,k) = th(i,k) + 1./((pres(i,k)*1.e-5)**(rd*i_cp))*(dum*xxlv(i,k)*i_cp)*dt
         endif
     enddo
    enddo
 endif

!.....................................................


#ifdef TIMING_P3
!timer_description(2) = 'i_loop_main'
call cpu_time(timer_end(2))
#endif

! Save final microphysics values of theta and qv as old values for next time step
!  note: This is not necessary for GEM, which already has these values available
!        from the beginning of the model time step (TT_moins and HU_moins) when
!        s/r 'p3_wrapper_gem' is called (from s/r 'condensation').
 if (trim(model) == 'WRF' .or. trim(model) == 'CM1') then
    th_old = th
    qv_old = qv
 endif

!...........................................................................................
! Compute diagnostic hydrometeor types for output as 3D fields and
! for partitioning into corresponding surface precipitation rates.
!
!   In the code below, the full columns of diagnostic types are computed
!   only at the specified frequency, freq3Ddiag.  At all other time
!   steps, they are computed at the lowest level only (kbot) in order to
!   partition surface precipitation rates into types (and aslo for the
!   maximum hail size, dhmax).

#ifdef TIMING_P3
timer_description(9) = 'type_diags'
call cpu_time(timer_start(9))
#endif

 ! freq3Ddiag = 0: every step; < 0: never (mod() is not evaluated with a zero divisor)
 if (freq3Ddiag > 0.) then
    log_outputStep = (mod(it*dt,freq3Ddiag*60.)==0.)
 else
    log_outputStep = (freq3Ddiag==0.)
 endif

!--- diagnostics CM1:
 if (log_outputStep .and. trim(model)=='CM1') then
    diag_3d(:,:,1) = sum(qitot(:,:,:),dim=3)
!     do k = ktop,kbot,-kdir
!      do i = its,ite
!          do iice = 1,nCat
!             tmp1 = qirim(i,k,iice)/max(qsmall,qitot(i,k,iice)-qiliq(i,k,iice))  !rime fraction
!             diag_dhmax(i,k,iice) = maxHailSize(rho(i,k),nitot(i,k,iice),rhofaci(i,k),    &
!                                                arr_lami(i,k,iice),arr_mui(i,k,iice),     &
!                                                diag_rhoi(i,k,iice),tmp1)
!          enddo
!      enddo
!     enddo
 endif
!---

 compute_type_diags: if (log_typeDiags .and. (trim(model)=='GEM'.or.trim(model)=='KIN1D'    &
                         .or.trim(model)=='CM1')) then

    if (.not.(present(prt_drzl).and.present(prt_rain).and.present(prt_crys).and.         &
              present(prt_snow).and.present(prt_grpl).and.present(prt_pell).and.         &
              present(prt_hail).and.present(prt_sndp))) then
       print*,'***  ABORT IN P3_MAIN ***'
       print*,'*  typeDiags_ON = .true. but prt_drzl, etc. are not passed into P3_MAIN'
       print*,'*************************'
       global_status = STATUS_ERROR
       return
    endif

    prt_drzl(:) = 0.
    prt_rain(:) = 0.
    prt_crys(:) = 0.
    prt_snow(:) = 0.
    prt_grpl(:) = 0.
    prt_pell(:) = 0.
    prt_hail(:) = 0.
    prt_sndp(:) = 0.
    prt_wsnow(:) = 0.

    if (present(qi_type)) qi_type(:,:,:) = 0.

   ! compute hydrometeor type diagnostics for full columns on output timesteps only;
   ! otherwise only calculate at bottom level (for precipitation rates)
    ktop_typeDiag = merge(ktop, kbot, log_outputStep)

    i_loop_typediag: do i = its,ite

      !-- rain vs. drizzle:
       k_loop_typdiag_1: do k = kbot,ktop_typeDiag,kdir

          Q_drizzle(i,k) = 0.
          Q_rain(i,k)    = 0.
          !note:  these can be broken down further (outside of microphysics) into
          !       liquid rain (drizzle) vs. freezing rain (drizzle) based on sfc temp.
          if (qr(i,k).ge.qsmall .and. nr(i,k).ge.nsmall) then
!              if (tmp1 < thres_raindrop) then
!                 Q_drizzle(i,k) = qr(i,k)
!              else
!                 Q_rain(i,k)    = qr(i,k)
!              endif
             tmp2 = merge(1., 0., tmp1 < thres_raindrop) !1. for drizzle, 0. for rain
             Q_drizzle(i,k) = qr(i,k)*tmp2
             Q_rain(i,k)    = qr(i,k)*(1.-tmp2)
          endif

       enddo k_loop_typdiag_1

!        if (Q_drizzle(i,kbot) > 0.) then
!           prt_drzl(i) = prt_liq(i)
!        elseif (Q_rain(i,kbot) > 0.) then
!           prt_rain(i) = prt_liq(i)
!        endif
       tmp1 = merge(1., 0., Q_drizzle(i,kbot) > 0.)
       prt_drzl(i) = prt_liq(i)*tmp1
       prt_rain(i) = prt_liq(i)*(1.-tmp1)

      !-- ice-phase:
      iice_loop_diag: do iice = 1,nCat

          k_loop_typdiag_2: do k = kbot,ktop_typeDiag,kdir

             Q_crystals(i,k,iice) = 0.
             Q_snow(i,k,iice)     = 0.
             Q_wsnow(i,k,iice)    = 0.
             Q_grpl(i,k,iice)     = 0.
             Q_pellets(i,k,iice)  = 0.
             Q_hail(i,k,iice)     = 0.
             liq_frac(i,k,iice)   = 0.
             rime_frac(i,k,iice)  = 0.
             rimedensity(i,k,iice) = 0.

             if ((qitot(i,k,iice)-qiliq(i,k,iice)).ge.qsmall .and.                    &
                  qitot(i,k,iice).ge.qsmall) then
                rime_frac(i,k,iice) = qirim(i,k,iice)/(qitot(i,k,iice)-               &
                                         qiliq(i,k,iice))                     ! rime mass fraction
                t_tmp   = th(i,kbot)*(pres(i,kbot)*1.e-5)**(rd*i_cp)          ! 1st level temperature
                if (birim(i,k,iice).ge.bsmall) then
                   rimedensity(i,k,iice) = qirim(i,k,iice)/birim(i,k,iice)    ! rime density
                endif
                liq_frac(i,k,iice) = qiliq(i,k,iice)/qitot(i,k,iice)          ! liquid fraction

                ! wet snow: wet AND lightly rimed; melting rimed ice goes on to the hail branch
                if (liq_frac(i,k,iice).ge.0.15 .and. rime_frac(i,k,iice).lt.0.6) then
                   Q_wsnow(i,k,iice) = qitot(i,k,iice)
                else
                   if (rime_frac(i,k,iice).lt.0.6) then
!                       if (diag_di(i,k,iice).lt.0.002) then
!                          Q_crystals(i,k,iice) = qitot(i,k,iice)
!                       else
!                          Q_snow(i,k,iice) = qitot(i,k,iice)
!                       endif
                      tmp1 = merge(1., 0., diag_di(i,k,iice).lt.0.002)
                      Q_crystals(i,k,iice) = qitot(i,k,iice)*tmp1
                      Q_snow(i,k,iice)     = qitot(i,k,iice)*(1.-tmp1)
                   else
                      if (rimedensity(i,k,iice).lt.850) then
                        Q_grpl(i,k,iice) = qitot(i,k,iice)
                      else
                        if (t_tmp.lt.283.15) then
                           Q_pellets(i,k,iice) = qitot(i,k,iice)
                        else
                           Q_hail(i,k,iice) = qitot(i,k,iice)
                           if (log_typeDiags .and. qitot(i,k,iice).ge.qsmall_dhmax) then
                              diag_dhmax(i,k,iice) = maxHailSize(rho(i,k),               &
                               nitot(i,k,iice),rhofaci(i,k),arr_lami(i,k,iice),          &
                               arr_mui(i,k,iice),diag_rhoi(i,k,iice),rime_frac(i,k,iice))
                           endif
                        endif
                        !here, surface temperature is a proxy for the likelihood of hail being physically reasonable
                        tmp1 = merge(1., 0., t_tmp.lt.283.15)
                        Q_pellets(i,k,iice) = qitot(i,k,iice)*tmp1
                        Q_hail(i,k,iice)    = qitot(i,k,iice)*(1.-tmp1)
                      endif
                   endif
                endif

             endif !qitot-qiliq>0

          enddo k_loop_typdiag_2

         !diagnostics for sfc precipitation rates: (liquid-equivalent volume flux, m s-1)
         !  note: these are summed for all ice categories
          if (Q_crystals(i,kbot,iice) .gt. 0.)    then
             prt_crys(i) = prt_crys(i) + prt_soli(i,iice)   !precip rate of small crystals
          elseif (Q_snow(i,kbot,iice) .gt. 0.)  then
             prt_snow(i) = prt_snow(i) + prt_soli(i,iice)   !precip rate of snow
          elseif (Q_wsnow(i,kbot,iice) .gt. 0.)  then
             prt_wsnow(i) = prt_wsnow(i) + prt_soli(i,iice) !precip rate of wsnow (wet low-rimed snow)
          elseif (Q_grpl(i,kbot,iice) .gt. 0.)    then
             prt_grpl(i) = prt_grpl(i) + prt_soli(i,iice)   !precip rate of graupel
          elseif (Q_pellets(i,kbot,iice) .gt. 0.) then
             prt_pell(i) = prt_pell(i) + prt_soli(i,iice)   !precip rate of ice pellets
          elseif (Q_hail(i,kbot,iice) .gt. 0.)    then
             prt_hail(i) = prt_hail(i) + prt_soli(i,iice)   !precip rate of hail
          endif

          !precip rate of unmelted total "snow":
          !  For now, an instananeous solid-to-liquid ratio (tmp1) is assumed and is multiplied
          !  by the total liquid-equivalent precip rates of snow (small crystals + lightly-rime + ..)
          !  Later, this can be computed explicitly as the volume flux of unmelted ice.
         !tmp1 = 10.  !assumes 10:1 ratio
         !tmp1 = 1000./max(1., diag_rhoi(i,kbot,iice))
          tmp1 = 1000./max(1., 5.*diag_rhoi(i,kbot,iice))
          ! Should we add prt_hail and prt_pell here
          prt_sndp(i) = prt_sndp(i) + tmp1*(prt_crys(i) + prt_snow(i) + prt_grpl(i))

       enddo iice_loop_diag

    enddo i_loop_typediag

   !- for output of 3D fields of diagnostic ice-phase hydrometeor type
    if (ktop_typeDiag==ktop .and. present(qi_type)) then
      !diag_3d(:,:,1) = Q_drizzle(:,:)
      !diag_3d(:,:,2) = Q_rain(:,:)
       do ii = 1,nCat
          qi_type(:,:,1) = qi_type(:,:,1) + Q_crystals(:,:,ii)
          qi_type(:,:,2) = qi_type(:,:,2) + Q_snow(:,:,ii)
          qi_type(:,:,3) = qi_type(:,:,3) + Q_wsnow(:,:,ii)
          qi_type(:,:,4) = qi_type(:,:,4) + Q_grpl(:,:,ii)
          qi_type(:,:,5) = qi_type(:,:,5) + Q_hail(:,:,ii)
          qi_type(:,:,6) = qi_type(:,:,6) + Q_pellets(:,:,ii)
       enddo
    endif

 endif compute_type_diags


 diag_visibility: if (present(diag_vis)  .and. present(diag_vis1) .and.                  &
                      present(diag_vis1) .and. present(diag_vis2)) then

    if (log_outputStep) then

       do k = kbot,ktop,kdir
        do i = its,ite

             !VIS1:  component through liquid cloud (fog); based on Gultepe and Milbrandt, 2007)
             tmp1 = qc(i,k)*rho(i,k)*1.e+3    !LWC [g m-3]
             tmp2 = nc(i,k)*rho(i,k)*1.e-6    !Nc  [cm-3]
             if (tmp1>0.005 .and. tmp2>1.) then
                diag_vis1(i,k) = max(minVIS,1000.*(1.13*(tmp1*tmp2)**(-0.51)))  !based on GM2007, eqn (4)
             else
                diag_vis1(i,k) = maxVIS
             endif

            !VIS2: component through rain;  based on Gultepe and Milbrandt, 2008, Table 2 eqn (1)
             tmp1 = massflux_r(i,k)*i_rhow*3.6e+6                               !rain rate [mm h-1]
             if (tmp1>0.01) then
                diag_vis2(i,k) = max(10.,1000.*(-4.12*tmp1**0.176+9.01))   ![m]
             else
                diag_vis2(i,k) = maxVIS
             endif

            !VIS3: component through snow;  based on Gultepe and Milbrandt, 2008, Table 2 eqn (6)
            !      - for nCat=1 only (to simplify and reduce cost; for operational use in HRDPS)
             tmp1 = diag_vmi(i,k,1)*qitot(i,k,1)*rho(i,k)*i_rhow*3.6e+6     !solid precip rate, liq-eq [mm h-1]
             if (tmp1>0.01) then
                diag_vis3(i,k) = max(minVIS,1000.*(1.10*tmp1**(-0.701)))      ![m]
             else
                diag_vis3(i,k) = maxVIS
             endif

             !VIS:  visibility due to reduction from all components 1, 2, and 3
             !      (based on sum of extinction coefficients and Koschmieders's Law)
             diag_vis(i,k) = min(maxVIS, 1./(1./diag_vis1(i,k) + 1./diag_vis2(i,k) +     &
                                             1./diag_vis3(i,k)))
             diag_vis1(i,k)= min(maxVIS, diag_vis1(i,k))
             diag_vis2(i,k)= min(maxVIS, diag_vis2(i,k))
             diag_vis3(i,k)= min(maxVIS, diag_vis3(i,k))

        enddo !i loop
       enddo !k loop

    else

       diag_vis(:,:)  = maxVIS
       diag_vis1(:,:) = maxVIS
       diag_vis2(:,:) = maxVIS
       diag_vis3(:,:) = maxVIS

    endif  !log_outputStep

 endif diag_visibility


#ifdef TIMING_P3
timer_description(9) = 'type_diags'
call cpu_time(timer_end(9))
#endif

 ! convert zitot to advected (dynamics) variable
 if (log_3momentIce) zitot = sqrt(zitot*nitot)

! end of main microphysics routine

#ifdef TIMING_P3
! for entire call to p3_main
call cpu_time(timer_end(1))
#endif

#ifdef TIMING_P3
timer(:) = timer_end(:) - timer_start(:)
#endif

 return

 END SUBROUTINE p3_main

!==========================================================================================!

! Scalar linear interpolation — gfortran inlines elemental functions unconditionally.
! Weight w = coord - real(idx).  The proc_from_LUT_* routines below spell out the
! corner-lookup tree explicitly so the compiler sees a static expression tree.

 elemental pure real function lerp(a, b, w)
 real, intent(in) :: a, b, w
 lerp = a + w*(b-a)
 end function lerp

!==========================================================================================!

 real function proc_from_LUT_main2mom(ind,args_r,args_i)

 !--------------------------------------------------------------------------------
 ! Obtains process rate (or other quantity) from LUT by accessing values from the
 ! LUT and performing the necessary interpolation.
 !
 ! This applies for the main LUT for 2-moment (LF on or off)
 ! Table dims: (density jj, rime fraction ii, liquid fraction ll, size i, quantity)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 integer, intent(in) :: ind
 real,    dimension(n_args_r), intent(in) :: args_r
 integer, dimension(n_args_i), intent(in) :: args_i
!local:
 real    :: iproc1,iproc2,iproc3,iproc4,tmp1,tmp2,tmp3,tmp4
 real    :: dum1,dum4,dum5,dum7
 integer :: dumjj,dumii,dumll,dumi

 !note: arg_r(5) and arg_i(5) are not used for main2mom
 dum1 = args_r(1);  dum4 = args_r(2);  dum5 = args_r(3);  dum7 = args_r(4)
 dumjj = args_i(1); dumii = args_i(2); dumll = args_i(3);  dumi = args_i(4)

 if (dumll==1 .and. dum7 < 1. + epsilon(dum7)) then  !skip interpolation for liq-frac if qiliq = 0.

    iproc1 = lerp(itab(dumjj,  dumii,  dumll,dumi,  ind), itab(dumjj,  dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(itab(dumjj,  dumii+1,dumll,dumi,  ind), itab(dumjj,  dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    tmp1   = lerp(iproc1, iproc2, dum4-real(dumii))

    iproc1 = lerp(itab(dumjj+1,dumii,  dumll,dumi,  ind), itab(dumjj+1,dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(itab(dumjj+1,dumii+1,dumll,dumi,  ind), itab(dumjj+1,dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    tmp2   = lerp(iproc1, iproc2, dum4-real(dumii))

    proc_from_LUT_main2mom = lerp(tmp1, tmp2, dum5-real(dumjj))

 else

    iproc1 = lerp(itab(dumjj,  dumii,  dumll,  dumi,ind), itab(dumjj,  dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(itab(dumjj,  dumii,  dumll+1,dumi,ind), itab(dumjj,  dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    tmp1   = lerp(iproc1, iproc2, dum7-real(dumll))

    iproc3 = lerp(itab(dumjj,  dumii+1,dumll,  dumi,ind), itab(dumjj,  dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    iproc4 = lerp(itab(dumjj,  dumii+1,dumll+1,dumi,ind), itab(dumjj,  dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    tmp2   = lerp(iproc3, iproc4, dum7-real(dumll))
    tmp3   = lerp(tmp1, tmp2, dum4-real(dumii))

    iproc1 = lerp(itab(dumjj+1,dumii,  dumll,  dumi,ind), itab(dumjj+1,dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(itab(dumjj+1,dumii,  dumll+1,dumi,ind), itab(dumjj+1,dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    tmp1   = lerp(iproc1, iproc2, dum7-real(dumll))

    iproc3 = lerp(itab(dumjj+1,dumii+1,dumll,  dumi,ind), itab(dumjj+1,dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    iproc4 = lerp(itab(dumjj+1,dumii+1,dumll+1,dumi,ind), itab(dumjj+1,dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    tmp2   = lerp(iproc3, iproc4, dum7-real(dumll))
    tmp4   = lerp(tmp1, tmp2, dum4-real(dumii))

    proc_from_LUT_main2mom = lerp(tmp3, tmp4, dum5-real(dumjj))

 endif

 end function proc_from_LUT_main2mom

!==========================================================================================!


 real function proc_from_LUT_ir2mom(ind,args_r,args_i)

 !--------------------------------------------------------------------------------
 ! Returns process rate (or other quantity) from LUT by accessing values from the
 ! LUT and performing the necessary interpolation.
 !
 ! This applies for the ice-rain collection LUT for 2-moment.
 ! Table dims: (density jj, rime fraction ii, liquid fraction ll, size i,
 !              rain size j, quantity)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 integer, intent(in) :: ind
 real,    dimension(n_args_r), intent(in) :: args_r
 integer, dimension(n_args_i), intent(in) :: args_i
!local:
 real    :: dproc1,dproc2,iproc1,iproc2,iproc3,iproc4,tmp1,tmp2
 real    :: dum1,dum3,dum4,dum5,dum7
 integer :: dumjj,dumii,dumll,dumj,dumi

 dum1 = args_r(1);  dum3 = args_r(2);  dum4 = args_r(3)
 dum5 = args_r(4);  dum7 = args_r(5)
 dumjj = args_i(1); dumii = args_i(2); dumll = args_i(3)
 dumj  = args_i(4); dumi  = args_i(5)

 if (dumll==1 .and. dum7 < 1. + epsilon(dum7)) then  !skip interpolation for liq-frac if qiliq = 0.

    dproc1 = lerp(itabcoll(dumjj,  dumii,  dumll,dumi,  dumj,  ind), itabcoll(dumjj,  dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii,  dumll,dumi,  dumj+1,ind), itabcoll(dumjj,  dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))

    dproc1 = lerp(itabcoll(dumjj,  dumii+1,dumll,dumi,  dumj,  ind), itabcoll(dumjj,  dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii+1,dumll,dumi,  dumj+1,ind), itabcoll(dumjj,  dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp1   = lerp(iproc1, iproc2, dum4-real(dumii))

    dproc1 = lerp(itabcoll(dumjj+1,dumii,  dumll,dumi,  dumj,  ind), itabcoll(dumjj+1,dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii,  dumll,dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))

    dproc1 = lerp(itabcoll(dumjj+1,dumii+1,dumll,dumi,  dumj,  ind), itabcoll(dumjj+1,dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii+1,dumll,dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp2   = lerp(iproc1, iproc2, dum4-real(dumii))

    proc_from_LUT_ir2mom = lerp(tmp1, tmp2, dum5-real(dumjj))

 else

    dproc1 = lerp(itabcoll(dumjj,  dumii,  dumll,  dumi,  dumj,  ind), itabcoll(dumjj,  dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii,  dumll,  dumi,  dumj+1,ind), itabcoll(dumjj,  dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll(dumjj,  dumii,  dumll+1,dumi,  dumj,  ind), itabcoll(dumjj,  dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll(dumjj,  dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))

    dproc1 = lerp(itabcoll(dumjj,  dumii+1,dumll,  dumi,  dumj,  ind), itabcoll(dumjj,  dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll(dumjj,  dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll(dumjj,  dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll(dumjj,  dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj,  dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll(dumjj,  dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp1   = lerp(iproc3, iproc4, dum4-real(dumii))

    dproc1 = lerp(itabcoll(dumjj+1,dumii,  dumll,  dumi,  dumj,  ind), itabcoll(dumjj+1,dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii,  dumll,  dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll(dumjj+1,dumii,  dumll+1,dumi,  dumj,  ind), itabcoll(dumjj+1,dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))

    dproc1 = lerp(itabcoll(dumjj+1,dumii+1,dumll,  dumi,  dumj,  ind), itabcoll(dumjj+1,dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll(dumjj+1,dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll(dumjj+1,dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll(dumjj+1,dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll(dumjj+1,dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp2   = lerp(iproc3, iproc4, dum4-real(dumii))

    proc_from_LUT_ir2mom = lerp(tmp1, tmp2, dum5-real(dumjj))

 endif

 end function proc_from_LUT_ir2mom

!==========================================================================================!


real function proc_from_LUT_3(ind,dumzq,dumjj,dumii,dumll,dumi,dum1,dum4,dum5,dum7,dum8)

 !--------------------------------------------------------------------------------
 ! Returns process rate (or other quantity) from LUT_3 by accessing values from the
 ! LUT and performing the necessary interpolation.
 !
 ! This applies for mu_i and rho_i LUT.
 ! Table dims: (G index zq, density jj, rime fraction ii, liquid fraction ll,
 !              size i, quantity)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 real,    intent(in)  :: dum1,dum4,dum5,dum7,dum8
 integer, intent(in)  :: ind,dumzq,dumjj,dumii,dumll,dumi
!local:
 real :: iproc1,iproc2,gproc1,gproc2,rproc1,rproc2,dproc1,dproc2

 if (dumll==1 .and. dum7 < 1. + epsilon(dum7)) then  !skip interpolation for liq-frac if qiliq = 0.

    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll,dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll,dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc1 = lerp(dproc1, dproc2, dum4-real(dumii))
    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll,dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll,dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc2 = lerp(dproc1, dproc2, dum4-real(dumii))
    rproc1 = lerp(gproc1, gproc2, dum5-real(dumjj))

    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc1 = lerp(dproc1, dproc2, dum4-real(dumii))
    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc2 = lerp(dproc1, dproc2, dum4-real(dumii))
    rproc2 = lerp(gproc1, gproc2, dum5-real(dumjj))

    proc_from_LUT_3 = lerp(rproc1, rproc2, dum8-real(dumzq))

 else

    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll,  dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll+1,dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll,  dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll+1,dumi,  ind), itab_3mom_mui(dumzq,  dumjj,  dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc1 = lerp(iproc1, iproc2, dum4-real(dumii))

    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll,  dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll+1,dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll,  dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll+1,dumi,  ind), itab_3mom_mui(dumzq,  dumjj+1,dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc2 = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc1 = lerp(gproc1, gproc2, dum5-real(dumjj))

    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll,  dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll+1,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll,  dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll+1,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj,  dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc1 = lerp(iproc1, iproc2, dum4-real(dumii))

    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll,  dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll+1,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll,  dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll+1,dumi,  ind), itab_3mom_mui(dumzq+1,dumjj+1,dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc2 = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc2 = lerp(gproc1, gproc2, dum5-real(dumjj))

    proc_from_LUT_3 = lerp(rproc1, rproc2, dum8-real(dumzq))

 endif

end function proc_from_LUT_3

!==========================================================================================!


!**** NOTE: optimization can be made here if filiq=0 ****

 pure real function lerp6_colli(tab, dumic,dumiic,dumjjc,dumi,dumii,dumjj, w1c,w4c,w5c,w1,w4,w5)

 !--------------------------------------------------------------------------------
 ! 6-D corner-tree interpolation for an itabcolli* table (no runtime loops).
 ! Dims: (ic, iic, jjc, i, ii, jj); reduction order: ic,i,ii,jj,iic,jjc.
 !--------------------------------------------------------------------------------

 implicit none
 real, dimension(:,:,:,:,:,:), intent(in) :: tab
 integer, intent(in) :: dumic,dumiic,dumjjc,dumi,dumii,dumjj
 real,    intent(in) :: w1c,w4c,w5c,w1,w4,w5
 real :: d1,d2,i1,i2,t1,t2,g1,g2,r1,r2

 ! (iic, jjc)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi,  dumii,  dumjj  ), tab(dumic+1,dumiic,  dumjjc,  dumi,  dumii,  dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi+1,dumii,  dumjj  ), tab(dumic+1,dumiic,  dumjjc,  dumi+1,dumii,  dumjj  ), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi,  dumii+1,dumjj  ), tab(dumic+1,dumiic,  dumjjc,  dumi,  dumii+1,dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi+1,dumii+1,dumjj  ), tab(dumic+1,dumiic,  dumjjc,  dumi+1,dumii+1,dumjj  ), w1c)
 i2 = lerp(d1,d2,w1)
 t1 = lerp(i1,i2,w4)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi,  dumii,  dumjj+1), tab(dumic+1,dumiic,  dumjjc,  dumi,  dumii,  dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi+1,dumii,  dumjj+1), tab(dumic+1,dumiic,  dumjjc,  dumi+1,dumii,  dumjj+1), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi,  dumii+1,dumjj+1), tab(dumic+1,dumiic,  dumjjc,  dumi,  dumii+1,dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc,  dumi+1,dumii+1,dumjj+1), tab(dumic+1,dumiic,  dumjjc,  dumi+1,dumii+1,dumjj+1), w1c)
 i2 = lerp(d1,d2,w1)
 t2 = lerp(i1,i2,w4)
 g1 = lerp(t1,t2,w5)
 ! (iic+1, jjc)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi,  dumii,  dumjj  ), tab(dumic+1,dumiic+1,dumjjc,  dumi,  dumii,  dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi+1,dumii,  dumjj  ), tab(dumic+1,dumiic+1,dumjjc,  dumi+1,dumii,  dumjj  ), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi,  dumii+1,dumjj  ), tab(dumic+1,dumiic+1,dumjjc,  dumi,  dumii+1,dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi+1,dumii+1,dumjj  ), tab(dumic+1,dumiic+1,dumjjc,  dumi+1,dumii+1,dumjj  ), w1c)
 i2 = lerp(d1,d2,w1)
 t1 = lerp(i1,i2,w4)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi,  dumii,  dumjj+1), tab(dumic+1,dumiic+1,dumjjc,  dumi,  dumii,  dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi+1,dumii,  dumjj+1), tab(dumic+1,dumiic+1,dumjjc,  dumi+1,dumii,  dumjj+1), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi,  dumii+1,dumjj+1), tab(dumic+1,dumiic+1,dumjjc,  dumi,  dumii+1,dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc,  dumi+1,dumii+1,dumjj+1), tab(dumic+1,dumiic+1,dumjjc,  dumi+1,dumii+1,dumjj+1), w1c)
 i2 = lerp(d1,d2,w1)
 t2 = lerp(i1,i2,w4)
 g2 = lerp(t1,t2,w5)
 r1 = lerp(g1,g2,w4c)
 ! (iic, jjc+1)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi,  dumii,  dumjj  ), tab(dumic+1,dumiic,  dumjjc+1,dumi,  dumii,  dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi+1,dumii,  dumjj  ), tab(dumic+1,dumiic,  dumjjc+1,dumi+1,dumii,  dumjj  ), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi,  dumii+1,dumjj  ), tab(dumic+1,dumiic,  dumjjc+1,dumi,  dumii+1,dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi+1,dumii+1,dumjj  ), tab(dumic+1,dumiic,  dumjjc+1,dumi+1,dumii+1,dumjj  ), w1c)
 i2 = lerp(d1,d2,w1)
 t1 = lerp(i1,i2,w4)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi,  dumii,  dumjj+1), tab(dumic+1,dumiic,  dumjjc+1,dumi,  dumii,  dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi+1,dumii,  dumjj+1), tab(dumic+1,dumiic,  dumjjc+1,dumi+1,dumii,  dumjj+1), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi,  dumii+1,dumjj+1), tab(dumic+1,dumiic,  dumjjc+1,dumi,  dumii+1,dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic,  dumjjc+1,dumi+1,dumii+1,dumjj+1), tab(dumic+1,dumiic,  dumjjc+1,dumi+1,dumii+1,dumjj+1), w1c)
 i2 = lerp(d1,d2,w1)
 t2 = lerp(i1,i2,w4)
 g1 = lerp(t1,t2,w5)
 ! (iic+1, jjc+1)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi,  dumii,  dumjj  ), tab(dumic+1,dumiic+1,dumjjc+1,dumi,  dumii,  dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi+1,dumii,  dumjj  ), tab(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,  dumjj  ), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi,  dumii+1,dumjj  ), tab(dumic+1,dumiic+1,dumjjc+1,dumi,  dumii+1,dumjj  ), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj  ), tab(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj  ), w1c)
 i2 = lerp(d1,d2,w1)
 t1 = lerp(i1,i2,w4)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi,  dumii,  dumjj+1), tab(dumic+1,dumiic+1,dumjjc+1,dumi,  dumii,  dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi+1,dumii,  dumjj+1), tab(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,  dumjj+1), w1c)
 i1 = lerp(d1,d2,w1)
 d1 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi,  dumii+1,dumjj+1), tab(dumic+1,dumiic+1,dumjjc+1,dumi,  dumii+1,dumjj+1), w1c)
 d2 = lerp(tab(dumic,  dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1), tab(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1), w1c)
 i2 = lerp(d1,d2,w1)
 t2 = lerp(i1,i2,w4)
 g2 = lerp(t1,t2,w5)
 r2 = lerp(g1,g2,w4c)

 lerp6_colli = lerp(r1,r2,w5c)

 end function lerp6_colli

!==========================================================================================!

 real function proc_from_LUT_ii(ind,args_r,args_i)

 !--------------------------------------------------------------------------------
 ! Returns process rate (or other quantity) from the ice-ice collection LUT (LUT2)
 ! by 6-D interpolation.
 !
 ! The liquid-fraction dependence is carried by 8 separate tables (2 collectee-
 ! liqfrac levels x 2 collector-liqfrac levels x 2 quantities); the tables are
 ! interpolated individually and blended linearly in the two liquid fractions.
 !
 ! Table dims: (collectee size ic, collectee rime fraction iic, collectee density
 !              jjc, collector size i, collector rime fraction ii, collector
 !              density jj)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 integer, intent(in) :: ind
 real,    dimension(n_args_r), intent(in) :: args_r
 integer, dimension(n_args_i), intent(in) :: args_i
!local:
 real    :: dum1c,dum4c,dum5c,dum7c,dum1,dum4,dum5,dum7
 real    :: w1c,w4c,w5c,w1,w4,w5,procll1,procll2
 integer :: dumjjc,dumiic,dumic,dumjj,dumii,dumi

 dum1c = args_r(1); dum4c = args_r(2); dum5c = args_r(3); dum7c = args_r(4)
 dum1  = args_r(5); dum4  = args_r(6); dum5  = args_r(7); dum7  = args_r(8)
 dumjjc = args_i(1); dumiic = args_i(2); dumic = args_i(3)
 dumjj  = args_i(4); dumii  = args_i(5); dumi  = args_i(6)
 w1c = dum1c-real(dumic);  w4c = dum4c-real(dumiic);  w5c = dum5c-real(dumjjc)
 w1  = dum1 -real(dumi);   w4  = dum4 -real(dumii);   w5  = dum5 -real(dumjj)

! note: unlike the LUT1 functions (where dum7 is a table coordinate in [1,liqsize] and
!       dum7=1 means qiliq=0), here dum7/dum7c are raw liquid mass fractions in [0,1]
!       (see find_lookupTable_indices_2) used directly as blend weights below.
!       qiliq=0 gives exactly 0. (0./qitot), so comparison to 0. is exact and safe.
!       Skipping the blend when both are 0. is bit-identical to the full blend:
!       (1.-0.)*proc001 + 0.*proc011 = proc001.
 if (dum7c == 0. .and. dum7 == 0.) then  !skip liq-frac blending if qiliq = 0. for both categories

    if (ind == LUTII_DN) then
       proc_from_LUT_ii = lerp6_colli(itabcolli001,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
    else
       proc_from_LUT_ii = lerp6_colli(itabcolli002,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
    endif

 else

    if (ind == LUTII_DN) then
       procll1 = (1.-dum7)*lerp6_colli(itabcolli001,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5) &
                +     dum7*lerp6_colli(itabcolli011,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
       procll2 = (1.-dum7)*lerp6_colli(itabcolli101,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5) &
                +     dum7*lerp6_colli(itabcolli111,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
    else
       procll1 = (1.-dum7)*lerp6_colli(itabcolli002,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5) &
                +     dum7*lerp6_colli(itabcolli012,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
       procll2 = (1.-dum7)*lerp6_colli(itabcolli102,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5) &
                +     dum7*lerp6_colli(itabcolli112,dumic,dumiic,dumjjc,dumi,dumii,dumjj,w1c,w4c,w5c,w1,w4,w5)
    endif

    proc_from_LUT_ii = (1.-dum7c)*procll1 + dum7c*procll2

 endif

 end function proc_from_LUT_ii

!==========================================================================================!


 subroutine args_for_LUT(args_r,args_i,                                                    &
                         arg_r_1,arg_r_2,arg_r_3,arg_r_4,arg_r_5,arg_r_6,arg_r_7,arg_r_8,  &
                         arg_i_1,arg_i_2,arg_i_3,arg_i_4,arg_i_5,arg_i_6)

 !--------------------------------------------------------------------------------
 ! Consolidates individual real and integer values used to access the LUTs
 ! into 2 arrays.  This is just to simplify the readibility of the code in p3_main.
 ! The two returned arrays contain the arguments to call the next function called in
 ! in the group 'proc_from_LUT_[x]'.
 !
 ! Note, for some functions 'proc_from_LUT_[x]', only a subset of the arguments are used.
 ! "Blank" values (0. or 0) are passed in here to create args_r and args_i but are
 ! ignored in the given 'proc_from_LUT_[x]' function.
 !--------------------------------------------------------------------------------

!arguments:
 real,    dimension(n_args_r), intent(out) :: args_r
 integer, dimension(n_args_i), intent(out) :: args_i
 real,    intent(in) :: arg_r_1,arg_r_2,arg_r_3,arg_r_4,arg_r_5,arg_r_6,arg_r_7,arg_r_8
 integer, intent(in) :: arg_i_1,arg_i_2,arg_i_3,arg_i_4,arg_i_5,arg_i_6

 args_r(1) = arg_r_1
 args_r(2) = arg_r_2
 args_r(3) = arg_r_3
 args_r(4) = arg_r_4
 args_r(5) = arg_r_5
 args_r(6) = arg_r_6
 args_r(7) = arg_r_7
 args_r(8) = arg_r_8

 args_i(1) = arg_i_1
 args_i(2) = arg_i_2
 args_i(3) = arg_i_3
 args_i(4) = arg_i_4
 args_i(5) = arg_i_5
 args_i(6) = arg_i_6

 end subroutine args_for_LUT
!==========================================================================================!

 real function proc_from_LUT_main3mom(ind,args_r,args_i)

 !--------------------------------------------------------------------------------
 ! Obtains process rate (or other quantity) from LUT by accessing values from the
 ! LUT and performing the necessary interpolation.
 !
 ! This applies for the main LUT for 3-moment (LF on or off)
 ! Table dims: (Z index zz, density jj, rime fraction ii, liquid fraction ll,
 !              size i, quantity)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 integer, intent(in) :: ind
 real,    dimension(n_args_r), intent(in) :: args_r
 integer, dimension(n_args_i), intent(in) :: args_i
!local:
 real    :: iproc1,iproc2,gproc1,gproc2,rproc1,rproc2,dproc1,dproc2
 real    :: dum1,dum4,dum5,dum6,dum7
 integer :: dumzz,dumjj,dumii,dumll,dumi

 dum1 = args_r(1);  dum4 = args_r(2);  dum5 = args_r(3)
 dum6 = args_r(4);  dum7 = args_r(5)
 dumzz = args_i(1); dumjj = args_i(2); dumii = args_i(3)
 dumll = args_i(4); dumi  = args_i(5)

 if (dumll==1 .and. dum7 < 1. + epsilon(dum7)) then  !skip interpolation for liq-frac if qiliq = 0.

    dproc1 = lerp(itab_3mom(dumzz,  dumjj,  dumii,  dumll,dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc1 = lerp(dproc1, dproc2, dum4-real(dumii))
    dproc1 = lerp(itab_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc2 = lerp(dproc1, dproc2, dum4-real(dumii))
    rproc1 = lerp(gproc1, gproc2, dum5-real(dumjj))

    dproc1 = lerp(itab_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc1 = lerp(dproc1, dproc2, dum4-real(dumii))
    dproc1 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi+1,ind), dum1-real(dumi))
    gproc2 = lerp(dproc1, dproc2, dum4-real(dumii))
    rproc2 = lerp(gproc1, gproc2, dum5-real(dumjj))

    proc_from_LUT_main3mom = lerp(rproc1, rproc2, dum6-real(dumzz))

 else

    dproc1 = lerp(itab_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi,  ind), itab_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc1 = lerp(iproc1, iproc2, dum4-real(dumii))

    dproc1 = lerp(itab_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi,  ind), itab_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc2 = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc1 = lerp(gproc1, gproc2, dum5-real(dumjj))

    dproc1 = lerp(itab_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi,  ind), itab_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc1 = lerp(iproc1, iproc2, dum4-real(dumii))

    dproc1 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum7-real(dumll))
    dproc1 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi+1,ind), dum1-real(dumi))
    dproc2 = lerp(itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi,  ind), itab_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum7-real(dumll))
    gproc2 = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc2 = lerp(gproc1, gproc2, dum5-real(dumjj))

    proc_from_LUT_main3mom = lerp(rproc1, rproc2, dum6-real(dumzz))

 endif

 end function proc_from_LUT_main3mom

!==========================================================================================!


 subroutine find_lookupTable_indices_3a(dumzq,dum8,zqsize,zitot,qitot)

 !------------------------------------------------------------------------------------------!
 ! Finds indices for G index in 3-moment ice lookup table
 !------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumzq
 integer, intent(in)  :: zqsize
 real,    intent(out) :: dum8
 real,    intent(in)  :: zitot,qitot

 ! find index for mu_i
 dum8  = (alog10(zitot/qitot)+23.)*3.10347652
 dumzq = int(dum8)
 dum8  = min(dum8,real(zqsize))
 dum8  = max(dum8,1.)
 dumzq = max(1,dumzq)
 dumzq = min(zqsize-1,dumzq)

 end subroutine find_lookupTable_indices_3a


!==========================================================================================!

 real function proc_from_LUT_ir3mom(ind,args_r,args_i)

 !--------------------------------------------------------------------------------
 ! Returns process rate (or other quantity) from LUT by accessing values from the
 ! LUT and performing the necessary interpolation.
 !
 ! This applies for the ice-rain collection LUT for 3-moment.
 ! Table dims: (Z index zz, density jj, rime fraction ii, liquid fraction ll,
 !              size i, rain size j, quantity)
 !--------------------------------------------------------------------------------

 implicit none

!arguments:
 integer, intent(in) :: ind
 real,    dimension(n_args_r), intent(in) :: args_r
 integer, dimension(n_args_i), intent(in) :: args_i
!local:
 real    :: dproc1,dproc2,iproc1,iproc2,iproc3,iproc4,tmp1,tmp2,rproc1,rproc2
 real    :: dum1,dum3,dum4,dum5,dum6,dum7
 integer :: dumzz,dumjj,dumii,dumll,dumj,dumi

 dum1 = args_r(1);  dum3 = args_r(2);  dum4 = args_r(3)
 dum5 = args_r(4);  dum6 = args_r(5);  dum7 = args_r(6)
 dumzz = args_i(1); dumjj = args_i(2); dumii = args_i(3)
 dumll = args_i(4); dumj  = args_i(5); dumi  = args_i(6)

 if (dumll==1 .and. dum7 < 1. + epsilon(dum7)) then  !skip interpolation for liq-frac if qiliq = 0.

    ! 5D: size, rain, rimefrac, density (liqfrac fixed), Z index
    ! at dumzz:
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp1   = lerp(iproc1, iproc2, dum4-real(dumii))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp2   = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc1 = lerp(tmp1, tmp2, dum5-real(dumjj))
    ! at dumzz+1:
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp1   = lerp(iproc1, iproc2, dum4-real(dumii))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    tmp2   = lerp(iproc1, iproc2, dum4-real(dumii))
    rproc2 = lerp(tmp1, tmp2, dum5-real(dumjj))

    proc_from_LUT_ir3mom = lerp(rproc1, rproc2, dum6-real(dumzz))

 else

    ! 6D: size, rain, liqfrac, rimefrac, density, Z index
    ! at dumzz:
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj,  dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp1   = lerp(iproc3, iproc4, dum4-real(dumii))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz,  dumjj+1,dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp2   = lerp(iproc3, iproc4, dum4-real(dumii))
    rproc1 = lerp(tmp1, tmp2, dum5-real(dumjj))
    ! at dumzz+1:
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj,  dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp1   = lerp(iproc3, iproc4, dum4-real(dumii))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii,  dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc3 = lerp(iproc1, iproc2, dum7-real(dumll))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll,  dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc1 = lerp(dproc1, dproc2, dum3-real(dumj))
    dproc1 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi,  dumj,  ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi+1,dumj,  ind), dum1-real(dumi))
    dproc2 = lerp(itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi,  dumj+1,ind), itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumll+1,dumi+1,dumj+1,ind), dum1-real(dumi))
    iproc2 = lerp(dproc1, dproc2, dum3-real(dumj))
    iproc4 = lerp(iproc1, iproc2, dum7-real(dumll))
    tmp2   = lerp(iproc3, iproc4, dum4-real(dumii))
    rproc2 = lerp(tmp1, tmp2, dum5-real(dumjj))

    proc_from_LUT_ir3mom = lerp(rproc1, rproc2, dum6-real(dumzz))

 endif

 end function proc_from_LUT_ir3mom

!==========================================================================================!


 real function polysvp1(T,i_type)

!-------------------------------------------
!  COMPUTE SATURATION VAPOR PRESSURE
!  POLYSVP1 RETURNED IN UNITS OF PA.
!  T IS INPUT IN UNITS OF K.
!  i_type REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)
!-------------------------------------------

      implicit none

      real    :: T
      integer :: i_type

! REPLACE GOFF-GRATCH WITH FASTER FORMULATION FROM FLATAU ET AL. 1992, TABLE 4 (RIGHT-HAND COLUMN)

! ice
      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
        6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/

! liquid
      real a0,a1,a2,a3,a4,a5,a6,a7,a8

! V1.7
      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
        6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt

!-------------------------------------------

      if (i_type.EQ.1 .and. T.lt.trplpt) then
! ICE

! use Goff-Gratch for T < 195.8 K and Flatau et al. equal or above 195.8 K
         if (t.ge.195.8) then
            dt=t-trplpt
            polysvp1 = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt)))))))
            polysvp1 = polysvp1*100.
         else
            polysvp1 = 10.**(-9.09718*(273.16/t-1.)-3.56654* &
                alog10(273.16/t)+0.876793*(1.-t/273.16)+ &
                alog10(6.1071))*100.
         end if

      elseif (i_type.EQ.0 .or. T.ge.trplpt) then
! LIQUID

! use Goff-Gratch for T < 202.0 K and Flatau et al. equal or above 202.0 K
         if (t.ge.202.0) then
            dt = t-trplpt
            polysvp1 = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
            polysvp1 = polysvp1*100.
         else
! note: uncertain below -70 C, but produces physical values (non-negative) unlike flatau
            polysvp1 = 10.**(-7.90298*(373.16/t-1.)+ &
                5.02808*alog10(373.16/t)- &
                1.3816e-7*(10**(11.344*(1.-t/373.16))-1.)+ &
                8.1328e-3*(10**(-3.49149*(373.16/t-1.))-1.)+ &
                alog10(1013.246))*100.
         end if

         endif


 end function polysvp1

!------------------------------------------------------------------------------------------!

 real function DERF(X)

 implicit none

 real :: X
 real, dimension(0 : 64) :: A, B
 real :: W,T,Y
 integer :: K,I
      data A/                                                 &
         0.00000000005958930743E0, -0.00000000113739022964E0, &
         0.00000001466005199839E0, -0.00000016350354461960E0, &
         0.00000164610044809620E0, -0.00001492559551950604E0, &
         0.00012055331122299265E0, -0.00085483269811296660E0, &
         0.00522397762482322257E0, -0.02686617064507733420E0, &
         0.11283791670954881569E0, -0.37612638903183748117E0, &
         1.12837916709551257377E0,                            &
         0.00000000002372510631E0, -0.00000000045493253732E0, &
         0.00000000590362766598E0, -0.00000006642090827576E0, &
         0.00000067595634268133E0, -0.00000621188515924000E0, &
         0.00005103883009709690E0, -0.00037015410692956173E0, &
         0.00233307631218880978E0, -0.01254988477182192210E0, &
         0.05657061146827041994E0, -0.21379664776456006580E0, &
         0.84270079294971486929E0,                            &
         0.00000000000949905026E0, -0.00000000018310229805E0, &
         0.00000000239463074000E0, -0.00000002721444369609E0, &
         0.00000028045522331686E0, -0.00000261830022482897E0, &
         0.00002195455056768781E0, -0.00016358986921372656E0, &
         0.00107052153564110318E0, -0.00608284718113590151E0, &
         0.02986978465246258244E0, -0.13055593046562267625E0, &
         0.67493323603965504676E0,                            &
         0.00000000000382722073E0, -0.00000000007421598602E0, &
         0.00000000097930574080E0, -0.00000001126008898854E0, &
         0.00000011775134830784E0, -0.00000111992758382650E0, &
         0.00000962023443095201E0, -0.00007404402135070773E0, &
         0.00050689993654144881E0, -0.00307553051439272889E0, &
         0.01668977892553165586E0, -0.08548534594781312114E0, &
         0.56909076642393639985E0,                            &
         0.00000000000155296588E0, -0.00000000003032205868E0, &
         0.00000000040424830707E0, -0.00000000471135111493E0, &
         0.00000005011915876293E0, -0.00000048722516178974E0, &
         0.00000430683284629395E0, -0.00003445026145385764E0, &
         0.00024879276133931664E0, -0.00162940941748079288E0, &
         0.00988786373932350462E0, -0.05962426839442303805E0, &
         0.49766113250947636708E0 /
      data (B(I), I = 0, 12) /                                 &
         -0.00000000029734388465E0,  0.00000000269776334046E0, &
         -0.00000000640788827665E0, -0.00000001667820132100E0, &
         -0.00000021854388148686E0,  0.00000266246030457984E0, &
          0.00001612722157047886E0, -0.00025616361025506629E0, &
          0.00015380842432375365E0,  0.00815533022524927908E0, &
         -0.01402283663896319337E0, -0.19746892495383021487E0, &
          0.71511720328842845913E0 /
      data (B(I), I = 13, 25) /                                &
         -0.00000000001951073787E0, -0.00000000032302692214E0, &
          0.00000000522461866919E0,  0.00000000342940918551E0, &
         -0.00000035772874310272E0,  0.00000019999935792654E0, &
          0.00002687044575042908E0, -0.00011843240273775776E0, &
         -0.00080991728956032271E0,  0.00661062970502241174E0, &
          0.00909530922354827295E0, -0.20160072778491013140E0, &
          0.51169696718727644908E0 /
      data (B(I), I = 26, 38) /                                &
         0.00000000003147682272E0, -0.00000000048465972408E0,  &
         0.00000000063675740242E0,  0.00000003377623323271E0,  &
        -0.00000015451139637086E0, -0.00000203340624738438E0,  &
         0.00001947204525295057E0,  0.00002854147231653228E0,  &
        -0.00101565063152200272E0,  0.00271187003520095655E0,  &
         0.02328095035422810727E0, -0.16725021123116877197E0,  &
         0.32490054966649436974E0 /
      data (B(I), I = 39, 51) /                                &
         0.00000000002319363370E0, -0.00000000006303206648E0,  &
        -0.00000000264888267434E0,  0.00000002050708040581E0,  &
         0.00000011371857327578E0, -0.00000211211337219663E0,  &
         0.00000368797328322935E0,  0.00009823686253424796E0,  &
        -0.00065860243990455368E0, -0.00075285814895230877E0,  &
         0.02585434424202960464E0, -0.11637092784486193258E0,  &
         0.18267336775296612024E0 /
      data (B(I), I = 52, 64) /                                &
        -0.00000000000367789363E0,  0.00000000020876046746E0,  &
        -0.00000000193319027226E0, -0.00000000435953392472E0,  &
         0.00000018006992266137E0, -0.00000078441223763969E0,  &
        -0.00000675407647949153E0,  0.00008428418334440096E0,  &
        -0.00017604388937031815E0, -0.00239729611435071610E0,  &
         0.02064129023876022970E0, -0.06905562880005864105E0,  &
         0.09084526782065478489E0 /
      W = ABS(X)
      if (W .LT. 2.2D0) then
          T = W * W
          K = INT(T)
          T = T - K
          K = K * 13
          Y = ((((((((((((A(K) * T + A(K + 1)) * T +              &
              A(K + 2)) * T + A(K + 3)) * T + A(K + 4)) * T +     &
              A(K + 5)) * T + A(K + 6)) * T + A(K + 7)) * T +     &
              A(K + 8)) * T + A(K + 9)) * T + A(K + 10)) * T +    &
              A(K + 11)) * T + A(K + 12)) * W
      elseif (W .LT. 6.9D0) then
          K = INT(W)
          T = W - K
          K = 13 * (K - 2)
          Y = (((((((((((B(K) * T + B(K + 1)) * T +               &
              B(K + 2)) * T + B(K + 3)) * T + B(K + 4)) * T +     &
              B(K + 5)) * T + B(K + 6)) * T + B(K + 7)) * T +     &
              B(K + 8)) * T + B(K + 9)) * T + B(K + 10)) * T +    &
              B(K + 11)) * T + B(K + 12)
          Y = Y * Y
          Y = Y * Y
          Y = Y * Y
          Y = 1 - Y * Y
      else
          Y = 1
      endif
      if (X .LT. 0) Y = -Y
      DERF = Y

 end function DERF

!------------------------------------------------------------------------------------------!
 logical function isnan(arg1)
       real,intent(in) :: arg1
       isnan = (arg1 .ne. arg1)
       return
 end function isnan

!==========================================================================================!
 subroutine icecat_destination(Qi,Di,D_nuc,deltaD_init,iice_dest)

 !--------------------------------------------------------------------------------------!
 ! Returns the ind of the destination ice category into which new ice is nucleated.
 !
 ! New ice will be nucleated into the category in which the existing ice is
 ! closest in size to the ice being nucleated.  The exception is that if the
 ! size difference between the nucleated ice and existing ice exceeds a threshold
 ! value for all categories, then ice is initiated into a new category.
 !
 ! D_nuc        = mean diameter of new particles being added to a category
 ! D(i)         = mean diameter of particles in category i
 ! diff(i)      = |D(i) - D_nuc|
 ! deltaD_init  = threshold size difference to consider a new (empty) category
 ! mindiff      = minimum of all diff(i) (for non-empty categories)
 !
 ! POSSIBLE CASES                      DESTINATION CATEGORY
 !---------------                      --------------------
 ! case 1:  all empty                  category 1
 ! case 2:  all full                   category with smallest diff
 ! case 3:  partly full
 !  case 3a:  mindiff <  diff_thrs     category with smallest diff
 !  case 3b:  mindiff >= diff_thrs     first empty category
 !--------------------------------------------------------------------------------------!

 implicit none

! arguments:
 real, intent(in), dimension(:) :: Qi,Di
 real, intent(in)               :: D_nuc,deltaD_init
 integer, intent(out)           :: iice_dest

! local variables:
 logical                        :: all_full,all_empty
 integer                        :: i_firstEmptyCategory,iice,i_mindiff,n_cat
 real                           :: mindiff,diff
 real, parameter                :: qsmall_loc = 1.e-14

 !--------------------------------------------------------------------------------------!

 n_cat     = size(Qi)
 iice_dest = -99

!-- test:
! iice_dest = 1
! return
!==

 if (sum(Qi(:))<qsmall_loc) then

 !case 1:
    iice_dest = 1
    return

 else

    all_full  = .true.
    all_empty = .false.
    mindiff   = 9.e+9
    i_firstEmptyCategory = 0

    do iice = 1,n_cat
       if (Qi(iice) .ge. qsmall_loc) then
          all_empty = .false.
          diff      = abs(Di(iice)-D_nuc)
          if (diff .lt. mindiff) then
             mindiff   = diff
             i_mindiff = iice
          endif
       else
          all_full = .false.
          if (i_firstEmptyCategory.eq.0) i_firstEmptyCategory = iice
       endif
    enddo

    if (all_full) then
 !case 2:
       iice_dest = i_mindiff
       return
    else
       if (mindiff .lt. deltaD_init) then
 !case 3a:
          iice_dest = i_mindiff
          return
       else
 !case 3b:
          iice_dest = i_firstEmptyCategory
          return
       endif
    endif

 endif

 print*, 'ERROR in s/r icecat_destination -- made it to end'
 global_status = STATUS_ERROR
 return

 end subroutine icecat_destination


!======================================================================================!

 subroutine find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,dum5,dum7,isize,rimsize,   &
                                        liqsize,densize,qitot,nitot,qirim,qiliq,rhop)

!------------------------------------------------------------------------------------------!
! Finds indices in 3D ice (only) lookup table.
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumi,dumjj,dumii,dumll
 real,    intent(out) :: dum1,dum4,dum5,dum7
 integer, intent(in)  :: isize,rimsize,densize,liqsize
 real,    intent(in)  :: qitot,nitot,qirim,rhop,qiliq

!------------------------------------------------------------------------------------------!

           ! find index for qi (normalized ice mass mixing ratio = qitot/nitot)

! we are inverting this equation from the lookup table to solve for i:
! qitot/nitot=261.7**((i+10)*0.1)*1.e-18, for lookup table beta <= 7
!             dum1 = (alog10(qitot/nitot)+18.)/(0.1*alog10(261.7))-10.
! qitot/nitot=800**((i+10)*0.1)*1.e-18, for lookup table beta >= 9
!             dum1 = (alog10(qitot/nitot)+18.)/(0.1*alog10(800.))-10.
             dum1 = (log10(qitot/nitot)+18.)*3.444606 - 10.  !optimized
             dumi = int(dum1)
             ! set limits (to make sure the calculated index doesn't exceed range of lookup table)
             dum1 = min(dum1,real(isize))
             dum1 = max(dum1,1.)
             dumi = max(1,dumi)
             dumi = min(isize-1,dumi)

           ! find index for rime mass fraction
             dum4  = (min(max(qirim,0.),max(qitot-qiliq,0.))/max(qitot-qiliq,tiny(qitot)))*3. + 1.
             dumii = int(dum4)
             ! set limits
             dum4  = min(dum4,real(rimsize))
             dum4  = max(dum4,1.)
             dumii = max(1,dumii)
             dumii = min(rimsize-1,dumii)

           ! find index for liquid mass fraction
             dum7  = (qiliq/qitot)*3. + 1.
             dumll = int(dum7)
             ! set limits
             dum7  = min(dum7,real(liqsize))
             dum7  = max(dum7,1.)
             dumll = max(1,dumll)
             dumll = min(liqsize-1,dumll)

           ! find index for bulk rime density
           ! (account for uneven spacing in lookup table for density)
             if (rhop.le.650.) then
                dum5 = (rhop-50.)*0.005 + 1.
             else
                dum5 =(rhop-650.)*0.004 + 4.
             endif
             dumjj = int(dum5)
             ! set limits
             dum5  = min(dum5,real(densize))
             dum5  = max(dum5,1.)
             dumjj = max(1,dumjj)
             dumjj = min(densize-1,dumjj)

 end subroutine find_lookupTable_indices_1a

!======================================================================================!

 subroutine find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr,nr)

 !------------------------------------------------------------------------------------------!
 ! Finds indices in 3D rain lookup table, for 2-moment and 3-moment ice
 !------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumj
 real,    intent(out) :: dum3
 integer, intent(in)  :: rcollsize
 real,    intent(in)  :: qr,nr

! local variables:
 real                 :: dumlr

!------------------------------------------------------------------------------------------!

           ! find index for scaled mean rain size
           ! if no rain, then just choose dumj = 1 and do not calculate rain-ice collection processes
             if (qr.ge.qsmall .and. nr.gt.0.) then
              ! calculate scaled mean size for consistency with ice lookup table
                dumlr = (qr/(pi*rhow*nr))**thrd
                dum3  = (alog10(1.*dumlr)+5.)*10.70415
                dumj  = int(dum3)
              ! set limits
                dum3  = min(dum3,real_rcollsize)
                dum3  = max(dum3,1.)
                dumj  = max(1,dumj)
                dumj  = min(rcollsize-1,dumj)
             else
                dumj  = 1
                dum3  = 1.
             endif

 end subroutine find_lookupTable_indices_1b

!======================================================================================!

 subroutine find_lookupTable_indices_1c(dumzz,dum6,zsize,mu_i)

 !------------------------------------------------------------------------------------------!
 ! Finds indices for G index in 3-moment ice lookup table
 !------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumzz
 integer, intent(in)  :: zsize
 real,    intent(out) :: dum6
 real,    intent(in)  :: mu_i

!------------------------------------------------------------------------------------------!

  ! find index for mu_i
  !if (qitot.ge.qsmall) then
! we are inverting this equation from the lookup table to solve for i:
! use old formula for now, we are solving zitot/qitot=9^(i)*1.e-23, for beta <= 7 lookup table
!     dum6  = (alog10(zitot/qitot)+23.)/alog10(9.)
! use new formula for beta >= 9 lookup table
! zitot/qitot=2.1^(i)*1.e-23

!    dum6  = (alog10(zitot/qitot)+23.)/alog10(2.1)
!    dum6  = (alog10(zitot/qitot)+23.)*3.10347652     !optimization
! HM replace with mu_i
!    dum6 = mu_i/2.+1. ! invert lookup table indices
     dum6 = mu_i*0.5+1. ! optimized

! for "two-moment", setting a constant mu = 0
!     dum6  = 100.  ! set dum6 to a very large value, corresponding to mu = 0

     dumzz = int(dum6)
     dum6  = min(dum6,real(zsize))
     dum6  = max(dum6,1.)
     dumzz = max(1,dumzz)
     dumzz = min(zsize-1,dumzz)

!  else
!
!     dumzz = 1
!     dum6  = 1.
!
!  endif

 end subroutine find_lookupTable_indices_1c

!======================================================================================!
 subroutine find_lookupTable_indices_2(dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc,      &
                                       dum1,   dum4,    dum5,   dum7, dum1c, dum4c,  dum5c, &
                                       dum7c, iisize, rimsize, densize,                     &
                                       qitot_1, qitot_2, nitot_1, nitot_2,                  &
                                       qirim_1, qirim_2, birim_1, birim_2,qiliq_1,qiliq_2)

!------------------------------------------------------------------------------------------!
! Finds indices in ice-ice interaction lookup table (2)
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc
 real,    intent(out) :: dum1,   dum4,    dum5,   dum1c, dum4c,  dum5c, dum7, dum7c
 integer, intent(in)  :: iisize, rimsize, densize
 real,    intent(in)  :: qitot_1,qitot_2,nitot_1,nitot_2,qirim_1,qirim_2,birim_1,birim_2,qiliq_1,qiliq_2

! local variables:
 real                 :: drhop

!------------------------------------------------------------------------------------------!

                    ! find index in lookup table for collector category

                    ! find index for qi (total ice mass mixing ratio)

!              !-- For LT2-5.0 (Dm_max = 2000.)
!              !   inverting the following (from create_LT2):  q = 261.7**((i+5)*0.2)*1.e-18
!              !   where q = qitot/nitot (normalized)
!                      !dum1 = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(261.7))-5.   !orig
!                       dum1 = (alog10(qitot_1/nitot_1)+18.)*(2.06799)-5.             !optimization

             !-- For LT2-5.1 (Dm_max = 400000.)
             !   inverting this equation from the lookup table to solve for i_Qnorm:
             !   from create_LT2:  q = 800.**(0.2*(i_Qnorm+5))*1.e-18   [where q = qitot/nitot]
                     !dum1 = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(800.)) - 5.   !original
                      dum1 = (alog10(qitot_1/nitot_1)+18.)*1.722303 - 5.             !optimized

                      dumi = int(dum1)
                      dum1 = min(dum1,real(iisize))
                      dum1 = max(dum1,1.)
                      dumi = max(1,dumi)
                      dumi = min(iisize-1,dumi)

   ! note that the code below for finding rime mass fraction and density index is
   ! redundant with code for main ice lookup table and can probably be omitted
   ! for efficiency; for now it is left in

                    ! find index for rime mass fraction
                      dum4  = (min(max(qirim_1,0.),max(qitot_1-qiliq_1,0.))/max(qitot_1-qiliq_1,tiny(qitot_1)))*3. + 1.
                      dumii = int(dum4)
                      dum4  = min(dum4,real(rimsize))
                      dum4  = max(dum4,1.)
                      dumii = max(1,dumii)
                      dumii = min(rimsize-1,dumii)

                    ! find index for liquid mass fraction (collector)
                      dum7  = qiliq_1/qitot_1
                      dum7  = min(dum7,1.)
                      dum7  = max(dum7,0.)

                    ! find index for bulk rime density
                    ! (account for uneven spacing in lookup table for density)
                    ! bulk rime density
                      if (birim_1.ge.bsmall) then
                         drhop = qirim_1/birim_1
                      else
                         drhop = 0.
                      endif

                      if (drhop.le.650.) then
                         dum5 = (drhop-50.)*0.005 + 1.
                      else
                         dum5 =(drhop-650.)*0.004 + 4.
                      endif
                      dumjj = int(dum5)
                      dum5  = min(dum5,real(densize))
                      dum5  = max(dum5,1.)
                      dumjj = max(1,dumjj)
                      dumjj = min(densize-1,dumjj)

                    ! find index in lookup table for collectee category, here 'q' is a scaled q/N
                    ! find index for qi (total ice mass mixing ratio)
!                      !dum1c = (alog10(qitot_2/nitot_2)+18.)/(0.2*alog10(261.7))-5. !orig
!                       dum1c = (alog10(qitot_2/nitot_2)+18.)/(0.483561)-5. !for computational efficiency

             !-- For LT2-5.1 (Dm_max = 400000.)
             !   inverting this equation from the lookup table to solve for i_Qnorm:
             !   from create_LT2:  q = 800.**(0.2*(i_Qnorm+5))*1.e-18   [where q = qitot/nitot]
                     !dum1c = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(800.)) - 5.   !original
                      dum1c = (alog10(qitot_2/nitot_2)+18.)*1.722303 - 5.             !optimized
                      dumic = int(dum1c)
                      dum1c = min(dum1c,real(iisize))
                      dum1c = max(dum1c,1.)
                      dumic = max(1,dumic)
                      dumic = min(iisize-1,dumic)

                    ! find index for rime mass fraction
                      dum4c  = (min(max(qirim_2,0.),max(qitot_2-qiliq_2,0.))/max(qitot_2-qiliq_2,tiny(qitot_2)))*3. + 1.
                      dumiic = int(dum4c)
                      dum4c  = min(dum4c,real(rimsize))
                      dum4c  = max(dum4c,1.)
                      dumiic = max(1,dumiic)
                      dumiic = min(rimsize-1,dumiic)

                    ! find index for liquid mass fraction (collectee)
                      dum7c  = qiliq_2/qitot_2
                      dum7c  = min(dum7c,1.)
                      dum7c  = max(dum7c,0.)

                    ! calculate predicted bulk rime density
                      if (birim_2.ge.1.e-15) then            !*** NOTE:  change to 'bsmall'
                         drhop = qirim_2/birim_2
                      else
                         drhop = 0.
                      endif

                    ! find index for bulk rime density
                    ! (account for uneven spacing in lookup table for density)
                      if (drhop.le.650.) then
                         dum5c = (drhop-50.)*0.005 + 1.
                      else
                         dum5c =(drhop-650.)*0.004 + 4.
                      endif
                      dumjjc = int(dum5c)
                      dum5c  = min(dum5c,real(densize))
                      dum5c  = max(dum5c,1.)
                      dumjjc = max(1,dumjjc)
                      dumjjc = min(densize-1,dumjjc)

 end subroutine find_lookupTable_indices_2


!======================================================================================!
 subroutine find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,i_dum3,mu_r,lamr)

!------------------------------------------------------------------------------------------!
! Finds indices in rain lookup table (3)
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumii,dumjj
 real,    intent(out) :: dum1,rdumii,rdumjj,i_dum3
 real,    intent(in)  :: mu_r,lamr

!------------------------------------------------------------------------------------------!

        ! find location in scaled mean size space
          dum1 = (mu_r+1.)/lamr
          if (dum1.le.195.e-6) then
             i_dum3  = 0.1
             rdumii = (dum1*1.e6+5.)*i_dum3
             rdumii = max(rdumii, 1.)
             rdumii = min(rdumii,20.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 1)
             dumii  = min(dumii,20)
          elseif (dum1.gt.195.e-6) then
             i_dum3  = thrd*0.1            !i.e. 1/30
             rdumii = (dum1*1.e+6-195.)*i_dum3 + 20.
             rdumii = max(rdumii, 20.)
             rdumii = min(rdumii,300.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 20)
             dumii  = min(dumii,299)
          endif

        ! find location in mu_r space
          rdumjj = mu_r+1.
          rdumjj = max(rdumjj,1.)
          rdumjj = min(rdumjj,10.)
          dumjj  = int(rdumjj)
          dumjj  = max(dumjj,1)
          dumjj  = min(dumjj,9)

 end subroutine find_lookupTable_indices_3


!===========================================================================================
 subroutine get_cloud_dsd2(qc_grd,nc_grd,mu_c,rho,nu,dnu,lamc,cdist,cdist1,iSCF)

!Note (BUG) need to be updated because problem when qc<qsmall but qc*iSCF>=qsmall
! This will change the solution

 implicit none

!arguments:
 real, dimension(:), intent(in)  :: dnu
 real,     intent(in)            :: rho
 real,     intent(in)            :: qc_grd
 real,     intent(inout)         :: nc_grd    !grid-mean value
 real,     intent(out)           :: mu_c,nu,lamc,cdist,cdist1
 real,     intent(in)            :: iSCF

!local variables
 real                            :: lammin,lammax,qc,nc
 integer                         :: dumi

!--------------------------------------------------------------------------

       qc = qc_grd*iSCF   !in-cloud value

       if (qc.ge.qsmall) then

          nc = nc_grd*iSCF   !in-cloud value

        ! set minimum nc to prevent floating point error
          nc   = max(nc,nsmall)
          mu_c = 0.0005714*(nc*1.e-6*rho)+0.2714
          mu_c = 1./(mu_c**2)-1.
          mu_c = max(mu_c,2.)
          mu_c = min(mu_c,15.)

        ! interpolate for mass distribution spectral shape parameter (for SB warm processes)
          if (autoAccr_param.eq.1) then
             dumi = int(mu_c)+1
             nu   = dnu(dumi)+(dnu(dumi+1)-dnu(dumi))*(mu_c-dumi)
          endif

        ! calculate lamc
          lamc = (cons1*nc*(mu_c+3.)*(mu_c+2.)*(mu_c+1.)/qc)**thrd

        ! apply lambda limiters
          lammin = (mu_c+1.)*2.5e+4   ! min: 40 micron mean diameter
          lammax = (mu_c+1.)*1.e+6    ! max:  1 micron mean diameter

          if (lamc.lt.lammin) then
             lamc = lammin
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          elseif (lamc.gt.lammax) then
             lamc = lammax
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          endif

          cdist  = nc*(mu_c+1.)/lamc
          cdist1 = nc/gamma(mu_c+1.)
          nc_grd = nc/iSCF   !compute modified grid-mean value

       else

          mu_c   = 0.
          lamc   = 0.
          cdist  = 0.
          cdist1 = 0.
          nu     = 0.

       endif

 end subroutine get_cloud_dsd2

!===========================================================================================
 subroutine get_rain_dsd2(qr_grd,nr_grd,mu_r,lamr,cdistr,logn0r,iSPF)

!Note (BUG) need to be updated because problem when qr<qsmall but qr*iSCF>=qsmall
! This will change the solution

! Computes and returns rain size distribution parameters

 implicit none

!arguments:
 real, intent(in)    :: qr_grd       !grid-mean
 real, intent(inout) :: nr_grd       !grid-mean
 real, intent(out)   :: lamr,mu_r,cdistr,logn0r
 real, intent(in)    :: iSPF

!local variables:
 real                :: inv_dum,lammax,lammin,qr,nr

!--------------------------------------------------------------------------

       qr = qr_grd*iSPF   !in-cloud value

       if (qr.ge.qsmall) then

          nr = nr_grd*iSPF   !in-cloud value

       ! use lookup table to get mu
       ! mu-lambda relationship is from Cao et al. (2008), eq. (7)

       ! find spot in lookup table
       ! (scaled N/q for lookup table parameter space_
          nr      = max(nr,nsmall)
          inv_dum = (qr/(cons1*nr*6.))**thrd

          mu_r = mu_r_constant

!--- apply diagnostic (variable) mu_r:
!          if (inv_dum.lt.282.e-6) then
!             mu_r = 8.282
!          elseif (inv_dum.ge.282.e-6 .and. inv_dum.lt.502.e-6) then
!           ! interpolate
!             rdumii = (inv_dum-250.e-6)*1.e+6*0.5
!             rdumii = max(rdumii,1.)
!             rdumii = min(rdumii,150.)
!             dumii  = int(rdumii)
!             dumii  = min(149,dumii)
!             mu_r   = mu_r_table(dumii)+(mu_r_table(dumii+1)-mu_r_table(dumii))*(rdumii-  &
!                        real(dumii))
!          elseif (inv_dum.ge.502.e-6) then
!             mu_r = 0.
!          endif
!===
          lamr   = (cons1*nr*(mu_r+3.)*(mu_r+2)*(mu_r+1.)/(qr))**thrd  ! recalculate slope based on mu_r

       ! apply lambda limiters for rain
          lammax = (mu_r+1.)*1.e+5
          lammin = (mu_r+1.)*inv_Drmax
          if (lamr.lt.lammin) then
             lamr = lammin
             nr   = 6.*lamr**3*qr/(pi*rhow*(mu_r+3.)*(mu_r+2.)*(mu_r+1.))
          elseif (lamr.gt.lammax) then
             lamr = lammax
             nr   = 6.*lamr**3*qr/(pi*rhow*(mu_r+3.)*(mu_r+2.)*(mu_r+1.))
          endif

          logn0r  = alog10(nr)+(mu_r+1.)*alog10(lamr)-alog10(gamma(mu_r+1)) !note: logn0r is calculated as log10(n0r)
          cdistr  = nr/gamma(mu_r+1.)
          nr_grd  = nr/iSPF  !compute modified grid-mean value (passed back)

       else

          lamr   = 0.
          cdistr = 0.
          logn0r = 0.

       endif

 end subroutine get_rain_dsd2


!===========================================================================================
 subroutine calc_bulkRhoRime(qi_tot,qi_rim,qi_liq,bi_rim,rho_rime)

!--------------------------------------------------------------------------------
!  Calculates and returns the bulk rime density from the prognostic ice variables
!  and adjusts qirim and birim appropriately.
!--------------------------------------------------------------------------------

 implicit none

!arguments:
 real, intent(in)    :: qi_tot,qi_liq
 real, intent(inout) :: qi_rim,bi_rim
 real, intent(out)   :: rho_rime

 !--------------------------------------------------------------------------

 if (bi_rim.ge.1.e-15) then
!if (bi_rim.ge.bsmall) then
    rho_rime = qi_rim/bi_rim
    !impose limits on rho_rime;  adjust bi_rim if needed
    if (rho_rime.lt.rho_rimeMin) then
       rho_rime = rho_rimeMin
       bi_rim   = qi_rim/rho_rime
    elseif (rho_rime.gt.rho_rimeMax) then
       rho_rime = rho_rimeMax
       bi_rim   = qi_rim/rho_rime
    endif
 else
    qi_rim   = 0.
    bi_rim   = 0.
    rho_rime = 0.
 endif

  if (qi_rim.lt.qsmall) then
    qi_rim = 0.
    bi_rim = 0.
 elseif (qi_rim.gt.(qi_tot-qi_liq) .and. rho_rime.gt.0.) then
  !set upper constraint qi_rim <= qi_tot
  !floored at 0 for the anomalous state qi_liq > qi_tot (possible after sedimentation
  !in drained cells); a negative qi_rim here corrupts the prognostic arrays and
  !overflows int(dum4) in find_lookupTable_indices_1a/_2 on FPE-trapping builds
    qi_rim = max(qi_tot-qi_liq, 0.)
    bi_rim = qi_rim/rho_rime
 endif


 end subroutine calc_bulkRhoRime

!===========================================================================================

 subroutine impose_max_Ni(nitot_local,max_Ni,i_rho_local)

!--------------------------------------------------------------------------------
! Impose maximum ice number concentration on each ice category individually.
! Note, with this approach the maximum total concentration (sum of all categories)
! can in principle be nCat*max_Ni.
!--------------------------------------------------------------------------------

 implicit none

!arguments:
 real, intent(inout), dimension(:) :: nitot_local           !note: dimension (nCat)
 real, intent(in)                  :: max_Ni,i_rho_local

!local variables:
 real                              :: dum
 integer                           :: iice

 nitot_local(:) = min(nitot_local(:),max_Ni*i_rho_local)

!---
! Previous apporach:
!    Impose maximum total ice number concentration (total of all ice categories).
!    If the sum of all nitot(:) exceeds maximum allowable, each category to preserve
!    ratio of number between categories.
!
!  if (sum(nitot_local(:)).ge.1.e-20) then
!     dum = max_total_Ni*i_rho_local/sum(nitot_local(:))
!     nitot_local(:) = nitot_local(:)*min(dum,1.)
!  endif
!
! Potential problem:
!    This following approach can decrease the number for a category that already has
!    small number, thereby creating unrealistic mean sizes and reflectivty values.
!---

 end subroutine impose_max_Ni

!===========================================================================================

 real function qv_sat(t_atm,p_atm,ind_wrt)

!------------------------------------------------------------------------------------
! Calls polysvp1 to obtain the saturation vapor pressure, and then computes
! and returns the saturation mixing ratio, with respect to either liquid or ice,
! depending on value of 'ind_wrt'
!------------------------------------------------------------------------------------

 implicit none

 !Calling parameters:
 real    :: t_atm    !temperature [K]
 real    :: p_atm    !pressure    [Pa]
 integer :: ind_wrt  !index, 0 = w.r.t. liquid, 1 = w.r.t. ice

 !Local variables:
 real    :: e_pres         !saturation vapor pressure [Pa]

 !------------------

#ifdef ECCCGEM
  if (ind_wrt.eq.1) e_pres = foew(t_atm)
  if (ind_wrt.eq.0) e_pres = foewa(t_atm)
  qv_sat = ep_2*e_pres/max(1.e-3,(p_atm-e_pres))
#else
  e_pres = polysvp1(t_atm,ind_wrt)
  qv_sat = ep_2*e_pres/max(1.e-3,(p_atm-e_pres))
#endif


 return
 end function qv_sat

!===========================================================================================

 subroutine check_values(Qv,T,Qc,Nc,Qr,Nr,Qitot,Qirim,Nitot,Birim,i,timestepcount,       &
                         force_abort_in,source_ind,Zitot,Qiliq)

!------------------------------------------------------------------------------------
! Checks current values of prognotic variables for reasonable values and
! stops and prints values if they are out of specified allowable ranges.
!
! 'check_consistency' means include trap for inconsistency in moments;
! otherwise, only trap for Q, T, and negative Qx, etc.  This option is here
! to allow for Q<qsmall.and.N>nsmall or Q>qsmall.and.N<small which can be produced
! at the leading edges due to sedimentation and whose values are accpetable
! since lambda limiters are later imposed after SEDI (so one does not necessarily
! want to trap for inconsistency after sedimentation has been called).
!
! The value 'source_ind' indicates the approximate location in 'p3_main'
! from where 'check_values' was called before it resulted in a trap.
!
!------------------------------------------------------------------------------------

  implicit none

 !Calling parameters:
  real, dimension(:),   intent(in) :: Qv,T,Qc,Qr,Nr,Nc
  real, dimension(:,:), intent(in) :: Qitot,Qirim,Nitot,Birim
  real, dimension(:,:), intent(in), optional :: Zitot,Qiliq
  integer,              intent(in) :: source_ind,i,timestepcount
  logical,              intent(in) :: force_abort_in         !.TRUE. = forces abort if value violation is detected

 !logical,              intent(in) :: check_consistency   !.TRUE. = check for sign consistency between Qx and Nx

 !Local variables:
  real, parameter :: T_low  = 173.
  real, parameter :: T_high = 323.
  real, parameter :: Q_high = 60.e-3
  real, parameter :: N_high = 1.e+20
  real, parameter :: B_high = Q_high*5.e-3
! increased by Cholette because in theory, but maybe not desired, Birim can be much higher than B_high
!  real, parameter :: B_high = Q_high*5.e-2
  real, parameter :: Z_high = 10.
  integer         :: k,iice,nk,ncat
  logical         :: badvalue_found

  nk   = size(Qitot,dim=1)
  nCat = size(Qitot,dim=2)

  badvalue_found = .false.

  k_loop: do k = 1,nk

   ! check unrealistic values T and Qv
     if (.not.(T(k)>T_low .and. T(k)<T_high)) then
        write(6,'(a41,4i5,6e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,T,Qiliq,Qirim,Qitot: ',                 &
           source_ind,i,k,timestepcount,T(k),Qiliq(k,1),Qirim(k,1),Qitot(k,1),Qirim(k,1)/max(1.e-10,(Qitot(k,1)-   &
           Qiliq(k,1))),Qiliq(k,1)/max(1.e-10,Qitot(k,1))
        badvalue_found = .true.
     endif
     if (.not.(Qv(k)>=0. .and. Qv(k)<Q_high)) then
        write(6,'(a42,4i5,7e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,Qv,T,Qiliq,Qirim,Qitot: ',                    &
           source_ind,i,k,timestepcount,Qv(k),T(k),Qiliq(k,1),Qirim(k,1),Qitot(k,1),Qirim(k,1)/max(1.e-10,(Qitot(k,1)-   &
           Qiliq(k,1))),Qiliq(k,1)/max(1.e-10,Qitot(k,1))
        badvalue_found = .true.
     endif

   ! check for NANs:
      if (.not.(T(k)  == T(k))  .or.            &
          .not.(Qv(k) == Qv(k)) .or.            &
          .not.(Qc(k) == Qc(k)) .or.            &
          .not.(Nc(k) == Nc(k)) .or.            &
          .not.(Qr(k) == Qr(k)) .or.            &
          .not.(Nr(k) == Nr(k)) ) then
         write(6,'(a56,4i5,6e15.6)') '*A WARNING IN P3_MAIN -- src,i,k,step,T,Qv,Qc,Nc,Qr,Nr: ', &
              source_ind,i,k,timestepcount,T(k),Qv(k),Qc(k),Nc(k),Qr(k),Nr(k)
         badvalue_found = .true.
      endif
      do iice = 1,ncat
         if (.not.(Qitot(k,iice) == Qitot(k,iice)) .or.            &
             .not.(Qirim(k,iice) == Qirim(k,iice)) .or.            &
             .not.(Qiliq(k,iice) == Qiliq(k,iice)) .or.            &
             .not.(Nitot(k,iice) == Nitot(k,iice)) .or.            &
             .not.(Birim(k,iice) == Birim(k,iice)) ) then
            write(6,'(a68,5i5,5e15.6)') '*B WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim,Qiliq: ',          &
                 source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice),Qiliq(k,iice)
            badvalue_found = .true.
         endif
      enddo

   ! check unrealistic values Qc,Nc
     if ( .not.(Qc(k)==0. .and. Nc(k)==0.) .and.                               &  !ignore for all zeroes
           ( ((Qc(k)>0..and.Nc(k)<=0.) .or. (Qc(k)<=0..and.Nc(k)>0.))          &  !inconsistency
            .or. Qc(k)<0. .or. Qc(k)>Q_high                                    &
            .or. Nc(k)<0. .or. Nc(k)>N_high  )                                 &  !unrealistic values
            .and. source_ind /= 100                                            &  !skip trap for this source_ind
            .and. source_ind /= 200                                            &  !skip trap for this source_ind
            .and. source_ind /= 300 ) then                                        !skip trap for this source_ind
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQc,Nc: ', &
           source_ind,i,k,timestepcount,Qc(k),Nc(k)
        badvalue_found = .true.
     endif

   ! check unrealistic values Qr,Nr
     if ( .not.(Qr(k)==0. .and. Nr(k)==0.) .and.                               &  !ignore for all zeroes
           ( ((Qr(k)>0..and.Nr(k)<=0.) .or. (Qr(k)<=0..and.Nr(k)>0.))          &  !inconsistency
            .or. Qr(k)<0. .or. Qr(k)>Q_high                                    &
            .or. Nr(k)<0. .or. Nr(k)>N_high  )                                 &  !unrealistic values
            .and. source_ind /= 100                                            &  !skip trap for this source_ind
            .and. source_ind /= 200                                            &  !skip trap for this source_ind
            .and. source_ind /= 300 ) then                                        !skip trap for this source_ind
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQr,Nr: ', &
           source_ind,i,k,timestepcount,Qr(k),Nr(k)
        badvalue_found = .true.
     endif

   ! check unrealistic values Qitot,Qirim,Nitot,Birim
     do iice = 1,ncat

        if ( .not.(Qitot(k,iice)==0..and.Qirim(k,iice)==0..and.Nitot(k,iice)==0..and.Birim(k,iice)==0.).and.  &  !ignore for all zeroes
             ( ((Qitot(k,iice)>0..and.Nitot(k,iice)<=0.) .or. (Qitot(k,iice)<=0..and.Nitot(k,iice)>0.) )      &  !inconsistency
               .or. Qitot(k,iice)<0. .or. Qitot(k,iice)>Q_high                                                &  !unrealistic values
               .or. Qirim(k,iice)<0. .or. Qirim(k,iice)>Q_high                                                &
               .or. Qiliq(k,iice)<0. .or. Qiliq(k,iice)>Q_high                                                &
               .or. Nitot(k,iice)<0. .or. Nitot(k,iice)>N_high                                                &
               .or. Birim(k,iice)<0. .or. Birim(k,iice)>B_high )                                              &  !skip trap for this source_ind
               .and. source_ind /= 100                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 200                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 300 ) then
           write(6,'(a68,5i5,5e15.6)') '*D WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim,Qiliq: ',        &
              source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice),Qiliq(k,iice)
           badvalue_found = .true.
            print*, '**: ',Qitot(k,iice)>Q_high, Qirim(k,iice)>Q_high, Nitot(k,iice)>N_high,  &
                           Birim(k,iice)>B_high, Qiliq(k,iice)>Q_high, Q_high, N_high, B_high
        endif

        if (present(Zitot) .and. source_ind/=100 .and. source_ind/=200 .and. source_ind/=300 .and. source_ind/=700) then
           if ( .not.(Qitot(k,iice)==0. .and. Nitot(k,iice)==0. .and. Zitot(k,iice)==0.) .and.  &
                .not.(Qitot(k,iice)>0.  .and. Nitot(k,iice)>0.  .and. Zitot(k,iice)>0. )) then
              write(6,'(a62,5i5,3e15.6)') '*E WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Nitot,Zitot: ', &
                 source_ind,i,k,timestepcount,iice,Qitot(k,iice),Nitot(k,iice),Zitot(k,iice)
              badvalue_found = .true.
           endif
        endif

!               (Birim(k,iice))>0. .and. ((Qirim(k,iice))/(Birim(k,iice))).gt.rho_rimeMax) then
        !if (present(Qiliq) .and. source_ind/=100 .and. source_ind/=200 .and. source_ind/=300) then
        !   if ((Qiliq(k,iice))>0. .and. (Qitot(k,iice))>0. .and. (Qiliq(k,iice))>=(Qitot(k,iice)) .or. &
        !       (Qirim(k,iice))>0. .and. (Qitot(k,iice)-Qiliq(k,iice))>0. .and. (Qirim(k,iice))>(Qitot(k,iice)-Qiliq(k,iice))) then
        !      write(6,'(a62,5i5,10e15.6)') '*F WARNING IN P3_MAIN -- src,i,k,step,iice,Filiq,Firim,rhorim, Qi,l,r: ',       &
        !         source_ind,i,k,timestepcount,iice,Qiliq(k,iice)/Qitot(k,iice),Qirim(k,iice)/(Qitot(k,iice)-Qiliq(k,iice)), &
        !                                           Qirim(k,iice)/Birim(k,iice),Qitot(k,iice),Qiliq(k,iice),Qirim(k,iice),   &
        !                                           dble(Qirim(k,iice)/(Qitot(k,iice)-Qiliq(k,iice))), &
        !                                           dble(Qiliq(k,iice)/Qitot(k,iice)),dble(Qitot(k,iice)-Qiliq(k,iice))
        !      badvalue_found = .true.
        !   endif
        !endif

     enddo  !iice-loop

  enddo k_loop

  if (badvalue_found .and. force_abort_in) then
     print*
     print*,'** DEBUG TRAP IN P3_MAIN, s/r CHECK_VALUES -- source: ',source_ind
     print*
     global_status = STATUS_ERROR
     stop
     return
  endif

 end subroutine check_values

!==========================================================================================!
 real function compute_mu_3mom_1(mom0,mom3,mom6,mu_max)

 !--------------------------------------------------------------------------
 ! Computes mu as a function of moments 0, 3, and 6 of the size distribution
 ! represented by N(D) = No*D^mu*e(-lambda*D).
 !
 ! * solution is done using a piecewise polynomial approximation *
 ! (faster and equivalent accuracy to the analytic cubic root for operational mu_i ranges)
 !
 ! note: moment 3 is not equal to the mass mixing ratio (due to variable density)
 !--------------------------------------------------------------------------

 implicit none

! Arguments passed:
 real, intent(in) :: mom0    !0th moment
 real, intent(in) :: mom3    !3th moment  (note, not normalized)
 real, intent(in) :: mom6    !6th moment  (note, not normalized)
 real, intent(in) :: mu_max  !maximum allowable value of mu

! Local variables:
 double precision :: G,g2,x1,x2,x3
 real             :: mu      !shape parameter in gamma distribution
 real, parameter  :: eps_m3 = 1.e-20

 real :: dum,c1,c2,c3,Q,R,aa,bb

 if (mom3>eps_m3) then

    !G = (mom0*mom6)/(mom3**2)
    !To avoid very small values of mom3**2 (not enough),
    !reformulated as: G = (mom0/mom3)*(mom6/mom3)
     x1 = dble(1./mom3)
     x2 = dble(mom0)*x1
     x3 = dble(mom6)*x1
     G  = x2*x3

!Piecewise-polynomial approximation of G(mu) to solve for mu:
     if (G>=20.d0) then
        mu = 0.
     else
        g2 = G*G
        if (G<20.d0  .and. G>=13.31d0) then
           mu = 3.3638e-3*sngl(g2) - 1.7152e-1*sngl(G) + 2.0857e+0
        elseif (G<13.31d0 .and. G>=7.123d0) then
           mu = 1.5900e-2*sngl(g2) - 4.8202e-1*sngl(G) + 4.0108e+0
        elseif (G<7.123d0 .and. G>=4.200d0) then
           mu = 1.0730e-1*sngl(g2) - 1.7481e+0*sngl(G) + 8.4246e+0
        elseif (G<4.200d0 .and. G>=2.946d0) then
           mu = 5.9070e-1*sngl(g2) - 5.7918e+0*sngl(G) + 1.6919e+1
        elseif (G<2.946d0 .and. G>=1.793d0) then
           mu = 4.3966e+0*sngl(g2) - 2.6659e+1*sngl(G) + 4.5477e+1
        elseif (G<1.793d0 .and. G>=1.472d0) then
           mu = 4.7552e+1*sngl(g2) - 1.7958e+2*sngl(G) + 1.8126e+2
        elseif (G<1.472d0) then
           mu = mu_max
        endif
     endif

!...................................................

     mu = min(mu,mu_max)

     compute_mu_3mom_1 = mu

 else

    print*, 'Input parameters out of bounds in function COMPUTE_MU_3MOMENT'
    print*, 'mom0 = ',mom0
    print*, 'mom3 = ',mom3
    print*, 'mom6 = ',mom6
    stop

 endif

 end function compute_mu_3mom_1

!======================================================================================!
 real function G_of_mu(mu)

!arguments:
 real, intent(in) :: mu

 G_of_mu = ((6.+mu)*(5.+mu)*(4.+mu))/((3.+mu)*(2.+mu)*(1.+mu))

 end function G_of_mu

!======================================================================================!
 subroutine get_mui_rhoi(mu_i,rholt3,dum6,dumzz,Qi,Ni,Zi,dum1,dum4,dum5,dum7,dumjj,      &
                        dumii,dumll,dumi,zsize,zqsize)

 !--------------------------------------------------------------------------
 ! Obtains mu_i and rho_i from qitot, nitot, and zitot.
 ! Also returns values of dum6 and dumzz which are later used.
 !
 ! Note, zitot being passed (to Zi) is modified, bounded by limits, in order
 ! to prevent log10 of negative number or overflow of (zitot/qitot) in the
 ! subroutine 'find_lookupTable_indices_3a'.
 !--------------------------------------------------------------------------

!arguments:
 real,    intent(out)   :: mu_i,dum6,rholt3
 integer, intent(out)   :: dumzz
 real,    intent(inout) :: Zi
 real,    intent(in)    :: Qi,Ni,dum1,dum4,dum5,dum7
 integer, intent(in)    :: dumjj,dumii,dumll,dumi,zqsize,zsize

!local:
 integer                :: dumzq
 real                   :: dum8

 Zi = max(zsmall, min(zlarge, Zi))

 ! first find index for LT3 and interpolates in LT3 to get mu_i
 call find_lookupTable_indices_3a(dumzq,dum8,zqsize,Zi,Qi)

 mu_i   = proc_from_LUT_3(LUT3_MUI,dumzq,dumjj,dumii,dumll,dumi,dum1,dum4,dum5,dum7,dum8)
 rholt3 = proc_from_LUT_3(LUT3_RHOI,dumzq,dumjj,dumii,dumll,dumi,dum1,dum4,dum5,dum7,dum8)

 !now find dum6, dumzz from mu_i
 call find_lookupTable_indices_1c(dumzz,dum6,zsize,mu_i)

 end subroutine get_mui_rhoi

!======================================================================================!
 subroutine solve_mui(mu_i,dum6,dumzz,Qi,Ni,Zi,dum1,dum4,dum5,dum7,dumjj,dumii,dumll,dumi)

 !--------------------------------------------------------------------------
 ! Solves for mu_i from qitot, nitot, and zitot.
 ! Also returns values of dum6 and dumzz which are later used.  This avoids
 ! the need to do an additional call to 'access_lookup_table_3mom_LF' in
 ! the main code.
 !
 ! Note, eventually this subroutine will be replaced with a function that
 ! solves mu_i = f(Qi,Ni,Zi) based on lookup table.  At this point, the
 ! call to 'find_lookupTable_indices_1c' will need to be put back into
 ! p3_main since dum6 and dumzz are used is calls to access_lookup_table_3mom_LF
 ! immediately after.  Presently, rhoi could also be passed back to save
 ! some calls to access_lookup_table_3mom_LF to obtain f1pr16 (rhoi), which
 ! have been added to p3_main with the introduction of 'solve_mui'; however,
 ! this may create confusion later, so for now it will be left as is.
 !
 ! - added April 2025
 !--------------------------------------------------------------------------

!arguments:
 real,    intent(in)  :: Qi,Ni,Zi,dum1,dum4,dum5,dum7
 integer, intent(in)  :: dumjj,dumii,dumll,dumi
 real,    intent(out) :: mu_i,dum6
 integer, intent(out) :: dumzz

!local:
 integer              :: ind
 real                 :: mu,mu_old     !shape parameter
 real                 :: rhoi          !bulk ice density
 real                 :: mom3          !estimate of 3rd moment
 real,    parameter   :: tol = 0.25    !tolerance for convergence
 integer, parameter   :: max_iterations = 5
 real,    dimension(n_args_r) :: args_r
 integer, dimension(n_args_i) :: args_i

! ! !--- original, for testing
! !                 mom3 =  6./(200.*pi)*Qi
! !                 do ind=1,5 !niter_mui
! !                    mu_i = compute_mu_3mom_1(Ni,mom3,Zi,mu_i_max)
! !                    call find_lookupTable_indices_1c(dumzz,dum6,zsize,mu_i)
! !                    call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,dumzz,dumjj,dumii,dumll,dumi,0)
! !                    rhoi = proc_from_LUT_main3mom(LUT1_RHOMM,args_r,args_i)
! !                    mom3 =  6./(rhoi*pi)*Qi  !estimate of moment3
! !                 enddo
! ! !-----

 mu_old = 0.5   !initial estimate of mu_i

 do ind = 1,max_iterations
    call find_lookupTable_indices_1c(dumzz,dum6,zsize,mu_old)
    call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,dumzz,dumjj,dumii,dumll,dumi,0)
    rhoi = proc_from_LUT_main3mom(LUT1_RHOMM,args_r,args_i)
    mom3 = 6./(rhoi*pi)*Qi
  ! compute_mu_3mom_1 (piecewise polynomial) is used rather than compute_mu_3mom_2
  ! (analytic cubic root) because it is substantially faster at equivalent accuracy
  ! for the mu_i ranges encountered in practice.  compute_mu_3mom_2 has been removed.
    mu_i = compute_mu_3mom_1(Ni,mom3,Zi,mu_i_max)   ! piecewise polynomial approximation
    if (abs(mu_old-mu_i) < tol) exit
    mu_old = mu_i
 enddo

 mu_i = min(mu_i,mu_i_max)

 end subroutine solve_mui

!======================================================================================!

 subroutine apply_mui_bounds_to_zi(zit,qit,nit,rhoi)

 !---------------------------------------------------------------------------
 ! mu_i is constrained to be within upper and lower bounds by adjusting zitot
 ! if mu_i is outside of the bounds.
 !---------------------------------------------------------------------------

!arguments:
 real, intent(inout) :: zit   !6th moment
 real, intent(in)    :: qit   !total mass
 real, intent(in)    :: nit   !total number (equal to 0th moment)
 real, intent(in)    :: rhoi  !bulk density

!local:
 real, parameter     :: mu_min =  0.
 real, parameter     :: mu_max = 20.
 real                :: mom3  !3rd moment
 real                :: tmp

 mom3 = 6./(pi*rhoi)*qit
 tmp  = mom3**2/nit
 zit = min(zit, G_of_mu(mu_min)*tmp)
 zit = max(zit, G_of_mu(mu_max)*tmp)

 end subroutine apply_mui_bounds_to_zi

!======================================================================================!

 subroutine update_zi_proc2(zit,mom0_tend,qit_tend,mu_i_new,dt)

 !--------------------------------------------------------------------------
 ! Updates zitot for "group 2" processes, where new ice is initiated and
 ! has a prescribed mu_i and density.
 !--------------------------------------------------------------------------

!arguments:
 real, intent(inout) :: zit          !zitot
 real, intent(in)    :: mom0_tend    !tendency for 0th moment
 real, intent(in)    :: qit_tend     !tendency for qitot
 real, intent(in)    :: mu_i_new     !mu_i for the new ice
 real, intent(in)    :: dt           !time step

 !local:
 real                :: mom3_tend    !tendency for 3rd moment
 real, parameter     :: rho_i = 900. !density of new ice

 if (qit_tend.ge.qsmall) then
    mom3_tend = qit_tend*6./(rho_i*pi)
    zit = zit + G_of_mu(mu_i_new)*mom3_tend**2/mom0_tend*dt
 endif

 end subroutine update_zi_proc2

!======================================================================================!

 real function maxHailSize(rho,nit,rhofaci,lam,mu,rhoi,Frime)

 !--------------------------------------------------------------------------
 ! Computes the maximum hail size by estimating the maximum size that is
 ! physically observable (and not just a numerical artifact of the complete
 ! gamma size distribution).
 !
 ! Follows the method described in Milbrandt and Yau (2006a).
 !--------------------------------------------------------------------------

 implicit none

! Arguments:
 real, intent(in) :: rho                 ! air density   [kg m-3]
 real, intent(in) :: nit                 ! total number mixing ratio  [# kg-1]
 real, intent(in) :: rhofaci             ! air density correction factor for ice fall speed
 real, intent(in) :: lam,mu              ! PSD slope and shape parameters
 real, intent(in) :: rhoi                ! density of ice [kg m-3]
 real, intent(in) :: Frime               ! rime fraction

! Local:
 real, parameter  :: dD       = 1.e-3    ! diameter bin width [m]
 real, parameter  :: Dmax_psd = 150.e-3  ! maximum diameter in PSD to compute integral  [m]
 real, parameter  :: Ncrit    = 5.e-4    ! threshold physically observable number concentration [# m-3]
 real, parameter  :: Rcrit    = 1.e-3    ! threshold physically observable number flux          [# m-2 s-1]
 real, parameter  :: ch       = 206.89   ! coefficient in V-D fall speed relation for hail (from MY2006a)
 real, parameter  :: dh       = 0.6384   ! exponent in V-D fall speed relation for hail (from MY2006a)
 double precision :: n0                  ! shape parameter in gamma distribution
 real             :: Di                  ! diameter  [m]
 real             :: N_tot               ! total number concentration  [# m-3]
 real             :: N_tail              ! number conc. from Di to infinity; i.e. trial for Nh*{D*} in MY2006a [# m-3]
 real             :: R_tail              ! number flux of large hail; i.e. trial for Rh*{D*} (corrected from MY2006a [# m-2 s-1]
 real             :: V_h                 ! fall speed of hail of size D     [m s-1]
 integer          :: nd                  ! maximum number of size bins for integral
 integer          :: i                   ! index for integration

!-----------------------------------------------------------------------

 maxHailSize = 0.

 if (nit>0. .and. rhoi>700. .and. Frime>0.7) then
   ! ice is diagnosed as hail if the bulk density and rime fractions are large

    nd  = int(Dmax_psd/dD)
   !note: Use of double-precision for for n0 and integral calculations below are
   !      necessary since intermediate calculations, and n0, can be quite large.
   !n0  = nit*lam**(mu+1.)/gamma(mu+1.)
    n0  = dble(nit)*dble(lam)**dble(mu+1.)/dble(gamma(mu+1.))

   !-- method 1, based on Rh*crit:
    R_tail  = 0.
    do i = nd,1,-1
       Di  = i*dD
       V_h = rhofaci*(ch*Di**Dh)
      !R_tail = R_tail + V_h*n0*Di**mu*exp(-lam*Di)*dD
       R_tail = R_tail + V_h*sngl(n0*dble(Di)**dble(mu)*dble(exp(-lam*Di)))*dD
       if (R_tail>Rcrit) then
          maxHailSize = Di
          exit
       endif
    enddo

! !-- method 2, based on Nh*crit:
!  N_tot = rho*nit
!  N_tail = 0.
!  do i = nd,1,-1
!     Di = i*dD
! !   N_tail = N_tail + n0*Di**mu*exp(-lam*Di)*dD
!     N_tail = N_tail + sngl(n0*dble(Di)**dble(mu)*dble(exp(-lam*Di)))*dD
!     !-- alternative:
!     ! N_tail = N_tail + nit*lam**(mu+1.)/gamma(mu+1.) *Di**mu*exp(-lam*Di)*dD  !formulated in terms of nit
!     ! reorganized to avoid overflows from intermediate calculations e.g. lam**(mu+1)]
!     ! tmp2 = lam**tmp1
!     !        N_tail = N_tail + nit*tmp2/gamma(mu+1.)*tmp2*Di**mu*tmp2*exp(-lam*Di)*dD
!     if (N_tail>Ncrit) then
!        maxHailSize = Di
!        exit
!     endif
!  enddo

 endif

 end function maxHailSize

!===========================================================================================

! subroutine generate_mur_table(mu_r)
! Generate lookup table for rain shape parameter mu_r
! this is very fast so it can be generated at the start of each run
! make a 150x1 1D lookup table, this is done in parameter
! space of a scaled mean size proportional qr/Nr -- initlamr

!if(owr) print*, '   Generating rain lookup-table ...'

!-- for variable mu_r only:
! ! !  do i = 1,150              ! loop over lookup table values
! ! !     initlamr = 1./((real(i)*2.)*1.e-6 + 250.e-6)
! ! !
! ! ! ! iterate to get mu_r
! ! ! ! mu_r-lambda relationship is from Cao et al. (2008), eq. (7)
! ! !
! ! ! ! start with first guess, mu_r = 0
! ! !
! ! !     mu_r = 0.
! ! !
! ! !     do ii=1,50
! ! !        lamr = initlamr*((mu_r+3.)*(mu_r+2.)*(mu_r+1.)/6.)**thrd
! ! !
! ! ! ! new estimate for mu_r based on lambda
! ! ! ! set max lambda in formula for mu_r to 20 mm-1, so Cao et al.
! ! ! ! formula is not extrapolated beyond Cao et al. data range
! ! !        dum  = min(20.,lamr*1.e-3)
! ! !        mu_r = max(0.,-0.0201*dum**2+0.902*dum-1.718)
! ! !
! ! ! ! if lambda is converged within 0.1%, then exit loop
! ! !        if (ii.ge.2 .and. abs((lamold-lamr)/lamr).lt.0.001) exit
! ! !
! ! !        lamold = lamr
! ! !
! ! !     enddo
! ! !
! ! ! ! assign lookup table values
! ! !     mu_r_table(i) = mu_r
! ! !
! ! !  enddo
!==

! end subroutine generate_mur_table

!===========================================================================================

#ifdef ECCCGEM

  ! Define bus requirements
  function p3_phybusinit() result(F_istat)

    use phy_status, only: PHY_OK, PHY_ERROR, physeterror
    use bus_builder, only: bb_request
    use phy_options, only: p3_liqfrac, p3_trplmomi

    implicit none
    integer :: F_istat                          !Function return status
    logical :: buserr

    F_istat = PHY_ERROR
    if (n_iceCat < 0) then
       call physeterror('microphy_p3::p3_phybusinit', &
            'Called mp_phybusinit() before mp_init()')
       return
    endif
    buserr = .false.
    if (bb_request((/ &
         'CLOUD_WATER_MASS ', &
         'CLOUD_WATER_NUM  ', &
         'RAIN_MASS        ', &
         'RAIN_NUM         ', &
         'ICE_MASS_TEND    ', &
         'ICE_EFF_RAD      ', &
         'RATE_PRECIP_TYPES', &
         'PARTICLE_DIAMETER', &
         'CCN_NUM          ', &
         'MPDIAG_2D        ', &
         'MPDIAG_3D        ', &
         'MPVIS            ', &
         'REFLECTIVITY     ', &
         'LIGHTNING        ' &
         /)) /= PHY_OK) buserr = .true.
    if (.not. buserr) then
       if (bb_request('ICE_CAT_1') /= PHY_OK) buserr = .true.
    endif
    if (p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_1_TM') /= PHY_OK) buserr = .true.
    endif
    if (p3_liqfrac .and. .not. buserr) then
       if (bb_request('ICE_CAT_1_LF') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 1 .and. .not. buserr) then
       if (bb_request('ICE_CAT_2') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 1 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_2_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 1 .and. p3_liqfrac .and. .not. buserr) then
       if (bb_request('ICE_CAT_2_LF') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 2 .and. .not. buserr) then
       if (bb_request('ICE_CAT_3') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 2 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_3_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 2 .and. p3_liqfrac .and. .not. buserr) then
       if (bb_request('ICE_CAT_3_LF') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 3 .and. .not. buserr) then
       if (bb_request('ICE_CAT_4') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 3 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_4_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 3 .and. p3_liqfrac .and. .not. buserr) then
       if (bb_request('ICE_CAT_4_LF') /= PHY_OK) buserr = .true.
    endif

    if (buserr) then
       call physeterror('microphy_p3::p3_phybusinit', &
            'Cannot construct bus request list')
       return
    endif
    F_istat = PHY_OK
    return
  end function p3_phybusinit

#include "phymkptr.hf"


!===========================================================================================

  ! Compute total water mass
  function p3_lwc(F_qltot, F_pvars, F_tminus) result(F_istat)
    use phybusidx
    use phymem, only: phyvar
    use phy_status, only: PHY_OK, PHY_ERROR
    implicit none
    real, dimension(:,:), intent(out) :: F_qltot        !Total water mass (kg/kg)
    type(phyvar), pointer, contiguous :: F_pvars(:)     !All phy vars (meta + slab data)
    logical, intent(in), optional :: F_tminus           !Compute fields at time-minus [false]
    integer :: F_istat                                  !Return status
    integer :: ni, nkm1
    real, dimension(:,:), pointer, contiguous :: zqc, zqr
    logical :: my_tminus
    F_istat = PHY_ERROR
    my_tminus = .false.
    if (present(F_tminus)) my_tminus = F_tminus
    ni = size(F_qltot, dim=1); nkm1 = size(F_qltot, dim=2)
    if (my_tminus) then
       MKPTR2Dm1(zqc, qcmoins, F_pvars)
       MKPTR2Dm1(zqr, qrmoins, F_pvars)
    else
       MKPTR2Dm1(zqc, qcplus, F_pvars)
       MKPTR2Dm1(zqr, qrplus, F_pvars)
    endif
    F_qltot(:,:) = zqc(:,:) + zqr(:,:)
    F_istat = PHY_OK
    return
  end function p3_lwc

!===========================================================================================

  ! Compute total ice mass
  function p3_iwc(F_qitot, F_pvars, F_tminus) result(F_istat)
    use phybusidx
    use phymem, only: phyvar
    use phy_status, only: PHY_OK, PHY_ERROR
    implicit none
    real, dimension(:,:), intent(out) :: F_qitot        !Total ice mass (kg/kg)
    type(phyvar), pointer, contiguous :: F_pvars(:)     !All phy vars (meta + slab data)
    logical, intent(in), optional :: F_tminus           !Compute fields at time-minus [false]
    integer :: F_istat                                  !Return status
    integer :: ni, nkm1
    real, dimension(:,:), pointer, contiguous :: zqti1, zqti2, zqti3, zqti4
    logical :: my_tminus
    F_istat = PHY_ERROR
    my_tminus = .false.
    if (present(F_tminus)) my_tminus = F_tminus
    ni = size(F_qitot, dim=1); nkm1 = size(F_qitot, dim=2)
    if (my_tminus) then
       MKPTR2Dm1(zqti1, qti1moins, F_pvars)
       MKPTR2Dm1(zqti2, qti2moins, F_pvars)
       MKPTR2Dm1(zqti3, qti3moins, F_pvars)
       MKPTR2Dm1(zqti4, qti4moins, F_pvars)
    else
       MKPTR2Dm1(zqti1, qti1plus, F_pvars)
       MKPTR2Dm1(zqti2, qti2plus, F_pvars)
       MKPTR2Dm1(zqti3, qti3plus, F_pvars)
       MKPTR2Dm1(zqti4, qti4plus, F_pvars)
    endif
    F_qitot = 0.
    if (associated(zqti1)) F_qitot = F_qitot + zqti1
    if (associated(zqti2)) F_qitot = F_qitot + zqti2
    if (associated(zqti3)) F_qitot = F_qitot + zqti3
    if (associated(zqti4)) F_qitot = F_qitot + zqti4
    F_istat = PHY_OK
    return
  end function p3_iwc


#endif

!======================================================================================!

 subroutine calculate_mu_change(nidum,qidum,zidum,nitend,qitend,zitend,f1pr16,den,dmudt,dt)

   real :: dum3mom,mu_old,mu_new
   real :: ninew,qinew,zinew
   real, intent(in) :: nidum,qidum,zidum,f1pr16,den,dt
   real, intent(inout) :: dmudt
   real :: nitend,qitend,zitend

      dum3mom =  6./(f1pr16*pi)*qidum
      mu_old = compute_mu_3mom_1(nidum,dum3mom,zidum,mu_i_max)

     ! update with process rate
      ninew=nidum+nitend*dt
      qinew=qidum+qitend*dt
      zinew=zidum+zitend*dt

      dum3mom =  6./(den*pi)*qinew
      mu_new = compute_mu_3mom_1(ninew,dum3mom,zinew,mu_i_max)

      dmudt=(mu_new-mu_old)/dt

 end subroutine calculate_mu_change

!======================================================================================!

 subroutine freeze_tiny_liqfrac(qitot,qiliq,qirim,birim,t,th,i_exn,xlf,i_cp)

 !-----------------------------------------------------------------------------------
 ! Freeze tiny amounts of liquid on ice to rime.
 !-----------------------------------------------------------------------------------

 implicit none
!arguments:
 real, intent(in),    dimension(:,:,:) :: qitot
 real, intent(inout), dimension(:,:,:) :: qiliq,qirim,birim
 real, intent(in),    dimension(:,:)   :: t,i_exn,xlf
 real, intent(inout), dimension(:,:)   :: th
 real, intent(in)                      :: i_cp
!local:
 integer                               :: i,k,iice

 do k = 1,size(qitot,dim=2)  !kbot,ktop,kdir
    do i = 1,size(qitot,dim=1)  ! its,ite
       do iice = 1,size(qitot,dim=3)  !1,nCat

          if (qitot(i,k,iice).ge.qsmall) then
             if (t(i,k).lt.trplpt .and. qiliq(i,k,iice)/qitot(i,k,iice).le.liqfracsmall) then
                th(i,k) = th(i,k) + i_exn(i,k)*qiliq(i,k,iice)*xlf(i,k)*i_cp
                birim(i,k,iice) = birim(i,k,iice) + qiliq(i,k,iice)*i_rho_rimeMax
                qirim(i,k,iice) = qirim(i,k,iice) + qiliq(i,k,iice)
                qiliq(i,k,iice) = 0.
             endif
          endif

       enddo
    enddo
 enddo

 end subroutine freeze_tiny_liqfrac

!======================================================================================!

 subroutine find_top(k_qxtop,log_qxpresent,qx,ktop,kbot,kdir)

 !--------------------------------------------------------------------------------
 ! Find highest level with non-tiny qx (top of "cloud" of given hydrometeor); also
 ! returns logical variable as .true. if any non-tiny qx is present in column
 !--------------------------------------------------------------------------------

 ! arguments:
 integer, intent(out) :: k_qxtop
 logical, intent(out) :: log_qxpresent
 integer, intent(in)  :: ktop,kbot,kdir
 real, intent(in), dimension(:) :: qx
 ! local:
 integer :: k

 log_qxpresent = .false.
 k_qxtop       = kbot

 do k = ktop,kbot,-kdir
    if (qx(k).ge.qsmall) then
       log_qxpresent = .true.
       k_qxtop = k
       exit
    endif
 enddo

 end subroutine find_top

!======================================================================================!
 integer function k_bottom(qx,k_qxtop,kbot,kdir)

 !--------------------------------------------------------------------------------------
 ! Find and return lowest level (bottom of "cloud"of given hydrometeor) with non-tiny qx
 !--------------------------------------------------------------------------------------

 ! arguments:
 integer, intent(in) :: k_qxtop,kbot,kdir
 real,    intent(in), dimension(:) :: qx
 ! local:
 integer :: k

 do k = kbot,k_qxtop,kdir
    if (qx(k).ge.qsmall) then
       k_bottom = k
       exit
    endif
 enddo

 end function k_bottom

!======================================================================================!
 subroutine sedimentation_liquid(qx,nx,liq_type,iSxF,prt_liq,rho,i_rho,i_dz,dt,ktop,     &
                                 kbot,kdir,acn,dnu,rhofacr,massflux)

 !----------------------------------------------------------------------
 ! Perform full sedimentation step for mass and number of cloud or rain
 !----------------------------------------------------------------------

 implicit none

!arguments:
 real, intent(inout), dimension(:)         :: qx,nx
 real, intent(inout)                       :: prt_liq
 real, intent(in)                          :: dt
 real, intent(in),  dimension(:)           :: rho,i_rho,i_dz,iSxF
 real, intent(in),  dimension(:), optional :: acn,rhofacr
 real, intent(in),  dimension(:), optional :: dnu
 real, intent(out), dimension(:), optional :: massflux
 integer, intent(in)                       :: liq_type,ktop,kbot,kdir

!local variables:
 real, dimension(size(qx)) :: V_qx,V_nx,flux_qx,flux_nx
 real                      :: dt_left,dt_sub,prt_accum,Co_max,mu_c,dum,lamc,mu_r,lamr,   &
                              rdumii,rdumjj,dum1,dum2,dum3,i_dum3,tmp1
 integer                   :: nk,k,k_qxtop,k_qxbot,k_temp,tmpint1,dumii,dumjj
 logical                   :: log_qxpresent


 call find_top(k_qxtop,log_qxpresent,qx(:)*iSxF(:),ktop,kbot,kdir)

 qx_present: if (log_qxpresent) then

    dt_left       = dt  !time remaining for sedi over full model (mp) time step
    prt_accum     = 0.  !precip rate for individual category
    k_qxbot       = k_bottom(qx(:),ktop,kbot,kdir)
    flux_qx(:)    = 0.  !BUG-003: init whole array; the flux loop below only fills the
    flux_nx(:)    = 0.  !  k_temp..k_qxtop rain subrange, but 'massflux = flux_qx' copies
                        !  the WHOLE array into massflux_r, which VIS2 later reads at all
                        !  levels -- uninitialized stack garbage there caused SIGFPE
                        !  OVERFLOW in the visibility diagnostic. (Subsumes the old
                        !  single-element flux_qx(kbot)=0. that guarded prt_accum only.)

    substep_sedi_r: do while (dt_left.gt.1.e-4)

       Co_max  = 0.
       V_qx(:) = 0.
       V_nx(:) = 0.

      !CLOUD only:
       if (liq_type == 1 .and. present(acn) .and. present(dnu)) then
          do k = k_qxtop,k_qxbot,-kdir

             if (qx(k)*iSxF(k).ge.qsmall) then
                call get_cloud_dsd2(qx(k),nx(k),mu_c,rho(k),dum,dnu,lamc,dum,dum,iSxF(k))
                dum = 1./lamc**bcn
                V_qx(k) = acn(k)*gamma(4.+bcn+mu_c)*dum/(gamma(mu_c+4.))
                V_nx(k) = acn(k)*gamma(1.+bcn+mu_c)*dum/(gamma(mu_c+1.))
             endif
             Co_max = max(Co_max, V_qx(k)*dt_left*i_dz(k))

          enddo
       endif  !liq_type

      !RAIN only:
       if (liq_type == 2 .and. present(rhofacr)) then
          kloop_sedi_r: do k = k_qxtop,k_qxbot,-kdir

             qr_not_small: if (qx(k)*iSxF(k).ge.qsmall) then
                nx(k)  = max(nx(k),nsmall)
                call get_rain_dsd2(qx(k),nx(k),mu_r,lamr,dum,dum,iSxF(k))
                call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,i_dum3,   &
                                                mu_r,lamr)
                !mass-weighted fall speed:
                dum1 = vm_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vm_table(dumii+1,dumjj)-vm_table(dumii,dumjj))         !at mu_r
                dum2 = vm_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vm_table(dumii+1,dumjj+1)-vm_table(dumii,dumjj+1))   !at mu_r+1
                V_qx(k) = dum1 + (rdumjj-real(dumjj))*(dum2-dum1)         !interpolated
                V_qx(k) = V_qx(k)*rhofacr(k)                  !corrected for air density
                 ! number-weighted fall speed:
                dum1 = vn_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vn_table(dumii+1,dumjj)-vn_table(dumii,dumjj))        !at mu_r
                dum2 = vn_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vn_table(dumii+1,dumjj+1)-vn_table(dumii,dumjj+1))    !at mu_r+1
                V_nx(k) = dum1+(rdumjj-real(dumjj))*(dum2-dum1)            !interpolated
                V_nx(k) = V_nx(k)*rhofacr(k)                  !corrected for air density
             endif qr_not_small
             Co_max = max(Co_max, V_qx(k)*dt_left*i_dz(k))

          enddo kloop_sedi_r
       endif  !liq_type

       tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
       dt_sub  = min(dt_left, dt_left/float(tmpint1))
       k_temp  = merge(k_qxbot, k_qxbot-kdir, k_qxbot==kbot)

       !-- calculate fluxes
       do k = k_temp,k_qxtop,kdir
          flux_qx(k)  = V_qx(k)*qx(k)*rho(k)
          flux_nx(k)  = V_nx(k)*nx(k)*rho(k)
       enddo

       if (present(massflux)) massflux = flux_qx  !store mass flux for use in visibility diagnostic (for rain)

       !accumulated precip during time step
       prt_accum = prt_accum + flux_qx(kbot)*dt_sub*merge(1., 0., k_qxbot==kbot)

       !-- update prognostic variables based on flux divergence:

       k = k_qxtop   !for top level only (since flux is 0 above)
       tmp1 = i_dz(k)*dt_sub*i_rho(k)
       qx(k) = qx(k) -flux_qx(k)*tmp1
       nx(k) = nx(k) -flux_nx(k)*tmp1   !BUG-004: number update must use flux_nx, not
                                        !  flux_qx (was a copy-paste typo from the qx line
                                        !  above; the interior-levels loop below already
                                        !  uses flux_nx correctly for nx).

       do k = k_qxtop-kdir,k_temp,-kdir
          tmp1 = i_dz(k)*dt_sub*i_rho(k)
          qx(k) = qx(k) + (flux_qx(k+kdir) - flux_qx(k))*tmp1
          nx(k) = nx(k) + (flux_nx(k+kdir) - flux_nx(k))*tmp1
       enddo

       dt_left = dt_left - dt_sub  !update time remaining for sedimentation
       k_qxbot = merge(k_qxbot, k_qxbot-kdir, k_qxbot==kbot)

    enddo substep_sedi_r

    prt_liq = prt_liq + prt_accum*i_rhow/dt

 else

    if (present(massflux)) massflux(:) = 0.  !BUG-003: guard optional; cloud calls
                                             !  (liq_type==1) do not pass massflux, so
                                             !  the bare assignment referenced an absent
                                             !  optional (UB) whenever a cloud column
                                             !  took this no-precipitation branch.

 endif qx_present

 end subroutine sedimentation_liquid

!======================================================================================!
 subroutine sedimentation_ice(log_3mom,log_liq,qit,qir,qil,nit,bir,zit,prt_sol,          &
                              prt_soli,rho,i_rho,rhofac,i_dz,ktop,kbot,kdir,dt)

 !--------------------------------------------------------------------------
 ! Performs full sedimentation step for all prognostic ice variables.
 ! Since this subroutine is passed from within an i-loop, the i-dimension is
 ! dropped from the ice arrays.
 !
 ! This single routine replaces the former four specialized copies
 ! (sedimentation_ice_TT/TF/FT/FF, one per trplMomIce x liqFrac combination).
 ! The configuration is selected by two logical flags:
 !   log_3mom - 3-moment ice:     advect zitot; Co_max uses the
 !              reflectivity-weighted fall speed (the largest)
 !   log_liq  - liquid fraction:  advect qiliq
 ! Both flags are loop-invariant, so the conditionals inside the k-loops are
 ! perfectly predicted; timing tests show no cost vs. the specialized copies.
 ! The qil and zit arrays are always present (p3_main carries them for all
 ! configurations); they are only read/updated under their respective flags.
 !--------------------------------------------------------------------------

 implicit none

!arguments:
 logical, intent(in)                  :: log_3mom,log_liq
 real, intent(inout), dimension(:,:)  :: qit,qir,qil,nit,bir,zit
 real, intent(inout)                  :: prt_sol
 real, intent(inout), dimension(:)    :: prt_soli
 integer, intent(in)                  :: ktop,kbot,kdir
 real,    intent(in)                  :: dt
 real,    intent(in), dimension(:)    :: rho,i_rho,rhofac,i_dz

!local variables:
 integer :: iice,k,k_qxtop,k_qxbot,dumi,dumjj,dumii,dumll,dumzz,tmpint1,k_temp
 logical :: log_qxpresent,log_qiproceed
 real    :: dt_left,prt_accum,Co_max,rhop,dum1,dum4,dum5,dum7,mu_i,f1pr16,dum6,          &
            f1pr01,f1pr02,f1pr09,f1pr10,f1pr19,tmp1,tmp2,dt_sub,qil_k

 real, dimension(size(qit,dim=1)) :: V_qit,V_nit,V_zit,flux_qit,flux_qir,flux_qil,       &
                                     flux_nit,flux_bir,flux_zit
 real,    dimension(n_args_r)     :: args_r
 integer, dimension(n_args_i)     :: args_i


 iice_loop_sedi_ice:  do iice = 1,size(qit,dim=2)  !i.e. 1,nCat

    call find_top(k_qxtop,log_qxpresent,qit(:,iice),ktop,kbot,kdir)

    qi_present: if (log_qxpresent) then

       dt_left        = dt  !time remaining for sedi over full model (mp) time step
       prt_accum      = 0.  !precip rate for individual category
       k_qxbot        = k_bottom(qit(:,iice),ktop,kbot,kdir)
       flux_qit(kbot) = 0.  !to prevent NaN in calculation of prt_accum below

       substep_sedi: do while (dt_left.gt.1.e-4)

          Co_max   = 0.
          V_qit(:) = 0.
          V_nit(:) = 0.
          if (log_3mom) V_zit(:) = 0.

          kloop_sedi_i4: do k = k_qxtop,k_qxbot,-kdir

            !compute Vq, Vn (get values from lookup table):
          
             log_qiproceed = qit(k,iice).ge.qsmall
             ! Note: Do not also require nit.ge.nsmall; skipping the block leaves V_qit = 0, 
             ! so ice mass with collapsed number stops sedimenting and accumulates, producing
             ! spurious extreme Ze (>100 dBZ). nit is floored at nsmall below before the LUT call.
             ! Also, it is expected that qit>qsmall and nitot<0 coming in is possible, due to the
             ! process rate calculations (which can overdeplete number but not mass).

             if (log_liq) log_qiproceed = log_qiproceed .and. qil(k,iice).lt.qit(k,iice)

             qi_notsmall: if (log_qiproceed) then

                nit(k,iice) = max(nit(k,iice),nsmall) !impose lower limits; prevents log(<0)

                qil_k = 0.
                if (log_liq) qil_k = qil(k,iice)

                call calc_bulkRhoRime(qit(k,iice),qir(k,iice),qil_k,bir(k,iice),rhop)

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dumll,dum1,dum4,       &
                          dum5,dum7,isize,rimsize,liqsize,densize,qit(k,iice),           &
                          nit(k,iice),qir(k,iice),qil_k,rhop)

                threeMom_LUT: if (log_3mom) then

                   call get_mui_rhoi(mu_i,f1pr16,dum6,dumzz,qit(k,iice),                 &
                                     nit(k,iice),zit(k,iice),dum1,dum4,dum5,dum7,        &
                                     dumjj,dumii,dumll,dumi,zsize,zqsize)

                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum6,dum7,0.,0.,0.,    &
                                     dumzz,dumjj,dumii,dumll,dumi,0)

                   f1pr01 = proc_from_LUT_main3mom(LUT1_UNS,args_r,args_i)
                   f1pr02 = proc_from_LUT_main3mom(LUT1_UMS,args_r,args_i)
                   f1pr09 = proc_from_LUT_main3mom(LUT1_QSML,args_r,args_i)
                   f1pr10 = proc_from_LUT_main3mom(LUT1_QLRG,args_r,args_i)
                   f1pr19 = proc_from_LUT_main3mom(LUT1_3M_UZS,args_r,args_i)

                else

                   call args_for_LUT(args_r,args_i,dum1,dum4,dum5,dum7,0.,0.,0.,0.,      &
                                     dumjj,dumii,dumll,dumi,0,0)

                   f1pr01 = proc_from_LUT_main2mom(LUT1_UNS,args_r,args_i)
                   f1pr02 = proc_from_LUT_main2mom(LUT1_UMS,args_r,args_i)
                   f1pr09 = proc_from_LUT_main2mom(LUT1_QSML,args_r,args_i)
                   f1pr10 = proc_from_LUT_main2mom(LUT1_QLRG,args_r,args_i)

                endif threeMom_LUT

              !impose mean ice size bounds (i.e. apply lambda limiters)
                nit(k,iice) = min(nit(k,iice),f1pr09*qit(k,iice))
                nit(k,iice) = max(nit(k,iice),f1pr10*qit(k,iice))

                if (log_3mom) then
                 !impose limiter on zitot; ensures mu_i is in bounds
                   tmp1 = 6./(f1pr16*pi)*qit(k,iice)
                   tmp2 = tmp1**2/nit(k,iice)
                   zit(k,iice) = min(zit(k,iice),G_of_mu( 0.)*tmp2)
                   zit(k,iice) = max(zit(k,iice),G_of_mu(20.)*tmp2)
                   V_zit(k) = f1pr19*rhofac(k)  !reflectivity-weighted fall speed (with air density factor)
                endif

                V_qit(k) = f1pr02*rhofac(k)   !mass-weighted fall speed (with air density factor)
                V_nit(k) = f1pr01*rhofac(k)   !number-weighted fall speed (with air density factor)

             endif qi_notsmall

             if (log_3mom) then
                Co_max = max(Co_max, V_zit(k)*dt_left*i_dz(k))  !note: V_zit is the largest fall speed
             else
                Co_max = max(Co_max, V_qit(k)*dt_left*i_dz(k))
             endif

          enddo kloop_sedi_i4

          tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
          dt_sub  = min(dt_left, dt_left/float(tmpint1))
          k_temp  = merge(k_qxbot, k_qxbot-kdir, k_qxbot==kbot)

          !-- calculate fluxes
          do k = k_temp,k_qxtop,kdir
             flux_qit(k) = V_qit(k)*qit(k,iice)*rho(k)
             flux_nit(k) = V_nit(k)*nit(k,iice)*rho(k)
             flux_qir(k) = V_qit(k)*qir(k,iice)*rho(k)
             flux_bir(k) = V_qit(k)*bir(k,iice)*rho(k)
             if (log_liq)  flux_qil(k) = V_qit(k)*qil(k,iice)*rho(k)
             if (log_3mom) flux_zit(k) = V_zit(k)*zit(k,iice)*rho(k)
!              mflux_i(i,k) = flux_qit(k)  !store mass flux for use in visibility diagnostic)
          enddo

          !accumulated precip during time step
          prt_accum = prt_accum + flux_qit(kbot)*dt_sub*merge(1., 0., k_qxbot==kbot)

          !-- update prognostic variables based on flux divergence:

          !for top level only (since flux is 0 above)
          k = k_qxtop
          tmp1 = i_dz(k)*dt_sub*i_rho(k)
          qit(k,iice) = qit(k,iice) - flux_qit(k)*tmp1
          qir(k,iice) = qir(k,iice) - flux_qir(k)*tmp1
          nit(k,iice) = nit(k,iice) - flux_nit(k)*tmp1
          bir(k,iice) = bir(k,iice) - flux_bir(k)*tmp1
          if (log_liq)  qil(k,iice) = qil(k,iice) - flux_qil(k)*tmp1
          if (log_3mom) zit(k,iice) = zit(k,iice) - flux_zit(k)*tmp1

          do k = k_qxtop-kdir,k_temp,-kdir
             tmp1 = i_dz(k)*dt_sub*i_rho(k)
             qit(k,iice) = qit(k,iice) + (flux_qit(k+kdir) - flux_qit(k))*tmp1
             qir(k,iice) = qir(k,iice) + (flux_qir(k+kdir) - flux_qir(k))*tmp1
             nit(k,iice) = nit(k,iice) + (flux_nit(k+kdir) - flux_nit(k))*tmp1
             bir(k,iice) = bir(k,iice) + (flux_bir(k+kdir) - flux_bir(k))*tmp1
             if (log_liq)  qil(k,iice) = qil(k,iice) + (flux_qil(k+kdir) - flux_qil(k))*tmp1
             if (log_3mom) zit(k,iice) = zit(k,iice) + (flux_zit(k+kdir) - flux_zit(k))*tmp1
          enddo

          dt_left = dt_left - dt_sub  !update time remaining for sedimentation
          k_qxbot = merge(k_qxbot, k_qxbot-kdir, k_qxbot==kbot)

       enddo substep_sedi

       tmp1 = 1./dt
       prt_sol = prt_sol + prt_accum*i_rhow*tmp1
       prt_soli(iice) = prt_soli(iice) + prt_accum*i_rhow*tmp1

       endif qi_present

    enddo iice_loop_sedi_ice

 end subroutine sedimentation_ice
!======================================================================================!

 END MODULE microphy_p3
