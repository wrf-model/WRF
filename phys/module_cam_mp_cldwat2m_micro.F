#define MODAL_AERO
#define WRF_PORT
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
module cldwat2m_micro

!---------------------------------------------------------------------------------
! Purpose:
!   CAM Interface for microphysics
!
! Author: Andrew Gettelman, Hugh Morrison.
! Contributions from: Xiaohong Liu and Steve Ghan
! December 2005-May 2010
! Description in: Morrison and Gettelman, 2008. J. Climate (MG2008)
!                 Gettelman et al., 2010 J. Geophys. Res. - Atmospheres (G2010)         
! for questions contact Hugh Morrison, Andrew Gettelman
! e-mail: morrison@ucar.edu, andrew@ucar.edu
!---------------------------------------------------------------------------------
! modification for sub-columns, HM, (orig 8/11/10)
! This is done using the logical 'sub_column' set to .true. = subcolumns
!---------------------------------------------------------------------------------

  use shr_kind_mod,  only: r8=>shr_kind_r8
#ifndef WRF_PORT
  use spmd_utils,    only: masterproc
  use ppgrid,        only: pcols, pver, pverp
#else
  use module_cam_support, only: masterproc, pcols, pver, pverp
#endif
  use physconst,     only: gravit, rair, tmelt, cpair, rh2o, rhoh2o
  use physconst,     only: latvap, latice
  use wv_saturation,  only: polysvp, epsqs 
#ifndef WRF_PORT
  use cam_history,    only: addfld, add_default, phys_decomp, outfld
  use cam_history_support, only : fillvalue
  use cam_logfile,    only: iulog
  use phys_control,   only: phys_getopts
  use cldwat2m_macro, only: rhmini
#else
  use module_cam_support, only: addfld, add_default, phys_decomp, outfld, &
       fillvalue, iulog
#endif

  implicit none
  private
  save

#ifdef WRF_PORT
  !Macrophysics is not ported yet therefore rhmini is defined here (obtained from Cam5's cldwat2_macro)-Balwinder.Singh@pnnl.gov
  real(r8), public,  parameter :: rhmini       = 0.80_r8       ! Minimum rh for ice cloud fraction > 0.
#endif
  logical, public :: liu_in = .true.   ! True = Liu et al 2007 Ice nucleation 
                                       ! False = cooper fixed ice nucleation (MG2008)

   public :: ini_micro, mmicro_pcond,gamma

!constants remaped
  real(r8), private::  g              !gravity
  real(r8), private::  r              !Dry air Gas constant
  real(r8), private::  rv             !water vapor gas contstant
  real(r8), private::  cpp            !specific heat of dry air
  real(r8), private::  rhow           !density of liquid water
  real(r8), private::  xxlv           ! latent heat of vaporization
  real(r8), private::  xlf            !latent heat of freezing
  real(r8), private::  xxls           !latent heat of sublimation

real(r8), private:: rhosn  ! bulk density snow
real(r8), private:: rhoi   ! bulk density ice

real(r8), private:: ac,bc,as,bs,ai,bi,ar,br  !fall speed parameters 
real(r8), private:: ci,di    !ice mass-diameter relation parameters
real(r8), private:: cs,ds    !snow mass-diameter relation parameters
real(r8), private:: cr,dr    !drop mass-diameter relation parameters
real(r8), private:: f1s,f2s  !ventilation param for snow
real(r8), private:: Eii      !collection efficiency aggregation of ice
real(r8), private:: Ecc      !collection efficiency
real(r8), private:: Ecr      !collection efficiency cloud droplets/rain
real(r8), private:: f1r,f2r  !ventilation param for rain
real(r8), private:: DCS      !autoconversion size threshold
real(r8), private:: qsmall   !min mixing ratio 
real(r8), private:: bimm,aimm !immersion freezing
real(r8), private:: rhosu     !typical 850mn air density
real(r8), private:: mi0       ! new crystal mass
real(r8), private:: rin       ! radius of contact nuclei
real(r8), private:: qcvar     ! 1/relative variance of sub-grid qc
real(r8), private:: pi       ! pi

! Additional constants to help speed up code

real(r8), private:: cons1
real(r8), private:: cons2
real(r8), private:: cons3
real(r8), private:: cons4
real(r8), private:: cons5
real(r8), private:: cons6
real(r8), private:: cons7
real(r8), private:: cons8
real(r8), private:: cons9
real(r8), private:: cons10
real(r8), private:: cons11
real(r8), private:: cons12
real(r8), private:: cons13
real(r8), private:: cons14
real(r8), private:: cons15
real(r8), private:: cons16
real(r8), private:: cons17
real(r8), private:: cons18
real(r8), private:: cons19
real(r8), private:: cons20
real(r8), private:: cons21
real(r8), private:: cons22
real(r8), private:: cons23
real(r8), private:: cons24
real(r8), private:: cons25
real(r8), private:: cons27
real(r8), private:: cons28

real(r8), private:: lammini
real(r8), private:: lammaxi
real(r8), private:: lamminr
real(r8), private:: lammaxr
real(r8), private:: lammins
real(r8), private:: lammaxs

! parameters for snow/rain fraction for convective clouds
real(r8), private:: tmax_fsnow ! max temperature for transition to convective snow
real(r8), private:: tmin_fsnow ! min temperature for transition to convective snow

!needed for findsp
real(r8), private:: tt0       ! Freezing temperature

real(r8), private:: csmin,csmax,minrefl,mindbz

contains

!===============================================================================

subroutine ini_micro

!----------------------------------------------------------------------- 
! 
! Purpose: 
! initialize constants for the morrison microphysics
! called from stratiform.F90
! 
! Author: Andrew Gettelman Dec 2005
! 
!-----------------------------------------------------------------------

   integer k

   integer l,m, iaer
   real(r8) surften       ! surface tension of water w/respect to air (N/m)
   real(r8) arg

   character(len=16) :: eddy_scheme = ' '
   logical           :: history_microphysics     ! output variables for microphysics diagnostics package
#ifndef WRF_PORT
   ! Query the PBL eddy scheme
   call phys_getopts(eddy_scheme_out              = eddy_scheme,           &
                     history_microphysics_out     = history_microphysics   )
#else
   history_microphysics = .FALSE.
#endif
   ! diagnostic precip
   call addfld ('QRAIN   ','kg/kg   ',pver, 'A','Diagnostic grid-mean rain mixing ratio'         ,phys_decomp)	
   call addfld ('QSNOW   ','kg/kg   ',pver, 'A','Diagnostic grid-mean snow mixing ratio'         ,phys_decomp)	
   call addfld ('NRAIN   ','m-3     ',pver, 'A','Diagnostic grid-mean rain number conc'         ,phys_decomp)	
   call addfld ('NSNOW   ','m-3     ',pver, 'A','Diagnostic grid-mean snow number conc'         ,phys_decomp)	

   ! size of precip
   call addfld ('RERCLD   ','m      ',pver, 'A','Diagnostic effective radius of Liquid Cloud and Rain' ,phys_decomp)

   call addfld ('DSNOW   ','m       ',pver, 'A','Diagnostic grid-mean snow diameter'         ,phys_decomp)	

   ! diagnostic radar reflectivity, cloud-averaged 
   call addfld ('REFL  ','DBz  ',pver, 'A','94 GHz radar reflectivity'       ,phys_decomp)
   call addfld ('AREFL  ','DBz  ',pver, 'A','Average 94 GHz radar reflectivity'       ,phys_decomp)
   call addfld ('FREFL  ','fraction  ',pver, 'A','Fractional occurance of radar reflectivity'       ,phys_decomp)
   
   call addfld ('CSRFL  ','DBz  ',pver, 'A','94 GHz radar reflectivity (CloudSat thresholds)'       ,phys_decomp)
   call addfld ('ACSRFL  ','DBz  ',pver, 'A','Average 94 GHz radar reflectivity (CloudSat thresholds)'       ,phys_decomp)
   call addfld ('FCSRFL  ','fraction  ',pver, 'A','Fractional occurance of radar reflectivity (CloudSat thresholds)' &
	,phys_decomp)
 
   call addfld ('AREFLZ ','mm^6/m^3 ',pver, 'A','Average 94 GHz radar reflectivity'       ,phys_decomp)

   ! Aerosol information
    call addfld ('NCAL    ','#/m3   ',pver, 'A','Number Concentation Activated for Liquid',phys_decomp)
    call addfld ('NCAI    ','#/m3   ',pver, 'A','Number Concentation Activated for Ice',phys_decomp)


   ! Average rain and snow mixing ratio (Q), number (N) and diameter (D), with frequency
   call addfld ('AQRAIN   ','kg/kg   ',pver, 'A','Average rain mixing ratio'         ,phys_decomp)	
   call addfld ('AQSNOW   ','kg/kg   ',pver, 'A','Average snow mixing ratio'         ,phys_decomp)	
   call addfld ('ANRAIN   ','m-3     ',pver, 'A','Average rain number conc'         ,phys_decomp)	
   call addfld ('ANSNOW   ','m-3     ',pver, 'A','Average snow number conc'         ,phys_decomp)
   call addfld ('ADRAIN   ','Micron  ',pver, 'A','Average rain effective Diameter'         ,phys_decomp)	
   call addfld ('ADSNOW   ','Micron  ',pver, 'A','Average snow effective Diameter'         ,phys_decomp)
   call addfld ('FREQR  ','fraction  ',pver, 'A','Fractional occurance of rain'       ,phys_decomp)
   call addfld ('FREQS  ','fraction  ',pver, 'A','Fractional occurance of snow'       ,phys_decomp)

   if ( history_microphysics) then
      call add_default ('AQSNOW   ', 1, ' ')
      call add_default ('FREQR    ', 1, ' ')
      call add_default ('FREQS    ', 1, ' ')
      call add_default ('AQRAIN   ', 1, ' ')
      call add_default ('AQSNOW   ', 1, ' ')
      call add_default ('ANRAIN   ', 1, ' ')
      call add_default ('ANSNOW   ', 1, ' ')
  end if

!declarations for morrison codes (transforms variable names)

   g= gravit                  !gravity
   r= rair       	      !Dry air Gas constant: note units(phys_constants are in J/K/kmol)
   rv= rh2o                   !water vapor gas contstant
   cpp = cpair                 !specific heat of dry air
   rhow = rhoh2o              !density of liquid water

! latent heats

   xxlv = latvap         ! latent heat vaporization
   xlf = latice          ! latent heat freezing
   xxls = xxlv + xlf     ! latent heat of sublimation

! parameters for snow/rain fraction for convective clouds

   tmax_fsnow = tmelt
   tmin_fsnow = tmelt-5._r8

! parameters below from Reisner et al. (1998)
! density parameters (kg/m3)

      rhosn = 250._r8    ! bulk density snow  (++ ceh)
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

! ventilation parameters for snow
! hall and prupacher

	f1s = 0.86_r8
	f2s = 0.28_r8

! collection efficiency, aggregation of cloud ice and snow

	Eii = 0.1_r8

! collection efficiency, accretion of cloud water by rain

	Ecr = 1.0_r8

! ventilation constants for rain

	f1r = 0.78_r8
	f2r = 0.32_r8

! autoconversion size threshold for cloud ice to snow (m)

       Dcs = 400.e-6_r8

! smallest mixing ratio considered in microphysics

	qsmall = 1.e-18_r8  

! immersion freezing parameters, bigg 1953

	bimm = 100._r8
	aimm = 0.66_r8

! typical air density at 850 mb

	rhosu = 85000._r8/(rair * tmelt)

! mass of new crystal due to aerosol freezing and growth (kg)

	mi0 = 4._r8/3._r8*pi*rhoi*(10.e-6_r8)*(10.e-6_r8)*(10.e-6_r8)

! radius of contact nuclei aerosol (m)

        rin = 0.1e-6_r8

! 1 / relative variance of sub-grid cloud water distribution
! see morrison and gettelman, 2007, J. Climate for details

	qcvar = 2._r8

! freezing temperature
	tt0=273.15_r8

      pi=4._r8*atan(1.0_r8)

      !Range of cloudsat reflectivities (dBz) for analytic simulator
      csmin= -30._r8
      csmax= 26._r8
      mindbz = -99._r8
      !      minrefl = 10._r8**(mindbz/10._r8)
      minrefl = 1.26e-10_r8

! Define constants to help speed up code (limit calls to gamma function)

	cons1=gamma(1._r8+di)
	cons2=gamma(qcvar+2.47_r8)
	cons3=gamma(qcvar)
	cons4=gamma(1._r8+br)
	cons5=gamma(4._r8+br)
	cons6=gamma(1._r8+ds)
	cons7=gamma(1._r8+bs)     
	cons8=gamma(4._r8+bs)     
	cons9=gamma(qcvar+2._r8)     
	cons10=gamma(qcvar+1._r8)
	cons11=gamma(3._r8+bs)
	cons12=gamma(qcvar+1.15_r8)
	cons13=gamma(5._r8/2._r8+br/2._r8)
	cons14=gamma(5._r8/2._r8+bs/2._r8)
	cons15=gamma(qcvar+bc/3._r8)
	cons16=gamma(1._r8+bi)
	cons17=gamma(4._r8+bi)
	cons18=qcvar**2.47_r8
	cons19=qcvar**2
	cons20=qcvar**1.15_r8
	cons21=qcvar**(bc/3._r8)
	cons22=(4._r8/3._r8*pi*rhow*(25.e-6_r8)**3)	
	cons23=dcs**3
	cons24=dcs**2
	cons25=dcs**bs
	cons27=xxlv**2
	cons28=xxls**2

        lammaxi = 1._r8/10.e-6_r8
        lammini = 1._r8/(2._r8*dcs)
	lammaxr = 1._r8/20.e-6_r8
	lamminr = 1._r8/500.e-6_r8
	lammaxs = 1._r8/10.e-6_r8
	lammins = 1._r8/2000.e-6_r8

   return
 end subroutine ini_micro

!===============================================================================
!microphysics routine for each timestep goes here...

subroutine mmicro_pcond ( sub_column,       &
   lchnk, ncol, deltatin, tn,               &
   qn, qc, qi,                              &
   nc, ni, p, pdel, cldn,                   &
   liqcldf, icecldf,                        &
   cldo,                                    &
   rate1ord_cw2pr_st,                       &   
   naai, npccnin, rndst,nacon,              &
   tlat, qvlat,        &
   qctend, qitend, nctend, nitend, effc,    &
   effc_fn, effi, prect, preci,             &  
   nevapr, evapsnow,      &
   prain, prodsnow, cmeout, deffi, pgamrad, &
   lamcrad,qsout,dsout, &
         rflx,sflx, qrout,reff_rain,reff_snow,  &
   qcsevap,qisevap,qvres,cmeiout, &
   vtrmc,vtrmi,qcsedten,qisedten, &
   prao,prco,mnuccco,mnuccto,msacwio,psacwso,&
   bergso,bergo,melto,homoo,qcreso,prcio,praio,qireso,&
   mnuccro,pracso,meltsdt,frzrdt,mnuccdo    &
#ifdef WRF_PORT
   , nsout, nrout                           &
#endif
                                            )

!Author: Hugh Morrison, Andrew Gettelman, NCAR
! e-mail: morrison@ucar.edu, andrew@ucar.edu

   use wv_saturation, only: vqsatd_water

   ! input arguments
   logical,  intent(in) :: sub_column  ! True = configure for sub-columns  False = use w/o sub-columns (standard)
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   real(r8), intent(in) :: deltatin             ! time step (s)
   real(r8), intent(in) :: tn(pcols,pver)       ! input temperature (K)
   real(r8), intent(in) :: qn(pcols,pver)       ! input h20 vapor mixing ratio (kg/kg)

   ! note: all input cloud variables are grid-averaged
   real(r8), intent(inout) :: qc(pcols,pver)    ! cloud water mixing ratio (kg/kg)
   real(r8), intent(inout) :: qi(pcols,pver)    ! cloud ice mixing ratio (kg/kg)
   real(r8), intent(inout) :: nc(pcols,pver)    ! cloud water number conc (1/kg)
   real(r8), intent(inout) :: ni(pcols,pver)    ! cloud ice number conc (1/kg)
   real(r8), intent(in) :: p(pcols,pver)        ! air pressure (pa)
   real(r8), intent(in) :: pdel(pcols,pver)     ! pressure difference across level (pa)
   real(r8), intent(in) :: cldn(pcols,pver)     ! cloud fraction
   real(r8), intent(in) :: icecldf(pcols,pver)  ! ice cloud fraction   
   real(r8), intent(in) :: liqcldf(pcols,pver)  ! liquid cloud fraction
   real(r8), intent(inout) :: cldo(pcols,pver)  ! old cloud fraction

   real(r8), intent(out) :: rate1ord_cw2pr_st(pcols,pver) ! 1st order rate for direct cw to precip conversion 
                                                          ! used for scavenging
! Inputs for aerosol activation
   real(r8), intent(in) :: naai(pcols,pver)      ! ice nulceation number (from microp_aero_ts) 
   real(r8), intent(in) :: npccnin(pcols,pver)   ! ccn activated number tendency (from microp_aero_ts)
   real(r8), intent(in) :: rndst(pcols,pver,4)   ! radius of 4 dust bins for contact freezing (from microp_aero_ts)
   real(r8), intent(in) :: nacon(pcols,pver,4)   ! number in 4 dust bins for contact freezing  (from microp_aero_ts)

   ! output arguments

   real(r8), intent(out) :: tlat(pcols,pver)    ! latent heating rate       (W/kg)
   real(r8), intent(out) :: qvlat(pcols,pver)   ! microphysical tendency qv (1/s)
   real(r8), intent(out) :: qctend(pcols,pver)  ! microphysical tendency qc (1/s) 
   real(r8), intent(out) :: qitend(pcols,pver)  ! microphysical tendency qi (1/s)
   real(r8), intent(out) :: nctend(pcols,pver)  ! microphysical tendency nc (1/(kg*s))
   real(r8), intent(out) :: nitend(pcols,pver)  ! microphysical tendency ni (1/(kg*s))
   real(r8), intent(out) :: effc(pcols,pver)    ! droplet effective radius (micron)
   real(r8), intent(out) :: effc_fn(pcols,pver) ! droplet effective radius, assuming nc = 1.e8 kg-1
   real(r8), intent(out) :: effi(pcols,pver)    ! cloud ice effective radius (micron)
   real(r8), intent(out) :: prect(pcols)        ! surface precip rate (m/s)
   real(r8), intent(out) :: preci(pcols)        ! cloud ice/snow precip rate (m/s)
   real(r8), intent(out) :: nevapr(pcols,pver)  ! evaporation rate of rain + snow
   real(r8), intent(out) :: evapsnow(pcols,pver)! sublimation rate of snow
   real(r8), intent(out) :: prain(pcols,pver)   ! production of rain + snow
   real(r8), intent(out) :: prodsnow(pcols,pver)! production of snow
   real(r8), intent(out) :: cmeout(pcols,pver)  ! evap/sub of cloud
   real(r8), intent(out) :: deffi(pcols,pver)   ! ice effective diameter for optics (radiation)
   real(r8), intent(out) :: pgamrad(pcols,pver) ! ice gamma parameter for optics (radiation)
   real(r8), intent(out) :: lamcrad(pcols,pver) ! slope of droplet distribution for optics (radiation)
   real(r8), intent(out) :: rflx(pcols,pver+1)  ! grid-box average rain flux (kg m^-2 s^-1)
   real(r8), intent(out) :: sflx(pcols,pver+1)  ! grid-box average snow flux (kg m^-2 s^-1)
   real(r8), intent(out) :: qcsevap(pcols,pver) ! cloud water evaporation due to sedimentation
   real(r8), intent(out) :: qisevap(pcols,pver) ! cloud ice sublimation due to sublimation
   real(r8), intent(out) :: qvres(pcols,pver) ! residual condensation term to ensure RH < 100%
   real(r8), intent(out) :: cmeiout(pcols,pver) ! grid-mean cloud ice sub/dep
   real(r8), intent(out) :: vtrmc(pcols,pver) ! mass-weighted cloud water fallspeed
   real(r8), intent(out) :: vtrmi(pcols,pver) ! mass-weighted cloud ice fallspeed
   real(r8), intent(out) :: qcsedten(pcols,pver) ! qc sedimentation tendency
   real(r8), intent(out) :: qisedten(pcols,pver) ! qi sedimentation tendency
! microphysical process rates for output (mixing ratio tendencies)
   real(r8), intent(out) :: prao(pcols,pver) ! accretion of cloud by rain 
   real(r8), intent(out) :: prco(pcols,pver) ! autoconversion of cloud to rain
   real(r8), intent(out) :: mnuccco(pcols,pver) ! mixing rat tend due to immersion freezing
   real(r8), intent(out) :: mnuccto(pcols,pver) ! mixing ratio tend due to contact freezing
   real(r8), intent(out) :: msacwio(pcols,pver) ! mixing ratio tend due to H-M splintering
   real(r8), intent(out) :: psacwso(pcols,pver) ! collection of cloud water by snow
   real(r8), intent(out) :: bergso(pcols,pver) ! bergeron process on snow
   real(r8), intent(out) :: bergo(pcols,pver) ! bergeron process on cloud ice
   real(r8), intent(out) :: melto(pcols,pver) ! melting of cloud ice
   real(r8), intent(out) :: homoo(pcols,pver) ! homogeneos freezign cloud water
   real(r8), intent(out) :: qcreso(pcols,pver) ! residual cloud condensation due to removal of excess supersat
   real(r8), intent(out) :: prcio(pcols,pver) ! autoconversion of cloud ice to snow
   real(r8), intent(out) :: praio(pcols,pver) ! accretion of cloud ice by snow
   real(r8), intent(out) :: qireso(pcols,pver) ! residual ice deposition due to removal of excess supersat
   real(r8), intent(out) :: mnuccro(pcols,pver) ! mixing ratio tendency due to heterogeneous freezing of rain to snow (1/s)
   real(r8), intent(out) :: pracso (pcols,pver) ! mixing ratio tendency due to accretion of rain by snow (1/s)
   real(r8), intent(out) :: meltsdt(pcols,pver) ! latent heating rate due to melting of snow  (W/kg)
   real(r8), intent(out) :: frzrdt (pcols,pver) ! latent heating rate due to homogeneous freezing of rain (W/kg)
   real(r8), intent(out) :: mnuccdo(pcols,pver) ! mass tendency from ice nucleation

! local workspace
! all units mks unless otherwise stated

! temporary variables for sub-stepping 
	real(r8) :: t1(pcols,pver)
	real(r8) :: q1(pcols,pver)
	real(r8) :: qc1(pcols,pver)
   	real(r8) :: qi1(pcols,pver)
   	real(r8) :: nc1(pcols,pver)
   	real(r8) :: ni1(pcols,pver)
  	real(r8) :: tlat1(pcols,pver)
   	real(r8) :: qvlat1(pcols,pver)
   	real(r8) :: qctend1(pcols,pver)
   	real(r8) :: qitend1(pcols,pver)
   	real(r8) :: nctend1(pcols,pver)
   	real(r8) :: nitend1(pcols,pver)
   	real(r8) :: prect1(pcols)
   	real(r8) :: preci1(pcols)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   	real(r8) :: deltat        ! sub-time step (s)
        real(r8) :: omsm    ! number near unity for round-off issues
        real(r8) :: dto2    ! dt/2 (s)
        real(r8) :: mincld  ! minimum allowed cloud fraction
        real(r8) :: q(pcols,pver) ! water vapor mixing ratio (kg/kg)
        real(r8) :: t(pcols,pver) ! temperature (K)
        real(r8) :: rho(pcols,pver) ! air density (kg m-3)
        real(r8) :: dv(pcols,pver)  ! diffusivity of water vapor in air
        real(r8) :: mu(pcols,pver)  ! viscocity of air
        real(r8) :: sc(pcols,pver)  ! schmidt number
        real(r8) :: kap(pcols,pver) ! thermal conductivity of air
        real(r8) :: rhof(pcols,pver) ! air density correction factor for fallspeed
        real(r8) :: cldmax(pcols,pver) ! precip fraction assuming maximum overlap
        real(r8) :: cldm(pcols,pver)   ! cloud fraction
        real(r8) :: icldm(pcols,pver)   ! ice cloud fraction
        real(r8) :: lcldm(pcols,pver)   ! liq cloud fraction
        real(r8) :: icwc(pcols)    ! in cloud water content (liquid+ice)
        real(r8) :: calpha(pcols)  ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cbeta(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cbetah(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cgamma(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cgamah(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: rcgama(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cmec1(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cmec2(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cmec3(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: cmec4(pcols) ! parameter for cond/evap (Zhang et al. 2003)
        real(r8) :: qtmp ! dummy qv 
        real(r8) :: dum  ! temporary dummy variable

        real(r8) :: cme(pcols,pver)  ! total (liquid+ice) cond/evap rate of cloud

        real(r8) :: cmei(pcols,pver) ! dep/sublimation rate of cloud ice
        real(r8) :: cwml(pcols,pver) ! cloud water mixing ratio
        real(r8) :: cwmi(pcols,pver) ! cloud ice mixing ratio
        real(r8) :: nnuccd(pver)   ! ice nucleation rate from deposition/cond.-freezing
        real(r8) :: mnuccd(pver)   ! mass tendency from ice nucleation
        real(r8) :: qcld              ! total cloud water
        real(r8) :: lcldn(pcols,pver) ! fractional coverage of new liquid cloud
        real(r8) :: lcldo(pcols,pver) ! fractional coverage of old liquid cloud
	real(r8) :: nctend_mixnuc(pcols,pver)
	real(r8) :: arg ! argument of erfc

        ! for calculation of rate1ord_cw2pr_st
        real(r8) :: qcsinksum_rate1ord(pver)   ! sum over iterations of cw to precip sink
        real(r8) :: qcsum_rate1ord(pver)    ! sum over iterations of cloud water       

        real(r8) :: alpha

        real(r8) :: dum1,dum2   !general dummy variables

        real(r8) :: npccn(pver)     ! droplet activation rate
        real(r8) :: qcic(pcols,pver) ! in-cloud cloud liquid mixing ratio
        real(r8) :: qiic(pcols,pver) ! in-cloud cloud ice mixing ratio
        real(r8) :: qniic(pcols,pver) ! in-precip snow mixing ratio
        real(r8) :: qric(pcols,pver) ! in-precip rain mixing ratio
        real(r8) :: ncic(pcols,pver) ! in-cloud droplet number conc
        real(r8) :: niic(pcols,pver) ! in-cloud cloud ice number conc
        real(r8) :: nsic(pcols,pver) ! in-precip snow number conc
        real(r8) :: nric(pcols,pver) ! in-precip rain number conc
        real(r8) :: lami(pver) ! slope of cloud ice size distr
        real(r8) :: n0i(pver) ! intercept of cloud ice size distr
        real(r8) :: lamc(pver) ! slope of cloud liquid size distr
        real(r8) :: n0c(pver) ! intercept of cloud liquid size distr
        real(r8) :: lams(pver) ! slope of snow size distr
        real(r8) :: n0s(pver) ! intercept of snow size distr
        real(r8) :: lamr(pver) ! slope of rain size distr
        real(r8) :: n0r(pver) ! intercept of rain size distr
        real(r8) :: cdist1(pver) ! size distr parameter to calculate droplet freezing
! combined size of precip & cloud drops
        real(r8) :: rercld(pcols,pver) ! effective radius calculation for rain + cloud
        real(r8) :: arcld(pcols,pver) ! averaging control flag
        real(r8) :: Actmp  !area cross section of drops
        real(r8) :: Artmp  !area cross section of rain

        real(r8) :: pgam(pver) ! spectral width parameter of droplet size distr
        real(r8) :: lammax  ! maximum allowed slope of size distr
        real(r8) :: lammin  ! minimum allowed slope of size distr
        real(r8) :: nacnt   ! number conc of contact ice nuclei
        real(r8) :: mnuccc(pver) ! mixing ratio tendency due to freezing of cloud water
        real(r8) :: nnuccc(pver) ! number conc tendency due to freezing of cloud water

        real(r8) :: mnucct(pver) ! mixing ratio tendency due to contact freezing of cloud water
        real(r8) :: nnucct(pver) ! number conc tendency due to contact freezing of cloud water
        real(r8) :: msacwi(pver) ! mixing ratio tendency due to HM ice multiplication
        real(r8) :: nsacwi(pver) ! number conc tendency due to HM ice multiplication

        real(r8) :: prc(pver) ! qc tendency due to autoconversion of cloud droplets
        real(r8) :: nprc(pver) ! number conc tendency due to autoconversion of cloud droplets
        real(r8) :: nprc1(pver) ! qr tendency due to autoconversion of cloud droplets
        real(r8) :: nsagg(pver) ! ns tendency due to self-aggregation of snow
        real(r8) :: dc0  ! mean size droplet size distr
        real(r8) :: ds0  ! mean size snow size distr (area weighted)
        real(r8) :: eci  ! collection efficiency for riming of snow by droplets
        real(r8) :: psacws(pver) ! mixing rat tendency due to collection of droplets by snow
        real(r8) :: npsacws(pver) ! number conc tendency due to collection of droplets by snow
        real(r8) :: uni ! number-weighted cloud ice fallspeed
        real(r8) :: umi ! mass-weighted cloud ice fallspeed
        real(r8) :: uns(pver) ! number-weighted snow fallspeed
        real(r8) :: ums(pver) ! mass-weighted snow fallspeed
        real(r8) :: unr(pver) ! number-weighted rain fallspeed
        real(r8) :: umr(pver) ! mass-weighted rain fallspeed
        real(r8) :: unc ! number-weighted cloud droplet fallspeed
        real(r8) :: umc ! mass-weighted cloud droplet fallspeed
        real(r8) :: pracs(pver) ! mixing rat tendency due to collection of rain	by snow
        real(r8) :: npracs(pver) ! number conc tendency due to collection of rain by snow
        real(r8) :: mnuccr(pver) ! mixing rat tendency due to freezing of rain
        real(r8) :: nnuccr(pver) ! number conc tendency due to freezing of rain
        real(r8) :: pra(pver) ! mixing rat tendnency due to accretion of droplets by rain
        real(r8) :: npra(pver) ! nc tendnency due to accretion of droplets by rain
        real(r8) :: nragg(pver) ! nr tendency due to self-collection of rain
        real(r8) :: prci(pver) ! mixing rat tendency due to autoconversion of cloud ice to snow
        real(r8) :: nprci(pver) ! number conc tendency due to autoconversion of cloud ice to snow
        real(r8) :: prai(pver) ! mixing rat tendency due to accretion of cloud ice by snow
        real(r8) :: nprai(pver) ! number conc tendency due to accretion of cloud ice by snow
        real(r8) :: qvs ! liquid saturation vapor mixing ratio
        real(r8) :: qvi ! ice saturation vapor mixing ratio
        real(r8) :: dqsdt ! change of sat vapor mixing ratio with temperature
        real(r8) :: dqsidt ! change of ice sat vapor mixing ratio with temperature
        real(r8) :: ab ! correction factor for rain evap to account for latent heat
        real(r8) :: qclr ! water vapor mixing ratio in clear air
        real(r8) :: abi ! correction factor for snow sublimation to account for latent heat
        real(r8) :: epss ! 1/ sat relaxation timescale for snow
        real(r8) :: epsr ! 1/ sat relaxation timescale for rain
        real(r8) :: pre(pver) ! rain mixing rat tendency due to evaporation
        real(r8) :: prds(pver) ! snow mixing rat tendency due to sublimation
        real(r8) :: qce ! dummy qc for conservation check
        real(r8) :: qie ! dummy qi for conservation check
        real(r8) :: nce ! dummy nc for conservation check
        real(r8) :: nie ! dummy ni for conservation check
        real(r8) :: ratio ! parameter for conservation check
        real(r8) :: dumc(pcols,pver) ! dummy in-cloud qc
        real(r8) :: dumnc(pcols,pver) ! dummy in-cloud nc
        real(r8) :: dumi(pcols,pver) ! dummy in-cloud qi
        real(r8) :: dumni(pcols,pver) ! dummy in-cloud ni
        real(r8) :: dums(pcols,pver) ! dummy in-cloud snow mixing rat
        real(r8) :: dumns(pcols,pver) ! dummy in-cloud snow number conc
        real(r8) :: dumr(pcols,pver) ! dummy in-cloud rain mixing rat
        real(r8) :: dumnr(pcols,pver) ! dummy in-cloud rain number conc
! below are parameters for cloud water and cloud ice sedimentation calculations
        real(r8) :: fr(pver)
        real(r8) :: fnr(pver)
        real(r8) :: fc(pver)
        real(r8) :: fnc(pver)
        real(r8) :: fi(pver)
        real(r8) :: fni(pver)
        real(r8) :: fs(pver)
        real(r8) :: fns(pver)
        real(r8) :: faloutr(pver)
        real(r8) :: faloutnr(pver)
        real(r8) :: faloutc(pver)
        real(r8) :: faloutnc(pver)
        real(r8) :: falouti(pver)
        real(r8) :: faloutni(pver)
        real(r8) :: falouts(pver)
        real(r8) :: faloutns(pver)
        real(r8) :: faltndr
        real(r8) :: faltndnr
        real(r8) :: faltndc
        real(r8) :: faltndnc
        real(r8) :: faltndi
        real(r8) :: faltndni
        real(r8) :: faltnds
        real(r8) :: faltndns
        real(r8) :: faltndqie
        real(r8) :: faltndqce
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        real(r8) :: relhum(pcols,pver) ! relative humidity
        real(r8) :: csigma(pcols) ! parameter for cond/evap of cloud water/ice
        real(r8) :: rgvm ! max fallspeed for all species
        real(r8) :: arn(pcols,pver) ! air density corrected rain fallspeed parameter
        real(r8) :: asn(pcols,pver) ! air density corrected snow fallspeed parameter
        real(r8) :: acn(pcols,pver) ! air density corrected cloud droplet fallspeed parameter
        real(r8) :: ain(pcols,pver) ! air density corrected cloud ice fallspeed parameter
        real(r8) :: nsubi(pver) ! evaporation of cloud ice number
        real(r8) :: nsubc(pver) ! evaporation of droplet number
        real(r8) :: nsubs(pver) ! evaporation of snow number
        real(r8) :: nsubr(pver) ! evaporation of rain number
	real(r8) :: mtime ! factor to account for droplet activation timescale
	real(r8) :: dz(pcols,pver) ! height difference across model vertical level

!fice variable
	real(r8) :: nfice(pcols,pver)

	!! add precip flux variables for sub-stepping
	real(r8) :: rflx1(pcols,pver+1)
        real(r8) :: sflx1(pcols,pver+1)

! returns from function/subroutine calls
        real(r8) :: tsp(pcols,pver)      ! saturation temp (K)
        real(r8) :: qsp(pcols,pver)      ! saturation mixing ratio (kg/kg)
        real(r8) :: qsphy(pcols,pver)      ! saturation mixing ratio (kg/kg): hybrid rh
        real(r8) :: qs(pcols)            ! liquid-ice weighted sat mixing rat (kg/kg)
        real(r8) :: es(pcols)            ! liquid-ice weighted sat vapor press (pa)
        real(r8) :: esl(pcols,pver)      ! liquid sat vapor pressure (pa)
        real(r8) :: esi(pcols,pver)      ! ice sat vapor pressure (pa)
        real(r8) :: gammas(pcols)        ! parameter for cond/evap of cloud water

! sum of source/sink terms for diagnostic precip

        real(r8) :: qnitend(pcols,pver) ! snow mixing ratio source/sink term
        real(r8) :: nstend(pcols,pver)  ! snow number concentration source/sink term
        real(r8) :: qrtend(pcols,pver) ! rain mixing ratio source/sink term
        real(r8) :: nrtend(pcols,pver)  ! rain number concentration source/sink term
	real(r8) :: qrtot ! vertically-integrated rain mixing rat source/sink term
	real(r8) :: nrtot ! vertically-integrated rain number conc source/sink term
	real(r8) :: qstot ! vertically-integrated snow mixing rat source/sink term
	real(r8) :: nstot ! vertically-integrated snow number conc source/sink term

! new terms for Bergeron process

	real(r8) :: dumnnuc ! provisional ice nucleation rate (for calculating bergeron)
	real(r8) :: ninew  ! provisional cloud ice number conc (for calculating bergeron)
	real(r8) :: qinew ! provisional cloud ice mixing ratio (for calculating bergeron)
	real(r8) :: qvl  ! liquid sat mixing ratio   
	real(r8) :: epsi ! 1/ sat relaxation timecale for cloud ice
	real(r8) :: prd ! provisional deposition rate of cloud ice at water sat 
	real(r8) :: berg(pcols,pver) ! mixing rat tendency due to bergeron process for cloud ice
	real(r8) :: bergs(pver) ! mixing rat tendency due to bergeron process for snow

!bergeron terms
        real(r8) :: bergtsf   !bergeron timescale to remove all liquid
        real(r8) :: rhin      !modified RH for vapor deposition

! diagnostic rain/snow for output to history
! values are in-precip (local) !!!!

        real(r8), intent(out) :: qrout(pcols,pver)     ! grid-box average rain mixing ratio (kg/kg)
	real(r8), intent(out) :: reff_rain(pcols,pver) ! rain effective radius (micron)
	real(r8), intent(out) :: reff_snow(pcols,pver) ! snow effective radius (micron)
	real(r8) 	      :: drout(pcols,pver)     ! rain diameter (m)
#ifndef WRF_PORT
	real(r8) :: nrout(pcols,pver) ! rain number concentration (1/m3)
	real(r8) :: nsout(pcols,pver) ! snow number concentration (1/m3)
#else
        !WRF needs nrout and nsout to be the ouput variables
        real(r8), intent(out) :: nrout(pcols,pver) ! rain number concentration (1/m3)
	real(r8), intent(out) :: nsout(pcols,pver) ! snow number concentration (1/m3)
#endif
	real(r8), intent(out) :: dsout(pcols,pver) ! snow diameter (m)
	real(r8), intent(out) :: qsout(pcols,pver) ! snow mixing ratio (kg/kg)

!averageed rain/snow for history
	real(r8) :: qrout2(pcols,pver)
	real(r8) :: qsout2(pcols,pver)
	real(r8) :: nrout2(pcols,pver)
	real(r8) :: nsout2(pcols,pver)
	real(r8) :: freqs(pcols,pver)
	real(r8) :: freqr(pcols,pver)
	real(r8) :: dumfice
	real(r8) :: drout2(pcols,pver) ! mean rain particle diameter (m)
	real(r8) :: dsout2(pcols,pver) ! mean snow particle diameter (m)

!ice nucleation, droplet activation
        real(r8) :: dum2i(pcols,pver) ! number conc of ice nuclei available (1/kg)
        real(r8) :: dum2l(pcols,pver) ! number conc of CCN (1/kg)
	real(r8) :: ncmax
	real(r8) :: nimax

!output fields for number conc
        real(r8) :: ncai(pcols,pver) ! output number conc of ice nuclei available (1/m3)
        real(r8) :: ncal(pcols,pver) ! output number conc of CCN (1/m3)

! loop array variables
         integer i,k,nstep,n, l
	 integer ii,kk, m

! loop variables for sub-step solution
	 integer iter,it,ltrue(pcols)

! used in contact freezing via dust particles
        real(r8)  tcnt, viscosity, mfp
        real(r8)  slip1, slip2, slip3, slip4
!        real(r8)  dfaer1, dfaer2, dfaer3, dfaer4
!        real(r8)  nacon1,nacon2,nacon3,nacon4
        real(r8)  ndfaer1, ndfaer2, ndfaer3, ndfaer4
        real(r8)  nslip1, nslip2, nslip3, nslip4

! used in ice effective radius
        real(r8)  bbi, cci, ak, iciwc, rvi

! used in Bergeron processe and water vapor deposition
        real(r8)  Tk, deles, Aprpr, Bprpr, Cice, qi0, Crate, qidep

! mean cloud fraction over the time step
        real(r8)  cldmw(pcols,pver)

! used in secondary ice production
        real(r8) ni_secp

! variabels to check for RH after rain evap

	real(r8) :: esn
	real(r8) :: qsn
	real(r8) :: ttmp


        real(r8) :: refl(pcols,pver)    ! analytic radar reflectivity        
        real(r8) :: rainrt(pcols,pver)  ! rain rate for reflectivity calculation
        real(r8) :: rainrt1(pcols,pver)
 	real(r8) :: csrfl(pcols,pver)	!cloudsat reflectivity 
        real(r8) :: arefl(pcols,pver)  !average reflectivity will zero points outside valid range
  	real(r8) :: acsrfl(pcols,pver) !cloudsat average
 	real(r8) :: frefl(pcols,pver)
  	real(r8) :: fcsrfl(pcols,pver)
 	real(r8) :: areflz(pcols,pver)  !average reflectivity in z.
	real(r8) :: tmp

        real(r8) dmc,ssmc,dstrn  ! variables for modal scheme.
  
      real(r8), parameter :: cdnl    = 0.e6_r8    ! cloud droplet number limiter

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! initialize  output fields for number conc qand ice nucleation
    ncai(1:ncol,1:pver)=0._r8 
    ncal(1:ncol,1:pver)=0._r8  

!Initialize rain size
    rercld(1:ncol,1:pver)=0._r8
    arcld(1:ncol,1:pver)=0._r8

!initialize radiation output variables
        pgamrad(1:ncol,1:pver)=0._r8 ! liquid gamma parameter for optics (radiation)
        lamcrad(1:ncol,1:pver)=0._r8 ! slope of droplet distribution for optics (radiation)
        deffi  (1:ncol,1:pver)=0._r8 ! slope of droplet distribution for optics (radiation)
!initialize radiation output variables
!initialize water vapor tendency term output
        qcsevap(1:ncol,1:pver)=0._r8 
        qisevap(1:ncol,1:pver)=0._r8 
        qvres  (1:ncol,1:pver)=0._r8 
        cmeiout (1:ncol,1:pver)=0._r8
        vtrmc (1:ncol,1:pver)=0._r8
        vtrmi (1:ncol,1:pver)=0._r8
        qcsedten (1:ncol,1:pver)=0._r8
        qisedten (1:ncol,1:pver)=0._r8    

        prao(1:ncol,1:pver)=0._r8 
        prco(1:ncol,1:pver)=0._r8 
        mnuccco(1:ncol,1:pver)=0._r8 
        mnuccto(1:ncol,1:pver)=0._r8 
        msacwio(1:ncol,1:pver)=0._r8 
        psacwso(1:ncol,1:pver)=0._r8 
        bergso(1:ncol,1:pver)=0._r8 
        bergo(1:ncol,1:pver)=0._r8 
        melto(1:ncol,1:pver)=0._r8 
        homoo(1:ncol,1:pver)=0._r8 
        qcreso(1:ncol,1:pver)=0._r8 
        prcio(1:ncol,1:pver)=0._r8 
        praio(1:ncol,1:pver)=0._r8 
        qireso(1:ncol,1:pver)=0._r8 
        mnuccro(1:ncol,1:pver)=0._r8 
        pracso (1:ncol,1:pver)=0._r8 
        meltsdt(1:ncol,1:pver)=0._r8
        frzrdt (1:ncol,1:pver)=0._r8
        mnuccdo(1:ncol,1:pver)=0._r8

! assign variable deltat for sub-stepping...
	deltat=deltatin	

! parameters for scheme

        omsm=0.99999_r8
        dto2=0.5_r8*deltat
	mincld=0.0001_r8
 
! initialize multi-level fields
        q(1:ncol,1:pver)=qn(1:ncol,1:pver)
        t(1:ncol,1:pver)=tn(1:ncol,1:pver)
	
! initialize time-varying parameters

        do k=1,pver
           do i=1,ncol
        rho(i,k)=p(i,k)/(r*t(i,k))
	dv(i,k) = 8.794E-5_r8*t(i,k)**1.81_r8/p(i,k)
	mu(i,k) = 1.496E-6_r8*t(i,k)**1.5_r8/ &
             (t(i,k)+120._r8)	
	sc(i,k) = mu(i,k)/(rho(i,k)*dv(i,k))
	kap(i,k) = 1.414e3_r8*1.496e-6_r8*t(i,k)** &
            1.5_r8/(t(i,k)+120._r8) 

! air density adjustment for fallspeed parameters
! includes air density correction factor to the
! power of 0.54 following Heymsfield and Bansemer 2007

	rhof(i,k)=(rhosu/rho(i,k))**0.54

        arn(i,k)=ar*rhof(i,k)
        asn(i,k)=as*rhof(i,k)
        acn(i,k)=ac*rhof(i,k)
        ain(i,k)=ai*rhof(i,k)

! get dz from dp and hydrostatic approx
! keep dz positive (define as layer k-1 - layer k)

        dz(i,k)= pdel(i,k)/(rho(i,k)*g)

        end do
        end do

! initialization
        t1(1:ncol,1:pver) = t(1:ncol,1:pver)
	q1(1:ncol,1:pver) = q(1:ncol,1:pver)
	qc1(1:ncol,1:pver) = qc(1:ncol,1:pver)
	qi1(1:ncol,1:pver) = qi(1:ncol,1:pver)
	nc1(1:ncol,1:pver) = nc(1:ncol,1:pver)
	ni1(1:ncol,1:pver) = ni(1:ncol,1:pver)

! initialize tendencies to zero
        tlat1(1:ncol,1:pver)=0._r8
	qvlat1(1:ncol,1:pver)=0._r8
	qctend1(1:ncol,1:pver)=0._r8
	qitend1(1:ncol,1:pver)=0._r8
	nctend1(1:ncol,1:pver)=0._r8
	nitend1(1:ncol,1:pver)=0._r8

! initialize precip output
	qrout(1:ncol,1:pver)=0._r8
	qsout(1:ncol,1:pver)=0._r8
	nrout(1:ncol,1:pver)=0._r8
	nsout(1:ncol,1:pver)=0._r8
        dsout(1:ncol,1:pver)=0._r8

        drout(1:ncol,1:pver)=0._r8
        !! initialize as fillvalue to avoid Floating Exceptions
	reff_rain(1:ncol,1:pver)=fillvalue
        reff_snow(1:ncol,1:pver)=fillvalue

! initialize variables for trop_mozart
	nevapr(1:ncol,1:pver) = 0._r8
	evapsnow(1:ncol,1:pver) = 0._r8
	prain(1:ncol,1:pver) = 0._r8
	prodsnow(1:ncol,1:pver) = 0._r8
	cmeout(1:ncol,1:pver) = 0._r8

! for refl calc
        rainrt1(1:ncol,1:pver) = 0._r8

! initialize precip fraction and output tendencies
        cldmax(1:ncol,1:pver)=mincld

!initialize aerosol number
!        naer2(1:ncol,1:pver,:)=0._r8
        dum2l(1:ncol,1:pver)=0._r8
        dum2i(1:ncol,1:pver)=0._r8

! initialize avg precip rate
	prect1(1:ncol)=0._r8
	preci1(1:ncol)=0._r8

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!Get humidity and saturation vapor pressures

        do k=1,pver

! find wet bulk temperature and saturation value for provisional t and q without
! condensation

        call vqsatd_water(t(1,k),p(1,k),es,qs,gammas,ncol) ! use rhw

        do i=1,ncol

	esl(i,k)=polysvp(t(i,k),0)
	esi(i,k)=polysvp(t(i,k),1)

! hm fix, make sure when above freezing that esi=esl, not active yet
	if (t(i,k).gt.tmelt)esi(i,k)=esl(i,k)

        relhum(i,k)=q(i,k)/qs(i)

! get cloud fraction, check for minimum

           cldm(i,k)=max(cldn(i,k),mincld)
           cldmw(i,k)=max(cldn(i,k),mincld)

           icldm(i,k)=max(icecldf(i,k),mincld)
           lcldm(i,k)=max(liqcldf(i,k),mincld)

! subcolumns, set cloud fraction variables to one
! if cloud water or ice is present, if not present
! set to mincld (mincld used instead of zero, to prevent
! possible division by zero errors

           if (sub_column) then

              cldm(i,k)=mincld
              cldmw(i,k)=mincld
              icldm(i,k)=mincld
              lcldm(i,k)=mincld
              
              if (qc(i,k).ge.qsmall) then
                 lcldm(i,k)=1.           
                 cldm(i,k)=1.
                 cldmw(i,k)=1.
              end if
              
              if (qi(i,k).ge.qsmall) then             
                 cldm(i,k)=1.
                 icldm(i,k)=1.
              end if
              
           end if               ! sub-columns

! calculate nfice based on liquid and ice mmr (no rain and snow mmr available yet)

        nfice(i,k)=0._r8
        dumfice=qc(i,k)+qi(i,k)
        if (dumfice.gt.qsmall .and. qi(i,k).gt.qsmall) then
           nfice(i,k)=qi(i,k)/dumfice
        endif

	if (t(i,k).lt.tmelt - 5._r8) then
           if (liu_in) then 

! if aerosols interact with ice set number of activated ice nuclei
              dum2=naai(i,k)

           else
! cooper curve (factor of 1000 is to convert from L-1 to m-3)
              dum2=0.005_r8*exp(0.304_r8*(273.15_r8-t(i,k)))*1000._r8
! put limit on number of nucleated crystals, set to number at T=-30 C
! cooper (limit to value at -35 C)
              dum2=min(dum,208.9e3_r8)/rho(i,k) ! convert from m-3 to kg-1
           endif

           dumnnuc=(dum2-ni(i,k)/icldm(i,k))/deltat*icldm(i,k)
           dumnnuc=max(dumnnuc,0._r8)
! get provisional ni and qi after nucleation in order to calculate
! Bergeron process below
           ninew=ni(i,k)+dumnnuc*deltat
           qinew=qi(i,k)+dumnnuc*deltat*mi0
!T>268
        else
           ninew=ni(i,k)
           qinew=qi(i,k)
        end if

! Initialize CME components

  cme(i,k) = 0._r8
  cmei(i,k)=0._r8


!-------------------------------------------------------------------
!Bergeron process

! make sure to initialize bergeron process to zero
       berg(i,k)=0._r8
       prd = 0._r8

!condensation loop.

! get in-cloud qi and ni after nucleation
       if (icldm(i,k) .gt. 0._r8) then 
          qiic(i,k)=qinew/icldm(i,k)
          niic(i,k)=ninew/icldm(i,k)
       else
          qiic(i,k)=0._r8
          niic(i,k)=0._r8
       endif

!if T < 0 C then bergeron.
           if (t(i,k).lt.273.15) then
!if ice exists
              if (qi(i,k).gt.qsmall) then

                 bergtsf = 0._r8 ! bergeron time scale (fraction of timestep)

                    qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
                    qvl=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))

                    dqsidt =  xxls*qvi/(rv*t(i,k)**2)
                    abi = 1._r8+dqsidt*xxls/cpp

! get ice size distribution parameters

                    if (qiic(i,k).ge.qsmall) then
                       lami(k) = (cons1*ci* &
                       niic(i,k)/qiic(i,k))**(1._r8/di)
                       n0i(k) = niic(i,k)*lami(k)

! check for slope
! adjust vars
                       if (lami(k).lt.lammini) then

                          lami(k) = lammini
                          n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
                       else if (lami(k).gt.lammaxi) then
                          lami(k) = lammaxi
                          n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
                       end if
                    
                       epsi = 2._r8*pi*n0i(k)*rho(i,k)*Dv(i,k) &
                       /(lami(k)*lami(k))

!if liquid exists  
                 if (qc(i,k).gt. qsmall) then 

!begin bergeron process
!     do bergeron (vapor deposition with RHw=1)
!     code to find berg (a rate) goes here

! calculate Bergeron process

                       prd = epsi*(qvl-qvi)/abi
                    
                    else
                       prd = 0._r8
                    end if

! multiply by cloud fraction

                    prd = prd*min(icldm(i,k),lcldm(i,k))

!     transfer of existing cloud liquid to ice

                    berg(i,k)=max(0._r8,prd)

                 end if  !end liquid exists bergeron

                 if (berg(i,k).gt.0._r8) then
                    bergtsf=max(0._r8,(qc(i,k)/berg(i,k))/deltat) 

                    if(bergtsf.lt.1._r8) berg(i,k) = max(0._r8,qc(i,k)/deltat)

                 endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                 if (bergtsf.lt.1._r8.or.icldm(i,k).gt.lcldm(i,k)) then

			if (qiic(i,k).ge.qsmall) then

! first case is for case when liquid water is present, but is completely depleted in time step, i.e., bergrsf > 0 but < 1

		if (qc(i,k).ge.qsmall) then
                       rhin  = (1.0_r8 + relhum(i,k)) / 2._r8
                       if ((rhin*esl(i,k)/esi(i,k)) > 1._r8) then
                          prd = epsi*(rhin*qvl-qvi)/abi

! multiply by cloud fraction assuming liquid/ice maximum overlap
                          prd = prd*min(icldm(i,k),lcldm(i,k))

! add to cmei
                          cmei(i,k) = cmei(i,k) + (prd * (1._r8- bergtsf))

	                end if ! rhin 
	        end if ! qc > qsmall

! second case is for pure ice cloud, either no liquid, or icldm > lcldm

		if (qc(i,k).lt.qsmall.or.icldm(i,k).gt.lcldm(i,k)) then

! note: for case of no liquid, need to set liquid cloud fraction to zero
! store liquid cloud fraction in 'dum'

			if (qc(i,k).lt.qsmall) then 
			dum=0._r8 
			else
			dum=lcldm(i,k)
			end if

! set RH to grid-mean value for pure ice cloud
                       rhin = relhum(i,k)

                if ((rhin*esl(i,k)/esi(i,k)) > 1._r8) then

                       prd = epsi*(rhin*qvl-qvi)/abi

! multiply by relevant cloud fraction for pure ice cloud
! assuming maximum overlap of liquid/ice
                       prd = prd*max((icldm(i,k)-dum),0._r8)
                       cmei(i,k) = cmei(i,k) + prd

		end if ! rhin
		end if ! qc or icldm > lcldm
	end if ! qiic
	end if ! bergtsf or icldm > lcldm

!     if deposition, it should not reduce grid mean rhi below 1.0
                 if(cmei(i,k) > 0.0_r8 .and. (relhum(i,k)*esl(i,k)/esi(i,k)) > 1._r8 ) &
                  cmei(i,k)=min(cmei(i,k),(q(i,k)-qs(i)*esi(i,k)/esl(i,k))/abi/deltat)

              end if            !end ice exists loop
   !this ends temperature < 0. loop

!-------------------------------------------------------------------
           end if  ! 
!..............................................................

! evaporation should not exceed available water

           if ((-berg(i,k)).lt.-qc(i,k)/deltat) &
             berg(i,k) = max(qc(i,k)/deltat,0._r8)

!sublimation process...

            if ((relhum(i,k)*esl(i,k)/esi(i,k)).lt.1._r8 .and. qiic(i,k).ge.qsmall ) then

               qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
               qvl=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
               dqsidt =  xxls*qvi/(rv*t(i,k)**2)
               abi = 1._r8+dqsidt*xxls/cpp

! get ice size distribution parameters

               lami(k) = (cons1*ci* &
               niic(i,k)/qiic(i,k))**(1._r8/di)
               n0i(k) = niic(i,k)*lami(k)

! check for slope
! adjust vars
               if (lami(k).lt.lammini) then
                  
                  lami(k) = lammini
                  n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
               else if (lami(k).gt.lammaxi) then
                  lami(k) = lammaxi
                  n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
               end if
                    
               epsi = 2._r8*pi*n0i(k)*rho(i,k)*Dv(i,k) &
               /(lami(k)*lami(k))

! modify for ice fraction below
               prd = epsi*(relhum(i,k)*qvl-qvi)/abi * icldm(i,k)
               cmei(i,k)=min(prd,0._r8)

            endif

! sublimation should not exceed available ice
           if (cmei(i,k).lt.-qi(i,k)/deltat) &
              cmei(i,k)=-qi(i,k)/deltat

! sublimation should not increase grid mean rhi above 1.0 
           if(cmei(i,k) < 0.0_r8 .and. (relhum(i,k)*esl(i,k)/esi(i,k)) < 1._r8 ) &
              cmei(i,k)=min(0._r8,max(cmei(i,k),(q(i,k)-qs(i)*esi(i,k)/esl(i,k))/abi/deltat))

! limit cmei due for roundoff error

           cmei(i,k)=cmei(i,k)*omsm

! conditional for ice nucleation 

        if (t(i,k).lt.(tmelt - 5._r8)) then 
          if ( liu_in ) then 

! using Liu et al. (2007) ice nucleation with hooks into simulated aerosol
! ice nucleation rate (dum2) has already been calculated and read in (naai)

             dum2i(i,k)=naai(i,k)
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

	end do ! i loop
	end do ! k loop

 
     cldo(:ncol,:)=cldn(:ncol,:)

!! initialize sub-step precip flux variables
	do i=1,ncol
	   !! flux is zero at top interface, so these should stay as 0.
           rflx1(i,1)=0._r8
           sflx1(i,1)=0._r8
	do k=1,pver

	   ! initialize normal and sub-step precip flux variables
           rflx1(i,k+1)=0._r8
           sflx1(i,k+1)=0._r8
	end do ! i loop
	end do ! k loop
!! initialize final precip flux variables.
	do i=1,ncol
	   !! flux is zero at top interface, so these should stay as 0.
           rflx(i,1)=0._r8
           sflx(i,1)=0._r8
	do k=1,pver
	   ! initialize normal and sub-step precip flux variables
           rflx(i,k+1)=0._r8
           sflx(i,k+1)=0._r8
	end do ! i loop
	end do ! k loop	

	do i=1,ncol
	ltrue(i)=0
	do k=1,pver
! skip microphysical calculations if no cloud water

	if (qc(i,k).ge.qsmall.or.qi(i,k).ge.qsmall.or.cmei(i,k).ge.qsmall) ltrue(i)=1
	end do
	end do

! assign number of sub-steps to iter
! use 2 sub-steps, following tests described in MG2008
	iter = 2

! get sub-step time step
	deltat=deltat/real(iter)

! since activation/nucleation processes are fast, need to take into account
! factor mtime = mixing timescale in cloud / model time step
! mixing time can be interpreted as cloud depth divided by sub-grid vertical velocity
! for now mixing timescale is assumed to be 1 timestep for modal aerosols, 20 min bulk

!        note: mtime for bulk aerosols was set to: mtime=deltat/1200._r8

        mtime=1._r8
        rate1ord_cw2pr_st(:,:)=0._r8 ! rce 2010/05/01

!!!! skip calculations if no cloud water
	do i=1,ncol
	if (ltrue(i).eq.0) then
           tlat(i,1:pver)=0._r8
           qvlat(i,1:pver)=0._r8
           qctend(i,1:pver)=0._r8
           qitend(i,1:pver)=0._r8
           qnitend(i,1:pver)=0._r8
           qrtend(i,1:pver)=0._r8
           nctend(i,1:pver)=0._r8
           nitend(i,1:pver)=0._r8
           nrtend(i,1:pver)=0._r8
           nstend(i,1:pver)=0._r8
           prect(i)=0._r8
           preci(i)=0._r8
           qniic(i,1:pver)=0._r8
           qric(i,1:pver)=0._r8
           nsic(i,1:pver)=0._r8
           nric(i,1:pver)=0._r8
           rainrt(i,1:pver)=0._r8
	goto 300
	end if

        qcsinksum_rate1ord(1:pver)=0._r8 
        qcsum_rate1ord(1:pver)=0._r8 


!!!!!!!!! begin sub-step!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!.....................................................................................................
	do it=1,iter

! initialize sub-step microphysical tendencies

	tlat(i,1:pver)=0._r8
	qvlat(i,1:pver)=0._r8
	qctend(i,1:pver)=0._r8
	qitend(i,1:pver)=0._r8
	qnitend(i,1:pver)=0._r8
	qrtend(i,1:pver)=0._r8
	nctend(i,1:pver)=0._r8
	nitend(i,1:pver)=0._r8
	nrtend(i,1:pver)=0._r8
	nstend(i,1:pver)=0._r8

! initialize diagnostic precipitation to zero

        qniic(i,1:pver)=0._r8
        qric(i,1:pver)=0._r8
        nsic(i,1:pver)=0._r8
        nric(i,1:pver)=0._r8
 
        rainrt(i,1:pver)=0._r8


! begin new i,k loop, calculate new cldmax after adjustment to cldm above

! initialize vertically-integrated rain and snow tendencies

	   qrtot = 0._r8
	   nrtot = 0._r8
	   qstot = 0._r8
	   nstot = 0._r8

! initialize precip at surface

	   prect(i)=0._r8
	   preci(i)=0._r8

	   do k=1,pver

! set cwml and cwmi to current qc and qi

	   cwml(i,k)=qc(i,k)
	   cwmi(i,k)=qi(i,k)

! initialize precip fallspeeds to zero

	   ums(k)=0._r8 
	   uns(k)=0._r8 
	   umr(k)=0._r8 
	   unr(k)=0._r8

! calculate precip fraction based on maximum overlap assumption

! for sub-columns cldm has already been set to 1 if cloud
! water or ice is present, so cldmax will be correctly set below
! and nothing extra needs to be done here

           if (k.eq.1) then
		cldmax(i,k)=cldm(i,k)
	   else
! if rain or snow mix ratio is smaller than
! threshold, then set cldmax to cloud fraction at current level
	   if (qric(i,k-1).ge.qsmall.or.qniic(i,k-1).ge.qsmall) then
                cldmax(i,k)=max(cldmax(i,k-1),cldm(i,k))
	   else
	   cldmax(i,k)=cldm(i,k)
	   end if
	   end if

! decrease in number concentration due to sublimation/evap
! divide by cloud fraction to get in-cloud decrease
! don't reduce Nc due to bergeron process

           if (cmei(i,k) < 0._r8 .and. qi(i,k) > qsmall .and. cldm(i,k) > mincld) then
              nsubi(k)=cmei(i,k)/qi(i,k)*ni(i,k)/cldm(i,k)
	   else
              nsubi(k)=0._r8
           end if
           nsubc(k)=0._r8


!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! ice nucleation if activated nuclei exist at t<-5C AND rhmini + 5%

        if (dum2i(i,k).gt.0._r8.and.t(i,k).lt.(tmelt - 5._r8).and. &
	   relhum(i,k)*esl(i,k)/esi(i,k).gt. rhmini+0.05_r8) then

!if NCAI > 0. then set numice = ncai (as before)
!note: this is gridbox averaged

              nnuccd(k)=(dum2i(i,k)-ni(i,k)/icldm(i,k))/deltat*icldm(i,k)
              nnuccd(k)=max(nnuccd(k),0._r8)
              nimax = dum2i(i,k)*icldm(i,k)

!Calc mass of new particles using new crystal mass...
!also this will be multiplied by mtime as nnuccd is...

              mnuccd(k) = nnuccd(k) * mi0

!  add mnuccd to cmei....
              cmei(i,k)= cmei(i,k) + mnuccd(k) * mtime

!  limit cmei

               qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
               dqsidt =  xxls*qvi/(rv*t(i,k)**2)
               abi = 1._r8+dqsidt*xxls/cpp
              cmei(i,k)=min(cmei(i,k),(q(i,k)-qvi)/abi/deltat)

! limit for roundoff error
              cmei(i,k)=cmei(i,k)*omsm

           else
              nnuccd(k)=0._r8
	      nimax = 0._r8
              mnuccd(k) = 0._r8
           end if

!c............................................................................
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! obtain in-cloud values of cloud water/ice mixing ratios and number concentrations
! for microphysical process calculations
! units are kg/kg for mixing ratio, 1/kg for number conc

! limit in-cloud values to 0.005 kg/kg

           qcic(i,k)=min(cwml(i,k)/lcldm(i,k),5.e-3_r8)
           qiic(i,k)=min(cwmi(i,k)/icldm(i,k),5.e-3_r8)
           ncic(i,k)=max(nc(i,k)/lcldm(i,k),0._r8)
           niic(i,k)=max(ni(i,k)/icldm(i,k),0._r8)

           if (qc(i,k) - berg(i,k)*deltat.lt.qsmall) then
              qcic(i,k)=0._r8
              ncic(i,k)=0._r8
              if (qc(i,k)-berg(i,k)*deltat.lt.0._r8) then
                 berg(i,k)=qc(i,k)/deltat*omsm
              end if
	   end if

	   if (qi(i,k)+(cmei(i,k)+berg(i,k))*deltat.lt.qsmall) then
	   qiic(i,k)=0._r8
	   niic(i,k)=0._r8
	   if (qi(i,k)+(cmei(i,k)+berg(i,k))*deltat.lt.0._r8) then
	   cmei(i,k)=(-qi(i,k)/deltat-berg(i,k))*omsm
	   end if
	   end if

! add to cme output

           cmeout(i,k) = cmeout(i,k)+cmei(i,k)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! droplet activation
! calculate potential for droplet activation if cloud water is present
! formulation from Abdul-Razzak and Ghan (2000) and Abdul-Razzak et al. (1998), AR98
! number tendency (npccnin) is read in from companion routine

! assume aerosols already activated are equal to number of existing droplets for simplicity
! multiply by cloud fraction to obtain grid-average tendency

           if (qcic(i,k).ge.qsmall) then   
              npccn(k) = max(0._r8,npccnin(i,k))  
	      dum2l(i,k)=(nc(i,k)+npccn(k)*deltat)/lcldm(i,k)
              dum2l(i,k)=max(dum2l(i,k),cdnl/rho(i,k)) ! sghan minimum in #/cm3  
	      ncmax = dum2l(i,k)*lcldm(i,k)
           else
              npccn(k)=0._r8
              dum2l(i,k)=0._r8
              ncmax = 0._r8
           end if 

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! get size distribution parameters based on in-cloud cloud water/ice 
! these calculations also ensure consistency between number and mixing ratio
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!......................................................................
! cloud ice

	if (qiic(i,k).ge.qsmall) then

! add upper limit to in-cloud number concentration to prevent numerical error
	niic(i,k)=min(niic(i,k),qiic(i,k)*1.e20_r8)

	lami(k) = (cons1*ci* &
              niic(i,k)/qiic(i,k))**(1._r8/di)
	n0i(k) = niic(i,k)*lami(k)

! check for slope
! adjust vars

	if (lami(k).lt.lammini) then

	lami(k) = lammini
	n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)
	else if (lami(k).gt.lammaxi) then
	lami(k) = lammaxi
	n0i(k) = lami(k)**(di+1._r8)*qiic(i,k)/(ci*cons1)
        niic(i,k) = n0i(k)/lami(k)
	end if

	else
	lami(k) = 0._r8
	n0i(k) = 0._r8
	end if

	if (qcic(i,k).ge.qsmall) then

! add upper limit to in-cloud number concentration to prevent numerical error
	ncic(i,k)=min(ncic(i,k),qcic(i,k)*1.e20_r8)

        ncic(i,k)=max(ncic(i,k),cdnl/rho(i,k)) ! sghan minimum in #/cm  

! get pgam from fit to observations of martin et al. 1994

        pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
        pgam(k)=1._r8/(pgam(k)**2)-1._r8
        pgam(k)=max(pgam(k),2._r8)
        pgam(k)=min(pgam(k),15._r8)

! calculate lamc

	lamc(k) = (pi/6._r8*rhow*ncic(i,k)*gamma(pgam(k)+4._r8)/ &
                 (qcic(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)

! lammin, 50 micron diameter max mean size

	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8

	if (lamc(k).lt.lammin) then
	lamc(k) = lammin
	ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                gamma(pgam(k)+1._r8)/ &
               (pi*rhow*gamma(pgam(k)+4._r8))
	else if (lamc(k).gt.lammax) then
	lamc(k) = lammax
	ncic(i,k) = 6._r8*lamc(k)**3*qcic(i,k)* &
                gamma(pgam(k)+1._r8)/ &
               (pi*rhow*gamma(pgam(k)+4._r8))
	end if

! parameter to calculate droplet freezing

        cdist1(k) = ncic(i,k)/gamma(pgam(k)+1._r8) 

	else
	lamc(k) = 0._r8
        cdist1(k) = 0._r8
	end if

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! begin micropysical process calculations 
!.................................................................
! autoconversion of cloud liquid water to rain
! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
! minimum qc of 1 x 10^-8 prevents floating point error

	if (qcic(i,k).ge.1.e-8_r8) then

! nprc is increase in rain number conc due to autoconversion
! nprc1 is decrease in cloud droplet conc due to autoconversion

! assume exponential sub-grid distribution of qc, resulting in additional
! factor related to qcvar below

 ! hm switch for sub-columns, don't include sub-grid qc
           if (sub_column) then
 
              prc(k) = 1350._r8*qcic(i,k)**2.47_r8* &
              (ncic(i,k)/1.e6_r8*rho(i,k))**(-1.79_r8)
              nprc(k) = prc(k)/(4._r8/3._r8*pi*rhow*(25.e-6_r8)**3)
              nprc1(k) = prc(k)/(qcic(i,k)/ncic(i,k))
 
           else

              prc(k) = cons2/(cons3*cons18)*1350._r8*qcic(i,k)**2.47_r8* &
              (ncic(i,k)/1.e6_r8*rho(i,k))**(-1.79_r8)
              nprc(k) = prc(k)/cons22
              nprc1(k) = prc(k)/(qcic(i,k)/ncic(i,k))
 
           end if               ! sub-column switch

        else
           prc(k)=0._r8
           nprc(k)=0._r8
	   nprc1(k)=0._r8
	end if
 
! add autoconversion to precip from above to get provisional rain mixing ratio
! and number concentration (qric and nric)

! 0.45 m/s is fallspeed of new rain drop (80 micron diameter)

	dum=0.45_r8
	dum1=0.45_r8

	if (k.eq.1) then
	qric(i,k)=prc(k)*lcldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	nric(i,k)=nprc(k)*lcldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	else
	if (qric(i,k-1).ge.qsmall) then
	dum=umr(k-1)
	dum1=unr(k-1)
	end if

! no autoconversion of rain number if rain/snow falling from above
! this assumes that new drizzle drops formed by autoconversion are rapidly collected
! by the existing rain/snow particles from above

	if (qric(i,k-1).ge.1.e-9_r8.or.qniic(i,k-1).ge.1.e-9_r8) then
	nprc(k)=0._r8
	end if

        qric(i,k) = (rho(i,k-1)*umr(k-1)*qric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*((pra(k-1)+prc(k))*lcldm(i,k)+(pre(k-1)-pracs(k-1)-mnuccr(k-1))*cldmax(i,k))))&
                   /(dum*rho(i,k)*cldmax(i,k))
	nric(i,k) = (rho(i,k-1)*unr(k-1)*nric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*(nprc(k)*lcldm(i,k)+(nsubr(k-1)-npracs(k-1)-nnuccr(k-1)+nragg(k-1))*cldmax(i,k))))&
                   /(dum1*rho(i,k)*cldmax(i,k))

	end if

!.......................................................................
! Autoconversion of cloud ice to snow
! similar to Ferrier (1994)

	if (t(i,k).le.273.15_r8.and.qiic(i,k).ge.qsmall) then

! note: assumes autoconversion timescale of 180 sec

           nprci(k) = n0i(k)/(lami(k)*180._r8)*exp(-lami(k)*dcs)

           prci(k) = pi*rhoi*n0i(k)/(6._r8*180._r8)* &
              (cons23/lami(k)+3._r8*cons24/lami(k)**2+ &
          6._r8*dcs/lami(k)**3+6._r8/lami(k)**4)*exp(-lami(k)*dcs)          

        else
              prci(k)=0._r8
              nprci(k)=0._r8
	end if

! add autoconversion to flux from level above to get provisional snow mixing ratio
! and number concentration (qniic and nsic)

	dum=(asn(i,k)*cons25)
	dum1=(asn(i,k)*cons25)

	if (k.eq.1) then
           qniic(i,k)=prci(k)*icldm(i,k)*dz(i,k)/cldmax(i,k)/dum
           nsic(i,k)=nprci(k)*icldm(i,k)*dz(i,k)/cldmax(i,k)/dum
	else
	if (qniic(i,k-1).ge.qsmall) then
	dum=ums(k-1)
	dum1=uns(k-1)
	end if

        qniic(i,k) = (rho(i,k-1)*ums(k-1)*qniic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*((prci(k)+prai(k-1)+psacws(k-1)+bergs(k-1))*icldm(i,k)+(prds(k-1)+ &
         pracs(k-1)+mnuccr(k-1))*cldmax(i,k))))&
                    /(dum*rho(i,k)*cldmax(i,k))

	nsic(i,k) = (rho(i,k-1)*uns(k-1)*nsic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*(nprci(k)*icldm(i,k)+(nsubs(k-1)+nsagg(k-1)+nnuccr(k-1))*cldmax(i,k))))&
                   /(dum1*rho(i,k)*cldmax(i,k))

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

!.......................................................................
! get size distribution parameters for precip
!......................................................................
! rain

	if (qric(i,k).ge.qsmall) then
	lamr(k) = (pi*rhow*nric(i,k)/qric(i,k))**(1._r8/3._r8)
	n0r(k) = nric(i,k)*lamr(k)

! check for slope
! adjust vars

	if (lamr(k).lt.lamminr) then

	lamr(k) = lamminr

	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	else if (lamr(k).gt.lammaxr) then
	lamr(k) = lammaxr
	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	end if

! provisional rain number and mass weighted mean fallspeed (m/s)

        unr(k) = min(arn(i,k)*cons4/lamr(k)**br,9.1_r8*rhof(i,k))
        umr(k) = min(arn(i,k)*cons5/(6._r8*lamr(k)**br),9.1_r8*rhof(i,k))

	else
	lamr(k) = 0._r8
	n0r(k) = 0._r8
	umr(k) = 0._r8
	unr(k) = 0._r8
	end if

!......................................................................
! snow

	if (qniic(i,k).ge.qsmall) then
	lams(k) = (cons6*cs*nsic(i,k)/ &
            qniic(i,k))**(1._r8/ds)
	n0s(k) = nsic(i,k)*lams(k)

! check for slope
! adjust vars

	if (lams(k).lt.lammins) then
	lams(k) = lammins
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)

	else if (lams(k).gt.lammaxs) then
	lams(k) = lammaxs
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)
	end if

! provisional snow number and mass weighted mean fallspeed (m/s)

        ums(k) = min(asn(i,k)*cons8/(6._r8*lams(k)**bs),1.2_r8*rhof(i,k))
        uns(k) = min(asn(i,k)*cons7/lams(k)**bs,1.2_r8*rhof(i,k))

	else
	lams(k) = 0._r8
	n0s(k) = 0._r8
	ums(k) = 0._r8
	uns(k) = 0._r8
	end if

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! heterogeneous freezing of cloud water

	if (qcic(i,k).ge.qsmall .and. t(i,k).lt.269.15_r8) then

! immersion freezing (Bigg, 1953)

 
! subcolumns
           
           if (sub_column) then
 
              mnuccc(k) = &
                       pi*pi/36._r8*rhow* &
                   cdist1(k)*gamma(7._r8+pgam(k))* &
                    bimm*(exp(aimm*(273.15_r8-t(i,k)))-1._r8)/ &
                    lamc(k)**3/lamc(k)**3
 
              nnuccc(k) = &
                    pi/6._r8*cdist1(k)*gamma(pgam(k)+4._r8) &
                    *bimm* &
                    (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamc(k)**3
 
           else
 
              mnuccc(k) = cons9/(cons3*cons19)* &
                      pi*pi/36._r8*rhow* &
                  cdist1(k)*gamma(7._r8+pgam(k))* &
                   bimm*(exp(aimm*(273.15_r8-t(i,k)))-1._r8)/ &
                   lamc(k)**3/lamc(k)**3

              nnuccc(k) = cons10/(cons3*qcvar)* &
                  pi/6._r8*cdist1(k)*gamma(pgam(k)+4._r8) &
                  *bimm* &
                  (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamc(k)**3
           end if           ! sub-columns


! contact freezing (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
! dust size and number in 4 bins are read in from companion routine

           tcnt=(270.16_r8-t(i,k))**1.3_r8
           viscosity=1.8e-5_r8*(t(i,k)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s)
           mfp=2.0_r8*viscosity/(p(i,k)  &                   ! Mean free path (m)
               *sqrt(8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i,k))))           

           nslip1=1.0_r8+(mfp/rndst(i,k,1))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,1)/mfp))))! Slip correction factor
           nslip2=1.0_r8+(mfp/rndst(i,k,2))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,2)/mfp))))
           nslip3=1.0_r8+(mfp/rndst(i,k,3))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,3)/mfp))))
           nslip4=1.0_r8+(mfp/rndst(i,k,4))*(1.257_r8+(0.4_r8*Exp(-(1.1_r8*rndst(i,k,4)/mfp))))

           ndfaer1=1.381e-23_r8*t(i,k)*nslip1/(6._r8*pi*viscosity*rndst(i,k,1))  ! aerosol diffusivity (m2/s)
           ndfaer2=1.381e-23_r8*t(i,k)*nslip2/(6._r8*pi*viscosity*rndst(i,k,2))
           ndfaer3=1.381e-23_r8*t(i,k)*nslip3/(6._r8*pi*viscosity*rndst(i,k,3))
           ndfaer4=1.381e-23_r8*t(i,k)*nslip4/(6._r8*pi*viscosity*rndst(i,k,4))


           if (sub_column) then
 
               mnucct(k) = &
                        (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                  ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*pi*pi/3._r8*rhow* &
                        cdist1(k)*gamma(pgam(k)+5._r8)/lamc(k)**4
 
               nnucct(k) = (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                     ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*2._r8*pi*  &
                        cdist1(k)*gamma(pgam(k)+2._r8)/lamc(k)
 
           else

               mnucct(k) = gamma(qcvar+4._r8/3._r8)/(cons3*qcvar**(4._r8/3._r8))*  &
                       (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                 ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*pi*pi/3._r8*rhow* &
                       cdist1(k)*gamma(pgam(k)+5._r8)/lamc(k)**4

                nnucct(k) =  gamma(qcvar+1._r8/3._r8)/(cons3*qcvar**(1._r8/3._r8))*  &
                         (ndfaer1*(nacon(i,k,1)*tcnt)+ndfaer2*(nacon(i,k,2)*tcnt)+&
                                   ndfaer3*(nacon(i,k,3)*tcnt)+ndfaer4*(nacon(i,k,4)*tcnt))*2._r8*pi*  &
                       cdist1(k)*gamma(pgam(k)+2._r8)/lamc(k)

           end if      ! sub-column switch

! make sure number of droplets frozen does not exceed available ice nuclei concentration
! this prevents 'runaway' droplet freezing

        if (nnuccc(k)*lcldm(i,k).gt.nnuccd(k)) then
        dum=(nnuccd(k)/(nnuccc(k)*lcldm(i,k)))
! scale mixing ratio of droplet freezing with limit
        mnuccc(k)=mnuccc(k)*dum
        nnuccc(k)=nnuccd(k)/lcldm(i,k)
        end if

        else
           mnuccc(k)=0._r8
           nnuccc(k)=0._r8
           mnucct(k)=0._r8
           nnucct(k)=0._r8
	end if

!.......................................................................
! snow self-aggregation from passarelli, 1978, used by reisner, 1998
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
! since minimum size ice particle for accretion is 50 - 150 micron

! ignore collision of snow with droplets above freezing

	if (qniic(i,k).ge.qsmall .and. t(i,k).le.tmelt .and. &
            qcic(i,k).ge.qsmall) then

! put in size dependent collection efficiency
! mean diameter of snow is area-weighted, since
! accretion is function of crystal geometric area
! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)

        dc0 = (pgam(k)+1._r8)/lamc(k)
        ds0 = 1._r8/lams(k)
        dum = dc0*dc0*uns(k)*rhow/(9._r8*mu(i,k)*ds0)
        eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))

        eci = max(eci,0._r8)
        eci = min(eci,1._r8)


! no impact of sub-grid distribution of qc since psacws
! is linear in qc

	psacws(k) = pi/4._r8*asn(i,k)*qcic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*cons11/ &
                  lams(k)**(bs+3._r8)	
	npsacws(k) = pi/4._r8*asn(i,k)*ncic(i,k)*rho(i,k)* &
                  n0s(k)*Eci*cons11/ &
                  lams(k)**(bs+3._r8)
        else
           psacws(k)=0._r8
           npsacws(k)=0._r8
        end if

! add secondary ice production due to accretion of droplets by snow 
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
                  (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamr(k)**3 &
                 /lamr(k)**3

	nnuccr(k) = pi*nric(i,k)*bimm* &
                   (exp(aimm*(273.15_r8-t(i,k)))-1._r8)/lamr(k)**3
        else
           mnuccr(k)=0._r8
           nnuccr(k)=0._r8
	end if

!.......................................................................
! accretion of cloud liquid water by rain
! formula from Khrouditnov and Kogan (2000)
! gravitational collection kernel, droplet fall speed neglected

	if (qric(i,k).ge.qsmall .and. qcic(i,k).ge.qsmall) then

! include sub-grid distribution of cloud water

! add sub-column switch
 
           if (sub_column) then
 
              pra(k) = &
                       67._r8*(qcic(i,k)*qric(i,k))**1.15_r8
              npra(k) = pra(k)/(qcic(i,k)/ncic(i,k))
 
           else

              pra(k) = cons12/(cons3*cons20)* &
                      67._r8*(qcic(i,k)*qric(i,k))**1.15_r8
              npra(k) = pra(k)/(qcic(i,k)/ncic(i,k))

           end if               ! sub-column switch

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
                  n0s(k)*Eii*cons11/ &
                  lams(k)**(bs+3._r8)	
	nprai(k) = pi/4._r8*asn(i,k)*niic(i,k)* &
                  rho(i,k)*n0s(k)*Eii*cons11/ &
                  lams(k)**(bs+3._r8)
        else
        prai(k)=0._r8
        nprai(k)=0._r8
	end if

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! calculate evaporation/sublimation of rain and snow
! note: evaporation/sublimation occurs only in cloud-free portion of grid cell
! in-cloud condensation/deposition of rain and snow is neglected
! except for transfer of cloud water to snow through bergeron process

! initialize evap/sub tendncies
	pre(k)=0._r8
	prds(k)=0._r8

! evaporation of rain
! only calculate if there is some precip fraction > cloud fraction

        if (qcic(i,k)+qiic(i,k).lt.1.e-6_r8.or.cldmax(i,k).gt.lcldm(i,k)) then

! set temporary cloud fraction to zero if cloud water + ice is very small
! this will ensure that evaporation/sublimation of precip occurs over
! entire grid cell, since min cloud fraction is specified otherwise
        if (qcic(i,k)+qiic(i,k).lt.1.e-6_r8) then
	dum=0._r8
	else
	dum=lcldm(i,k)
	end if

! saturation vapor pressure
        esn=polysvp(t(i,k),0)
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)

! recalculate saturation vapor pressure for liquid and ice
	esl(i,k)=esn
	esi(i,k)=polysvp(t(i,k),1)
! hm fix, make sure when above freezing that esi=esl, not active yet
	if (t(i,k).gt.tmelt)esi(i,k)=esl(i,k)

! calculate q for out-of-cloud region
	qclr=(q(i,k)-dum*qsn)/(1._r8-dum)

	if (qric(i,k).ge.qsmall) then

        qvs=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
	dqsdt = xxlv*qvs/(rv*t(i,k)**2)
        ab = 1._r8+dqsdt*xxlv/cpp
        epsr = 2._r8*pi*n0r(k)*rho(i,k)*Dv(i,k)* &
                   (f1r/(lamr(k)*lamr(k))+ &
                    f2r*(arn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons13/ &
                (lamr(k)**(5._r8/2._r8+br/2._r8)))

	   pre(k) = epsr*(qclr-qvs)/ab

! only evaporate in out-of-cloud region
! and distribute across cldmax
           pre(k)=min(pre(k)*(cldmax(i,k)-dum),0._r8)
           pre(k)=pre(k)/cldmax(i,k)
	end if

! sublimation of snow
	if (qniic(i,k).ge.qsmall) then
        qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
        dqsidt =  xxls*qvi/(rv*t(i,k)**2)
        abi = 1._r8+dqsidt*xxls/cpp
        epss = 2._r8*pi*n0s(k)*rho(i,k)*Dv(i,k)* &
                   (f1s/(lams(k)*lams(k))+ &
                    f2s*(asn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons14/ &
               (lams(k)**(5._r8/2._r8+bs/2._r8)))
	   prds(k) = epss*(qclr-qvi)/abi	

! only sublimate in out-of-cloud region and distribute over cldmax
           prds(k)=min(prds(k)*(cldmax(i,k)-dum),0._r8)
	   prds(k)=prds(k)/cldmax(i,k)
	end if

! make sure RH not pushed above 100% due to rain evaporation/snow sublimation
! get updated RH at end of time step based on cloud water/ice condensation/evap

	qtmp=q(i,k)-(cmei(i,k)+(pre(k)+prds(k))*cldmax(i,k))*deltat
	ttmp=t(i,k)+((pre(k)*cldmax(i,k))*xxlv+ &
                (cmei(i,k)+prds(k)*cldmax(i,k))*xxls)*deltat/cpp

!limit range of temperatures!
        ttmp=max(180._r8,min(ttmp,323._r8))

        esn=polysvp(ttmp,0)  ! use rhw to allow ice supersaturation
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)

! modify precip evaporation rate if q > qsat
	if (qtmp.gt.qsn) then
	if (pre(k)+prds(k).lt.-1.e-20) then
	dum1=pre(k)/(pre(k)+prds(k))
! recalculate q and t after cloud water cond but without precip evap
	qtmp=q(i,k)-(cmei(i,k))*deltat
	ttmp=t(i,k)+(cmei(i,k)*xxls)*deltat/cpp
        esn=polysvp(ttmp,0) ! use rhw to allow ice supersaturation
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)
	dum=(qtmp-qsn)/(1._r8 + cons27*qsn/(cpp*rv*ttmp**2))
	dum=min(dum,0._r8)

! modify rates if needed, divide by cldmax to get local (in-precip) value
	pre(k)=dum*dum1/deltat/cldmax(i,k)

! do separately using RHI for prds....
        esn=polysvp(ttmp,1) ! use rhi to allow ice supersaturation
	qsn=min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)
	dum=(qtmp-qsn)/(1._r8 + cons28*qsn/(cpp*rv*ttmp**2))
	dum=min(dum,0._r8)

! modify rates if needed, divide by cldmax to get local (in-precip) value
	prds(k)=dum*(1._r8-dum1)/deltat/cldmax(i,k)
	end if
	end if

	end if

! bergeron process - evaporation of droplets and deposition onto snow

	if (qniic(i,k).ge.qsmall.and.qcic(i,k).ge.qsmall.and.t(i,k).lt.tmelt) then
        qvi=epsqs*esi(i,k)/(p(i,k)-(1._r8-epsqs)*esi(i,k))
        qvs=epsqs*esl(i,k)/(p(i,k)-(1._r8-epsqs)*esl(i,k))
        dqsidt =  xxls*qvi/(rv*t(i,k)**2)
        abi = 1._r8+dqsidt*xxls/cpp
        epss = 2._r8*pi*n0s(k)*rho(i,k)*Dv(i,k)* &
                   (f1s/(lams(k)*lams(k))+ &
                    f2s*(asn(i,k)*rho(i,k)/mu(i,k))**0.5_r8* &
                    sc(i,k)**(1._r8/3._r8)*cons14/ &
               (lams(k)**(5._r8/2._r8+bs/2._r8)))
	bergs(k)=epss*(qvs-qvi)/abi
	else
	bergs(k)=0._r8
	end if

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! conservation to ensure no negative values of cloud water/precipitation
! in case microphysical process rates are large

! make sure and use end-of-time step values for cloud water, ice, due
! condensation/deposition

! note: for check on conservation, processes are multiplied by omsm
! to prevent problems due to round off error

! include mixing timescale  (mtime)

        qce=(qc(i,k) - berg(i,k)*deltat)
        nce=(nc(i,k)+npccn(k)*deltat*mtime)
        qie=(qi(i,k)+(cmei(i,k)+berg(i,k))*deltat)
        nie=(ni(i,k)+nnuccd(k)*deltat*mtime)

! conservation of qc

        dum = (prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+ &

              psacws(k)+bergs(k))*lcldm(i,k)*deltat

	if (dum.gt.qce) then
!PMA
         if (abs(prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+psacws(k)+bergs(k)) > 1.e-20_r8) then
          ratio = qce/deltat/lcldm(i,k)/(prc(k)+pra(k)+mnuccc(k)+mnucct(k)+msacwi(k)+psacws(k)+bergs(k))*omsm 
         else
          ratio = 0.5_r8
         end if
	prc(k) = prc(k)*ratio
	pra(k) = pra(k)*ratio
	mnuccc(k) = mnuccc(k)*ratio
        mnucct(k) = mnucct(k)*ratio  
        msacwi(k) = msacwi(k)*ratio  
	psacws(k) = psacws(k)*ratio	
	bergs(k) = bergs(k)*ratio	
        end if

! conservation of nc

        dum = (nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+ &
              npsacws(k)-nsubc(k))*lcldm(i,k)*deltat

	if (dum.gt.nce) then
!PMA
          if (abs(nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+npsacws(k)-nsubc(k)) > 1.e-20_r8) then
        ratio = nce/deltat/((nprc1(k)+npra(k)+nnuccc(k)+nnucct(k)+&
            npsacws(k)-nsubc(k))*lcldm(i,k))*omsm
          else
        ratio = 0.5_r8
          end if

	nprc1(k) = nprc1(k)*ratio
	npra(k) = npra(k)*ratio
	nnuccc(k) = nnuccc(k)*ratio
        nnucct(k) = nnucct(k)*ratio  
	npsacws(k) = npsacws(k)*ratio	
	nsubc(k)=nsubc(k)*ratio
        end if

! conservation of qi

        dum = ((-mnuccc(k)-mnucct(k)-msacwi(k))*lcldm(i,k)+(prci(k)+ &
               prai(k))*icldm(i,k))*deltat

	if (dum.gt.qie) then

!jdf
        if(abs(prci(k)+prai(k)).gt.1.0e-20) then
          ratio = (qie/deltat+(mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k))/((prci(k)+prai(k))*icldm(i,k))*omsm
        else
          ratio=0.5
        endif
!jdf        ratio = (qie/deltat+(mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k))/((prci(k)+prai(k))*icldm(i,k))*omsm 
        prci(k) = prci(k)*ratio
        prai(k) = prai(k)*ratio
        end if

! conservation of ni

        dum = ((-nnucct(k)-nsacwi(k))*lcldm(i,k)+(nprci(k)+ &
               nprai(k)-nsubi(k))*icldm(i,k))*deltat

	if (dum.gt.nie) then

           if(abs( ((nprci(k)+nprai(k)-nsubi(k))*icldm(i,k))*omsm )  .gt. 1.0e-20_r8) then
              ratio = (nie/deltat+(nnucct(k)+nsacwi(k))*lcldm(i,k))/ &  
                   ((nprci(k)+nprai(k)-nsubi(k))*icldm(i,k))*omsm
           else
              ratio = 0.5
           endif
        nprci(k) = nprci(k)*ratio
        nprai(k) = nprai(k)*ratio
	nsubi(k) = nsubi(k)*ratio
        end if

! for preciptiation conservation, use logic that vertical integral 
! of tendency from current level to top of model (i.e., qrtot) cannot be negative

! conservation of rain mixing rat

	if (((prc(k)+pra(k))*lcldm(i,k)+(-mnuccr(k)+pre(k)-pracs(k))*&
               cldmax(i,k))*dz(i,k)*rho(i,k)+qrtot.lt.0._r8) then

	if (-pre(k)+pracs(k)+mnuccr(k).ge.qsmall) then
	     
	ratio = (qrtot/(dz(i,k)*rho(i,k))+(prc(k)+pra(k))*lcldm(i,k))/&
                ((-pre(k)+pracs(k)+mnuccr(k))*cldmax(i,k))*omsm 

        pre(k) = pre(k)*ratio
        pracs(k) = pracs(k)*ratio
        mnuccr(k) = mnuccr(k)*ratio
	end if
        end if

! conservation of nr
! for now neglect evaporation of nr
       nsubr(k)=0._r8

       if ((nprc(k)*lcldm(i,k)+(-nnuccr(k)+nsubr(k)-npracs(k)&
            +nragg(k))*cldmax(i,k))*dz(i,k)*rho(i,k)+nrtot.lt.0._r8) then

	if (-nsubr(k)-nragg(k)+npracs(k)+nnuccr(k).ge.qsmall) then

	ratio = (nrtot/(dz(i,k)*rho(i,k))+nprc(k)*lcldm(i,k))/&
               ((-nsubr(k)-nragg(k)+npracs(k)+nnuccr(k))*cldmax(i,k))*omsm

        nsubr(k) = nsubr(k)*ratio
        npracs(k) = npracs(k)*ratio
        nnuccr(k) = nnuccr(k)*ratio
        nragg(k) = nragg(k)*ratio
	end if
        end if

! conservation of snow mix ratio

	if (((bergs(k)+psacws(k))*lcldm(i,k)+(prai(k)+prci(k))*icldm(i,k)+(pracs(k)+&
            mnuccr(k)+prds(k))*cldmax(i,k))*dz(i,k)*rho(i,k)+qstot.lt.0._r8) then

	if (-prds(k).ge.qsmall) then

	ratio = (qstot/(dz(i,k)*rho(i,k))+(bergs(k)+psacws(k))*lcldm(i,k)+(prai(k)+prci(k))*icldm(i,k)+&
                (pracs(k)+mnuccr(k))*cldmax(i,k))/(-prds(k)*cldmax(i,k))*omsm

        prds(k) = prds(k)*ratio
	end if
       end if

! conservation of ns

! calculate loss of number due to sublimation
! for now neglect sublimation of ns
       nsubs(k)=0._r8

       if ((nprci(k)*icldm(i,k)+(nnuccr(k)+nsubs(k)+nsagg(k))*cldmax(i,k))*&
            dz(i,k)*rho(i,k)+nstot.lt.0._r8) then

	if (-nsubs(k)-nsagg(k).ge.qsmall) then

	ratio = (nstot/(dz(i,k)*rho(i,k))+nprci(k)*icldm(i,k)+&
                nnuccr(k)*cldmax(i,k))/((-nsubs(k)-nsagg(k))*cldmax(i,k))*omsm

        nsubs(k) = nsubs(k)*ratio
        nsagg(k) = nsagg(k)*ratio
	end if
        end if

! get tendencies due to microphysical conversion processes
! note: tendencies are multiplied by appropaiate cloud/precip 
! fraction to get grid-scale values
! note: cmei is already grid-average values

	qvlat(i,k) = qvlat(i,k)- &
       (pre(k)+prds(k))*cldmax(i,k)-cmei(i,k) 

	tlat(i,k) = tlat(i,k)+((pre(k)*cldmax(i,k)) &
           *xxlv+(prds(k)*cldmax(i,k)+cmei(i,k))*xxls+ &
           ((bergs(k)+psacws(k)+mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k)+(mnuccr(k)+ &
               pracs(k))*cldmax(i,k)+berg(i,k))*xlf)

	qctend(i,k) = qctend(i,k)+ &
                 (-pra(k)-prc(k)-mnuccc(k)-mnucct(k)-msacwi(k)- & 
                  psacws(k)-bergs(k))*lcldm(i,k)-berg(i,k)

	qitend(i,k) = qitend(i,k)+ &
         (mnuccc(k)+mnucct(k)+msacwi(k))*lcldm(i,k)+(-prci(k)- & 
                  prai(k))*icldm(i,k)+cmei(i,k)+berg(i,k)

	qrtend(i,k) = qrtend(i,k)+ &
                 (pra(k)+prc(k))*lcldm(i,k)+(pre(k)-pracs(k)- &
                  mnuccr(k))*cldmax(i,k)

	qnitend(i,k) = qnitend(i,k)+ &
                (prai(k)+prci(k))*icldm(i,k)+(psacws(k)+bergs(k))*lcldm(i,k)+(prds(k)+ &
                   pracs(k)+mnuccr(k))*cldmax(i,k)

! add output for cmei (accumulate)
	cmeiout(i,k) = cmeiout(i,k) + cmei(i,k)

! assign variables for trop_mozart, these are grid-average
! evaporation/sublimation is stored here as positive term

	evapsnow(i,k) = evapsnow(i,k)-prds(k)*cldmax(i,k)
	nevapr(i,k) = nevapr(i,k)-pre(k)*cldmax(i,k)

! change to make sure prain is positive: do not remove snow from
! prain used for wet deposition
        	prain(i,k) = prain(i,k)+(pra(k)+prc(k))*lcldm(i,k)+(-pracs(k)- &
                          mnuccr(k))*cldmax(i,k)
	prodsnow(i,k) = prodsnow(i,k)+(prai(k)+prci(k))*icldm(i,k)+(psacws(k)+bergs(k))*lcldm(i,k)+(&
                   pracs(k)+mnuccr(k))*cldmax(i,k)

! following are used to calculate 1st order conversion rate of cloud water
!    to rain and snow (1/s), for later use in aerosol wet removal routine
! previously, wetdepa used (prain/qc) for this, and the qc in wetdepa may be smaller than the qc
!    used to calculate pra, prc, ... in this routine
! qcsinksum_rate1ord = sum over iterations{ rate of direct transfer of cloud water to rain & snow }
!                      (no cloud ice or bergeron terms)
! qcsum_rate1ord     = sum over iterations{ qc used in calculation of the transfer terms }

        qcsinksum_rate1ord(k) = qcsinksum_rate1ord(k) + (pra(k)+prc(k)+psacws(k))*lcldm(i,k) 
        qcsum_rate1ord(k) = qcsum_rate1ord(k) + qc(i,k) 

! microphysics output, note this is grid-averaged
        prao(i,k)=prao(i,k)+pra(k)*lcldm(i,k)
        prco(i,k)=prco(i,k)+prc(k)*lcldm(i,k)
        mnuccco(i,k)=mnuccco(i,k)+mnuccc(k)*lcldm(i,k)
        mnuccto(i,k)=mnuccto(i,k)+mnucct(k)*lcldm(i,k)
        mnuccdo(i,k)=mnuccdo(i,k)+mnuccd(k)*lcldm(i,k)
        msacwio(i,k)=msacwio(i,k)+msacwi(k)*lcldm(i,k)
        psacwso(i,k)=psacwso(i,k)+psacws(k)*lcldm(i,k)
        bergso(i,k)=bergso(i,k)+bergs(k)*lcldm(i,k)
        bergo(i,k)=bergo(i,k)+berg(i,k)
        prcio(i,k)=prcio(i,k)+prci(k)*icldm(i,k)
        praio(i,k)=praio(i,k)+prai(k)*icldm(i,k)
        mnuccro(i,k)=mnuccro(i,k)+mnuccr(k)*cldmax(i,k)
        pracso (i,k)=pracso (i,k)+pracs (k)*cldmax(i,k)

! multiply activation/nucleation by mtime to account for fast timescale

	nctend(i,k) = nctend(i,k)+ npccn(k)*mtime+&
                  (-nnuccc(k)-nnucct(k)-npsacws(k)+nsubc(k) & 
                  -npra(k)-nprc1(k))*lcldm(i,k)      

	nitend(i,k) = nitend(i,k)+ nnuccd(k)*mtime+ & 
                  (nnucct(k)+nsacwi(k))*lcldm(i,k)+(nsubi(k)-nprci(k)- &   
                  nprai(k))*icldm(i,k)

	nstend(i,k) = nstend(i,k)+(nsubs(k)+ &
                  nsagg(k)+nnuccr(k))*cldmax(i,k)+nprci(k)*icldm(i,k)

	nrtend(i,k) = nrtend(i,k)+ &
                  nprc(k)*lcldm(i,k)+(nsubr(k)-npracs(k)-nnuccr(k) &
                   +nragg(k))*cldmax(i,k)

! make sure that nc and ni at advanced time step do not exceed
! maximum (existing N + source terms*dt), which is possible due to
! fast nucleation timescale

	if (nctend(i,k).gt.0._r8.and.nc(i,k)+nctend(i,k)*deltat.gt.ncmax) then
	nctend(i,k)=max(0._r8,(ncmax-nc(i,k))/deltat)
	end if
	if (nitend(i,k).gt.0._r8.and.ni(i,k)+nitend(i,k)*deltat.gt.nimax) then
	nitend(i,k)=max(0._r8,(nimax-ni(i,k))/deltat)
	end if

! get final values for precipitation q and N, based on
! flux of precip from above, source/sink term, and terminal fallspeed
! see eq. 15-16 in MG2008

! rain

        if (qric(i,k).ge.qsmall) then
	if (k.eq.1) then
	qric(i,k)=qrtend(i,k)*dz(i,k)/cldmax(i,k)/umr(k)
	nric(i,k)=nrtend(i,k)*dz(i,k)/cldmax(i,k)/unr(k)
	else
        qric(i,k) = (rho(i,k-1)*umr(k-1)*qric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*qrtend(i,k)))/(umr(k)*rho(i,k)*cldmax(i,k))
	nric(i,k) = (rho(i,k-1)*unr(k-1)*nric(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*nrtend(i,k)))/(unr(k)*rho(i,k)*cldmax(i,k))

	end if
	else
	qric(i,k)=0._r8
	nric(i,k)=0._r8
	end if

! snow

        if (qniic(i,k).ge.qsmall) then
	if (k.eq.1) then
	qniic(i,k)=qnitend(i,k)*dz(i,k)/cldmax(i,k)/ums(k)	
	nsic(i,k)=nstend(i,k)*dz(i,k)/cldmax(i,k)/uns(k)
	else
        qniic(i,k) = (rho(i,k-1)*ums(k-1)*qniic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*qnitend(i,k)))/(ums(k)*rho(i,k)*cldmax(i,k))
	nsic(i,k) = (rho(i,k-1)*uns(k-1)*nsic(i,k-1)*cldmax(i,k-1)+ &
         (rho(i,k)*dz(i,k)*nstend(i,k)))/(uns(k)*rho(i,k)*cldmax(i,k))
	end if
	else
	qniic(i,k)=0._r8
	nsic(i,k)=0._r8
	end if

! calculate precipitation flux at surface
! divide by density of water to get units of m/s

	prect(i) = prect(i)+(qrtend(i,k)*dz(i,k)*rho(i,k)+&
                   qnitend(i,k)*dz(i,k)*rho(i,k))/rhow
	preci(i) = preci(i)+qnitend(i,k)*dz(i,k)*rho(i,k)/rhow

        ! convert rain rate from m/s to mm/hr
 
         rainrt(i,k)=qric(i,k)*rho(i,k)*umr(k)/rhow*3600._r8*1000._r8

! vertically-integrated precip source/sink terms (note: grid-averaged)

	qrtot = max(qrtot+qrtend(i,k)*dz(i,k)*rho(i,k),0._r8)
	qstot = max(qstot+qnitend(i,k)*dz(i,k)*rho(i,k),0._r8)
	nrtot = max(nrtot+nrtend(i,k)*dz(i,k)*rho(i,k),0._r8)
	nstot = max(nstot+nstend(i,k)*dz(i,k)*rho(i,k),0._r8)

! calculate melting and freezing of precip

! melt snow at +2 C

        if (t(i,k)+tlat(i,k)/cpp*deltat > 275.15_r8) then
           if (qstot > 0._r8) then

! make sure melting snow doesn't reduce temperature below threshold
	      dum = -xlf/cpp*qstot/(dz(i,k)*rho(i,k))
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.275.15_r8) then
	       dum = (t(i,k)+tlat(i,k)/cpp*deltat-275.15_r8)*cpp/xlf
	       dum = dum/(xlf/cpp*qstot/(dz(i,k)*rho(i,k)))
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
               dum = 1._r8
              end if

	      qric(i,k)=qric(i,k)+dum*qniic(i,k)
	      nric(i,k)=nric(i,k)+dum*nsic(i,k)
	      qniic(i,k)=(1._r8-dum)*qniic(i,k)
	      nsic(i,k)=(1._r8-dum)*nsic(i,k)
! heating tendency 
              tmp=-xlf*dum*qstot/(dz(i,k)*rho(i,k))
              meltsdt(i,k)=meltsdt(i,k) + tmp

	      tlat(i,k)=tlat(i,k)+tmp
	      qrtot=qrtot+dum*qstot
	      nrtot=nrtot+dum*nstot
	      qstot=(1._r8-dum)*qstot
	      nstot=(1._r8-dum)*nstot
	      preci(i)=(1._r8-dum)*preci(i)
           end if
        end if

! freeze all rain at -5C for Arctic

        if (t(i,k)+tlat(i,k)/cpp*deltat < (tmelt - 5._r8)) then

           if (qrtot > 0._r8) then

! make sure freezing rain doesn't increase temperature above threshold
          dum = xlf/cpp*qrtot/(dz(i,k)*rho(i,k))
          if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.(tmelt - 5._r8)) then
           dum = -(t(i,k)+tlat(i,k)/cpp*deltat-(tmelt-5._r8))*cpp/xlf
           dum = dum/(xlf/cpp*qrtot/(dz(i,k)*rho(i,k)))
           dum = max(0._r8,dum)
           dum = min(1._r8,dum)
          else
             dum = 1._r8
          end if
            
	      qniic(i,k)=qniic(i,k)+dum*qric(i,k)
	      nsic(i,k)=nsic(i,k)+dum*nric(i,k)
	      qric(i,k)=(1._r8-dum)*qric(i,k)
	      nric(i,k)=(1._r8-dum)*nric(i,k)
! heating tendency 
	      tmp = xlf*dum*qrtot/(dz(i,k)*rho(i,k))
	      frzrdt(i,k)=frzrdt(i,k) + tmp

	      tlat(i,k)=tlat(i,k)+tmp
	      qstot=qstot+dum*qrtot
	      qrtot=(1._r8-dum)*qrtot
	      nstot=nstot+dum*nrtot
	      nrtot=(1._r8-dum)*nrtot
	      preci(i)=preci(i)+dum*(prect(i)-preci(i))
           end if
        end if

! if rain/snow mix ratio is zero so should number concentration

	if (qniic(i,k).lt.qsmall) then
	qniic(i,k)=0._r8
	nsic(i,k)=0._r8
	end if

	if (qric(i,k).lt.qsmall) then
	qric(i,k)=0._r8
	nric(i,k)=0._r8
	end if

! make sure number concentration is a positive number to avoid 
! taking root of negative

	nric(i,k)=max(nric(i,k),0._r8)
	nsic(i,k)=max(nsic(i,k),0._r8)

!.......................................................................
! get size distribution parameters for fallspeed calculations
!......................................................................
! rain

	if (qric(i,k).ge.qsmall) then
	lamr(k) = (pi*rhow*nric(i,k)/qric(i,k))**(1._r8/3._r8)
	n0r(k) = nric(i,k)*lamr(k)

! check for slope
! change lammax and lammin for rain and snow
! adjust vars

	if (lamr(k).lt.lamminr) then

	lamr(k) = lamminr

	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	else if (lamr(k).gt.lammaxr) then
	lamr(k) = lammaxr
	n0r(k) = lamr(k)**4*qric(i,k)/(pi*rhow)
	nric(i,k) = n0r(k)/lamr(k)
	end if


! 'final' values of number and mass weighted mean fallspeed for rain (m/s)

        unr(k) = min(arn(i,k)*cons4/lamr(k)**br,9.1_r8*rhof(i,k))
        umr(k) = min(arn(i,k)*cons5/(6._r8*lamr(k)**br),9.1_r8*rhof(i,k))

	else
	lamr(k) = 0._r8
	n0r(k) = 0._r8
	umr(k)=0._r8
	unr(k)=0._r8
	end if

!calculate mean size of combined rain and snow

        if (lamr(k).gt.0._r8) then
           Artmp = n0r(k) * pi / (2 * lamr(k)**3._r8)
        else 
           Artmp = 0._r8
        endif

        if (lamc(k).gt.0._r8) then
           Actmp = cdist1(k) * pi * gamma(pgam(k)+3._r8)/(4._r8 * lamc(k)**2._r8)
        else 
           Actmp = 0._r8
        endif

        if (Actmp.gt.0_r8.or.Artmp.gt.0) then
           rercld(i,k)=rercld(i,k) + 3._r8 *(qric(i,k) + qcic(i,k)) / (4._r8 * rhow * (Actmp + Artmp))
           arcld(i,k)=arcld(i,k)+1._r8
        endif

!......................................................................
! snow

	if (qniic(i,k).ge.qsmall) then
	lams(k) = (cons6*cs*nsic(i,k)/ &
            qniic(i,k))**(1._r8/ds)
	n0s(k) = nsic(i,k)*lams(k)

! check for slope
! adjust vars

	if (lams(k).lt.lammins) then
	lams(k) = lammins
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)

	else if (lams(k).gt.lammaxs) then
	lams(k) = lammaxs
	n0s(k) = lams(k)**(ds+1._r8)*qniic(i,k)/(cs*cons6)
	nsic(i,k) = n0s(k)/lams(k)
	end if

! 'final' values of number and mass weighted mean fallspeed for snow (m/s)

        ums(k) = min(asn(i,k)*cons8/(6._r8*lams(k)**bs),1.2_r8*rhof(i,k))
        uns(k) = min(asn(i,k)*cons7/lams(k)**bs,1.2_r8*rhof(i,k))

	else
	lams(k) = 0._r8
	n0s(k) = 0._r8
	ums(k) = 0._r8
	uns(k) = 0._r8
	end if

!c........................................................................
! sum over sub-step for average process rates

! convert rain/snow q and N for output to history, note, 
! output is for gridbox average

	qrout(i,k)=qrout(i,k)+qric(i,k)*cldmax(i,k)
	qsout(i,k)=qsout(i,k)+qniic(i,k)*cldmax(i,k)
	nrout(i,k)=nrout(i,k)+nric(i,k)*rho(i,k)*cldmax(i,k)
	nsout(i,k)=nsout(i,k)+nsic(i,k)*rho(i,k)*cldmax(i,k)

	tlat1(i,k)=tlat1(i,k)+tlat(i,k)
	qvlat1(i,k)=qvlat1(i,k)+qvlat(i,k)
	qctend1(i,k)=qctend1(i,k)+qctend(i,k)
	qitend1(i,k)=qitend1(i,k)+qitend(i,k)
	nctend1(i,k)=nctend1(i,k)+nctend(i,k)
	nitend1(i,k)=nitend1(i,k)+nitend(i,k)

	t(i,k)=t(i,k)+tlat(i,k)*deltat/cpp
	q(i,k)=q(i,k)+qvlat(i,k)*deltat
	qc(i,k)=qc(i,k)+qctend(i,k)*deltat
	qi(i,k)=qi(i,k)+qitend(i,k)*deltat
	nc(i,k)=nc(i,k)+nctend(i,k)*deltat
	ni(i,k)=ni(i,k)+nitend(i,k)*deltat

        rainrt1(i,k)=rainrt1(i,k)+rainrt(i,k)

!divide rain radius over substeps for average
        if (arcld(i,k) .gt. 0._r8) then
           rercld(i,k)=rercld(i,k)/arcld(i,k)
        end if

!calculate precip fluxes and adding them to summing sub-stepping variables
      !! flux is zero at top interface
      	rflx(i,1)=0.0_r8
      	sflx(i,1)=0.0_r8
	
      !! calculating the precip flux (kg/m2/s) as mixingratio(kg/kg)*airdensity(kg/m3)*massweightedfallspeed(m/s)
      	rflx(i,k+1)=qrout(i,k)*rho(i,k)*umr(k)
	sflx(i,k+1)=qsout(i,k)*rho(i,k)*ums(k)
	
      !! add to summing sub-stepping variable
	rflx1(i,k+1)=rflx1(i,k+1)+rflx(i,k+1)
	sflx1(i,k+1)=sflx1(i,k+1)+sflx(i,k+1)	

!c........................................................................

	end do ! k loop

	prect1(i)=prect1(i)+prect(i)
	preci1(i)=preci1(i)+preci(i)

	end do ! it loop, sub-step

        do k = 1, pver               
        rate1ord_cw2pr_st(i,k) = qcsinksum_rate1ord(k)/max(qcsum_rate1ord(k),1.0e-30_r8) 
        end do                       

 300    continue  ! continue if no cloud water
	end do ! i loop

! convert dt from sub-step back to full time step
	deltat=deltat*real(iter)

!c.............................................................................

        do i=1,ncol

! skip all calculations if no cloud water
	if (ltrue(i).eq.0) then

	do k=1,pver
! assign default values for effective radius
	effc(i,k)=10._r8
	effi(i,k)=25._r8
	effc_fn(i,k)=10._r8
        lamcrad(i,k)=0._r8
        pgamrad(i,k)=0._r8
        deffi(i,k)=0._r8
	end do
	goto 500
	end if

! initialize nstep for sedimentation sub-steps
	nstep = 1

! divide precip rate by number of sub-steps to get average over time step

	prect(i)=prect1(i)/real(iter)
	preci(i)=preci1(i)/real(iter)

       do k=1,pver

! assign variables back to start-of-timestep values before updating after sub-steps 

	t(i,k)=t1(i,k)
	q(i,k)=q1(i,k)
	qc(i,k)=qc1(i,k)
	qi(i,k)=qi1(i,k)
	nc(i,k)=nc1(i,k)
	ni(i,k)=ni1(i,k)

! divide microphysical tendencies by number of sub-steps to get average over time step

	tlat(i,k)=tlat1(i,k)/real(iter)
	qvlat(i,k)=qvlat1(i,k)/real(iter)
	qctend(i,k)=qctend1(i,k)/real(iter)
	qitend(i,k)=qitend1(i,k)/real(iter)
	nctend(i,k)=nctend1(i,k)/real(iter)
	nitend(i,k)=nitend1(i,k)/real(iter)
 
        rainrt(i,k)=rainrt1(i,k)/real(iter)

! divide by number of sub-steps to find final values
	rflx(i,k+1)=rflx1(i,k+1)/real(iter)
	sflx(i,k+1)=sflx1(i,k+1)/real(iter)

! divide output precip q and N by number of sub-steps to get average over time step

	qrout(i,k)=qrout(i,k)/real(iter)
	qsout(i,k)=qsout(i,k)/real(iter)
	nrout(i,k)=nrout(i,k)/real(iter)
	nsout(i,k)=nsout(i,k)/real(iter)

! divide trop_mozart variables by number of sub-steps to get average over time step 

	nevapr(i,k) = nevapr(i,k)/real(iter)
	evapsnow(i,k) = evapsnow(i,k)/real(iter)
	prain(i,k) = prain(i,k)/real(iter)
	prodsnow(i,k) = prodsnow(i,k)/real(iter)
	cmeout(i,k) = cmeout(i,k)/real(iter)

	cmeiout(i,k) = cmeiout(i,k)/real(iter)
        meltsdt(i,k) = meltsdt(i,k)/real(iter)
        frzrdt (i,k) = frzrdt (i,k)/real(iter)


! microphysics output
        prao(i,k)=prao(i,k)/real(iter)
        prco(i,k)=prco(i,k)/real(iter)
        mnuccco(i,k)=mnuccco(i,k)/real(iter)
        mnuccto(i,k)=mnuccto(i,k)/real(iter)
        msacwio(i,k)=msacwio(i,k)/real(iter)
        psacwso(i,k)=psacwso(i,k)/real(iter)
        bergso(i,k)=bergso(i,k)/real(iter)
        bergo(i,k)=bergo(i,k)/real(iter)
        prcio(i,k)=prcio(i,k)/real(iter)
        praio(i,k)=praio(i,k)/real(iter)

        mnuccro(i,k)=mnuccro(i,k)/real(iter)
        pracso (i,k)=pracso (i,k)/real(iter)

        mnuccdo(i,k)=mnuccdo(i,k)/real(iter)

! modify to include snow. in prain & evap (diagnostic here: for wet dep)
        nevapr(i,k) = nevapr(i,k) + evapsnow(i,k)
        prain(i,k) = prain(i,k) + prodsnow(i,k)

!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! calculate sedimentation for cloud water and ice
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! update in-cloud cloud mixing ratio and number concentration 
! with microphysical tendencies to calculate sedimentation, assign to dummy vars
! note: these are in-cloud values***, hence we divide by cloud fraction

        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)/icldm(i,k)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k),0._r8)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat)/icldm(i,k),0._r8)

! obtain new slope parameter to avoid possible singularity

	if (dumi(i,k).ge.qsmall) then
! add upper limit to in-cloud number concentration to prevent numerical error
	dumni(i,k)=min(dumni(i,k),dumi(i,k)*1.e20_r8)

	lami(k) = (cons1*ci* &
              dumni(i,k)/dumi(i,k))**(1._r8/di)
        lami(k)=max(lami(k),lammini)
        lami(k)=min(lami(k),lammaxi)
        else
           lami(k)=0._r8
        end if

	if (dumc(i,k).ge.qsmall) then
! add upper limit to in-cloud number concentration to prevent numerical error
	dumnc(i,k)=min(dumnc(i,k),dumc(i,k)*1.e20_r8)
! add lower limit to in-cloud number concentration
         dumnc(i,k)=max(dumnc(i,k),cdnl/rho(i,k)) ! sghan minimum in #/cm3 
         pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
         pgam(k)=1._r8/(pgam(k)**2)-1._r8
         pgam(k)=max(pgam(k),2._r8)
         pgam(k)=min(pgam(k),15._r8)

	lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                 (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8
        lamc(k)=max(lamc(k),lammin)
        lamc(k)=min(lamc(k),lammax)
        else
           lamc(k)=0._r8
        end if

! calculate number and mass weighted fall velocity for droplets
! include effects of sub-grid distribution of cloud water


	if (dumc(i,k).ge.qsmall) then
	unc = &
              acn(i,k)*gamma(1._r8+bc+pgam(k))/ &
               (lamc(k)**bc*gamma(pgam(k)+1._r8))
	umc = &
              acn(i,k)*gamma(4._r8+bc+pgam(k))/ &
             (lamc(k)**bc*gamma(pgam(k)+4._r8))
! fallspeed for output
	vtrmc(i,k)=umc
	else
	umc = 0._r8
	unc = 0._r8
	end if

! calculate number and mass weighted fall velocity for cloud ice

	if (dumi(i,k).ge.qsmall) then
	uni =  ain(i,k)*cons16/lami(k)**bi
	umi = ain(i,k)*cons17/(6._r8*lami(k)**bi)
        uni=min(uni,1.2_r8*rhof(i,k))
        umi=min(umi,1.2_r8*rhof(i,k))

! fallspeed
	vtrmi(i,k)=umi
	else
	umi = 0._r8
	uni = 0._r8
	end if

	fi(k) = g*rho(i,k)*umi
	fni(k) = g*rho(i,k)*uni
	fc(k) = g*rho(i,k)*umc
	fnc(k) = g*rho(i,k)*unc

! calculate number of split time steps to ensure courant stability criteria
! for sedimentation calculations

	rgvm = max(fi(k),fc(k),fni(k),fnc(k))
	nstep = max(int(rgvm*deltat/pdel(i,k)+1._r8),nstep)

! redefine dummy variables - sedimentation is calculated over grid-scale
! quantities to ensure conservation

        dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
        dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat),0._r8)
        dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat),0._r8)

	if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
	if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8

	end do       !!! vertical loop

	do n = 1,nstep  !! loop over sub-time step to ensure stability	
	
	do k = 1,pver
	falouti(k) = fi(k)*dumi(i,k)
	faloutni(k) = fni(k)*dumni(i,k)
	faloutc(k) = fc(k)*dumc(i,k)
	faloutnc(k) = fnc(k)*dumnc(i,k)
	end do

! top of model

	k = 1
	faltndi = falouti(k)/pdel(i,k)
	faltndni = faloutni(k)/pdel(i,k)
	faltndc = faloutc(k)/pdel(i,k)
        faltndnc = faloutnc(k)/pdel(i,k)

! add fallout terms to microphysical tendencies	

	qitend(i,k) = qitend(i,k)-faltndi/nstep
	nitend(i,k) = nitend(i,k)-faltndni/nstep
	qctend(i,k) = qctend(i,k)-faltndc/nstep
        nctend(i,k) = nctend(i,k)-faltndnc/nstep

! sedimentation tendencies for output
	qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
	qisedten(i,k)=qisedten(i,k)-faltndi/nstep

	dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
	dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
	dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep

	do k = 2,pver

! for cloud liquid and ice, if cloud fraction increases with height
! then add flux from above to both vapor and cloud water of current level
! this means that flux entering clear portion of cell from above evaporates
! instantly

        dum=lcldm(i,k)/lcldm(i,k-1)
	dum=min(dum,1._r8)
        dum1=icldm(i,k)/icldm(i,k-1)
	dum1=min(dum1,1._r8)

	faltndqie=(falouti(k)-falouti(k-1))/pdel(i,k)
	faltndi=(falouti(k)-dum1*falouti(k-1))/pdel(i,k)
	faltndni=(faloutni(k)-dum1*faloutni(k-1))/pdel(i,k)
	faltndqce=(faloutc(k)-faloutc(k-1))/pdel(i,k)
	faltndc=(faloutc(k)-dum*faloutc(k-1))/pdel(i,k)
	faltndnc=(faloutnc(k)-dum*faloutnc(k-1))/pdel(i,k)

! add fallout terms to eulerian tendencies

	qitend(i,k) = qitend(i,k)-faltndi/nstep
	nitend(i,k) = nitend(i,k)-faltndni/nstep
	qctend(i,k) = qctend(i,k)-faltndc/nstep
        nctend(i,k) = nctend(i,k)-faltndnc/nstep

! sedimentation tendencies for output
	qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
	qisedten(i,k)=qisedten(i,k)-faltndi/nstep
	
! add terms to to evap/sub of cloud water

        qvlat(i,k)=qvlat(i,k)-(faltndqie-faltndi)/nstep
! for output
        qisevap(i,k)=qisevap(i,k)-(faltndqie-faltndi)/nstep
        qvlat(i,k)=qvlat(i,k)-(faltndqce-faltndc)/nstep
! for output
        qcsevap(i,k)=qcsevap(i,k)-(faltndqce-faltndc)/nstep

	tlat(i,k)=tlat(i,k)+(faltndqie-faltndi)*xxls/nstep
	tlat(i,k)=tlat(i,k)+(faltndqce-faltndc)*xxlv/nstep

	dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
	dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
	dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
        dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep

        Fni(K)=MAX(Fni(K)/pdel(i,K),Fni(K-1)/pdel(i,K-1))*pdel(i,K)
        FI(K)=MAX(FI(K)/pdel(i,K),FI(K-1)/pdel(i,K-1))*pdel(i,K)
        fnc(k)=max(fnc(k)/pdel(i,k),fnc(k-1)/pdel(i,k-1))*pdel(i,k)
        Fc(K)=MAX(Fc(K)/pdel(i,K),Fc(K-1)/pdel(i,K-1))*pdel(i,K)

	end do   !! k loop

! units below are m/s
! cloud water/ice sedimentation flux at surface 
! is added to precip flux at surface to get total precip (cloud + precip water)
! rate

	  prect(i) = prect(i)+(faloutc(pver)+falouti(pver)) &
                     /g/nstep/1000._r8	
	  preci(i) = preci(i)+(falouti(pver)) &
                     /g/nstep/1000._r8

        end do   !! nstep loop

! end sedimentation
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! get new update for variables that includes sedimentation tendency
! note : here dum variables are grid-average, NOT in-cloud

  do k=1,pver	

        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)

	if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
	if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
	
! calculate instantaneous processes (melting, homogeneous freezing)

        if (t(i,k)+tlat(i,k)/cpp*deltat > tmelt) then
           if (dumi(i,k) > 0._r8) then

! limit so that melting does not push temperature below freezing
	      dum = -dumi(i,k)*xlf/cpp
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.tmelt) then
	       dum = (t(i,k)+tlat(i,k)/cpp*deltat-tmelt)*cpp/xlf
	       dum = dum/dumi(i,k)*xlf/cpp 
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
	       dum = 1._r8
	      end if

              qctend(i,k)=qctend(i,k)+dum*dumi(i,k)/deltat

! for output
              melto(i,k)=dum*dumi(i,k)/deltat

! assume melting ice produces droplet
! mean volume radius of 8 micron

	      nctend(i,k)=nctend(i,k)+3._r8*dum*dumi(i,k)/deltat/ &
               (4._r8*pi*5.12e-16_r8*rhow)

              qitend(i,k)=((1._r8-dum)*dumi(i,k)-qi(i,k))/deltat
              nitend(i,k)=((1._r8-dum)*dumni(i,k)-ni(i,k))/deltat
              tlat(i,k)=tlat(i,k)-xlf*dum*dumi(i,k)/deltat
           end if
        end if

! homogeneously freeze droplets at -40 C

        if (t(i,k)+tlat(i,k)/cpp*deltat < 233.15_r8) then
           if (dumc(i,k) > 0._r8) then

! limit so that freezing does not push temperature above threshold
	      dum = dumc(i,k)*xlf/cpp
	      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.233.15_r8) then
	       dum = -(t(i,k)+tlat(i,k)/cpp*deltat-233.15_r8)*cpp/xlf
	       dum = dum/dumc(i,k)*xlf/cpp
	       dum = max(0._r8,dum)
	       dum = min(1._r8,dum)
	      else
	       dum = 1._r8
	      end if

              qitend(i,k)=qitend(i,k)+dum*dumc(i,k)/deltat
! for output
              homoo(i,k)=dum*dumc(i,k)/deltat

! assume 25 micron mean volume radius of homogeneously frozen droplets
! consistent with size of detrained ice in stratiform.F90
	      nitend(i,k)=nitend(i,k)+dum*3._r8*dumc(i,k)/(4._r8*3.14_r8*1.563e-14_r8* &
                 500._r8)/deltat
              qctend(i,k)=((1._r8-dum)*dumc(i,k)-qc(i,k))/deltat
              nctend(i,k)=((1._r8-dum)*dumnc(i,k)-nc(i,k))/deltat
              tlat(i,k)=tlat(i,k)+xlf*dum*dumc(i,k)/deltat
           end if
        end if

! remove any excess over-saturation, which is possible due to non-linearity when adding 
! together all microphysical processes
! follow code similar to old CAM scheme

	qtmp=q(i,k)+qvlat(i,k)*deltat
	ttmp=t(i,k)+tlat(i,k)/cpp*deltat

        esn = polysvp(ttmp,0)  ! use rhw to allow ice supersaturation
	qsn = min(epsqs*esn/(p(i,k)-(1._r8-epsqs)*esn),1._r8)

	if (qtmp > qsn .and. qsn > 0) then
! expression below is approximate since there may be ice deposition
	dum = (qtmp-qsn)/(1._r8+cons27*qsn/(cpp*rv*ttmp**2))/deltat
! add to output cme
	cmeout(i,k) = cmeout(i,k)+dum
! now add to tendencies, partition between liquid and ice based on temperature
           if (ttmp > 268.15_r8) then
              dum1=0.0_r8
! now add to tendencies, partition between liquid and ice based on te
           else if (ttmp < 238.15_r8) then
              dum1=1.0_r8
           else
              dum1=(268.15_r8-ttmp)/30._r8
	   end if  

	dum = (qtmp-qsn)/(1._r8+(xxls*dum1+xxlv*(1._r8-dum1))**2 &
                     *qsn/(cpp*rv*ttmp**2))/deltat
	qctend(i,k)=qctend(i,k)+dum*(1._r8-dum1)
! for output
        qcreso(i,k)=dum*(1._r8-dum1)
	qitend(i,k)=qitend(i,k)+dum*dum1
	qireso(i,k)=dum*dum1
	qvlat(i,k)=qvlat(i,k)-dum
! for output
        qvres(i,k)=-dum
	tlat(i,k)=tlat(i,k)+dum*(1._r8-dum1)*xxlv+dum*dum1*xxls
	end if

!...............................................................................
! calculate effective radius for pass to radiation code
! if no cloud water, default value is 10 micron for droplets,
! 25 micron for cloud ice

! update cloud variables after instantaneous processes to get effective radius
! variables are in-cloud to calculate size dist parameters

        dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)/icldm(i,k)
        dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)/lcldm(i,k)
        dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)/icldm(i,k)

! limit in-cloud mixing ratio to reasonable value of 5 g kg-1

	dumc(i,k)=min(dumc(i,k),5.e-3_r8)
	dumi(i,k)=min(dumi(i,k),5.e-3_r8)

!...................
! cloud ice effective radius

	if (dumi(i,k).ge.qsmall) then
! add upper limit to in-cloud number concentration to prevent numerical error
	dumni(i,k)=min(dumni(i,k),dumi(i,k)*1.e20_r8)
	lami(k) = (cons1*ci* &
              dumni(i,k)/dumi(i,k))**(1._r8/di)

	if (lami(k).lt.lammini) then
	lami(k) = lammini
	n0i(k) = lami(k)**(di+1._r8)*dumi(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)
! adjust number conc if needed to keep mean size in reasonable range
	nitend(i,k)=(niic(i,k)*icldm(i,k)-ni(i,k))/deltat
	else if (lami(k).gt.lammaxi) then
	lami(k) = lammaxi
	n0i(k) = lami(k)**(di+1._r8)*dumi(i,k)/(ci*cons1)
	niic(i,k) = n0i(k)/lami(k)
! adjust number conc if needed to keep mean size in reasonable range
	nitend(i,k)=(niic(i,k)*icldm(i,k)-ni(i,k))/deltat
	end if
	   effi(i,k) = 1.5_r8/lami(k)*1.e6_r8

        else
	   effi(i,k) = 25._r8
        end if

!...................
! cloud droplet effective radius

	if (dumc(i,k).ge.qsmall) then

! add upper limit to in-cloud number concentration to prevent numerical error
           dumnc(i,k)=min(dumnc(i,k),dumc(i,k)*1.e20_r8)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! set tendency to ensure minimum droplet concentration
! after update by microphysics, except when lambda exceeds bounds on mean drop
! size or if there is no cloud water
           if (dumnc(i,k).lt.cdnl/rho(i,k)) then   
              nctend(i,k)=(cdnl/rho(i,k)*lcldm(i,k)-nc(i,k))/deltat   
           end if
           dumnc(i,k)=max(dumnc(i,k),cdnl/rho(i,k)) ! sghan minimum in #/cm3 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
           pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
           pgam(k)=1._r8/(pgam(k)**2)-1._r8
           pgam(k)=max(pgam(k),2._r8)
           pgam(k)=min(pgam(k),15._r8)
           
           lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                     (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
           lammin = (pgam(k)+1._r8)/50.e-6_r8
           lammax = (pgam(k)+1._r8)/2.e-6_r8
           if (lamc(k).lt.lammin) then
              lamc(k) = lammin
              ncic(i,k) = 6._r8*lamc(k)**3*dumc(i,k)* &
                   gamma(pgam(k)+1._r8)/ &
                   (pi*rhow*gamma(pgam(k)+4._r8))
! adjust number conc if needed to keep mean size in reasonable range
              nctend(i,k)=(ncic(i,k)*lcldm(i,k)-nc(i,k))/deltat

           else if (lamc(k).gt.lammax) then
              lamc(k) = lammax
              ncic(i,k) = 6._r8*lamc(k)**3*dumc(i,k)* &
                   gamma(pgam(k)+1._r8)/ &
                   (pi*rhow*gamma(pgam(k)+4._r8))
! adjust number conc if needed to keep mean size in reasonable range
              nctend(i,k)=(ncic(i,k)*lcldm(i,k)-nc(i,k))/deltat
	end if

        effc(i,k) = &
             gamma(pgam(k)+4._r8)/ &
             gamma(pgam(k)+3._r8)/lamc(k)/2._r8*1.e6_r8
!assign output fields for shape here
        lamcrad(i,k)=lamc(k)
        pgamrad(i,k)=pgam(k)

        else
           effc(i,k) = 10._r8
           lamcrad(i,k)=0._r8
           pgamrad(i,k)=0._r8
        end if

! ice effective diameter for david mitchell's optics
        deffi(i,k)=effi(i,k)*rhoi/917._r8*2._r8


!!! recalculate effective radius for constant number, in order to separate
! first and second indirect effects
! assume constant number of 10^8 kg-1

	dumnc(i,k)=1.e8

	if (dumc(i,k).ge.qsmall) then
         pgam(k)=0.0005714_r8*(ncic(i,k)/1.e6_r8*rho(i,k))+0.2714_r8
         pgam(k)=1._r8/(pgam(k)**2)-1._r8
         pgam(k)=max(pgam(k),2._r8)
         pgam(k)=min(pgam(k),15._r8)

	lamc(k) = (pi/6._r8*rhow*dumnc(i,k)*gamma(pgam(k)+4._r8)/ &
                 (dumc(i,k)*gamma(pgam(k)+1._r8)))**(1._r8/3._r8)
	lammin = (pgam(k)+1._r8)/50.e-6_r8
	lammax = (pgam(k)+1._r8)/2.e-6_r8
	if (lamc(k).lt.lammin) then
	lamc(k) = lammin
	else if (lamc(k).gt.lammax) then
	lamc(k) = lammax
	end if
	effc_fn(i,k) = &
             gamma(pgam(k)+4._r8)/ &
             gamma(pgam(k)+3._r8)/lamc(k)/2._r8*1.e6_r8

        else
	effc_fn(i,k) = 10._r8
        end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1!

	end do ! vertical k loop

 500    continue

	do k=1,pver
! if updated q (after microphysics) is zero, then ensure updated n is also zero

        if (qc(i,k)+qctend(i,k)*deltat.lt.qsmall) nctend(i,k)=-nc(i,k)/deltat
        if (qi(i,k)+qitend(i,k)*deltat.lt.qsmall) nitend(i,k)=-ni(i,k)/deltat
	end do

	end do ! i loop


! hm add rain/snow mixing ratio and number concentration as diagnostic

      call outfld('QRAIN',qrout,   pcols, lchnk)
      call outfld('QSNOW',qsout,   pcols, lchnk)
      call outfld('NRAIN',nrout,   pcols, lchnk)
      call outfld('NSNOW',nsout,   pcols, lchnk)

! add snow ouptut
      do i = 1,ncol
         do k=1,pver
            if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
               dsout(i,k)=3._r8*rhosn/917._r8*(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)
            endif
         end do
      end do
      call outfld('DSNOW',dsout,   pcols, lchnk)
 
!calculate effective radius of rain and snow in microns for COSP using Eq. 9 of COSP v1.3 manual
      do i = 1,ncol
         do k=1,pver
	   !! RAIN
            if (qrout(i,k).gt.1.e-7_r8.and.nrout(i,k).gt.0._r8) then
		reff_rain(i,k)=1.5_r8*(pi * rhow * nrout(i,k)/qrout(i,k))**(-1._r8/3._r8)*1.e6_r8 
            endif
	    !! SNOW
            if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
		reff_snow(i,k)=1.5_r8*(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)*1.e6_r8 

            endif
         end do
      end do

      ! analytic radar reflectivity
      ! formulas from Matthew Shupe, NOAA/CERES
      ! *****note: radar reflectivity is local (in-precip average)
      ! units of mm^6/m^3
 
      do i = 1,ncol
         do k=1,pver
            if (qc(i,k)+qctend(i,k)*deltat.ge.qsmall) then
               dum=((qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)*1000._r8)**2 &
                  /(0.109_r8*(nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)/1.e6_r8)*lcldm(i,k)/cldmax(i,k)
            else
               dum=0._r8
            end if
            if (qi(i,k)+qitend(i,k)*deltat.ge.qsmall) then
               dum1=((qi(i,k)+qitend(i,k)*deltat)*rho(i,k)/icldm(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)*icldm(i,k)/cldmax(i,k)
            else 
               dum1=0._r8
            end if
         
            if (qsout(i,k).ge.qsmall) then
               dum1=dum1+(qsout(i,k)*rho(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)
            end if
            
            refl(i,k)=dum+dum1
 
            ! add rain rate, but for 37 GHz formulation instead of 94 GHz
            ! formula approximated from data of Matrasov (2007)
            ! rainrt is the rain rate in mm/hr
            ! reflectivity (dum) is in DBz
 
            if (rainrt(i,k).ge.0.001) then
               dum=log10(rainrt(i,k)**6._r8)+16._r8
 
               ! convert from DBz to mm^6/m^3
 
               dum = 10._r8**(dum/10._r8)
            else
               ! don't include rain rate in R calculation for values less than 0.001 mm/hr
               dum=0.
            end if
 
            ! add to refl
 
            refl(i,k)=refl(i,k)+dum
 
            !output reflectivity in Z.
            areflz(i,k)=refl(i,k)
 
            ! convert back to DBz 
 
            if (refl(i,k).gt.minrefl) then 
               refl(i,k)=10._r8*log10(refl(i,k))
            else	
               refl(i,k)=-9999._r8
            end if
  
            !set averaging flag
            if (refl(i,k).gt.mindbz) then 
               arefl(i,k)=refl(i,k)
               frefl(i,k)=1.0_r8  
            else
               arefl(i,k)=0._r8
               areflz(i,k)=0._r8
               frefl(i,k)=0._r8
            end if
 
            ! bound cloudsat reflectivity
 
            csrfl(i,k)=min(csmax,refl(i,k))
 
            !set averaging flag
            if (csrfl(i,k).gt.csmin) then 
               acsrfl(i,k)=refl(i,k)
               fcsrfl(i,k)=1.0_r8  
            else
               acsrfl(i,k)=0._r8
               fcsrfl(i,k)=0._r8
            end if
 
         end do
      end do
       
      call outfld('REFL',refl,   pcols, lchnk)
      call outfld('AREFL',arefl,   pcols, lchnk)
      call outfld('AREFLZ',areflz,   pcols, lchnk)
      call outfld('FREFL',frefl,   pcols, lchnk)
      call outfld('CSRFL',csrfl,   pcols, lchnk)
      call outfld('ACSRFL',acsrfl,   pcols, lchnk)
      call outfld('FCSRFL',fcsrfl,   pcols, lchnk)

      call outfld('RERCLD',rercld,   pcols, lchnk)

! averaging for snow and rain number and diameter

  qrout2(:,:)=0._r8
  qsout2(:,:)=0._r8
  nrout2(:,:)=0._r8
  nsout2(:,:)=0._r8	
  drout2(:,:)=0._r8
  dsout2(:,:)=0._r8	
  freqs(:,:)=0._r8
  freqr(:,:)=0._r8
  do i = 1,ncol
     do k=1,pver
	if (qrout(i,k).gt.1.e-7_r8.and.nrout(i,k).gt.0._r8) then
 	   qrout2(i,k)=qrout(i,k)
	   nrout2(i,k)=nrout(i,k)
	   drout2(i,k)=(pi * rhow * nrout(i,k)/qrout(i,k))**(-1._r8/3._r8)
	   freqr(i,k)=1._r8
	endif
	if (qsout(i,k).gt.1.e-7_r8.and.nsout(i,k).gt.0._r8) then
 	   qsout2(i,k)=qsout(i,k)
	   nsout2(i,k)=nsout(i,k)
	   dsout2(i,k)=(pi * rhosn * nsout(i,k)/qsout(i,k))**(-1._r8/3._r8)
	   freqs(i,k)=1._r8
	endif
     end do
  end do

! output activated liquid and ice (convert from #/kg -> #/m3)
  do i = 1,ncol
     do k=1,pver
        ncai(i,k)=dum2i(i,k)*rho(i,k)
        ncal(i,k)=dum2l(i,k)*rho(i,k)
     end do
  end do
  call outfld('NCAL',ncal,    pcols,lchnk)
  call outfld('NCAI',ncai,    pcols,lchnk)

!add averaged output fields.
  call outfld('AQRAIN',qrout2,    pcols,lchnk)
  call outfld('AQSNOW',qsout2,    pcols,lchnk)
  call outfld('ANRAIN',nrout2,    pcols,lchnk)
  call outfld('ANSNOW',nsout2,    pcols,lchnk)
  call outfld('ADRAIN',drout2,    pcols,lchnk)
  call outfld('ADSNOW',dsout2,    pcols,lchnk)
  call outfld('FREQR',freqr,    pcols,lchnk)
  call outfld('FREQS',freqs,    pcols,lchnk)

!redefine fice here....
	nfice(:,:)=0._r8
	do k=1,pver
	   do i=1,ncol
        	dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
        	dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
		dumfice=qsout(i,k) + qrout(i,k) + dumc(i,k) + dumi(i,k)  

                if (dumfice.gt.qsmall.and.(qsout(i,k)+dumi(i,k).gt.qsmall)) then
                  nfice(i,k)=(qsout(i,k) + dumi(i,k))/dumfice
                endif

                if (nfice(i,k).gt.1._r8) then
                   nfice(i,k)=1._r8
                endif

	   enddo
	enddo

      call outfld('FICE',nfice,   pcols, lchnk)

return
end subroutine mmicro_pcond

!
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      FUNCTION GAMMA(X)

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

      real(r8) gamma
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
  900 GAMMA=RES
!D900 DGAMMA = RES
      RETURN
! ---------- LAST LINE OF GAMMA ----------
      END function gamma


end module cldwat2m_micro
