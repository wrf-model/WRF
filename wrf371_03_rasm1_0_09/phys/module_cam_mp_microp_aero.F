#define WRF_PORT
#if ( WRF_CHEM == 1 )
#       include "../chem/MODAL_AERO_CPP_DEFINES.h"
#else
#       define MODAL_AERO
#       define MODAL_AERO_3MODE
#endif
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
module microp_aero

!---------------------------------------------------------------------------------
! Purpose:
!   CAM Interface for aerosol activation 
!
! Author: Andrew Gettelman
! Based on code from: Hugh Morrison, Xiaohong Liu and Steve Ghan
! May 2010
! Description in: Morrison and Gettelman, 2008. J. Climate (MG2008)
!                 Gettelman et al., 2010 J. Geophys. Res. - Atmospheres (G2010)         
! for questions contact Andrew Gettelman  (andrew@ucar.edu)
! Modifications: A. Gettelman Nov 2010  - changed to support separation of 
!                microphysics and macrophysics and concentrate aerosol information here
!
!---------------------------------------------------------------------------------

  use shr_kind_mod,  only: r8=>shr_kind_r8
#ifndef WRF_PORT
  use spmd_utils,    only: masterproc
  use ppgrid,        only: pcols, pver, pverp
#else
  use module_cam_support, only: masterproc, pcols, pver, pverp
#endif
  use physconst,     only: gravit, rair, tmelt, cpair, rh2o, r_universal, mwh2o, rhoh2o
  use physconst,     only: latvap, latice
#ifndef WRF_PORT
  use abortutils,    only: endrun
#else
   use module_cam_support, only: endrun
#endif
  use error_function, only: erf,erfc
  use wv_saturation,  only: estblf, hlatv, tmin, hlatf, rgasv, pcf, cp, epsqs, ttrice, &
                            vqsatd2, vqsatd2_single,polysvp
#ifndef WRF_PORT
  use cam_history,    only: addfld, add_default, phys_decomp, outfld 
  use cam_logfile,    only: iulog
  use rad_constituents, only: rad_cnst_get_info, rad_cnst_get_aer_props
  use phys_control,   only: phys_getopts
  use cldwat2m_macro, only: rhmini, rhmaxi
#else
  use module_cam_support, only: addfld, add_default, phys_decomp, outfld, iulog
#endif


 implicit none
 private
 save

 public :: ini_microp_aero, microp_aero_ts

! Private module data

   logical             :: wsubTKE  ! If .true. (.false.), use UW's TKE (kkvh) for computing wsub


      real(r8), private:: rn_dst1, rn_dst2, rn_dst3, rn_dst4 !dust number mean radius for contact freezing
      real(r8), private:: pi    ! pi
      real(r8), private:: tt0   ! Freezing temperature
      real(r8), private:: qsmall !min mixing ratio 

  real(r8), private::  r              !Dry air Gas constant


! activate parameters

      integer, private:: psat
      parameter (psat=6) ! number of supersaturations to calc ccn concentration
      real(r8), private:: aten

      real(r8), allocatable, private:: alogsig(:) ! natl log of geometric standard dev of aerosol
      real(r8), allocatable, private:: exp45logsig(:)
      real(r8), allocatable, private:: argfactor(:)
      real(r8), allocatable, private:: amcube(:) ! cube of dry mode radius (m)
      real(r8), allocatable, private:: smcrit(:) ! critical supersatuation for activation
      real(r8), allocatable, private:: lnsm(:) ! ln(smcrit)
      real(r8), private:: amcubesulfate(pcols) ! cube of dry mode radius (m) of sulfate
      real(r8), private:: smcritsulfate(pcols) ! critical supersatuation for activation of sulfate
      real(r8), allocatable, private:: amcubefactor(:) ! factors for calculating mode radius
      real(r8), allocatable, private:: smcritfactor(:) ! factors for calculating critical supersaturation
      real(r8), private:: super(psat)
      real(r8), private:: alogten,alog2,alog3,alogaten
      real(r8), private, parameter :: supersat(psat)= &! supersaturation (%) to determine ccn concentration
               (/0.02,0.05,0.1,0.2,0.5,1.0/)
      real(r8), allocatable, private:: ccnfact(:,:)

      real(r8), allocatable, private:: f1(:),f2(:) ! abdul-razzak functions of width
      real(r8), private:: third, sixth,zero
      real(r8), private:: sq2, sqpi

      integer :: naer_all    ! number of aerosols affecting climate
      integer :: idxsul = -1 ! index in aerosol list for sulfate
      integer :: idxdst1 = -1 ! index in aerosol list for dust1
      integer :: idxdst2 = -1 ! index in aerosol list for dust2
      integer :: idxdst3 = -1 ! index in aerosol list for dust3
      integer :: idxdst4 = -1 ! index in aerosol list for dust4

      integer :: idxbcphi = -1 ! index in aerosol list for Soot (BCPHIL)

      ! aerosol properties
      character(len=20), allocatable :: aername(:)
      real(r8), allocatable :: dryrad_aer(:)
      real(r8), allocatable :: density_aer(:)
      real(r8), allocatable :: hygro_aer(:)
      real(r8), allocatable :: dispersion_aer(:)
      real(r8), allocatable :: num_to_mass_aer(:)

contains

!===============================================================================

subroutine ini_microp_aero

!----------------------------------------------------------------------- 
! 
! Purpose: 
! initialize constants for aerosols needed by microphysics
! called from stratiform.F90
! 
! Author: Andrew Gettelman May 2010
! 
!-----------------------------------------------------------------------
#ifdef MODAL_AERO
    use ndrop,           only: activate_init
    use constituents,    only: cnst_name
#ifndef WRF_PORT
    use cam_history,     only: fieldname_len
    use spmd_utils,      only: masterproc
    use modal_aero_data, only: cnst_name_cw, &
                               lmassptr_amode, lmassptrcw_amode, &
                               nspec_amode, ntot_amode, numptr_amode, numptrcw_amode, ntot_amode
#else
    use module_cam_support, only: fieldname_len, masterproc
    use modal_aero_data,    only: cnst_name_cw => cnst_name_cw_mp, &
         lmassptr_amode => lmassptr_amode_mp, lmassptrcw_amode => lmassptrcw_amode_mp , &
         nspec_amode    => nspec_amode_mp   , ntot_amode       => ntot_amode_mp,        &
         numptr_amode   => numptr_amode_mp  , numptrcw_amode   => numptrcw_amode_mp
#endif
      
    integer                        :: lphase, lspec
    character(len=fieldname_len)   :: tmpname
    character(len=fieldname_len+3) :: fieldname
    character(128)                 :: long_name
    character(8)                   :: unit
    logical                        :: history_aerosol      ! Output the MAM aerosol tendencies

#endif

   integer k

   integer l,m, iaer
   real(r8) surften       ! surface tension of water w/respect to air (N/m)
   real(r8) arg
   real(r8) derf

   character(len=16) :: eddy_scheme = ' '
   logical           :: history_microphysics     ! output variables for microphysics diagnostics package
#ifndef WRF_PORT
   ! Query the PBL eddy scheme
   call phys_getopts(eddy_scheme_out              = eddy_scheme,           &
                     history_microphysics_out     = history_microphysics   )
   wsubTKE = .false.
   if (trim(eddy_scheme) .eq. 'diag_TKE') wsubTKE = .true.
#else
   history_microphysics =.FALSE.
   wsubTKE              =.TRUE.
#endif

   ! Access the physical properties of the aerosols that are affecting the climate
   ! by using routines from the rad_constituents module.
#ifndef WRF_PORT
   call rad_cnst_get_info(0, naero=naer_all)
   allocate( &
      aername(naer_all),        &
      dryrad_aer(naer_all),     &
      density_aer(naer_all),    &
      hygro_aer(naer_all),      &
      dispersion_aer(naer_all), &
      num_to_mass_aer(naer_all) )

   do iaer = 1, naer_all
      call rad_cnst_get_aer_props(0, iaer, &
         aername         = aername(iaer), &
         dryrad_aer      = dryrad_aer(iaer), &
         density_aer     = density_aer(iaer), &
         hygro_aer       = hygro_aer(iaer), &
         dispersion_aer  = dispersion_aer(iaer), &
         num_to_mass_aer = num_to_mass_aer(iaer) )


      ! Look for sulfate aerosol in this list (Bulk aerosol only)
      if (trim(aername(iaer)) == 'SULFATE') idxsul = iaer
      if (trim(aername(iaer)) == 'DUST1') idxdst1 = iaer
      if (trim(aername(iaer)) == 'DUST2') idxdst2 = iaer
      if (trim(aername(iaer)) == 'DUST3') idxdst3 = iaer
      if (trim(aername(iaer)) == 'DUST4') idxdst4 = iaer
      if (trim(aername(iaer)) == 'BCPHIL') idxbcphi = iaer


!addfield calls for outputting aerosol number concentration

      call addfld(aername(iaer)//'_m3', 'm-3', pver, 'A', 'aerosol number concentration', phys_decomp)

   end do

   if (masterproc) then
      write(iulog,*) 'ini_micro: iaer, name, dryrad, density, hygro, dispersion, num_to_mass'
#ifdef WRF_PORT
        call wrf_message(iulog)
#endif 
      do iaer = 1, naer_all
         write(iulog,*) iaer, aername(iaer), dryrad_aer(iaer), density_aer(iaer), hygro_aer(iaer), &
            dispersion_aer(iaer), num_to_mass_aer(iaer)
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
      end do
      if (idxsul < 1) then
         write(iulog,*) 'ini_micro: SULFATE aerosol properties NOT FOUND'
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif          
      else
         write(iulog,*) 'ini_micro: SULFATE aerosol properties FOUND at index ',idxsul
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
      end if
   end if
#endif
   call addfld ('CCN1    ','#/cm3   ',pver, 'A','CCN concentration at S=0.02%',phys_decomp)
   call addfld ('CCN2    ','#/cm3   ',pver, 'A','CCN concentration at S=0.05%',phys_decomp)
   call addfld ('CCN3    ','#/cm3   ',pver, 'A','CCN concentration at S=0.1%',phys_decomp)
   call addfld ('CCN4    ','#/cm3   ',pver, 'A','CCN concentration at S=0.2%',phys_decomp)
   call addfld ('CCN5    ','#/cm3   ',pver, 'A','CCN concentration at S=0.5%',phys_decomp)
   call addfld ('CCN6    ','#/cm3   ',pver, 'A','CCN concentration at S=1.0%',phys_decomp)

   call add_default('CCN3',  1, ' ' )

   call addfld ('NIHF    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to homogenous freezing',phys_decomp)
   call addfld ('NIDEP    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to deposition nucleation',phys_decomp)
   call addfld ('NIIMM    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to immersion freezing',phys_decomp)
   call addfld ('NIMEY    ','#/m3   ',pver, 'A','Activated Ice Number Concentation due to meyers deposition',phys_decomp)

! contact freezing due to dust
! dust number mean radius (m), Zender et al JGR 2003 assuming number mode radius of 0.6 micron, sigma=2

   rn_dst1=0.258e-6_r8
   rn_dst2=0.717e-6_r8
   rn_dst3=1.576e-6_r8
   rn_dst4=3.026e-6_r8

! smallest mixing ratio considered in microphysics

   qsmall = 1.e-18_r8  

! set parameters for droplet activation, following abdul-razzak and ghan 2000, JGR

!      mathematical constants

      zero=0._r8
      third=1./3._r8
      sixth=1./6._r8
      sq2=sqrt(2._r8)
      pi=4._r8*atan(1.0_r8)
      sqpi=sqrt(pi)

      r= rair        !Dry air Gas constant: note units(phys_constants are in J/K/kmol)

! freezing temperature
      tt0=273.15_r8  !should be tmelt

      surften=0.076_r8
      aten=2.*mwh2o*surften/(r_universal*tt0*rhoh2o)
      alogaten=log(aten)
      alog2=log(2._r8)
      alog3=log(3._r8)
      super(:)=0.01*supersat(:)
#ifndef WRF_PORT
      allocate(alogsig(naer_all),      &
               exp45logsig(naer_all),  &
               argfactor(naer_all),    &
               amcube(naer_all),       &
               smcrit(naer_all),       &
               lnsm(naer_all),         &
               amcubefactor(naer_all), &
               smcritfactor(naer_all), &
               ccnfact(psat,naer_all), &
               f1(naer_all),           &
               f2(naer_all)            )

      do m=1,naer_all
          alogsig(m)=log(dispersion_aer(m))
          exp45logsig(m)=exp(4.5*alogsig(m)*alogsig(m))
	  argfactor(m)=2./(3.*sqrt(2.)*alogsig(m))
          f1(m)=0.5*exp(2.5*alogsig(m)*alogsig(m))
	  f2(m)=1.+0.25*alogsig(m)
          amcubefactor(m)=3._r8/(4._r8*pi*exp45logsig(m)*density_aer(m))
	  smcritfactor(m)=2._r8*aten*sqrt(aten/(27._r8*max(1.e-10_r8,hygro_aer(m))))
          amcube(m)=amcubefactor(m)/(num_to_mass_aer(m))
          if(hygro_aer(m).gt.1.e-10) then
             smcrit(m)=smcritfactor(m)/sqrt(amcube(m))
	  else
             smcrit(m)=100.
	  endif
          lnsm(m)=log(smcrit(m))

          do l=1,psat
             arg=argfactor(m)*log(smcrit(m)/super(l))
	     if(arg<2)then
	        if(arg<-2)then
		   ccnfact(l,m)=1.e-6
	        else
		   ccnfact(l,m)=1.e-6_r8*0.5_r8*erfc(arg)
                endif
             else
	        ccnfact(l,m) = 0.
             endif
          enddo
      end do

#ifdef MODAL_AERO
 call phys_getopts( history_aerosol_out        = history_aerosol)
 call activate_init

! Add dropmixnuc tendencies for all modal aerosol species
    do m = 1, ntot_amode
    do lphase = 1, 2
    do lspec = 0, nspec_amode(m)+1   ! loop over number + chem constituents + water
       unit = 'kg/m2/s'
       if (lspec == 0) then   ! number
          unit = '#/m2/s'
          if (lphase == 1) then
             l = numptr_amode(m)
          else
             l = numptrcw_amode(m)
          endif
       else if (lspec <= nspec_amode(m)) then   ! non-water mass
          if (lphase == 1) then
             l = lmassptr_amode(lspec,m)
          else
             l = lmassptrcw_amode(lspec,m)
          endif
       else   ! water mass
          cycle
       end if
       if (lphase == 1) then
          tmpname = cnst_name(l)
       else
          tmpname = cnst_name_cw(l)
       end if

       fieldname = trim(tmpname) // '_mixnuc1'
       long_name = trim(tmpname) // ' dropmixnuc mixnuc column tendency'
       call addfld( fieldname, unit, 1, 'A', long_name, phys_decomp )
       if ( history_aerosol ) then
          call add_default( fieldname, 1, ' ' )
          if ( masterproc ) write(*,'(2a)') 'microp_driver_init addfld - ', fieldname
       endif

    end do   ! lspec
    end do   ! lphase
    end do   ! m
#endif
#endif
 return
 end subroutine ini_microp_aero


!===============================================================================
!activation routine for each timestep goes here...

      subroutine microp_aero_ts (   &
   lchnk, ncol, deltatin, t, ttend,        &
   qn, qc, qi,               &
   nc, ni, p, pdel, cldn,                   &
   liqcldf, icecldf,                        &
   cldo, pint, rpdel, zm, omega,            &
#ifdef MODAL_AERO
   qaer, cflx, qaertend, dgnumwet,dgnum, &
#else
   aer_mmr,                                 &
#endif
   kkvh, tke, turbtype, smaw, wsub, wsubi, &
   naai, naai_hom, npccn, rndst, nacon     &
#ifdef WRF_PORT
   ,qqcw                                   &
#endif
                                           )

   use wv_saturation, only: vqsatd, vqsatd_water
#ifndef WRF_PORT
   use constituents,  only: pcnst
#else
   use module_cam_support, only: pcnst => pcnst_mp
#endif
#ifdef MODAL_AERO
   use ndrop,         only: dropmixnuc
#ifndef WRF_PORT
   use modal_aero_data, only:numptr_amode, modeptr_accum, modeptr_coarse, modeptr_aitken, &
                              lptr_dust_a_amode,lptr_nacl_a_amode,ntot_amode
#else
   use modal_aero_data, only:numptr_amode => numptr_amode_mp, modeptr_accum => modeptr_accum_mp, &
        modeptr_coarse    => modeptr_coarse_mp   , modeptr_aitken    => modeptr_aitken_mp, &
        lptr_dust_a_amode => lptr_dust_a_amode_mp, lptr_nacl_a_amode => lptr_nacl_a_amode_mp, &
        ntot_amode        => ntot_amode_mp, modeptr_coardust => modeptr_coardust_mp  !BSINGH - Added modeptr_coarsedust for MAM7 compliance
     
#endif
#endif

   ! input arguments
   integer,  intent(in) :: lchnk
   integer,  intent(in) :: ncol
   real(r8), intent(in) :: deltatin             ! time step (s)
   real(r8), intent(in) :: t(pcols,pver)       ! input temperature (K)
   real(r8), intent(in) :: ttend(pcols,pver)    ! non-microphysical temperature tendency (K/s)
   real(r8), intent(in) :: qn(pcols,pver)       ! input h20 vapor mixing ratio (kg/kg)
   ! note: all input cloud variables are grid-averaged
   real(r8), intent(in) :: qc(pcols,pver)    ! cloud water mixing ratio (kg/kg)
   real(r8), intent(in) :: qi(pcols,pver)    ! cloud ice mixing ratio (kg/kg)
   real(r8), intent(in) :: nc(pcols,pver)    ! cloud water number conc (1/kg)
   real(r8), intent(in) :: ni(pcols,pver)    ! cloud ice number conc (1/kg)
   real(r8), intent(in) :: p(pcols,pver)        ! air pressure (pa)
   real(r8), intent(in) :: pdel(pcols,pver)     ! pressure difference across level (pa)
   real(r8), intent(in) :: cldn(pcols,pver)     ! cloud fraction
   real(r8), intent(in) :: icecldf(pcols,pver)  ! ice cloud fraction   
   real(r8), intent(in) :: liqcldf(pcols,pver)  ! liquid cloud fraction
   real(r8), intent(in) :: cldo(pcols,pver)  ! old cloud fraction
   real(r8), intent(in) :: pint(pcols,pverp)    ! air pressure layer interfaces (pa)
   real(r8), intent(in) :: rpdel(pcols,pver)    ! inverse pressure difference across level (pa)
   real(r8), intent(in) :: zm(pcols,pver)       ! geopotential height of model levels (m)
   real(r8), intent(in) :: omega(pcols,pver)    ! vertical velocity (Pa/s)
#ifdef MODAL_AERO
   real(r8), intent(in) :: qaer(pcols,pver,pcnst) ! aerosol number and mass mixing ratios
   real(r8), intent(in) :: cflx(pcols,pcnst)      ! constituent flux from surface
   real(r8), intent(inout) :: qaertend(pcols,pver,pcnst) ! qaer tendency (1/s)
#ifdef WRF_PORT
   real(r8), intent(inout) :: qqcw(pcols,pver,pcnst) ! cloud-borne aerosol
#endif
   real(r8), intent(in) :: dgnumwet(pcols,pver,ntot_amode) ! aerosol mode diameter
   real(r8), intent(in) :: dgnum(pcols,pver,ntot_amode) ! aerosol mode dry diameter
#else
   real(r8), intent(in) :: aer_mmr(:,:,:)       ! aerosol mass mixing ratio
#endif
   real(r8), intent(in) :: kkvh(pcols,pver+1) ! vertical eddy diff coef (m2 s-1)
   real(r8), intent(in) :: tke(pcols,pver+1)    ! TKE from the UW PBL scheme (m2 s-2)
   real(r8), intent(in) :: turbtype(pcols,pver+1) ! Turbulence type at each interface
   real(r8), intent(in) :: smaw(pcols,pver+1)     ! Normalized instability function of momentum. 0 <=  <= 4.964 and 1 at neutral condition.

   ! output arguments

   real(r8), intent(out) :: wsub(pcols,pver)    ! diagnosed sub-grid vertical velocity st. dev. (m/s)
   real(r8), intent(out) :: wsubi(pcols,pver)   ! diagnosed sub-grid vertical velocity ice (m/s)
   real(r8), intent(out) :: naai(pcols,pver)    ! number of activated aerosol for ice nucleation
   real(r8), intent(out) :: naai_hom(pcols,pver)! number of activated aerosol for ice nucleation (homogeneous freezing only)
   real(r8), intent(out) :: npccn(pcols,pver)   ! number of CCN (liquid activated)
   real(r8), intent(out) :: rndst(pcols,pver,4) ! radius of 4 dust bins for contact freezing (from microp_aero_ts)
   real(r8), intent(out) :: nacon(pcols,pver,4) ! number in 4 dust bins for contact freezing  (from microp_aero_ts)


! local workspace
! all units mks unless otherwise stated

   real(r8) :: tkem(pcols,pver)     ! Layer-mean TKE
   real(r8) :: smm(pcols,pver)      ! Layer-mean instability function
   real(r8) :: relhum(pcols,pver) ! relative humidity
   real(r8) :: cldm(pcols,pver)   ! cloud fraction
   real(r8) :: icldm(pcols,pver)   ! ice cloud fraction
   real(r8) :: lcldm(pcols,pver)   ! liq cloud fraction
   real(r8) :: nfice(pcols,pver) !fice variable
   real(r8) :: dumfice
   real(r8) :: qcld          ! total cloud water
        real(r8) :: lcldn(pcols,pver) ! fractional coverage of new liquid cloud
        real(r8) :: lcldo(pcols,pver) ! fractional coverage of old liquid cloud
        real(r8) :: nctend_mixnuc(pcols,pver)
   	real(r8) :: deltat        ! sub-time step (s)
        real(r8) :: nihf2,niimm2,nidep2,nimey2,dum2 !dummy variables for activated ice nuclei by diffferent processes
        real(r8) arg  !variable for CCN number (BAM)

	 integer ftrue  ! integer used to determine cloud depth
         real(r8) :: dum        ! temporary dummy variable

        real(r8) :: q(pcols,pver) ! water vapor mixing ratio (kg/kg)
!       real(r8) :: t(pcols,pver) ! temperature (K)
        real(r8) :: ncloc(pcols,pver) ! local variable for nc
        real(r8) :: rho(pcols,pver) ! air density (kg m-3)
        real(r8) :: mincld  ! minimum allowed cloud fraction

! used in contact freezing via dust particles
        real(r8)  tcnt, viscosity, mfp
        real(r8)  slip1, slip2, slip3, slip4
        real(r8)  dfaer1, dfaer2, dfaer3, dfaer4
        real(r8)  nacon1,nacon2,nacon3,nacon4

        real(r8) dmc,ssmc,dstrn  ! variables for modal scheme.

! returns from function/subroutine calls
        real(r8) :: tsp(pcols,pver)      ! saturation temp (K)
        real(r8) :: qsp(pcols,pver)      ! saturation mixing ratio (kg/kg)
        real(r8) :: qsphy(pcols,pver)      ! saturation mixing ratio (kg/kg): hybrid rh
        real(r8) :: qs(pcols)            ! liquid-ice weighted sat mixing rat (kg/kg)
        real(r8) :: es(pcols)            ! liquid-ice weighted sat vapor press (pa)
        real(r8) :: esl(pcols,pver)      ! liquid sat vapor pressure (pa)
        real(r8) :: esi(pcols,pver)      ! ice sat vapor pressure (pa)
        real(r8) :: gammas(pcols)        ! parameter for cond/evap of cloud water

! new aerosol variables
        real(r8), allocatable :: naermod(:) ! aerosol number concentration (/m3)
	real(r8), allocatable :: naer2(:,:,:)   ! new aerosol number concentration (/m3)

        real(r8), allocatable :: maerosol(:,:)   ! aerosol mass conc (kg/m3)
	real(r8) naer(pcols)
        real(r8) ccn(pcols,pver,psat)        ! number conc of aerosols activated at supersat
        character*8, parameter :: ccn_name(psat)=(/'CCN1','CCN2','CCN3','CCN4','CCN5','CCN6'/)

!output for ice nucleation
          real(r8) :: nimey(pcols,pver) !output number conc of ice nuclei due to meyers deposition (1/m3)
          real(r8) :: nihf(pcols,pver)  !output number conc of ice nuclei due to heterogenous freezing (1/m3)
          real(r8) :: nidep(pcols,pver) !output number conc of ice nuclei due to deoposion nucleation (hetero nuc) (1/m3)
          real(r8) :: niimm(pcols,pver) !output number conc of ice nuclei due to immersion freezing (hetero nuc) (1/m3)

! loop array variables
         integer i,k,nstep,n, l
	 integer ii,kk, m
         integer, allocatable :: ntype(:)

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

! initialize  output fields for ice nucleation
    nimey(1:ncol,1:pver)=0._r8 
    nihf(1:ncol,1:pver)=0._r8  
    nidep(1:ncol,1:pver)=0._r8 
    niimm(1:ncol,1:pver)=0._r8  

! initialize output
    naai(1:ncol,1:pver)=0._r8
    naai_hom(1:ncol,1:pver)=0._r8
    npccn(1:ncol,1:pver)=0._r8  
    nacon(1:ncol,1:pver,:)=0._r8

! set default or fixed dust bins for contact freezing
    rndst(1:ncol,1:pver,1)=rn_dst1
    rndst(1:ncol,1:pver,2)=rn_dst2
    rndst(1:ncol,1:pver,3)=rn_dst3
    rndst(1:ncol,1:pver,4)=rn_dst4
     
! TKE initialization
    tkem(1:ncol,1:pver)=0._r8    
    smm(1:ncol,1:pver)=0._r8    

    allocate(naermod(naer_all), &
      naer2(pcols,pver,naer_all), &
      maerosol(1,naer_all), &
      ntype(naer_all))

! assign variable deltat for sub-stepping...
	deltat=deltatin	

! initialize multi-level fields
        q(1:ncol,1:pver)=qn(1:ncol,1:pver)
        ncloc(1:ncol,1:pver)=nc(1:ncol,1:pver)

! parameters for scheme
	mincld=0.0001_r8

! initialize time-varying parameters

        do k=1,pver
           do i=1,ncol
              rho(i,k)=p(i,k)/(r*t(i,k))
           end do
        end do

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! More refined computation of sub-grid vertical velocity 
! Set to be zero at the surface by initialization.

        if( wsubTKE ) then

            do i = 1, ncol
            do k = 1, pver
               tkem(i,k) = 0.5_r8 * ( tke(i,k)  +  tke(i,k+1) )
               smm(i,k)  = 0.5_r8 * ( smaw(i,k) + smaw(i,k+1) )
               if( turbtype(i,k) .eq. 3._r8 ) then       ! Bottom entrainment interface
                   tkem(i,k) = 0.5_r8 *  tke(i,k+1)
                   smm(i,k)  = 0.5_r8 * smaw(i,k+1)
               elseif( turbtype(i,k+1) .eq. 4._r8 ) then ! Top entrainment interface
                   tkem(i,k) = 0.5_r8 *  tke(i,k)  
                   smm(i,k)  = 0.5_r8 * smaw(i,k)
               endif
               smm(i,k) = 0.259_r8*smm(i,k)
               smm(i,k) = max(smm(i,k), 0.4743_r8)
	    end do
            end do

        endif

           do i=1,ncol
	      ftrue=0
              do k=1,pver
! get sub-grid vertical velocity from diff coef.
! following morrison et al. 2005, JAS
! assume mixing length of 30 m

	      dum=(kkvh(i,k)+kkvh(i,k+1))/2._r8/30._r8
! use maximum sub-grid vertical vel of 10 m/s
	      dum=min(dum,10._r8)
! set wsub to value at current vertical level
	      wsub(i,k)=dum
            ! new computation
              if( wsubTKE ) then
                  wsub(i,k) = sqrt(0.5_r8*(tke(i,k)+tke(i,k+1))*(2._r8/3._r8))
                  wsub(i,k) = min(wsub(i,k),10._r8)
              endif
              wsubi(i,k) = max(0.001_r8,wsub(i,k))
              wsubi(i,k) = min(wsubi(i,k),0.2_r8)
              wsub(i,k)  = max(0.20_r8,wsub(i,k))
           end do
        end do

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

           icldm(i,k)=max(icecldf(i,k),mincld)
           lcldm(i,k)=max(liqcldf(i,k),mincld)


! calculate nfice based on liquid and ice mmr (no rain and snow mmr available yet)

        nfice(i,k)=0._r8
        dumfice=qc(i,k)+qi(i,k)
        if (dumfice.gt.qsmall .and. qi(i,k).gt.qsmall) then
           nfice(i,k)=qi(i,k)/dumfice
        endif

#ifndef MODAL_AERO

        do m=1,naer_all
           ntype(m)=1
           maerosol(1,m)=aer_mmr(i,k,m)*rho(i,k)

! For bulk aerosols only:
!   set number nucleated for sulfate based on Lohmann et al 2000 (JGR) eq 2
!           Na=340.*(massSO4)^0.58  where Na=cm-3 and massSO4=ug/m3
!   convert units to Na [m-3] and SO4 [kgm-3]
!   Na(m-3)= 1.e6 cm3 m-3 Na(cm-3)=340. *(massSO4[kg/m3]*1.e9ug/kg)^0.58
!   or Na(m-3)= 1.e6* 340.*(1.e9ug/kg)^0.58 * (massSO4[kg/m3])^0.58

           if(m .eq. idxsul) then
!              naer2(i,k,m)= 5.64259e13_r8 * maerosol(1,m)**0.58
              naer2(i,k,m)=maerosol(1,m)*num_to_mass_aer(m)*2._r8
           else
              naer2(i,k,m)=maerosol(1,m)*num_to_mass_aer(m)
           endif
        enddo
#endif

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ICE Nucleation

        if (t(i,k).lt.tmelt - 5._r8) then


! get ice nucleation rate

           call nucleati(wsubi(i,k),t(i,k),relhum(i,k),icldm(i,k),qc(i,k),nfice(i,k),rho(i,k),  &
#ifdef MODAL_AERO
                         qaer(i,k,:)*rho(i,k),dgnum(i,k,:),1,naer_all,dum2,nihf2,niimm2,nidep2,nimey2)
#else
                         naer2(i,k,:)/25._r8,1,naer_all,dum2,nihf2,niimm2,nidep2,nimey2)
#endif

           naai(i,k)=dum2
           naai_hom(i,k)=nihf2
           nihf(i,k)=nihf2
           niimm(i,k)=niimm2
           nidep(i,k)=nidep2
           nimey(i,k)=nimey2
        end if


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!droplet activation for bulk aerosol

#ifndef MODAL_AERO
        if (qc(i,k).ge.qsmall) then

! get droplet activation rate

        call activate(wsub(i,k),t(i,k),rho(i,k), &
                 naer2(i,k,:), naer_all,naer_all, maerosol,  &
                 dispersion_aer,hygro_aer, density_aer, dum2)
	dum = dum2

	else
	dum = 0._r8
	end if

 !note: deltat = deltat/2. to account for sub step in microphysics
        npccn(i,k) = (dum - nc(i,k)/lcldm(i,k))/(deltat/2._r8)*lcldm(i,k)
#endif

	end do ! i loop
	end do ! k loop

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!droplet activation for modal aerosol

#ifdef MODAL_AERO
!     partition cloud fraction into liquid water part

      do k=1,pver
      do i=1,ncol
         qcld=qc(i,k)+qi(i,k)
         if(qcld.gt.qsmall)then
            lcldn(i,k)=cldn(i,k)*qc(i,k)/qcld
            lcldo(i,k)=cldo(i,k)*qc(i,k)/qcld
         else
            lcldn(i,k)=0._r8
            lcldo(i,k)=0._r8
         endif
      enddo
      enddo

      call dropmixnuc(lchnk, ncol, ncloc, nctend_mixnuc, t, omega,  &
                    p, pint, pdel, rpdel, zm, kkvh, wsub, lcldn, lcldo,     &
                    qaer, cflx, qaertend, deltat                    &
#ifdef WRF_PORT
                    , qqcw                                          &
#endif
                                                                    )

      npccn(:ncol,:)= nctend_mixnuc(:ncol,:)
#endif


!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Contact freezing  (-40<T<-3 C) (Young, 1974) with hooks into simulated dust
! estimate rndst and nanco for 4 dust bins here to pass to MG microphysics


      do k=1,pver
      do i=1,ncol

         if (t(i,k).lt.269.15_r8) then

#ifdef MODAL_AERO
!BSINGH - Following #if added for MAM7 compliance
#ifdef MODAL_AERO_7MODE 
             nacon(i,k,3)=qaer(i,k,numptr_amode(modeptr_coardust))*rho(i,k)
             rndst(i,k,3)=0.5_r8*dgnumwet(i,k,modeptr_coardust)
#elif ( defined MODAL_AERO_3MODE )
! For modal aerosols:
!  use size '3' for dust coarse mode...
!  scale by dust fraction in coarse mode
               dmc= qaer(i,k,lptr_dust_a_amode(modeptr_coarse))
               ssmc=qaer(i,k,lptr_nacl_a_amode(modeptr_coarse))
               if (dmc.gt.0.0_r8) then
                  nacon(i,k,3)=dmc/(ssmc+dmc) * qaer(i,k,numptr_amode(modeptr_coarse))*rho(i,k) !*tcnt
               else 
                  nacon(i,k,3)=0._r8
               endif
               
               !also redefine parameters based on size...
               
               rndst(i,k,3)=0.5_r8*dgnumwet(i,k,modeptr_coarse)

!BSINGH - Following #endif added for MAM7 compliance
# endif 
           if (rndst(i,k,3).le.0._r8) then 
              rndst(i,k,3)=rn_dst3
           endif

#else

!For Bulk Aerosols: set equal to aerosol number for dust for bins 2-4 (bin 1=0)

           if (idxdst2.gt.0) then 
              nacon(i,k,2)=naer2(i,k,idxdst2) !*tcnt ! 1/m3
           endif
           if (idxdst3.gt.0) then 
              nacon(i,k,3)=naer2(i,k,idxdst3) !*tcnt
           endif
           if (idxdst4.gt.0) then 
              nacon(i,k,4)=naer2(i,k,idxdst4) ! *tcnt
           endif
#endif
        endif
      enddo
      enddo

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!bulk aerosol ccn concentration (modal does it in ndrop, from dropmixnuc)

!        ccn concentration as diagnostic
#ifndef MODAL_AERO
         do k=1,pver
	    ccn(:ncol,k,:) = 0.
            do m=1,naer_all
               if(m.eq.idxsul)then
	       ! Lohmann treatment for sulfate has variable size distribution
	          do i=1,ncol
                     if (naer2(i,k,m).gt.0._r8) then 
	                amcubesulfate(i)=amcubefactor(m)*aer_mmr(i,k,m)*rho(i,k)/(naer2(i,k,m))
                        smcritsulfate(i)=smcritfactor(m)/sqrt(amcubesulfate(i))
                     else
                        smcritsulfate(i)=1._r8
                     endif
                  end do
	       end if
               do l=1,psat
                  if(m.eq.idxsul)then
	             do i=1,ncol
                        arg=argfactor(m)*log(smcritsulfate(i)/super(l))
	                if(arg<2)then
	                   if(arg<-2)then
		              ccnfact(l,m)=1.0e-6_r8
	                   else
		              ccnfact(l,m)=0.5e-6_r8*erfc(arg)
			   endif
		        else
		           ccnfact(l,m)=0.0_r8
		        endif
                        ccn(i,k,l)=ccn(i,k,l)+naer2(i,k,m)*ccnfact(l,m)
		     end do
		  else
                  ccn(:ncol,k,l)=ccn(:ncol,k,l)+naer2(:ncol,k,m)*ccnfact(l,m)
               endif
            enddo
         enddo
      enddo

      do l=1,psat
         call outfld(ccn_name(l), ccn(1,1,l)    , pcols, lchnk   )
      enddo
#endif

      do l=1,naer_all
         call outfld(aername(l)//'_m3', naer2(1,1,l), pcols, lchnk)
      enddo

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! output activated liquid and ice (convert from #/kg -> #/m3)
  do i = 1,ncol
     do k=1,pver
        nihf(i,k)=nihf(i,k)*rho(i,k)
        niimm(i,k)=niimm(i,k)*rho(i,k)   
        nidep(i,k)=nidep(i,k)*rho(i,k)
        nimey(i,k)=nimey(i,k)*rho(i,k)
     end do
  end do
  call outfld('NIHF',nihf,    pcols,lchnk)
  call outfld('NIIMM',niimm,    pcols,lchnk)
  call outfld('NIDEP',nidep,    pcols,lchnk)
  call outfld('NIMEY',nimey,    pcols,lchnk)

      deallocate( &
         naermod,  &
         naer2,    &
         maerosol, &
         ntype     )

return
end subroutine microp_aero_ts

!##############################################################################

      subroutine activate(wbar, tair, rhoair,  &
                          na, pmode, nmode, ma, sigman, hygro, rhodry, nact)

!      calculates number fraction of aerosols activated as CCN
!      assumes an internal mixture within each of up to pmode multiple aerosol modes
!      a gaussiam spectrum of updrafts can be treated.

!      mks units

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      use physconst, only: rair, epsilo, cpair, rh2o, latvap, gravit,   &
                                 rhoh2o, mwh2o, r_universal
      use wv_saturation, only: estblf, epsqs

      implicit none


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

!      external erf,erfc
!      real(r8) erf,erfc
#if (defined AIX)
#define ERF erf
#define ERFC erfc
#else
#define ERF derf
#define ERFC derfc
      real(r8) derf,derfc
#endif

      integer, parameter:: nx=200
      integer :: maxmodes
      real(r8) surften       ! surface tension of water w/respect to air (N/m)
      data surften/0.076/
      save surften
      real(r8) p0     ! reference pressure (Pa)
      data p0/1013.25e2/
      save p0

      real(r8), allocatable :: volc(:) ! total aerosol volume  concentration (m3/m3)
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
      real(r8), allocatable :: eta(:)
      real(r8), allocatable :: smc(:)
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
      real(r8),allocatable :: etafactor2(:)
      real(r8) es
      integer m,n

      real(r8),allocatable :: amcubeloc(:)
      real(r8),allocatable :: lnsmloc(:)

      maxmodes = naer_all
      allocate( &
         volc(maxmodes),       &
         eta(maxmodes),        &
         smc(maxmodes),        &
         etafactor2(maxmodes), &
         amcubeloc(maxmodes),  &
         lnsmloc(maxmodes)     )

      if(maxmodes<pmode)then
         write(iulog,*)'maxmodes,pmode in activate =',maxmodes,pmode
#ifdef WRF_PORT
         call wrf_message(iulog)
#endif 
	 call endrun('activate')
      endif

      nact=0._r8

      if(nmode.eq.1.and.na(1).lt.1.e-20)return

      if(wbar.le.0.)return

      pres=rair*rhoair*tair
      diff0=0.211e-4*(p0/pres)*(tair/tt0)**1.94
      conduct0=(5.69+0.017*(tair-tt0))*4.186e2*1.e-5 ! convert to J/m/s/deg
      es = estblf(tair)
      qs = epsilo*es/(pres-(1.0_r8 - epsqs)*es)
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
! pjj/cray - use TINY intrinsic
!        if(volc(m).gt.1.e-39.and.na(m).gt.1.e-39)then
         if(volc(m).gt. TINY(volc) .and.na(m).gt. TINY(na))then
            etafactor2(m)=1./(na(m)*beta*sqrtg)  !fixed or variable size dist
!            number mode radius (m)
            amcubeloc(m)=(3.*volc(m)/(4.*pi*exp45logsig(m)*na(m)))  ! only if variable size dist
	    smc(m)=smcrit(m) ! only for prescribed size dist

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

            w=wbar
            alw=alpha*wnuc
            sqrtalw=sqrt(alw)
            zeta=2.*sqrtalw*aten/(3.*sqrtg)
	    etafactor1=2.*alw*sqrtalw

            do m=1,nmode
               eta(m)=etafactor1*etafactor2(m)
            enddo

            call maxsat(zeta,eta,nmode,smc,smax)

            lnsmax=log(smax)
            xmincoeff=alogaten-2.*third*(lnsmax-alog2)-alog3

	    nact=0._r8
            do m=1,nmode
	       x=2*(lnsmloc(m)-lnsmax)/(3*sq2*alogsig(m))
               nact=nact+0.5*(1.-erf(x))*na(m)
            enddo
	    nact=nact/rhoair ! convert from #/m3 to #/kg

      deallocate( &
         volc,       &
         eta,        &
         smc,        &
         etafactor2, &
         amcubeloc,  &
         lnsmloc     )

      return
      end subroutine activate

      subroutine maxsat(zeta,eta,nmode,smc,smax)

!      calculates maximum supersaturation for multiple
!      competing aerosol modes.

!      Abdul-Razzak and Ghan, A parameterization of aerosol activation.
!      2. Multiple aerosol types. J. Geophys. Res., 105, 6837-6844.

      implicit none

    ! integer pmode
    ! parameter (pmode=naer_all)
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
         else
            sum=1.e20
         endif
      enddo

      smax=1./sqrt(sum)

      return

      end subroutine maxsat


subroutine nucleati(wbar, tair, relhum, cldn, qc, nfice, rhoair, &
#ifdef MODAL_AERO
       qaerpt, dgnum, ptype, naer_all, nuci, onihf, oniimm, onidep, onimey)
#else
       na, ptype, naer_all, nuci, onihf, oniimm, onidep, onimey)
#endif
 
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
#ifdef MODAL_AERO      
#ifndef WRF_PORT
      use modal_aero_data, only:numptr_amode, modeptr_accum, modeptr_coarse, modeptr_aitken, &
                                 ntot_amode, sigmag_amode, &
                                 lptr_dust_a_amode,lptr_nacl_a_amode
      use constituents, only: pcnst
#else
      use modal_aero_data, only:numptr_amode => numptr_amode_mp, modeptr_accum => modeptr_accum_mp, &
           modeptr_coarse    => modeptr_coarse_mp   , modeptr_aitken    => modeptr_aitken_mp, &
           ntot_amode        => ntot_amode_mp       , sigmag_amode      => sigmag_amode_mp,   &
           lptr_dust_a_amode => lptr_dust_a_amode_mp, lptr_nacl_a_amode => lptr_nacl_a_amode_mp, &
           modeptr_coardust => modeptr_coardust_mp  !BSINGH - Added modeptr_coarsedust for MAM7 compliance
      use module_cam_support, only: pcnst =>pcnst_mp
#endif
#endif

!-----------------------------------------------------
! Input Arguments
!
  integer ptype, naer_all
  real(r8), intent(in) :: wbar                ! grid cell mean vertical velocity (m/s)
  real(r8), intent(in) :: tair                ! temperature (K)
  real(r8), intent(in) :: relhum              ! relative humidity with respective to liquid
  real(r8), intent(in) :: cldn                ! new value of cloud fraction    (fraction)
  real(r8), intent(in) :: qc                  ! liquid water mixing ratio (kg/kg)
  real(r8), intent(in) :: nfice               ! ice mass fraction
  real(r8), intent(in) :: rhoair              ! air density (kg/m3)
#ifdef MODAL_AERO
  real(r8), intent(in) :: qaerpt(pcnst) ! aerosol number and mass mixing ratios 
  real(r8), intent(in) :: dgnum(ntot_amode)   ! aerosol mode dry diameter (m)
#else      
  real(r8), intent(in) :: na(naer_all)        ! aerosol number concentration (/m3)
#endif


!
! Output Arguments
!
  real(r8), intent(out) :: nuci               ! ice number nucleated (#/kg)
  real(r8), intent(out) :: onihf              ! nucleated number from homogeneous freezing of so4
  real(r8), intent(out) :: oniimm             ! nucleated number from immersion freezing
  real(r8), intent(out) :: onidep             ! nucleated number from deposition nucleation
  real(r8), intent(out) :: onimey             ! nucleated number from deposition nucleation  (meyers: mixed phase)

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
  real(r8)  tc,A,B,C,regm,RHw                            ! work variable
  real(r8)  esl,esi,deles                                ! work variable
  real(r8)  dst_scale
  real(r8)  subgrid
  real(r8) dmc,ssmc         ! variables for modal scheme.

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

#ifdef MODAL_AERO
      soot_num = qaerpt(numptr_amode(modeptr_accum))*1.0e-6_r8 !#/cm^3
!BSINGH - Following #if added for MAM7 compliance
#ifdef MODAL_AERO_7MODE
      dst_num=qaerpt(numptr_amode(modeptr_coardust))*1.0e-6_r8 !#/cm^3
#elif ( defined MODAL_AERO_3MODE )
      dmc= qaerpt(lptr_dust_a_amode(modeptr_coarse))
      ssmc=qaerpt(lptr_nacl_a_amode(modeptr_coarse))
      if (dmc.gt.0._r8) then
         dst_num=dmc/(ssmc+dmc) * qaerpt(numptr_amode(modeptr_coarse))*1.0e-6_r8 !#/cm^3
      else 
         dst_num=0.0_r8
      endif
!BSINGH - Following #endif added for MAM7 compliance
#endif
      if (dgnum(modeptr_aitken) .gt. 0._r8  ) then
         so4_num  = qaerpt(numptr_amode(modeptr_aitken))*1.0e-6_r8 & !#/cm^3, only allow so4 with D>0.1 um in ice nucleation
               * (0.5_r8 - 0.5_r8*erf(log(0.1e-6_r8/dgnum(modeptr_aitken))/  &
                 (2._r8**0.5_r8*log(sigmag_amode(modeptr_aitken)))))
      else 
         so4_num = 0.0_r8 
      endif
      so4_num = max(0.0_r8,so4_num)
#else
    if(idxsul .gt. 0) then 
       so4_num=na(idxsul)*1.0e-6_r8 ! #/cm^3
    end if

!continue above philosophy here....

    if(idxbcphi .gt. 0) then 
      soot_num=na(idxbcphi)*1.0e-6_r8 !#/cm^3
    end if

    if(idxdst1 .gt. 0) then 
       dst1_num=na(idxdst1)  *1.0e-6_r8 !#/cm^3
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

#endif

! no soot nucleation 
    soot_num=0.0_r8

    ni=0._r8
    tc=tair-273.15_r8

    ! initialize
    niimm=0._r8
    nidep=0._r8
    nihf=0._r8

    if(so4_num.ge.1.0e-10_r8 .and. (soot_num+dst_num).ge.1.0e-10_r8 .and. cldn.gt.0._r8) then

!-----------------------------
! RHw parameterization for heterogeneous immersion nucleation
    A = 0.0073_r8
    B = 1.477_r8
    C = 131.74_r8
    RHw=(A*tc*tc+B*tc+C)*0.01_r8   ! RHi ~ 120-130%

    subgrid = 1.2_r8

    if((tc.le.-35.0_r8) .and. ((relhum*polysvp(tair,0)/polysvp(tair,1)*subgrid).ge.1.2_r8)) then ! use higher RHi threshold

       A = -1.4938_r8 * log(soot_num+dst_num) + 12.884_r8
       B = -10.41_r8  * log(soot_num+dst_num) - 67.69_r8
       regm = A * log(wbar) + B

       if(tc.gt.regm) then    ! heterogeneous nucleation only
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then ! exclude T<-40 & W>1m/s from hetero. nucleation
           call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else
           call hetero(tc,wbar,soot_num+dst_num,niimm,nidep)
           nihf=0._r8
           n1=niimm+nidep
         endif
       elseif (tc.lt.regm-5._r8) then ! homogeneous nucleation only
         call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
         niimm=0._r8
         nidep=0._r8
         n1=nihf
       else        ! transition between homogeneous and heterogeneous: interpolate in-between
         if(tc.lt.-40._r8 .and. wbar.gt.1._r8) then ! exclude T<-40 & W>1m/s from hetero. nucleation
           call hf(tc,wbar,relhum,subgrid,so4_num,nihf)
           niimm=0._r8
           nidep=0._r8
           n1=nihf
         else

           call hf(regm-5._r8,wbar,relhum,subgrid,so4_num,nihf)
           call hetero(regm,wbar,soot_num+dst_num,niimm,nidep)

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

! deposition/condensation nucleation in mixed clouds (-40<T<0C) (Meyers, 1992)
    if(tc.lt.0._r8 .and. tc.gt.-37._r8 .and. qc.gt.1.e-12_r8) then
      esl = polysvp(tair,0)     ! over water in mixed clouds
      esi = polysvp(tair,1)     ! over ice
      deles = (esl - esi)
      nimey=1.e-3_r8*exp(12.96_r8*deles/esi - 0.639_r8) 
    else
      nimey=0._r8
    endif

    nuci=ni+nimey
    if(nuci.gt.9999._r8.or.nuci.lt.0._r8) then
       write(iulog, *) 'Warning: incorrect ice nucleation number (nuci reset =0)'
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif 
       write(iulog, *) ni, tair, relhum, wbar, nihf, niimm, nidep,deles,esi,dst2_num,dst3_num,dst4_num
#ifdef WRF_PORT
       call wrf_message(iulog)
#endif 
       nuci=0._r8
    endif

    nuci=nuci*1.e+6_r8/rhoair    ! change unit from #/cm3 to #/kg
    onimey=nimey*1.e+6_r8/rhoair
    onidep=nidep*1.e+6_r8/rhoair
    oniimm=niimm*1.e+6_r8/rhoair
    onihf=nihf*1.e+6_r8/rhoair

  return
  end subroutine nucleati

  subroutine hetero(T,ww,Ns,Nis,Nid)

    real(r8), intent(in)  :: T, ww, Ns
    real(r8), intent(out) :: Nis, Nid

    real(r8) A11,A12,A21,A22,B11,B12,B21,B22
    real(r8) A,B,C

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

!     ice from immersion nucleation (cm^-3)

      B = (A11+B11*log(Ns)) * log(ww) + (A12+B12*log(Ns))
      C =  A21+B21*log(Ns)

      Nis = exp(A22) * Ns**B22 * exp(B*T) * ww**C
      Nis = min(Nis,Ns)

      Nid = 0.0_r8    ! don't include deposition nucleation for cirrus clouds when T<-37C

      return
  end subroutine hetero


  subroutine hf(T,ww,RH,subgrid,Na,Ni)

      real(r8), intent(in)  :: T, ww, RH, subgrid, Na
      real(r8), intent(out) :: Ni

      real(r8)    A1_fast,A21_fast,A22_fast,B1_fast,B21_fast,B22_fast
      real(r8)    A2_fast,B2_fast
      real(r8)    C1_fast,C2_fast,k1_fast,k2_fast
      real(r8)    A1_slow,A2_slow,B1_slow,B2_slow,B3_slow
      real(r8)    C1_slow,C2_slow,k1_slow,k2_slow
      real(r8)    regm
      real(r8)    A,B,C
      real(r8)    RHw

!---------------------------------------------------------------------
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
!RHw parameters
      A = 6.0e-4_r8*log(ww)+6.6e-3_r8
      B = 6.0e-2_r8*log(ww)+1.052_r8
      C = 1.68_r8  *log(ww)+129.35_r8
      RHw=(A*T*T+B*T+C)*0.01_r8

      if((T.le.-37.0_r8) .and. ((RH*subgrid).ge.RHw)) then

        regm = 6.07_r8*log(ww)-55.0_r8

        if(T.ge.regm) then    ! fast-growth regime

          if(T.gt.-64.0_r8) then
            A2_fast=A21_fast
            B2_fast=B21_fast
          else
            A2_fast=A22_fast
            B2_fast=B22_fast
          endif

          k1_fast = exp(A2_fast + B2_fast*T + C2_fast*log(ww))
          k2_fast = A1_fast+B1_fast*T+C1_fast*log(ww)

          Ni = k1_fast*Na**(k2_fast)
          Ni = min(Ni,Na)

        else       ! slow-growth regime

          k1_slow = exp(A2_slow + (B2_slow+B3_slow*log(ww))*T + C2_slow*log(ww))
          k2_slow = A1_slow+B1_slow*T+C1_slow*log(ww)

          Ni = k1_slow*Na**(k2_slow)
          Ni = min(Ni,Na)

        endif

      end if

      return
  end subroutine hf

!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! error function in single precision
!
!    Copyright(C) 1996 Takuya OOURA (email: ooura@mmm.t.u-tokyo.ac.jp).
!    You may use, copy, modify this code for any purpose and 
!    without fee. You may distribute this ORIGINAL package.

      real(r8) function derf(x)
      implicit real (a - h, o - z)
! pjj/cray function type
!     real(r8) a,b,x
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
!

end module microp_aero

