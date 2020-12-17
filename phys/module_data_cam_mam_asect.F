	module module_data_cam_mam_asect

!-----------------------------------------------------------------------
!   New in June 2010, by r.c.easter
!   This file is similar to module_data_mosaic_asect, 
!   but works with the CAM modal aerosol packages in WRF-Chem.
!
!   Note that the position/index arrays in this file refer to the "chem" array,
!   while those in file module_data_cam_mam_aero refer to the "q" array
!   used in the CAM modal aerosol routines
!
!   *** Important note on units ***
!   in this file, 
!	aerosol densities here are g/cm3
!	aerosol sizes and volumes are cm and cm3
!       thus the density/size/volume variables in the file have the same units
!	    as their counterparts in module_data_mosaic_asect.F and module_data_sorgam.F
!   in module_data_cam_mam_aero.F,
!	aerosol densities here are kg/m3
!	aerosol sizes and volumes are m and m3
!-----------------------------------------------------------------------


	use shr_kind_mod,    only:  r8 => shr_kind_r8

	use modal_aero_data, only:  ntot_amode, maxd_aspectype


	implicit none


!-----------------------------------------------------------------------
!
!   The variables in this module provide a means of organizing and accessing
!   aerosol species in the "chem" array by their chemical component, 
!   size bin (or mode), "type", and "phase"
!
!   Their purpose is to allow flexible coding of process modules, 
!   compared to "hard-coding" using the chem array p_xxx indices
!   (e.g., p_so4_a01, p_so4_a02, ...; p_num_a01, ...)
!
!-----------------------------------------------------------------------
!
!   rce & sg 2004-dec-03 - added phase and type capability,
!	which changed this module almost completely
!
!-----------------------------------------------------------------------
!
!   maxd_atype = maximum allowable number of aerosol types
!   maxd_asize = maximum allowable number of aerosol size bins
!   maxd_acomp = maximum allowable number of chemical components
!	in each aerosol size bin
!   maxd_aphase = maximum allowable number of aerosol phases 
!	(gas, cloud, ice, rain, ...)
!
!   ntype_aer = number of aerosol types
!	The aerosol type will allow treatment of an externally mixed 
!	aerosol.  The current MOSAIC code has only 1 type, with the implicit
!	assumption of internal mixing.  Eventually, multiple types 
!	could treat fresh primary BC/OC, fresh SO4 from nucleation, 
!	aged BC/OC/SO4/... mixture, soil dust, sea salt, ... 
!
!   nphase_aer = number of aerosol phases
!
!   ai_phase = phase (p) index for interstitial (unactivated) aerosol particles
!   cw_phase = phase (p) index for aerosol particles in cloud water
!   ci_phase = phase (p) index for aerosol particles in cloud ice
!   rn_phase = phase (p) index for aerosol particles in rain
!   sn_phase = phase (p) index for aerosol particles in snow
!   gr_phase = phase (p) index for aerosol particles in graupel
!   [Note:  the value of "xx_phase" will be between 1 and nphase_aer 
!	for phases that are active in a simulation.  The others
!	will have non-positive values.]
!
!   nsize_aer(t) = number of aerosol size bins for aerosol type t
!
!   ncomp_aer(t) = number of "regular" chemical components for aerosol type t
!   ncomp_plustracer_aer(t) = number of "regular" plus "tracer"
!	chemical components for aerosol type t
!   [Note:  only "regular" components are used for calculating
!	aerosol physical (mass, volume) and chemical properties.
!	"Tracer" components are optional, and can be used to track source 
!	regions, source mechanisms, etc.]
!   [Note:  for aerosol type t, all phases have the same number of size
!	bins, and all size bins have the same number of 
!	both regular and tracer components.]
!
!   ntot_mastercomp_aer = number of aerosol chemical components defined
!	in the "master component list".
!   [Note:  each aerosol type will use some but not necessarily all
!	of the components in the "master component list".]
!
!   mastercompptr_aer(c,t) = the position/index/i.d. in the 
!       "master component list" for chemical component c of aerosol type t.
!	(1=sulfate, others to be defined by user.)
!
!   massptr_aer(c,s,t,p) = the position/index in the chem array for mixing- 
!	ratio for chemical component c, size bin s, type t, and phase p.
!
!   lptr_so4_aer(s,t,p) = the position/index in the chem array for mixing-
!	ratio for sulfate for aerosol size bin s, type t, and phase p
!   (similar lptr's are defined for no3, cl, msa, co3, 
!	nh4, na, ca, oin, oc, bc, ...)
!   [Note:  the massptr_aer allow you to loop over all species of 
!	an aerosol type.  The lptr_so4_aer, etc., allow you to access
!	a specific chemical component.]
!
!   waterptr_aer(s,t) = the position/index in the chem array for mixing-
!	ratio of aerosol water content for size bin s, type t.
!	[Note:  water content is only carried for the interstitial aerosol
!	phase, so there is no p dimension.]
!
!   hyswptr_aer(s,t) = the position/index in the chem array for mixing-
!	ratio of aerosol "hysteresis water" content for size bin s, type t.
!	This is used to determine if aerosol is in the dry or wet state, when
!	the ambient RH is between the crystallization and deliquescence RH.
!	[Note:  hysteresis water content is only carried for the 
!	interstitial aerosol phase, so there is no p dimension.]
!
!   numptr_aer(s,t,p) = the position/index in the chem array for mixing- 
!	ratio of particle number for size bin s, type t, and phase p.
!
!   mprognum_aer(s,t,p) - if positive, number mixing-ratio for size s, type t,
!       and phase p will be prognosed.  Otherwise, it is diagnosed using
!	mass mixing-ratio add assumed/prescribed size.
!
!	mixing ratio (moles-water/mole-air) for water
!       associated with aerosol size bin s and type t
!
!
!   mastercompindx_so4_aer = the position/index in the 
!       "master component list" for sulfate.  
!   (similar lptr's are defined for no3, cl, msa, co3, 
!	nh4, na, ca, oin, oc, bc, ...)
!   [Note:  the mastercompindx_xxx_aer are used primarily in 
!	initialization routines, and generally aren't needed elsewhere.]
!
!-----------------------------------------------------------------------
!
!   dens_mastercomp_aer(mc) = dry density (g/cm^3) of component mc 
!	of the master component list.
!   dens_aer(c,t) = dry density (g/cm^3) of aerosol chemical component 
!	c of type t
!   [Note:  dens_aer(c,t) == dens_mastercomp_aer(mastercompptr_aer(c,t))
!	The dens_mastercomp_aer is used in some initialization routines.
!	The dens_aer is used in most other places because of convenience.]
!
!   mw_mastercomp_aer(mc) = molecular weight (g/mole) of component mc 
!	of the master component list.
!   mw_aer(c,t) = molecular weight (g/mole) of aerosol chemical component 
!	c of type t
!   [Note:  mw_aer(c,t) == mw_mastercomp_aer(mastercompptr_aer(c,t)) ]
!
!   name_mastercomp_aer(mc) = name of component mc of the 
!	master component list (e.g., "sulfate", "nitrate", ...).
!   name_aer(c,t) = molecular weight (g/mole) of aerosol chemical component 
!	c of type t
!   [Note:  name_aer(c,t) == name_mastercomp_aer(mastercompptr_aer(c,t)) ]
!
!   hygro_mastercomp_aer(mc) = bulk hygroscopicity (--) at dilute conditions
!	(RH near 100%) of component mc of the master component list.
!   hygro_aer(c,t) = bulk hygroscopicity (--) at dilute conditions 
!	(RH near 100%) of aerosol chemical component c of type t
!   [For definition of bulk hygroscopicity, 
!	see Abdul-Razzak and Ghan, 2004, J Geophys Res, V105, p. 6837-6844.]
!   [Note:  hygro_aer(c,t) == hygro_mastercomp_aer(mastercompptr_aer(c,t)) ]
!
!-----------------------------------------------------------------------
!
!   volumlo_sect(s,t) = 1-particle volume (cm^3) at lower boundary of section m
!   volumhi_sect(s,t) = 1-particle volume (cm^3) at upper boundary of section m
!   volumcen_sect(s,t)= 1-particle volume (cm^3) at "center" of section m
!
!   dlo_sect(s,t) = 1-particle diameter (cm) at lower boundary of section m
!   dhi_sect(s,t) = 1-particle diameter (cm) at upper boundary of section m
!   dcen_sect(s,t) = 1-particle diameter (cm) at "center" section m
!
!   [Note:  the "center" values are defined as follows:
!       volumcen_sect == 0.5*(volumlo_sect + volumhi_sect)
!                     == (pi/6) * (dcen_sect**3) ]
!
!-----------------------------------------------------------------------
!
!   msectional - if positive, each aerosol size bin is a section.
!   if equals 10, use jacobson moving center
!   if equals 20, use tzivion mass-number advection 
!	if zero/negative, each size bin is a mode (aitken, accumulation, ...)
!
!   maerosolincw - if positive, both unactivated/interstitial and activated
!       aerosol species are simulated.  if zero/negative, only the
!       unactivated are simulated.  [maerosolincw>0 only when cw_phase>0]
!
!   maerocoag - if positive, aerosol coagulation is done.
!	If zero/negative, it is skipped.
!	(This is not yet implemented in WRF-Chem.)
!
!   maerchem - if positive, aerosol gas-particle condensation/evaporation
!	of inorganic species is done.  If zero/negative, it is skipped.
!	(This is not yet implemented in WRF-Chem.)
!
!   maerchem_boxtest_output - if positive, "boxtest" output is done from
!	the aerchemistry routine.  If zero/negative, it is skipped.
!	(This is not yet implemented in WRF-Chem.)
!
!   maeroptical - if positive, aerosol optical properties are calculated. 
!	If zero/negative, it is skipped.
!	(This is not yet implemented in WRF-Chem.)
!
!-----------------------------------------------------------------------

	integer, parameter :: maxd_atype = ntot_amode
	integer, parameter :: maxd_asize = 1
	integer, parameter :: maxd_acomp = maxd_aspectype
	integer, parameter :: maxd_aphase = 2

	integer, save :: ai_phase = 1
	integer, save :: cw_phase = 2
	integer, save :: ci_phase = -999888777
	integer, save :: rn_phase = -999888777
	integer, save :: sn_phase = -999888777
	integer, save :: gr_phase = -999888777

	integer, save :: ntype_aer = 0 ! number of types
	integer, save :: ntot_mastercomp_aer = 0 ! number of master components
	integer, save :: nphase_aer = 0 ! number of phases

	integer, save ::   &
      	  nsize_aer( maxd_atype ),   & ! number of size bins
      	  ncomp_aer( maxd_atype ),   & ! number of chemical components
      	  ncomp_plustracer_aer( maxd_atype ),   &
          mastercompptr_aer(maxd_acomp, maxd_atype), &   !  mastercomp index
      	  massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), & 
		! index for mixing ratio
      	  waterptr_aer( maxd_asize, maxd_atype ), & ! index for aerosol water
      	  hyswptr_aer( maxd_asize, maxd_atype ), &
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), & 
		! index for the number mixing ratio
          mprognum_aer(maxd_asize,maxd_atype,maxd_aphase)


!   these indices give the location in the "mastercomp list" of
!   the different aerosol chemical (or tracer) components
	integer, save :: mastercompindx_so4_aer = -999888777
	integer, save :: mastercompindx_nh4_aer = -999888777
	integer, save :: mastercompindx_no3_aer = -999888777
	integer, save :: mastercompindx_pom_aer = -999888777
	integer, save :: mastercompindx_soa_aer = -999888777
	integer, save :: mastercompindx_bc_aer  = -999888777
	integer, save :: mastercompindx_dust_aer = -999888777
	integer, save :: mastercompindx_seas_aer = -999888777


	real, save ::   &
          dens_aer( maxd_acomp, maxd_atype ),  &
          dens_mastercomp_aer( maxd_acomp ),   &
      	  mw_mastercomp_aer( maxd_acomp ),     &
      	  mw_aer( maxd_acomp, maxd_atype ),    &
      	  hygro_mastercomp_aer( maxd_acomp ),  &
      	  hygro_aer( maxd_acomp, maxd_atype )

	real, save ::   &
          volumcen_sect( maxd_asize, maxd_atype ),  &
          volumlo_sect( maxd_asize, maxd_atype ),   &
          volumhi_sect( maxd_asize, maxd_atype ),   &
          dcen_sect( maxd_asize, maxd_atype ),      &
          dlo_sect( maxd_asize, maxd_atype ),       &
          dhi_sect( maxd_asize, maxd_atype ),       &
          sigmag_aer(maxd_asize, maxd_atype)

	character*10, save ::   &
      	  name_mastercomp_aer( maxd_acomp ),  &
      	  namebb_mastercomp_aer( maxd_acomp ),  &
      	  name_aer( maxd_acomp, maxd_atype )

	integer, save ::                     &
      	  lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_pom_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_soa_aer(maxd_asize, maxd_atype, maxd_aphase),      &
      	  lptr_bc_aer(maxd_asize, maxd_atype, maxd_aphase),       &
      	  lptr_dust_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_seas_aer(maxd_asize, maxd_atype, maxd_aphase)

! rce 11-sep-2004 - eliminated all of the "..._wrfch" pointers
!    so now there is only one set of pointers ("..._amode")
! sg/rce nov-2004 - totally new pointer system  - "..._aer"


!   the mw_xxx_aer and dens_xxx_aer will be set from 
!      the values in module_data_cam_mam_aero
!   molecular weights (g/mol)
	real, save ::   &
          mw_so4_aer, mw_nh4_aer,   &
          mw_no3_aer, mw_pom_aer,   &
          mw_soa_aer, mw_bc_aer,   &
          mw_dust_aer, mw_seas_aer

!   dry densities (g/cm3)
	real, save ::   &
          dens_so4_aer, dens_nh4_aer,   &
          dens_no3_aer, dens_pom_aer,   &
          dens_soa_aer, dens_bc_aer,   &
          dens_dust_aer, dens_seas_aer

!   water density (g/cm3)
!	real, parameter :: dens_water_asize  = 1.0
	real, parameter :: dens_water_aer  = 1.0


	integer, save ::   &
      	  msectional, maerosolincw,   &
      	  maerocoag, maerchem, maeroptical, maerchem_boxtest_output


      integer, allocatable ::  &
         lptr_chem_to_q(:), lptr_chem_to_qqcw(:)
! for chem array species l, the corresponding q    array species is lptr_chem_to_q(l)
! for chem array species l, the corresponding qqcw array species is lptr_chem_to_qqcw(l)

      real, allocatable ::  &
         factconv_chem_to_q(:), factconv_chem_to_qqcw(:)
! factor for converting chem array species l to corresponding q    array species is factconv_chem_to_q(l)
! factor for converting chem array species l to corresponding qqcw array species is factconv_chem_to_qqcw(l)

!     real(r8), allocatable ::  &
      real, allocatable ::  &
         mw_chem_array(:), mw_q_array(:), mw_q_mo_array(:)
! mw_chem_array = molecular weights for species in wrf-chem chem array (g)
! mw_q_array    = molecular weights for species in cam      q    array (g)
! mw_q_mo_array = molecular weights for species in chemistry portion of the cam q array (g)
!                 (trace gas and aerosol species, but no water species)
! *** note - for gases, value is 1.0 currently, unless the gas is really used by cam_mam routines
!            (e.g., mw for nox/noy and vocs are 1.0)


	end module module_data_cam_mam_asect

