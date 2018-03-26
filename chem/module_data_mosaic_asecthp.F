      module module_data_mosaic_asecthp
!-----------------------------------------------------------------------
!
! 2015-06-09 r.c.easter - changes to the MOSAIC box model version for use in WRF-Chem  
!
!-----------------------------------------------------------------------

      use module_data_mosaic_kind, only:  r8
      use module_data_mosaic_aero, only:  naer, nbin_a_max, ngas_aerchtot

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

	integer, parameter :: maxd_atype = 1
        integer, parameter :: maxd_atype_md1 = 1
        integer, parameter :: maxd_atype_md2 = 1

	integer, parameter :: maxd_asize = 8
	integer, parameter :: maxd_acomp = 120 ! for additional SOA species changed by Manish Shrivastava on 01/25/10
	integer, parameter :: maxd_aphase = 2

	integer, save :: ai_phase = -999888777
	integer, save :: cw_phase = -999888777
	integer, save :: ci_phase = -999888777
	integer, save :: rn_phase = -999888777
	integer, save :: sn_phase = -999888777
	integer, save :: gr_phase = -999888777

	integer, save :: ntype_aer = 0 ! number of types
	integer, save :: ntot_mastercomp_aer = 0 ! number of master components
	integer, save :: nphase_aer = 0 ! number of phases

                                                          ! currently these must all be positive, but that could change
        integer, parameter :: identical_comps_optaa  = 1  ! when positive, all aerosol types must have same chemical components
        integer, parameter :: identical_phases_optaa = 1  ! when positive, all aerosol types must have same phases
        integer, parameter :: identical_sizes_optaa  = 1  ! when positive, all aerosol types must have same sizes


	integer, save ::   &
      	  nsize_aer( maxd_atype ),   & ! number of size bins
      	  ncomp_aer( maxd_atype ),   & ! number of chemical components
      	  ncomp_plustracer_aer( maxd_atype ),   &
          mastercompptr_aer(maxd_acomp, maxd_atype), &         !  mastercomp index
      	  massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), & 
		! index for mixing ratio
      	  waterptr_aer( maxd_asize, maxd_atype ), &            ! index for aerosol water
      	  hyswptr_aer( maxd_asize, maxd_atype ), &
      	  numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), & ! index for the number mixing ratio
          mprognum_aer(maxd_asize,maxd_atype,maxd_aphase)

      integer, save :: ntype_md1_aer = 0 ! number of md1 types
      integer, save :: ntype_md2_aer = 0 ! number of md2 types

      integer, save ::   &
         ibin_of_isize_itype(maxd_asize,maxd_atype), &
         isize_of_ibin(nbin_a_max), &
         itype_of_ibin(nbin_a_max), &
         itype_of_itype_md1md2(maxd_atype_md1,maxd_atype_md2), &
         itype_md1_of_itype(maxd_atype), &
         itype_md2_of_itype(maxd_atype)

      integer, save :: lunerr=-1, lunout=-1

      integer, save ::   &
         rbox_aer_ptr(-3:naer,nbin_a_max),  &   ! maps aerosol species between rbox and aer, num_a, water_a arrays
         rbox_gas_ptr(ngas_aerchtot)            ! maps gas     species between rbox and gas arrays


      logical, save ::   &
          is_tracer_mastercomp_aer(  maxd_acomp )


      real(r8), save ::   &
         dens_aer( maxd_acomp, maxd_atype ),  &
         dens_mastercomp_aer( maxd_acomp ),   &
         mw_mastercomp_aer( maxd_acomp ),     &
         mw_aer( maxd_acomp, maxd_atype ),    &
         hygro_mastercomp_aer( maxd_acomp ),  &
         hygro_aer( maxd_acomp, maxd_atype )

      real(r8), save ::   &
         volumcen_sect( maxd_asize, maxd_atype ),  &
         volumlo_sect( maxd_asize, maxd_atype ),   &
         volumhi_sect( maxd_asize, maxd_atype ),   &
         dcen_sect( maxd_asize, maxd_atype ),      &
         dlo_sect( maxd_asize, maxd_atype ),       &
         dhi_sect( maxd_asize, maxd_atype ),       &
         sigmag_aer(maxd_asize, maxd_atype)

      real(r8), save ::   &
         volumcut_sect( 0:maxd_asize, maxd_atype ),  &
         dcut_sect( 0:maxd_asize, maxd_atype )

! these are the cut values that separate the "md1" and "md2" types
! for example, the bc mass fraction (itype_md1) is between
!    xcut_atype_md1(0) and xcut_atype_md1(1) for itype_md1=1
!    xcut_atype_md1(1) and xcut_atype_md1(2) for itype_md1=2
      real(r8), save ::    &
         xcut_atype_md1(0:maxd_atype_md1),   &
         xcut_atype_md2(0:maxd_atype_md2)


	character*20, save ::   &
      	  name_mastercomp_aer( maxd_acomp ),   &
      	  sname_mastercomp_aer( maxd_acomp ),  &
      	  name_aer( maxd_acomp, maxd_atype ),  &
      	  sname_aer( maxd_acomp, maxd_atype )

        character*20, save ::   &
          phase_chars_aer( maxd_acomp ),  &
          type_chars_aer( maxd_acomp )


!-----------------------------------------------------------------------
!   following are used in movesect, newnuc, and coag routines
!   to identify bins with essentially negligible mass
!
!   if bin mass mixrat < smallmassaa (1.0e-22 g/g-air), 
!   then assume no growth AND no water AND conform number so that size is within bin limits
!       real(r8), parameter :: smallmassaa = 1.0e-22_r8
        real,     parameter :: smallmassaa = 1.0e-22
!   if bin mass mixrat < smallmassab (1.0e-32 g/g-air),
!   then assume default density to avoid divide by zero
!       real(r8), parameter :: smallmassbb = 1.0e-32_r8
        real,     parameter :: smallmassbb = 1.0e-32
!
!   with single-particle diameter = 1 nm and mass ~1e-21 g, 
!       and number = 1e-4 #/cm3 ~= 1e-1 #/g-air, the mass mixing ratio ~= 1e-22 g/g-air
!   for simulations focusing on nucleation and ultrafine particles, 
!      one might want to use reduce smallmassaa 
!-----------------------------------------------------------------------



!   water density (g/cm3)
!	real, parameter :: dens_water_asize  = 1.0
	real, parameter :: dens_water_aer  = 1.0

!   aerosol component dry densities (g/cm3)
        real, parameter :: dens_so4_aer = 1.80
        real, parameter :: dens_no3_aer = 1.80
        real, parameter :: dens_cl_aer  = 2.20
        real, parameter :: dens_msa_aer = 1.80
        real, parameter :: dens_co3_aer = 2.60
        real, parameter :: dens_nh4_aer = 1.80
        real, parameter :: dens_na_aer  = 2.20
        real, parameter :: dens_ca_aer  = 2.60
        real, parameter :: dens_oin_aer = 2.60
!       real, parameter :: dens_dust_aer = 2.60
!       real, parameter :: dens_tr1r1_aer = 2.60
!       real, parameter :: dens_tr1r2_aer = 2.60
!       real, parameter :: dens_tr1r3_aer = 2.60
!       real, parameter :: dens_tr1r4_aer = 2.60
        real, parameter :: dens_oc_aer  = 1.00
        real, parameter :: dens_bc_aer  = 1.70
!       real, parameter :: dens_tanv_aer  = 1.00
!       real, parameter :: dens_sopnv_aer  = 1.00
!       real, parameter :: dens_ternv_aer  = 1.00
!       real, parameter :: dens_seqnv_aer  = 1.00
!       real, parameter :: dens_sianv_aer  = 1.00
        real, parameter :: dens_pcg1_b_c_aer = 1.0
        real, parameter :: dens_pcg2_b_c_aer = 1.0
        real, parameter :: dens_pcg3_b_c_aer = 1.0
        real, parameter :: dens_pcg4_b_c_aer = 1.0
        real, parameter :: dens_pcg5_b_c_aer = 1.0
        real, parameter :: dens_pcg6_b_c_aer = 1.0
        real, parameter :: dens_pcg7_b_c_aer = 1.0
        real, parameter :: dens_pcg8_b_c_aer = 1.0
        real, parameter :: dens_pcg9_b_c_aer = 1.0
        real, parameter :: dens_pcg1_b_o_aer = 1.0
        real, parameter :: dens_pcg2_b_o_aer = 1.0
        real, parameter :: dens_pcg3_b_o_aer = 1.0
        real, parameter :: dens_pcg4_b_o_aer = 1.0
        real, parameter :: dens_pcg5_b_o_aer = 1.0
        real, parameter :: dens_pcg6_b_o_aer = 1.0
        real, parameter :: dens_pcg7_b_o_aer = 1.0
        real, parameter :: dens_pcg8_b_o_aer = 1.0
        real, parameter :: dens_pcg9_b_o_aer = 1.0
        real, parameter :: dens_opcg1_b_c_aer = 1.0
        real, parameter :: dens_opcg2_b_c_aer = 1.0
        real, parameter :: dens_opcg3_b_c_aer = 1.0
        real, parameter :: dens_opcg4_b_c_aer = 1.0
        real, parameter :: dens_opcg5_b_c_aer = 1.0
        real, parameter :: dens_opcg6_b_c_aer = 1.0
        real, parameter :: dens_opcg7_b_c_aer = 1.0
        real, parameter :: dens_opcg8_b_c_aer = 1.0
        real, parameter :: dens_opcg1_b_o_aer = 1.0
        real, parameter :: dens_opcg2_b_o_aer = 1.0
        real, parameter :: dens_opcg3_b_o_aer = 1.0
        real, parameter :: dens_opcg4_b_o_aer = 1.0
        real, parameter :: dens_opcg5_b_o_aer = 1.0
        real, parameter :: dens_opcg6_b_o_aer = 1.0
        real, parameter :: dens_opcg7_b_o_aer = 1.0
        real, parameter :: dens_opcg8_b_o_aer = 1.0
        real, parameter :: dens_pcg1_f_c_aer = 1.0
        real, parameter :: dens_pcg2_f_c_aer = 1.0
        real, parameter :: dens_pcg3_f_c_aer = 1.0
        real, parameter :: dens_pcg4_f_c_aer = 1.0
        real, parameter :: dens_pcg5_f_c_aer = 1.0
        real, parameter :: dens_pcg6_f_c_aer = 1.0
        real, parameter :: dens_pcg7_f_c_aer = 1.0
        real, parameter :: dens_pcg8_f_c_aer = 1.0
        real, parameter :: dens_pcg9_f_c_aer = 1.0
        real, parameter :: dens_pcg1_f_o_aer = 1.0
        real, parameter :: dens_pcg2_f_o_aer = 1.0
        real, parameter :: dens_pcg3_f_o_aer = 1.0
        real, parameter :: dens_pcg4_f_o_aer = 1.0
        real, parameter :: dens_pcg5_f_o_aer = 1.0
        real, parameter :: dens_pcg6_f_o_aer = 1.0
        real, parameter :: dens_pcg7_f_o_aer = 1.0
        real, parameter :: dens_pcg8_f_o_aer = 1.0
        real, parameter :: dens_pcg9_f_o_aer = 1.0
        real, parameter :: dens_opcg1_f_c_aer = 1.0
        real, parameter :: dens_opcg2_f_c_aer = 1.0
        real, parameter :: dens_opcg3_f_c_aer = 1.0
        real, parameter :: dens_opcg4_f_c_aer = 1.0
        real, parameter :: dens_opcg5_f_c_aer = 1.0
        real, parameter :: dens_opcg6_f_c_aer = 1.0
        real, parameter :: dens_opcg7_f_c_aer = 1.0
        real, parameter :: dens_opcg8_f_c_aer = 1.0
        real, parameter :: dens_opcg1_f_o_aer = 1.0
        real, parameter :: dens_opcg2_f_o_aer = 1.0
        real, parameter :: dens_opcg3_f_o_aer = 1.0
        real, parameter :: dens_opcg4_f_o_aer = 1.0
        real, parameter :: dens_opcg5_f_o_aer = 1.0
        real, parameter :: dens_opcg6_f_o_aer = 1.0
        real, parameter :: dens_opcg7_f_o_aer = 1.0
        real, parameter :: dens_opcg8_f_o_aer = 1.0
        real, parameter :: dens_smpa_aer = 1.0
        real, parameter :: dens_smpbb_aer = 1.0
!       real, parameter :: dens_iepox_aer = 1.0
!       real, parameter :: dens_iepoxos_aer = 1.0
!       real, parameter :: dens_tetrol_aer = 1.0
!       real, parameter :: dens_gly_aer = 1.0
        real, parameter :: dens_ant1_c_aer = 1.0
        real, parameter :: dens_ant2_c_aer = 1.0
        real, parameter :: dens_ant3_c_aer = 1.0
        real, parameter :: dens_ant4_c_aer = 1.0
        real, parameter :: dens_ant1_o_aer = 1.0
        real, parameter :: dens_ant2_o_aer = 1.0
        real, parameter :: dens_ant3_o_aer = 1.0
        real, parameter :: dens_ant4_o_aer = 1.0
        real, parameter :: dens_biog1_c_aer = 1.0
        real, parameter :: dens_biog2_c_aer = 1.0
        real, parameter :: dens_biog3_c_aer = 1.0
        real, parameter :: dens_biog4_c_aer = 1.0
        real, parameter :: dens_biog1_o_aer = 1.0
        real, parameter :: dens_biog2_o_aer = 1.0
        real, parameter :: dens_biog3_o_aer = 1.0
        real, parameter :: dens_biog4_o_aer = 1.0

        real, parameter :: dens_glysoa_r1_aer   = 1.0
        real, parameter :: dens_glysoa_r2_aer   = 1.0
        real, parameter :: dens_glysoa_oh_aer   = 1.0
        real, parameter :: dens_glysoa_nh4_aer   = 1.0
        real, parameter :: dens_glysoa_sfc_aer   = 1.0
! dens based on Murphy and Pandis, ES&T, 2009
        real, parameter :: dens_asoaX_aer = 1.5
        real, parameter :: dens_asoa1_aer = 1.5
        real, parameter :: dens_asoa2_aer = 1.5
        real, parameter :: dens_asoa3_aer = 1.5
        real, parameter :: dens_asoa4_aer = 1.5
        real, parameter :: dens_bsoaX_aer = 1.5
        real, parameter :: dens_bsoa1_aer = 1.5
        real, parameter :: dens_bsoa2_aer = 1.5
        real, parameter :: dens_bsoa3_aer = 1.5
        real, parameter :: dens_bsoa4_aer = 1.5

!   aerosol component molecular weights (g/mol)
!       real, parameter :: mw_oc_aer  =   1.0   ! use   1 for standard mosaic
        real, parameter :: mw_oc_aer  = 250.0   ! use 250 for m. shrivastava soa treatments

        real, parameter :: mw_so4_aer = 96.066
        real, parameter :: mw_no3_aer = 62.007
        real, parameter :: mw_cl_aer  = 35.450
        real, parameter :: mw_msa_aer = 96.109
        real, parameter :: mw_co3_aer = 60.007
        real, parameter :: mw_nh4_aer = 18.042
        real, parameter :: mw_na_aer  = 22.990
        real, parameter :: mw_ca_aer  = 40.080
        real, parameter :: mw_oin_aer = 1.0
!       real, parameter :: mw_dust_aer = 1.0
        real, parameter :: mw_bc_aer  = 1.0
        real, parameter :: mw_water_aer  = 18.016

!       real, parameter :: mw_tr1r1_aer = 1.0
!       real, parameter :: mw_tr1r2_aer = 1.0
!       real, parameter :: mw_tr1r3_aer = 1.0
!       real, parameter :: mw_tr1r4_aer = 1.0
!       real, parameter :: mw_tanv_aer  = 250.0
!       real, parameter :: mw_sopnv_aer  = 250.0
!       real, parameter :: mw_ternv_aer  = 250.0
!       real, parameter :: mw_seqnv_aer  = 250.0
!       real, parameter :: mw_sianv_aer  = 250.0
        real, parameter :: mw_pcg1_b_c_aer = 250.0
        real, parameter :: mw_pcg2_b_c_aer = 250.0
        real, parameter :: mw_pcg3_b_c_aer = 250.0
        real, parameter :: mw_pcg4_b_c_aer = 250.0
        real, parameter :: mw_pcg5_b_c_aer = 250.0
        real, parameter :: mw_pcg6_b_c_aer = 250.0
        real, parameter :: mw_pcg7_b_c_aer = 250.0
        real, parameter :: mw_pcg8_b_c_aer = 250.0
        real, parameter :: mw_pcg9_b_c_aer = 250.0
        real, parameter :: mw_pcg1_b_o_aer = 250.0
        real, parameter :: mw_pcg2_b_o_aer = 250.0
        real, parameter :: mw_pcg3_b_o_aer = 250.0
        real, parameter :: mw_pcg4_b_o_aer = 250.0
        real, parameter :: mw_pcg5_b_o_aer = 250.0
        real, parameter :: mw_pcg6_b_o_aer = 250.0
        real, parameter :: mw_pcg7_b_o_aer = 250.0
        real, parameter :: mw_pcg8_b_o_aer = 250.0
        real, parameter :: mw_pcg9_b_o_aer = 250.0
        real, parameter :: mw_opcg1_b_c_aer = 250.0
        real, parameter :: mw_opcg2_b_c_aer = 250.0
        real, parameter :: mw_opcg3_b_c_aer = 250.0
        real, parameter :: mw_opcg4_b_c_aer = 250.0
        real, parameter :: mw_opcg5_b_c_aer = 250.0
        real, parameter :: mw_opcg6_b_c_aer = 250.0
        real, parameter :: mw_opcg7_b_c_aer = 250.0
        real, parameter :: mw_opcg8_b_c_aer = 250.0
        real, parameter :: mw_opcg1_b_o_aer = 250.0
        real, parameter :: mw_opcg2_b_o_aer = 250.0
        real, parameter :: mw_opcg3_b_o_aer = 250.0
        real, parameter :: mw_opcg4_b_o_aer = 250.0
        real, parameter :: mw_opcg5_b_o_aer = 250.0
        real, parameter :: mw_opcg6_b_o_aer = 250.0
        real, parameter :: mw_opcg7_b_o_aer = 250.0
        real, parameter :: mw_opcg8_b_o_aer = 250.0
        real, parameter :: mw_pcg1_f_c_aer = 250.0
        real, parameter :: mw_pcg2_f_c_aer = 250.0
        real, parameter :: mw_pcg3_f_c_aer = 250.0
        real, parameter :: mw_pcg4_f_c_aer = 250.0
        real, parameter :: mw_pcg5_f_c_aer = 250.0
        real, parameter :: mw_pcg6_f_c_aer = 250.0
        real, parameter :: mw_pcg7_f_c_aer = 250.0
        real, parameter :: mw_pcg8_f_c_aer = 250.0
        real, parameter :: mw_pcg9_f_c_aer = 250.0
        real, parameter :: mw_pcg1_f_o_aer = 250.0
        real, parameter :: mw_pcg2_f_o_aer = 250.0
        real, parameter :: mw_pcg3_f_o_aer = 250.0
        real, parameter :: mw_pcg4_f_o_aer = 250.0
        real, parameter :: mw_pcg5_f_o_aer = 250.0
        real, parameter :: mw_pcg6_f_o_aer = 250.0
        real, parameter :: mw_pcg7_f_o_aer = 250.0
        real, parameter :: mw_pcg8_f_o_aer = 250.0
        real, parameter :: mw_pcg9_f_o_aer = 250.0
        real, parameter :: mw_opcg1_f_c_aer = 250.0
        real, parameter :: mw_opcg2_f_c_aer = 250.0
        real, parameter :: mw_opcg3_f_c_aer = 250.0
        real, parameter :: mw_opcg4_f_c_aer = 250.0
        real, parameter :: mw_opcg5_f_c_aer = 250.0
        real, parameter :: mw_opcg6_f_c_aer = 250.0
        real, parameter :: mw_opcg7_f_c_aer = 250.0
        real, parameter :: mw_opcg8_f_c_aer = 250.0
        real, parameter :: mw_opcg1_f_o_aer = 250.0
        real, parameter :: mw_opcg2_f_o_aer = 250.0
        real, parameter :: mw_opcg3_f_o_aer = 250.0
        real, parameter :: mw_opcg4_f_o_aer = 250.0
        real, parameter :: mw_opcg5_f_o_aer = 250.0
        real, parameter :: mw_opcg6_f_o_aer = 250.0
        real, parameter :: mw_opcg7_f_o_aer = 250.0
        real, parameter :: mw_opcg8_f_o_aer = 250.0
        real, parameter :: mw_smpa_aer = 250.0
        real, parameter :: mw_smpbb_aer = 250.0
!       real, parameter :: mw_iepox_aer = 118.0
!       real, parameter :: mw_iepoxos_aer = 216.0
!       real, parameter :: mw_tetrol_aer = 136.0
!       real, parameter :: mw_gly_aer = 58.0
        real, parameter :: mw_ant1_c_aer = 250.0
        real, parameter :: mw_ant2_c_aer = 250.0
        real, parameter :: mw_ant3_c_aer = 250.0
        real, parameter :: mw_ant4_c_aer = 250.0
        real, parameter :: mw_ant1_o_aer = 250.0
        real, parameter :: mw_ant2_o_aer = 250.0
        real, parameter :: mw_ant3_o_aer = 250.0
        real, parameter :: mw_ant4_o_aer = 250.0
        real, parameter :: mw_biog1_c_aer = 250.0
        real, parameter :: mw_biog2_c_aer = 250.0
        real, parameter :: mw_biog3_c_aer = 250.0
        real, parameter :: mw_biog4_c_aer = 250.0
        real, parameter :: mw_biog1_o_aer = 250.0
        real, parameter :: mw_biog2_o_aer = 250.0
        real, parameter :: mw_biog3_o_aer = 250.0
        real, parameter :: mw_biog4_o_aer = 250.0

        real, parameter :: mw_glysoa_r1_aer   = 250.0
        real, parameter :: mw_glysoa_r2_aer   = 250.0
        real, parameter :: mw_glysoa_oh_aer   = 250.0
        real, parameter :: mw_glysoa_nh4_aer   = 250.0
        real, parameter :: mw_glysoa_sfc_aer   = 250.0
        real, parameter :: mw_asoaX_aer = 250.0
        real, parameter :: mw_asoa1_aer = 250.0
        real, parameter :: mw_asoa2_aer = 250.0
        real, parameter :: mw_asoa3_aer = 250.0
        real, parameter :: mw_asoa4_aer = 250.0
        real, parameter :: mw_bsoaX_aer = 250.0
        real, parameter :: mw_bsoa1_aer = 250.0
        real, parameter :: mw_bsoa2_aer = 250.0
        real, parameter :: mw_bsoa3_aer = 250.0
        real, parameter :: mw_bsoa4_aer = 250.0

!   aerosol component hygroscopicities (dimensionless)
        real, parameter :: hygro_so4_aer = 0.5
        real, parameter :: hygro_no3_aer = 0.5
        real, parameter :: hygro_ca_aer  = 0.1
        real, parameter :: hygro_co3_aer = 0.1
        real, parameter :: hygro_nh4_aer = 0.5
        real, parameter :: hygro_msa_aer = 0.58
        real, parameter :: hygro_cl_aer  = 1.16
        real, parameter :: hygro_na_aer  = 1.16
        real, parameter :: hygro_oin_aer = 0.068 ! = cam5/mam value, previously 0.14
!      !real, parameter :: hygro_dust_aer = 0.14
!       real, parameter :: hygro_dust_aer = 0.10
!       real, parameter :: hygro_tr1r1_aer = 0.10
!       real, parameter :: hygro_tr1r2_aer = 0.10
!       real, parameter :: hygro_tr1r3_aer = 0.10
!       real, parameter :: hygro_tr1r4_aer = 0.10
        real, parameter :: hygro_oc_aer  = 1.0e-4 ! previously 0.20
        real, parameter :: hygro_bc_aer  = 1.0e-6
        real, parameter :: hygro_smpa_aer  = 0.10
        real, parameter :: hygro_smpbb_aer  = 0.140
!       real, parameter :: hygro_iepox_aer  = 0.10
!       real, parameter :: hygro_iepoxos_aer  = 0.10
!       real, parameter :: hygro_tetrol_aer  = 0.10
!       real, parameter :: hygro_gly_aer  = 0.10
!       real, parameter :: hygro_tanv_aer  = 0.10
!       real, parameter :: hygro_sopnv_aer  = 0.10
!       real, parameter :: hygro_ternv_aer  = 0.10
!       real, parameter :: hygro_seqnv_aer  = 0.10
!       real, parameter :: hygro_sianv_aer  = 0.10
        real, parameter :: hygro_pcg1_b_c_aer = 0.04
        real, parameter :: hygro_pcg2_b_c_aer = 0.04
        real, parameter :: hygro_pcg3_b_c_aer = 0.04
        real, parameter :: hygro_pcg4_b_c_aer = 0.04
        real, parameter :: hygro_pcg5_b_c_aer = 0.04
        real, parameter :: hygro_pcg6_b_c_aer = 0.04
        real, parameter :: hygro_pcg7_b_c_aer = 0.04
        real, parameter :: hygro_pcg8_b_c_aer = 0.04
        real, parameter :: hygro_pcg9_b_c_aer = 0.04
        real, parameter :: hygro_pcg1_b_o_aer = 0.04
        real, parameter :: hygro_pcg2_b_o_aer = 0.04
        real, parameter :: hygro_pcg3_b_o_aer = 0.04
        real, parameter :: hygro_pcg4_b_o_aer = 0.04
        real, parameter :: hygro_pcg5_b_o_aer = 0.04
        real, parameter :: hygro_pcg6_b_o_aer = 0.04
        real, parameter :: hygro_pcg7_b_o_aer = 0.04
        real, parameter :: hygro_pcg8_b_o_aer = 0.04
        real, parameter :: hygro_pcg9_b_o_aer = 0.04
        real, parameter :: hygro_opcg1_b_c_aer = 0.10
        real, parameter :: hygro_opcg2_b_c_aer = 0.10
        real, parameter :: hygro_opcg3_b_c_aer = 0.10
        real, parameter :: hygro_opcg4_b_c_aer = 0.10
        real, parameter :: hygro_opcg5_b_c_aer = 0.10
        real, parameter :: hygro_opcg6_b_c_aer = 0.10
        real, parameter :: hygro_opcg7_b_c_aer = 0.10
        real, parameter :: hygro_opcg8_b_c_aer = 0.10
        real, parameter :: hygro_opcg1_b_o_aer = 0.10
        real, parameter :: hygro_opcg2_b_o_aer = 0.10
        real, parameter :: hygro_opcg3_b_o_aer = 0.10
        real, parameter :: hygro_opcg4_b_o_aer = 0.10
        real, parameter :: hygro_opcg5_b_o_aer = 0.10
        real, parameter :: hygro_opcg6_b_o_aer = 0.10
        real, parameter :: hygro_opcg7_b_o_aer = 0.10
        real, parameter :: hygro_opcg8_b_o_aer = 0.10
        real, parameter :: hygro_pcg1_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg2_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg3_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg4_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg5_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg6_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg7_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg8_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg9_f_c_aer = 1.0e-6
        real, parameter :: hygro_pcg1_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg2_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg3_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg4_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg5_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg6_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg7_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg8_f_o_aer = 1.0e-6
        real, parameter :: hygro_pcg9_f_o_aer = 1.0e-6
        real, parameter :: hygro_opcg1_f_c_aer = 0.10
        real, parameter :: hygro_opcg2_f_c_aer = 0.10
        real, parameter :: hygro_opcg3_f_c_aer = 0.10
        real, parameter :: hygro_opcg4_f_c_aer = 0.10
        real, parameter :: hygro_opcg5_f_c_aer = 0.10
        real, parameter :: hygro_opcg6_f_c_aer = 0.10
        real, parameter :: hygro_opcg7_f_c_aer = 0.10
        real, parameter :: hygro_opcg8_f_c_aer = 0.10
        real, parameter :: hygro_opcg1_f_o_aer = 0.10
        real, parameter :: hygro_opcg2_f_o_aer = 0.10
        real, parameter :: hygro_opcg3_f_o_aer = 0.10
        real, parameter :: hygro_opcg4_f_o_aer = 0.10
        real, parameter :: hygro_opcg5_f_o_aer = 0.10
        real, parameter :: hygro_opcg6_f_o_aer = 0.10
        real, parameter :: hygro_opcg7_f_o_aer = 0.10
        real, parameter :: hygro_opcg8_f_o_aer = 0.10
        real, parameter :: hygro_ant1_c_aer = 0.10
        real, parameter :: hygro_ant2_c_aer = 0.10
        real, parameter :: hygro_ant3_c_aer = 0.10
        real, parameter :: hygro_ant4_c_aer = 0.10
        real, parameter :: hygro_ant1_o_aer = 0.10
        real, parameter :: hygro_ant2_o_aer = 0.10
        real, parameter :: hygro_ant3_o_aer = 0.10
        real, parameter :: hygro_ant4_o_aer = 0.10
        real, parameter :: hygro_biog1_c_aer = 0.10
        real, parameter :: hygro_biog2_c_aer = 0.10
        real, parameter :: hygro_biog3_c_aer = 0.10
        real, parameter :: hygro_biog4_c_aer = 0.10
        real, parameter :: hygro_biog1_o_aer = 0.10
        real, parameter :: hygro_biog2_o_aer = 0.10
        real, parameter :: hygro_biog3_o_aer = 0.10
        real, parameter :: hygro_biog4_o_aer = 0.10

        real, parameter :: hygro_glysoa_r1_aer   = 0.14
        real, parameter :: hygro_glysoa_r2_aer   = 0.14
        real, parameter :: hygro_glysoa_oh_aer   = 0.14
        real, parameter :: hygro_glysoa_nh4_aer   = 0.14
        real, parameter :: hygro_glysoa_sfc_aer   = 0.14
        real, parameter :: hygro_asoaX_aer = 0.14
        real, parameter :: hygro_asoa1_aer = 0.14
        real, parameter :: hygro_asoa2_aer = 0.14
        real, parameter :: hygro_asoa3_aer = 0.14
        real, parameter :: hygro_asoa4_aer = 0.14
        real, parameter :: hygro_bsoaX_aer = 0.14
        real, parameter :: hygro_bsoa1_aer = 0.14
        real, parameter :: hygro_bsoa2_aer = 0.14
        real, parameter :: hygro_bsoa3_aer = 0.14
        real, parameter :: hygro_bsoa4_aer = 0.14

!   these indices give the location in the "mastercomp list" of
!   the different aerosol chemical (or tracer) components
        integer, save :: mastercompindx_so4_aer = -999888777
        integer, save :: mastercompindx_no3_aer = -999888777
        integer, save :: mastercompindx_cl_aer  = -999888777
        integer, save :: mastercompindx_msa_aer = -999888777
        integer, save :: mastercompindx_co3_aer = -999888777
        integer, save :: mastercompindx_nh4_aer = -999888777
        integer, save :: mastercompindx_na_aer  = -999888777
        integer, save :: mastercompindx_ca_aer  = -999888777
        integer, save :: mastercompindx_oin_aer = -999888777
!       integer, save :: mastercompindx_dust_aer = -999888777
!       integer, save :: mastercompindx_tr1r1_aer = -999888777
!       integer, save :: mastercompindx_tr1r2_aer = -999888777
!       integer, save :: mastercompindx_tr1r3_aer = -999888777
!       integer, save :: mastercompindx_tr1r4_aer = -999888777
        integer, save :: mastercompindx_oc_aer  = -999888777
        integer, save :: mastercompindx_bc_aer  = -999888777
!       integer, save :: mastercompindx_tanv_aer  = -999888777
!       integer, save :: mastercompindx_sopnv_aer  = -999888777
!       integer, save :: mastercompindx_ternv_aer  = -999888777
!       integer, save :: mastercompindx_seqnv_aer  = -999888777
!       integer, save :: mastercompindx_sianv_aer  = -999888777
        integer, save :: mastercompindx_pcg1_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg2_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg3_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg4_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg5_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg6_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg7_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg8_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg9_b_c_aer = -999888777
        integer, save :: mastercompindx_pcg1_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg2_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg3_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg4_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg5_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg6_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg7_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg8_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg9_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg1_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg2_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg3_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg4_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg5_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg6_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg7_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg8_b_c_aer = -999888777
        integer, save :: mastercompindx_opcg1_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg2_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg3_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg4_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg5_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg6_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg7_b_o_aer = -999888777
        integer, save :: mastercompindx_opcg8_b_o_aer = -999888777
        integer, save :: mastercompindx_pcg1_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg2_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg3_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg4_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg5_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg6_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg7_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg8_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg9_f_c_aer = -999888777
        integer, save :: mastercompindx_pcg1_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg2_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg3_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg4_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg5_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg6_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg7_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg8_f_o_aer = -999888777
        integer, save :: mastercompindx_pcg9_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg1_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg2_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg3_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg4_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg5_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg6_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg7_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg8_f_c_aer = -999888777
        integer, save :: mastercompindx_opcg1_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg2_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg3_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg4_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg5_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg6_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg7_f_o_aer = -999888777
        integer, save :: mastercompindx_opcg8_f_o_aer = -999888777
        integer, save :: mastercompindx_smpa_aer = -999888777
        integer, save :: mastercompindx_smpbb_aer = -999888777
!       integer, save :: mastercompindx_iepox_aer = -999888777
!       integer, save :: mastercompindx_iepoxos_aer = -999888777
!       integer, save :: mastercompindx_tetrol_aer = -999888777
!       integer, save :: mastercompindx_gly_aer = -999888777
        integer, save :: mastercompindx_ant1_c_aer = -999888777
        integer, save :: mastercompindx_ant2_c_aer = -999888777
        integer, save :: mastercompindx_ant3_c_aer = -999888777
        integer, save :: mastercompindx_ant4_c_aer = -999888777
        integer, save :: mastercompindx_ant1_o_aer = -999888777
        integer, save :: mastercompindx_ant2_o_aer = -999888777
        integer, save :: mastercompindx_ant3_o_aer = -999888777
        integer, save :: mastercompindx_ant4_o_aer = -999888777
        integer, save :: mastercompindx_biog1_c_aer = -999888777
        integer, save :: mastercompindx_biog2_c_aer = -999888777
        integer, save :: mastercompindx_biog3_c_aer = -999888777
        integer, save :: mastercompindx_biog4_c_aer = -999888777
        integer, save :: mastercompindx_biog1_o_aer = -999888777
        integer, save :: mastercompindx_biog2_o_aer = -999888777
        integer, save :: mastercompindx_biog3_o_aer = -999888777
        integer, save :: mastercompindx_biog4_o_aer = -999888777

        integer, save :: mastercompindx_glysoa_r1_aer = -999888777
        integer, save :: mastercompindx_glysoa_r2_aer = -999888777
        integer, save :: mastercompindx_glysoa_oh_aer = -999888777
        integer, save :: mastercompindx_glysoa_nh4_aer = -999888777
        integer, save :: mastercompindx_glysoa_sfc_aer = -999888777
        integer, save :: mastercompindx_asoaX_aer = -999888777
        integer, save :: mastercompindx_asoa1_aer = -999888777
        integer, save :: mastercompindx_asoa2_aer = -999888777
        integer, save :: mastercompindx_asoa3_aer = -999888777
        integer, save :: mastercompindx_asoa4_aer = -999888777
        integer, save :: mastercompindx_bsoaX_aer = -999888777
        integer, save :: mastercompindx_bsoa1_aer = -999888777
        integer, save :: mastercompindx_bsoa2_aer = -999888777
        integer, save :: mastercompindx_bsoa3_aer = -999888777
        integer, save :: mastercompindx_bsoa4_aer = -999888777

! the lptr values are the locations in the chem array of 
! the various aerosol components
        integer, save ::                     &
          lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_msa_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_cl_aer(maxd_asize, maxd_atype, maxd_aphase),       &
          lptr_co3_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_na_aer(maxd_asize, maxd_atype, maxd_aphase),       &
          lptr_ca_aer(maxd_asize, maxd_atype, maxd_aphase),       &
          lptr_oin_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_aro1_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_aro2_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_alk1_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_ole1_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_api1_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_api2_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_lim1_aer(maxd_asize, maxd_atype, maxd_aphase),     &
          lptr_lim2_aer(maxd_asize, maxd_atype, maxd_aphase),     &
!         lptr_dust_aer(maxd_asize, maxd_atype, maxd_aphase),     &
!         lptr_tr1r1_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_tr1r2_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_tr1r3_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_tr1r4_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_oc_aer(maxd_asize, maxd_atype, maxd_aphase),       &
          lptr_bc_aer(maxd_asize, maxd_atype, maxd_aphase),       &
!         lptr_tanv_aer(maxd_asize, maxd_atype, maxd_aphase),     &
!         lptr_sopnv_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_ternv_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_seqnv_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_sianv_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_pcg1_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg2_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg3_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg4_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg5_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg6_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg7_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg8_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg9_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg1_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg2_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg3_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg4_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg5_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg6_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg7_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg8_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg9_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg1_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg2_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg3_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg4_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg5_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg6_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg7_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg8_b_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg1_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg2_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg3_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg4_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg5_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg6_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg7_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg8_b_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg1_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg2_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg3_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg4_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg5_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg6_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg7_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg8_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg9_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg1_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg2_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg3_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg4_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg5_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg6_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg7_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg8_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_pcg9_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg1_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg2_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg3_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg4_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg5_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg6_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg7_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg8_f_c_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg1_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg2_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg3_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg4_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg5_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg6_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg7_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_opcg8_f_o_aer(maxd_asize, maxd_atype, maxd_aphase), &
          lptr_smpa_aer(maxd_asize, maxd_atype, maxd_aphase),      &
          lptr_smpbb_aer(maxd_asize, maxd_atype, maxd_aphase),     &
!         lptr_iepox_aer(maxd_asize, maxd_atype, maxd_aphase),     &
!         lptr_iepoxos_aer(maxd_asize, maxd_atype, maxd_aphase),   &
!         lptr_tetrol_aer(maxd_asize, maxd_atype, maxd_aphase),    &
!         lptr_gly_aer(maxd_asize, maxd_atype, maxd_aphase),       &
          lptr_ant1_c_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant2_c_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant3_c_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant4_c_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant1_o_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant2_o_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant3_o_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_ant4_o_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_biog1_c_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog2_c_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog3_c_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog4_c_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog1_o_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog2_o_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog3_o_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_biog4_o_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_glysoa_r1_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_glysoa_r2_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_glysoa_oh_aer(maxd_asize, maxd_atype, maxd_aphase),   &
          lptr_glysoa_nh4_aer(maxd_asize, maxd_atype, maxd_aphase),  &
          lptr_glysoa_sfc_aer(maxd_asize, maxd_atype, maxd_aphase),  &
          lptr_asoaX_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_asoa1_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_asoa2_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_asoa3_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_asoa4_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_bsoaX_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_bsoa1_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_bsoa2_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_bsoa3_aer(maxd_asize, maxd_atype, maxd_aphase),    &
          lptr_bsoa4_aer(maxd_asize, maxd_atype, maxd_aphase)


      end module module_data_mosaic_asecthp

