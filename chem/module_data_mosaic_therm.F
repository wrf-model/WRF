!************************************************************************
! This computer software was prepared by Battelle Memorial Institute,
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of
! use
!************************************************************************
!
! 2017-10-07 r.c.easter changes for new (openmp) mosaic version
!
! nbin_a is now in module_data_mosaic_aero
! nbin_a_maxd is set to nbin_a_max of module_data_mosaic_aero
!
!-----------------------------------------------------------------------

      module module_data_mosaic_therm


      use module_data_mosaic_aero, only:  nbin_a_max, nbin_a


      implicit none


! mosaic.h (mosaic.22.0)
!   30-apr-07 raz - deleted alpha_gas
!   05-feb-07 wig - converted reals to double
!   10-jan-07 raz - compatible with mosaic.21.0
!   19-dec-05 raz - compatible with mosaic.16.4
!   27-apr-05 raz - compatible with mosaic.14.3
!   07-jan-05 raz - updated and cleaned up variable lists
!   08-jul-03 raz - updated many variables
!   07-aug-02 rce - this is rahul's latest version from freshair
!   19-aug-02 raz - declared mass_soluble_a and kg as real
!   07-oct-02 raz - declared zc and za as integer
!   09-oct-02 raz - explicitly declared all variables
!   29-oct-02 raz - defined naercomp as the total number of aerosol compounds
!----------------------------------------------------------------------

! nbin_a_maxd = maximum num of aerosol bins and is used to dimension arrays
!     integer, parameter :: nbin_a_maxd = 8
      integer, parameter :: nbin_a_maxd = nbin_a_max
! nbin_a = number of bins that are used in a run
! (nbin_a is set at run time, and must be <= nbin_a_maxd)
!     integer, save :: nbin_a = 999888777

! mosaic-specific parameters
! Alma added 2 smp species - just before the traditional ant1_c 
! SOA is treated using a simplified approach for anthropogenic and biomass burning species based on Hodzic and Jimenez, GMD, 2011
      integer ngas_ioa, ngas_soa, ngas_volatile, ngas_het,	&
              naer, naercomp, nelectrolyte, nsalt,	&
              nsoluble, ncation, nanion
      parameter(ngas_ioa = 5)	! inorganic volatile aerosol species that have a gaseous counterpart
!      parameter(ngas_soa = 68+2+16)	! volatile soa species that have a gaseous counterpart
!      parameter(ngas_volatile = ngas_ioa + ngas_soa)
!      parameter(ngas_het = 2)   ! gas species only involved in heterogeneous reactions ! DL - 9/9/2011
!      parameter(naer = 11+68+2+16)	! num of chemical species per bin (inorg + oc + bc + oin + soa)
!      parameter(naercomp = 26+68+2+16)	! num of electrolytes + oc, bc, oin, & soa
! 10 new VBS species
      parameter(ngas_soa = 68+2+16+10)	! volatile soa species that have a gaseous counterpart
! OH and glyoxal at the end
      parameter(ngas_volatile = ngas_ioa + ngas_soa + 1 + 1)
      parameter(ngas_het = 2)   ! gas species only involved in heterogeneous reactions ! DL - 9/9/2011
! 5 glyoxal SOA species, 10 VBS species
      parameter(naer = 11+68+2+16+5+10)	! num of chemical species per bin (inorg + oc + bc + oin + soa) + 5*glysoa + 10*VBS
      parameter(naercomp = 26+68+2+16+5+10)	! num of electrolytes + oc, bc, oin, & soa + 5*glysoa + 10*VBS
      parameter(nelectrolyte = 22) ! num of electrolytes
      parameter(nsalt    = 15)	! num of soluble salts
      parameter(nsoluble = 20)	! num of soluble electrolytes
      parameter(ncation = 4)	! num of cations
      parameter(nanion  = 5)	! num of anions

      integer nrxn_aer_gl, nrxn_aer_ll, nrxn_aer_sg, nrxn_aer_sl
      parameter(nrxn_aer_gl = 4) ! num of gas-liquid equilibria
      parameter(nrxn_aer_ll = 3) ! num of liquid-liquid equilibria
      parameter(nrxn_aer_sg = 2) ! num of solid-gas equilibria
      parameter(nrxn_aer_sl = nsalt)! num of solid-liquid equilibria

      integer mmodal, msection,   &
              mon, moff, myes, mno
      parameter(mmodal  = 1)	! modal size distribution framework
      parameter(msection= 2)	! sectional size distribution framework
      parameter(mon     = 1)	! flag: on
      parameter(moff    = 0)    ! flag:off
      parameter(myes	= mon)	! flag: yes or true
      parameter(mno	= moff)	! flag: no or false


      integer jtotal, jsolid, jliquid
      parameter(jsolid = 1)
      parameter(jliquid= 2)
      parameter(jtotal = 3)

      integer jhyst_lo, jhyst_up
      parameter(jhyst_lo = 0)	! lower hysteresis leg
      parameter(jhyst_up = 1) 	! upper hysteresis leg

  
      ! allowable values for mhyst_method
      integer, parameter :: mhyst_uporlo_waterhyst = 2	! select upper/lower using "3-d method" involving water_a_hyst
      integer, parameter :: mhyst_force_up = 3	        ! force upper water hysteresis leg
      integer, parameter :: mhyst_force_lo = 4	        ! force lower water hysteresis leg
      integer, parameter :: mhyst_method = mhyst_uporlo_waterhyst
  
      real(kind=8), parameter :: xhyst_up_crustal_thresh = 0.30
      ! when rh_crystal < rh < rh_deliquesce and bin mass fraction of crustal species (oin, caco3, and caso4) 
      !    exceeds xhyst_up_crustal_thresh, the bin is not allowed to be on the upper hysteresis curve.
      ! mineral dust is mostly externally mixed, so a minor fraction of mineral dust species 
      !   will most likely reside in a minor fraction of the particles of the bin, 
      !   and should not control the bin aerosol water.  (value = 0.0 in previous versions of mosaic)
  
      integer, parameter :: mwater_kappa_nonelectro = 1  ! when > 0, use kappa_nonelectro for oin, oc, soa aerosol water
  
      integer no_aerosol, all_solid, all_liquid, mixed
      parameter(no_aerosol = 0)	! flag
      parameter(all_solid  = 1) ! flag
      parameter(all_liquid = 2) ! flag
      parameter(mixed      = 3)	! flag

      integer soluble, insoluble
      parameter(soluble   = 1)  ! flag
      parameter(insoluble = 2)  ! flag

      real(kind=8) mass_cutoff
      parameter(mass_cutoff = 1.d-15)	! ng/m^3


!----------------------------------------------------------------------
! mosaic species indices
!
! gas
      integer, save ::   &
       ih2so4_g,     ihno3_g,      ihcl_g,      inh3_g,   &
       imsa_g, in2o5_g, iclno2_g

      integer, save ::   &
      ipcg1_b_c_g,ipcg2_b_c_g,ipcg3_b_c_g,ipcg4_b_c_g, &
      ipcg5_b_c_g,ipcg6_b_c_g,ipcg7_b_c_g,ipcg8_b_c_g, &
      ipcg9_b_c_g,ipcg1_b_o_g,ipcg2_b_o_g,ipcg3_b_o_g, &
      ipcg4_b_o_g,ipcg5_b_o_g,ipcg6_b_o_g,ipcg7_b_o_g, &
      ipcg8_b_o_g,ipcg9_b_o_g,iopcg1_b_c_g,iopcg2_b_c_g,&
      iopcg3_b_c_g, iopcg4_b_c_g,iopcg5_b_c_g,iopcg6_b_c_g,&
      iopcg7_b_c_g,iopcg8_b_c_g,iopcg1_b_o_g,iopcg2_b_o_g,&
      iopcg3_b_o_g,iopcg4_b_o_g,iopcg5_b_o_g,iopcg6_b_o_g,&
      iopcg7_b_o_g,iopcg8_b_o_g,&
      ipcg1_f_c_g,ipcg2_f_c_g,ipcg3_f_c_g,ipcg4_f_c_g, &
      ipcg5_f_c_g,ipcg6_f_c_g,ipcg7_f_c_g,ipcg8_f_c_g, &
      ipcg9_f_c_g,ipcg1_f_o_g,ipcg2_f_o_g,ipcg3_f_o_g, &
      ipcg4_f_o_g,ipcg5_f_o_g,ipcg6_f_o_g,ipcg7_f_o_g, &
      ipcg8_f_o_g,ipcg9_f_o_g,iopcg1_f_c_g,iopcg2_f_c_g,&
      iopcg3_f_c_g, iopcg4_f_c_g,iopcg5_f_c_g,iopcg6_f_c_g,&
      iopcg7_f_c_g,iopcg8_f_c_g,iopcg1_f_o_g,iopcg2_f_o_g,&
      iopcg3_f_o_g,iopcg4_f_o_g,iopcg5_f_o_g,iopcg6_f_o_g,&
      iopcg7_f_o_g,iopcg8_f_o_g,iant1_c_g,iant2_c_g,iant3_c_g, &
      iant4_c_g,ibiog1_c_g,ibiog2_c_g,ibiog3_c_g,ibiog4_c_g, &
      iant1_o_g,iant2_o_g,iant3_o_g, &
      iant4_o_g,ibiog1_o_g,ibiog2_o_g,ibiog3_o_g,ibiog4_o_g, &
      ismpa_g,ismpbb_g, &
      iasoaX_g, iasoa1_g, iasoa2_g, iasoa3_g, iasoa4_g, &
      ibsoaX_g, ibsoa1_g, ibsoa2_g, ibsoa3_g, ibsoa4_g, &
      igly, iho


! aerosol generic
      integer, save ::   &
       iso4_a,     ino3_a,     icl_a,     inh4_a,     ico3_a,  &
       imsa_a,     ina_a,      ica_a,     ioc_a,      ibc_a,   &
       ioin_a

      integer, save ::   &
      ipcg1_b_c_a,ipcg2_b_c_a,ipcg3_b_c_a,ipcg4_b_c_a, &
      ipcg5_b_c_a,ipcg6_b_c_a,ipcg7_b_c_a,ipcg8_b_c_a, &
      ipcg9_b_c_a,ipcg1_b_o_a,ipcg2_b_o_a,ipcg3_b_o_a, &
      ipcg4_b_o_a,ipcg5_b_o_a,ipcg6_b_o_a,ipcg7_b_o_a, &
      ipcg8_b_o_a,ipcg9_b_o_a,iopcg1_b_c_a,iopcg2_b_c_a,&
      iopcg3_b_c_a, iopcg4_b_c_a,iopcg5_b_c_a,iopcg6_b_c_a,&
      iopcg7_b_c_a,iopcg8_b_c_a,iopcg1_b_o_a,iopcg2_b_o_a,&
      iopcg3_b_o_a,iopcg4_b_o_a,iopcg5_b_o_a,iopcg6_b_o_a,&
      iopcg7_b_o_a,iopcg8_b_o_a,&
      ipcg1_f_c_a,ipcg2_f_c_a,ipcg3_f_c_a,ipcg4_f_c_a, &
      ipcg5_f_c_a,ipcg6_f_c_a,ipcg7_f_c_a,ipcg8_f_c_a, &
      ipcg9_f_c_a,ipcg1_f_o_a,ipcg2_f_o_a,ipcg3_f_o_a, &
      ipcg4_f_o_a,ipcg5_f_o_a,ipcg6_f_o_a,ipcg7_f_o_a, &
      ipcg8_f_o_a,ipcg9_f_o_a,iopcg1_f_c_a,iopcg2_f_c_a,&
      iopcg3_f_c_a, iopcg4_f_c_a,iopcg5_f_c_a,iopcg6_f_c_a,&
      iopcg7_f_c_a,iopcg8_f_c_a,iopcg1_f_o_a,iopcg2_f_o_a,&
      iopcg3_f_o_a,iopcg4_f_o_a,iopcg5_f_o_a,iopcg6_f_o_a,&
      iopcg7_f_o_a,iopcg8_f_o_a, &
      ismpa_a,ismpbb_a, &
      iglysoa_r1_a, iglysoa_r2_a, iglysoa_oh_a, iglysoa_sfc_a, iglysoa_nh4_a, &
      iant1_c_a,iant2_c_a,iant3_c_a, &
      iant4_c_a,ibiog1_c_a,ibiog2_c_a,ibiog3_c_a,ibiog4_c_a, &
      iant1_o_a,iant2_o_a,iant3_o_a, &
      iant4_o_a,ibiog1_o_a,ibiog2_o_a,ibiog3_o_a,ibiog4_o_a, &
      iasoaX_a, iasoa1_a,iasoa2_a,iasoa3_a,iasoa4_a,&
      ibsoaX_a, ibsoa1_a,ibsoa2_a,ibsoa3_a,ibsoa4_a


! aerosol elecctrolytes/compounds
      integer, save ::   &
       jnh4so4,    jlvcite,    jnh4hso4,   jnh4no3,    jnh4cl,  &
       jna2so4,    jna3hso4,   jnahso4,    jnano3,     jnacl,   &
       jcaso4,     jcano3,     jcacl2,     jcaco3,     jh2so4,  &
       jhno3,      jhcl,       jhhso4,                          &
       jnh4msa,    jnamsa,     jcamsa2,    jmsa,                &
       joc,        jbc,        join,       jh2o

      integer, save ::   &
      jpcg1_b_c,jpcg2_b_c,jpcg3_b_c,jpcg4_b_c, &
      jpcg5_b_c,jpcg6_b_c,jpcg7_b_c,jpcg8_b_c, &
      jpcg9_b_c,jpcg1_b_o,jpcg2_b_o,jpcg3_b_o, &
      jpcg4_b_o,jpcg5_b_o,jpcg6_b_o,jpcg7_b_o, &
      jpcg8_b_o,jpcg9_b_o,jopcg1_b_c,jopcg2_b_c,&
      jopcg3_b_c, jopcg4_b_c,jopcg5_b_c,jopcg6_b_c,&
      jopcg7_b_c,jopcg8_b_c,jopcg1_b_o,jopcg2_b_o,&
      jopcg3_b_o,jopcg4_b_o,jopcg5_b_o,jopcg6_b_o,&
      jopcg7_b_o,jopcg8_b_o,&
      jpcg1_f_c,jpcg2_f_c,jpcg3_f_c,jpcg4_f_c, &
      jpcg5_f_c,jpcg6_f_c,jpcg7_f_c,jpcg8_f_c, &
      jpcg9_f_c,jpcg1_f_o,jpcg2_f_o,jpcg3_f_o, &
      jpcg4_f_o,jpcg5_f_o,jpcg6_f_o,jpcg7_f_o, &
      jpcg8_f_o,jpcg9_f_o,jopcg1_f_c,jopcg2_f_c,&
      jopcg3_f_c, jopcg4_f_c,jopcg5_f_c,jopcg6_f_c,&
      jopcg7_f_c,jopcg8_f_c,jopcg1_f_o,jopcg2_f_o,&
      jopcg3_f_o,jopcg4_f_o,jopcg5_f_o,jopcg6_f_o,&
      jopcg7_f_o,jopcg8_f_o, &
      jsmpa,jsmpbb, &
      jglysoa_r1, jglysoa_r2, jglysoa_oh, jglysoa_sfc, jglysoa_nh4, &
      jant1_c,jant2_c,jant3_c, &
      jant4_c,jbiog1_c,jbiog2_c,jbiog3_c,jbiog4_c, &
      jant1_o,jant2_o,jant3_o, &
      jant4_o,jbiog1_o,jbiog2_o,jbiog3_o,jbiog4_o, &
      jasoaX,jasoa1,jasoa2,jasoa3,jasoa4,&
      jbsoaX,jbsoa1,jbsoa2,jbsoa3,jbsoa4


! aerosol ions
      integer, save ::   			&
       jc_h,    jc_nh4, jc_na,  jc_ca,		&
       ja_hso4, ja_so4, ja_no3, ja_cl, ja_msa     ! , ja_co3


!----------------------------------------------------------------------
! mosaic variables
      integer, save ::			&
	iclm_aer,			&  ! i-location
	jclm_aer,			&  ! j-location
	kclm_aer,			&  ! k-location
	kclm_aer_calcbgn,		&  ! k-loc for calc. to begin
	kclm_aer_calcend,		&  ! k-loc for calc. to end
	mclm_aer,			&  ! m-subarea
	mgas_aer_xfer,			&  ! flag: mon, moff
	mdynamic_solver,		&  ! flag: masteem, masceem
	msize_framework,		&  ! flag: mmodal, msectional
	jaerosolstate(nbin_a_maxd),	&  ! flag: no_aerosol, all_solid, all_liquid, mixed
	jphase(nbin_a_maxd),		&  ! phase index: jtotal, jsolid, jliquid
	jhyst_leg(nbin_a_maxd),		&  ! hysteresis leg: jhyst_up, jhyst_lo
	iprint_input,			&  ! flag: mon, moff
	lunerr_aer,			&  ! 
	ncorecnt_aer,       &  !
	n2o5_flag				! flag to control N2O5 het chem (0=off, 1=no Cl pathway, 2=full)

! NOTE: Some of the following informational output defaults are overridden in 
!       module_mosaic_driver.F based on the internal MOSAIC debug_level setting.
      integer, save :: istat_mosaic_fe1       
                       ! "fatal error status" for current problem (grid cell)
                       ! negative value means a fatal error has occured
      integer, save :: nfe1_mosaic_cur = 0
                       ! fatal error count for current host-code time step
      integer, save :: nfe1_mosaic_tot = 0
                       ! fatal error count for all time steps
      integer, save :: iprint_mosaic_fe1 = 1
                       ! turns on/off output of fatal error diagnostics & counts
                       !    if iprint_mosaic_fe1 >= 10, mosaic_aerchem_error_dump
                       !    is called for each fatal error
      integer, save :: iprint_mosaic_perform_stats = 1 
                       ! turns on/off output of mosaic performance statistics
      integer, save :: iprint_mosaic_diag1 = 1 
                       ! turns on/off output of other warnings & diagnostics
      integer, save :: iprint_mosaic_input_ok = 1 
                       ! turns on/off output of mosaic initial values
                       ! when a serious error occurs


      real(kind=8), save ::		&
      	num_a(nbin_a_maxd),		&  ! #/cc(air)
      	dpgn_a(nbin_a_maxd),		&  ! cm
      	dp_dry_a(nbin_a_maxd),		&  ! cm
      	dp_wet_a(nbin_a_maxd),		&  ! cm
      	area_dry_a(nbin_a_maxd),	&  ! cm^2/cc(air)
      	area_wet_a(nbin_a_maxd),	&  ! cm^2/cc(air)
	mass_dry_salt(nbin_a_maxd),	&  ! g/cc(air)
      	mass_dry_a(nbin_a_maxd),	&  ! g/cc(air)
      	mass_wet_a(nbin_a_maxd),	&  ! g/cc(air)
      	mass_soluble_a(nbin_a_maxd),	&  ! ng/cc(air)
      	vol_dry_a(nbin_a_maxd),		&  ! cc/cc(air)
      	vol_wet_a(nbin_a_maxd),		&  ! cc/cc(air)
      	dens_dry_a(nbin_a_maxd),	&  ! g/cc
      	dens_wet_a(nbin_a_maxd),	&  ! g/cc
      	sigmag_a(nbin_a_maxd),		&  ! -
      	water_a(nbin_a_maxd), 		&  ! kg(water)/m^3(air)
      	water_a_hyst(nbin_a_maxd),	&  ! kg(water)/m^3(air) hysteresis (at 60% rh)
      	water_a_up(nbin_a_maxd),	&  ! kg(water)/m^3(air) at 60% rh
      	ph(nbin_a_maxd),		&  ! ph
        c_as(nbin_a_maxd),          & ! ammonium sulfate concentration (mol/kg water)
        c_an(nbin_a_maxd),          & ! ammonium nitrate concentration (mol/kg water)
        a_nh4(nbin_a_maxd),         & ! ammonium sulfate activity (mol/kg water)
      	aer(naer,3,nbin_a_maxd),	&  ! nmol/m^3
	aer_sum(3,nbin_a_maxd),		&  ! nmol/m^3
      	aer_percent(naer,3,nbin_a_maxd), &  ! %
      	comp_a(naercomp),		&  ! g/cc(air)
      	electrolyte(nelectrolyte,3,nbin_a_maxd),   &  ! nmol/m^3
      	electrolyte_sum(nelectrolyte,nbin_a_maxd), &  ! nmol/m^3
      	epercent(nelectrolyte,3,nbin_a_maxd),	   &  ! %
      	gas(ngas_volatile+ngas_het),		&  ! nmol/m^3
      	ah2o,				&  ! -
      	ah2o_a(nbin_a_maxd),		&  ! -
      	dpmv(nbin_a_maxd),		&  ! 
      	volume_a(nbin_a_maxd),		&  ! 
	volume_bin(nbin_a_maxd),	&  ! dry volume of one particle
      	kelvin(nbin_a_maxd),		&  ! kelvin factor
	kel(ngas_volatile+ngas_het,nbin_a_maxd),	&  ! kelvin factor for condensing species
	kelvin_nh4no3,			&  ! -
	kelvin_nh4cl,			&  ! - 
	total_species(ngas_volatile)	   ! 


!----------------------------------------------------------------------
! astem variables
      integer, save ::			&
	idry_case3a(nbin_a_maxd),	&  ! mYES, mNO
	ieqblm_bin(nbin_a_maxd),	&  ! myes, mno
	ieqblm_astem,			&  ! myes, mno
        ieqblm_soa,                     &  ! mYES, mNO
	nastem_call,			&  !
	nastem_fail,			&  !
	isteps_astem,			&  !
	nsteps_astem,			&  !
        isteps_SOA,                     &
	nsteps_astem_max,		&  !
	nmax_ASTEM,			&  !
        flagsoap(ngas_soa),             &       ! Added by Manish Shrivastav on 01/11/10
	integrate(ngas_volatile,3,nbin_a_maxd)  ! mYES, mNO


      real(kind=8), save ::			&
	po_soa(ngas_volatile),			&  ! pascal
	sat_soa(ngas_volatile),			&  ! nmol/m^3(air)
	x_soa(naer),				&  ! soa mole fraction
	sfc_a(ngas_volatile),			&  ! nmol/m^3
	Heff(ngas_volatile,nbin_a_maxd),	&  ! 
	kg(ngas_volatile+ngas_het,nbin_a_maxd),		&  ! 1/s
        fraceq(ngas_volatile,nbin_a_maxd),      &  ! 1/s
	df_gas_s(ngas_volatile,nbin_a_maxd),	&  ! nmol/m^3 (g-g*) = driving force)
	df_gas_l(ngas_volatile,nbin_a_maxd),	&  ! nmol/m^3 (g-g*) = driving force)
        df_gas_o(ngas_volatile,nbin_a_maxd),     &  ! nmol/m^3 (G-G*) = driving force)
	flux_s(ngas_volatile,nbin_a_maxd),	&  ! nmol/m^3/s
	flux_l(ngas_volatile,nbin_a_maxd),	&  ! nmol/m^3/s
        flux_o(ngas_volatile,nbin_a_maxd),      &  ! nmol/m^3/s
	sumkg_h2so4,				&  ! 1/s
	sumkg_msa,				&  ! 1/s
	sumkg_nh3,				&  ! 1/s
	sumkg_hno3,				&  ! 1/s
	sumkg_hcl,				&  ! 1/s
	sumkg_n2o5,				&  ! 1/s
	delta_nh3_max(nbin_a_maxd),		&  ! nmol/m^3
	delta_hno3_max(nbin_a_maxd),		&  ! nmol/m^3
	delta_hcl_max(nbin_a_maxd),		&  ! nmol/m^3
	keq_nh4no3,				&  ! -
	keq_nh4cl,				&  ! -
	Keq_nh4no3_0,				&  ! -
	Keq_nh4cl_0,				&  ! -
	volatile_s(ngas_volatile,nbin_a_maxd), 	&  ! nmol/m^3
	phi_volatile_s(ngas_volatile,nbin_a_maxd),	&  ! relative dr. force = (g-g*)/g
	phi_volatile_l(ngas_volatile,nbin_a_maxd),	&  ! relative dr. force = (g-g*)/g
        phi_volatile_o(ngas_volatile,nbin_a_maxd),      &  ! relative dr. force = (g-g*)/g
	phi_nh4no3_s,				&  ! relative dr. force: 0 to 1
	phi_nh4cl_s,				&  ! relative dr. force: 0 to 1
	sum_vdf_s(ngas_volatile),		&  ! (nmol/m^3)^2
	sum_vol_s(ngas_volatile),		&  ! nmol/m^3
	sum_bin_s(ngas_volatile),		&  ! number of bins that have flux_s(iv) < 0
	avg_df_gas_s(ngas_volatile),		&  ! nmol/m^3
	h_s_i_m(ngas_volatile,nbin_a_maxd),	&  ! s
	alpha_astem,				&  ! 0.01 to 0.05
	rtol_eqb_astem,				&  ! 0.01 to 0.03
	ptol_mol_astem,				&  ! 0.01 to 1.0
	nsteps_astem_avg			   !

      integer, parameter :: glysoa_param_off     = 0, &
                            glysoa_param_simple  = 1, &
                            glysoa_param_complex = 2
      integer, save      :: glysoa_param

!----------------------------------------------------------------------
! mesa variables
      integer, save ::   		&
      	jsalt_index(nsalt),		&
      	jsulf_poor(211),		&
      	jsulf_rich(71),			&
      	jsalt_present(nsalt),		&
      	nmax_mesa,			&
      	nmesa_call,			&
      	nmesa_fail,			&
	iter_mesa(nbin_a_maxd),		&
	niter_mesa,			&
      	niter_mesa_max


      real(kind=8), save ::   		&
	eleliquid(nelectrolyte),	&
	flux_sl(nsalt),			&
	phi_salt(nsalt),		&
	phi_salt_old(nsalt),		&
	phi_bar(nsalt),			&
	alpha_salt(nsalt),		&
	sat_ratio(nsalt),		&
	hsalt(nsalt),			&
	hsalt_max,			&
	frac_salt_liq(nsalt),		&
	frac_salt_solid(nsalt),		&
	growth_factor(nbin_a_maxd),	&
	d_mdrh(63,4),			&  ! mdrh(t) poly coeffs
	mdrh(nbin_a_maxd),		&
	mdrh_t(63),			&
	molality0(nelectrolyte),	&
	rtol_mesa,			&
	niter_mesa_avg


!----------------------------------------------------------------------
! mosaic physico-chemical constants
      character(len=8), save ::		&
	ename(nelectrolyte),		&  ! electrolyte names
	aer_name(naer),			&  ! generic aerosol species name
	gas_name(ngas_volatile+ngas_het)		   ! gas species name

      character(len=6), save ::		&
	phasestate(4)


      real(kind=8), save ::			&
	t_k,				&  ! temperature (k)
	p_atm,				&  ! pressure (atm)
	rh_pc,				&  ! relative humidity (%)
	cair_mol_cc,			&  ! air conc in mol/cc
	cair_mol_m3,			&  ! air conc in mol/m^3
	conv1a,				&
	conv1b,				&
	conv2a,				&
	conv2b,				&
	mw_electrolyte(nelectrolyte),	&  ! molecular wt of electrolytes
	mw_aer_mac(naer),		&  ! molecular wt of generic species
	mw_comp_a(naercomp),		&  ! molecular wt of compounds
	mw_c(ncation),			&  ! molecular wt of cations
	mw_a(nanion),			&  ! molecular wt of anions
	dens_electrolyte(nelectrolyte),	&  ! g/cc
	dens_aer_mac(naer),		&  ! g/cc
	dens_comp_a(naercomp),		&  ! g/cc (density of compounds)
	kappa_nonelectro(naer),		&  ! 
	partial_molar_vol(ngas_volatile+ngas_het), & ! cc/mol
	sigma_water,			&  ! water surface tension (n/m)
	sigma_soln(nbin_a_maxd),	&  ! solution surface tension (n/m)
	keq_gl(nrxn_aer_gl),		&  ! gas-liq eqblm const
	keq_ll(nrxn_aer_ll),		&  ! liq-liq eqblm const
	keq_sg(nrxn_aer_sg),		&  ! solid-gas eqbln const
	keq_sl(nrxn_aer_sl), 		&  ! solid-liq eqblm const
	kp_nh3, 			&  !
	kp_nh4no3, 			&  !
	kp_nh4no3_0, 			&  !
	kp_nh4cl,	                &  !
	kp_nh4cl_0,			&   !
	frac_n2o5_h2o(nbin_a_maxd)	! fraction of N2O5 which reacts with H2O after heterogeneous uptake

      complex, save ::			&
      		ref_index_a(naercomp),	&  ! refractive index of compounds
      		ri_avg_a(nbin_a_maxd)	   ! vol avg ref index of bin


!----------------------------------------------------------------------
! mosaic activity coefficient models parameters

      real(kind=8), save ::			&
	mc(ncation,nbin_a_maxd),		&  ! mol/kg(water)
	ma(nanion,nbin_a_maxd),			&  ! mol/kg(water)
	msulf,					&  ! 
	zc(ncation),				&  ! real charge
	za(nanion),				&  ! real charge
	gam(nelectrolyte,nbin_a_maxd),		&
	gam_ratio(nbin_a_maxd),			&
	log_gamz(nelectrolyte,nelectrolyte),	&
	log_gam(nelectrolyte),			&
	activity(nelectrolyte,nbin_a_maxd),	&
	xeq_a(nanion),				&
	xeq_c(ncation),				&
	na_ma(nanion),				&
	nc_mc(ncation),				&
	a_zsr(6,nelectrolyte),			&  ! binary molality polynomial coeffs
	b_zsr(nelectrolyte),			&  ! binary molality coeff
	aw_min(nelectrolyte),			&  ! minimum frh at which molality polynomial can be used
	b_mtem(6,nelectrolyte,nelectrolyte)	   ! mtem poly coeffs


!----------------------------------------------------------------------
! mosaic massbalance variables
      real(kind=8), save ::	&
	tot_so4_in,	&
	tot_no3_in,	&
	tot_cl_in,	&
	tot_nh4_in,	&
	tot_na_in,	&
	tot_ca_in,	&
	tot_so4_out,	&
	tot_no3_out,	&
	tot_cl_out,	&
	tot_nh4_out,	&
	tot_na_out,	&
	tot_ca_out,	&
	diff_so4,	&
	diff_no3,	&
	diff_cl,	&
	diff_nh4,	&
	diff_na,	&
	diff_ca,	&
	reldiff_so4,	&
	reldiff_no3,	&
	reldiff_cl,	&
	reldiff_nh4,	&
	reldiff_na,	&
	reldiff_ca
!----------------------------------------------------------------------



      end module module_data_mosaic_therm
