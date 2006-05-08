!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for information and terms of use
!**********************************************************************************  
      module module_data_mosaic_therm



      implicit none



! mosaic.h (mosaic.14.3)
!   29-apr-05 rce - converted to f90 module; changed nbin_a to nbin_a_maxd;
!	added kclm_aer_calcbgn/end, lunerr_aer, ncorecnt_aer, masbalout_...
!   27-apr-05 raz - compatible with mosaic.14.3
!   07-jan-05 raz - updated and cleaned up variable lists
!   08-jul-03 raz - updated many variables
!   07-aug-02 rce - this is rahul's latest version from freshair
!   19-aug-02 raz - declared mass_soluble_a and kg as real
!   07-oct-02 raz - declared zc and za as integer
!   09-oct-02 raz - explicitly declared all variables
!   29-oct-02 raz - defined naercomp as the total number of aerosol compounds
!----------------------------------------------------------------------

! rce 11-sep-2004
! nbin_a_maxd = maximum num of aerosol bins and is used to dimension arrays
      integer, parameter :: nbin_a_maxd = 8
! nbin_a = ntot_amode = number of bins that are used in a run
! (nbin_a is set = ntot_amode at run time, and must be <= nbin_a_maxd)
      integer, save :: nbin_a = 999888777


! mosaic-specific parameters
      integer naer_vol, naer, naercomp, nelectrolyte, nsalt,   &
       nsoluble, ncation, nanion, ncation_clegg, nanion_clegg
      parameter(naer_vol = 4)	! volatile aerosol species that have a gaseous counterpart
      parameter(naer = 11)	! num of chemical species per bin
      parameter(naercomp = 22)	! num of electrolytes + oc, bc, and oin
      parameter(nelectrolyte = 18) ! num of electrolytes
      parameter(nsalt   = 12)	! num of soluble salts
      parameter(nsoluble = 16)	! num of soluble electrolytes
      parameter(ncation = 4)	! num of cations
      parameter(nanion  = 4)	! num of anions
      parameter(ncation_clegg = ncation) ! num of cations in clegg's model
      parameter(nanion_clegg  = nanion)  ! num of anions in clegg's model

      integer nrxn_aer_gl, nrxn_aer_ll, nrxn_aer_sg, nrxn_aer_sl
      parameter(nrxn_aer_gl = 4) ! num of gas-liquid equilibria
      parameter(nrxn_aer_ll = 3) ! num of liquid-liquid equilibria
      parameter(nrxn_aer_sg = 2) ! num of solid-gas equilibria
      parameter(nrxn_aer_sl = nsalt)! num of solid-liquid equilibria

      integer mmtem, mpsc, mkm, mbrom, masteem, masceem,   &
      	      mmodal, msection,   &
              mon, moff, myes, mno
      parameter(mmtem = 1)	! multicomponent taylor expansion method
      parameter(mpsc  = 2)	! pitzer-simonson-clegg model
      parameter(mkm   = 3)	! kusik and meissner mixing rule
      parameter(mbrom = 4)	! bromley mixing rule
      parameter(masteem = 1)	! adaptive step time-split explicit euler method
      parameter(masceem = 2)	! adaptive step coupled explicit euler method
      parameter(mmodal  = 1)	! modal size distribution framework
      parameter(msection= 2)	! sectional size distribution framework
      parameter(mon     = 1)	! flag: on
      parameter(moff    = 0)    ! flag:off
      parameter(myes	= mon)	! flag: yes or true
      parameter(mno	= moff)	! flag: no or false


      integer jtotal, jsolid, jliquid
      parameter(jtotal = 1)
      parameter(jsolid = 2)
      parameter(jliquid= 3)

      integer jhyst_lo, jhyst_up
      parameter(jhyst_lo = 0)	! lower hysteresis leg
      parameter(jhyst_up = 1) 	! upper hysteresis leg

      integer no_aerosol, all_solid, all_liquid, mixed
      parameter(no_aerosol = 0)	! flag
      parameter(all_solid  = 1) ! flag
      parameter(all_liquid = 2) ! flag
      parameter(mixed      = 3)	! flag

      integer soluble, insoluble
      parameter(soluble   = 1)  ! flag
      parameter(insoluble = 2)  ! flag

      real mass_cutoff
      parameter(mass_cutoff = 1.e-3)	! ng/m^3

      real smallp
      parameter(smallp = 1.0)	! percent


!----------------------------------------------------------------------
! box
!      real, save ::   &
!     &		statelo_a(nbin_a_maxd),
!     &		stateup_a(nbin_a_maxd)





! mosaic species indices
!
! gas
      integer, save ::   &
       ih2so4_g,   ihno3_g,    ihcl_g,    inh3_g

! aerosol generic
      integer, save ::   &
       iso4_a,     ino3_a,     icl_a,     inh4_a,     ico3_a,   &
       imsa_a,     ina_a,      ica_a,     ioc_a,      ibc_a,   &
       ioin_a

! aerosol elecctrolytes/compounds
      integer, save ::   &
       jnh4so4,    jlvcite,    jnh4hso4,   jnh4no3,    jnh4cl,   &
       jna2so4,    jna3hso4,   jnahso4,    jnano3,     jnacl,   &
       jcaso4,     jcano3,     jcacl2,     jcaco3,     jh2so4,   &
       jhno3,      jhcl,       jhhso4,   &
       joc,        jbc,        join,       jh2o

! aerosol ions
      integer, save ::   &
       jc_h,    jc_nh4, jc_na,  jc_ca,   &
       ja_hso4, ja_so4, ja_no3, ja_cl     ! , ja_co3


!----------------------------------------------------------------------
! mosaic variables
      integer, save ::   &
      		iclm_aer,			   &  ! i-location
      		jclm_aer,			   &  ! j-location
      		kclm_aer,			   &  ! k-location
      		kclm_aer_calcbgn,		   &  ! k-location for calc. to begin
      		kclm_aer_calcend,		   &  ! k-location for calc. to end
      		mclm_aer,			   &  ! m-subarea
      		mgas_aer_xfer,			   &  ! flag: mon, moff
      		mdynamic_solver,		   &  ! flag: masteem, masceem
      		msize_framework,		   &  ! flag: mmodal, msectional
      		mactivity_coeff, 		   &  ! flag: mmtem, mpsc, mkm, mbrom
      		madapt_alpha,			   &  ! flag: mon, moff
      		jaerosolstate(nbin_a_maxd),		   &  ! flag: no_aerosol, all_solid, all_liquid, mixed
      		jphase(nbin_a_maxd),			   &  ! phase index: jtotal, jsolid, jliquid
                jhyst_leg(nbin_a_maxd),		   &  ! hysteresis leg: jhyst_up, jhyst_lo
      		iprint_input,			   &  ! flag: mon, moff
      		lunerr_aer,			   &
      		ncorecnt_aer


      real, save ::   &
      		num_a(nbin_a_maxd), 				   &  ! #/cc(air)
      		dpgn_a(nbin_a_maxd), 			   &  ! cm
      		dp_dry_a(nbin_a_maxd),			   &  ! cm
      		dp_wet_a(nbin_a_maxd),			   &  ! cm
      		mass_dry_a(nbin_a_maxd),			   &  ! g/cc(air)
      		mass_wet_a(nbin_a_maxd),			   &  ! g/cc(air)
      		mass_soluble_a(nbin_a_maxd),			   &  ! ng/cc(air)
      		vol_dry_a(nbin_a_maxd),			   &  ! cc/cc(air)
      		vol_wet_a(nbin_a_maxd),			   &  ! cc/cc(air)
      		dens_dry_a(nbin_a_maxd),			   &  ! g/cc
      		dens_wet_a(nbin_a_maxd),			   &  ! g/cc
      		sigmag_a(nbin_a_maxd),			   &  ! -
      		water_a(nbin_a_maxd), 			   &  ! kg(water)/m^3(air)
      		water_a_hyst(nbin_a_maxd),			   &  ! kg(water)/m^3(air) hysteresis (at 60% rh)
      		water_a_up(nbin_a_maxd),			   &  ! kg(water)/m^3(air) at 60% rh
      		ph(nbin_a_maxd),				   &  ! ph
      		ph_est(nbin_a_maxd),				   &  ! ph (estimated)
      		aer(naer,3,nbin_a_maxd),			   &  ! nmol/m^3
      		aer_percent(naer,3,nbin_a_maxd),		   &  ! %
      		comp_a(naercomp),			   &  ! g/cc(air)
      		electrolyte(nelectrolyte,3,nbin_a_maxd),	   &  ! nmol/m^3
      		electrolyte_sum(nelectrolyte,nbin_a_maxd),	   &  ! nmol/m^3
      		epercent(nelectrolyte,3,nbin_a_maxd),	   &  ! %
      		gas(naer_vol),				   &  ! nmol/m^3
      		volatile_a(naer_vol),			   &  ! nmol/m^3
      		ctot_a(naer_vol),			   &  ! nmol/m^3
      		ah2o,   &
      		ah2o_a(nbin_a_maxd),   &
      		dry_vol(nbin_a_maxd),   &
      		dpmv(nbin_a_maxd),   &
      		volume_a(nbin_a_maxd),   &
      		kelvin(nbin_a_maxd)				! kelvin factor


!----------------------------------------------------------------------
! asteem (asceem) variables
      integer, save ::   &
      		mxfer_massbal(nbin_a_maxd),		   &  ! mon, moff
      		ieqblm_bin(nbin_a_maxd),		   &  ! myes, mno
      		jasteem_call,   &
      		jasteem_fail


      real, save ::   &
      		sfc_a(naer_vol),		   &  ! nmol/m^3
      		kg(naer_vol,nbin_a_maxd), 		   &  ! 1/s
      		df_gas(naer_vol,nbin_a_maxd),	   &  ! nmol/m^3 (g-g*) = driving force)
      		flux(naer_vol,nbin_a_maxd),		   &  ! nmol/m^3/s
      		aer_nh4_max(nbin_a_maxd), 		   &  ! nmol/m^3
      		phi_volatile(naer_vol,nbin_a_maxd),	   &  ! relative dr. force = (g-g*)/g
      		phi_nh4no3(nbin_a_maxd),		   &  ! relative dr. force: 0 to 1
      		phi_nh4cl(nbin_a_maxd),		   &  ! relative dr. force: 0 to 1
      		alpha_gas(naer_vol),		   &  ! 0.01 to 0.05
      		alpha_aer(naer_vol,nbin_a_maxd),	   &  ! 0.01 to 4.0
      		alpha_asteem,			   &  ! 0.01 to 0.05
      		steps_asteem(nbin_a_maxd),   &
      		steps_asteem_avg(nbin_a_maxd),   &
      		steps_asteem_max(nbin_a_maxd)



!----------------------------------------------------------------------
! mesa variables
      integer, save ::   &
      		jsalt_index(nsalt),   &
      		jsulf_poor(211),   &
      		jsulf_rich(71),   &
      		jsalt_present(nsalt),   &
      		jmesa_call_tot,   &
      		jmesa_call,   &
      		jmesa_fail


      real, save ::   &
      		eleliquid(nelectrolyte),   &
      		flux_sl(nsalt),   &
      		phi_salt(nsalt),   &
      		sat_ratio(nsalt),   &
      		hsalt(nsalt),   &
      		hsalt_max,   &
      		frac_salt_liq(nsalt),   &
      		frac_salt_solid(nsalt),   &
      		dry_mass(nbin_a_maxd),   &
      		total_dry_mass(nbin_a_maxd),   &
      		growth_factor(nbin_a_maxd),   &
      		d_mdrh(63,4),				   &  ! mdrh(t) poly coeffs
      		mdrh(nbin_a_maxd),   &
      		iter_mesa,   &
      		iter_mesa_avg


!----------------------------------------------------------------------
! mosaic physico-chemical constants
      character*8, save ::   &
      		ename(nelectrolyte),			   &  ! electrolyte names
      		aer_name(naer),				   &  ! generic aerosol species name
      		gas_name(naer_vol)			! gas species name

      real, save ::   &
      		t_k,					   &  ! temperature (k)
      		p_atm,					   &  ! pressure (atm)
      		rh_pc,					   &  ! relative humidity (%)
      		cair_mol_cc,				   &  ! air conc in mol/cc
      		cair_mol_m3,				   &  ! air conc in mol/m^3
      		conv1a,   &
      		conv1b,   &
      		conv2a,   &
      		conv2b,   &
      		mw_electrolyte(nelectrolyte),		   &  ! molecular wt of electrolytes
      		mw_aer_mac(naer),				   &  ! molecular wt of generic species
      		mw_comp_a(naercomp),			   &  ! molecular wt of compounds
      		mw_c(ncation),				   &  ! molecular wt of cations
      		mw_a(nanion),				   &  ! molecular wt of anions
      		dens_electrolyte(nelectrolyte),		   &  ! g/cc
                dens_aer_mac(naer),				   &  ! g/cc
      		dens_comp_a(naercomp),			   &  ! g/cc (density of compounds)
      		sigma_water,				   &  ! water surface tension (n/m)
      		sigma_soln(nbin_a_maxd),    			   &  ! solution surface tension (n/m)
      		keq_gl(nrxn_aer_gl),			   &  ! gas-liq eqblm const
      		keq_ll(nrxn_aer_ll),			   &  ! liq-liq eqblm const
      		keq_sg(nrxn_aer_sg),			   &  ! solid-gas eqbln const
      		keq_sl(nrxn_aer_sl), 			   &  ! solid-liq eqblm const
      		keq_nh4no3_0, 				   &  ! pure lumped nh4no3 const
      		keq_nh4cl_0,				   &  ! pure lumped nh4cl const
      		kp_nh3, 				   &  !
      		kp_nh4no3, 				   &  !
      		kp_nh4cl				!


      complex, save ::   &
      		ref_index_a(naercomp),			   &  ! refractive index of compounds
      		ri_avg_a(nbin_a_maxd)			! vol avg ref index of bin


!----------------------------------------------------------------------
! mosaic activity coefficient models parameters
      integer, save ::   &
      		izc(ncation),			   &  ! integer charge
      		iza(nanion)			! integer charge


      real, save ::   &
      		mc(ncation,nbin_a_maxd),		   &  ! mol/kg(water)
      		ma(nanion,nbin_a_maxd),		   &  ! mol/kg(water)
      		mh2o,				   &  ! mol/kg(water)
      		msulf,   &
      		zc(ncation),			   &  ! real charge
      		za(nanion),			   &  ! real charge
      		xc(ncation),   &
                xa(nanion),   &
      		xh2o,   &
      		fc(ncation,nbin_a_maxd),   &
      		fa(nanion,nbin_a_maxd),   &
      		fh2o,   &
      		gam_cation(ncation,nbin_a_maxd),   &
      		gam_anion(nanion,nbin_a_maxd),   &
      		gh2o,   &
      		acation(ncation),   &
      		aanion(nanion),   &
      		ec(ncation),   &
      		ea(nanion),   &
      		emc(ncation,ncation),   &
      		exa(nanion,nanion),   &
      		ix,   &
      		ff,   &
                thetahoe(2,2),   &
                dthetahoe(2,2),   &
                k_dash(nbin_a_maxd),   &
      		ax,   &
      		rho,   &
      		bc_a(ncation,nanion),   &
      		b1_c_a(ncation,nanion),   &
      		alpha_c_a(ncation,nanion),   &
      		alpha1_c_a(ncation,nanion),   &
      		u1_c_a(ncation,nanion),   &
      		v1_c_a(ncation,nanion),   &
      		w1_c_a(ncation,nanion),   &
      		uc_cp_a(ncation,ncation,nanion),   &
      		wc_cp_a(ncation,ncation,nanion),   &
      		q1_c_cp_a(ncation,ncation,nanion),   &
      		ua_ap_c(nanion,nanion,ncation),   &
      		wa_ap_c(nanion,nanion,ncation),   &
      		q1_a_ap_c(nanion,nanion,ncation),   &
      		xnuc(ncation,nanion),   &
      		xnua(nanion,ncation),   &
      		gam(nelectrolyte,nbin_a_maxd),   &
      		gam_ratio(nbin_a_maxd),   &
      		im(nbin_a_maxd),   &
      		log_gam0(nelectrolyte),   &
      		log_gamz(nelectrolyte,nelectrolyte),   &
      		log_gam(nelectrolyte),   &
      		gam_nh4no3_0,   &
      		gam_nh4cl_0,   &
      		activity(nelectrolyte,nbin_a_maxd),   &
      		xeq_a(nanion),   &
      		xeq_c(ncation),   &
      		na_ma(nanion),   &
      		nc_mc(ncation),   &
      		a_zsr(6,nelectrolyte),			   &  ! binary molality polynomial coeffs
      		b_zsr(nelectrolyte),			   &  ! binary molality coeff
                aw_min(nelectrolyte),			   &  ! minimum frh at which molality polynomial can be used
      		b_km(5,nelectrolyte),			   &  ! kusik and meissner poly coeffs
      		im_max(nelectrolyte),			   &  ! max valid im for b_km ploy
      		b_mtem(6,nelectrolyte,nelectrolyte)	! mtem poly coeffs


!----------------------------------------------------------------------
! mosaic massbalance variables
      real, save ::   &
      		total_so4_in,   &
      		total_no3_in,   &
      		total_cl_in,   &
      		total_nh4_in,   &
      		total_na_in,   &
                total_ca_in,   &
      		total_so4,   &
      		total_no3,   &
      		total_cl,   &
      		total_nh4,   &
      		total_na,   &
      		total_ca,   &
      		rel_diff_so4,   &
      		rel_diff_no3,   &
      		rel_diff_cl,   &
      		rel_diff_nh4,   &
      		rel_diff_na,   &
      		rel_diff_ca,   &
      		aersav(naer),   &
      		gassav(naer_vol),   &
           tot_so4_in, tot_no3_in,  tot_cl_in,  tot_nh4_in,   &
           tot_na_in,  tot_ca_in,   &
           tot_so4_out,tot_no3_out, tot_cl_out, tot_nh4_out,   &
           tot_na_out, tot_ca_out,   &
           diff_so4,   diff_no3,    diff_cl,    diff_nh4,   &
           diff_na,    diff_ca,   &
           reldiff_so4,reldiff_no3, reldiff_cl, reldiff_nh4,   &
           reldiff_na, reldiff_ca,   &
           masbalout_maxreldiff_so4(nbin_a_maxd),   &
           masbalout_maxreldiff_nh4(nbin_a_maxd),   &
           masbalout_maxreldiff_no3(nbin_a_maxd),   &
           masbalout_maxreldiff_cl(nbin_a_maxd),   &
           masbalout_maxreldiff_na(nbin_a_maxd),   &
           masbalout_maxreldiff_ca(nbin_a_maxd)

      integer, save ::   &
           masbalout_err_count(nbin_a_maxd)

!----------------------------------------------------------------------




!----------------------------------------------------------------------



      end module module_data_mosaic_therm
