! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Aerosol Option:  MOSAIC (Model for Simulating Aerosol Interactions &
! Chemistry)
! * Primary investigator: Rahul A. Zaveri
! * Co-investigator: Richard C. Easter, William I. Gustafson Jr.
! Last update: February 2009
!
! Contacts:
! Rahul A. Zaveri, PhD                    Jerome D. Fast, PhD
! Senior Research Scientist               Staff Scientist
! Pacific Northwest National Laboratory   Pacific Northwest National Laboratory
! P.O. Box 999, MSIN K9-30                P.O. Box 999, MSIN K9-30
! Richland, WA 99352                      Richland, WA, 99352
! Phone: (509) 372-6159                   Phone: (509) 372-6116
! Email: Rahul.Zaveri@pnl.gov             Email: Jerome.Fast@pnl.gov
!
! Please report any bugs or problems to Rahul Zaveri, the primary author
! of the code, or Jerome Fast, the WRF-chem implementation team leader
! for PNNL.
!
! Terms of Use:
!  1) Users are requested to consult the primary author prior to
!     modifying the MOSAIC code or incorporating it or its submodules in
!     another code. This is meant to ensure that the any linkages and/or
!     assumptions will not adversely affect the operation of MOSAIC.
!  2) The MOSAIC source code is intended for research and educational
!     purposes. Users are requested to contact the primary author
!     regarding the use of MOSAIC code for any commercial application.
!  3) Users preparing publications resulting from the usage of MOSAIC are
!     requested to cite one or more of the references below (depending on
!     the application) for proper acknowledgement.
!
! References for MOSAIC (The first one is the primary MOSAIC reference):
! * Zaveri R.A., R.C. Easter, J.D. Fast, and L.K. Peters (2008), Model
!   for Simulating Aerosol Interactions and Chemistry (MOSAIC), J.
!   Geophys. Res., 113, D13204, doi:10.1029/2007JD008782.
! * Zaveri R.A., R.C. Easter, and A.S. Wexler (2005), A new method for
!   multi-component activity coefficients of electrolytes in aqueous
!   atmospheric aerosols, J. Geophys. Res., 110, D02201,
!   doi:10.1029/2004JD004681.
! * Zaveri R.A., R.C. Easter, and L.K. Peters (2005), A computationally
!   efficient multicomponent equilibrium solver for aerosols (MESA), J.
!   Geophys. Res., 110, D24203, doi:24203, doi:10.1029/2004JD005618.
!
! Reference for volatility basis set SOA code:
! * Shrivastava, M., J. Fast, R. Easter, W. I. Gustafson Jr., R. A.
!   Zaveri, J. L. Jimenez, P. Saide, and A. Hodzic, 2010: Modeling
!   organic aerosols in a megacity: comparison of simple and complex
!   representations of the volatility basis set approach. Atmos. Chem.
!   Phys., in review.
!
! Reference for the alternative SOA treatment that is less expensive to run 
! and is used in the package  mozart_mosaic_4bin_vbs0
! SOA is treated using a simplified approach for anthropogenic and biomass burning species based on Hodzic and Jimenez, GMD, 2011
! Hodzic, A. and Jimenez, J. L.: Modeling anthropogenically controlled secondary organic aerosols in a megacity: 
! a simplified framework for global and climate models, Geosci. Model Dev., 4, 901-917, doi:10.5194/gmd-4-901-2011, 2011.
!
! References for implementation of the aerosol optical properties and
! direct effect in WRF-Chem:
! * Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!   Barnard, E.G. Chapman, G.A. Grell, and S.E. Peckham (2005), Evolution
!   of ozone, particulates, and aerosol direct radiative forcing in the
!   vicinity of Houston using a fully-coupled meteorology-chemistry-
!   aerosol model, J. Geophys. Res., 111, D21305,
!   doi:10.1029/2005JD006721.
! * Barnard, J.C., J.D. Fast, G. Paredes-Miranda, W.P. Arnott, and
!   A. Laskin (2010), Technical note: evaluation of the WRF-Chem
!   "aerosol chemical to aerosol optical properties" module using data
!   from the MILAGRO campaign, Atmos. Chem. Phys., 10, 7325-7340,
!   doi:10.5194/acp-10-7325-2010.
!
! References for implementation of the aerosol-cloud interactions and
! indirect effects in WRF-Chem:
! * Gustafson, W. I., E. G. Chapman, S. J. Ghan, R. C. Easter, and J. D.
!   Fast (2007), Impact on modeled cloud characteristics due to
!   simplified treatment of uniform cloud condensation nuclei during
!   NEAQS 2004, Geophys. Res. Lett., 34, L19809, L19809,
!   doi:10.1029/2007GL0300321.
! * Chapman, E.G., W. I. Gustafson Jr., R. C. Easter, J. C. Barnard,
!   S. J. Ghan, M. S. Pekour, and J. D. Fast (2009): Coupling aerosol-
!   cloud-radiative processes in the WRF-Chem model: Investigating the
!   radiative impact of elevated point sources, Atmos. Chem. Phys., 9,
!   945-964, www.atmos-chem-phys.net/9/945/2009/.
!
! Contact Jerome Fast for updates on the status of manuscripts under
! review.
!
! Additional information:
! * www.pnl.gov/atmospheric/research/wrf-chem
!
! Support: 
! Funding for developing and evaluating MOSAIC was provided by the U.S.
! Department of Energy under the auspices of Atmospheric Science Program
! of the Office of Biological and Environmental Research, the NASA Earth
! Sciences Enterprise under grant NAGW 3367, and PNNL Laboratory Directed
! Research and Development program.
!************************************************************************
	module module_mosaic_driver


	use module_mosaic_sumpm


!
!   *** NOTE - when the cw species are NOT in the registry, then
!   then the p_xxx_cwnn variables are not in module_state_description,
!   and the following cpp directive should be commented out
!
#define cw_species_are_in_registry


	contains

!-----------------------------------------------------------------------
!
! rce 2005-feb-18 - one fix involving dcen_sect indices [now (isize,itype)]
!
! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect
!   nov-04 sg ! replaced amode with aer and expanded aerosol dimension 
!     to include type and phase
!
! rce 11-sep-2004 - numerous changes
!   eliminated use of the _wrfch pointers (lptr_xxx_a_wrfch,
!	lwaterptr_wrfch, numptr_wrfch); use only the _aer pointers now
!   aboxtest_... variables are now in module_data_mosaic_other
!
!-----------------------------------------------------------------------

	subroutine mosaic_aerchem_driver(                         &
		id, curr_secs, ktau, dtstep, ktauc, dtstepc, config_flags, &
		t_phy, rho_phy, p_phy,                            &
		moist, chem, vbs_nbin,                            &
		ids,ide, jds,jde, kds,kde,                        &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )


	use module_configure, only:  grid_config_rec_type,            &
			p_qv,                                         &
			p_so2, p_ho2, p_so4aj, p_corn, p_hcl, p_mtf,  &
			p_so4_a01, p_water_a01, p_num_a01,            &
			p_so4_a04, p_water_a04, p_num_a04

	use module_state_description, only:  num_moist, num_chem

	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_mosaic_therm, only:  aerchemistry, print_mosaic_stats, &
         iprint_mosaic_fe1, iprint_mosaic_perform_stats, &
         iprint_mosaic_diag1, iprint_mosaic_input_ok
	use module_mosaic2_driver, only:  mosaic2_aerchem_driver
	use module_mosaic_newnuc, only:  mosaic_newnuc_1clm
	use module_mosaic_coag, only:  mosaic_coag_1clm
	use module_peg_util, only:  peg_error_fatal, peg_message

        use module_data_mosaic_therm, only: glysoa_param,                 &
               glysoa_param_off, glysoa_param_simple, glysoa_param_complex
        use module_state_description, only: mozart_mosaic_4bin_kpp, &
         mozart_mosaic_4bin_aq_kpp

	implicit none

!-----------------------------------------------------------------------
! DESCRIPTION
!
! mosaic_aerchem_driver is the interface between wrf-chem and the
!   mosaic aerosol-chemistry routine cat computes condensation/evaporation
!   of trace gases to/from aerosol particles (AP).  It currently treats
!   water vapor and the 4 inorganic trace gases (nh3, h2so4, hno3, and hcl).
!   The aerosol-chemistry routine can work with either a sectional
!   (multiple size bins) or modal (multiple modes) representation.  
!
!   In both cases, condensation/evaporation to/from each bins/mode is 
!   first computed.  For sectional representation, AP mass and number 
!   are then transferred between size bins as a result of AP 
!   positive/negative growth.  Either a moving-center or two-moment
!   algorithm can be used to compute this transfer.
!
! mosaic_aerchem_driver is organized as follows
!   loop over j and i
!	call mapaer_tofrom_host to map 1 column of gas and aerosol mixing 
!	    ratios from the chem array to the rsub array (and convert units)
!	call aerchemistry to do the aerosol chemistry calculations
!	    for timestep = dtstepc
!	call mapaer_tofrom_host to map 1 column of gas and aerosol mixing 
!	    ratios from the rsub array back to the chem array
!
!-----------------------------------------------------------------------

!   subr arguments
	integer, intent(in) ::              &
		id, ktau, ktauc,                &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte,   &
                vbs_nbin(1)
!   id - domain index
!   ktau - time step number
!   ktauc - gas and aerosol chemistry time step number

!   [ids:ide, kds:kde, jds:jde] - spatial (x,z,y) indices for "domain"
!   [ims:ime, kms:kme, jms:jme] - spatial (x,z,y) indices for "memory"
!	Most arrays that are arguments to chem_driver 
!	are dimensioned with these spatial indices.
!   [its:ite, kts:kte, jts:jte] - spatial (x,z,y) indices for "tile"
!	chem_driver and routines under it do calculations
!	over these spatial indices.

    real(kind=8), intent(in) :: curr_secs
	real, intent(in) :: dtstep, dtstepc
!   dtstep - main model time step (s)
!   dtstepc - time step for gas and aerosol chemistry(s)

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		t_phy, rho_phy, p_phy
!   t_phy - temperature (K)
!   rho_phy - air density (kg/m^3)
!   p_phy - air pressure (Pa)

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
		moist
!   moist - mixing ratios of moisture species (water vapor, 
!	cloud water, ...) (kg/kg for mass species, #/kg for number species)
 
	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem
!   chem - mixing ratios of trace gase (ppm) and aerosol species
!	(ug/kg for mass species, #/kg for number species)

	type(grid_config_rec_type), intent(in) :: config_flags
!   config_flags - configuration and control parameters

!-----------------------------------------------------------------------
!   local variables
	integer :: i, idum, istat, it, j, jt, k, l, n
	integer :: k_pegshift, kclm_calcbgn, kclm_calcend
	integer :: ktmaps, ktmape
	integer :: levdbg_err, levdbg_info
	integer :: i_force_dump, mode_force_dump
	integer :: idiagaa_dum, idiagbb_dum, ijcount_dum
	integer, parameter :: debug_level=0
!	integer, parameter :: aercoag_onoff = 1
!	integer, parameter :: aernewnuc_onoff = 1
	integer :: aercoag_onoff = 1
	integer :: aernewnuc_onoff = 1
	
	real :: dtchem, dtcoag, dtnuc
	real :: dum
	real :: rsub0(l2maxd,kmaxd,nsubareamaxd)

	character*100 msg


        if ( config_flags%aerchem_onoff <= 0 ) return   ! aerosol chem/dynam turned off


        i = max( config_flags%mosaic_aerchem_optaa, 0 )
        if ( mod(i,10) == 2 ) then   ! use mosaic2 when 1s digit = 2
            call mosaic2_aerchem_driver(                          &
                id, curr_secs, ktau, dtstep, ktauc, dtstepc,      &
                config_flags,                                     &
                t_phy, rho_phy, p_phy,                            &
                moist, chem,                                      &
                ids,ide, jds,jde, kds,kde,                        &
                ims,ime, jms,jme, kms,kme,                        &
                its,ite, jts,jte, kts,kte                         )
            return
        end if


    if (debug_level .ge. 15) then
!rcetestc diagnostics --------------------------------------------------
!   if (kte .eq. -99887766) then
    if (ktauc .le. 2) then
    print 93010, ' '
    print 93010, 'rcetestc diagnostics from mosaic_aerchem_driver'
    print 93010, 'id, chem_opt, ktau, ktauc    ',   &
         id, config_flags%chem_opt, ktau, ktauc
    print 93020, 'dtstep, dtstepc                 ',   &
         dtstep, dtstepc
    print 93010, 'ims/e, j, k', ims, ime, jms, jme, kms, kme
    print 93010, 'its/e, j, k', its, ite, jts, jte, kts, kte
    print 93010, 'num_chem, p_so2, p_ho2       ', num_chem, p_so2, p_ho2
    print 93010, 'p_so4aj, p_corn, p_hcl, p_mtf', p_so4aj, p_corn, p_hcl, p_mtf
    print 93010, 'p_so4_a01, p_water, p_num_a01', p_so4_a01, p_water_a01, p_num_a01
    print 93010, 'p_so4_a04, p_water, p_num_a04', p_so4_a04, p_water_a04, p_num_a04

    k = kts
	print 93020, 't, p, rho, qv at its/kts /jts', t_phy(its,k,jts),   &
		p_phy(its,k,jts), rho_phy(its,k,jts), moist(its,k,jts,p_qv)
	k = (kts + kte)/2
	print 93020, 't, p, rho, qv at its/ktmi/jts', t_phy(its,k,jts),   &
		p_phy(its,k,jts), rho_phy(its,k,jts), moist(its,k,jts,p_qv)
	k = kte
	print 93020, 't, p, rho, qv at its/kte /jts', t_phy(its,k,jts),   &
		p_phy(its,k,jts), rho_phy(its,k,jts), moist(its,k,jts,p_qv)
93010	format( a, 8(1x,i6) )
93020	format( a, 8(1p,e14.6) )
    end if
!   end if
!rcetestc diagnostics --------------------------------------------------
    end if

! The default values for these informational printout settings are set
! in module_data_mosaic_therm.F.
    if (debug_level .lt. 15) then 
       iprint_mosaic_fe1 = 1
       iprint_mosaic_perform_stats = 0
       iprint_mosaic_diag1 = 0
       iprint_mosaic_input_ok = 0
    end if

   glysoa_param = glysoa_param_off
   if (config_flags%chem_opt == mozart_mosaic_4bin_kpp) &
     glysoa_param = glysoa_param_simple
   if (config_flags%chem_opt == mozart_mosaic_4bin_aq_kpp) &
     glysoa_param = glysoa_param_complex


        aercoag_onoff = 1
        aernewnuc_onoff = 1
! set some control flags using config_flags%mosaic_aerchem_optaa
        i = max( config_flags%mosaic_aerchem_optaa, 0 )
        if (i >= 10000) then
            j = mod(i,100)/10  ! 10s digit controls movesect
            if (j == 0) msectional =  0  ! causes method_movesect =  0
            if (j == 1) msectional = 10  ! causes method_movesect = 10
            if (j == 2) msectional = 20  ! causes method_movesect = 20

            j = mod(i,1000)/100  ! 100s digit controls newnuc
            if (j == 0) aernewnuc_onoff = 0

            j = mod(i,10000)/1000  ! 1000s digit controls coag
            if (j == 0) aercoag_onoff = 0
        end if ! (i >= 10000) then


!   ktmaps,ktmape = first/last wrf kt for which aer chem is done
	ktmaps = kts
	ktmape = kte

!   rce 2005-mar-09 - added kclm_calcbgn/end 
!   kclm_calcbgn,kclm_calcend = first/last pegasus array k
!   for which aer chem is done
	k_pegshift = k_pegbegin - kts 
	kclm_calcbgn = kts + k_pegshift
	kclm_calcend = kte + k_pegshift

!   set some variables to their wrf-chem "standard" values
	mode_force_dump = 0
	levdbg_err = 0
        levdbg_info = 15

!   eventually iymdcur & ihmscur should be set to the correct date/time 
!   using wrf timing routines
	iymdcur = 1 + int( curr_secs/86400._8 )
	ihmscur = nint( mod( curr_secs, 86400._8 ) )

	t = curr_secs
	ncorecnt = ktau - 1

#if defined ( aboxtest_box_testing_active )
! *** start of "box testing" code section ***
!     these code lines should be inactive when running wrf-chem
!
!   get values for some "box test" variables
 	call aboxtest_get_extra_args( 20,   &
 		iymdcur, ihmscur,   &
 		idum, idum, idum, idum, idum, idum, idum,   &
 		t, dum )
! ***  end  of "box testing" code section ***
#endif


!   set "pegasus" grid size variables
	itot = ite
	jtot = jte
	nsubareas = 1

	ijcount_dum = 0

	call print_mosaic_stats( 0 )


	do 2920 jt = jts, jte
	do 2910 it = its, ite

	ijcount_dum = ijcount_dum + 1
	dtchem = dtstepc


!   mode_force_dump selects a detailed dump of gaschem at either
!   first ijk grid, first ij column, all ijk, or no ijk
	i_force_dump = 0
!	if (mode_force_dump .eq. 10) then
!	    if ((it.eq.its) .and. (jt.eq.jts)) i_force_dump = 1
!	else if (mode_force_dump .eq. 100) then
!	    i_force_dump = 1
!	else if (mode_force_dump .eq. 77) then
!	    if ( (it .eq.  (its+ite)/2) .and.   &
!	         (jt .eq.  (jts+jte)/2) ) i_force_dump = 1
!	end if


!	print 93010, 'calling mapaeraa - it, jt =', it, jt
	call mapaer_tofrom_host( 0,                       &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      ktmaps,ktmape,              &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )

!rce 22-jul-2006 - save initial mixrats
	rsub0(:,:,:) = rsub(:,:,:)

	idiagaa_dum = 0
	idiagbb_dum = 110
!rce 29-apr-2004 - following is for debugging texas 16 km run
!	if ((its.eq.38) .and. (jts.eq.38)   &
!			.and. (ktau.eq.240)) idiagaa_dum = 1
!	if ((it .eq.45) .and. (jt .eq.71)   &
!			.and. (ktau.eq.240)) idiagaa_dum = 1
!	if ( ijcount_dum > 169 .and. ktau > 2579 ) then !fastj crash
!	if ( ijcount_dum > 300 .and. ktau > 2969 ) then !madronovich crash
!       idiagaa_dum = 111
!       i_force_dump = 1
!    end if

!	if (ijcount_dum .le. 1) i_force_dump = 1
!	i_force_dump = 0

	if (i_force_dump > 0) call aerchem_debug_dump( 1, it, jt, dtchem )

!	if ((it .eq.45) .and. (jt .eq.71)   &
!			.and. (ktau.eq.240)) then
!	    call aerchem_debug_dump( 1, it, jt, dtchem )
!	    call aerchem_debug_dump( 3, it, jt, dtchem )
!	end if

	if (idiagaa_dum > 0)   &
 	print 93010, 'calling aerchem - it,jt,maerchem =', it, jt, maerchem
!	print 93010, 'calling aerchem - it,jt,maerchem =', it, jt, maerchem
	call aerchemistry( it, jt, kclm_calcbgn, kclm_calcend,   &
                           dtchem, idiagaa_dum, vbs_nbin )

!  note units for aerosol is now ug/m3

    call wrf_debug(300,"mosaic_aerchem_driver: back from aerchemistry")
!	if ((it .eq.45) .and. (jt .eq.71)   &
!			.and. (ktau.eq.240)) then
!	    call aerchem_debug_dump( 3, it, jt, dtchem )
!	end if

	if (i_force_dump > 0) call aerchem_debug_dump( 3, it, jt, dtchem )


	if (aernewnuc_onoff > 0) then
	    if (idiagaa_dum > 0) print 93010, 'calling mosaic_newnuc_1clm'
	    dtnuc = dtchem
	    call mosaic_newnuc_1clm( istat,  &
		it, jt, kclm_calcbgn, kclm_calcend,   &
		idiagbb_dum, dtchem, dtnuc, rsub0,   &
		id, ktau, ktauc, its, ite, jts, jte, kts, kte )
	end if


	if (aercoag_onoff > 0) then
	    if (idiagaa_dum > 0) print 93010, 'calling mosaic_coag_1clm'
	    dtcoag = dtchem
	    call mosaic_coag_1clm( istat,  &
		it, jt, kclm_calcbgn, kclm_calcend,   &
		idiagbb_dum, dtchem, dtcoag,   &
		id, ktau, ktauc, its, ite, jts, jte, kts, kte )
	end if


	if (idiagaa_dum > 0)   &
 	print 93010, 'calling mapaerbb'
	call mapaer_tofrom_host( 1,                       &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      ktmaps,ktmape,              &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )

!	print 93010, 'backfrm mapaerbb', it, jt
2910	continue
2920	continue


!   rce 2005-apr-30 - added 2 calls to print_mosaic_stats
	call print_mosaic_stats( 1 )
	print 93010, 'leaving mosaic_aerchem_driver - ktau =', ktau

	return
	end subroutine mosaic_aerchem_driver



!----------------------------------------------------------------------




	subroutine mapaer_tofrom_host( imap,                  &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      ktmaps,ktmape,              &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )

        use module_configure, only:   &
		p_qv, p_qc, p_h2so4,p_sulf, p_hno3, p_hcl, p_nh3, p_o3,   &
		p_so2, p_h2o2, p_hcho, p_ho, p_ho2, p_no3,   &
		p_no, p_no2, p_hono, p_pan,  &
       p_pcg1_b_c,p_pcg2_b_c,p_pcg3_b_c,p_pcg4_b_c,p_pcg5_b_c,p_pcg6_b_c, &
       p_pcg7_b_c,p_pcg8_b_c,p_pcg9_b_c,p_opcg1_b_c,p_opcg2_b_c,p_opcg3_b_c, &
       p_opcg4_b_c,p_opcg5_b_c,p_opcg6_b_c,p_opcg7_b_c,p_opcg8_b_c,p_pcg1_b_o,&
       p_pcg2_b_o,p_pcg3_b_o,p_pcg4_b_o,p_pcg5_b_o,p_pcg6_b_o,p_pcg7_b_o,&
       p_pcg8_b_o,p_pcg9_b_o,p_opcg1_b_o,p_opcg2_b_o,p_opcg3_b_o,p_opcg4_b_o, &
       p_opcg5_b_o,p_opcg6_b_o,p_opcg7_b_o,p_opcg8_b_o,p_pcg1_f_c,p_pcg2_f_c, &
       p_pcg3_f_c,p_pcg4_f_c,p_pcg5_f_c,p_pcg6_f_c,p_pcg7_f_c,p_pcg8_f_c, &
       p_pcg9_f_c, p_opcg1_f_c,p_opcg2_f_c,p_opcg3_f_c,p_opcg4_f_c, &
       p_opcg5_f_c,p_opcg6_f_c,p_opcg7_f_c,p_opcg8_f_c,p_pcg1_f_o, &
       p_pcg2_f_o,p_pcg3_f_o,p_pcg4_f_o,p_pcg5_f_o,p_pcg6_f_o,p_pcg7_f_o, &
       p_pcg8_f_o,p_pcg9_f_o,p_opcg1_f_o,p_opcg2_f_o,p_opcg3_f_o,p_opcg4_f_o,&
       p_opcg5_f_o,p_opcg6_f_o,p_opcg7_f_o,p_opcg8_f_o, &
       p_smpa,p_smpbb, &
       p_gly, &
       p_ant1_c,p_ant2_c,p_ant3_c,p_ant4_c,p_ant1_o,p_ant2_o,p_ant3_o,p_ant4_o,&
       p_biog1_c,p_biog2_c,p_biog3_c,p_biog4_c,p_biog1_o, &
       p_biog2_o,p_biog3_o,p_biog4_o, &
!       p_n2o5, p_clno2
       p_n2o5, p_clno2, &
       p_asoaX_a01, p_asoaX_a02, p_asoaX_a03, p_asoaX_a04, &
       p_asoa1_a01, p_asoa1_a02, p_asoa1_a03, p_asoa1_a04, &
       p_asoa2_a01, p_asoa2_a02, p_asoa2_a03, p_asoa2_a04, &
       p_asoa3_a01, p_asoa3_a02, p_asoa3_a03, p_asoa3_a04, &
       p_asoa4_a01, p_asoa4_a02, p_asoa4_a03, p_asoa4_a04, &
       p_bsoaX_a01, p_bsoaX_a02, p_bsoaX_a03, p_bsoaX_a04, &
       p_bsoa1_a01, p_bsoa1_a02, p_bsoa1_a03, p_bsoa1_a04, &
       p_bsoa2_a01, p_bsoa2_a02, p_bsoa2_a03, p_bsoa2_a04, &
       p_bsoa3_a01, p_bsoa3_a02, p_bsoa3_a03, p_bsoa3_a04, &
       p_bsoa4_a01, p_bsoa4_a02, p_bsoa4_a03, p_bsoa4_a04, &
       p_cvasoaX, p_cvasoa1, p_cvasoa2, p_cvasoa3, p_cvasoa4, &
       p_cvbsoaX, p_cvbsoa1, p_cvbsoa2, p_cvbsoa3, p_cvbsoa4

	use module_state_description, only:  param_first_scalar
	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_mosaic_csuesat, only:  esat_gchm
	use module_peg_util, only:  peg_error_fatal, peg_message

	implicit none

!   subr arguments

!   imap determines mapping direction (chem-->rsub if <=0, rsub-->chem if >0)
	integer, intent(in) :: imap
!   wrf array dimensions
	integer, intent(in) :: num_moist, num_chem
	integer, intent(in) :: ims, ime, jms, jme, kms, kme
	integer, intent(in) :: its, ite, jts, jte, kts, kte
!   do mapping for wrf i,k,j grid points = [it,ktmaps:ktmape,jt]
	integer, intent(in) :: it, jt, ktmaps, ktmape
!   
	real, intent(in), dimension( ims:ime, kms:kme, jms:jme ) :: &
		t_phy, rho_phy, p_phy

	real, intent(in), &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
		moist
 
	real, intent(inout), &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem


!   local variables
	integer ido_l, idum, iphase, itype,   &
		k, k1, k2, kt, kt1, kt2, k_pegshift, l, n
	integer p1st
	real dum, dumesat, dumrsat, dumrelhum, onemeps
	real factdens, factpres, factmoist, factgas,   &
		factaerso4, factaerno3, factaercl, factaermsa,   &
		factaerco3, factaernh4, factaerna, factaerca,   &
		factaeroin, factaeroc, factaerbc,   &
       factaerpcg1_b_c,factaerpcg2_b_c,factaerpcg3_b_c,factaerpcg4_b_c,factaerpcg5_b_c,factaerpcg6_b_c, &
       factaerpcg7_b_c,factaerpcg8_b_c,factaerpcg9_b_c,factaeropcg1_b_c,factaeropcg2_b_c,factaeropcg3_b_c, &
       factaeropcg4_b_c,factaeropcg5_b_c,factaeropcg6_b_c,factaeropcg7_b_c,factaeropcg8_b_c,factaerpcg1_b_o,&
       factaerpcg2_b_o,factaerpcg3_b_o,factaerpcg4_b_o,factaerpcg5_b_o,factaerpcg6_b_o,factaerpcg7_b_o,&
       factaerpcg8_b_o,factaerpcg9_b_o,factaeropcg1_b_o,factaeropcg2_b_o,factaeropcg3_b_o,factaeropcg4_b_o, &
       factaeropcg5_b_o,factaeropcg6_b_o,factaeropcg7_b_o,factaeropcg8_b_o,factaerpcg1_f_c,factaerpcg2_f_c, &
       factaerpcg3_f_c,factaerpcg4_f_c,factaerpcg5_f_c,factaerpcg6_f_c,factaerpcg7_f_c,factaerpcg8_f_c, &
       factaerpcg9_f_c, factaeropcg1_f_c,factaeropcg2_f_c,factaeropcg3_f_c,factaeropcg4_f_c, &
       factaeropcg5_f_c,factaeropcg6_f_c,factaeropcg7_f_c,factaeropcg8_f_c,factaerpcg1_f_o, &
       factaerpcg2_f_o,factaerpcg3_f_o,factaerpcg4_f_o,factaerpcg5_f_o,factaerpcg6_f_o,factaerpcg7_f_o, &
       factaerpcg8_f_o,factaerpcg9_f_o,factaeropcg1_f_o,factaeropcg2_f_o,factaeropcg3_f_o,factaeropcg4_f_o,&
       factaeropcg5_f_o,factaeropcg6_f_o,factaeropcg7_f_o,factaeropcg8_f_o,&
       factaersmpa,factaersmpbb, &
       factaerglyr1, factaerglyr2, factaerglysfc, factaerglynh4, factaerglyoh, &
       factaerant1_c,factaerant2_c,factaerant3_c,factaerant4_c, &
       factaerant1_o,factaerant2_o,factaerant3_o,factaerant4_o, &
       factaerbiog1_c,factaerbiog2_c,factaerbiog3_c,factaerbiog4_c, &
       factaerbiog1_o,factaerbiog2_o,factaerbiog3_o,factaerbiog4_o, &
       factaerasoaX,factaerasoa1,factaerasoa2,factaerasoa3,factaerasoa4, &
       factaerbsoaX,factaerbsoa1,factaerbsoa2,factaerbsoa3,factaerbsoa4, &
		factaerhysw, factaerwater, factaernum

	real, parameter :: eps=0.622

	character*80 msg


!
!   units conversion factors 
!   wrf-chem value = pegasus value X factor
!
	factdens = 28.966e3      ! moleair/cm3 --> kgair/m3
	factpres = 0.1           ! dyne/cm2 --> pa
	factmoist = eps          ! moleh2o/moleair --> kgh2o/kgair
	factgas = 1.0e6          ! mole/moleair --> ppm

!wig 9-Nov-2004: Change to converting from concentration to converting
!                from mixing ratio.
!	factaernum = 40.9        ! #/moleair --> #/m3 at STP
!! at 1 atm & 298 k,  1 m3 = 40.9 moleair,  1 liter = 0.0409 moleair
	factaernum = 1000./28.966 ! 1 kg air = (1000/28.966) moleair

	dum = factaernum*1.0e6   ! g/moleair --> ug/m3 at STP
	factaerso4   = dum*mw_so4_aer
	factaerno3   = dum*mw_no3_aer
	factaercl    = dum*mw_cl_aer
	factaermsa   = dum*mw_msa_aer
	factaerco3   = dum*mw_co3_aer
	factaernh4   = dum*mw_nh4_aer
	factaerna    = dum*mw_na_aer
	factaerca    = dum*mw_ca_aer
	factaeroin   = dum*mw_oin_aer
	factaeroc    = dum*mw_oc_aer
	factaerbc    = dum*mw_bc_aer
	factaerhysw  = dum*mw_water_aer
	factaerwater = dum*mw_water_aer
        factaerpcg1_b_c=dum*mw_pcg1_b_c_aer
        factaerpcg2_b_c=dum*mw_pcg2_b_c_aer
        factaerpcg3_b_c=dum*mw_pcg3_b_c_aer
        factaerpcg4_b_c=dum*mw_pcg4_b_c_aer
        factaerpcg5_b_c=dum*mw_pcg5_b_c_aer
        factaerpcg6_b_c=dum*mw_pcg6_b_c_aer
        factaerpcg7_b_c=dum*mw_pcg7_b_c_aer
        factaerpcg8_b_c=dum*mw_pcg8_b_c_aer
        factaerpcg9_b_c=dum*mw_pcg9_b_c_aer
        factaerpcg1_b_o=dum*mw_pcg1_b_o_aer
        factaerpcg2_b_o=dum*mw_pcg2_b_o_aer
        factaerpcg3_b_o=dum*mw_pcg3_b_o_aer
        factaerpcg4_b_o=dum*mw_pcg4_b_o_aer
        factaerpcg5_b_o=dum*mw_pcg5_b_o_aer
        factaerpcg6_b_o=dum*mw_pcg6_b_o_aer
        factaerpcg7_b_o=dum*mw_pcg7_b_o_aer
        factaerpcg8_b_o=dum*mw_pcg8_b_o_aer
        factaerpcg9_b_o=dum*mw_pcg9_b_o_aer
        factaeropcg1_b_c=dum*mw_opcg1_b_c_aer
        factaeropcg2_b_c=dum*mw_opcg2_b_c_aer
        factaeropcg3_b_c=dum*mw_opcg3_b_c_aer
        factaeropcg4_b_c=dum*mw_opcg4_b_c_aer
        factaeropcg5_b_c=dum*mw_opcg5_b_c_aer
        factaeropcg6_b_c=dum*mw_opcg6_b_c_aer
        factaeropcg7_b_c=dum*mw_opcg7_b_c_aer
        factaeropcg8_b_c=dum*mw_opcg8_b_c_aer
        factaeropcg1_b_o=dum*mw_opcg1_b_o_aer
        factaeropcg2_b_o=dum*mw_opcg2_b_o_aer
        factaeropcg3_b_o=dum*mw_opcg3_b_o_aer
        factaeropcg4_b_o=dum*mw_opcg4_b_o_aer
        factaeropcg5_b_o=dum*mw_opcg5_b_o_aer
        factaeropcg6_b_o=dum*mw_opcg6_b_o_aer
        factaeropcg7_b_o=dum*mw_opcg7_b_o_aer
        factaeropcg8_b_o=dum*mw_opcg8_b_o_aer
        factaerpcg1_f_c=dum*mw_pcg1_f_c_aer
        factaerpcg2_f_c=dum*mw_pcg2_f_c_aer
        factaerpcg3_f_c=dum*mw_pcg3_f_c_aer
        factaerpcg4_f_c=dum*mw_pcg4_f_c_aer
        factaerpcg5_f_c=dum*mw_pcg5_f_c_aer
        factaerpcg6_f_c=dum*mw_pcg6_f_c_aer
        factaerpcg7_f_c=dum*mw_pcg7_f_c_aer
        factaerpcg8_f_c=dum*mw_pcg8_f_c_aer
        factaerpcg9_f_c=dum*mw_pcg9_f_c_aer
        factaerpcg1_f_o=dum*mw_pcg1_f_o_aer
        factaerpcg2_f_o=dum*mw_pcg2_f_o_aer
        factaerpcg3_f_o=dum*mw_pcg3_f_o_aer
        factaerpcg4_f_o=dum*mw_pcg4_f_o_aer
        factaerpcg5_f_o=dum*mw_pcg5_f_o_aer
        factaerpcg6_f_o=dum*mw_pcg6_f_o_aer
        factaerpcg7_f_o=dum*mw_pcg7_f_o_aer
        factaerpcg8_f_o=dum*mw_pcg8_f_o_aer
        factaerpcg9_f_o=dum*mw_pcg9_f_o_aer
        factaeropcg1_f_c=dum*mw_opcg1_f_c_aer
        factaeropcg2_f_c=dum*mw_opcg2_f_c_aer
        factaeropcg3_f_c=dum*mw_opcg3_f_c_aer
        factaeropcg4_f_c=dum*mw_opcg4_f_c_aer
        factaeropcg5_f_c=dum*mw_opcg5_f_c_aer
        factaeropcg6_f_c=dum*mw_opcg6_f_c_aer
        factaeropcg7_f_c=dum*mw_opcg7_f_c_aer
        factaeropcg8_f_c=dum*mw_opcg8_f_c_aer
        factaeropcg1_f_o=dum*mw_opcg1_f_o_aer
        factaeropcg2_f_o=dum*mw_opcg2_f_o_aer
        factaeropcg3_f_o=dum*mw_opcg3_f_o_aer
        factaeropcg4_f_o=dum*mw_opcg4_f_o_aer
        factaeropcg5_f_o=dum*mw_opcg5_f_o_aer
        factaeropcg6_f_o=dum*mw_opcg6_f_o_aer
        factaeropcg7_f_o=dum*mw_opcg7_f_o_aer
        factaeropcg8_f_o=dum*mw_opcg8_f_o_aer
        factaersmpa=dum*mw_smpa_aer
        factaersmpbb=dum*mw_smpbb_aer
        factaerglyr1=dum*mw_glysoa_r1_aer
        factaerglyr2=dum*mw_glysoa_r2_aer
        factaerglysfc=dum*mw_glysoa_sfc_aer
        factaerglynh4=dum*mw_glysoa_nh4_aer
        factaerglyoh=dum*mw_glysoa_oh_aer
        factaerant1_c=dum*mw_ant1_c_aer
        factaerant2_c=dum*mw_ant2_c_aer
        factaerant3_c=dum*mw_ant3_c_aer
        factaerant4_c=dum*mw_ant4_c_aer
        factaerant1_o=dum*mw_ant1_o_aer
        factaerant2_o=dum*mw_ant2_o_aer
        factaerant3_o=dum*mw_ant3_o_aer
        factaerant4_o=dum*mw_ant4_o_aer
        factaerbiog1_c=dum*mw_biog1_c_aer
        factaerbiog2_c=dum*mw_biog2_c_aer
        factaerbiog3_c=dum*mw_biog3_c_aer
        factaerbiog4_c=dum*mw_biog4_c_aer
        factaerbiog1_o=dum*mw_biog1_o_aer
        factaerbiog2_o=dum*mw_biog2_o_aer
        factaerbiog3_o=dum*mw_biog3_o_aer
        factaerbiog4_o=dum*mw_biog4_o_aer
        factaerasoaX=dum*mw_asoaX_aer
        factaerasoa1=dum*mw_asoa1_aer
        factaerasoa2=dum*mw_asoa2_aer
        factaerasoa3=dum*mw_asoa3_aer
        factaerasoa4=dum*mw_asoa4_aer
        factaerbsoaX=dum*mw_bsoaX_aer
        factaerbsoa1=dum*mw_bsoa1_aer
        factaerbsoa2=dum*mw_bsoa2_aer
        factaerbsoa3=dum*mw_bsoa3_aer
        factaerbsoa4=dum*mw_bsoa4_aer




!   If aboxtest_units_convert=10, turn off units conversions both here
!   and in module_mosaic.  This is for testing, to allow exact agreements.
	if (aboxtest_units_convert .eq. 10) then
	    factdens = 1.0
	    factpres = 1.0
	    factmoist = 1.0
	    factgas = 1.0
	    factaernum = 1.0
	    factaerso4   = 1.0
	    factaerno3   = 1.0
	    factaercl    = 1.0
	    factaermsa   = 1.0
	    factaerco3   = 1.0
	    factaernh4   = 1.0
	    factaerna    = 1.0
	    factaerca    = 1.0
	    factaeroin   = 1.0
	    factaeroc    = 1.0
	    factaerbc    = 1.0
	    factaerhysw  = 1.0
	    factaerwater = 1.0
            factaerpcg1_b_c=1.0
            factaerpcg2_b_c=1.0
            factaerpcg3_b_c=1.0
            factaerpcg4_b_c=1.0
            factaerpcg5_b_c=1.0
            factaerpcg6_b_c=1.0
            factaerpcg7_b_c=1.0
            factaerpcg8_b_c=1.0
            factaerpcg9_b_c=1.0
            factaerpcg1_b_o=1.0
            factaerpcg2_b_o=1.0
            factaerpcg3_b_o=1.0
            factaerpcg4_b_o=1.0
            factaerpcg5_b_o=1.0
            factaerpcg6_b_o=1.0
            factaerpcg7_b_o=1.0
            factaerpcg8_b_o=1.0
            factaerpcg9_b_o=1.0
            factaeropcg1_b_c=1.0
            factaeropcg2_b_c=1.0
            factaeropcg3_b_c=1.0
            factaeropcg4_b_c=1.0
            factaeropcg5_b_c=1.0
            factaeropcg6_b_c=1.0
            factaeropcg7_b_c=1.0
            factaeropcg8_b_c=1.0
            factaeropcg1_b_o=1.0
            factaeropcg2_b_o=1.0
            factaeropcg3_b_o=1.0
            factaeropcg4_b_o=1.0
            factaeropcg5_b_o=1.0
            factaeropcg6_b_o=1.0
            factaeropcg7_b_o=1.0
            factaeropcg8_b_o=1.0
            factaerpcg1_f_c=1.0
            factaerpcg2_f_c=1.0
            factaerpcg3_f_c=1.0
            factaerpcg4_f_c=1.0
            factaerpcg5_f_c=1.0
            factaerpcg6_f_c=1.0
            factaerpcg7_f_c=1.0
            factaerpcg8_f_c=1.0
            factaerpcg9_f_c=1.0
            factaerpcg1_f_o=1.0
            factaerpcg2_f_o=1.0
            factaerpcg3_f_o=1.0
            factaerpcg4_f_o=1.0
            factaerpcg5_f_o=1.0
            factaerpcg6_f_o=1.0
            factaerpcg7_f_o=1.0
            factaerpcg8_f_o=1.0
            factaerpcg9_f_o=1.0
            factaeropcg1_f_c=1.0
            factaeropcg2_f_c=1.0
            factaeropcg3_f_c=1.0
            factaeropcg4_f_c=1.0
            factaeropcg5_f_c=1.0
            factaeropcg6_f_c=1.0
            factaeropcg7_f_c=1.0
            factaeropcg8_f_c=1.0
            factaeropcg1_f_o=1.0
            factaeropcg2_f_o=1.0
            factaeropcg3_f_o=1.0
            factaeropcg4_f_o=1.0
            factaeropcg5_f_o=1.0
            factaeropcg6_f_o=1.0
            factaeropcg7_f_o=1.0
            factaeropcg8_f_o=1.0
            factaersmpa=1.0
            factaersmpbb=1.0
            factaerglyr1=1.0
            factaerglyr2=1.0
            factaerglysfc=1.0
            factaerglynh4=1.0
            factaerglyoh=1.0
            factaerant1_c=1.0
            factaerant2_c=1.0
            factaerant3_c=1.0
            factaerant4_c=1.0
            factaerant1_o=1.0
            factaerant2_o=1.0
            factaerant3_o=1.0
            factaerant4_o=1.0
            factaerbiog1_c=1.0
            factaerbiog2_c=1.0
            factaerbiog3_c=1.0
            factaerbiog4_c=1.0
            factaerbiog1_o=1.0
            factaerbiog2_o=1.0
            factaerbiog3_o=1.0
            factaerbiog4_o=1.0
            factaerasoaX=1.0
            factaerasoa1=1.0
            factaerasoa2=1.0
            factaerasoa3=1.0
            factaerasoa4=1.0
            factaerbsoaX=1.0
            factaerbsoa1=1.0
            factaerbsoa2=1.0
            factaerbsoa3=1.0
            factaerbsoa4=1.0




	end if


!   rce 2005-mar-09 - set ktot in mapaer_tofrom_host;
!	use k_pegshift for calc of ktot and k (=k_peg)
!   k_pegshift = k index shift between wrf-chem and pegasus arrays
	k_pegshift = k_pegbegin - kts

!   set ktot = highest k index for pegasus arrays
!   since kts=1 and k_pegbegin=1, get k_pegshift=0 and ktot=kte-1
	ktot = (kte-1) + k_pegshift
!   *** check that ktot and kte <= kmaxd ***
	if ((kte > kmaxd) .or. (ktot > kmaxd) .or. (ktot <= 0)) then
	    write( msg, '(a,4i5)' )   &
		'*** subr mapaer_tofrom_host -- ' //   &
		'ktot, kmaxd, kts, kte', ktot, kmaxd, kts, kte
	    call peg_message( lunerr, msg )
	    msg = '*** subr mosaic_aerchem_driver -- ' //   &
		'kte>kmaxd OR ktot>kmaxd OR ktot<=0'
	    call peg_error_fatal( lunerr, msg )
	end if

!   rce 2005-apr-28 - changed mapping loops to improve memory access
!   now do rsub(l,k1:k2,m) <--> chem(it,kt1:kt2,jt,l) for each species
	kt1 = ktmaps
	kt2 = ktmape
	k1 = kt1 + k_pegshift
	k2 = kt2 + k_pegshift

	if (imap .gt. 0) goto 2000
 
!
!   imap==0 -- map species and state variables from host arrays 
!              to rsub, cairclm, ptotclm

!   first zero everything (except relhumclm)
	rsub(:,:,:) = 0.0
	cairclm(:) = 0.0
	ptotclm(:) = 0.0
	afracsubarea(:,:) = 0.0
	relhumclm(:) = aboxtest_min_relhum
	rcldwtr_sub(:,:) = 0.0

	adrydens_sub( :,:,:,:) = 0.0
	aqmassdry_sub(:,:,:,:) = 0.0
	aqvoldry_sub( :,:,:,:) = 0.0

!   map gas and aerosol mixing ratios based on aboxtest_map_method
!	1 - map aerosol species and h2so4/hno3/hcl/nh3 using the p_xxx
!       2 - map 181 pegasus species using rsub(l) = chem(l+1)
!       3 - do 2 followed by 1
!	other - same as 1
!   (2 and 3 are for box test purposes)
	if ((aboxtest_map_method .eq. 2) .or.   &
	    (aboxtest_map_method .eq. 3)) then
	    do l = 2, num_chem
		rsub(l,k1:k2,1) = chem(it,kt1:kt2,jt,l)/factgas
	    end do
	end if

	p1st = param_first_scalar
	if (aboxtest_map_method .ne. 2) then
            if (p_h2so4 .ge. p1st) then  
                rsub(kh2so4,k1:k2,1) = chem(it,kt1:kt2,jt,p_h2so4)/factgas
            elseif (p_sulf .ge. p1st)   then
                rsub(kh2so4,k1:k2,1) = chem(it,kt1:kt2,jt,p_sulf)/factgas
            endif

	    if (p_hno3 .ge. p1st)   &
		rsub(khno3,k1:k2,1)  = chem(it,kt1:kt2,jt,p_hno3)/factgas
	    if (p_hcl .ge. p1st)   &
		rsub(khcl,k1:k2,1)   = chem(it,kt1:kt2,jt,p_hcl)/factgas
	    if (p_nh3 .ge. p1st)   &
		rsub(knh3,k1:k2,1)   = chem(it,kt1:kt2,jt,p_nh3)/factgas
	    if (p_n2o5 .ge. p1st)   &
		rsub(kn2o5,k1:k2,1)   = chem(it,kt1:kt2,jt,p_n2o5)/factgas
	    if (p_clno2 .ge. p1st)   &
		rsub(kclno2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_clno2)/factgas

!   rce 2005-apr-12 - added following species for cldchem, here and below:
!   ko3, kso2, kh2o2, khcho, khcooh, koh, kho2, 
!   kno3, kno, kno2, khono, kpan, kch3o2, kch3oh, kch3ooh
	    if (p_o3 .ge. p1st)   &
		rsub(ko3,k1:k2,1)   = chem(it,kt1:kt2,jt,p_o3)/factgas
	    if (p_so2 .ge. p1st)   &
		rsub(kso2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_so2)/factgas
	    if (p_h2o2 .ge. p1st)   &
		rsub(kh2o2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_h2o2)/factgas
	    if (p_hcho .ge. p1st)   &
		rsub(khcho,k1:k2,1)   = chem(it,kt1:kt2,jt,p_hcho)/factgas
	    if (p_ho .ge. p1st)   &
		rsub(koh,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ho)/factgas
	    if (p_ho2 .ge. p1st)   &
		rsub(kho2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ho2)/factgas
	    if (p_no3 .ge. p1st)   &
		rsub(kno3,k1:k2,1)   = chem(it,kt1:kt2,jt,p_no3)/factgas
	    if (p_no .ge. p1st)   &
		rsub(kno,k1:k2,1)   = chem(it,kt1:kt2,jt,p_no)/factgas
	    if (p_no2 .ge. p1st)   &
		rsub(kno2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_no2)/factgas
	    if (p_hono .ge. p1st)   &
		rsub(khono,k1:k2,1)   = chem(it,kt1:kt2,jt,p_hono)/factgas
	    if (p_pan .ge. p1st)   &
		rsub(kpan,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pan)/factgas
            if (p_pcg1_b_c .ge. p1st)   &
                rsub(kpcg1_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg1_b_c)/factgas
            if (p_pcg2_b_c .ge. p1st)   &
                rsub(kpcg2_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg2_b_c)/factgas
            if (p_pcg3_b_c .ge. p1st)   &
                rsub(kpcg3_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg3_b_c)/factgas
            if (p_pcg4_b_c .ge. p1st)   &
                rsub(kpcg4_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg4_b_c)/factgas
            if (p_pcg5_b_c .ge. p1st)   &
                rsub(kpcg5_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg5_b_c)/factgas
            if (p_pcg6_b_c .ge. p1st)   &
                rsub(kpcg6_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg6_b_c)/factgas
            if (p_pcg7_b_c .ge. p1st)   &
                rsub(kpcg7_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg7_b_c)/factgas
            if (p_pcg8_b_c .ge. p1st)   &
                rsub(kpcg8_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg8_b_c)/factgas
            if (p_pcg9_b_c .ge. p1st)   &
                rsub(kpcg9_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg9_b_c)/factgas
            if (p_pcg1_b_o .ge. p1st)   &
                rsub(kpcg1_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg1_b_o)/factgas
            if (p_pcg2_b_o .ge. p1st)   &
                rsub(kpcg2_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg2_b_o)/factgas
            if (p_pcg3_b_o .ge. p1st)   &
                rsub(kpcg3_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg3_b_o)/factgas
            if (p_pcg4_b_o .ge. p1st)   &
                rsub(kpcg4_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg4_b_o)/factgas
            if (p_pcg5_b_o .ge. p1st)   &
                rsub(kpcg5_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg5_b_o)/factgas
            if (p_pcg6_b_o .ge. p1st)   &
                rsub(kpcg6_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg6_b_o)/factgas
            if (p_pcg7_b_o .ge. p1st)   &
                rsub(kpcg7_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg7_b_o)/factgas
            if (p_pcg8_b_o .ge. p1st)   &
                rsub(kpcg8_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg8_b_o)/factgas
            if (p_pcg9_b_o .ge. p1st)   &
                rsub(kpcg9_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg9_b_o)/factgas
            if (p_opcg1_b_c .ge. p1st)   &
                rsub(kopcg1_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg1_b_c)/factgas
            if (p_opcg2_b_c .ge. p1st)   &
                rsub(kopcg2_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg2_b_c)/factgas
            if (p_opcg3_b_c .ge. p1st)   &
                rsub(kopcg3_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg3_b_c)/factgas
            if (p_opcg4_b_c .ge. p1st)   &
                rsub(kopcg4_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg4_b_c)/factgas
            if (p_opcg5_b_c .ge. p1st)   &
                rsub(kopcg5_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg5_b_c)/factgas
            if (p_opcg6_b_c .ge. p1st)   &
                rsub(kopcg6_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg6_b_c)/factgas
            if (p_opcg7_b_c .ge. p1st)   &
                rsub(kopcg7_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg7_b_c)/factgas
            if (p_opcg8_b_c .ge. p1st)   &
                rsub(kopcg8_b_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg8_b_c)/factgas
            if (p_opcg1_b_o .ge. p1st)   &
                rsub(kopcg1_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg1_b_o)/factgas
            if (p_opcg2_b_o .ge. p1st)   &
                rsub(kopcg2_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg2_b_o)/factgas
            if (p_opcg3_b_o .ge. p1st)   &
                rsub(kopcg3_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg3_b_o)/factgas
            if (p_opcg4_b_o .ge. p1st)   &
                rsub(kopcg4_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg4_b_o)/factgas
            if (p_opcg5_b_o .ge. p1st)   &
                rsub(kopcg5_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg5_b_o)/factgas
            if (p_opcg6_b_o .ge. p1st)   &
                rsub(kopcg6_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg6_b_o)/factgas
            if (p_opcg7_b_o .ge. p1st)   &
                rsub(kopcg7_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg7_b_o)/factgas
            if (p_opcg8_b_o .ge. p1st)   &
                rsub(kopcg8_b_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg8_b_o)/factgas
            if (p_pcg1_f_c .ge. p1st)   &
                rsub(kpcg1_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg1_f_c)/factgas
            if (p_pcg2_f_c .ge. p1st)   &
                rsub(kpcg2_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg2_f_c)/factgas
            if (p_pcg3_f_c .ge. p1st)   &
                rsub(kpcg3_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg3_f_c)/factgas
            if (p_pcg4_f_c .ge. p1st)   &
                rsub(kpcg4_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg4_f_c)/factgas
            if (p_pcg5_f_c .ge. p1st)   &
                rsub(kpcg5_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg5_f_c)/factgas
            if (p_pcg6_f_c .ge. p1st)   &
                rsub(kpcg6_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg6_f_c)/factgas
            if (p_pcg7_f_c .ge. p1st)   &
                rsub(kpcg7_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg7_f_c)/factgas
            if (p_pcg8_f_c .ge. p1st)   &
                rsub(kpcg8_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg8_f_c)/factgas
            if (p_pcg9_f_c .ge. p1st)   &
                rsub(kpcg9_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg9_f_c)/factgas
            if (p_pcg1_f_o .ge. p1st)   &
                rsub(kpcg1_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg1_f_o)/factgas
            if (p_pcg2_f_o .ge. p1st)   &
                rsub(kpcg2_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg2_f_o)/factgas
            if (p_pcg3_f_o .ge. p1st)   &
                rsub(kpcg3_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg3_f_o)/factgas
            if (p_pcg4_f_o .ge. p1st)   &
                rsub(kpcg4_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg4_f_o)/factgas
            if (p_pcg5_f_o .ge. p1st)   &
                rsub(kpcg5_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg5_f_o)/factgas
            if (p_pcg6_f_o .ge. p1st)   &
                rsub(kpcg6_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg6_f_o)/factgas
            if (p_pcg7_f_o .ge. p1st)   &
                rsub(kpcg7_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg7_f_o)/factgas
            if (p_pcg8_f_o .ge. p1st)   &
                rsub(kpcg8_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg8_f_o)/factgas
            if (p_pcg9_f_o .ge. p1st)   &
                rsub(kpcg9_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_pcg9_f_o)/factgas
            if (p_opcg1_f_c .ge. p1st)   &
                rsub(kopcg1_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg1_f_c)/factgas
            if (p_opcg2_f_c .ge. p1st)   &
                rsub(kopcg2_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg2_f_c)/factgas
            if (p_opcg3_f_c .ge. p1st)   &
                rsub(kopcg3_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg3_f_c)/factgas
            if (p_opcg4_f_c .ge. p1st)   &
                rsub(kopcg4_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg4_f_c)/factgas
            if (p_opcg5_f_c .ge. p1st)   &
                rsub(kopcg5_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg5_f_c)/factgas
            if (p_opcg6_f_c .ge. p1st)   &
                rsub(kopcg6_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg6_f_c)/factgas
            if (p_opcg7_f_c .ge. p1st)   &
                rsub(kopcg7_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg7_f_c)/factgas
            if (p_opcg8_f_c .ge. p1st)   &
                rsub(kopcg8_f_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg8_f_c)/factgas
            if (p_opcg1_f_o .ge. p1st)   &
                rsub(kopcg1_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg1_f_o)/factgas
            if (p_opcg2_f_o .ge. p1st)   &
                rsub(kopcg2_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg2_f_o)/factgas
            if (p_opcg3_f_o .ge. p1st)   &
                rsub(kopcg3_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg3_f_o)/factgas
            if (p_opcg4_f_o .ge. p1st)   &
                rsub(kopcg4_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg4_f_o)/factgas
            if (p_opcg5_f_o .ge. p1st)   &
                rsub(kopcg5_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg5_f_o)/factgas
            if (p_opcg6_f_o .ge. p1st)   &
                rsub(kopcg6_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg6_f_o)/factgas
            if (p_opcg7_f_o .ge. p1st)   &
                rsub(kopcg7_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg7_f_o)/factgas
            if (p_opcg8_f_o .ge. p1st)   &
                rsub(kopcg8_f_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_opcg8_f_o)/factgas
            if (p_smpa .ge. p1st)   &
                rsub(ksmpa,k1:k2,1)   = chem(it,kt1:kt2,jt,p_smpa)/factgas
            if (p_smpbb .ge. p1st)   &
                rsub(ksmpbb,k1:k2,1)   = chem(it,kt1:kt2,jt,p_smpbb)/factgas
            if (p_gly .ge. p1st)   &
                rsub(kgly,k1:k2,1)   = chem(it,kt1:kt2,jt,p_gly)/factgas
            if (p_ant1_c .ge. p1st)   &
                rsub(kant1_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant1_c)/factgas
            if (p_ant2_c .ge. p1st)   &
                rsub(kant2_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant2_c)/factgas
            if (p_ant3_c .ge. p1st)   &
                rsub(kant3_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant3_c)/factgas
            if (p_ant4_c .ge. p1st)   &
                rsub(kant4_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant4_c)/factgas
            if (p_ant1_o .ge. p1st)   &
                rsub(kant1_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant1_o)/factgas
            if (p_ant2_o .ge. p1st)   &
                rsub(kant2_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant2_o)/factgas
            if (p_ant3_o .ge. p1st)   &
                rsub(kant3_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant3_o)/factgas
            if (p_ant4_o .ge. p1st)   &
                rsub(kant4_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_ant4_o)/factgas
            if (p_biog1_c .ge. p1st)   &
                rsub(kbiog1_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog1_c)/factgas
            if (p_biog2_c .ge. p1st)   &
                rsub(kbiog2_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog2_c)/factgas
            if (p_biog3_c .ge. p1st)   &
                rsub(kbiog3_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog3_c)/factgas
            if (p_biog4_c .ge. p1st)   &
                rsub(kbiog4_c,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog4_c)/factgas
            if (p_biog1_o .ge. p1st)   &
                rsub(kbiog1_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog1_o)/factgas
            if (p_biog2_o .ge. p1st)   &
                rsub(kbiog2_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog2_o)/factgas
            if (p_biog3_o .ge. p1st)   &
                rsub(kbiog3_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog3_o)/factgas
            if (p_biog4_o .ge. p1st)   &
                rsub(kbiog4_o,k1:k2,1)   = chem(it,kt1:kt2,jt,p_biog4_o)/factgas
            if (p_cvasoaX .ge. p1st)   &
                rsub(kasoaX,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvasoaX)/factgas
            if (p_cvasoa1 .ge. p1st)   &
                rsub(kasoa1,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvasoa1)/factgas
            if (p_cvasoa2 .ge. p1st)   &
                rsub(kasoa2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvasoa2)/factgas
            if (p_cvasoa3 .ge. p1st)   &
                rsub(kasoa3,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvasoa3)/factgas
            if (p_cvasoa4 .ge. p1st)   &
                rsub(kasoa4,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvasoa4)/factgas
            if (p_cvbsoaX .ge. p1st)   &
                rsub(kbsoaX,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvbsoaX)/factgas
            if (p_cvbsoa1 .ge. p1st)   &
                rsub(kbsoa1,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvbsoa1)/factgas
            if (p_cvbsoa2 .ge. p1st)   &
                rsub(kbsoa2,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvbsoa2)/factgas
            if (p_cvbsoa3 .ge. p1st)   &
                rsub(kbsoa3,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvbsoa3)/factgas
            if (p_cvbsoa4 .ge. p1st)   &
                rsub(kbsoa4,k1:k2,1)   = chem(it,kt1:kt2,jt,p_cvbsoa4)/factgas


	    do iphase=1,nphase_aer
	    do itype=1,ntype_aer
	    do n = 1, nsize_aer(itype)
		rsub(lptr_so4_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_so4_aer(n,itype,iphase))/factaerso4
		rsub(numptr_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,numptr_aer(n,itype,iphase))/factaernum

		if (lptr_no3_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_no3_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_no3_aer(n,itype,iphase))/factaerno3
		if (lptr_cl_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_cl_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_cl_aer(n,itype,iphase))/factaercl
		if (lptr_msa_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_msa_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_msa_aer(n,itype,iphase))/factaermsa
		if (lptr_co3_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_co3_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_co3_aer(n,itype,iphase))/factaerco3
		if (lptr_nh4_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_nh4_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_nh4_aer(n,itype,iphase))/factaernh4
		if (lptr_na_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_na_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_na_aer(n,itype,iphase))/factaerna
		if (lptr_ca_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_ca_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_ca_aer(n,itype,iphase))/factaerca
		if (lptr_oin_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_oin_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_oin_aer(n,itype,iphase))/factaeroin
		if (lptr_oc_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_oc_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_oc_aer(n,itype,iphase))/factaeroc
		if (lptr_bc_aer(n,itype,iphase) .ge. p1st)   &
		    rsub(lptr_bc_aer(n,itype,iphase),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,lptr_bc_aer(n,itype,iphase))/factaerbc
		if (hyswptr_aer(n,itype) .ge. p1st)   &
		    rsub(hyswptr_aer(n,itype),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,hyswptr_aer(n,itype))/factaerhysw
		if (waterptr_aer(n,itype) .ge. p1st)   &
		    rsub(waterptr_aer(n,itype),k1:k2,1) =   &
		    chem(it,kt1:kt2,jt,waterptr_aer(n,itype))/factaerwater
                if (lptr_pcg1_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg1_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_b_c_aer(n,itype,iphase))/factaerpcg1_b_c
                if (lptr_pcg2_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg2_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_b_c_aer(n,itype,iphase))/factaerpcg2_b_c
                if (lptr_pcg3_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg3_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_b_c_aer(n,itype,iphase))/factaerpcg3_b_c
                if (lptr_pcg4_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg4_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_b_c_aer(n,itype,iphase))/factaerpcg4_b_c
                if (lptr_pcg5_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg5_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_b_c_aer(n,itype,iphase))/factaerpcg5_b_c
                if (lptr_pcg6_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg6_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_b_c_aer(n,itype,iphase))/factaerpcg6_b_c
                if (lptr_pcg7_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg7_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_b_c_aer(n,itype,iphase))/factaerpcg7_b_c
                if (lptr_pcg8_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg8_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_b_c_aer(n,itype,iphase))/factaerpcg8_b_c
                if (lptr_pcg9_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg9_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_b_c_aer(n,itype,iphase))/factaerpcg9_b_c
                if (lptr_pcg1_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg1_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_b_o_aer(n,itype,iphase))/factaerpcg1_b_o
                if (lptr_pcg2_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg2_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_b_o_aer(n,itype,iphase))/factaerpcg2_b_o
                if (lptr_pcg3_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg3_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_b_o_aer(n,itype,iphase))/factaerpcg3_b_o
                if (lptr_pcg4_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg4_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_b_o_aer(n,itype,iphase))/factaerpcg4_b_o
                if (lptr_pcg5_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg5_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_b_o_aer(n,itype,iphase))/factaerpcg5_b_o
                if (lptr_pcg6_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg6_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_b_o_aer(n,itype,iphase))/factaerpcg6_b_o
                if (lptr_pcg7_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg7_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_b_o_aer(n,itype,iphase))/factaerpcg7_b_o
                if (lptr_pcg8_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg8_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_b_o_aer(n,itype,iphase))/factaerpcg8_b_o
                if (lptr_pcg9_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg9_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_b_o_aer(n,itype,iphase))/factaerpcg9_b_o
                if (lptr_opcg1_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg1_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_b_c_aer(n,itype,iphase))/factaeropcg1_b_c
                if (lptr_opcg2_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg2_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_b_c_aer(n,itype,iphase))/factaeropcg2_b_c
                if (lptr_opcg3_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg3_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_b_c_aer(n,itype,iphase))/factaeropcg3_b_c
                if (lptr_opcg4_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg4_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_b_c_aer(n,itype,iphase))/factaeropcg4_b_c
                if (lptr_opcg5_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg5_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_b_c_aer(n,itype,iphase))/factaeropcg5_b_c
                if (lptr_opcg6_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg6_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_b_c_aer(n,itype,iphase))/factaeropcg6_b_c
                if (lptr_opcg7_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg7_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_b_c_aer(n,itype,iphase))/factaeropcg7_b_c
                if (lptr_opcg8_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg8_b_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_b_c_aer(n,itype,iphase))/factaeropcg8_b_c
                if (lptr_opcg1_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg1_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_b_o_aer(n,itype,iphase))/factaeropcg1_b_o
                if (lptr_opcg2_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg2_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_b_o_aer(n,itype,iphase))/factaeropcg2_b_o
                if (lptr_opcg3_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg3_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_b_o_aer(n,itype,iphase))/factaeropcg3_b_o
                if (lptr_opcg4_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg4_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_b_o_aer(n,itype,iphase))/factaeropcg4_b_o
                if (lptr_opcg5_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg5_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_b_o_aer(n,itype,iphase))/factaeropcg5_b_o
                if (lptr_opcg6_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg6_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_b_o_aer(n,itype,iphase))/factaeropcg6_b_o
                if (lptr_opcg7_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg7_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_b_o_aer(n,itype,iphase))/factaeropcg7_b_o
                if (lptr_opcg8_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg8_b_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_b_o_aer(n,itype,iphase))/factaeropcg8_b_o
                if (lptr_pcg1_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg1_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_f_c_aer(n,itype,iphase))/factaerpcg1_f_c
                if (lptr_pcg2_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg2_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_f_c_aer(n,itype,iphase))/factaerpcg2_f_c
                if (lptr_pcg3_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg3_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_f_c_aer(n,itype,iphase))/factaerpcg3_f_c
                if (lptr_pcg4_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg4_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_f_c_aer(n,itype,iphase))/factaerpcg4_f_c
                if (lptr_pcg5_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg5_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_f_c_aer(n,itype,iphase))/factaerpcg5_f_c
                if (lptr_pcg6_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg6_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_f_c_aer(n,itype,iphase))/factaerpcg6_f_c
                if (lptr_pcg7_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg7_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_f_c_aer(n,itype,iphase))/factaerpcg7_f_c
                if (lptr_pcg8_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg8_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_f_c_aer(n,itype,iphase))/factaerpcg8_f_c
                if (lptr_pcg9_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg9_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_f_c_aer(n,itype,iphase))/factaerpcg9_f_c
                if (lptr_pcg1_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg1_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_f_o_aer(n,itype,iphase))/factaerpcg1_f_o
                if (lptr_pcg2_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg2_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_f_o_aer(n,itype,iphase))/factaerpcg2_f_o
                if (lptr_pcg3_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg3_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_f_o_aer(n,itype,iphase))/factaerpcg3_f_o
                if (lptr_pcg4_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg4_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_f_o_aer(n,itype,iphase))/factaerpcg4_f_o
                if (lptr_pcg5_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg5_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_f_o_aer(n,itype,iphase))/factaerpcg5_f_o
                if (lptr_pcg6_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg6_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_f_o_aer(n,itype,iphase))/factaerpcg6_f_o
                if (lptr_pcg7_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg7_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_f_o_aer(n,itype,iphase))/factaerpcg7_f_o
                if (lptr_pcg8_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg8_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_f_o_aer(n,itype,iphase))/factaerpcg8_f_o
                if (lptr_pcg9_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_pcg9_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_f_o_aer(n,itype,iphase))/factaerpcg9_f_o
                if (lptr_opcg1_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg1_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_f_c_aer(n,itype,iphase))/factaeropcg1_f_c
                if (lptr_opcg2_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg2_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_f_c_aer(n,itype,iphase))/factaeropcg2_f_c
                if (lptr_opcg3_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg3_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_f_c_aer(n,itype,iphase))/factaeropcg3_f_c
                if (lptr_opcg4_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg4_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_f_c_aer(n,itype,iphase))/factaeropcg4_f_c
                if (lptr_opcg5_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg5_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_f_c_aer(n,itype,iphase))/factaeropcg5_f_c
                if (lptr_opcg6_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg6_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_f_c_aer(n,itype,iphase))/factaeropcg6_f_c
                if (lptr_opcg7_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg7_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_f_c_aer(n,itype,iphase))/factaeropcg7_f_c
                if (lptr_opcg8_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg8_f_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_f_c_aer(n,itype,iphase))/factaeropcg8_f_c
                if (lptr_opcg1_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg1_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_f_o_aer(n,itype,iphase))/factaeropcg1_f_o
                if (lptr_opcg2_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg2_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_f_o_aer(n,itype,iphase))/factaeropcg2_f_o
                if (lptr_opcg3_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg3_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_f_o_aer(n,itype,iphase))/factaeropcg3_f_o
                if (lptr_opcg4_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg4_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_f_o_aer(n,itype,iphase))/factaeropcg4_f_o
                if (lptr_opcg5_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg5_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_f_o_aer(n,itype,iphase))/factaeropcg5_f_o
                if (lptr_opcg6_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg6_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_f_o_aer(n,itype,iphase))/factaeropcg6_f_o
                if (lptr_opcg7_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg7_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_f_o_aer(n,itype,iphase))/factaeropcg7_f_o
                if (lptr_opcg8_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_opcg8_f_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_f_o_aer(n,itype,iphase))/factaeropcg8_f_o
                if (lptr_smpa_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_smpa_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_smpa_aer(n,itype,iphase))/factaersmpa
                if (lptr_smpbb_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_smpbb_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_smpbb_aer(n,itype,iphase))/factaersmpbb
                if (lptr_glysoa_r1_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_glysoa_r1_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_r1_aer(n,itype,iphase))/factaerglyr1
                if (lptr_glysoa_r2_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_glysoa_r2_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_r2_aer(n,itype,iphase))/factaerglyr2
                if (lptr_glysoa_sfc_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_glysoa_sfc_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_sfc_aer(n,itype,iphase))/factaerglysfc
                if (lptr_glysoa_oh_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_glysoa_oh_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_oh_aer(n,itype,iphase))/factaerglyoh
                if (lptr_glysoa_nh4_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_glysoa_nh4_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_nh4_aer(n,itype,iphase))/factaerglynh4
                if (lptr_ant1_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant1_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant1_c_aer(n,itype,iphase))/factaerant1_c
                if (lptr_ant2_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant2_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant2_c_aer(n,itype,iphase))/factaerant2_c
                if (lptr_ant3_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant3_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant3_c_aer(n,itype,iphase))/factaerant3_c
                if (lptr_ant4_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant4_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant4_c_aer(n,itype,iphase))/factaerant4_c
                if (lptr_ant1_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant1_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant1_o_aer(n,itype,iphase))/factaerant1_o
                if (lptr_ant2_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant2_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant2_o_aer(n,itype,iphase))/factaerant2_o
                if (lptr_ant3_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant3_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant3_o_aer(n,itype,iphase))/factaerant3_o
                if (lptr_ant4_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_ant4_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_ant4_o_aer(n,itype,iphase))/factaerant4_o
                if (lptr_biog1_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog1_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog1_c_aer(n,itype,iphase))/factaerbiog1_c
                if (lptr_biog2_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog2_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog2_c_aer(n,itype,iphase))/factaerbiog2_c
                if (lptr_biog3_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog3_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog3_c_aer(n,itype,iphase))/factaerbiog3_c
                if (lptr_biog4_c_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog4_c_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog4_c_aer(n,itype,iphase))/factaerbiog4_c
                if (lptr_biog1_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog1_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog1_o_aer(n,itype,iphase))/factaerbiog1_o
                if (lptr_biog2_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog2_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog2_o_aer(n,itype,iphase))/factaerbiog2_o
                if (lptr_biog3_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog3_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog3_o_aer(n,itype,iphase))/factaerbiog3_o
                if (lptr_biog4_o_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_biog4_o_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_biog4_o_aer(n,itype,iphase))/factaerbiog4_o
                if (lptr_asoaX_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_asoaX_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_asoaX_aer(n,itype,iphase))/factaerasoaX
                if (lptr_asoa1_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_asoa1_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_asoa1_aer(n,itype,iphase))/factaerasoa1
                if (lptr_asoa2_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_asoa2_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_asoa2_aer(n,itype,iphase))/factaerasoa2
                if (lptr_asoa3_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_asoa3_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_asoa3_aer(n,itype,iphase))/factaerasoa3
                if (lptr_asoa4_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_asoa4_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_asoa4_aer(n,itype,iphase))/factaerasoa4
                if (lptr_bsoaX_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_bsoaX_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_bsoaX_aer(n,itype,iphase))/factaerbsoaX
                if (lptr_bsoa1_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_bsoa1_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_bsoa1_aer(n,itype,iphase))/factaerbsoa1
                if (lptr_bsoa2_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_bsoa2_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_bsoa2_aer(n,itype,iphase))/factaerbsoa2
                if (lptr_bsoa3_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_bsoa3_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_bsoa3_aer(n,itype,iphase))/factaerbsoa3
                if (lptr_bsoa4_aer(n,itype,iphase) .ge. p1st)   &
                    rsub(lptr_bsoa4_aer(n,itype,iphase),k1:k2,1) =   &
                    chem(it,kt1:kt2,jt,lptr_bsoa4_aer(n,itype,iphase))/factaerbsoa4

	    end do ! size
	    end do ! type
	    end do ! phase
	end if

!   map state variables
	afracsubarea(k1:k2,1) = 1.0
	rsub(ktemp,k1:k2,1) = t_phy(it,kt1:kt2,jt)
	rsub(kh2o,k1:k2,1) = moist(it,kt1:kt2,jt,p_qv)/factmoist
	cairclm(k1:k2) = rho_phy(it,kt1:kt2,jt)/factdens
	ptotclm(k1:k2) = p_phy(it,kt1:kt2,jt)/factpres
	if (p_qc .ge. p1st)   &
	    rcldwtr_sub(k1:k2,1) = moist(it,kt1:kt2,jt,p_qc)/factmoist

!   compute or get relative humidity, based on aboxtest_rh_method
!	1 - compute from water vapor, temperature, and pressure
!       2 - get from test driver via aboxtest_get_extra_args with iflag=30
!       3 - do both, and use the relhum from test driver
!	other positive - same as 1
!	0 or negative - set to aboxtest_min_relhum

#if defined ( aboxtest_box_testing_active )
! *** start of "box testing" code section ***
!     these code lines should be inactive when running wrf-chem
!
!   get relhumclm from box test driver
 	if ((aboxtest_rh_method .eq. 2) .or.   &
 	    (aboxtest_rh_method .eq. 3)) then
 	    do kt = ktmaps, ktmape
 		k = kt + k_pegshift
 		call aboxtest_get_extra_args( 30,   &
 		    it, jt, k, idum, idum, idum, idum, idum, idum,   &
 		    relhumclm(k), dum )
 	    end do
 	end if
! ***  end  of "box testing" code section ***
#endif

!   compute relhumclm from water vapor, temperature, and pressure
!   *** force relhum to between aboxtest_min/max_relhum
	if ((aboxtest_rh_method .gt. 0) .and.   &
	    (aboxtest_rh_method .ne. 2)) then
	    do kt = ktmaps, ktmape
		k = kt + k_pegshift
		onemeps = 1.0 - 0.622
		dumesat = esat_gchm( rsub(ktemp,k,1) )
		dumrsat = dumesat / (ptotclm(k) - onemeps*dumesat)
		dumrelhum = rsub(kh2o,k,1) / max( dumrsat, 1.e-20 )

		dumrelhum = max( 0.0, min( 0.99, dumrelhum ) )

		if (aboxtest_rh_method .eq. 3) then
!		    write(msg,9720) k, relhumclm(k), dumrelhum,   &
!			(dumrelhum-relhumclm(k))
!9720		    format( 'k,rh1,rh2,2-1', i4, 3f14.10 )
!		    call peg_message( lunerr, msg )
		    continue
		else
		    relhumclm(k) = dumrelhum
		end if
		relhumclm(k) = max( relhumclm(k), aboxtest_min_relhum )
		relhumclm(k) = min( relhumclm(k), aboxtest_max_relhum )
	    end do
	end if

!   *** force temperature to be > aboxtest_min_temp
	do kt = ktmaps, ktmape
	    k = kt + k_pegshift
	    rsub(ktemp,k,1) =   &
		max( rsub(ktemp,k,1), aboxtest_min_temp )
	end do

	return


!
!   imap==1 -- map species from rsub back to host arrays 
!   (map gas and aerosol mixing ratios based on aboxtest_map_method as above)
!
!   when aboxtest_gases_fixed==10, leave gases (h2so4,hno3,...) unchanged
!
2000	continue
!   map gas and aerosol mixing ratios based on aboxtest_map_method
!	1 - map aerosol species and h2so4/hno3/hcl/nh3 using the p_xxx
!       2 - map 181 pegasus species using rsub(l) = chem(l+1)
!       3 - do 2 followed by 1
!	other - same as 1
!   (2 and 3 are for box test purposes)
	if ((aboxtest_map_method .eq. 2) .or.   &
	    (aboxtest_map_method .eq. 3)) then
	    do l = 2, num_chem
		ido_l = 1
		if (aboxtest_gases_fixed .eq. 10) then
		    if ((l .eq. kh2so4  ) .or. (l .eq. khno3  ) .or.   &
		        (l .eq. khcl    ) .or. (l .eq. knh3   ) .or.   &
		        (l .eq. kclno2  ) .or. (l .eq. kn2o5  ) .or.   &
		        (l .eq. ko3     ) .or.                         &
		        (l .eq. kso2    ) .or. (l .eq. kh2o2  ) .or.   &
		        (l .eq. khcho   )  .or.   &
		        (l .eq. koh     ) .or. (l .eq. kho2   ) .or.   &
		        (l .eq. kno3    ) .or. (l .eq. kno    ) .or.   &
		        (l .eq. kno2    ) .or. (l .eq. khono  ) .or.   &
		        (l .eq. kpan    )  .or.   &
                        (l .eq. kpcg1_b_c    ) .or. (l .eq. kpcg2_b_c   ) .or.   &
                        (l .eq. kpcg3_b_c    ) .or. (l .eq. kpcg4_b_c   ) .or.   &
                        (l .eq. kpcg5_b_c    ) .or. (l .eq. kpcg6_b_c   ) .or.   &
                        (l .eq. kpcg7_b_c    ) .or. (l .eq. kpcg8_b_c   ) .or.   &
                        (l .eq. kpcg9_b_c    ) .or. (l .eq. kpcg1_b_o   ) .or.   &
                        (l .eq. kpcg2_b_o    ) .or. (l .eq. kpcg3_b_o   ) .or.   &
                        (l .eq. kpcg4_b_o    ) .or. (l .eq. kpcg5_b_o   ) .or.   &
                        (l .eq. kpcg6_b_o    ) .or. (l .eq. kpcg7_b_o   ) .or.   &
                        (l .eq. kpcg8_b_o    ) .or. (l .eq. kpcg9_b_o   ) .or.   &
                        (l .eq. kopcg1_b_c    ) .or. (l .eq. kopcg2_b_c   ) .or.   &
                        (l .eq. kopcg3_b_c    ) .or. (l .eq. kopcg4_b_c   ) .or.   &
                        (l .eq. kopcg5_b_c    ) .or. (l .eq. kopcg6_b_c   ) .or.   &
                        (l .eq. kopcg7_b_c    ) .or. (l .eq. kopcg8_b_c   ) .or.   &
                        (l .eq. kopcg1_b_o    ) .or. (l .eq. kopcg2_b_o   ) .or.   &
                        (l .eq. kopcg3_b_o    ) .or. (l .eq. kopcg4_b_o   ) .or.   &
                        (l .eq. kopcg5_b_o    ) .or. (l .eq. kopcg6_b_o   ) .or.   &
                        (l .eq. kopcg7_b_o    ) .or. (l .eq. kopcg8_b_o   ) .or.   &
                        (l .eq. kpcg1_f_c    ) .or. (l .eq. kpcg2_f_c   ) .or.   &
                        (l .eq. kpcg3_f_c    ) .or. (l .eq. kpcg4_f_c   ) .or.   &
                        (l .eq. kpcg5_f_c    ) .or. (l .eq. kpcg6_f_c   ) .or.   &
                        (l .eq. kpcg7_f_c    ) .or. (l .eq. kpcg8_f_c   ) .or.   &
                        (l .eq. kpcg9_f_c    ) .or. (l .eq. kpcg1_f_o   ) .or.   &
                        (l .eq. kpcg2_f_o    ) .or. (l .eq. kpcg3_f_o   ) .or.   &
                        (l .eq. kpcg4_f_o    ) .or. (l .eq. kpcg5_f_o   ) .or.   &
                        (l .eq. kpcg6_f_o    ) .or. (l .eq. kpcg7_f_o   ) .or.   &
                        (l .eq. kpcg8_f_o    ) .or. (l .eq. kpcg9_f_o   ) .or.   &
                        (l .eq. kopcg1_f_c    ) .or. (l .eq. kopcg2_f_c   ) .or.   &
                        (l .eq. kopcg3_f_c    ) .or. (l .eq. kopcg4_f_c   ) .or.   &
                        (l .eq. kopcg5_f_c    ) .or. (l .eq. kopcg6_f_c   ) .or.   &
                        (l .eq. kopcg7_f_c    ) .or. (l .eq. kopcg8_f_c   ) .or.   &
                        (l .eq. kopcg1_f_o    ) .or. (l .eq. kopcg2_f_o   ) .or.   &
                        (l .eq. kopcg3_f_o    ) .or. (l .eq. kopcg4_f_o   ) .or.   &
                        (l .eq. kopcg5_f_o    ) .or. (l .eq. kopcg6_f_o   ) .or.   &
                        (l .eq. kopcg7_f_o    ) .or. (l .eq. kopcg8_f_o   ) .or.   &
                        (l .eq. ksmpa    ) .or. (l .eq. ksmpbb   ) .or.         &
                        (l .eq. kgly    ) .or. &
                        (l .eq. kant1_c    ) .or. (l .eq. kant2_c   ) .or.         &
                        (l .eq. kant3_c    ) .or. (l .eq. kant4_c   ) .or.         &
                        (l .eq. kant1_o    ) .or. (l .eq. kant2_o   ) .or.         &
                        (l .eq. kant3_o    ) .or. (l .eq. kant4_o   ) .or.         &
                        (l .eq. kbiog1_c    ) .or. (l .eq. kbiog2_c   ) .or.         &
                        (l .eq. kbiog3_c    ) .or. (l .eq. kbiog4_c   ) .or.         &
                        (l .eq. kbiog1_o    ) .or. (l .eq. kbiog2_o   ) .or.         &
                        (l .eq. kbiog3_o    ) .or. (l .eq. kbiog4_o   ) .or. &
                        (l .eq. kasoaX    ) .or. &
                        (l .eq. kasoa1    ) .or. (l .eq. kasoa2   ) .or.         &
                        (l .eq. kasoa3    ) .or. (l .eq. kasoa4   ) .or.         &
                        (l .eq. kbsoaX    ) .or. &
                        (l .eq. kbsoa1    ) .or. (l .eq. kbsoa2   ) .or.         &
                        (l .eq. kbsoa3    ) .or. (l .eq. kbsoa4   )) then
			ido_l = 0
		    end if
		end if
		if (ido_l .gt. 0) then
		    chem(it,kt1:kt2,jt,l) = rsub(l,k1:k2,1)*factgas
		end if
	    end do
	end if

	p1st = param_first_scalar
	if (aboxtest_map_method .ne. 2) then
	  if (aboxtest_gases_fixed .ne. 10) then
            if (p_h2so4 .ge. p1st)   then
                chem(it,kt1:kt2,jt,p_h2so4) = rsub(kh2so4,k1:k2,1)*factgas
           elseif (p_sulf .ge. p1st)   then
                chem(it,kt1:kt2,jt,p_sulf) = rsub(kh2so4,k1:k2,1)*factgas
            endif
	    if (p_hno3 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_hno3)  = rsub(khno3,k1:k2,1)*factgas
	    if (p_hcl .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_hcl)   = rsub(khcl,k1:k2,1)*factgas
	    if (p_nh3 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_nh3)  = rsub(knh3,k1:k2,1)*factgas
	    if (p_n2o5 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_n2o5)  = rsub(kn2o5,k1:k2,1)*factgas
	    if (p_clno2 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_clno2)  = rsub(kclno2,k1:k2,1)*factgas

	    if (p_o3 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_o3)  = rsub(ko3,k1:k2,1)*factgas
	    if (p_so2 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_so2)  = rsub(kso2,k1:k2,1)*factgas
	    if (p_h2o2 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_h2o2)  = rsub(kh2o2,k1:k2,1)*factgas
	    if (p_hcho .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_hcho)  = rsub(khcho,k1:k2,1)*factgas
	    if (p_ho .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_ho)  = rsub(koh,k1:k2,1)*factgas
	    if (p_ho2 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_ho2)  = rsub(kho2,k1:k2,1)*factgas
	    if (p_no3 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_no3)  = rsub(kno3,k1:k2,1)*factgas
	    if (p_no .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_no)  = rsub(kno,k1:k2,1)*factgas
	    if (p_no2 .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_no2)  = rsub(kno2,k1:k2,1)*factgas
	    if (p_hono .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_hono)  = rsub(khono,k1:k2,1)*factgas
	    if (p_pan .ge. p1st)   &
		chem(it,kt1:kt2,jt,p_pan)  = rsub(kpan,k1:k2,1)*factgas
            if (p_pcg1_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg1_b_c)  = rsub(kpcg1_b_c,k1:k2,1)*factgas
            if (p_pcg2_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg2_b_c)  = rsub(kpcg2_b_c,k1:k2,1)*factgas
            if (p_pcg3_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg3_b_c)  = rsub(kpcg3_b_c,k1:k2,1)*factgas
            if (p_pcg4_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg4_b_c)  = rsub(kpcg4_b_c,k1:k2,1)*factgas
            if (p_pcg5_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg5_b_c)  = rsub(kpcg5_b_c,k1:k2,1)*factgas
            if (p_pcg6_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg6_b_c)  = rsub(kpcg6_b_c,k1:k2,1)*factgas
            if (p_pcg7_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg7_b_c)  = rsub(kpcg7_b_c,k1:k2,1)*factgas
            if (p_pcg8_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg8_b_c)  = rsub(kpcg8_b_c,k1:k2,1)*factgas
            if (p_pcg9_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg9_b_c)  = rsub(kpcg9_b_c,k1:k2,1)*factgas
            if (p_pcg1_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg1_b_o)  = rsub(kpcg1_b_o,k1:k2,1)*factgas
            if (p_pcg2_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg2_b_o)  = rsub(kpcg2_b_o,k1:k2,1)*factgas
            if (p_pcg3_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg3_b_o)  = rsub(kpcg3_b_o,k1:k2,1)*factgas
            if (p_pcg4_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg4_b_o)  = rsub(kpcg4_b_o,k1:k2,1)*factgas
            if (p_pcg5_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg5_b_o)  = rsub(kpcg5_b_o,k1:k2,1)*factgas
            if (p_pcg6_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg6_b_o)  = rsub(kpcg6_b_o,k1:k2,1)*factgas
            if (p_pcg7_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg7_b_o)  = rsub(kpcg7_b_o,k1:k2,1)*factgas
            if (p_pcg8_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg8_b_o)  = rsub(kpcg8_b_o,k1:k2,1)*factgas
            if (p_pcg9_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg9_b_o)  = rsub(kpcg9_b_o,k1:k2,1)*factgas
            if (p_opcg1_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg1_b_c)  = rsub(kopcg1_b_c,k1:k2,1)*factgas
            if (p_opcg2_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg2_b_c)  = rsub(kopcg2_b_c,k1:k2,1)*factgas
            if (p_opcg3_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg3_b_c)  = rsub(kopcg3_b_c,k1:k2,1)*factgas
            if (p_opcg4_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg4_b_c)  = rsub(kopcg4_b_c,k1:k2,1)*factgas
            if (p_opcg5_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg5_b_c)  = rsub(kopcg5_b_c,k1:k2,1)*factgas
            if (p_opcg6_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg6_b_c)  = rsub(kopcg6_b_c,k1:k2,1)*factgas
            if (p_opcg7_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg7_b_c)  = rsub(kopcg7_b_c,k1:k2,1)*factgas
            if (p_opcg8_b_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg8_b_c)  = rsub(kopcg8_b_c,k1:k2,1)*factgas
            if (p_opcg1_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg1_b_o)  = rsub(kopcg1_b_o,k1:k2,1)*factgas
            if (p_opcg2_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg2_b_o)  = rsub(kopcg2_b_o,k1:k2,1)*factgas
            if (p_opcg3_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg3_b_o)  = rsub(kopcg3_b_o,k1:k2,1)*factgas
            if (p_opcg4_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg4_b_o)  = rsub(kopcg4_b_o,k1:k2,1)*factgas
            if (p_opcg5_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg5_b_o)  = rsub(kopcg5_b_o,k1:k2,1)*factgas
            if (p_opcg6_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg6_b_o)  = rsub(kopcg6_b_o,k1:k2,1)*factgas
            if (p_opcg7_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg7_b_o)  = rsub(kopcg7_b_o,k1:k2,1)*factgas
            if (p_opcg8_b_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg8_b_o)  = rsub(kopcg8_b_o,k1:k2,1)*factgas
            if (p_pcg1_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg1_f_c)  = rsub(kpcg1_f_c,k1:k2,1)*factgas
            if (p_pcg2_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg2_f_c)  = rsub(kpcg2_f_c,k1:k2,1)*factgas
            if (p_pcg3_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg3_f_c)  = rsub(kpcg3_f_c,k1:k2,1)*factgas
            if (p_pcg4_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg4_f_c)  = rsub(kpcg4_f_c,k1:k2,1)*factgas
            if (p_pcg5_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg5_f_c)  = rsub(kpcg5_f_c,k1:k2,1)*factgas
            if (p_pcg6_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg6_f_c)  = rsub(kpcg6_f_c,k1:k2,1)*factgas
            if (p_pcg7_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg7_f_c)  = rsub(kpcg7_f_c,k1:k2,1)*factgas
            if (p_pcg8_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg8_f_c)  = rsub(kpcg8_f_c,k1:k2,1)*factgas
            if (p_pcg9_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg9_f_c)  = rsub(kpcg9_f_c,k1:k2,1)*factgas
            if (p_pcg1_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg1_f_o)  = rsub(kpcg1_f_o,k1:k2,1)*factgas
            if (p_pcg2_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg2_f_o)  = rsub(kpcg2_f_o,k1:k2,1)*factgas
            if (p_pcg3_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg3_f_o)  = rsub(kpcg3_f_o,k1:k2,1)*factgas
            if (p_pcg4_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg4_f_o)  = rsub(kpcg4_f_o,k1:k2,1)*factgas
            if (p_pcg5_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg5_f_o)  = rsub(kpcg5_f_o,k1:k2,1)*factgas
            if (p_pcg6_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg6_f_o)  = rsub(kpcg6_f_o,k1:k2,1)*factgas
            if (p_pcg7_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg7_f_o)  = rsub(kpcg7_f_o,k1:k2,1)*factgas
            if (p_pcg8_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg8_f_o)  = rsub(kpcg8_f_o,k1:k2,1)*factgas
            if (p_pcg9_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_pcg9_f_o)  = rsub(kpcg9_f_o,k1:k2,1)*factgas
            if (p_opcg1_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg1_f_c)  = rsub(kopcg1_f_c,k1:k2,1)*factgas
            if (p_opcg2_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg2_f_c)  = rsub(kopcg2_f_c,k1:k2,1)*factgas
            if (p_opcg3_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg3_f_c)  = rsub(kopcg3_f_c,k1:k2,1)*factgas
            if (p_opcg4_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg4_f_c)  = rsub(kopcg4_f_c,k1:k2,1)*factgas
            if (p_opcg5_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg5_f_c)  = rsub(kopcg5_f_c,k1:k2,1)*factgas
            if (p_opcg6_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg6_f_c)  = rsub(kopcg6_f_c,k1:k2,1)*factgas
            if (p_opcg7_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg7_f_c)  = rsub(kopcg7_f_c,k1:k2,1)*factgas
            if (p_opcg8_f_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg8_f_c)  = rsub(kopcg8_f_c,k1:k2,1)*factgas
            if (p_opcg1_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg1_f_o)  = rsub(kopcg1_f_o,k1:k2,1)*factgas
            if (p_opcg2_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg2_f_o)  = rsub(kopcg2_f_o,k1:k2,1)*factgas
            if (p_opcg3_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg3_f_o)  = rsub(kopcg3_f_o,k1:k2,1)*factgas
            if (p_opcg4_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg4_f_o)  = rsub(kopcg4_f_o,k1:k2,1)*factgas
            if (p_opcg5_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg5_f_o)  = rsub(kopcg5_f_o,k1:k2,1)*factgas
            if (p_opcg6_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg6_f_o)  = rsub(kopcg6_f_o,k1:k2,1)*factgas
            if (p_opcg7_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg7_f_o)  = rsub(kopcg7_f_o,k1:k2,1)*factgas
            if (p_opcg8_f_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_opcg8_f_o)  = rsub(kopcg8_f_o,k1:k2,1)*factgas
            if (p_smpa .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_smpa)  = rsub(ksmpa,k1:k2,1)*factgas
            if (p_smpbb .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_smpbb)  = rsub(ksmpbb,k1:k2,1)*factgas
            if (p_gly .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_gly)  = rsub(kgly,k1:k2,1)*factgas
            if (p_ant1_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant1_c)  = rsub(kant1_c,k1:k2,1)*factgas
            if (p_ant2_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant2_c)  = rsub(kant2_c,k1:k2,1)*factgas
            if (p_ant3_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant3_c)  = rsub(kant3_c,k1:k2,1)*factgas
            if (p_ant4_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant4_c)  = rsub(kant4_c,k1:k2,1)*factgas
            if (p_ant1_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant1_o)  = rsub(kant1_o,k1:k2,1)*factgas
            if (p_ant2_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant2_o)  = rsub(kant2_o,k1:k2,1)*factgas
            if (p_ant3_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant3_o)  = rsub(kant3_o,k1:k2,1)*factgas
            if (p_ant4_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_ant4_o)  = rsub(kant4_o,k1:k2,1)*factgas
            if (p_biog1_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog1_c)  = rsub(kbiog1_c,k1:k2,1)*factgas
            if (p_biog2_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog2_c)  = rsub(kbiog2_c,k1:k2,1)*factgas
            if (p_biog3_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog3_c)  = rsub(kbiog3_c,k1:k2,1)*factgas
            if (p_biog4_c .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog4_c)  = rsub(kbiog4_c,k1:k2,1)*factgas
            if (p_biog1_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog1_o)  = rsub(kbiog1_o,k1:k2,1)*factgas
            if (p_biog2_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog2_o)  = rsub(kbiog2_o,k1:k2,1)*factgas
            if (p_biog3_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog3_o)  = rsub(kbiog3_o,k1:k2,1)*factgas
            if (p_biog4_o .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_biog4_o)  = rsub(kbiog4_o,k1:k2,1)*factgas
            if (p_cvasoaX .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvasoaX)  = rsub(kasoaX,k1:k2,1)*factgas
            if (p_cvasoa1 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvasoa1)  = rsub(kasoa1,k1:k2,1)*factgas
            if (p_cvasoa2 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvasoa2)  = rsub(kasoa2,k1:k2,1)*factgas
            if (p_cvasoa3 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvasoa3)  = rsub(kasoa3,k1:k2,1)*factgas
            if (p_cvasoa4 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvasoa4)  = rsub(kasoa4,k1:k2,1)*factgas
            if (p_cvbsoaX .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvbsoaX)  = rsub(kbsoaX,k1:k2,1)*factgas
            if (p_cvbsoa1 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvbsoa1)  = rsub(kbsoa1,k1:k2,1)*factgas
            if (p_cvbsoa2 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvbsoa2)  = rsub(kbsoa2,k1:k2,1)*factgas
            if (p_cvbsoa3 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvbsoa3)  = rsub(kbsoa3,k1:k2,1)*factgas
            if (p_cvbsoa4 .ge. p1st)   &
                chem(it,kt1:kt2,jt,p_cvbsoa4)  = rsub(kbsoa4,k1:k2,1)*factgas
	  end if

	    do iphase=1,nphase_aer
	    do itype=1,ntype_aer
	    do n = 1, nsize_aer(itype)
		chem(it,kt1:kt2,jt,lptr_so4_aer(n,itype,iphase)) =   &
		    rsub(lptr_so4_aer(n,itype,iphase),k1:k2,1)*factaerso4
		chem(it,kt1:kt2,jt,numptr_aer(n,itype,iphase)) =   &
		    rsub(numptr_aer(n,itype,iphase),k1:k2,1)*factaernum

		if (lptr_no3_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_no3_aer(n,itype,iphase)) =   &
		    rsub(lptr_no3_aer(n,itype,iphase),k1:k2,1)*factaerno3
		if (lptr_cl_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_cl_aer(n,itype,iphase)) =   &
		    rsub(lptr_cl_aer(n,itype,iphase),k1:k2,1)*factaercl
		if (lptr_msa_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_msa_aer(n,itype,iphase)) =   &
		    rsub(lptr_msa_aer(n,itype,iphase),k1:k2,1)*factaermsa
		if (lptr_co3_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_co3_aer(n,itype,iphase)) =   &
		    rsub(lptr_co3_aer(n,itype,iphase),k1:k2,1)*factaerco3
		if (lptr_nh4_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_nh4_aer(n,itype,iphase)) =   &
		    rsub(lptr_nh4_aer(n,itype,iphase),k1:k2,1)*factaernh4
		if (lptr_na_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_na_aer(n,itype,iphase)) =   &
		    rsub(lptr_na_aer(n,itype,iphase),k1:k2,1)*factaerna
		if (lptr_ca_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_ca_aer(n,itype,iphase)) =   &
		    rsub(lptr_ca_aer(n,itype,iphase),k1:k2,1)*factaerca
		if (lptr_oin_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_oin_aer(n,itype,iphase)) =   &
		    rsub(lptr_oin_aer(n,itype,iphase),k1:k2,1)*factaeroin
		if (lptr_oc_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_oc_aer(n,itype,iphase)) =   &
		    rsub(lptr_oc_aer(n,itype,iphase),k1:k2,1)*factaeroc
		if (lptr_bc_aer(n,itype,iphase) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,lptr_bc_aer(n,itype,iphase)) =   &
		    rsub(lptr_bc_aer(n,itype,iphase),k1:k2,1)*factaerbc
		if (hyswptr_aer(n,itype) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,hyswptr_aer(n,itype)) =   &
		    rsub(hyswptr_aer(n,itype),k1:k2,1)*factaerhysw
		if (waterptr_aer(n,itype) .ge. p1st)   &
		    chem(it,kt1:kt2,jt,waterptr_aer(n,itype)) =   &
		    rsub(waterptr_aer(n,itype),k1:k2,1)*factaerwater
                if (lptr_pcg1_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg1_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg1_b_c
                if (lptr_pcg2_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg2_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg2_b_c
                if (lptr_pcg3_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg3_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg3_b_c
                if (lptr_pcg4_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg4_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg4_b_c
                if (lptr_pcg5_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg5_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg5_b_c
                if (lptr_pcg6_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg6_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg6_b_c
                if (lptr_pcg7_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg7_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg7_b_c
                if (lptr_pcg8_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg8_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg8_b_c
                if (lptr_pcg9_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg9_b_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg9_b_c
                if (lptr_pcg1_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg1_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg1_b_o
                if (lptr_pcg2_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg2_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg2_b_o
                if (lptr_pcg3_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg3_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg3_b_o
                if (lptr_pcg4_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg4_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg4_b_o
                if (lptr_pcg5_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg5_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg5_b_o
                if (lptr_pcg6_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg6_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg6_b_o
                if (lptr_pcg7_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg7_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg7_b_o
                if (lptr_pcg8_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg8_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg8_b_o
                if (lptr_pcg9_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg9_b_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg9_b_o
                if (lptr_opcg1_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg1_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg1_b_c
                if (lptr_opcg2_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg2_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg2_b_c
                if (lptr_opcg3_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg3_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg3_b_c
                if (lptr_opcg4_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg4_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg4_b_c
                if (lptr_opcg5_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg5_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg5_b_c
                if (lptr_opcg6_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg6_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg6_b_c
                if (lptr_opcg7_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg7_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg7_b_c
                if (lptr_opcg8_b_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_b_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg8_b_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg8_b_c
                if (lptr_opcg1_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg1_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg1_b_o
                if (lptr_opcg2_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg2_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg2_b_o
                if (lptr_opcg3_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg3_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg3_b_o
                if (lptr_opcg4_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg4_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg4_b_o
                if (lptr_opcg5_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg5_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg5_b_o
                if (lptr_opcg6_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg6_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg6_b_o
                if (lptr_opcg7_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg7_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg7_b_o
                if (lptr_opcg8_b_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_b_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg8_b_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg8_b_o
                if (lptr_pcg1_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg1_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg1_f_c
                if (lptr_pcg2_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg2_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg2_f_c
                if (lptr_pcg3_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg3_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg3_f_c
                if (lptr_pcg4_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg4_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg4_f_c
                if (lptr_pcg5_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg5_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg5_f_c
                if (lptr_pcg6_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg6_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg6_f_c
                if (lptr_pcg7_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg7_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg7_f_c
                if (lptr_pcg8_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg8_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg8_f_c
                if (lptr_pcg9_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg9_f_c_aer(n,itype,iphase),k1:k2,1)*factaerpcg9_f_c
                if (lptr_pcg1_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg1_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg1_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg1_f_o
                if (lptr_pcg2_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg2_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg2_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg2_f_o
                if (lptr_pcg3_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg3_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg3_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg3_f_o
                if (lptr_pcg4_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg4_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg4_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg4_f_o
                if (lptr_pcg5_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg5_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg5_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg5_f_o
                if (lptr_pcg6_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg6_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg6_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg6_f_o
                if (lptr_pcg7_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg7_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg7_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg7_f_o
                if (lptr_pcg8_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg8_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg8_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg8_f_o
                if (lptr_pcg9_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_pcg9_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_pcg9_f_o_aer(n,itype,iphase),k1:k2,1)*factaerpcg9_f_o
                if (lptr_opcg1_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg1_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg1_f_c
                if (lptr_opcg2_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg2_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg2_f_c
                if (lptr_opcg3_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg3_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg3_f_c
                if (lptr_opcg4_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg4_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg4_f_c
                if (lptr_opcg5_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg5_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg5_f_c
                if (lptr_opcg6_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg6_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg6_f_c
                if (lptr_opcg7_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg7_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg7_f_c
                if (lptr_opcg8_f_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_f_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg8_f_c_aer(n,itype,iphase),k1:k2,1)*factaeropcg8_f_c
                if (lptr_opcg1_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg1_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg1_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg1_f_o
                if (lptr_opcg2_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg2_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg2_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg2_f_o
                if (lptr_opcg3_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg3_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg3_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg3_f_o
                if (lptr_opcg4_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg4_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg4_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg4_f_o
                if (lptr_opcg5_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg5_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg5_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg5_f_o
                if (lptr_opcg6_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg6_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg6_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg6_f_o
                if (lptr_opcg7_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg7_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg7_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg7_f_o
                if (lptr_opcg8_f_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_opcg8_f_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_opcg8_f_o_aer(n,itype,iphase),k1:k2,1)*factaeropcg8_f_o
               if (lptr_smpa_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_smpa_aer(n,itype,iphase)) =   &
                    rsub(lptr_smpa_aer(n,itype,iphase),k1:k2,1)*factaersmpa
                if (lptr_smpbb_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_smpbb_aer(n,itype,iphase)) =   &
                    rsub(lptr_smpbb_aer(n,itype,iphase),k1:k2,1)*factaersmpbb
                if (lptr_glysoa_r1_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_r1_aer(n,itype,iphase)) =   &
                    rsub(lptr_glysoa_r1_aer(n,itype,iphase),k1:k2,1)*factaerglyr1
                if (lptr_glysoa_r2_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_r2_aer(n,itype,iphase)) =   &
                    rsub(lptr_glysoa_r2_aer(n,itype,iphase),k1:k2,1)*factaerglyr2
                if (lptr_glysoa_sfc_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_sfc_aer(n,itype,iphase)) =   &
                    rsub(lptr_glysoa_sfc_aer(n,itype,iphase),k1:k2,1)*factaerglysfc
                if (lptr_glysoa_oh_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_oh_aer(n,itype,iphase)) =   &
                    rsub(lptr_glysoa_oh_aer(n,itype,iphase),k1:k2,1)*factaerglyoh
                if (lptr_glysoa_nh4_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_glysoa_nh4_aer(n,itype,iphase)) =   &
                    rsub(lptr_glysoa_nh4_aer(n,itype,iphase),k1:k2,1)*factaerglynh4
                if (lptr_ant1_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant1_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant1_c_aer(n,itype,iphase),k1:k2,1)*factaerant1_c
                if (lptr_ant2_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant2_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant2_c_aer(n,itype,iphase),k1:k2,1)*factaerant2_c
                if (lptr_ant3_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant3_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant3_c_aer(n,itype,iphase),k1:k2,1)*factaerant3_c
                if (lptr_ant4_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant4_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant4_c_aer(n,itype,iphase),k1:k2,1)*factaerant4_c
                if (lptr_ant1_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant1_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant1_o_aer(n,itype,iphase),k1:k2,1)*factaerant1_o
                if (lptr_ant2_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant2_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant2_o_aer(n,itype,iphase),k1:k2,1)*factaerant2_o
                if (lptr_ant3_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant3_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant3_o_aer(n,itype,iphase),k1:k2,1)*factaerant3_o
                if (lptr_ant4_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_ant4_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_ant4_o_aer(n,itype,iphase),k1:k2,1)*factaerant4_o
                if (lptr_biog1_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog1_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog1_c_aer(n,itype,iphase),k1:k2,1)*factaerbiog1_c
                if (lptr_biog2_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog2_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog2_c_aer(n,itype,iphase),k1:k2,1)*factaerbiog2_c
                if (lptr_biog3_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog3_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog3_c_aer(n,itype,iphase),k1:k2,1)*factaerbiog3_c
                if (lptr_biog4_c_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog4_c_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog4_c_aer(n,itype,iphase),k1:k2,1)*factaerbiog4_c
                if (lptr_biog1_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog1_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog1_o_aer(n,itype,iphase),k1:k2,1)*factaerbiog1_o
                if (lptr_biog2_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog2_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog2_o_aer(n,itype,iphase),k1:k2,1)*factaerbiog2_o
                if (lptr_biog3_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog3_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog3_o_aer(n,itype,iphase),k1:k2,1)*factaerbiog3_o
                if (lptr_biog4_o_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_biog4_o_aer(n,itype,iphase)) =   &
                    rsub(lptr_biog4_o_aer(n,itype,iphase),k1:k2,1)*factaerbiog4_o
                if (lptr_asoaX_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_asoaX_aer(n,itype,iphase)) =   &
                    rsub(lptr_asoaX_aer(n,itype,iphase),k1:k2,1)*factaerasoaX
                if (lptr_asoa1_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_asoa1_aer(n,itype,iphase)) =   &
                    rsub(lptr_asoa1_aer(n,itype,iphase),k1:k2,1)*factaerasoa1
                if (lptr_asoa2_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_asoa2_aer(n,itype,iphase)) =   &
                    rsub(lptr_asoa2_aer(n,itype,iphase),k1:k2,1)*factaerasoa2
                if (lptr_asoa3_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_asoa3_aer(n,itype,iphase)) =   &
                    rsub(lptr_asoa3_aer(n,itype,iphase),k1:k2,1)*factaerasoa3
                if (lptr_asoa4_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_asoa4_aer(n,itype,iphase)) =   &
                    rsub(lptr_asoa4_aer(n,itype,iphase),k1:k2,1)*factaerasoa4
                if (lptr_bsoaX_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_bsoaX_aer(n,itype,iphase)) =   &
                    rsub(lptr_bsoaX_aer(n,itype,iphase),k1:k2,1)*factaerbsoaX
                if (lptr_bsoa1_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_bsoa1_aer(n,itype,iphase)) =   &
                    rsub(lptr_bsoa1_aer(n,itype,iphase),k1:k2,1)*factaerbsoa1
                if (lptr_bsoa2_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_bsoa2_aer(n,itype,iphase)) =   &
                    rsub(lptr_bsoa2_aer(n,itype,iphase),k1:k2,1)*factaerbsoa2
                if (lptr_bsoa3_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_bsoa3_aer(n,itype,iphase)) =   &
                    rsub(lptr_bsoa3_aer(n,itype,iphase),k1:k2,1)*factaerbsoa3
                if (lptr_bsoa4_aer(n,itype,iphase) .ge. p1st)   &
                    chem(it,kt1:kt2,jt,lptr_bsoa4_aer(n,itype,iphase)) =   &
                    rsub(lptr_bsoa4_aer(n,itype,iphase),k1:k2,1)*factaerbsoa4

                      

	    end do ! size
	    end do ! type
	    end do ! phase
	end if


	return

	end subroutine mapaer_tofrom_host


!-----------------------------------------------------------------------
! *** note - eventually is_aerosol will be a subr argument
	subroutine init_data_mosaic_asect( id, config_flags, vbs_nbin, is_aerosol )
!	subroutine init_data_mosaic_asect( id, n2o5_hetchem, is_aerosol )
!	subroutine init_data_mosaic_asect( )

	use module_data_mosaic_asect
	use module_data_mosaic_other, only:  lunerr, lunout,   &
		aboxtest_testmode, aboxtest_units_convert,   &
		aboxtest_rh_method, aboxtest_map_method,   &
		aboxtest_gases_fixed, aboxtest_min_temp,   &
		aboxtest_min_relhum, aboxtest_max_relhum
	use module_data_mosaic_therm, only:  nbin_a, nbin_a_maxd, n2o5_flag
	use module_mosaic2_driver, only:  init_data_mosaic2_asect
	use module_mosaic_csuesat, only:  init_csuesat
	use module_mosaic_movesect, only:  move_sections, test_move_sections
	use module_peg_util, only:  peg_error_fatal


	use module_configure, only:   &
		grid_config_rec_type, &
		p_so4_a01, p_so4_a02, p_so4_a03, p_so4_a04,   &
		p_so4_a05, p_so4_a06, p_so4_a07, p_so4_a08
#if defined ( cw_species_are_in_registry )
        use module_configure, only:   &
	   p_so4_cw01, p_no3_cw01, p_cl_cw01, p_nh4_cw01, p_na_cw01,   &
	   p_so4_cw02, p_no3_cw02, p_cl_cw02, p_nh4_cw02, p_na_cw02,   &
	   p_so4_cw03, p_no3_cw03, p_cl_cw03, p_nh4_cw03, p_na_cw03,   &
	   p_so4_cw04, p_no3_cw04, p_cl_cw04, p_nh4_cw04, p_na_cw04,   &
	   p_so4_cw05, p_no3_cw05, p_cl_cw05, p_nh4_cw05, p_na_cw05,   &
	   p_so4_cw06, p_no3_cw06, p_cl_cw06, p_nh4_cw06, p_na_cw06,   &
	   p_so4_cw07, p_no3_cw07, p_cl_cw07, p_nh4_cw07, p_na_cw07,   &
	   p_so4_cw08, p_no3_cw08, p_cl_cw08, p_nh4_cw08, p_na_cw08,   &
	   p_oin_cw01, p_oc_cw01,  p_bc_cw01, p_num_cw01,              &
	   p_oin_cw02, p_oc_cw02,  p_bc_cw02, p_num_cw02,              &
	   p_oin_cw03, p_oc_cw03,  p_bc_cw03, p_num_cw03,              &
	   p_oin_cw04, p_oc_cw04,  p_bc_cw04, p_num_cw04,              &
	   p_oin_cw05, p_oc_cw05,  p_bc_cw05, p_num_cw05,              &
	   p_oin_cw06, p_oc_cw06,  p_bc_cw06, p_num_cw06,              &
	   p_oin_cw07, p_oc_cw07,  p_bc_cw07, p_num_cw07,              &
	   p_oin_cw08, p_oc_cw08,  p_bc_cw08, p_num_cw08,              &
     p_glysoa_r1_cw01, p_glysoa_r2_cw01, p_glysoa_sfc_cw01, p_glysoa_nh4_cw01, p_glysoa_oh_cw01,    &
     p_glysoa_r1_cw02, p_glysoa_r2_cw02, p_glysoa_sfc_cw02, p_glysoa_nh4_cw02, p_glysoa_oh_cw02,    &
     p_glysoa_r1_cw03, p_glysoa_r2_cw03, p_glysoa_sfc_cw03, p_glysoa_nh4_cw03, p_glysoa_oh_cw03,    &
     p_glysoa_r1_cw04, p_glysoa_r2_cw04, p_glysoa_sfc_cw04, p_glysoa_nh4_cw04, p_glysoa_oh_cw04,    &
     p_asoaX_cw01, p_asoa1_cw01, p_asoa2_cw01, p_asoa3_cw01, p_asoa4_cw01,     &
     p_bsoaX_cw01, p_bsoa1_cw01, p_bsoa2_cw01, p_bsoa3_cw01, p_bsoa4_cw01,     &
     p_asoaX_cw02, p_asoa1_cw02, p_asoa2_cw02, p_asoa3_cw02, p_asoa4_cw02,     &
     p_bsoaX_cw02, p_bsoa1_cw02, p_bsoa2_cw02, p_bsoa3_cw02, p_bsoa4_cw02,     &
     p_asoaX_cw03, p_asoa1_cw03, p_asoa2_cw03, p_asoa3_cw03, p_asoa4_cw03,     &
     p_bsoaX_cw03, p_bsoa1_cw03, p_bsoa2_cw03, p_bsoa3_cw03, p_bsoa4_cw03,     &
     p_asoaX_cw04, p_asoa1_cw04, p_asoa2_cw04, p_asoa3_cw04, p_asoa4_cw04,     &
     p_bsoaX_cw04, p_bsoa1_cw04, p_bsoa2_cw04, p_bsoa3_cw04, p_bsoa4_cw04,              &
       p_pcg1_b_c_cw01,p_pcg1_b_o_cw01,p_opcg1_b_c_cw01,p_opcg1_b_o_cw01, &
           p_pcg1_f_c_cw01,p_pcg1_f_o_cw01,p_opcg1_f_c_cw01,p_opcg1_f_o_cw01, &
           p_ant1_c_cw01,p_biog1_c_cw01,                                      &
           p_pcg1_b_c_cw02,p_pcg1_b_o_cw02,p_opcg1_b_c_cw02,p_opcg1_b_o_cw02, &
           p_pcg1_f_c_cw02,p_pcg1_f_o_cw02,p_opcg1_f_c_cw02,p_opcg1_f_o_cw02, &
           p_ant1_c_cw02,p_biog1_c_cw02,                                      &
           p_pcg1_b_c_cw03,p_pcg1_b_o_cw03,p_opcg1_b_c_cw03,p_opcg1_b_o_cw03, &
           p_pcg1_f_c_cw03,p_pcg1_f_o_cw03,p_opcg1_f_c_cw03,p_opcg1_f_o_cw03, &
           p_ant1_c_cw03,p_biog1_c_cw03,                                      &
           p_pcg1_b_c_cw04,p_pcg1_b_o_cw04,p_opcg1_b_c_cw04,p_opcg1_b_o_cw04, &
           p_pcg1_f_c_cw04,p_pcg1_f_o_cw04,p_opcg1_f_c_cw04,p_opcg1_f_o_cw04, &
           p_ant1_c_cw04,p_biog1_c_cw04,                                      &
           p_pcg1_b_c_cw05,p_pcg1_b_o_cw05,p_opcg1_b_c_cw05,p_opcg1_b_o_cw05, &
           p_pcg1_f_c_cw05,p_pcg1_f_o_cw05,p_opcg1_f_c_cw05,p_opcg1_f_o_cw05, &
           p_ant1_c_cw05,p_biog1_c_cw05,                                      &
           p_pcg1_b_c_cw06,p_pcg1_b_o_cw06,p_opcg1_b_c_cw06,p_opcg1_b_o_cw06, &
           p_pcg1_f_c_cw06,p_pcg1_f_o_cw06,p_opcg1_f_c_cw06,p_opcg1_f_o_cw06, &
           p_ant1_c_cw06,p_biog1_c_cw06,                                      &
           p_pcg1_b_c_cw07,p_pcg1_b_o_cw07,p_opcg1_b_c_cw07,p_opcg1_b_o_cw07, &
           p_pcg1_f_c_cw07,p_pcg1_f_o_cw07,p_opcg1_f_c_cw07,p_opcg1_f_o_cw07, &
           p_ant1_c_cw07,p_biog1_c_cw07,                                      &
           p_pcg1_b_c_cw08,p_pcg1_b_o_cw08,p_opcg1_b_c_cw08,p_opcg1_b_o_cw08, &
           p_pcg1_f_c_cw08,p_pcg1_f_o_cw08,p_opcg1_f_c_cw08,p_opcg1_f_o_cw08, &
           p_ant1_c_cw08,p_biog1_c_cw08
#endif

	use module_state_description, only:  param_first_scalar, num_chem

	implicit none

! *** note - eventually is_aerosol will be a subr argument
	logical, intent(out) :: is_aerosol(num_chem)
!   control flag for N2O5 het chem scheme
	integer, intent(in) :: id, &
	                       vbs_nbin(1)

	type(grid_config_rec_type), intent(in) :: config_flags

!   local variables
	integer idum, itype, jdum, l, ldum, n, nhi, nsize_aer_dum
	real dum
	real, parameter :: pi = 3.14159265

!
!   set some "pegasus" control variables
!
	msectional = 20
	maerocoag = -2
	maerchem = 1
	maeroptical = 1
	maerchem_boxtest_output = -1
! set some control flags using config_flags%mosaic_aerchem_optaa
        idum = max( config_flags%mosaic_aerchem_optaa, 0 )
        if (idum >= 10000) then
            jdum = mod(idum,100)/10  ! 10s digit controls movesect
            if (jdum == 0) msectional =  0  ! causes method_movesect =  0
            if (jdum == 1) msectional = 10  ! causes method_movesect = 10
            if (jdum == 2) msectional = 20  ! causes method_movesect = 20

            jdum = mod(idum,1000)/100  ! 100s digit controls newnuc

            jdum = mod(idum,10000)/1000  ! 1000s digit controls coag
        end if ! (idum >= 100 0) the

!
!   set ntype_aer = 1
!
	ntype_aer = 1

!
!   set number of aerosol bins using the wrf-chem sulfate pointers
!
	nsize_aer(:) = 0
        itype=1
	if (p_so4_a01 .ge. param_first_scalar) nsize_aer(itype) = 1
	if (p_so4_a02 .ge. param_first_scalar) nsize_aer(itype) = 2
	if (p_so4_a03 .ge. param_first_scalar) nsize_aer(itype) = 3
	if (p_so4_a04 .ge. param_first_scalar) nsize_aer(itype) = 4
	if (p_so4_a05 .ge. param_first_scalar) nsize_aer(itype) = 5
	if (p_so4_a06 .ge. param_first_scalar) nsize_aer(itype) = 6
	if (p_so4_a07 .ge. param_first_scalar) nsize_aer(itype) = 7
	if (p_so4_a08 .ge. param_first_scalar) nsize_aer(itype) = 8

	if (nsize_aer(itype) .le. 0) then
	    call peg_error_fatal( lunerr,   &
		'init_data_mosaic_asect - nsize_aer = 0' )
	else if (nsize_aer(itype) .gt. maxd_asize) then
	    call peg_error_fatal( lunerr,   &
		'init_data_mosaic_asect - nsize_aer > maxd_asize' )
	end if

!
!   set nbin_a to total number of aerosol bins (for all types)
!
	nbin_a = 0
	do itype = 1, ntype_aer
	    nbin_a = nbin_a + nsize_aer(itype)
	end do
	if (nbin_a .gt. nbin_a_maxd) then
	    call peg_error_fatal( lunerr,   &
		'init_data_mosaic_asect - nbin_a > nbin_a_maxd' )
	end if

!
!   set nphase_aer (number of active aerosol species phases),
!   the xx_phase, and maerosolincw
!
	nphase_aer = 0
	maerosolincw = 0
	if (nsize_aer(1) .gt. 0) then
	    nphase_aer = 1
	    ai_phase = 1

#if defined ( cw_species_are_in_registry )
	    if (p_so4_cw01 .ge. param_first_scalar) then
		nphase_aer = 2
		cw_phase = 2
		maerosolincw = 1
	    end if
#endif
	end if


#if defined ( aboxtest_box_testing_active )
! *** start of "box testing" code section ***
!     these code lines should be inactive when running wrf-chem
!
!   set some variables to "box test" values
 	call aboxtest_get_extra_args( 10,   &
 		msectional, maerosolincw, maerocoag,   &
 		maerchem, maeroptical, maerchem_boxtest_output,   &
 		lunerr, lunout, idum, dum, dum )
 	call aboxtest_get_extra_args( 11,   &
 		aboxtest_testmode, aboxtest_units_convert,   &
 		aboxtest_rh_method, aboxtest_map_method,   &
 		aboxtest_gases_fixed, nsize_aer_dum,   &
 		idum, idum, idum, dum, dum )
 
 	itype = 1
 	if (nsize_aer_dum > 0) nsize_aer(itype) = nsize_aer_dum
 
 	aboxtest_min_temp = 0.0
 	aboxtest_min_relhum = 0.0
 	aboxtest_max_relhum = 1.0
! ***  end  of "box testing" code section ***
#endif

! copy over N2O5 control flag (DL 9/6/2013)
n2o5_flag = config_flags%n2o5_hetchem

!
!   set master aerosol chemical types
!
!	ntot_mastercomp_aer = 96
	ntot_mastercomp_aer = 111

	l = 1
	mastercompindx_so4_aer = l
	name_mastercomp_aer( l ) = 'sulfate'
	dens_mastercomp_aer( l ) =  dens_so4_aer
	mw_mastercomp_aer(   l ) =    mw_so4_aer
	hygro_mastercomp_aer(l ) = hygro_so4_aer

	l = 2
	mastercompindx_no3_aer = l
	name_mastercomp_aer( l ) = 'nitrate'
	dens_mastercomp_aer( l ) =  dens_no3_aer
	mw_mastercomp_aer(   l ) =    mw_no3_aer
	hygro_mastercomp_aer(l ) = hygro_no3_aer

	l = 3
	mastercompindx_cl_aer = l
	name_mastercomp_aer( l ) = 'chloride'
	dens_mastercomp_aer( l ) =  dens_cl_aer
	mw_mastercomp_aer(   l ) =    mw_cl_aer
	hygro_mastercomp_aer(l ) = hygro_cl_aer

	l = 4
	mastercompindx_co3_aer = l
	name_mastercomp_aer( l ) = 'carbonate'
	dens_mastercomp_aer( l ) =  dens_co3_aer
	mw_mastercomp_aer(   l ) =    mw_co3_aer
	hygro_mastercomp_aer(l ) = hygro_co3_aer

	l = 5
	mastercompindx_nh4_aer = l
	name_mastercomp_aer( l ) = 'ammonium'
	dens_mastercomp_aer( l ) =  dens_nh4_aer
	mw_mastercomp_aer(   l ) =    mw_nh4_aer
	hygro_mastercomp_aer(l ) = hygro_nh4_aer

	l = 6
	mastercompindx_na_aer = l
	name_mastercomp_aer( l ) = 'sodium'
	dens_mastercomp_aer( l ) =  dens_na_aer
	mw_mastercomp_aer(   l ) =    mw_na_aer
	hygro_mastercomp_aer(l ) = hygro_na_aer

	l = 7
	mastercompindx_ca_aer = l
	name_mastercomp_aer( l ) = 'calcium'
	dens_mastercomp_aer( l ) =  dens_ca_aer
	mw_mastercomp_aer(   l ) =    mw_ca_aer
	hygro_mastercomp_aer(l ) = hygro_ca_aer

	l = 8
	mastercompindx_oin_aer = l
	name_mastercomp_aer( l ) = 'otherinorg'
	dens_mastercomp_aer( l ) =  dens_oin_aer
	mw_mastercomp_aer(   l ) =    mw_oin_aer
	hygro_mastercomp_aer(l ) = hygro_oin_aer

	l = 9
	mastercompindx_oc_aer = l
	name_mastercomp_aer( l ) = 'organic-c'
	dens_mastercomp_aer( l ) =  dens_oc_aer
	mw_mastercomp_aer(   l ) =    mw_oc_aer
	hygro_mastercomp_aer(l ) = hygro_oc_aer

	l = 10
	mastercompindx_bc_aer = l
	name_mastercomp_aer( l ) = 'black-c'
	dens_mastercomp_aer( l ) =  dens_bc_aer
	mw_mastercomp_aer(   l ) =    mw_bc_aer
	hygro_mastercomp_aer(l ) = hygro_bc_aer

        l = 11
        mastercompindx_pcg1_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg1_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg1_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg1_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg1_b_c_aer

        l = 12
        mastercompindx_pcg2_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg2_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg2_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg2_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg2_b_c_aer

        l = 13
        mastercompindx_pcg3_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg3_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg3_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg3_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg3_b_c_aer

        l = 14
        mastercompindx_pcg4_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg4_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg4_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg4_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg4_b_c_aer

        l = 15
        mastercompindx_pcg5_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg5_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg5_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg5_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg5_b_c_aer

        l = 16
        mastercompindx_pcg6_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg6_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg6_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg6_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg6_b_c_aer

        l = 17
        mastercompindx_pcg7_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg7_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg7_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg7_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg7_b_c_aer

        l = 18
        mastercompindx_pcg8_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg8_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg8_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg8_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg8_b_c_aer

        l = 19
        mastercompindx_pcg9_b_c_aer = l
        name_mastercomp_aer( l ) = 'pcg9_b_c'
        dens_mastercomp_aer( l ) =  dens_pcg9_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg9_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg9_b_c_aer

        l = 20
        mastercompindx_pcg1_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg1_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg1_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg1_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg1_b_o_aer

        l = 21
        mastercompindx_pcg2_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg2_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg2_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg2_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg2_b_o_aer

        l = 22
        mastercompindx_pcg3_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg3_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg3_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg3_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg3_b_o_aer

        l = 23
        mastercompindx_pcg4_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg4_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg4_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg4_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg4_b_o_aer

        l = 24
        mastercompindx_pcg5_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg5_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg5_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg5_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg5_b_o_aer

        l = 25
        mastercompindx_pcg6_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg6_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg6_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg6_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg6_b_o_aer

        l = 26
        mastercompindx_pcg7_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg7_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg7_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg7_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg7_b_o_aer

        l = 27
        mastercompindx_pcg8_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg8_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg8_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg8_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg8_b_o_aer

        l = 28
        mastercompindx_pcg9_b_o_aer = l
        name_mastercomp_aer( l ) = 'pcg9_b_o'
        dens_mastercomp_aer( l ) =  dens_pcg9_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg9_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg9_b_o_aer

        l = 29
        mastercompindx_opcg1_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg1_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg1_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg1_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg1_b_c_aer

        l = 30
        mastercompindx_opcg2_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg2_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg2_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg2_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg2_b_c_aer

        l = 31
        mastercompindx_opcg3_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg3_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg3_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg3_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg3_b_c_aer

        l = 32
        mastercompindx_opcg4_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg4_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg4_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg4_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg4_b_c_aer

        l = 33
        mastercompindx_opcg5_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg5_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg5_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg5_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg5_b_c_aer

        l = 34
        mastercompindx_opcg6_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg6_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg6_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg6_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg6_b_c_aer

        l = 35
        mastercompindx_opcg7_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg7_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg7_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg7_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg7_b_c_aer

        l = 36
        mastercompindx_opcg8_b_c_aer = l
        name_mastercomp_aer( l ) = 'opcg8_b_c'
        dens_mastercomp_aer( l ) =  dens_opcg8_b_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg8_b_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg8_b_c_aer

        l = 37
        mastercompindx_opcg1_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg1_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg1_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg1_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg1_b_o_aer

        l = 38
        mastercompindx_opcg2_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg2_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg2_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg2_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg2_b_o_aer

        l = 39
        mastercompindx_opcg3_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg3_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg3_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg3_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg3_b_o_aer

        l = 40
        mastercompindx_opcg4_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg4_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg4_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg4_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg4_b_o_aer

        l = 41
        mastercompindx_opcg5_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg5_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg5_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg5_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg5_b_o_aer

        l = 42
        mastercompindx_opcg6_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg6_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg6_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg6_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg6_b_o_aer

        l = 43
        mastercompindx_opcg7_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg7_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg7_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg7_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg7_b_o_aer

        l = 44
        mastercompindx_opcg8_b_o_aer = l
        name_mastercomp_aer( l ) = 'opcg8_b_o'
        dens_mastercomp_aer( l ) =  dens_opcg8_b_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg8_b_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg8_b_o_aer

        l = 45
        mastercompindx_pcg1_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg1_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg1_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg1_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg1_f_c_aer

        l = 46
        mastercompindx_pcg2_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg2_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg2_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg2_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg2_f_c_aer

        l = 47
        mastercompindx_pcg3_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg3_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg3_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg3_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg3_f_c_aer

        l = 48
        mastercompindx_pcg4_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg4_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg4_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg4_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg4_f_c_aer

        l = 49
        mastercompindx_pcg5_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg5_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg5_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg5_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg5_f_c_aer

        l = 50
        mastercompindx_pcg6_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg6_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg6_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg6_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg6_f_c_aer

        l = 51
        mastercompindx_pcg7_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg7_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg7_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg7_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg7_f_c_aer

        l = 52
        mastercompindx_pcg8_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg8_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg8_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg8_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg8_f_c_aer

        l = 53
        mastercompindx_pcg9_f_c_aer = l
        name_mastercomp_aer( l ) = 'pcg9_f_c'
        dens_mastercomp_aer( l ) =  dens_pcg9_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_pcg9_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_pcg9_f_c_aer

        l = 54
        mastercompindx_pcg1_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg1_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg1_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg1_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg1_f_o_aer

        l = 55
        mastercompindx_pcg2_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg2_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg2_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg2_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg2_f_o_aer

        l = 56
        mastercompindx_pcg3_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg3_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg3_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg3_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg3_f_o_aer

        l = 57
        mastercompindx_pcg4_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg4_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg4_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg4_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg4_f_o_aer

        l = 58
        mastercompindx_pcg5_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg5_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg5_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg5_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg5_f_o_aer

        l = 59
        mastercompindx_pcg6_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg6_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg6_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg6_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg6_f_o_aer

        l = 60
        mastercompindx_pcg7_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg7_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg7_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg7_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg7_f_o_aer

        l = 61
        mastercompindx_pcg8_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg8_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg8_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg8_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg8_f_o_aer

        l = 62
        mastercompindx_pcg9_f_o_aer = l
        name_mastercomp_aer( l ) = 'pcg9_f_o'
        dens_mastercomp_aer( l ) =  dens_pcg9_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_pcg9_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_pcg9_f_o_aer

        l = 63
        mastercompindx_opcg1_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg1_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg1_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg1_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg1_f_c_aer

        l = 64
        mastercompindx_opcg2_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg2_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg2_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg2_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg2_f_c_aer

        l = 65
        mastercompindx_opcg3_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg3_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg3_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg3_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg3_f_c_aer

        l = 66
        mastercompindx_opcg4_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg4_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg4_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg4_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg4_f_c_aer

        l = 67
        mastercompindx_opcg5_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg5_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg5_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg5_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg5_f_c_aer

        l = 68
        mastercompindx_opcg6_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg6_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg6_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg6_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg6_f_c_aer

        l = 69
        mastercompindx_opcg7_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg7_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg7_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg7_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg7_f_c_aer

        l = 70
        mastercompindx_opcg8_f_c_aer = l
        name_mastercomp_aer( l ) = 'opcg8_f_c'
        dens_mastercomp_aer( l ) =  dens_opcg8_f_c_aer
        mw_mastercomp_aer(   l ) =    mw_opcg8_f_c_aer
        hygro_mastercomp_aer(l ) = hygro_opcg8_f_c_aer

        l = 71
        mastercompindx_opcg1_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg1_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg1_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg1_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg1_f_o_aer

        l = 72
        mastercompindx_opcg2_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg2_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg2_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg2_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg2_f_o_aer

        l = 73
        mastercompindx_opcg3_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg3_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg3_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg3_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg3_f_o_aer

        l = 74
        mastercompindx_opcg4_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg4_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg4_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg4_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg4_f_o_aer

        l = 75
        mastercompindx_opcg5_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg5_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg5_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg5_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg5_f_o_aer

        l = 76
        mastercompindx_opcg6_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg6_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg6_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg6_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg6_f_o_aer

        l = 77
        mastercompindx_opcg7_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg7_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg7_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg7_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg7_f_o_aer

        l = 78
        mastercompindx_opcg8_f_o_aer = l
        name_mastercomp_aer( l ) = 'opcg8_f_o'
        dens_mastercomp_aer( l ) =  dens_opcg8_f_o_aer
        mw_mastercomp_aer(   l ) =    mw_opcg8_f_o_aer
        hygro_mastercomp_aer(l ) = hygro_opcg8_f_o_aer

        l = 79
        mastercompindx_ant1_c_aer = l
        name_mastercomp_aer( l ) = 'ant1_c'
        dens_mastercomp_aer( l ) =  dens_ant1_c_aer
        mw_mastercomp_aer(   l ) =    mw_ant1_c_aer
        hygro_mastercomp_aer(l ) = hygro_ant1_c_aer

        l = 80
        mastercompindx_ant2_c_aer = l
        name_mastercomp_aer( l ) = 'ant2_c'
        dens_mastercomp_aer( l ) =  dens_ant2_c_aer
        mw_mastercomp_aer(   l ) =    mw_ant2_c_aer
        hygro_mastercomp_aer(l ) = hygro_ant2_c_aer

        l = 81
        mastercompindx_ant3_c_aer = l
        name_mastercomp_aer( l ) = 'ant3_c'
        dens_mastercomp_aer( l ) =  dens_ant3_c_aer
        mw_mastercomp_aer(   l ) =    mw_ant3_c_aer
        hygro_mastercomp_aer(l ) = hygro_ant3_c_aer

        l = 82
        mastercompindx_ant4_c_aer = l
        name_mastercomp_aer( l ) = 'ant4_c'
        dens_mastercomp_aer( l ) =  dens_ant4_c_aer
        mw_mastercomp_aer(   l ) =    mw_ant4_c_aer
        hygro_mastercomp_aer(l ) = hygro_ant4_c_aer

        l = 83
        mastercompindx_ant1_o_aer = l
        name_mastercomp_aer( l ) = 'ant1_o'
        dens_mastercomp_aer( l ) =  dens_ant1_o_aer
        mw_mastercomp_aer(   l ) =    mw_ant1_o_aer
        hygro_mastercomp_aer(l ) = hygro_ant1_o_aer

        l = 84
        mastercompindx_ant2_o_aer = l
        name_mastercomp_aer( l ) = 'ant2_o'
        dens_mastercomp_aer( l ) =  dens_ant2_o_aer
        mw_mastercomp_aer(   l ) =    mw_ant2_o_aer
        hygro_mastercomp_aer(l ) = hygro_ant2_o_aer

        l = 85
        mastercompindx_ant3_o_aer = l
        name_mastercomp_aer( l ) = 'ant3_o'
        dens_mastercomp_aer( l ) =  dens_ant3_o_aer
        mw_mastercomp_aer(   l ) =    mw_ant3_o_aer
        hygro_mastercomp_aer(l ) = hygro_ant3_o_aer

        l = 86
        mastercompindx_ant4_o_aer = l
        name_mastercomp_aer( l ) = 'ant4_o'
        dens_mastercomp_aer( l ) =  dens_ant4_o_aer
        mw_mastercomp_aer(   l ) =    mw_ant4_o_aer
        hygro_mastercomp_aer(l ) = hygro_ant4_o_aer


        l = 87
        mastercompindx_biog1_c_aer = l
        name_mastercomp_aer( l ) = 'biog1_c'
        dens_mastercomp_aer( l ) =  dens_biog1_c_aer
        mw_mastercomp_aer(   l ) =    mw_biog1_c_aer
        hygro_mastercomp_aer(l ) = hygro_biog1_c_aer

        l = 88
        mastercompindx_biog2_c_aer = l
        name_mastercomp_aer( l ) = 'biog2_c'
        dens_mastercomp_aer( l ) =  dens_biog2_c_aer
        mw_mastercomp_aer(   l ) =    mw_biog2_c_aer
        hygro_mastercomp_aer(l ) = hygro_biog2_c_aer

        l = 89
        mastercompindx_biog3_c_aer = l
        name_mastercomp_aer( l ) = 'biog3_c'
        dens_mastercomp_aer( l ) =  dens_biog3_c_aer
        mw_mastercomp_aer(   l ) =    mw_biog3_c_aer
        hygro_mastercomp_aer(l ) = hygro_biog3_c_aer

        l = 90
        mastercompindx_biog4_c_aer = l
        name_mastercomp_aer( l ) = 'biog4_c'
        dens_mastercomp_aer( l ) =  dens_biog4_c_aer
        mw_mastercomp_aer(   l ) =    mw_biog4_c_aer
        hygro_mastercomp_aer(l ) = hygro_biog4_c_aer

        l = 91
        mastercompindx_biog1_o_aer = l
        name_mastercomp_aer( l ) = 'biog1_o'
        dens_mastercomp_aer( l ) =  dens_biog1_o_aer
        mw_mastercomp_aer(   l ) =    mw_biog1_o_aer
        hygro_mastercomp_aer(l ) = hygro_biog1_o_aer

        l = 92
        mastercompindx_biog2_o_aer = l
        name_mastercomp_aer( l ) = 'biog2_o'
        dens_mastercomp_aer( l ) =  dens_biog2_o_aer
        mw_mastercomp_aer(   l ) =    mw_biog2_o_aer
        hygro_mastercomp_aer(l ) = hygro_biog2_o_aer

        l = 93
        mastercompindx_biog3_o_aer = l
        name_mastercomp_aer( l ) = 'biog3_o'
        dens_mastercomp_aer( l ) =  dens_biog3_o_aer
        mw_mastercomp_aer(   l ) =    mw_biog3_o_aer
        hygro_mastercomp_aer(l ) = hygro_biog3_o_aer

        l = 94
        mastercompindx_biog4_o_aer = l
        name_mastercomp_aer( l ) = 'biog4_o'
        dens_mastercomp_aer( l ) =  dens_biog4_o_aer
        mw_mastercomp_aer(   l ) =    mw_biog4_o_aer
        hygro_mastercomp_aer(l ) = hygro_biog4_o_aer

        l = 95
        mastercompindx_smpa_aer = l
        name_mastercomp_aer( l ) = 'smpa'
        dens_mastercomp_aer( l ) =  dens_smpa_aer
        mw_mastercomp_aer(   l ) =    mw_smpa_aer
        hygro_mastercomp_aer(l ) = hygro_smpa_aer

        l = 96
        mastercompindx_smpbb_aer = l
        name_mastercomp_aer( l ) = 'smpbb'
        dens_mastercomp_aer( l ) =  dens_smpbb_aer
        mw_mastercomp_aer(   l ) =    mw_smpbb_aer
        hygro_mastercomp_aer(l ) = hygro_smpbb_aer

        l = 97
        mastercompindx_glysoa_r1_aer = l
        name_mastercomp_aer( l ) = 'glysoa_r1'
        dens_mastercomp_aer( l ) =  dens_glysoa_r1_aer
        mw_mastercomp_aer(   l ) =    mw_glysoa_r1_aer
        hygro_mastercomp_aer(l ) = hygro_glysoa_r1_aer

        l = 98
        mastercompindx_glysoa_r2_aer = l
        name_mastercomp_aer( l ) = 'glysoa_r2'
        dens_mastercomp_aer( l ) =  dens_glysoa_r2_aer
        mw_mastercomp_aer(   l ) =    mw_glysoa_r2_aer
        hygro_mastercomp_aer(l ) = hygro_glysoa_r2_aer

        l = 99
        mastercompindx_glysoa_sfc_aer = l
        name_mastercomp_aer( l ) = 'glysoa_sfc'
        dens_mastercomp_aer( l ) =  dens_glysoa_sfc_aer
        mw_mastercomp_aer(   l ) =    mw_glysoa_sfc_aer
        hygro_mastercomp_aer(l ) = hygro_glysoa_sfc_aer

        l = 100
        mastercompindx_glysoa_nh4_aer = l
        name_mastercomp_aer( l ) = 'glysoa_nh4'
        dens_mastercomp_aer( l ) =  dens_glysoa_nh4_aer
        mw_mastercomp_aer(   l ) =    mw_glysoa_nh4_aer
        hygro_mastercomp_aer(l ) = hygro_glysoa_nh4_aer

        l = 101
        mastercompindx_glysoa_oh_aer = l
        name_mastercomp_aer( l ) = 'glysoa_oh'
        dens_mastercomp_aer( l ) =  dens_glysoa_oh_aer
        mw_mastercomp_aer(   l ) =    mw_glysoa_oh_aer
        hygro_mastercomp_aer(l ) = hygro_glysoa_oh_aer

        l = 102
        mastercompindx_asoaX_aer = l
        name_mastercomp_aer( l ) = 'asoaX'
        dens_mastercomp_aer( l ) =  dens_asoaX_aer
        mw_mastercomp_aer(   l ) =    mw_asoaX_aer
        hygro_mastercomp_aer(l ) = hygro_asoaX_aer

        l = 103
        mastercompindx_asoa1_aer = l
        name_mastercomp_aer( l ) = 'asoa1'
        dens_mastercomp_aer( l ) =  dens_asoa1_aer
        mw_mastercomp_aer(   l ) =    mw_asoa1_aer
        hygro_mastercomp_aer(l ) = hygro_asoa1_aer

        l = 104
        mastercompindx_asoa2_aer = l
        name_mastercomp_aer( l ) = 'asoa2'
        dens_mastercomp_aer( l ) =  dens_asoa2_aer
        mw_mastercomp_aer(   l ) =    mw_asoa2_aer
        hygro_mastercomp_aer(l ) = hygro_asoa2_aer

        l = 105
        mastercompindx_asoa3_aer = l
        name_mastercomp_aer( l ) = 'asoa3'
        dens_mastercomp_aer( l ) =  dens_asoa3_aer
        mw_mastercomp_aer(   l ) =    mw_asoa3_aer
        hygro_mastercomp_aer(l ) = hygro_asoa3_aer

        l = 106
        mastercompindx_asoa4_aer = l
        name_mastercomp_aer( l ) = 'asoa4'
        dens_mastercomp_aer( l ) =  dens_asoa4_aer
        mw_mastercomp_aer(   l ) =    mw_asoa4_aer
        hygro_mastercomp_aer(l ) = hygro_asoa4_aer

        l = 107
        mastercompindx_bsoaX_aer = l
        name_mastercomp_aer( l ) = 'bsoaX'
        dens_mastercomp_aer( l ) =  dens_bsoaX_aer
        mw_mastercomp_aer(   l ) =    mw_bsoaX_aer
        hygro_mastercomp_aer(l ) = hygro_bsoaX_aer

        l = 108
        mastercompindx_bsoa1_aer = l
        name_mastercomp_aer( l ) = 'bsoa1'
        dens_mastercomp_aer( l ) =  dens_bsoa1_aer
        mw_mastercomp_aer(   l ) =    mw_bsoa1_aer
        hygro_mastercomp_aer(l ) = hygro_bsoa1_aer

        l = 109
        mastercompindx_bsoa2_aer = l
        name_mastercomp_aer( l ) = 'bsoa2'
        dens_mastercomp_aer( l ) =  dens_bsoa2_aer
        mw_mastercomp_aer(   l ) =    mw_bsoa2_aer
        hygro_mastercomp_aer(l ) = hygro_bsoa2_aer

        l = 110
        mastercompindx_bsoa3_aer = l
        name_mastercomp_aer( l ) = 'bsoa3'
        dens_mastercomp_aer( l ) =  dens_bsoa3_aer
        mw_mastercomp_aer(   l ) =    mw_bsoa3_aer
        hygro_mastercomp_aer(l ) = hygro_bsoa3_aer

        l = 111
        mastercompindx_bsoa4_aer = l
        name_mastercomp_aer( l ) = 'bsoa4'
        dens_mastercomp_aer( l ) =  dens_bsoa4_aer
        mw_mastercomp_aer(   l ) =    mw_bsoa4_aer
        hygro_mastercomp_aer(l ) = hygro_bsoa4_aer

!
!   set section size arrays
!
        do itype = 1, ntype_aer
	    nhi = nsize_aer(itype)
	    dlo_sect(1,itype) = 3.90625e-6
	    dhi_sect(nhi,itype) = 10.0e-4

	    dum = log( dhi_sect(nhi,itype)/dlo_sect(1,itype) ) / nhi !replaced alog by log by Manish Shrivastava on 11/28/2011. alog denoted natural log in fortran 77. log(x) is natural log in fortran 90
	    do n = 2, nhi
		dlo_sect(n,itype) = dlo_sect(1,itype) * exp( (n-1)*dum )
		dhi_sect(n-1,itype) = dlo_sect(n,itype)
	    end do
	    do n = 1, nhi
		dcen_sect(n,itype) = sqrt( dlo_sect(n,itype)*dhi_sect(n,itype) )
		volumlo_sect(n,itype) = (pi/6.) * (dlo_sect(n,itype)**3)
		volumhi_sect(n,itype) = (pi/6.) * (dhi_sect(n,itype)**3)
		volumcen_sect(n,itype) = (pi/6.) * (dcen_sect(n,itype)**3)
		sigmag_aer(n,itype) = (dhi_sect(n,itype)/dlo_sect(n,itype))**0.289
	    end do
	end do

!
!   set pointers to wrf chem-array species
!
	call init_data_mosaic_ptr( id,is_aerosol )

!
!   csuesat initialization
!
	call init_csuesat

!
!   move_sect initialization (and testing)
!
!	subr move_sections( iflag, iclm, jclm, k, m )
	call move_sections(    -1,    1,    1, 1, 1 )

	call test_move_sections( 1,   1,    1, 1, 1 )
    

        idum = mod( max( config_flags%mosaic_aerchem_optaa, 0 ), 10 )   ! use mosaic2 when 1s digit = 2
        if ( idum == 2 ) then
            call init_data_mosaic2_asect( id, config_flags, is_aerosol, vbs_nbin )
        end if


	end subroutine init_data_mosaic_asect


!-----------------------------------------------------------------------
	subroutine init_data_mosaic_ptr( id,is_aerosol )

	use module_configure
	use module_state_description, only:  param_first_scalar,num_chem
	use module_data_mosaic_asect
	use module_data_mosaic_other, only:   &
		kh2so4, khno3, khcl, knh3, ko3, kh2o, ktemp,   &
		kso2, kh2o2, khcho, koh, kho2,   &
		kno3, kno, kno2, khono, kpan,   &
                lmaxd, l2maxd, ltot, ltot2, lunout, lunerr, &
                name,kpcg1_b_c,kpcg2_b_c,kpcg3_b_c,kpcg4_b_c, &
                kpcg5_b_c,kpcg6_b_c,kpcg7_b_c,kpcg8_b_c, &
                kpcg9_b_c,kpcg1_b_o,kpcg2_b_o,kpcg3_b_o, &
                kpcg4_b_o,kpcg5_b_o,kpcg6_b_o,kpcg7_b_o, &
                kpcg8_b_o,kpcg9_b_o,kopcg1_b_c,kopcg2_b_c,&
                kopcg3_b_c, kopcg4_b_c,kopcg5_b_c,kopcg6_b_c,&
                kopcg7_b_c,kopcg8_b_c,kopcg1_b_o,kopcg2_b_o,&
                kopcg3_b_o,kopcg4_b_o,kopcg5_b_o,kopcg6_b_o,&
                kopcg7_b_o,kopcg8_b_o,&
                kpcg1_f_c,kpcg2_f_c,kpcg3_f_c,kpcg4_f_c, &
                kpcg5_f_c,kpcg6_f_c,kpcg7_f_c,kpcg8_f_c, &
                kpcg9_f_c,kpcg1_f_o,kpcg2_f_o,kpcg3_f_o, &
                kpcg4_f_o,kpcg5_f_o,kpcg6_f_o,kpcg7_f_o, &
                kpcg8_f_o,kpcg9_f_o,kopcg1_f_c,kopcg2_f_c,&
                kopcg3_f_c, kopcg4_f_c,kopcg5_f_c,kopcg6_f_c,&
                kopcg7_f_c,kopcg8_f_c,kopcg1_f_o,kopcg2_f_o,&
                kopcg3_f_o,kopcg4_f_o,kopcg5_f_o,kopcg6_f_o,&
                kopcg7_f_o,kopcg8_f_o,                      &
                ksmpa,ksmpbb,                      &
                kgly, &
                kant1_c,kant2_c,kant3_c,kant4_c,kant1_o,kant2_o, &
                kant3_o,kant4_o,                            &
                kbiog1_c,kbiog2_c,kbiog3_c,kbiog4_c,kbiog1_o,kbiog2_o, &
                kbiog3_o,kbiog4_o, &
!                kn2o5, kclno2                            
                kn2o5, kclno2, &                            
                kasoaX,kasoa1,kasoa2,kasoa3,kasoa4,&
                kbsoaX,kbsoa1,kbsoa2,kbsoa3,kbsoa4

	use module_peg_util, only:  peg_error_fatal, peg_message
	use module_mosaic_wetscav, only: initwet
    use module_scalar_tables, only:  chem_dname_table
	implicit none

!   subr arguments
        logical, intent(out) :: is_aerosol(num_chem)
        integer, intent(in) :: id
!   local variables
	integer l, ll, n, p1st
	integer iaddto_ncomp, iaddto_ncomp_plustracer
	integer l_mastercomp, lptr_dum
	integer mcindx_dum
	integer isize, itype, iphase
	integer nphasetxt, nsizetxt, nspectxt, ntypetxt
	integer ncomp_dum(maxd_asize,maxd_aphase)
	integer ncomp_plustracer_dum(maxd_asize,maxd_aphase)

	integer y_so4, y_no3, y_cl, y_msa, y_co3, y_nh4, y_na,   &
		y_ca, y_oin, y_oc, y_bc, y_hysw, y_water, &
                y_num, &
      y_pcg1_b_c,y_pcg2_b_c,y_pcg3_b_c,y_pcg4_b_c, &
      y_pcg5_b_c,y_pcg6_b_c,y_pcg7_b_c,y_pcg8_b_c, &
      y_pcg9_b_c,y_pcg1_b_o,y_pcg2_b_o,y_pcg3_b_o, &
      y_pcg4_b_o,y_pcg5_b_o,y_pcg6_b_o,y_pcg7_b_o, &
      y_pcg8_b_o,y_pcg9_b_o,y_opcg1_b_c,y_opcg2_b_c,&
      y_opcg3_b_c, y_opcg4_b_c,y_opcg5_b_c,y_opcg6_b_c,&
      y_opcg7_b_c,y_opcg8_b_c,y_opcg1_b_o,y_opcg2_b_o,&
      y_opcg3_b_o,y_opcg4_b_o,y_opcg5_b_o,y_opcg6_b_o,&
      y_opcg7_b_o,y_opcg8_b_o,&
      y_pcg1_f_c,y_pcg2_f_c,y_pcg3_f_c,y_pcg4_f_c, &
      y_pcg5_f_c,y_pcg6_f_c,y_pcg7_f_c,y_pcg8_f_c, &
      y_pcg9_f_c,y_pcg1_f_o,y_pcg2_f_o,y_pcg3_f_o, &
      y_pcg4_f_o,y_pcg5_f_o,y_pcg6_f_o,y_pcg7_f_o, &
      y_pcg8_f_o,y_pcg9_f_o,y_opcg1_f_c,y_opcg2_f_c,&
      y_opcg3_f_c, y_opcg4_f_c,y_opcg5_f_c,y_opcg6_f_c,&
      y_opcg7_f_c,y_opcg8_f_c,y_opcg1_f_o,y_opcg2_f_o,&
      y_opcg3_f_o,y_opcg4_f_o,y_opcg5_f_o,y_opcg6_f_o,&
      y_opcg7_f_o,y_opcg8_f_o, &
      y_smpa,y_smpbb, &
      y_glysoa_r1, y_glysoa_r2, y_glysoa_sfc, y_glysoa_nh4, y_glysoa_oh, &
      y_ant1_c,y_ant2_c,y_ant3_c,y_ant4_c, &
      y_ant1_o,y_ant2_o,y_ant3_o,y_ant4_o, &
      y_biog1_c,y_biog2_c,y_biog3_c,y_biog4_c, &
      y_biog1_o,y_biog2_o,y_biog3_o,y_biog4_o, &
      y_asoaX, y_asoa1, y_asoa2, y_asoa3, y_asoa4, &
      y_bsoaX, y_bsoa1, y_bsoa2, y_bsoa3, y_bsoa4

	integer y_cw_so4, y_cw_no3, y_cw_cl, y_cw_msa, y_cw_co3,   &
		y_cw_nh4, y_cw_na,   &
		y_cw_ca, y_cw_oin, y_cw_oc, y_cw_bc, y_cw_num

	character*200 msg
	character*20 phasetxt, sizetxt, spectxt, typetxt


	p1st = param_first_scalar
!
!   set up pointers to aerosol species in the wrf-chem "chem" array
!   note:  lptr=1 points to the first chem species which is "unused"
!
	itype=1
	lptr_so4_aer(:,itype,:)      = 1
	lptr_no3_aer(:,itype,:)      = 1
	lptr_cl_aer(:,itype,:)       = 1
	lptr_msa_aer(:,itype,:)      = 1
	lptr_co3_aer(:,itype,:)      = 1
	lptr_nh4_aer(:,itype,:)      = 1
	lptr_na_aer(:,itype,:)       = 1
	lptr_ca_aer(:,itype,:)       = 1
	lptr_oin_aer(:,itype,:)      = 1
	lptr_oc_aer(:,itype,:)       = 1
	lptr_bc_aer(:,itype,:)       = 1
	hyswptr_aer(:,itype)    = 1
	waterptr_aer(:,itype)        = 1
	numptr_aer(:,itype,:)        = 1
        lptr_pcg1_b_c_aer(:,itype,:)      = 1
        lptr_pcg2_b_c_aer(:,itype,:)      = 1
        lptr_pcg3_b_c_aer(:,itype,:)      = 1
        lptr_pcg4_b_c_aer(:,itype,:)      = 1
        lptr_pcg5_b_c_aer(:,itype,:)      = 1
        lptr_pcg6_b_c_aer(:,itype,:)      = 1
        lptr_pcg7_b_c_aer(:,itype,:)      = 1
        lptr_pcg8_b_c_aer(:,itype,:)      = 1
        lptr_pcg9_b_c_aer(:,itype,:)      = 1
        lptr_pcg1_b_o_aer(:,itype,:)      = 1
        lptr_pcg2_b_o_aer(:,itype,:)      = 1
        lptr_pcg3_b_o_aer(:,itype,:)      = 1
        lptr_pcg4_b_o_aer(:,itype,:)      = 1
        lptr_pcg5_b_o_aer(:,itype,:)      = 1
        lptr_pcg6_b_o_aer(:,itype,:)      = 1
        lptr_pcg7_b_o_aer(:,itype,:)      = 1
        lptr_pcg8_b_o_aer(:,itype,:)      = 1
        lptr_pcg9_b_o_aer(:,itype,:)      = 1
        lptr_opcg1_b_c_aer(:,itype,:)      = 1
        lptr_opcg2_b_c_aer(:,itype,:)      = 1
        lptr_opcg3_b_c_aer(:,itype,:)      = 1
        lptr_opcg4_b_c_aer(:,itype,:)      = 1
        lptr_opcg5_b_c_aer(:,itype,:)      = 1
        lptr_opcg6_b_c_aer(:,itype,:)      = 1
        lptr_opcg7_b_c_aer(:,itype,:)      = 1
        lptr_opcg8_b_c_aer(:,itype,:)      = 1
        lptr_opcg1_b_o_aer(:,itype,:)      = 1
        lptr_opcg2_b_o_aer(:,itype,:)      = 1
        lptr_opcg3_b_o_aer(:,itype,:)      = 1
        lptr_opcg4_b_o_aer(:,itype,:)      = 1
        lptr_opcg5_b_o_aer(:,itype,:)      = 1
        lptr_opcg6_b_o_aer(:,itype,:)      = 1
        lptr_opcg7_b_o_aer(:,itype,:)      = 1
        lptr_opcg8_b_o_aer(:,itype,:)      = 1
        lptr_pcg1_f_c_aer(:,itype,:)      = 1
        lptr_pcg2_f_c_aer(:,itype,:)      = 1
        lptr_pcg3_f_c_aer(:,itype,:)      = 1
        lptr_pcg4_f_c_aer(:,itype,:)      = 1
        lptr_pcg5_f_c_aer(:,itype,:)      = 1
        lptr_pcg6_f_c_aer(:,itype,:)      = 1
        lptr_pcg7_f_c_aer(:,itype,:)      = 1
        lptr_pcg8_f_c_aer(:,itype,:)      = 1
        lptr_pcg9_f_c_aer(:,itype,:)      = 1
        lptr_pcg1_f_o_aer(:,itype,:)      = 1
        lptr_pcg2_f_o_aer(:,itype,:)      = 1
        lptr_pcg3_f_o_aer(:,itype,:)      = 1
        lptr_pcg4_f_o_aer(:,itype,:)      = 1
        lptr_pcg5_f_o_aer(:,itype,:)      = 1
        lptr_pcg6_f_o_aer(:,itype,:)      = 1
        lptr_pcg7_f_o_aer(:,itype,:)      = 1
        lptr_pcg8_f_o_aer(:,itype,:)      = 1
        lptr_pcg9_f_o_aer(:,itype,:)      = 1
        lptr_opcg1_f_c_aer(:,itype,:)      = 1
        lptr_opcg2_f_c_aer(:,itype,:)      = 1
        lptr_opcg3_f_c_aer(:,itype,:)      = 1
        lptr_opcg4_f_c_aer(:,itype,:)      = 1
        lptr_opcg5_f_c_aer(:,itype,:)      = 1
        lptr_opcg6_f_c_aer(:,itype,:)      = 1
        lptr_opcg7_f_c_aer(:,itype,:)      = 1
        lptr_opcg8_f_c_aer(:,itype,:)      = 1
        lptr_opcg1_f_o_aer(:,itype,:)      = 1
        lptr_opcg2_f_o_aer(:,itype,:)      = 1
        lptr_opcg3_f_o_aer(:,itype,:)      = 1
        lptr_opcg4_f_o_aer(:,itype,:)      = 1
        lptr_opcg5_f_o_aer(:,itype,:)      = 1
        lptr_opcg6_f_o_aer(:,itype,:)      = 1
        lptr_opcg7_f_o_aer(:,itype,:)      = 1
        lptr_opcg8_f_o_aer(:,itype,:)      = 1
        lptr_smpa_aer(:,itype,:)      = 1
        lptr_smpbb_aer(:,itype,:)      = 1
        lptr_glysoa_r1_aer(:,itype,:)  = 1
        lptr_glysoa_r2_aer(:,itype,:)  = 1
        lptr_glysoa_sfc_aer(:,itype,:) = 1
        lptr_glysoa_nh4_aer(:,itype,:) = 1
        lptr_glysoa_oh_aer(:,itype,:)  = 1
        lptr_ant1_c_aer(:,itype,:)      = 1
        lptr_ant2_c_aer(:,itype,:)      = 1
        lptr_ant3_c_aer(:,itype,:)      = 1
        lptr_ant4_c_aer(:,itype,:)      = 1
        lptr_ant1_o_aer(:,itype,:)      = 1
        lptr_ant2_o_aer(:,itype,:)      = 1
        lptr_ant3_o_aer(:,itype,:)      = 1
        lptr_ant4_o_aer(:,itype,:)      = 1
        lptr_biog1_c_aer(:,itype,:)      = 1
        lptr_biog2_c_aer(:,itype,:)      = 1
        lptr_biog3_c_aer(:,itype,:)      = 1
        lptr_biog4_c_aer(:,itype,:)      = 1
        lptr_biog1_o_aer(:,itype,:)      = 1
        lptr_biog2_o_aer(:,itype,:)      = 1
        lptr_biog3_o_aer(:,itype,:)      = 1
        lptr_biog4_o_aer(:,itype,:)      = 1
        lptr_asoaX_aer(:,itype,:)        = 1
        lptr_asoa1_aer(:,itype,:)        = 1
        lptr_asoa2_aer(:,itype,:)        = 1
        lptr_asoa3_aer(:,itype,:)        = 1
        lptr_asoa4_aer(:,itype,:)        = 1
        lptr_bsoaX_aer(:,itype,:)        = 1
        lptr_bsoa1_aer(:,itype,:)        = 1
        lptr_bsoa2_aer(:,itype,:)        = 1
        lptr_bsoa3_aer(:,itype,:)        = 1
        lptr_bsoa4_aer(:,itype,:)        = 1


	if (nsize_aer(itype) .ge. 1) then
            
	if (p_so4_a01 .ge. p1st) lptr_so4_aer(01,itype,ai_phase) = p_so4_a01
	if (p_no3_a01 .ge. p1st) lptr_no3_aer(01,itype,ai_phase) = p_no3_a01
	if (p_cl_a01 .ge. p1st) lptr_cl_aer(01,itype,ai_phase)  = p_cl_a01
	if (p_msa_a01 .ge. p1st) lptr_msa_aer(01,itype,ai_phase) = p_msa_a01
	if (p_co3_a01 .ge. p1st) lptr_co3_aer(01,itype,ai_phase) = p_co3_a01
	if (p_nh4_a01 .ge. p1st) lptr_nh4_aer(01,itype,ai_phase) = p_nh4_a01
	if (p_na_a01 .ge. p1st) lptr_na_aer(01,itype,ai_phase)  = p_na_a01
	if (p_ca_a01 .ge. p1st) lptr_ca_aer(01,itype,ai_phase)  = p_ca_a01
	if (p_oin_a01 .ge. p1st) lptr_oin_aer(01,itype,ai_phase) = p_oin_a01
	if (p_oc_a01 .ge. p1st) lptr_oc_aer(01,itype,ai_phase)  = p_oc_a01
	if (p_bc_a01 .ge. p1st) lptr_bc_aer(01,itype,ai_phase)  = p_bc_a01
	if (p_hysw_a01 .ge. p1st) hyswptr_aer(01,itype)         = p_hysw_a01
	if (p_water_a01 .ge. p1st) waterptr_aer(01,itype)       = p_water_a01
        if (p_pcg1_b_c_a01 .ge. p1st) lptr_pcg1_b_c_aer(01,itype,ai_phase) = p_pcg1_b_c_a01
        if (p_pcg2_b_c_a01 .ge. p1st) lptr_pcg2_b_c_aer(01,itype,ai_phase)  = p_pcg2_b_c_a01
        if (p_pcg3_b_c_a01 .ge. p1st) lptr_pcg3_b_c_aer(01,itype,ai_phase)  = p_pcg3_b_c_a01
        if (p_pcg4_b_c_a01 .ge. p1st) lptr_pcg4_b_c_aer(01,itype,ai_phase)  = p_pcg4_b_c_a01
        if (p_pcg5_b_c_a01 .ge. p1st) lptr_pcg5_b_c_aer(01,itype,ai_phase)  = p_pcg5_b_c_a01
        if (p_pcg6_b_c_a01 .ge. p1st) lptr_pcg6_b_c_aer(01,itype,ai_phase)  = p_pcg6_b_c_a01
        if (p_pcg7_b_c_a01 .ge. p1st) lptr_pcg7_b_c_aer(01,itype,ai_phase)  = p_pcg7_b_c_a01
        if (p_pcg8_b_c_a01 .ge. p1st) lptr_pcg8_b_c_aer(01,itype,ai_phase)  = p_pcg8_b_c_a01
        if (p_pcg9_b_c_a01 .ge. p1st) lptr_pcg9_b_c_aer(01,itype,ai_phase)  = p_pcg9_b_c_a01
        if (p_pcg1_b_o_a01 .ge. p1st) lptr_pcg1_b_o_aer(01,itype,ai_phase) = p_pcg1_b_o_a01
        if (p_pcg2_b_o_a01 .ge. p1st) lptr_pcg2_b_o_aer(01,itype,ai_phase)  = p_pcg2_b_o_a01
        if (p_pcg3_b_o_a01 .ge. p1st) lptr_pcg3_b_o_aer(01,itype,ai_phase)  = p_pcg3_b_o_a01
        if (p_pcg4_b_o_a01 .ge. p1st) lptr_pcg4_b_o_aer(01,itype,ai_phase)  = p_pcg4_b_o_a01
        if (p_pcg5_b_o_a01 .ge. p1st) lptr_pcg5_b_o_aer(01,itype,ai_phase)  = p_pcg5_b_o_a01
        if (p_pcg6_b_o_a01 .ge. p1st) lptr_pcg6_b_o_aer(01,itype,ai_phase)  = p_pcg6_b_o_a01
        if (p_pcg7_b_o_a01 .ge. p1st) lptr_pcg7_b_o_aer(01,itype,ai_phase)  = p_pcg7_b_o_a01
        if (p_pcg8_b_o_a01 .ge. p1st) lptr_pcg8_b_o_aer(01,itype,ai_phase)  = p_pcg8_b_o_a01
        if (p_pcg9_b_o_a01 .ge. p1st) lptr_pcg9_b_o_aer(01,itype,ai_phase)  = p_pcg9_b_o_a01
        if (p_opcg1_b_c_a01 .ge. p1st) lptr_opcg1_b_c_aer(01,itype,ai_phase) = p_opcg1_b_c_a01
        if (p_opcg2_b_c_a01 .ge. p1st) lptr_opcg2_b_c_aer(01,itype,ai_phase)  = p_opcg2_b_c_a01
        if (p_opcg3_b_c_a01 .ge. p1st) lptr_opcg3_b_c_aer(01,itype,ai_phase)  = p_opcg3_b_c_a01
        if (p_opcg4_b_c_a01 .ge. p1st) lptr_opcg4_b_c_aer(01,itype,ai_phase)  = p_opcg4_b_c_a01
        if (p_opcg5_b_c_a01 .ge. p1st) lptr_opcg5_b_c_aer(01,itype,ai_phase)  = p_opcg5_b_c_a01
        if (p_opcg6_b_c_a01 .ge. p1st) lptr_opcg6_b_c_aer(01,itype,ai_phase)  = p_opcg6_b_c_a01
        if (p_opcg7_b_c_a01 .ge. p1st) lptr_opcg7_b_c_aer(01,itype,ai_phase)  = p_opcg7_b_c_a01
        if (p_opcg8_b_c_a01 .ge. p1st) lptr_opcg8_b_c_aer(01,itype,ai_phase)  = p_opcg8_b_c_a01
        if (p_opcg1_b_o_a01 .ge. p1st) lptr_opcg1_b_o_aer(01,itype,ai_phase) = p_opcg1_b_o_a01
        if (p_opcg2_b_o_a01 .ge. p1st) lptr_opcg2_b_o_aer(01,itype,ai_phase)  = p_opcg2_b_o_a01
        if (p_opcg3_b_o_a01 .ge. p1st) lptr_opcg3_b_o_aer(01,itype,ai_phase)  = p_opcg3_b_o_a01
        if (p_opcg4_b_o_a01 .ge. p1st) lptr_opcg4_b_o_aer(01,itype,ai_phase)  = p_opcg4_b_o_a01
        if (p_opcg5_b_o_a01 .ge. p1st) lptr_opcg5_b_o_aer(01,itype,ai_phase)  = p_opcg5_b_o_a01
        if (p_opcg6_b_o_a01 .ge. p1st) lptr_opcg6_b_o_aer(01,itype,ai_phase)  = p_opcg6_b_o_a01
        if (p_opcg7_b_o_a01 .ge. p1st) lptr_opcg7_b_o_aer(01,itype,ai_phase)  = p_opcg7_b_o_a01
        if (p_opcg8_b_o_a01 .ge. p1st) lptr_opcg8_b_o_aer(01,itype,ai_phase)  = p_opcg8_b_o_a01
        if (p_pcg1_f_c_a01 .ge. p1st) lptr_pcg1_f_c_aer(01,itype,ai_phase) = p_pcg1_f_c_a01
        if (p_pcg2_f_c_a01 .ge. p1st) lptr_pcg2_f_c_aer(01,itype,ai_phase)  = p_pcg2_f_c_a01
        if (p_pcg3_f_c_a01 .ge. p1st) lptr_pcg3_f_c_aer(01,itype,ai_phase)  = p_pcg3_f_c_a01
        if (p_pcg4_f_c_a01 .ge. p1st) lptr_pcg4_f_c_aer(01,itype,ai_phase)  = p_pcg4_f_c_a01
        if (p_pcg5_f_c_a01 .ge. p1st) lptr_pcg5_f_c_aer(01,itype,ai_phase)  = p_pcg5_f_c_a01
        if (p_pcg6_f_c_a01 .ge. p1st) lptr_pcg6_f_c_aer(01,itype,ai_phase)  = p_pcg6_f_c_a01
        if (p_pcg7_f_c_a01 .ge. p1st) lptr_pcg7_f_c_aer(01,itype,ai_phase)  = p_pcg7_f_c_a01
        if (p_pcg8_f_c_a01 .ge. p1st) lptr_pcg8_f_c_aer(01,itype,ai_phase)  = p_pcg8_f_c_a01
        if (p_pcg9_f_c_a01 .ge. p1st) lptr_pcg9_f_c_aer(01,itype,ai_phase)  = p_pcg9_f_c_a01
        if (p_pcg1_f_o_a01 .ge. p1st) lptr_pcg1_f_o_aer(01,itype,ai_phase) = p_pcg1_f_o_a01
        if (p_pcg2_f_o_a01 .ge. p1st) lptr_pcg2_f_o_aer(01,itype,ai_phase)  = p_pcg2_f_o_a01
        if (p_pcg3_f_o_a01 .ge. p1st) lptr_pcg3_f_o_aer(01,itype,ai_phase)  = p_pcg3_f_o_a01
        if (p_pcg4_f_o_a01 .ge. p1st) lptr_pcg4_f_o_aer(01,itype,ai_phase)  = p_pcg4_f_o_a01
        if (p_pcg5_f_o_a01 .ge. p1st) lptr_pcg5_f_o_aer(01,itype,ai_phase)  = p_pcg5_f_o_a01
        if (p_pcg6_f_o_a01 .ge. p1st) lptr_pcg6_f_o_aer(01,itype,ai_phase)  = p_pcg6_f_o_a01
        if (p_pcg7_f_o_a01 .ge. p1st) lptr_pcg7_f_o_aer(01,itype,ai_phase)  = p_pcg7_f_o_a01
        if (p_pcg8_f_o_a01 .ge. p1st) lptr_pcg8_f_o_aer(01,itype,ai_phase)  = p_pcg8_f_o_a01
        if (p_pcg9_f_o_a01 .ge. p1st) lptr_pcg9_f_o_aer(01,itype,ai_phase)  = p_pcg9_f_o_a01
        if (p_opcg1_f_c_a01 .ge. p1st) lptr_opcg1_f_c_aer(01,itype,ai_phase) = p_opcg1_f_c_a01
        if (p_opcg2_f_c_a01 .ge. p1st) lptr_opcg2_f_c_aer(01,itype,ai_phase)  = p_opcg2_f_c_a01
        if (p_opcg3_f_c_a01 .ge. p1st) lptr_opcg3_f_c_aer(01,itype,ai_phase)  = p_opcg3_f_c_a01
        if (p_opcg4_f_c_a01 .ge. p1st) lptr_opcg4_f_c_aer(01,itype,ai_phase)  = p_opcg4_f_c_a01
        if (p_opcg5_f_c_a01 .ge. p1st) lptr_opcg5_f_c_aer(01,itype,ai_phase)  = p_opcg5_f_c_a01
        if (p_opcg6_f_c_a01 .ge. p1st) lptr_opcg6_f_c_aer(01,itype,ai_phase)  = p_opcg6_f_c_a01
        if (p_opcg7_f_c_a01 .ge. p1st) lptr_opcg7_f_c_aer(01,itype,ai_phase)  = p_opcg7_f_c_a01
        if (p_opcg8_f_c_a01 .ge. p1st) lptr_opcg8_f_c_aer(01,itype,ai_phase)  = p_opcg8_f_c_a01
        if (p_opcg1_f_o_a01 .ge. p1st) lptr_opcg1_f_o_aer(01,itype,ai_phase) = p_opcg1_f_o_a01
        if (p_opcg2_f_o_a01 .ge. p1st) lptr_opcg2_f_o_aer(01,itype,ai_phase)  = p_opcg2_f_o_a01
        if (p_opcg3_f_o_a01 .ge. p1st) lptr_opcg3_f_o_aer(01,itype,ai_phase)  = p_opcg3_f_o_a01
        if (p_opcg4_f_o_a01 .ge. p1st) lptr_opcg4_f_o_aer(01,itype,ai_phase)  = p_opcg4_f_o_a01
        if (p_opcg5_f_o_a01 .ge. p1st) lptr_opcg5_f_o_aer(01,itype,ai_phase)  = p_opcg5_f_o_a01
        if (p_opcg6_f_o_a01 .ge. p1st) lptr_opcg6_f_o_aer(01,itype,ai_phase)  = p_opcg6_f_o_a01
        if (p_opcg7_f_o_a01 .ge. p1st) lptr_opcg7_f_o_aer(01,itype,ai_phase)  = p_opcg7_f_o_a01
        if (p_opcg8_f_o_a01 .ge. p1st) lptr_opcg8_f_o_aer(01,itype,ai_phase)  = p_opcg8_f_o_a01
        if (p_smpa_a01 .ge. p1st) lptr_smpa_aer(01,itype,ai_phase)  = p_smpa_a01
        if (p_smpbb_a01 .ge. p1st) lptr_smpbb_aer(01,itype,ai_phase)  = p_smpbb_a01
        if (p_glysoa_r1_a01 .ge. p1st) lptr_glysoa_r1_aer (01,itype,ai_phase) = p_glysoa_r1_a01
        if (p_glysoa_r2_a01 .ge. p1st) lptr_glysoa_r2_aer (01,itype,ai_phase) = p_glysoa_r2_a01
        if (p_glysoa_sfc_a01 .ge. p1st) lptr_glysoa_sfc_aer (01,itype,ai_phase) = p_glysoa_sfc_a01
        if (p_glysoa_nh4_a01 .ge. p1st) lptr_glysoa_nh4_aer (01,itype,ai_phase) = p_glysoa_nh4_a01
        if (p_glysoa_oh_a01 .ge. p1st) lptr_glysoa_oh_aer (01,itype,ai_phase) = p_glysoa_oh_a01
        if (p_ant1_c_a01 .ge. p1st) lptr_ant1_c_aer(01,itype,ai_phase)  = p_ant1_c_a01
        if (p_ant2_c_a01 .ge. p1st) lptr_ant2_c_aer(01,itype,ai_phase)  = p_ant2_c_a01
        if (p_ant3_c_a01 .ge. p1st) lptr_ant3_c_aer(01,itype,ai_phase)  = p_ant3_c_a01
        if (p_ant4_c_a01 .ge. p1st) lptr_ant4_c_aer(01,itype,ai_phase)  = p_ant4_c_a01
        if (p_ant1_o_a01 .ge. p1st) lptr_ant1_o_aer(01,itype,ai_phase)  = p_ant1_o_a01
        if (p_ant2_o_a01 .ge. p1st) lptr_ant2_o_aer(01,itype,ai_phase)  = p_ant2_o_a01
        if (p_ant3_o_a01 .ge. p1st) lptr_ant3_o_aer(01,itype,ai_phase)  = p_ant3_o_a01
        if (p_ant4_o_a01 .ge. p1st) lptr_ant4_o_aer(01,itype,ai_phase)  = p_ant4_o_a01
        if (p_biog1_c_a01 .ge. p1st) lptr_biog1_c_aer(01,itype,ai_phase)  = p_biog1_c_a01
        if (p_biog2_c_a01 .ge. p1st) lptr_biog2_c_aer(01,itype,ai_phase)  = p_biog2_c_a01
        if (p_biog3_c_a01 .ge. p1st) lptr_biog3_c_aer(01,itype,ai_phase)  = p_biog3_c_a01
        if (p_biog4_c_a01 .ge. p1st) lptr_biog4_c_aer(01,itype,ai_phase)  = p_biog4_c_a01
        if (p_biog1_o_a01 .ge. p1st) lptr_biog1_o_aer(01,itype,ai_phase)  = p_biog1_o_a01
        if (p_biog2_o_a01 .ge. p1st) lptr_biog2_o_aer(01,itype,ai_phase)  = p_biog2_o_a01
        if (p_biog3_o_a01 .ge. p1st) lptr_biog3_o_aer(01,itype,ai_phase)  = p_biog3_o_a01
        if (p_biog4_o_a01 .ge. p1st) lptr_biog4_o_aer(01,itype,ai_phase)  = p_biog4_o_a01
        if (p_asoaX_a01 .ge. p1st) lptr_asoaX_aer(01,itype,ai_phase)  = p_asoaX_a01
        if (p_asoa1_a01 .ge. p1st) lptr_asoa1_aer(01,itype,ai_phase)  = p_asoa1_a01
        if (p_asoa2_a01 .ge. p1st) lptr_asoa2_aer(01,itype,ai_phase)  = p_asoa2_a01
        if (p_asoa3_a01 .ge. p1st) lptr_asoa3_aer(01,itype,ai_phase)  = p_asoa3_a01
        if (p_asoa4_a01 .ge. p1st) lptr_asoa4_aer(01,itype,ai_phase)  = p_asoa4_a01
        if (p_bsoaX_a01 .ge. p1st) lptr_bsoaX_aer(01,itype,ai_phase)  = p_bsoaX_a01
        if (p_bsoa1_a01 .ge. p1st) lptr_bsoa1_aer(01,itype,ai_phase)  = p_bsoa1_a01
        if (p_bsoa2_a01 .ge. p1st) lptr_bsoa2_aer(01,itype,ai_phase)  = p_bsoa2_a01
        if (p_bsoa3_a01 .ge. p1st) lptr_bsoa3_aer(01,itype,ai_phase)  = p_bsoa3_a01
        if (p_bsoa4_a01 .ge. p1st) lptr_bsoa4_aer(01,itype,ai_phase)  = p_bsoa4_a01
	if (p_num_a01 .ge. p1st)  numptr_aer(01,itype,ai_phase)        = p_num_a01
	end if

	if (nsize_aer(itype) .ge. 2) then
        if (p_so4_a02 .ge. p1st) lptr_so4_aer(02,itype,ai_phase) = p_so4_a02
        if (p_no3_a02 .ge. p1st) lptr_no3_aer(02,itype,ai_phase) = p_no3_a02
        if (p_cl_a02 .ge. p1st) lptr_cl_aer(02,itype,ai_phase)  = p_cl_a02
        if (p_msa_a02 .ge. p1st) lptr_msa_aer(02,itype,ai_phase) = p_msa_a02
        if (p_co3_a02 .ge. p1st) lptr_co3_aer(02,itype,ai_phase) = p_co3_a02
        if (p_nh4_a02 .ge. p1st) lptr_nh4_aer(02,itype,ai_phase) = p_nh4_a02
        if (p_na_a02 .ge. p1st) lptr_na_aer(02,itype,ai_phase)  = p_na_a02
        if (p_ca_a02 .ge. p1st) lptr_ca_aer(02,itype,ai_phase)  = p_ca_a02
        if (p_oin_a02 .ge. p1st) lptr_oin_aer(02,itype,ai_phase) = p_oin_a02
        if (p_oc_a02 .ge. p1st) lptr_oc_aer(02,itype,ai_phase)  = p_oc_a02
        if (p_bc_a02 .ge. p1st) lptr_bc_aer(02,itype,ai_phase)  = p_bc_a02
        if (p_hysw_a02 .ge. p1st) hyswptr_aer(02,itype)         = p_hysw_a02
        if (p_water_a02 .ge. p1st) waterptr_aer(02,itype)       = p_water_a02
        if (p_pcg1_b_c_a02 .ge. p1st) lptr_pcg1_b_c_aer(02,itype,ai_phase) = p_pcg1_b_c_a02
        if (p_pcg2_b_c_a02 .ge. p1st) lptr_pcg2_b_c_aer(02,itype,ai_phase)  = p_pcg2_b_c_a02
        if (p_pcg3_b_c_a02 .ge. p1st) lptr_pcg3_b_c_aer(02,itype,ai_phase)  = p_pcg3_b_c_a02
        if (p_pcg4_b_c_a02 .ge. p1st) lptr_pcg4_b_c_aer(02,itype,ai_phase)  = p_pcg4_b_c_a02
        if (p_pcg5_b_c_a02 .ge. p1st) lptr_pcg5_b_c_aer(02,itype,ai_phase)  = p_pcg5_b_c_a02
        if (p_pcg6_b_c_a02 .ge. p1st) lptr_pcg6_b_c_aer(02,itype,ai_phase)  = p_pcg6_b_c_a02
        if (p_pcg7_b_c_a02 .ge. p1st) lptr_pcg7_b_c_aer(02,itype,ai_phase)  = p_pcg7_b_c_a02
        if (p_pcg8_b_c_a02 .ge. p1st) lptr_pcg8_b_c_aer(02,itype,ai_phase)  = p_pcg8_b_c_a02
        if (p_pcg9_b_c_a02 .ge. p1st) lptr_pcg9_b_c_aer(02,itype,ai_phase)  = p_pcg9_b_c_a02
        if (p_pcg1_b_o_a02 .ge. p1st) lptr_pcg1_b_o_aer(02,itype,ai_phase) = p_pcg1_b_o_a02
        if (p_pcg2_b_o_a02 .ge. p1st) lptr_pcg2_b_o_aer(02,itype,ai_phase)  = p_pcg2_b_o_a02
        if (p_pcg3_b_o_a02 .ge. p1st) lptr_pcg3_b_o_aer(02,itype,ai_phase)  = p_pcg3_b_o_a02
        if (p_pcg4_b_o_a02 .ge. p1st) lptr_pcg4_b_o_aer(02,itype,ai_phase)  = p_pcg4_b_o_a02
        if (p_pcg5_b_o_a02 .ge. p1st) lptr_pcg5_b_o_aer(02,itype,ai_phase)  = p_pcg5_b_o_a02
        if (p_pcg6_b_o_a02 .ge. p1st) lptr_pcg6_b_o_aer(02,itype,ai_phase)  = p_pcg6_b_o_a02
        if (p_pcg7_b_o_a02 .ge. p1st) lptr_pcg7_b_o_aer(02,itype,ai_phase)  = p_pcg7_b_o_a02
        if (p_pcg8_b_o_a02 .ge. p1st) lptr_pcg8_b_o_aer(02,itype,ai_phase)  = p_pcg8_b_o_a02
        if (p_pcg9_b_o_a02 .ge. p1st) lptr_pcg9_b_o_aer(02,itype,ai_phase)  = p_pcg9_b_o_a02
        if (p_opcg1_b_c_a02 .ge. p1st) lptr_opcg1_b_c_aer(02,itype,ai_phase) = p_opcg1_b_c_a02
        if (p_opcg2_b_c_a02 .ge. p1st) lptr_opcg2_b_c_aer(02,itype,ai_phase)  = p_opcg2_b_c_a02
        if (p_opcg3_b_c_a02 .ge. p1st) lptr_opcg3_b_c_aer(02,itype,ai_phase)  = p_opcg3_b_c_a02
        if (p_opcg4_b_c_a02 .ge. p1st) lptr_opcg4_b_c_aer(02,itype,ai_phase)  = p_opcg4_b_c_a02
        if (p_opcg5_b_c_a02 .ge. p1st) lptr_opcg5_b_c_aer(02,itype,ai_phase)  = p_opcg5_b_c_a02
        if (p_opcg6_b_c_a02 .ge. p1st) lptr_opcg6_b_c_aer(02,itype,ai_phase)  = p_opcg6_b_c_a02
        if (p_opcg7_b_c_a02 .ge. p1st) lptr_opcg7_b_c_aer(02,itype,ai_phase)  = p_opcg7_b_c_a02
        if (p_opcg8_b_c_a02 .ge. p1st) lptr_opcg8_b_c_aer(02,itype,ai_phase)  = p_opcg8_b_c_a02
        if (p_opcg1_b_o_a02 .ge. p1st) lptr_opcg1_b_o_aer(02,itype,ai_phase) = p_opcg1_b_o_a02
        if (p_opcg2_b_o_a02 .ge. p1st) lptr_opcg2_b_o_aer(02,itype,ai_phase)  = p_opcg2_b_o_a02
        if (p_opcg3_b_o_a02 .ge. p1st) lptr_opcg3_b_o_aer(02,itype,ai_phase)  = p_opcg3_b_o_a02
        if (p_opcg4_b_o_a02 .ge. p1st) lptr_opcg4_b_o_aer(02,itype,ai_phase)  = p_opcg4_b_o_a02
        if (p_opcg5_b_o_a02 .ge. p1st) lptr_opcg5_b_o_aer(02,itype,ai_phase)  = p_opcg5_b_o_a02
        if (p_opcg6_b_o_a02 .ge. p1st) lptr_opcg6_b_o_aer(02,itype,ai_phase)  = p_opcg6_b_o_a02
        if (p_opcg7_b_o_a02 .ge. p1st) lptr_opcg7_b_o_aer(02,itype,ai_phase)  = p_opcg7_b_o_a02
        if (p_opcg8_b_o_a02 .ge. p1st) lptr_opcg8_b_o_aer(02,itype,ai_phase)  = p_opcg8_b_o_a02
        if (p_pcg1_f_c_a02 .ge. p1st) lptr_pcg1_f_c_aer(02,itype,ai_phase) = p_pcg1_f_c_a02
        if (p_pcg2_f_c_a02 .ge. p1st) lptr_pcg2_f_c_aer(02,itype,ai_phase)  = p_pcg2_f_c_a02
        if (p_pcg3_f_c_a02 .ge. p1st) lptr_pcg3_f_c_aer(02,itype,ai_phase)  = p_pcg3_f_c_a02
        if (p_pcg4_f_c_a02 .ge. p1st) lptr_pcg4_f_c_aer(02,itype,ai_phase)  = p_pcg4_f_c_a02
        if (p_pcg5_f_c_a02 .ge. p1st) lptr_pcg5_f_c_aer(02,itype,ai_phase)  = p_pcg5_f_c_a02
        if (p_pcg6_f_c_a02 .ge. p1st) lptr_pcg6_f_c_aer(02,itype,ai_phase)  = p_pcg6_f_c_a02
        if (p_pcg7_f_c_a02 .ge. p1st) lptr_pcg7_f_c_aer(02,itype,ai_phase)  = p_pcg7_f_c_a02
        if (p_pcg8_f_c_a02 .ge. p1st) lptr_pcg8_f_c_aer(02,itype,ai_phase)  = p_pcg8_f_c_a02
        if (p_pcg9_f_c_a02 .ge. p1st) lptr_pcg9_f_c_aer(02,itype,ai_phase)  = p_pcg9_f_c_a02
        if (p_pcg1_f_o_a02 .ge. p1st) lptr_pcg1_f_o_aer(02,itype,ai_phase) = p_pcg1_f_o_a02
        if (p_pcg2_f_o_a02 .ge. p1st) lptr_pcg2_f_o_aer(02,itype,ai_phase)  = p_pcg2_f_o_a02
        if (p_pcg3_f_o_a02 .ge. p1st) lptr_pcg3_f_o_aer(02,itype,ai_phase)  = p_pcg3_f_o_a02
        if (p_pcg4_f_o_a02 .ge. p1st) lptr_pcg4_f_o_aer(02,itype,ai_phase)  = p_pcg4_f_o_a02
        if (p_pcg5_f_o_a02 .ge. p1st) lptr_pcg5_f_o_aer(02,itype,ai_phase)  = p_pcg5_f_o_a02
        if (p_pcg6_f_o_a02 .ge. p1st) lptr_pcg6_f_o_aer(02,itype,ai_phase)  = p_pcg6_f_o_a02
        if (p_pcg7_f_o_a02 .ge. p1st) lptr_pcg7_f_o_aer(02,itype,ai_phase)  = p_pcg7_f_o_a02
        if (p_pcg8_f_o_a02 .ge. p1st) lptr_pcg8_f_o_aer(02,itype,ai_phase)  = p_pcg8_f_o_a02
        if (p_pcg9_f_o_a02 .ge. p1st) lptr_pcg9_f_o_aer(02,itype,ai_phase)  = p_pcg9_f_o_a02
        if (p_opcg1_f_c_a02 .ge. p1st) lptr_opcg1_f_c_aer(02,itype,ai_phase) = p_opcg1_f_c_a02
        if (p_opcg2_f_c_a02 .ge. p1st) lptr_opcg2_f_c_aer(02,itype,ai_phase)  = p_opcg2_f_c_a02
        if (p_opcg3_f_c_a02 .ge. p1st) lptr_opcg3_f_c_aer(02,itype,ai_phase)  = p_opcg3_f_c_a02
        if (p_opcg4_f_c_a02 .ge. p1st) lptr_opcg4_f_c_aer(02,itype,ai_phase)  = p_opcg4_f_c_a02
        if (p_opcg5_f_c_a02 .ge. p1st) lptr_opcg5_f_c_aer(02,itype,ai_phase)  = p_opcg5_f_c_a02
        if (p_opcg6_f_c_a02 .ge. p1st) lptr_opcg6_f_c_aer(02,itype,ai_phase)  = p_opcg6_f_c_a02
        if (p_opcg7_f_c_a02 .ge. p1st) lptr_opcg7_f_c_aer(02,itype,ai_phase)  = p_opcg7_f_c_a02
        if (p_opcg8_f_c_a02 .ge. p1st) lptr_opcg8_f_c_aer(02,itype,ai_phase)  = p_opcg8_f_c_a02
        if (p_opcg1_f_o_a02 .ge. p1st) lptr_opcg1_f_o_aer(02,itype,ai_phase) = p_opcg1_f_o_a02
        if (p_opcg2_f_o_a02 .ge. p1st) lptr_opcg2_f_o_aer(02,itype,ai_phase)  = p_opcg2_f_o_a02
        if (p_opcg3_f_o_a02 .ge. p1st) lptr_opcg3_f_o_aer(02,itype,ai_phase)  = p_opcg3_f_o_a02
        if (p_opcg4_f_o_a02 .ge. p1st) lptr_opcg4_f_o_aer(02,itype,ai_phase)  = p_opcg4_f_o_a02
        if (p_opcg5_f_o_a02 .ge. p1st) lptr_opcg5_f_o_aer(02,itype,ai_phase)  = p_opcg5_f_o_a02
        if (p_opcg6_f_o_a02 .ge. p1st) lptr_opcg6_f_o_aer(02,itype,ai_phase)  = p_opcg6_f_o_a02
        if (p_opcg7_f_o_a02 .ge. p1st) lptr_opcg7_f_o_aer(02,itype,ai_phase)  = p_opcg7_f_o_a02
        if (p_opcg8_f_o_a02 .ge. p1st) lptr_opcg8_f_o_aer(02,itype,ai_phase)  = p_opcg8_f_o_a02
        if (p_smpa_a02 .ge. p1st) lptr_smpa_aer(02,itype,ai_phase)  = p_smpa_a02
        if (p_smpbb_a02 .ge. p1st) lptr_smpbb_aer(02,itype,ai_phase)  = p_smpbb_a02
        if (p_glysoa_r1_a02 .ge. p1st) lptr_glysoa_r1_aer (02,itype,ai_phase) = p_glysoa_r1_a02
        if (p_glysoa_r2_a02 .ge. p1st) lptr_glysoa_r2_aer (02,itype,ai_phase) = p_glysoa_r2_a02
        if (p_glysoa_sfc_a02 .ge. p1st) lptr_glysoa_sfc_aer(02,itype,ai_phase)  = p_glysoa_sfc_a02
        if (p_glysoa_nh4_a02 .ge. p1st) lptr_glysoa_nh4_aer(02,itype,ai_phase)  = p_glysoa_nh4_a02
        if (p_glysoa_oh_a02 .ge. p1st) lptr_glysoa_oh_aer(02,itype,ai_phase)  = p_glysoa_oh_a02
        if (p_ant1_c_a02 .ge. p1st) lptr_ant1_c_aer(02,itype,ai_phase)  = p_ant1_c_a02
        if (p_ant2_c_a02 .ge. p1st) lptr_ant2_c_aer(02,itype,ai_phase)  = p_ant2_c_a02
        if (p_ant3_c_a02 .ge. p1st) lptr_ant3_c_aer(02,itype,ai_phase)  = p_ant3_c_a02
        if (p_ant4_c_a02 .ge. p1st) lptr_ant4_c_aer(02,itype,ai_phase)  = p_ant4_c_a02
        if (p_ant1_o_a02 .ge. p1st) lptr_ant1_o_aer(02,itype,ai_phase)  = p_ant1_o_a02
        if (p_ant2_o_a02 .ge. p1st) lptr_ant2_o_aer(02,itype,ai_phase)  = p_ant2_o_a02
        if (p_ant3_o_a02 .ge. p1st) lptr_ant3_o_aer(02,itype,ai_phase)  = p_ant3_o_a02
        if (p_ant4_o_a02 .ge. p1st) lptr_ant4_o_aer(02,itype,ai_phase)  = p_ant4_o_a02
        if (p_biog1_c_a02 .ge. p1st) lptr_biog1_c_aer(02,itype,ai_phase)  = p_biog1_c_a02
        if (p_biog2_c_a02 .ge. p1st) lptr_biog2_c_aer(02,itype,ai_phase)  = p_biog2_c_a02
        if (p_biog3_c_a02 .ge. p1st) lptr_biog3_c_aer(02,itype,ai_phase)  = p_biog3_c_a02
        if (p_biog4_c_a02 .ge. p1st) lptr_biog4_c_aer(02,itype,ai_phase)  = p_biog4_c_a02
        if (p_biog1_o_a02 .ge. p1st) lptr_biog1_o_aer(02,itype,ai_phase)  = p_biog1_o_a02
        if (p_biog2_o_a02 .ge. p1st) lptr_biog2_o_aer(02,itype,ai_phase)  = p_biog2_o_a02
        if (p_biog3_o_a02 .ge. p1st) lptr_biog3_o_aer(02,itype,ai_phase)  = p_biog3_o_a02
        if (p_biog4_o_a02 .ge. p1st) lptr_biog4_o_aer(02,itype,ai_phase)  = p_biog4_o_a02
        if (p_asoaX_a02 .ge. p1st) lptr_asoaX_aer(02,itype,ai_phase)  = p_asoaX_a02
        if (p_asoa1_a02 .ge. p1st) lptr_asoa1_aer(02,itype,ai_phase)  = p_asoa1_a02
        if (p_asoa2_a02 .ge. p1st) lptr_asoa2_aer(02,itype,ai_phase)  = p_asoa2_a02
        if (p_asoa3_a02 .ge. p1st) lptr_asoa3_aer(02,itype,ai_phase)  = p_asoa3_a02
        if (p_asoa4_a02 .ge. p1st) lptr_asoa4_aer(02,itype,ai_phase)  = p_asoa4_a02
        if (p_bsoaX_a02 .ge. p1st) lptr_bsoaX_aer(02,itype,ai_phase)  = p_bsoaX_a02
        if (p_bsoa1_a02 .ge. p1st) lptr_bsoa1_aer(02,itype,ai_phase)  = p_bsoa1_a02
        if (p_bsoa2_a02 .ge. p1st) lptr_bsoa2_aer(02,itype,ai_phase)  = p_bsoa2_a02
        if (p_bsoa3_a02 .ge. p1st) lptr_bsoa3_aer(02,itype,ai_phase)  = p_bsoa3_a02
        if (p_bsoa4_a02 .ge. p1st) lptr_bsoa4_aer(02,itype,ai_phase)  = p_bsoa4_a02
        if (p_num_a02 .ge. p1st)  numptr_aer(02,itype,ai_phase)        = p_num_a02
        end if

	if (nsize_aer(itype) .ge. 3) then
        if (p_so4_a03 .ge. p1st) lptr_so4_aer(03,itype,ai_phase) = p_so4_a03
        if (p_no3_a03 .ge. p1st) lptr_no3_aer(03,itype,ai_phase) = p_no3_a03
        if (p_cl_a03 .ge. p1st) lptr_cl_aer(03,itype,ai_phase)  = p_cl_a03
        if (p_msa_a03 .ge. p1st) lptr_msa_aer(03,itype,ai_phase) = p_msa_a03
        if (p_co3_a03 .ge. p1st) lptr_co3_aer(03,itype,ai_phase) = p_co3_a03
        if (p_nh4_a03 .ge. p1st) lptr_nh4_aer(03,itype,ai_phase) = p_nh4_a03
        if (p_na_a03 .ge. p1st) lptr_na_aer(03,itype,ai_phase)  = p_na_a03
        if (p_ca_a03 .ge. p1st) lptr_ca_aer(03,itype,ai_phase)  = p_ca_a03
        if (p_oin_a03 .ge. p1st) lptr_oin_aer(03,itype,ai_phase) = p_oin_a03
        if (p_oc_a03 .ge. p1st) lptr_oc_aer(03,itype,ai_phase)  = p_oc_a03
        if (p_bc_a03 .ge. p1st) lptr_bc_aer(03,itype,ai_phase)  = p_bc_a03
        if (p_hysw_a03 .ge. p1st) hyswptr_aer(03,itype)         = p_hysw_a03
        if (p_water_a03 .ge. p1st) waterptr_aer(03,itype)       = p_water_a03
        if (p_pcg1_b_c_a03 .ge. p1st) lptr_pcg1_b_c_aer(03,itype,ai_phase) = p_pcg1_b_c_a03
        if (p_pcg2_b_c_a03 .ge. p1st) lptr_pcg2_b_c_aer(03,itype,ai_phase)  = p_pcg2_b_c_a03
        if (p_pcg3_b_c_a03 .ge. p1st) lptr_pcg3_b_c_aer(03,itype,ai_phase)  = p_pcg3_b_c_a03
        if (p_pcg4_b_c_a03 .ge. p1st) lptr_pcg4_b_c_aer(03,itype,ai_phase)  = p_pcg4_b_c_a03
        if (p_pcg5_b_c_a03 .ge. p1st) lptr_pcg5_b_c_aer(03,itype,ai_phase)  = p_pcg5_b_c_a03
        if (p_pcg6_b_c_a03 .ge. p1st) lptr_pcg6_b_c_aer(03,itype,ai_phase)  = p_pcg6_b_c_a03
        if (p_pcg7_b_c_a03 .ge. p1st) lptr_pcg7_b_c_aer(03,itype,ai_phase)  = p_pcg7_b_c_a03
        if (p_pcg8_b_c_a03 .ge. p1st) lptr_pcg8_b_c_aer(03,itype,ai_phase)  = p_pcg8_b_c_a03
        if (p_pcg9_b_c_a03 .ge. p1st) lptr_pcg9_b_c_aer(03,itype,ai_phase)  = p_pcg9_b_c_a03
        if (p_pcg1_b_o_a03 .ge. p1st) lptr_pcg1_b_o_aer(03,itype,ai_phase) = p_pcg1_b_o_a03
        if (p_pcg2_b_o_a03 .ge. p1st) lptr_pcg2_b_o_aer(03,itype,ai_phase)  = p_pcg2_b_o_a03
        if (p_pcg3_b_o_a03 .ge. p1st) lptr_pcg3_b_o_aer(03,itype,ai_phase)  = p_pcg3_b_o_a03
        if (p_pcg4_b_o_a03 .ge. p1st) lptr_pcg4_b_o_aer(03,itype,ai_phase)  = p_pcg4_b_o_a03
        if (p_pcg5_b_o_a03 .ge. p1st) lptr_pcg5_b_o_aer(03,itype,ai_phase)  = p_pcg5_b_o_a03
        if (p_pcg6_b_o_a03 .ge. p1st) lptr_pcg6_b_o_aer(03,itype,ai_phase)  = p_pcg6_b_o_a03
        if (p_pcg7_b_o_a03 .ge. p1st) lptr_pcg7_b_o_aer(03,itype,ai_phase)  = p_pcg7_b_o_a03
        if (p_pcg8_b_o_a03 .ge. p1st) lptr_pcg8_b_o_aer(03,itype,ai_phase)  = p_pcg8_b_o_a03
        if (p_pcg9_b_o_a03 .ge. p1st) lptr_pcg9_b_o_aer(03,itype,ai_phase)  = p_pcg9_b_o_a03
        if (p_opcg1_b_c_a03 .ge. p1st) lptr_opcg1_b_c_aer(03,itype,ai_phase) = p_opcg1_b_c_a03
        if (p_opcg2_b_c_a03 .ge. p1st) lptr_opcg2_b_c_aer(03,itype,ai_phase)  = p_opcg2_b_c_a03
        if (p_opcg3_b_c_a03 .ge. p1st) lptr_opcg3_b_c_aer(03,itype,ai_phase)  = p_opcg3_b_c_a03
        if (p_opcg4_b_c_a03 .ge. p1st) lptr_opcg4_b_c_aer(03,itype,ai_phase)  = p_opcg4_b_c_a03
        if (p_opcg5_b_c_a03 .ge. p1st) lptr_opcg5_b_c_aer(03,itype,ai_phase)  = p_opcg5_b_c_a03
        if (p_opcg6_b_c_a03 .ge. p1st) lptr_opcg6_b_c_aer(03,itype,ai_phase)  = p_opcg6_b_c_a03
        if (p_opcg7_b_c_a03 .ge. p1st) lptr_opcg7_b_c_aer(03,itype,ai_phase)  = p_opcg7_b_c_a03
        if (p_opcg8_b_c_a03 .ge. p1st) lptr_opcg8_b_c_aer(03,itype,ai_phase)  = p_opcg8_b_c_a03
        if (p_opcg1_b_o_a03 .ge. p1st) lptr_opcg1_b_o_aer(03,itype,ai_phase) = p_opcg1_b_o_a03
        if (p_opcg2_b_o_a03 .ge. p1st) lptr_opcg2_b_o_aer(03,itype,ai_phase)  = p_opcg2_b_o_a03
        if (p_opcg3_b_o_a03 .ge. p1st) lptr_opcg3_b_o_aer(03,itype,ai_phase)  = p_opcg3_b_o_a03
        if (p_opcg4_b_o_a03 .ge. p1st) lptr_opcg4_b_o_aer(03,itype,ai_phase)  = p_opcg4_b_o_a03
        if (p_opcg5_b_o_a03 .ge. p1st) lptr_opcg5_b_o_aer(03,itype,ai_phase)  = p_opcg5_b_o_a03
        if (p_opcg6_b_o_a03 .ge. p1st) lptr_opcg6_b_o_aer(03,itype,ai_phase)  = p_opcg6_b_o_a03
        if (p_opcg7_b_o_a03 .ge. p1st) lptr_opcg7_b_o_aer(03,itype,ai_phase)  = p_opcg7_b_o_a03
        if (p_opcg8_b_o_a03 .ge. p1st) lptr_opcg8_b_o_aer(03,itype,ai_phase)  = p_opcg8_b_o_a03
        if (p_pcg1_f_c_a03 .ge. p1st) lptr_pcg1_f_c_aer(03,itype,ai_phase) = p_pcg1_f_c_a03
        if (p_pcg2_f_c_a03 .ge. p1st) lptr_pcg2_f_c_aer(03,itype,ai_phase)  = p_pcg2_f_c_a03
        if (p_pcg3_f_c_a03 .ge. p1st) lptr_pcg3_f_c_aer(03,itype,ai_phase)  = p_pcg3_f_c_a03
        if (p_pcg4_f_c_a03 .ge. p1st) lptr_pcg4_f_c_aer(03,itype,ai_phase)  = p_pcg4_f_c_a03
        if (p_pcg5_f_c_a03 .ge. p1st) lptr_pcg5_f_c_aer(03,itype,ai_phase)  = p_pcg5_f_c_a03
        if (p_pcg6_f_c_a03 .ge. p1st) lptr_pcg6_f_c_aer(03,itype,ai_phase)  = p_pcg6_f_c_a03
        if (p_pcg7_f_c_a03 .ge. p1st) lptr_pcg7_f_c_aer(03,itype,ai_phase)  = p_pcg7_f_c_a03
        if (p_pcg8_f_c_a03 .ge. p1st) lptr_pcg8_f_c_aer(03,itype,ai_phase)  = p_pcg8_f_c_a03
        if (p_pcg9_f_c_a03 .ge. p1st) lptr_pcg9_f_c_aer(03,itype,ai_phase)  = p_pcg9_f_c_a03
        if (p_pcg1_f_o_a03 .ge. p1st) lptr_pcg1_f_o_aer(03,itype,ai_phase) = p_pcg1_f_o_a03
        if (p_pcg2_f_o_a03 .ge. p1st) lptr_pcg2_f_o_aer(03,itype,ai_phase)  = p_pcg2_f_o_a03
        if (p_pcg3_f_o_a03 .ge. p1st) lptr_pcg3_f_o_aer(03,itype,ai_phase)  = p_pcg3_f_o_a03
        if (p_pcg4_f_o_a03 .ge. p1st) lptr_pcg4_f_o_aer(03,itype,ai_phase)  = p_pcg4_f_o_a03
        if (p_pcg5_f_o_a03 .ge. p1st) lptr_pcg5_f_o_aer(03,itype,ai_phase)  = p_pcg5_f_o_a03
        if (p_pcg6_f_o_a03 .ge. p1st) lptr_pcg6_f_o_aer(03,itype,ai_phase)  = p_pcg6_f_o_a03
        if (p_pcg7_f_o_a03 .ge. p1st) lptr_pcg7_f_o_aer(03,itype,ai_phase)  = p_pcg7_f_o_a03
        if (p_pcg8_f_o_a03 .ge. p1st) lptr_pcg8_f_o_aer(03,itype,ai_phase)  = p_pcg8_f_o_a03
        if (p_pcg9_f_o_a03 .ge. p1st) lptr_pcg9_f_o_aer(03,itype,ai_phase)  = p_pcg9_f_o_a03
        if (p_opcg1_f_c_a03 .ge. p1st) lptr_opcg1_f_c_aer(03,itype,ai_phase) = p_opcg1_f_c_a03
        if (p_opcg2_f_c_a03 .ge. p1st) lptr_opcg2_f_c_aer(03,itype,ai_phase)  = p_opcg2_f_c_a03
        if (p_opcg3_f_c_a03 .ge. p1st) lptr_opcg3_f_c_aer(03,itype,ai_phase)  = p_opcg3_f_c_a03
        if (p_opcg4_f_c_a03 .ge. p1st) lptr_opcg4_f_c_aer(03,itype,ai_phase)  = p_opcg4_f_c_a03
        if (p_opcg5_f_c_a03 .ge. p1st) lptr_opcg5_f_c_aer(03,itype,ai_phase)  = p_opcg5_f_c_a03
        if (p_opcg6_f_c_a03 .ge. p1st) lptr_opcg6_f_c_aer(03,itype,ai_phase)  = p_opcg6_f_c_a03
        if (p_opcg7_f_c_a03 .ge. p1st) lptr_opcg7_f_c_aer(03,itype,ai_phase)  = p_opcg7_f_c_a03
        if (p_opcg8_f_c_a03 .ge. p1st) lptr_opcg8_f_c_aer(03,itype,ai_phase)  = p_opcg8_f_c_a03
        if (p_opcg1_f_o_a03 .ge. p1st) lptr_opcg1_f_o_aer(03,itype,ai_phase) = p_opcg1_f_o_a03
        if (p_opcg2_f_o_a03 .ge. p1st) lptr_opcg2_f_o_aer(03,itype,ai_phase)  = p_opcg2_f_o_a03
        if (p_opcg3_f_o_a03 .ge. p1st) lptr_opcg3_f_o_aer(03,itype,ai_phase)  = p_opcg3_f_o_a03
        if (p_opcg4_f_o_a03 .ge. p1st) lptr_opcg4_f_o_aer(03,itype,ai_phase)  = p_opcg4_f_o_a03
        if (p_opcg5_f_o_a03 .ge. p1st) lptr_opcg5_f_o_aer(03,itype,ai_phase)  = p_opcg5_f_o_a03
        if (p_opcg6_f_o_a03 .ge. p1st) lptr_opcg6_f_o_aer(03,itype,ai_phase)  = p_opcg6_f_o_a03
        if (p_opcg7_f_o_a03 .ge. p1st) lptr_opcg7_f_o_aer(03,itype,ai_phase)  = p_opcg7_f_o_a03
        if (p_opcg8_f_o_a03 .ge. p1st) lptr_opcg8_f_o_aer(03,itype,ai_phase)  = p_opcg8_f_o_a03
        if (p_smpa_a03 .ge. p1st) lptr_smpa_aer(03,itype,ai_phase)  = p_smpa_a03
        if (p_smpbb_a03 .ge. p1st) lptr_smpbb_aer(03,itype,ai_phase)  = p_smpbb_a03
        if (p_glysoa_r1_a03 .ge. p1st) lptr_glysoa_r1_aer (03,itype,ai_phase) = p_glysoa_r1_a03
        if (p_glysoa_r2_a03 .ge. p1st) lptr_glysoa_r2_aer (03,itype,ai_phase) = p_glysoa_r2_a03
        if (p_glysoa_sfc_a03 .ge. p1st) lptr_glysoa_sfc_aer(03,itype,ai_phase)  = p_glysoa_sfc_a03
        if (p_glysoa_nh4_a03 .ge. p1st) lptr_glysoa_nh4_aer(03,itype,ai_phase)  = p_glysoa_nh4_a03
        if (p_glysoa_oh_a03 .ge. p1st) lptr_glysoa_oh_aer(03,itype,ai_phase)  = p_glysoa_oh_a03
        if (p_ant1_c_a03 .ge. p1st) lptr_ant1_c_aer(03,itype,ai_phase)  = p_ant1_c_a03
        if (p_ant2_c_a03 .ge. p1st) lptr_ant2_c_aer(03,itype,ai_phase)  = p_ant2_c_a03
        if (p_ant3_c_a03 .ge. p1st) lptr_ant3_c_aer(03,itype,ai_phase)  = p_ant3_c_a03
        if (p_ant4_c_a03 .ge. p1st) lptr_ant4_c_aer(03,itype,ai_phase)  = p_ant4_c_a03
        if (p_ant1_o_a03 .ge. p1st) lptr_ant1_o_aer(03,itype,ai_phase)  = p_ant1_o_a03
        if (p_ant2_o_a03 .ge. p1st) lptr_ant2_o_aer(03,itype,ai_phase)  = p_ant2_o_a03
        if (p_ant3_o_a03 .ge. p1st) lptr_ant3_o_aer(03,itype,ai_phase)  = p_ant3_o_a03
        if (p_ant4_o_a03 .ge. p1st) lptr_ant4_o_aer(03,itype,ai_phase)  = p_ant4_o_a03
        if (p_biog1_c_a03 .ge. p1st) lptr_biog1_c_aer(03,itype,ai_phase)  = p_biog1_c_a03
        if (p_biog2_c_a03 .ge. p1st) lptr_biog2_c_aer(03,itype,ai_phase)  = p_biog2_c_a03
        if (p_biog3_c_a03 .ge. p1st) lptr_biog3_c_aer(03,itype,ai_phase)  = p_biog3_c_a03
        if (p_biog4_c_a03 .ge. p1st) lptr_biog4_c_aer(03,itype,ai_phase)  = p_biog4_c_a03
        if (p_biog1_o_a03 .ge. p1st) lptr_biog1_o_aer(03,itype,ai_phase)  = p_biog1_o_a03
        if (p_biog2_o_a03 .ge. p1st) lptr_biog2_o_aer(03,itype,ai_phase)  = p_biog2_o_a03
        if (p_biog3_o_a03 .ge. p1st) lptr_biog3_o_aer(03,itype,ai_phase)  = p_biog3_o_a03
        if (p_biog4_o_a03 .ge. p1st) lptr_biog4_o_aer(03,itype,ai_phase)  = p_biog4_o_a03
        if (p_asoaX_a03 .ge. p1st) lptr_asoaX_aer(03,itype,ai_phase)  = p_asoaX_a03
        if (p_asoa1_a03 .ge. p1st) lptr_asoa1_aer(03,itype,ai_phase)  = p_asoa1_a03
        if (p_asoa2_a03 .ge. p1st) lptr_asoa2_aer(03,itype,ai_phase)  = p_asoa2_a03
        if (p_asoa3_a03 .ge. p1st) lptr_asoa3_aer(03,itype,ai_phase)  = p_asoa3_a03
        if (p_asoa4_a03 .ge. p1st) lptr_asoa4_aer(03,itype,ai_phase)  = p_asoa4_a03
        if (p_bsoaX_a03 .ge. p1st) lptr_bsoaX_aer(03,itype,ai_phase)  = p_bsoaX_a03
        if (p_bsoa1_a03 .ge. p1st) lptr_bsoa1_aer(03,itype,ai_phase)  = p_bsoa1_a03
        if (p_bsoa2_a03 .ge. p1st) lptr_bsoa2_aer(03,itype,ai_phase)  = p_bsoa2_a03
        if (p_bsoa3_a03 .ge. p1st) lptr_bsoa3_aer(03,itype,ai_phase)  = p_bsoa3_a03
        if (p_bsoa4_a03 .ge. p1st) lptr_bsoa4_aer(03,itype,ai_phase)  = p_bsoa4_a03
        if (p_num_a03 .ge. p1st)  numptr_aer(03,itype,ai_phase)        = p_num_a03
        end if


	if (nsize_aer(itype) .ge. 4) then
        if (p_so4_a04 .ge. p1st) lptr_so4_aer(04,itype,ai_phase) = p_so4_a04
        if (p_no3_a04 .ge. p1st) lptr_no3_aer(04,itype,ai_phase) = p_no3_a04
        if (p_cl_a04 .ge. p1st) lptr_cl_aer(04,itype,ai_phase)  = p_cl_a04
        if (p_msa_a04 .ge. p1st) lptr_msa_aer(04,itype,ai_phase) = p_msa_a04
        if (p_co3_a04 .ge. p1st) lptr_co3_aer(04,itype,ai_phase) = p_co3_a04
        if (p_nh4_a04 .ge. p1st) lptr_nh4_aer(04,itype,ai_phase) = p_nh4_a04
        if (p_na_a04 .ge. p1st) lptr_na_aer(04,itype,ai_phase)  = p_na_a04
        if (p_ca_a04 .ge. p1st) lptr_ca_aer(04,itype,ai_phase)  = p_ca_a04
        if (p_oin_a04 .ge. p1st) lptr_oin_aer(04,itype,ai_phase) = p_oin_a04
        if (p_oc_a04 .ge. p1st) lptr_oc_aer(04,itype,ai_phase)  = p_oc_a04
        if (p_bc_a04 .ge. p1st) lptr_bc_aer(04,itype,ai_phase)  = p_bc_a04
        if (p_hysw_a04 .ge. p1st) hyswptr_aer(04,itype)         = p_hysw_a04
        if (p_water_a04 .ge. p1st) waterptr_aer(04,itype)       = p_water_a04
        if (p_pcg1_b_c_a04 .ge. p1st) lptr_pcg1_b_c_aer(04,itype,ai_phase) = p_pcg1_b_c_a04
        if (p_pcg2_b_c_a04 .ge. p1st) lptr_pcg2_b_c_aer(04,itype,ai_phase)  = p_pcg2_b_c_a04
        if (p_pcg3_b_c_a04 .ge. p1st) lptr_pcg3_b_c_aer(04,itype,ai_phase)  = p_pcg3_b_c_a04
        if (p_pcg4_b_c_a04 .ge. p1st) lptr_pcg4_b_c_aer(04,itype,ai_phase)  = p_pcg4_b_c_a04
        if (p_pcg5_b_c_a04 .ge. p1st) lptr_pcg5_b_c_aer(04,itype,ai_phase)  = p_pcg5_b_c_a04
        if (p_pcg6_b_c_a04 .ge. p1st) lptr_pcg6_b_c_aer(04,itype,ai_phase)  = p_pcg6_b_c_a04
        if (p_pcg7_b_c_a04 .ge. p1st) lptr_pcg7_b_c_aer(04,itype,ai_phase)  = p_pcg7_b_c_a04
        if (p_pcg8_b_c_a04 .ge. p1st) lptr_pcg8_b_c_aer(04,itype,ai_phase)  = p_pcg8_b_c_a04
        if (p_pcg9_b_c_a04 .ge. p1st) lptr_pcg9_b_c_aer(04,itype,ai_phase)  = p_pcg9_b_c_a04
        if (p_pcg1_b_o_a04 .ge. p1st) lptr_pcg1_b_o_aer(04,itype,ai_phase) = p_pcg1_b_o_a04
        if (p_pcg2_b_o_a04 .ge. p1st) lptr_pcg2_b_o_aer(04,itype,ai_phase)  = p_pcg2_b_o_a04
        if (p_pcg3_b_o_a04 .ge. p1st) lptr_pcg3_b_o_aer(04,itype,ai_phase)  = p_pcg3_b_o_a04
        if (p_pcg4_b_o_a04 .ge. p1st) lptr_pcg4_b_o_aer(04,itype,ai_phase)  = p_pcg4_b_o_a04
        if (p_pcg5_b_o_a04 .ge. p1st) lptr_pcg5_b_o_aer(04,itype,ai_phase)  = p_pcg5_b_o_a04
        if (p_pcg6_b_o_a04 .ge. p1st) lptr_pcg6_b_o_aer(04,itype,ai_phase)  = p_pcg6_b_o_a04
        if (p_pcg7_b_o_a04 .ge. p1st) lptr_pcg7_b_o_aer(04,itype,ai_phase)  = p_pcg7_b_o_a04
        if (p_pcg8_b_o_a04 .ge. p1st) lptr_pcg8_b_o_aer(04,itype,ai_phase)  = p_pcg8_b_o_a04
        if (p_pcg9_b_o_a04 .ge. p1st) lptr_pcg9_b_o_aer(04,itype,ai_phase)  = p_pcg9_b_o_a04
        if (p_opcg1_b_c_a04 .ge. p1st) lptr_opcg1_b_c_aer(04,itype,ai_phase) = p_opcg1_b_c_a04
        if (p_opcg2_b_c_a04 .ge. p1st) lptr_opcg2_b_c_aer(04,itype,ai_phase)  = p_opcg2_b_c_a04
        if (p_opcg3_b_c_a04 .ge. p1st) lptr_opcg3_b_c_aer(04,itype,ai_phase)  = p_opcg3_b_c_a04
        if (p_opcg4_b_c_a04 .ge. p1st) lptr_opcg4_b_c_aer(04,itype,ai_phase)  = p_opcg4_b_c_a04
        if (p_opcg5_b_c_a04 .ge. p1st) lptr_opcg5_b_c_aer(04,itype,ai_phase)  = p_opcg5_b_c_a04
        if (p_opcg6_b_c_a04 .ge. p1st) lptr_opcg6_b_c_aer(04,itype,ai_phase)  = p_opcg6_b_c_a04
        if (p_opcg7_b_c_a04 .ge. p1st) lptr_opcg7_b_c_aer(04,itype,ai_phase)  = p_opcg7_b_c_a04
        if (p_opcg8_b_c_a04 .ge. p1st) lptr_opcg8_b_c_aer(04,itype,ai_phase)  = p_opcg8_b_c_a04
        if (p_opcg1_b_o_a04 .ge. p1st) lptr_opcg1_b_o_aer(04,itype,ai_phase) = p_opcg1_b_o_a04
        if (p_opcg2_b_o_a04 .ge. p1st) lptr_opcg2_b_o_aer(04,itype,ai_phase)  = p_opcg2_b_o_a04
        if (p_opcg3_b_o_a04 .ge. p1st) lptr_opcg3_b_o_aer(04,itype,ai_phase)  = p_opcg3_b_o_a04
        if (p_opcg4_b_o_a04 .ge. p1st) lptr_opcg4_b_o_aer(04,itype,ai_phase)  = p_opcg4_b_o_a04
        if (p_opcg5_b_o_a04 .ge. p1st) lptr_opcg5_b_o_aer(04,itype,ai_phase)  = p_opcg5_b_o_a04
        if (p_opcg6_b_o_a04 .ge. p1st) lptr_opcg6_b_o_aer(04,itype,ai_phase)  = p_opcg6_b_o_a04
        if (p_opcg7_b_o_a04 .ge. p1st) lptr_opcg7_b_o_aer(04,itype,ai_phase)  = p_opcg7_b_o_a04
        if (p_opcg8_b_o_a04 .ge. p1st) lptr_opcg8_b_o_aer(04,itype,ai_phase)  = p_opcg8_b_o_a04
        if (p_pcg1_f_c_a04 .ge. p1st) lptr_pcg1_f_c_aer(04,itype,ai_phase) = p_pcg1_f_c_a04
        if (p_pcg2_f_c_a04 .ge. p1st) lptr_pcg2_f_c_aer(04,itype,ai_phase)  = p_pcg2_f_c_a04
        if (p_pcg3_f_c_a04 .ge. p1st) lptr_pcg3_f_c_aer(04,itype,ai_phase)  = p_pcg3_f_c_a04
        if (p_pcg4_f_c_a04 .ge. p1st) lptr_pcg4_f_c_aer(04,itype,ai_phase)  = p_pcg4_f_c_a04
        if (p_pcg5_f_c_a04 .ge. p1st) lptr_pcg5_f_c_aer(04,itype,ai_phase)  = p_pcg5_f_c_a04
        if (p_pcg6_f_c_a04 .ge. p1st) lptr_pcg6_f_c_aer(04,itype,ai_phase)  = p_pcg6_f_c_a04
        if (p_pcg7_f_c_a04 .ge. p1st) lptr_pcg7_f_c_aer(04,itype,ai_phase)  = p_pcg7_f_c_a04
        if (p_pcg8_f_c_a04 .ge. p1st) lptr_pcg8_f_c_aer(04,itype,ai_phase)  = p_pcg8_f_c_a04
        if (p_pcg9_f_c_a04 .ge. p1st) lptr_pcg9_f_c_aer(04,itype,ai_phase)  = p_pcg9_f_c_a04
        if (p_pcg1_f_o_a04 .ge. p1st) lptr_pcg1_f_o_aer(04,itype,ai_phase) = p_pcg1_f_o_a04
        if (p_pcg2_f_o_a04 .ge. p1st) lptr_pcg2_f_o_aer(04,itype,ai_phase)  = p_pcg2_f_o_a04
        if (p_pcg3_f_o_a04 .ge. p1st) lptr_pcg3_f_o_aer(04,itype,ai_phase)  = p_pcg3_f_o_a04
        if (p_pcg4_f_o_a04 .ge. p1st) lptr_pcg4_f_o_aer(04,itype,ai_phase)  = p_pcg4_f_o_a04
        if (p_pcg5_f_o_a04 .ge. p1st) lptr_pcg5_f_o_aer(04,itype,ai_phase)  = p_pcg5_f_o_a04
        if (p_pcg6_f_o_a04 .ge. p1st) lptr_pcg6_f_o_aer(04,itype,ai_phase)  = p_pcg6_f_o_a04
        if (p_pcg7_f_o_a04 .ge. p1st) lptr_pcg7_f_o_aer(04,itype,ai_phase)  = p_pcg7_f_o_a04
        if (p_pcg8_f_o_a04 .ge. p1st) lptr_pcg8_f_o_aer(04,itype,ai_phase)  = p_pcg8_f_o_a04
        if (p_pcg9_f_o_a04 .ge. p1st) lptr_pcg9_f_o_aer(04,itype,ai_phase)  = p_pcg9_f_o_a04
        if (p_opcg1_f_c_a04 .ge. p1st) lptr_opcg1_f_c_aer(04,itype,ai_phase) = p_opcg1_f_c_a04
        if (p_opcg2_f_c_a04 .ge. p1st) lptr_opcg2_f_c_aer(04,itype,ai_phase)  = p_opcg2_f_c_a04
        if (p_opcg3_f_c_a04 .ge. p1st) lptr_opcg3_f_c_aer(04,itype,ai_phase)  = p_opcg3_f_c_a04
        if (p_opcg4_f_c_a04 .ge. p1st) lptr_opcg4_f_c_aer(04,itype,ai_phase)  = p_opcg4_f_c_a04
        if (p_opcg5_f_c_a04 .ge. p1st) lptr_opcg5_f_c_aer(04,itype,ai_phase)  = p_opcg5_f_c_a04
        if (p_opcg6_f_c_a04 .ge. p1st) lptr_opcg6_f_c_aer(04,itype,ai_phase)  = p_opcg6_f_c_a04
        if (p_opcg7_f_c_a04 .ge. p1st) lptr_opcg7_f_c_aer(04,itype,ai_phase)  = p_opcg7_f_c_a04
        if (p_opcg8_f_c_a04 .ge. p1st) lptr_opcg8_f_c_aer(04,itype,ai_phase)  = p_opcg8_f_c_a04
        if (p_opcg1_f_o_a04 .ge. p1st) lptr_opcg1_f_o_aer(04,itype,ai_phase) = p_opcg1_f_o_a04
        if (p_opcg2_f_o_a04 .ge. p1st) lptr_opcg2_f_o_aer(04,itype,ai_phase)  = p_opcg2_f_o_a04
        if (p_opcg3_f_o_a04 .ge. p1st) lptr_opcg3_f_o_aer(04,itype,ai_phase)  = p_opcg3_f_o_a04
        if (p_opcg4_f_o_a04 .ge. p1st) lptr_opcg4_f_o_aer(04,itype,ai_phase)  = p_opcg4_f_o_a04
        if (p_opcg5_f_o_a04 .ge. p1st) lptr_opcg5_f_o_aer(04,itype,ai_phase)  = p_opcg5_f_o_a04
        if (p_opcg6_f_o_a04 .ge. p1st) lptr_opcg6_f_o_aer(04,itype,ai_phase)  = p_opcg6_f_o_a04
        if (p_opcg7_f_o_a04 .ge. p1st) lptr_opcg7_f_o_aer(04,itype,ai_phase)  = p_opcg7_f_o_a04
        if (p_opcg8_f_o_a04 .ge. p1st) lptr_opcg8_f_o_aer(04,itype,ai_phase)  = p_opcg8_f_o_a04
        if (p_smpa_a04 .ge. p1st) lptr_smpa_aer(04,itype,ai_phase)  = p_smpa_a04
        if (p_smpbb_a04 .ge. p1st) lptr_smpbb_aer(04,itype,ai_phase)  = p_smpbb_a04
        if (p_glysoa_r1_a04 .ge. p1st) lptr_glysoa_r1_aer (04,itype,ai_phase) = p_glysoa_r1_a04
        if (p_glysoa_r2_a04 .ge. p1st) lptr_glysoa_r2_aer (04,itype,ai_phase) = p_glysoa_r2_a04
        if (p_glysoa_sfc_a04 .ge. p1st) lptr_glysoa_sfc_aer(04,itype,ai_phase)  = p_glysoa_sfc_a04
        if (p_glysoa_nh4_a04 .ge. p1st) lptr_glysoa_nh4_aer(04,itype,ai_phase)  = p_glysoa_nh4_a04
        if (p_glysoa_oh_a04 .ge. p1st) lptr_glysoa_oh_aer(04,itype,ai_phase)  = p_glysoa_oh_a04
        if (p_ant1_c_a04 .ge. p1st) lptr_ant1_c_aer(04,itype,ai_phase)  = p_ant1_c_a04
        if (p_ant2_c_a04 .ge. p1st) lptr_ant2_c_aer(04,itype,ai_phase)  = p_ant2_c_a04
        if (p_ant3_c_a04 .ge. p1st) lptr_ant3_c_aer(04,itype,ai_phase)  = p_ant3_c_a04
        if (p_ant4_c_a04 .ge. p1st) lptr_ant4_c_aer(04,itype,ai_phase)  = p_ant4_c_a04
        if (p_ant1_o_a04 .ge. p1st) lptr_ant1_o_aer(04,itype,ai_phase)  = p_ant1_o_a04
        if (p_ant2_o_a04 .ge. p1st) lptr_ant2_o_aer(04,itype,ai_phase)  = p_ant2_o_a04
        if (p_ant3_o_a04 .ge. p1st) lptr_ant3_o_aer(04,itype,ai_phase)  = p_ant3_o_a04
        if (p_ant4_o_a04 .ge. p1st) lptr_ant4_o_aer(04,itype,ai_phase)  = p_ant4_o_a04
        if (p_biog1_c_a04 .ge. p1st) lptr_biog1_c_aer(04,itype,ai_phase)  = p_biog1_c_a04
        if (p_biog2_c_a04 .ge. p1st) lptr_biog2_c_aer(04,itype,ai_phase)  = p_biog2_c_a04
        if (p_biog3_c_a04 .ge. p1st) lptr_biog3_c_aer(04,itype,ai_phase)  = p_biog3_c_a04
        if (p_biog4_c_a04 .ge. p1st) lptr_biog4_c_aer(04,itype,ai_phase)  = p_biog4_c_a04
        if (p_biog1_o_a04 .ge. p1st) lptr_biog1_o_aer(04,itype,ai_phase)  = p_biog1_o_a04
        if (p_biog2_o_a04 .ge. p1st) lptr_biog2_o_aer(04,itype,ai_phase)  = p_biog2_o_a04
        if (p_biog3_o_a04 .ge. p1st) lptr_biog3_o_aer(04,itype,ai_phase)  = p_biog3_o_a04
        if (p_biog4_o_a04 .ge. p1st) lptr_biog4_o_aer(04,itype,ai_phase)  = p_biog4_o_a04
        if (p_asoaX_a04 .ge. p1st) lptr_asoaX_aer(04,itype,ai_phase)  = p_asoaX_a04
        if (p_asoa1_a04 .ge. p1st) lptr_asoa1_aer(04,itype,ai_phase)  = p_asoa1_a04
        if (p_asoa2_a04 .ge. p1st) lptr_asoa2_aer(04,itype,ai_phase)  = p_asoa2_a04
        if (p_asoa3_a04 .ge. p1st) lptr_asoa3_aer(04,itype,ai_phase)  = p_asoa3_a04
        if (p_asoa4_a04 .ge. p1st) lptr_asoa4_aer(04,itype,ai_phase)  = p_asoa4_a04
        if (p_bsoaX_a04 .ge. p1st) lptr_bsoaX_aer(04,itype,ai_phase)  = p_bsoaX_a04
        if (p_bsoa1_a04 .ge. p1st) lptr_bsoa1_aer(04,itype,ai_phase)  = p_bsoa1_a04
        if (p_bsoa2_a04 .ge. p1st) lptr_bsoa2_aer(04,itype,ai_phase)  = p_bsoa2_a04
        if (p_bsoa3_a04 .ge. p1st) lptr_bsoa3_aer(04,itype,ai_phase)  = p_bsoa3_a04
        if (p_bsoa4_a04 .ge. p1st) lptr_bsoa4_aer(04,itype,ai_phase)  = p_bsoa4_a04
        if (p_num_a04 .ge. p1st)  numptr_aer(04,itype,ai_phase)        = p_num_a04
        end if


	if (nsize_aer(itype) .ge. 5) then
	    lptr_so4_aer(05,itype,ai_phase)      = p_so4_a05
	    lptr_no3_aer(05,itype,ai_phase)      = p_no3_a05
	    lptr_cl_aer(05,itype,ai_phase)       = p_cl_a05
	    lptr_msa_aer(05,itype,ai_phase)      = p_msa_a05
	    lptr_co3_aer(05,itype,ai_phase)      = p_co3_a05
	    lptr_nh4_aer(05,itype,ai_phase)      = p_nh4_a05
	    lptr_na_aer(05,itype,ai_phase)       = p_na_a05
	    lptr_ca_aer(05,itype,ai_phase)       = p_ca_a05
	    lptr_oin_aer(05,itype,ai_phase)      = p_oin_a05
	    lptr_oc_aer(05,itype,ai_phase)       = p_oc_a05
	    lptr_bc_aer(05,itype,ai_phase)       = p_bc_a05
	    hyswptr_aer(05,itype)                = p_hysw_a05
	    waterptr_aer(05,itype)               = p_water_a05

            if (p_pcg1_b_c_a05 .ge. p1st) lptr_pcg1_b_c_aer(05,itype,ai_phase) = p_pcg1_b_c_a05
            if (p_pcg2_b_c_a05 .ge. p1st) lptr_pcg2_b_c_aer(05,itype,ai_phase)  = p_pcg2_b_c_a05
            if (p_pcg3_b_c_a05 .ge. p1st) lptr_pcg3_b_c_aer(05,itype,ai_phase)  = p_pcg3_b_c_a05
            if (p_pcg4_b_c_a05 .ge. p1st) lptr_pcg4_b_c_aer(05,itype,ai_phase)  = p_pcg4_b_c_a05
            if (p_pcg5_b_c_a05 .ge. p1st) lptr_pcg5_b_c_aer(05,itype,ai_phase)  = p_pcg5_b_c_a05
            if (p_pcg6_b_c_a05 .ge. p1st) lptr_pcg6_b_c_aer(05,itype,ai_phase)  = p_pcg6_b_c_a05
            if (p_pcg7_b_c_a05 .ge. p1st) lptr_pcg7_b_c_aer(05,itype,ai_phase)  = p_pcg7_b_c_a05
            if (p_pcg8_b_c_a05 .ge. p1st) lptr_pcg8_b_c_aer(05,itype,ai_phase)  = p_pcg8_b_c_a05
            if (p_pcg9_b_c_a05 .ge. p1st) lptr_pcg9_b_c_aer(05,itype,ai_phase)  = p_pcg9_b_c_a05
            if (p_pcg1_b_o_a05 .ge. p1st) lptr_pcg1_b_o_aer(05,itype,ai_phase) = p_pcg1_b_o_a05
            if (p_pcg2_b_o_a05 .ge. p1st) lptr_pcg2_b_o_aer(05,itype,ai_phase)  = p_pcg2_b_o_a05
            if (p_pcg3_b_o_a05 .ge. p1st) lptr_pcg3_b_o_aer(05,itype,ai_phase)  = p_pcg3_b_o_a05
            if (p_pcg4_b_o_a05 .ge. p1st) lptr_pcg4_b_o_aer(05,itype,ai_phase)  = p_pcg4_b_o_a05
            if (p_pcg5_b_o_a05 .ge. p1st) lptr_pcg5_b_o_aer(05,itype,ai_phase)  = p_pcg5_b_o_a05
            if (p_pcg6_b_o_a05 .ge. p1st) lptr_pcg6_b_o_aer(05,itype,ai_phase)  = p_pcg6_b_o_a05
            if (p_pcg7_b_o_a05 .ge. p1st) lptr_pcg7_b_o_aer(05,itype,ai_phase)  = p_pcg7_b_o_a05
            if (p_pcg8_b_o_a05 .ge. p1st) lptr_pcg8_b_o_aer(05,itype,ai_phase)  = p_pcg8_b_o_a05
            if (p_pcg9_b_o_a05 .ge. p1st) lptr_pcg9_b_o_aer(05,itype,ai_phase)  = p_pcg9_b_o_a05
            if (p_opcg1_b_c_a05 .ge. p1st) lptr_opcg1_b_c_aer(05,itype,ai_phase) = p_opcg1_b_c_a05
            if (p_opcg2_b_c_a05 .ge. p1st) lptr_opcg2_b_c_aer(05,itype,ai_phase)  = p_opcg2_b_c_a05
            if (p_opcg3_b_c_a05 .ge. p1st) lptr_opcg3_b_c_aer(05,itype,ai_phase)  = p_opcg3_b_c_a05
            if (p_opcg4_b_c_a05 .ge. p1st) lptr_opcg4_b_c_aer(05,itype,ai_phase)  = p_opcg4_b_c_a05
            if (p_opcg5_b_c_a05 .ge. p1st) lptr_opcg5_b_c_aer(05,itype,ai_phase)  = p_opcg5_b_c_a05
            if (p_opcg6_b_c_a05 .ge. p1st) lptr_opcg6_b_c_aer(05,itype,ai_phase)  = p_opcg6_b_c_a05
            if (p_opcg7_b_c_a05 .ge. p1st) lptr_opcg7_b_c_aer(05,itype,ai_phase)  = p_opcg7_b_c_a05
            if (p_opcg8_b_c_a05 .ge. p1st) lptr_opcg8_b_c_aer(05,itype,ai_phase)  = p_opcg8_b_c_a05
            if (p_opcg1_b_o_a05 .ge. p1st) lptr_opcg1_b_o_aer(05,itype,ai_phase) = p_opcg1_b_o_a05
            if (p_opcg2_b_o_a05 .ge. p1st) lptr_opcg2_b_o_aer(05,itype,ai_phase)  = p_opcg2_b_o_a05
            if (p_opcg3_b_o_a05 .ge. p1st) lptr_opcg3_b_o_aer(05,itype,ai_phase)  = p_opcg3_b_o_a05
            if (p_opcg4_b_o_a05 .ge. p1st) lptr_opcg4_b_o_aer(05,itype,ai_phase)  = p_opcg4_b_o_a05
            if (p_opcg5_b_o_a05 .ge. p1st) lptr_opcg5_b_o_aer(05,itype,ai_phase)  = p_opcg5_b_o_a05
            if (p_opcg6_b_o_a05 .ge. p1st) lptr_opcg6_b_o_aer(05,itype,ai_phase)  = p_opcg6_b_o_a05
            if (p_opcg7_b_o_a05 .ge. p1st) lptr_opcg7_b_o_aer(05,itype,ai_phase)  = p_opcg7_b_o_a05
            if (p_opcg8_b_o_a05 .ge. p1st) lptr_opcg8_b_o_aer(05,itype,ai_phase)  = p_opcg8_b_o_a05
            if (p_pcg1_f_c_a05 .ge. p1st) lptr_pcg1_f_c_aer(05,itype,ai_phase) = p_pcg1_f_c_a05
            if (p_pcg2_f_c_a05 .ge. p1st) lptr_pcg2_f_c_aer(05,itype,ai_phase)  = p_pcg2_f_c_a05
            if (p_pcg3_f_c_a05 .ge. p1st) lptr_pcg3_f_c_aer(05,itype,ai_phase)  = p_pcg3_f_c_a05
            if (p_pcg4_f_c_a05 .ge. p1st) lptr_pcg4_f_c_aer(05,itype,ai_phase)  = p_pcg4_f_c_a05
            if (p_pcg5_f_c_a05 .ge. p1st) lptr_pcg5_f_c_aer(05,itype,ai_phase)  = p_pcg5_f_c_a05
            if (p_pcg6_f_c_a05 .ge. p1st) lptr_pcg6_f_c_aer(05,itype,ai_phase)  = p_pcg6_f_c_a05
            if (p_pcg7_f_c_a05 .ge. p1st) lptr_pcg7_f_c_aer(05,itype,ai_phase)  = p_pcg7_f_c_a05
            if (p_pcg8_f_c_a05 .ge. p1st) lptr_pcg8_f_c_aer(05,itype,ai_phase)  = p_pcg8_f_c_a05
            if (p_pcg9_f_c_a05 .ge. p1st) lptr_pcg9_f_c_aer(05,itype,ai_phase)  = p_pcg9_f_c_a05
            if (p_pcg1_f_o_a05 .ge. p1st) lptr_pcg1_f_o_aer(05,itype,ai_phase) = p_pcg1_f_o_a05
            if (p_pcg2_f_o_a05 .ge. p1st) lptr_pcg2_f_o_aer(05,itype,ai_phase)  = p_pcg2_f_o_a05
            if (p_pcg3_f_o_a05 .ge. p1st) lptr_pcg3_f_o_aer(05,itype,ai_phase)  = p_pcg3_f_o_a05
            if (p_pcg4_f_o_a05 .ge. p1st) lptr_pcg4_f_o_aer(05,itype,ai_phase)  = p_pcg4_f_o_a05
            if (p_pcg5_f_o_a05 .ge. p1st) lptr_pcg5_f_o_aer(05,itype,ai_phase)  = p_pcg5_f_o_a05
            if (p_pcg6_f_o_a05 .ge. p1st) lptr_pcg6_f_o_aer(05,itype,ai_phase)  = p_pcg6_f_o_a05
            if (p_pcg7_f_o_a05 .ge. p1st) lptr_pcg7_f_o_aer(05,itype,ai_phase)  = p_pcg7_f_o_a05
            if (p_pcg8_f_o_a05 .ge. p1st) lptr_pcg8_f_o_aer(05,itype,ai_phase)  = p_pcg8_f_o_a05
            if (p_pcg9_f_o_a05 .ge. p1st) lptr_pcg9_f_o_aer(05,itype,ai_phase)  = p_pcg9_f_o_a05
            if (p_opcg1_f_c_a05 .ge. p1st) lptr_opcg1_f_c_aer(05,itype,ai_phase) = p_opcg1_f_c_a05
            if (p_opcg2_f_c_a05 .ge. p1st) lptr_opcg2_f_c_aer(05,itype,ai_phase)  = p_opcg2_f_c_a05
            if (p_opcg3_f_c_a05 .ge. p1st) lptr_opcg3_f_c_aer(05,itype,ai_phase)  = p_opcg3_f_c_a05
            if (p_opcg4_f_c_a05 .ge. p1st) lptr_opcg4_f_c_aer(05,itype,ai_phase)  = p_opcg4_f_c_a05
            if (p_opcg5_f_c_a05 .ge. p1st) lptr_opcg5_f_c_aer(05,itype,ai_phase)  = p_opcg5_f_c_a05
            if (p_opcg6_f_c_a05 .ge. p1st) lptr_opcg6_f_c_aer(05,itype,ai_phase)  = p_opcg6_f_c_a05
            if (p_opcg7_f_c_a05 .ge. p1st) lptr_opcg7_f_c_aer(05,itype,ai_phase)  = p_opcg7_f_c_a05
            if (p_opcg8_f_c_a05 .ge. p1st) lptr_opcg8_f_c_aer(05,itype,ai_phase)  = p_opcg8_f_c_a05
            if (p_opcg1_f_o_a05 .ge. p1st) lptr_opcg1_f_o_aer(05,itype,ai_phase) = p_opcg1_f_o_a05
            if (p_opcg2_f_o_a05 .ge. p1st) lptr_opcg2_f_o_aer(05,itype,ai_phase)  = p_opcg2_f_o_a05
            if (p_opcg3_f_o_a05 .ge. p1st) lptr_opcg3_f_o_aer(05,itype,ai_phase)  = p_opcg3_f_o_a05
            if (p_opcg4_f_o_a05 .ge. p1st) lptr_opcg4_f_o_aer(05,itype,ai_phase)  = p_opcg4_f_o_a05
            if (p_opcg5_f_o_a05 .ge. p1st) lptr_opcg5_f_o_aer(05,itype,ai_phase)  = p_opcg5_f_o_a05
            if (p_opcg6_f_o_a05 .ge. p1st) lptr_opcg6_f_o_aer(05,itype,ai_phase)  = p_opcg6_f_o_a05
            if (p_opcg7_f_o_a05 .ge. p1st) lptr_opcg7_f_o_aer(05,itype,ai_phase)  = p_opcg7_f_o_a05
            if (p_opcg8_f_o_a05 .ge. p1st) lptr_opcg8_f_o_aer(05,itype,ai_phase)  = p_opcg8_f_o_a05
            if (p_ant1_c_a05 .ge. p1st) lptr_ant1_c_aer(05,itype,ai_phase)  = p_ant1_c_a05
            if (p_ant2_c_a05 .ge. p1st) lptr_ant2_c_aer(05,itype,ai_phase)  = p_ant2_c_a05
            if (p_ant3_c_a05 .ge. p1st) lptr_ant3_c_aer(05,itype,ai_phase)  = p_ant3_c_a05
            if (p_ant4_c_a05 .ge. p1st) lptr_ant4_c_aer(05,itype,ai_phase)  = p_ant4_c_a05
            if (p_biog1_c_a05 .ge. p1st) lptr_biog1_c_aer(05,itype,ai_phase)  = p_biog1_c_a05
            if (p_biog2_c_a05 .ge. p1st) lptr_biog2_c_aer(05,itype,ai_phase)  = p_biog2_c_a05
            if (p_biog3_c_a05 .ge. p1st) lptr_biog3_c_aer(05,itype,ai_phase)  = p_biog3_c_a05
            if (p_biog4_c_a05 .ge. p1st) lptr_biog4_c_aer(05,itype,ai_phase)  = p_biog4_c_a05
            if (p_ant1_o_a05 .ge. p1st) lptr_ant1_o_aer(05,itype,ai_phase)  = p_ant1_o_a05
            if (p_ant2_o_a05 .ge. p1st) lptr_ant2_o_aer(05,itype,ai_phase)  = p_ant2_o_a05
            if (p_ant3_o_a05 .ge. p1st) lptr_ant3_o_aer(05,itype,ai_phase)  = p_ant3_o_a05
            if (p_ant4_o_a05 .ge. p1st) lptr_ant4_o_aer(05,itype,ai_phase)  = p_ant4_o_a05
            if (p_biog1_o_a05 .ge. p1st) lptr_biog1_o_aer(05,itype,ai_phase)  = p_biog1_o_a05
            if (p_biog2_o_a05 .ge. p1st) lptr_biog2_o_aer(05,itype,ai_phase)  = p_biog2_o_a05
            if (p_biog3_o_a05 .ge. p1st) lptr_biog3_o_aer(05,itype,ai_phase)  = p_biog3_o_a05
            if (p_biog4_o_a05 .ge. p1st) lptr_biog4_o_aer(05,itype,ai_phase)  = p_biog4_o_a05

	    numptr_aer(05,itype,ai_phase)        = p_num_a05
	end if

	if (nsize_aer(itype) .ge. 6) then
	    lptr_so4_aer(06,itype,ai_phase)      = p_so4_a06
	    lptr_no3_aer(06,itype,ai_phase)      = p_no3_a06
	    lptr_cl_aer(06,itype,ai_phase)       = p_cl_a06
	    lptr_msa_aer(06,itype,ai_phase)      = p_msa_a06
	    lptr_co3_aer(06,itype,ai_phase)      = p_co3_a06
	    lptr_nh4_aer(06,itype,ai_phase)      = p_nh4_a06
	    lptr_na_aer(06,itype,ai_phase)       = p_na_a06
	    lptr_ca_aer(06,itype,ai_phase)       = p_ca_a06
	    lptr_oin_aer(06,itype,ai_phase)      = p_oin_a06
	    lptr_oc_aer(06,itype,ai_phase)       = p_oc_a06
	    lptr_bc_aer(06,itype,ai_phase)       = p_bc_a06
	    hyswptr_aer(06,itype)                = p_hysw_a06
	    waterptr_aer(06,itype)               = p_water_a06

            if (p_pcg1_b_c_a06 .ge. p1st) lptr_pcg1_b_c_aer(06,itype,ai_phase) = p_pcg1_b_c_a06
            if (p_pcg2_b_c_a06 .ge. p1st) lptr_pcg2_b_c_aer(06,itype,ai_phase)  = p_pcg2_b_c_a06
            if (p_pcg3_b_c_a06 .ge. p1st) lptr_pcg3_b_c_aer(06,itype,ai_phase)  = p_pcg3_b_c_a06
            if (p_pcg4_b_c_a06 .ge. p1st) lptr_pcg4_b_c_aer(06,itype,ai_phase)  = p_pcg4_b_c_a06
            if (p_pcg5_b_c_a06 .ge. p1st) lptr_pcg5_b_c_aer(06,itype,ai_phase)  = p_pcg5_b_c_a06
            if (p_pcg6_b_c_a06 .ge. p1st) lptr_pcg6_b_c_aer(06,itype,ai_phase)  = p_pcg6_b_c_a06
            if (p_pcg7_b_c_a06 .ge. p1st) lptr_pcg7_b_c_aer(06,itype,ai_phase)  = p_pcg7_b_c_a06
            if (p_pcg8_b_c_a06 .ge. p1st) lptr_pcg8_b_c_aer(06,itype,ai_phase)  = p_pcg8_b_c_a06
            if (p_pcg9_b_c_a06 .ge. p1st) lptr_pcg9_b_c_aer(06,itype,ai_phase)  = p_pcg9_b_c_a06
            if (p_pcg1_b_o_a06 .ge. p1st) lptr_pcg1_b_o_aer(06,itype,ai_phase) = p_pcg1_b_o_a06
            if (p_pcg2_b_o_a06 .ge. p1st) lptr_pcg2_b_o_aer(06,itype,ai_phase)  = p_pcg2_b_o_a06
            if (p_pcg3_b_o_a06 .ge. p1st) lptr_pcg3_b_o_aer(06,itype,ai_phase)  = p_pcg3_b_o_a06
            if (p_pcg4_b_o_a06 .ge. p1st) lptr_pcg4_b_o_aer(06,itype,ai_phase)  = p_pcg4_b_o_a06
            if (p_pcg5_b_o_a06 .ge. p1st) lptr_pcg5_b_o_aer(06,itype,ai_phase)  = p_pcg5_b_o_a06
            if (p_pcg6_b_o_a06 .ge. p1st) lptr_pcg6_b_o_aer(06,itype,ai_phase)  = p_pcg6_b_o_a06
            if (p_pcg7_b_o_a06 .ge. p1st) lptr_pcg7_b_o_aer(06,itype,ai_phase)  = p_pcg7_b_o_a06
            if (p_pcg8_b_o_a06 .ge. p1st) lptr_pcg8_b_o_aer(06,itype,ai_phase)  = p_pcg8_b_o_a06
            if (p_pcg9_b_o_a06 .ge. p1st) lptr_pcg9_b_o_aer(06,itype,ai_phase)  = p_pcg9_b_o_a06
            if (p_opcg1_b_c_a06 .ge. p1st) lptr_opcg1_b_c_aer(06,itype,ai_phase) = p_opcg1_b_c_a06
            if (p_opcg2_b_c_a06 .ge. p1st) lptr_opcg2_b_c_aer(06,itype,ai_phase)  = p_opcg2_b_c_a06
            if (p_opcg3_b_c_a06 .ge. p1st) lptr_opcg3_b_c_aer(06,itype,ai_phase)  = p_opcg3_b_c_a06
            if (p_opcg4_b_c_a06 .ge. p1st) lptr_opcg4_b_c_aer(06,itype,ai_phase)  = p_opcg4_b_c_a06
            if (p_opcg5_b_c_a06 .ge. p1st) lptr_opcg5_b_c_aer(06,itype,ai_phase)  = p_opcg5_b_c_a06
            if (p_opcg6_b_c_a06 .ge. p1st) lptr_opcg6_b_c_aer(06,itype,ai_phase)  = p_opcg6_b_c_a06
            if (p_opcg7_b_c_a06 .ge. p1st) lptr_opcg7_b_c_aer(06,itype,ai_phase)  = p_opcg7_b_c_a06
            if (p_opcg8_b_c_a06 .ge. p1st) lptr_opcg8_b_c_aer(06,itype,ai_phase)  = p_opcg8_b_c_a06
            if (p_opcg1_b_o_a06 .ge. p1st) lptr_opcg1_b_o_aer(06,itype,ai_phase) = p_opcg1_b_o_a06
            if (p_opcg2_b_o_a06 .ge. p1st) lptr_opcg2_b_o_aer(06,itype,ai_phase)  = p_opcg2_b_o_a06
            if (p_opcg3_b_o_a06 .ge. p1st) lptr_opcg3_b_o_aer(06,itype,ai_phase)  = p_opcg3_b_o_a06
            if (p_opcg4_b_o_a06 .ge. p1st) lptr_opcg4_b_o_aer(06,itype,ai_phase)  = p_opcg4_b_o_a06
            if (p_opcg5_b_o_a06 .ge. p1st) lptr_opcg5_b_o_aer(06,itype,ai_phase)  = p_opcg5_b_o_a06
            if (p_opcg6_b_o_a06 .ge. p1st) lptr_opcg6_b_o_aer(06,itype,ai_phase)  = p_opcg6_b_o_a06
            if (p_opcg7_b_o_a06 .ge. p1st) lptr_opcg7_b_o_aer(06,itype,ai_phase)  = p_opcg7_b_o_a06
            if (p_opcg8_b_o_a06 .ge. p1st) lptr_opcg8_b_o_aer(06,itype,ai_phase)  = p_opcg8_b_o_a06
            if (p_pcg1_f_c_a06 .ge. p1st) lptr_pcg1_f_c_aer(06,itype,ai_phase) = p_pcg1_f_c_a06
            if (p_pcg2_f_c_a06 .ge. p1st) lptr_pcg2_f_c_aer(06,itype,ai_phase)  = p_pcg2_f_c_a06
            if (p_pcg3_f_c_a06 .ge. p1st) lptr_pcg3_f_c_aer(06,itype,ai_phase)  = p_pcg3_f_c_a06
            if (p_pcg4_f_c_a06 .ge. p1st) lptr_pcg4_f_c_aer(06,itype,ai_phase)  = p_pcg4_f_c_a06
            if (p_pcg5_f_c_a06 .ge. p1st) lptr_pcg5_f_c_aer(06,itype,ai_phase)  = p_pcg5_f_c_a06
            if (p_pcg6_f_c_a06 .ge. p1st) lptr_pcg6_f_c_aer(06,itype,ai_phase)  = p_pcg6_f_c_a06
            if (p_pcg7_f_c_a06 .ge. p1st) lptr_pcg7_f_c_aer(06,itype,ai_phase)  = p_pcg7_f_c_a06
            if (p_pcg8_f_c_a06 .ge. p1st) lptr_pcg8_f_c_aer(06,itype,ai_phase)  = p_pcg8_f_c_a06
            if (p_pcg9_f_c_a06 .ge. p1st) lptr_pcg9_f_c_aer(06,itype,ai_phase)  = p_pcg9_f_c_a06
            if (p_pcg1_f_o_a06 .ge. p1st) lptr_pcg1_f_o_aer(06,itype,ai_phase) = p_pcg1_f_o_a06
            if (p_pcg2_f_o_a06 .ge. p1st) lptr_pcg2_f_o_aer(06,itype,ai_phase)  = p_pcg2_f_o_a06
            if (p_pcg3_f_o_a06 .ge. p1st) lptr_pcg3_f_o_aer(06,itype,ai_phase)  = p_pcg3_f_o_a06
            if (p_pcg4_f_o_a06 .ge. p1st) lptr_pcg4_f_o_aer(06,itype,ai_phase)  = p_pcg4_f_o_a06
            if (p_pcg5_f_o_a06 .ge. p1st) lptr_pcg5_f_o_aer(06,itype,ai_phase)  = p_pcg5_f_o_a06
            if (p_pcg6_f_o_a06 .ge. p1st) lptr_pcg6_f_o_aer(06,itype,ai_phase)  = p_pcg6_f_o_a06
            if (p_pcg7_f_o_a06 .ge. p1st) lptr_pcg7_f_o_aer(06,itype,ai_phase)  = p_pcg7_f_o_a06
            if (p_pcg8_f_o_a06 .ge. p1st) lptr_pcg8_f_o_aer(06,itype,ai_phase)  = p_pcg8_f_o_a06
            if (p_pcg9_f_o_a06 .ge. p1st) lptr_pcg9_f_o_aer(06,itype,ai_phase)  = p_pcg9_f_o_a06
            if (p_opcg1_f_c_a06 .ge. p1st) lptr_opcg1_f_c_aer(06,itype,ai_phase) = p_opcg1_f_c_a06
            if (p_opcg2_f_c_a06 .ge. p1st) lptr_opcg2_f_c_aer(06,itype,ai_phase)  = p_opcg2_f_c_a06
            if (p_opcg3_f_c_a06 .ge. p1st) lptr_opcg3_f_c_aer(06,itype,ai_phase)  = p_opcg3_f_c_a06
            if (p_opcg4_f_c_a06 .ge. p1st) lptr_opcg4_f_c_aer(06,itype,ai_phase)  = p_opcg4_f_c_a06
            if (p_opcg5_f_c_a06 .ge. p1st) lptr_opcg5_f_c_aer(06,itype,ai_phase)  = p_opcg5_f_c_a06
            if (p_opcg6_f_c_a06 .ge. p1st) lptr_opcg6_f_c_aer(06,itype,ai_phase)  = p_opcg6_f_c_a06
            if (p_opcg7_f_c_a06 .ge. p1st) lptr_opcg7_f_c_aer(06,itype,ai_phase)  = p_opcg7_f_c_a06
            if (p_opcg8_f_c_a06 .ge. p1st) lptr_opcg8_f_c_aer(06,itype,ai_phase)  = p_opcg8_f_c_a06
            if (p_opcg1_f_o_a06 .ge. p1st) lptr_opcg1_f_o_aer(06,itype,ai_phase) = p_opcg1_f_o_a06
            if (p_opcg2_f_o_a06 .ge. p1st) lptr_opcg2_f_o_aer(06,itype,ai_phase)  = p_opcg2_f_o_a06
            if (p_opcg3_f_o_a06 .ge. p1st) lptr_opcg3_f_o_aer(06,itype,ai_phase)  = p_opcg3_f_o_a06
            if (p_opcg4_f_o_a06 .ge. p1st) lptr_opcg4_f_o_aer(06,itype,ai_phase)  = p_opcg4_f_o_a06
            if (p_opcg5_f_o_a06 .ge. p1st) lptr_opcg5_f_o_aer(06,itype,ai_phase)  = p_opcg5_f_o_a06
            if (p_opcg6_f_o_a06 .ge. p1st) lptr_opcg6_f_o_aer(06,itype,ai_phase)  = p_opcg6_f_o_a06
            if (p_opcg7_f_o_a06 .ge. p1st) lptr_opcg7_f_o_aer(06,itype,ai_phase)  = p_opcg7_f_o_a06
            if (p_opcg8_f_o_a06 .ge. p1st) lptr_opcg8_f_o_aer(06,itype,ai_phase)  = p_opcg8_f_o_a06
            if (p_ant1_c_a06 .ge. p1st) lptr_ant1_c_aer(06,itype,ai_phase)  = p_ant1_c_a06
            if (p_ant2_c_a06 .ge. p1st) lptr_ant2_c_aer(06,itype,ai_phase)  = p_ant2_c_a06
            if (p_ant3_c_a06 .ge. p1st) lptr_ant3_c_aer(06,itype,ai_phase)  = p_ant3_c_a06
            if (p_ant4_c_a06 .ge. p1st) lptr_ant4_c_aer(06,itype,ai_phase)  = p_ant4_c_a06
            if (p_biog1_c_a06 .ge. p1st) lptr_biog1_c_aer(06,itype,ai_phase)  = p_biog1_c_a06
            if (p_biog2_c_a06 .ge. p1st) lptr_biog2_c_aer(06,itype,ai_phase)  = p_biog2_c_a06
            if (p_biog3_c_a06 .ge. p1st) lptr_biog3_c_aer(06,itype,ai_phase)  = p_biog3_c_a06
            if (p_biog4_c_a06 .ge. p1st) lptr_biog4_c_aer(06,itype,ai_phase)  = p_biog4_c_a06
            if (p_ant1_o_a06 .ge. p1st) lptr_ant1_o_aer(06,itype,ai_phase)  = p_ant1_o_a06
            if (p_ant2_o_a06 .ge. p1st) lptr_ant2_o_aer(06,itype,ai_phase)  = p_ant2_o_a06
            if (p_ant3_o_a06 .ge. p1st) lptr_ant3_o_aer(06,itype,ai_phase)  = p_ant3_o_a06
            if (p_ant4_o_a06 .ge. p1st) lptr_ant4_o_aer(06,itype,ai_phase)  = p_ant4_o_a06
            if (p_biog1_o_a06 .ge. p1st) lptr_biog1_o_aer(06,itype,ai_phase)  = p_biog1_o_a06
            if (p_biog2_o_a06 .ge. p1st) lptr_biog2_o_aer(06,itype,ai_phase)  = p_biog2_o_a06
            if (p_biog3_o_a06 .ge. p1st) lptr_biog3_o_aer(06,itype,ai_phase)  = p_biog3_o_a06
            if (p_biog4_o_a06 .ge. p1st) lptr_biog4_o_aer(06,itype,ai_phase)  = p_biog4_o_a06

	    numptr_aer(06,itype,ai_phase)        = p_num_a06
	end if

	if (nsize_aer(itype) .ge. 7) then
	    lptr_so4_aer(07,itype,ai_phase)      = p_so4_a07
	    lptr_no3_aer(07,itype,ai_phase)      = p_no3_a07
	    lptr_cl_aer(07,itype,ai_phase)       = p_cl_a07
	    lptr_msa_aer(07,itype,ai_phase)      = p_msa_a07
	    lptr_co3_aer(07,itype,ai_phase)      = p_co3_a07
	    lptr_nh4_aer(07,itype,ai_phase)      = p_nh4_a07
	    lptr_na_aer(07,itype,ai_phase)       = p_na_a07
	    lptr_ca_aer(07,itype,ai_phase)       = p_ca_a07
	    lptr_oin_aer(07,itype,ai_phase)      = p_oin_a07
	    lptr_oc_aer(07,itype,ai_phase)       = p_oc_a07
	    lptr_bc_aer(07,itype,ai_phase)       = p_bc_a07
	    hyswptr_aer(07,itype)                = p_hysw_a07
	    waterptr_aer(07,itype)               = p_water_a07

            if (p_pcg1_b_c_a07 .ge. p1st) lptr_pcg1_b_c_aer(07,itype,ai_phase) = p_pcg1_b_c_a07
            if (p_pcg2_b_c_a07 .ge. p1st) lptr_pcg2_b_c_aer(07,itype,ai_phase)  = p_pcg2_b_c_a07
            if (p_pcg3_b_c_a07 .ge. p1st) lptr_pcg3_b_c_aer(07,itype,ai_phase)  = p_pcg3_b_c_a07
            if (p_pcg4_b_c_a07 .ge. p1st) lptr_pcg4_b_c_aer(07,itype,ai_phase)  = p_pcg4_b_c_a07
            if (p_pcg5_b_c_a07 .ge. p1st) lptr_pcg5_b_c_aer(07,itype,ai_phase)  = p_pcg5_b_c_a07
            if (p_pcg6_b_c_a07 .ge. p1st) lptr_pcg6_b_c_aer(07,itype,ai_phase)  = p_pcg6_b_c_a07
            if (p_pcg7_b_c_a07 .ge. p1st) lptr_pcg7_b_c_aer(07,itype,ai_phase)  = p_pcg7_b_c_a07
            if (p_pcg8_b_c_a07 .ge. p1st) lptr_pcg8_b_c_aer(07,itype,ai_phase)  = p_pcg8_b_c_a07
            if (p_pcg9_b_c_a07 .ge. p1st) lptr_pcg9_b_c_aer(07,itype,ai_phase)  = p_pcg9_b_c_a07
            if (p_pcg1_b_o_a07 .ge. p1st) lptr_pcg1_b_o_aer(07,itype,ai_phase) = p_pcg1_b_o_a07
            if (p_pcg2_b_o_a07 .ge. p1st) lptr_pcg2_b_o_aer(07,itype,ai_phase)  = p_pcg2_b_o_a07
            if (p_pcg3_b_o_a07 .ge. p1st) lptr_pcg3_b_o_aer(07,itype,ai_phase)  = p_pcg3_b_o_a07
            if (p_pcg4_b_o_a07 .ge. p1st) lptr_pcg4_b_o_aer(07,itype,ai_phase)  = p_pcg4_b_o_a07
            if (p_pcg5_b_o_a07 .ge. p1st) lptr_pcg5_b_o_aer(07,itype,ai_phase)  = p_pcg5_b_o_a07
            if (p_pcg6_b_o_a07 .ge. p1st) lptr_pcg6_b_o_aer(07,itype,ai_phase)  = p_pcg6_b_o_a07
            if (p_pcg7_b_o_a07 .ge. p1st) lptr_pcg7_b_o_aer(07,itype,ai_phase)  = p_pcg7_b_o_a07
            if (p_pcg8_b_o_a07 .ge. p1st) lptr_pcg8_b_o_aer(07,itype,ai_phase)  = p_pcg8_b_o_a07
            if (p_pcg9_b_o_a07 .ge. p1st) lptr_pcg9_b_o_aer(07,itype,ai_phase)  = p_pcg9_b_o_a07
            if (p_opcg1_b_c_a07 .ge. p1st) lptr_opcg1_b_c_aer(07,itype,ai_phase) = p_opcg1_b_c_a07
            if (p_opcg2_b_c_a07 .ge. p1st) lptr_opcg2_b_c_aer(07,itype,ai_phase)  = p_opcg2_b_c_a07
            if (p_opcg3_b_c_a07 .ge. p1st) lptr_opcg3_b_c_aer(07,itype,ai_phase)  = p_opcg3_b_c_a07
            if (p_opcg4_b_c_a07 .ge. p1st) lptr_opcg4_b_c_aer(07,itype,ai_phase)  = p_opcg4_b_c_a07
            if (p_opcg5_b_c_a07 .ge. p1st) lptr_opcg5_b_c_aer(07,itype,ai_phase)  = p_opcg5_b_c_a07
            if (p_opcg6_b_c_a07 .ge. p1st) lptr_opcg6_b_c_aer(07,itype,ai_phase)  = p_opcg6_b_c_a07
            if (p_opcg7_b_c_a07 .ge. p1st) lptr_opcg7_b_c_aer(07,itype,ai_phase)  = p_opcg7_b_c_a07
            if (p_opcg8_b_c_a07 .ge. p1st) lptr_opcg8_b_c_aer(07,itype,ai_phase)  = p_opcg8_b_c_a07
            if (p_opcg1_b_o_a07 .ge. p1st) lptr_opcg1_b_o_aer(07,itype,ai_phase) = p_opcg1_b_o_a07
            if (p_opcg2_b_o_a07 .ge. p1st) lptr_opcg2_b_o_aer(07,itype,ai_phase)  = p_opcg2_b_o_a07
            if (p_opcg3_b_o_a07 .ge. p1st) lptr_opcg3_b_o_aer(07,itype,ai_phase)  = p_opcg3_b_o_a07
            if (p_opcg4_b_o_a07 .ge. p1st) lptr_opcg4_b_o_aer(07,itype,ai_phase)  = p_opcg4_b_o_a07
            if (p_opcg5_b_o_a07 .ge. p1st) lptr_opcg5_b_o_aer(07,itype,ai_phase)  = p_opcg5_b_o_a07
            if (p_opcg6_b_o_a07 .ge. p1st) lptr_opcg6_b_o_aer(07,itype,ai_phase)  = p_opcg6_b_o_a07
            if (p_opcg7_b_o_a07 .ge. p1st) lptr_opcg7_b_o_aer(07,itype,ai_phase)  = p_opcg7_b_o_a07
            if (p_opcg8_b_o_a07 .ge. p1st) lptr_opcg8_b_o_aer(07,itype,ai_phase)  = p_opcg8_b_o_a07
            if (p_pcg1_f_c_a07 .ge. p1st) lptr_pcg1_f_c_aer(07,itype,ai_phase) = p_pcg1_f_c_a07
            if (p_pcg2_f_c_a07 .ge. p1st) lptr_pcg2_f_c_aer(07,itype,ai_phase)  = p_pcg2_f_c_a07
            if (p_pcg3_f_c_a07 .ge. p1st) lptr_pcg3_f_c_aer(07,itype,ai_phase)  = p_pcg3_f_c_a07
            if (p_pcg4_f_c_a07 .ge. p1st) lptr_pcg4_f_c_aer(07,itype,ai_phase)  = p_pcg4_f_c_a07
            if (p_pcg5_f_c_a07 .ge. p1st) lptr_pcg5_f_c_aer(07,itype,ai_phase)  = p_pcg5_f_c_a07
            if (p_pcg6_f_c_a07 .ge. p1st) lptr_pcg6_f_c_aer(07,itype,ai_phase)  = p_pcg6_f_c_a07
            if (p_pcg7_f_c_a07 .ge. p1st) lptr_pcg7_f_c_aer(07,itype,ai_phase)  = p_pcg7_f_c_a07
            if (p_pcg8_f_c_a07 .ge. p1st) lptr_pcg8_f_c_aer(07,itype,ai_phase)  = p_pcg8_f_c_a07
            if (p_pcg9_f_c_a07 .ge. p1st) lptr_pcg9_f_c_aer(07,itype,ai_phase)  = p_pcg9_f_c_a07
            if (p_pcg1_f_o_a07 .ge. p1st) lptr_pcg1_f_o_aer(07,itype,ai_phase) = p_pcg1_f_o_a07
            if (p_pcg2_f_o_a07 .ge. p1st) lptr_pcg2_f_o_aer(07,itype,ai_phase)  = p_pcg2_f_o_a07
            if (p_pcg3_f_o_a07 .ge. p1st) lptr_pcg3_f_o_aer(07,itype,ai_phase)  = p_pcg3_f_o_a07
            if (p_pcg4_f_o_a07 .ge. p1st) lptr_pcg4_f_o_aer(07,itype,ai_phase)  = p_pcg4_f_o_a07
            if (p_pcg5_f_o_a07 .ge. p1st) lptr_pcg5_f_o_aer(07,itype,ai_phase)  = p_pcg5_f_o_a07
            if (p_pcg6_f_o_a07 .ge. p1st) lptr_pcg6_f_o_aer(07,itype,ai_phase)  = p_pcg6_f_o_a07
            if (p_pcg7_f_o_a07 .ge. p1st) lptr_pcg7_f_o_aer(07,itype,ai_phase)  = p_pcg7_f_o_a07
            if (p_pcg8_f_o_a07 .ge. p1st) lptr_pcg8_f_o_aer(07,itype,ai_phase)  = p_pcg8_f_o_a07
            if (p_pcg9_f_o_a07 .ge. p1st) lptr_pcg9_f_o_aer(07,itype,ai_phase)  = p_pcg9_f_o_a07
            if (p_opcg1_f_c_a07 .ge. p1st) lptr_opcg1_f_c_aer(07,itype,ai_phase) = p_opcg1_f_c_a07
            if (p_opcg2_f_c_a07 .ge. p1st) lptr_opcg2_f_c_aer(07,itype,ai_phase)  = p_opcg2_f_c_a07
            if (p_opcg3_f_c_a07 .ge. p1st) lptr_opcg3_f_c_aer(07,itype,ai_phase)  = p_opcg3_f_c_a07
            if (p_opcg4_f_c_a07 .ge. p1st) lptr_opcg4_f_c_aer(07,itype,ai_phase)  = p_opcg4_f_c_a07
            if (p_opcg5_f_c_a07 .ge. p1st) lptr_opcg5_f_c_aer(07,itype,ai_phase)  = p_opcg5_f_c_a07
            if (p_opcg6_f_c_a07 .ge. p1st) lptr_opcg6_f_c_aer(07,itype,ai_phase)  = p_opcg6_f_c_a07
            if (p_opcg7_f_c_a07 .ge. p1st) lptr_opcg7_f_c_aer(07,itype,ai_phase)  = p_opcg7_f_c_a07
            if (p_opcg8_f_c_a07 .ge. p1st) lptr_opcg8_f_c_aer(07,itype,ai_phase)  = p_opcg8_f_c_a07
            if (p_opcg1_f_o_a07 .ge. p1st) lptr_opcg1_f_o_aer(07,itype,ai_phase) = p_opcg1_f_o_a07
            if (p_opcg2_f_o_a07 .ge. p1st) lptr_opcg2_f_o_aer(07,itype,ai_phase)  = p_opcg2_f_o_a07
            if (p_opcg3_f_o_a07 .ge. p1st) lptr_opcg3_f_o_aer(07,itype,ai_phase)  = p_opcg3_f_o_a07
            if (p_opcg4_f_o_a07 .ge. p1st) lptr_opcg4_f_o_aer(07,itype,ai_phase)  = p_opcg4_f_o_a07
            if (p_opcg5_f_o_a07 .ge. p1st) lptr_opcg5_f_o_aer(07,itype,ai_phase)  = p_opcg5_f_o_a07
            if (p_opcg6_f_o_a07 .ge. p1st) lptr_opcg6_f_o_aer(07,itype,ai_phase)  = p_opcg6_f_o_a07
            if (p_opcg7_f_o_a07 .ge. p1st) lptr_opcg7_f_o_aer(07,itype,ai_phase)  = p_opcg7_f_o_a07
            if (p_opcg8_f_o_a07 .ge. p1st) lptr_opcg8_f_o_aer(07,itype,ai_phase)  = p_opcg8_f_o_a07
            if (p_ant1_c_a07 .ge. p1st) lptr_ant1_c_aer(07,itype,ai_phase)  = p_ant1_c_a07
            if (p_ant2_c_a07 .ge. p1st) lptr_ant2_c_aer(07,itype,ai_phase)  = p_ant2_c_a07
            if (p_ant3_c_a07 .ge. p1st) lptr_ant3_c_aer(07,itype,ai_phase)  = p_ant3_c_a07
            if (p_ant4_c_a07 .ge. p1st) lptr_ant4_c_aer(07,itype,ai_phase)  = p_ant4_c_a07
            if (p_biog1_c_a07 .ge. p1st) lptr_biog1_c_aer(07,itype,ai_phase)  = p_biog1_c_a07
            if (p_biog2_c_a07 .ge. p1st) lptr_biog2_c_aer(07,itype,ai_phase)  = p_biog2_c_a07
            if (p_biog3_c_a07 .ge. p1st) lptr_biog3_c_aer(07,itype,ai_phase)  = p_biog3_c_a07
            if (p_biog4_c_a07 .ge. p1st) lptr_biog4_c_aer(07,itype,ai_phase)  = p_biog4_c_a07
            if (p_ant1_o_a07 .ge. p1st) lptr_ant1_o_aer(07,itype,ai_phase)  = p_ant1_o_a07
            if (p_ant2_o_a07 .ge. p1st) lptr_ant2_o_aer(07,itype,ai_phase)  = p_ant2_o_a07
            if (p_ant3_o_a07 .ge. p1st) lptr_ant3_o_aer(07,itype,ai_phase)  = p_ant3_o_a07
            if (p_ant4_o_a07 .ge. p1st) lptr_ant4_o_aer(07,itype,ai_phase)  = p_ant4_o_a07
            if (p_biog1_o_a07 .ge. p1st) lptr_biog1_o_aer(07,itype,ai_phase)  = p_biog1_o_a07
            if (p_biog2_o_a07 .ge. p1st) lptr_biog2_o_aer(07,itype,ai_phase)  = p_biog2_o_a07
            if (p_biog3_o_a07 .ge. p1st) lptr_biog3_o_aer(07,itype,ai_phase)  = p_biog3_o_a07
            if (p_biog4_o_a07 .ge. p1st) lptr_biog4_o_aer(07,itype,ai_phase)  = p_biog4_o_a07

	    numptr_aer(07,itype,ai_phase)        = p_num_a07
         end if

	if (nsize_aer(itype) .ge. 8) then
	    lptr_so4_aer(08,itype,ai_phase)      = p_so4_a08
	    lptr_no3_aer(08,itype,ai_phase)      = p_no3_a08
	    lptr_cl_aer(08,itype,ai_phase)       = p_cl_a08
	    lptr_msa_aer(08,itype,ai_phase)      = p_msa_a08
	    lptr_co3_aer(08,itype,ai_phase)      = p_co3_a08
	    lptr_nh4_aer(08,itype,ai_phase)      = p_nh4_a08
	    lptr_na_aer(08,itype,ai_phase)       = p_na_a08
	    lptr_ca_aer(08,itype,ai_phase)       = p_ca_a08
	    lptr_oin_aer(08,itype,ai_phase)      = p_oin_a08
	    lptr_oc_aer(08,itype,ai_phase)       = p_oc_a08
	    lptr_bc_aer(08,itype,ai_phase)       = p_bc_a08
	    hyswptr_aer(08,itype)                = p_hysw_a08
	    waterptr_aer(08,itype)               = p_water_a08

            if (p_pcg1_b_c_a08 .ge. p1st) lptr_pcg1_b_c_aer(08,itype,ai_phase) = p_pcg1_b_c_a08
            if (p_pcg2_b_c_a08 .ge. p1st) lptr_pcg2_b_c_aer(08,itype,ai_phase)  = p_pcg2_b_c_a08
            if (p_pcg3_b_c_a08 .ge. p1st) lptr_pcg3_b_c_aer(08,itype,ai_phase)  = p_pcg3_b_c_a08
            if (p_pcg4_b_c_a08 .ge. p1st) lptr_pcg4_b_c_aer(08,itype,ai_phase)  = p_pcg4_b_c_a08
            if (p_pcg5_b_c_a08 .ge. p1st) lptr_pcg5_b_c_aer(08,itype,ai_phase)  = p_pcg5_b_c_a08
            if (p_pcg6_b_c_a08 .ge. p1st) lptr_pcg6_b_c_aer(08,itype,ai_phase)  = p_pcg6_b_c_a08
            if (p_pcg7_b_c_a08 .ge. p1st) lptr_pcg7_b_c_aer(08,itype,ai_phase)  = p_pcg7_b_c_a08
            if (p_pcg8_b_c_a08 .ge. p1st) lptr_pcg8_b_c_aer(08,itype,ai_phase)  = p_pcg8_b_c_a08
            if (p_pcg9_b_c_a08 .ge. p1st) lptr_pcg9_b_c_aer(08,itype,ai_phase)  = p_pcg9_b_c_a08
            if (p_pcg1_b_o_a08 .ge. p1st) lptr_pcg1_b_o_aer(08,itype,ai_phase) = p_pcg1_b_o_a08
            if (p_pcg2_b_o_a08 .ge. p1st) lptr_pcg2_b_o_aer(08,itype,ai_phase)  = p_pcg2_b_o_a08
            if (p_pcg3_b_o_a08 .ge. p1st) lptr_pcg3_b_o_aer(08,itype,ai_phase)  = p_pcg3_b_o_a08
            if (p_pcg4_b_o_a08 .ge. p1st) lptr_pcg4_b_o_aer(08,itype,ai_phase)  = p_pcg4_b_o_a08
            if (p_pcg5_b_o_a08 .ge. p1st) lptr_pcg5_b_o_aer(08,itype,ai_phase)  = p_pcg5_b_o_a08
            if (p_pcg6_b_o_a08 .ge. p1st) lptr_pcg6_b_o_aer(08,itype,ai_phase)  = p_pcg6_b_o_a08
            if (p_pcg7_b_o_a08 .ge. p1st) lptr_pcg7_b_o_aer(08,itype,ai_phase)  = p_pcg7_b_o_a08
            if (p_pcg8_b_o_a08 .ge. p1st) lptr_pcg8_b_o_aer(08,itype,ai_phase)  = p_pcg8_b_o_a08
            if (p_pcg9_b_o_a08 .ge. p1st) lptr_pcg9_b_o_aer(08,itype,ai_phase)  = p_pcg9_b_o_a08
            if (p_opcg1_b_c_a08 .ge. p1st) lptr_opcg1_b_c_aer(08,itype,ai_phase) = p_opcg1_b_c_a08
            if (p_opcg2_b_c_a08 .ge. p1st) lptr_opcg2_b_c_aer(08,itype,ai_phase)  = p_opcg2_b_c_a08
            if (p_opcg3_b_c_a08 .ge. p1st) lptr_opcg3_b_c_aer(08,itype,ai_phase)  = p_opcg3_b_c_a08
            if (p_opcg4_b_c_a08 .ge. p1st) lptr_opcg4_b_c_aer(08,itype,ai_phase)  = p_opcg4_b_c_a08
            if (p_opcg5_b_c_a08 .ge. p1st) lptr_opcg5_b_c_aer(08,itype,ai_phase)  = p_opcg5_b_c_a08
            if (p_opcg6_b_c_a08 .ge. p1st) lptr_opcg6_b_c_aer(08,itype,ai_phase)  = p_opcg6_b_c_a08
            if (p_opcg7_b_c_a08 .ge. p1st) lptr_opcg7_b_c_aer(08,itype,ai_phase)  = p_opcg7_b_c_a08
            if (p_opcg8_b_c_a08 .ge. p1st) lptr_opcg8_b_c_aer(08,itype,ai_phase)  = p_opcg8_b_c_a08
            if (p_opcg1_b_o_a08 .ge. p1st) lptr_opcg1_b_o_aer(08,itype,ai_phase) = p_opcg1_b_o_a08
            if (p_opcg2_b_o_a08 .ge. p1st) lptr_opcg2_b_o_aer(08,itype,ai_phase)  = p_opcg2_b_o_a08
            if (p_opcg3_b_o_a08 .ge. p1st) lptr_opcg3_b_o_aer(08,itype,ai_phase)  = p_opcg3_b_o_a08
            if (p_opcg4_b_o_a08 .ge. p1st) lptr_opcg4_b_o_aer(08,itype,ai_phase)  = p_opcg4_b_o_a08
            if (p_opcg5_b_o_a08 .ge. p1st) lptr_opcg5_b_o_aer(08,itype,ai_phase)  = p_opcg5_b_o_a08
            if (p_opcg6_b_o_a08 .ge. p1st) lptr_opcg6_b_o_aer(08,itype,ai_phase)  = p_opcg6_b_o_a08
            if (p_opcg7_b_o_a08 .ge. p1st) lptr_opcg7_b_o_aer(08,itype,ai_phase)  = p_opcg7_b_o_a08
            if (p_opcg8_b_o_a08 .ge. p1st) lptr_opcg8_b_o_aer(08,itype,ai_phase)  = p_opcg8_b_o_a08
            if (p_pcg1_f_c_a08 .ge. p1st) lptr_pcg1_f_c_aer(08,itype,ai_phase) = p_pcg1_f_c_a08
            if (p_pcg2_f_c_a08 .ge. p1st) lptr_pcg2_f_c_aer(08,itype,ai_phase)  = p_pcg2_f_c_a08
            if (p_pcg3_f_c_a08 .ge. p1st) lptr_pcg3_f_c_aer(08,itype,ai_phase)  = p_pcg3_f_c_a08
            if (p_pcg4_f_c_a08 .ge. p1st) lptr_pcg4_f_c_aer(08,itype,ai_phase)  = p_pcg4_f_c_a08
            if (p_pcg5_f_c_a08 .ge. p1st) lptr_pcg5_f_c_aer(08,itype,ai_phase)  = p_pcg5_f_c_a08
            if (p_pcg6_f_c_a08 .ge. p1st) lptr_pcg6_f_c_aer(08,itype,ai_phase)  = p_pcg6_f_c_a08
            if (p_pcg7_f_c_a08 .ge. p1st) lptr_pcg7_f_c_aer(08,itype,ai_phase)  = p_pcg7_f_c_a08
            if (p_pcg8_f_c_a08 .ge. p1st) lptr_pcg8_f_c_aer(08,itype,ai_phase)  = p_pcg8_f_c_a08
            if (p_pcg9_f_c_a08 .ge. p1st) lptr_pcg9_f_c_aer(08,itype,ai_phase)  = p_pcg9_f_c_a08
            if (p_pcg1_f_o_a08 .ge. p1st) lptr_pcg1_f_o_aer(08,itype,ai_phase) = p_pcg1_f_o_a08
            if (p_pcg2_f_o_a08 .ge. p1st) lptr_pcg2_f_o_aer(08,itype,ai_phase)  = p_pcg2_f_o_a08
            if (p_pcg3_f_o_a08 .ge. p1st) lptr_pcg3_f_o_aer(08,itype,ai_phase)  = p_pcg3_f_o_a08
            if (p_pcg4_f_o_a08 .ge. p1st) lptr_pcg4_f_o_aer(08,itype,ai_phase)  = p_pcg4_f_o_a08
            if (p_pcg5_f_o_a08 .ge. p1st) lptr_pcg5_f_o_aer(08,itype,ai_phase)  = p_pcg5_f_o_a08
            if (p_pcg6_f_o_a08 .ge. p1st) lptr_pcg6_f_o_aer(08,itype,ai_phase)  = p_pcg6_f_o_a08
            if (p_pcg7_f_o_a08 .ge. p1st) lptr_pcg7_f_o_aer(08,itype,ai_phase)  = p_pcg7_f_o_a08
            if (p_pcg8_f_o_a08 .ge. p1st) lptr_pcg8_f_o_aer(08,itype,ai_phase)  = p_pcg8_f_o_a08
            if (p_pcg9_f_o_a08 .ge. p1st) lptr_pcg9_f_o_aer(08,itype,ai_phase)  = p_pcg9_f_o_a08
            if (p_opcg1_f_c_a08 .ge. p1st) lptr_opcg1_f_c_aer(08,itype,ai_phase) = p_opcg1_f_c_a08
            if (p_opcg2_f_c_a08 .ge. p1st) lptr_opcg2_f_c_aer(08,itype,ai_phase)  = p_opcg2_f_c_a08
            if (p_opcg3_f_c_a08 .ge. p1st) lptr_opcg3_f_c_aer(08,itype,ai_phase)  = p_opcg3_f_c_a08
            if (p_opcg4_f_c_a08 .ge. p1st) lptr_opcg4_f_c_aer(08,itype,ai_phase)  = p_opcg4_f_c_a08
            if (p_opcg5_f_c_a08 .ge. p1st) lptr_opcg5_f_c_aer(08,itype,ai_phase)  = p_opcg5_f_c_a08
            if (p_opcg6_f_c_a08 .ge. p1st) lptr_opcg6_f_c_aer(08,itype,ai_phase)  = p_opcg6_f_c_a08
            if (p_opcg7_f_c_a08 .ge. p1st) lptr_opcg7_f_c_aer(08,itype,ai_phase)  = p_opcg7_f_c_a08
            if (p_opcg8_f_c_a08 .ge. p1st) lptr_opcg8_f_c_aer(08,itype,ai_phase)  = p_opcg8_f_c_a08
            if (p_opcg1_f_o_a08 .ge. p1st) lptr_opcg1_f_o_aer(08,itype,ai_phase) = p_opcg1_f_o_a08
            if (p_opcg2_f_o_a08 .ge. p1st) lptr_opcg2_f_o_aer(08,itype,ai_phase)  = p_opcg2_f_o_a08
            if (p_opcg3_f_o_a08 .ge. p1st) lptr_opcg3_f_o_aer(08,itype,ai_phase)  = p_opcg3_f_o_a08
            if (p_opcg4_f_o_a08 .ge. p1st) lptr_opcg4_f_o_aer(08,itype,ai_phase)  = p_opcg4_f_o_a08
            if (p_opcg5_f_o_a08 .ge. p1st) lptr_opcg5_f_o_aer(08,itype,ai_phase)  = p_opcg5_f_o_a08
            if (p_opcg6_f_o_a08 .ge. p1st) lptr_opcg6_f_o_aer(08,itype,ai_phase)  = p_opcg6_f_o_a08
            if (p_opcg7_f_o_a08 .ge. p1st) lptr_opcg7_f_o_aer(08,itype,ai_phase)  = p_opcg7_f_o_a08
            if (p_opcg8_f_o_a08 .ge. p1st) lptr_opcg8_f_o_aer(08,itype,ai_phase)  = p_opcg8_f_o_a08
            if (p_ant1_c_a08 .ge. p1st) lptr_ant1_c_aer(08,itype,ai_phase)  = p_ant1_c_a08
            if (p_ant2_c_a08 .ge. p1st) lptr_ant2_c_aer(08,itype,ai_phase)  = p_ant2_c_a08
            if (p_ant3_c_a08 .ge. p1st) lptr_ant3_c_aer(08,itype,ai_phase)  = p_ant3_c_a08
            if (p_ant4_c_a08 .ge. p1st) lptr_ant4_c_aer(08,itype,ai_phase)  = p_ant4_c_a08
            if (p_biog1_c_a08 .ge. p1st) lptr_biog1_c_aer(08,itype,ai_phase)  = p_biog1_c_a08
            if (p_biog2_c_a08 .ge. p1st) lptr_biog2_c_aer(08,itype,ai_phase)  = p_biog2_c_a08
            if (p_biog3_c_a08 .ge. p1st) lptr_biog3_c_aer(08,itype,ai_phase)  = p_biog3_c_a08
            if (p_biog4_c_a08 .ge. p1st) lptr_biog4_c_aer(08,itype,ai_phase)  = p_biog4_c_a08
            if (p_ant1_o_a08 .ge. p1st) lptr_ant1_o_aer(08,itype,ai_phase)  = p_ant1_o_a08
            if (p_ant2_o_a08 .ge. p1st) lptr_ant2_o_aer(08,itype,ai_phase)  = p_ant2_o_a08
            if (p_ant3_o_a08 .ge. p1st) lptr_ant3_o_aer(08,itype,ai_phase)  = p_ant3_o_a08
            if (p_ant4_o_a08 .ge. p1st) lptr_ant4_o_aer(08,itype,ai_phase)  = p_ant4_o_a08
            if (p_biog1_o_a08 .ge. p1st) lptr_biog1_o_aer(08,itype,ai_phase)  = p_biog1_o_a08
            if (p_biog2_o_a08 .ge. p1st) lptr_biog2_o_aer(08,itype,ai_phase)  = p_biog2_o_a08
            if (p_biog3_o_a08 .ge. p1st) lptr_biog3_o_aer(08,itype,ai_phase)  = p_biog3_o_a08
            if (p_biog4_o_a08 .ge. p1st) lptr_biog4_o_aer(08,itype,ai_phase)  = p_biog4_o_a08
            
	    numptr_aer(08,itype,ai_phase)        = p_num_a08
	end if


#if defined ( cw_species_are_in_registry )
!   this code is "active" only when cw species are in the registry
	if (nsize_aer(itype) .ge. 1) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(01,itype,cw_phase)      = p_so4_cw01
	    lptr_no3_aer(01,itype,cw_phase)      = p_no3_cw01
	    lptr_cl_aer(01,itype,cw_phase)       = p_cl_cw01
	    lptr_msa_aer(01,itype,cw_phase)      = p_msa_cw01
	    lptr_co3_aer(01,itype,cw_phase)      = p_co3_cw01
	    lptr_nh4_aer(01,itype,cw_phase)      = p_nh4_cw01
	    lptr_na_aer(01,itype,cw_phase)       = p_na_cw01
	    lptr_ca_aer(01,itype,cw_phase)       = p_ca_cw01
	    lptr_oin_aer(01,itype,cw_phase)      = p_oin_cw01
	    lptr_oc_aer(01,itype,cw_phase)       = p_oc_cw01
	    lptr_bc_aer(01,itype,cw_phase)       = p_bc_cw01

            lptr_pcg1_b_c_aer(01,itype,cw_phase) = p_pcg1_b_c_cw01
            lptr_opcg1_b_c_aer(01,itype,cw_phase) = p_opcg1_b_c_cw01
            lptr_pcg1_b_o_aer(01,itype,cw_phase) = p_pcg1_b_o_cw01
            lptr_opcg1_b_o_aer(01,itype,cw_phase) = p_opcg1_b_o_cw01
            lptr_pcg1_f_c_aer(01,itype,cw_phase) = p_pcg1_f_c_cw01
            lptr_opcg1_f_c_aer(01,itype,cw_phase) = p_opcg1_f_c_cw01
            lptr_pcg1_f_o_aer(01,itype,cw_phase) = p_pcg1_f_o_cw01
            lptr_opcg1_f_o_aer(01,itype,cw_phase) = p_opcg1_f_o_cw01
            lptr_ant1_c_aer(01,itype,cw_phase) = p_ant1_c_cw01
            lptr_biog1_c_aer(01,itype,cw_phase) = p_biog1_c_cw01

      if (p_glysoa_r1_cw01 .ge. p1st) lptr_glysoa_r1_aer(01,itype,cw_phase) = p_glysoa_r1_cw01
      if (p_glysoa_r2_cw01 .ge. p1st) lptr_glysoa_r2_aer(01,itype,cw_phase) = p_glysoa_r2_cw01
      if (p_glysoa_sfc_cw01 .ge. p1st) lptr_glysoa_sfc_aer(01,itype,cw_phase) = p_glysoa_sfc_cw01
      if (p_glysoa_nh4_cw01 .ge. p1st) lptr_glysoa_nh4_aer(01,itype,cw_phase) = p_glysoa_nh4_cw01
      if (p_glysoa_oh_cw01 .ge. p1st) lptr_glysoa_oh_aer(01,itype,cw_phase) = p_glysoa_oh_cw01
      if (p_asoaX_cw01 .ge. p1st)    lptr_asoaX_aer(01,itype,cw_phase)    = p_asoaX_cw01
      if (p_asoa1_cw01 .ge. p1st)    lptr_asoa1_aer(01,itype,cw_phase)    = p_asoa1_cw01
      if (p_asoa2_cw01 .ge. p1st)    lptr_asoa2_aer(01,itype,cw_phase)    = p_asoa2_cw01
      if (p_asoa3_cw01 .ge. p1st)    lptr_asoa3_aer(01,itype,cw_phase)    = p_asoa3_cw01
      if (p_asoa4_cw01 .ge. p1st)    lptr_asoa4_aer(01,itype,cw_phase)    = p_asoa4_cw01
      if (p_bsoaX_cw01 .ge. p1st)    lptr_bsoaX_aer(01,itype,cw_phase)    = p_bsoaX_cw01
      if (p_bsoa1_cw01 .ge. p1st)    lptr_bsoa1_aer(01,itype,cw_phase)    = p_bsoa1_cw01
      if (p_bsoa2_cw01 .ge. p1st)    lptr_bsoa2_aer(01,itype,cw_phase)    = p_bsoa2_cw01
      if (p_bsoa3_cw01 .ge. p1st)    lptr_bsoa3_aer(01,itype,cw_phase)    = p_bsoa3_cw01
      if (p_bsoa4_cw01 .ge. p1st)    lptr_bsoa4_aer(01,itype,cw_phase)    = p_bsoa4_cw01

	    numptr_aer(01,itype,cw_phase)        = p_num_cw01
	  end if
	end if

	if (nsize_aer(itype) .ge. 2) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(02,itype,cw_phase)      = p_so4_cw02
	    lptr_no3_aer(02,itype,cw_phase)      = p_no3_cw02
	    lptr_cl_aer(02,itype,cw_phase)       = p_cl_cw02
	    lptr_msa_aer(02,itype,cw_phase)      = p_msa_cw02
	    lptr_co3_aer(02,itype,cw_phase)      = p_co3_cw02
	    lptr_nh4_aer(02,itype,cw_phase)      = p_nh4_cw02
	    lptr_na_aer(02,itype,cw_phase)       = p_na_cw02
	    lptr_ca_aer(02,itype,cw_phase)       = p_ca_cw02
	    lptr_oin_aer(02,itype,cw_phase)      = p_oin_cw02
	    lptr_oc_aer(02,itype,cw_phase)       = p_oc_cw02
	    lptr_bc_aer(02,itype,cw_phase)       = p_bc_cw02

            lptr_pcg1_b_c_aer(02,itype,cw_phase) = p_pcg1_b_c_cw02
            lptr_opcg1_b_c_aer(02,itype,cw_phase) = p_opcg1_b_c_cw02
            lptr_pcg1_b_o_aer(02,itype,cw_phase) = p_pcg1_b_o_cw02
            lptr_opcg1_b_o_aer(02,itype,cw_phase) = p_opcg1_b_o_cw02
            lptr_pcg1_f_c_aer(02,itype,cw_phase) = p_pcg1_f_c_cw02
            lptr_opcg1_f_c_aer(02,itype,cw_phase) = p_opcg1_f_c_cw02
            lptr_pcg1_f_o_aer(02,itype,cw_phase) = p_pcg1_f_o_cw02
            lptr_opcg1_f_o_aer(02,itype,cw_phase) = p_opcg1_f_o_cw02
            lptr_ant1_c_aer(02,itype,cw_phase) = p_ant1_c_cw02
            lptr_biog1_c_aer(02,itype,cw_phase) = p_biog1_c_cw02

      if (p_glysoa_r1_cw02 .ge. p1st) lptr_glysoa_r1_aer(02,itype,cw_phase) = p_glysoa_r1_cw02
      if (p_glysoa_r2_cw02 .ge. p1st) lptr_glysoa_r2_aer(02,itype,cw_phase) = p_glysoa_r2_cw02
      if (p_glysoa_sfc_cw02 .ge. p1st) lptr_glysoa_sfc_aer(02,itype,cw_phase) = p_glysoa_sfc_cw02
      if (p_glysoa_nh4_cw02 .ge. p1st) lptr_glysoa_nh4_aer(02,itype,cw_phase) = p_glysoa_nh4_cw02
      if (p_glysoa_oh_cw02 .ge. p1st) lptr_glysoa_oh_aer(02,itype,cw_phase) = p_glysoa_oh_cw02
      if (p_asoaX_cw02 .ge. p1st)    lptr_asoaX_aer(02,itype,cw_phase)    = p_asoaX_cw02
      if (p_asoa1_cw02 .ge. p1st)    lptr_asoa1_aer(02,itype,cw_phase)    = p_asoa1_cw02
      if (p_asoa2_cw02 .ge. p1st)    lptr_asoa2_aer(02,itype,cw_phase)    = p_asoa2_cw02
      if (p_asoa3_cw02 .ge. p1st)    lptr_asoa3_aer(02,itype,cw_phase)    = p_asoa3_cw02
      if (p_asoa4_cw02 .ge. p1st)    lptr_asoa4_aer(02,itype,cw_phase)    = p_asoa4_cw02
      if (p_bsoaX_cw02 .ge. p1st)    lptr_bsoaX_aer(02,itype,cw_phase)    = p_bsoaX_cw02
      if (p_bsoa1_cw02 .ge. p1st)    lptr_bsoa1_aer(02,itype,cw_phase)    = p_bsoa1_cw02
      if (p_bsoa2_cw02 .ge. p1st)    lptr_bsoa2_aer(02,itype,cw_phase)    = p_bsoa2_cw02
      if (p_bsoa3_cw02 .ge. p1st)    lptr_bsoa3_aer(02,itype,cw_phase)    = p_bsoa3_cw02
      if (p_bsoa4_cw02 .ge. p1st)    lptr_bsoa4_aer(02,itype,cw_phase)    = p_bsoa4_cw02
	    numptr_aer(02,itype,cw_phase)        = p_num_cw02
	  end if
	end if

	if (nsize_aer(itype) .ge. 3) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(03,itype,cw_phase)      = p_so4_cw03
	    lptr_no3_aer(03,itype,cw_phase)      = p_no3_cw03
	    lptr_cl_aer(03,itype,cw_phase)       = p_cl_cw03
	    lptr_msa_aer(03,itype,cw_phase)      = p_msa_cw03
	    lptr_co3_aer(03,itype,cw_phase)      = p_co3_cw03
	    lptr_nh4_aer(03,itype,cw_phase)      = p_nh4_cw03
	    lptr_na_aer(03,itype,cw_phase)       = p_na_cw03
	    lptr_ca_aer(03,itype,cw_phase)       = p_ca_cw03
	    lptr_oin_aer(03,itype,cw_phase)      = p_oin_cw03
	    lptr_oc_aer(03,itype,cw_phase)       = p_oc_cw03
	    lptr_bc_aer(03,itype,cw_phase)       = p_bc_cw03
            
            lptr_pcg1_b_c_aer(03,itype,cw_phase) = p_pcg1_b_c_cw03
            lptr_opcg1_b_c_aer(03,itype,cw_phase) = p_opcg1_b_c_cw03
            lptr_pcg1_b_o_aer(03,itype,cw_phase) = p_pcg1_b_o_cw03
            lptr_opcg1_b_o_aer(03,itype,cw_phase) = p_opcg1_b_o_cw03
            lptr_pcg1_f_c_aer(03,itype,cw_phase) = p_pcg1_f_c_cw03
            lptr_opcg1_f_c_aer(03,itype,cw_phase) = p_opcg1_f_c_cw03
            lptr_pcg1_f_o_aer(03,itype,cw_phase) = p_pcg1_f_o_cw03
            lptr_opcg1_f_o_aer(03,itype,cw_phase) = p_opcg1_f_o_cw03
            lptr_ant1_c_aer(03,itype,cw_phase) = p_ant1_c_cw03
            lptr_biog1_c_aer(03,itype,cw_phase) = p_biog1_c_cw03

      if (p_glysoa_r1_cw03 .ge. p1st) lptr_glysoa_r1_aer(03,itype,cw_phase) = p_glysoa_r1_cw03
      if (p_glysoa_r2_cw03 .ge. p1st) lptr_glysoa_r2_aer(03,itype,cw_phase) = p_glysoa_r2_cw03
      if (p_glysoa_sfc_cw03 .ge. p1st) lptr_glysoa_sfc_aer(03,itype,cw_phase) = p_glysoa_sfc_cw03
      if (p_glysoa_nh4_cw03 .ge. p1st) lptr_glysoa_nh4_aer(03,itype,cw_phase) = p_glysoa_nh4_cw03
      if (p_glysoa_oh_cw03 .ge. p1st) lptr_glysoa_oh_aer(03,itype,cw_phase) = p_glysoa_oh_cw03
      if (p_asoaX_cw03 .ge. p1st)    lptr_asoaX_aer(03,itype,cw_phase)    = p_asoaX_cw03
      if (p_asoa1_cw03 .ge. p1st)    lptr_asoa1_aer(03,itype,cw_phase)    = p_asoa1_cw03
      if (p_asoa2_cw03 .ge. p1st)    lptr_asoa2_aer(03,itype,cw_phase)    = p_asoa2_cw03
      if (p_asoa3_cw03 .ge. p1st)    lptr_asoa3_aer(03,itype,cw_phase)    = p_asoa3_cw03
      if (p_asoa4_cw03 .ge. p1st)    lptr_asoa4_aer(03,itype,cw_phase)    = p_asoa4_cw03
      if (p_bsoaX_cw03 .ge. p1st)    lptr_bsoaX_aer(03,itype,cw_phase)    = p_bsoaX_cw03
      if (p_bsoa1_cw03 .ge. p1st)    lptr_bsoa1_aer(03,itype,cw_phase)    = p_bsoa1_cw03
      if (p_bsoa2_cw03 .ge. p1st)    lptr_bsoa2_aer(03,itype,cw_phase)    = p_bsoa2_cw03
      if (p_bsoa3_cw03 .ge. p1st)    lptr_bsoa3_aer(03,itype,cw_phase)    = p_bsoa3_cw03
      if (p_bsoa4_cw03 .ge. p1st)    lptr_bsoa4_aer(03,itype,cw_phase)    = p_bsoa4_cw03
	    numptr_aer(03,itype,cw_phase)        = p_num_cw03
	  end if
	end if

	if (nsize_aer(itype) .ge. 4) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(04,itype,cw_phase)      = p_so4_cw04
	    lptr_no3_aer(04,itype,cw_phase)      = p_no3_cw04
	    lptr_cl_aer(04,itype,cw_phase)       = p_cl_cw04
	    lptr_msa_aer(04,itype,cw_phase)      = p_msa_cw04
	    lptr_co3_aer(04,itype,cw_phase)      = p_co3_cw04
	    lptr_nh4_aer(04,itype,cw_phase)      = p_nh4_cw04
	    lptr_na_aer(04,itype,cw_phase)       = p_na_cw04
	    lptr_ca_aer(04,itype,cw_phase)       = p_ca_cw04
	    lptr_oin_aer(04,itype,cw_phase)      = p_oin_cw04
	    lptr_oc_aer(04,itype,cw_phase)       = p_oc_cw04
	    lptr_bc_aer(04,itype,cw_phase)       = p_bc_cw04

            lptr_pcg1_b_c_aer(04,itype,cw_phase) = p_pcg1_b_c_cw04
            lptr_opcg1_b_c_aer(04,itype,cw_phase) = p_opcg1_b_c_cw04
            lptr_pcg1_b_o_aer(04,itype,cw_phase) = p_pcg1_b_o_cw04
            lptr_opcg1_b_o_aer(04,itype,cw_phase) = p_opcg1_b_o_cw04
            lptr_pcg1_f_c_aer(04,itype,cw_phase) = p_pcg1_f_c_cw04
            lptr_opcg1_f_c_aer(04,itype,cw_phase) = p_opcg1_f_c_cw04
            lptr_pcg1_f_o_aer(04,itype,cw_phase) = p_pcg1_f_o_cw04
            lptr_opcg1_f_o_aer(04,itype,cw_phase) = p_opcg1_f_o_cw04
            lptr_ant1_c_aer(04,itype,cw_phase) = p_ant1_c_cw04
            lptr_biog1_c_aer(04,itype,cw_phase) = p_biog1_c_cw04

      if (p_glysoa_r1_cw04 .ge. p1st) lptr_glysoa_r1_aer(04,itype,cw_phase) = p_glysoa_r1_cw04
      if (p_glysoa_r2_cw04 .ge. p1st) lptr_glysoa_r2_aer(04,itype,cw_phase) = p_glysoa_r2_cw04
      if (p_glysoa_sfc_cw04 .ge. p1st) lptr_glysoa_sfc_aer(04,itype,cw_phase) = p_glysoa_sfc_cw04
      if (p_glysoa_nh4_cw04 .ge. p1st) lptr_glysoa_nh4_aer(04,itype,cw_phase) = p_glysoa_nh4_cw04
      if (p_glysoa_oh_cw04 .ge. p1st) lptr_glysoa_oh_aer(04,itype,cw_phase) = p_glysoa_oh_cw04
      if (p_asoaX_cw04 .ge. p1st)    lptr_asoaX_aer(04,itype,cw_phase)    = p_asoaX_cw04
      if (p_asoa1_cw04 .ge. p1st)    lptr_asoa1_aer(04,itype,cw_phase)    = p_asoa1_cw04
      if (p_asoa2_cw04 .ge. p1st)    lptr_asoa2_aer(04,itype,cw_phase)    = p_asoa2_cw04
      if (p_asoa3_cw04 .ge. p1st)    lptr_asoa3_aer(04,itype,cw_phase)    = p_asoa3_cw04
      if (p_asoa4_cw04 .ge. p1st)    lptr_asoa4_aer(04,itype,cw_phase)    = p_asoa4_cw04
      if (p_bsoaX_cw04 .ge. p1st)    lptr_bsoaX_aer(04,itype,cw_phase)    = p_bsoaX_cw04
      if (p_bsoa1_cw04 .ge. p1st)    lptr_bsoa1_aer(04,itype,cw_phase)    = p_bsoa1_cw04
      if (p_bsoa2_cw04 .ge. p1st)    lptr_bsoa2_aer(04,itype,cw_phase)    = p_bsoa2_cw04
      if (p_bsoa3_cw04 .ge. p1st)    lptr_bsoa3_aer(04,itype,cw_phase)    = p_bsoa3_cw04
      if (p_bsoa4_cw04 .ge. p1st)    lptr_bsoa4_aer(04,itype,cw_phase)    = p_bsoa4_cw04
	    numptr_aer(04,itype,cw_phase)        = p_num_cw04
	  end if
	end if

	if (nsize_aer(itype) .ge. 5) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(05,itype,cw_phase)      = p_so4_cw05
	    lptr_no3_aer(05,itype,cw_phase)      = p_no3_cw05
	    lptr_cl_aer(05,itype,cw_phase)       = p_cl_cw05
	    lptr_msa_aer(05,itype,cw_phase)      = p_msa_cw05
	    lptr_co3_aer(05,itype,cw_phase)      = p_co3_cw05
	    lptr_nh4_aer(05,itype,cw_phase)      = p_nh4_cw05
	    lptr_na_aer(05,itype,cw_phase)       = p_na_cw05
	    lptr_ca_aer(05,itype,cw_phase)       = p_ca_cw05
	    lptr_oin_aer(05,itype,cw_phase)      = p_oin_cw05
	    lptr_oc_aer(05,itype,cw_phase)       = p_oc_cw05
	    lptr_bc_aer(05,itype,cw_phase)       = p_bc_cw05
            
            lptr_pcg1_b_c_aer(05,itype,cw_phase) = p_pcg1_b_c_cw05
            lptr_opcg1_b_c_aer(05,itype,cw_phase) = p_opcg1_b_c_cw05
            lptr_pcg1_b_o_aer(05,itype,cw_phase) = p_pcg1_b_o_cw05
            lptr_opcg1_b_o_aer(05,itype,cw_phase) = p_opcg1_b_o_cw05
            lptr_pcg1_f_c_aer(05,itype,cw_phase) = p_pcg1_f_c_cw05
            lptr_opcg1_f_c_aer(05,itype,cw_phase) = p_opcg1_f_c_cw05
            lptr_pcg1_f_o_aer(05,itype,cw_phase) = p_pcg1_f_o_cw05
            lptr_opcg1_f_o_aer(05,itype,cw_phase) = p_opcg1_f_o_cw05
            lptr_ant1_c_aer(05,itype,cw_phase) = p_ant1_c_cw05
            lptr_biog1_c_aer(05,itype,cw_phase) = p_biog1_c_cw05

	    numptr_aer(05,itype,cw_phase)        = p_num_cw05
	  end if
	end if

	if (nsize_aer(itype) .ge. 6) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(06,itype,cw_phase)      = p_so4_cw06
	    lptr_no3_aer(06,itype,cw_phase)      = p_no3_cw06
	    lptr_cl_aer(06,itype,cw_phase)       = p_cl_cw06
	    lptr_msa_aer(06,itype,cw_phase)      = p_msa_cw06
	    lptr_co3_aer(06,itype,cw_phase)      = p_co3_cw06
	    lptr_nh4_aer(06,itype,cw_phase)      = p_nh4_cw06
	    lptr_na_aer(06,itype,cw_phase)       = p_na_cw06
	    lptr_ca_aer(06,itype,cw_phase)       = p_ca_cw06
	    lptr_oin_aer(06,itype,cw_phase)      = p_oin_cw06
	    lptr_oc_aer(06,itype,cw_phase)       = p_oc_cw06
	    lptr_bc_aer(06,itype,cw_phase)       = p_bc_cw06
            
            lptr_pcg1_b_c_aer(06,itype,cw_phase) = p_pcg1_b_c_cw06
            lptr_opcg1_b_c_aer(06,itype,cw_phase) = p_opcg1_b_c_cw06
            lptr_pcg1_b_o_aer(06,itype,cw_phase) = p_pcg1_b_o_cw06
            lptr_opcg1_b_o_aer(06,itype,cw_phase) = p_opcg1_b_o_cw06
            lptr_pcg1_f_c_aer(06,itype,cw_phase) = p_pcg1_f_c_cw06
            lptr_opcg1_f_c_aer(06,itype,cw_phase) = p_opcg1_f_c_cw06
            lptr_pcg1_f_o_aer(06,itype,cw_phase) = p_pcg1_f_o_cw06
            lptr_opcg1_f_o_aer(06,itype,cw_phase) = p_opcg1_f_o_cw06
            lptr_ant1_c_aer(06,itype,cw_phase) = p_ant1_c_cw06
            lptr_biog1_c_aer(06,itype,cw_phase) = p_biog1_c_cw06

	    numptr_aer(06,itype,cw_phase)        = p_num_cw06
	  end if
	end if

	if (nsize_aer(itype) .ge. 7) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(07,itype,cw_phase)      = p_so4_cw07
	    lptr_no3_aer(07,itype,cw_phase)      = p_no3_cw07
	    lptr_cl_aer(07,itype,cw_phase)       = p_cl_cw07
	    lptr_msa_aer(07,itype,cw_phase)      = p_msa_cw07
	    lptr_co3_aer(07,itype,cw_phase)      = p_co3_cw07
	    lptr_nh4_aer(07,itype,cw_phase)      = p_nh4_cw07
	    lptr_na_aer(07,itype,cw_phase)       = p_na_cw07
	    lptr_ca_aer(07,itype,cw_phase)       = p_ca_cw07
	    lptr_oin_aer(07,itype,cw_phase)      = p_oin_cw07
	    lptr_oc_aer(07,itype,cw_phase)       = p_oc_cw07
	    lptr_bc_aer(07,itype,cw_phase)       = p_bc_cw07

            lptr_pcg1_b_c_aer(07,itype,cw_phase) = p_pcg1_b_c_cw07
            lptr_opcg1_b_c_aer(07,itype,cw_phase) = p_opcg1_b_c_cw07
            lptr_pcg1_b_o_aer(07,itype,cw_phase) = p_pcg1_b_o_cw07
            lptr_opcg1_b_o_aer(07,itype,cw_phase) = p_opcg1_b_o_cw07
            lptr_pcg1_f_c_aer(07,itype,cw_phase) = p_pcg1_f_c_cw07
            lptr_opcg1_f_c_aer(07,itype,cw_phase) = p_opcg1_f_c_cw07
            lptr_pcg1_f_o_aer(07,itype,cw_phase) = p_pcg1_f_o_cw07
            lptr_opcg1_f_o_aer(07,itype,cw_phase) = p_opcg1_f_o_cw07
            lptr_ant1_c_aer(07,itype,cw_phase) = p_ant1_c_cw07
            lptr_biog1_c_aer(07,itype,cw_phase) = p_biog1_c_cw07

	    numptr_aer(07,itype,cw_phase)        = p_num_cw07
	  end if
	end if

	if (nsize_aer(itype) .ge. 8) then
	  if (cw_phase .gt. 0) then
	    lptr_so4_aer(08,itype,cw_phase)      = p_so4_cw08
	    lptr_no3_aer(08,itype,cw_phase)      = p_no3_cw08
	    lptr_cl_aer(08,itype,cw_phase)       = p_cl_cw08
	    lptr_msa_aer(08,itype,cw_phase)      = p_msa_cw08
	    lptr_co3_aer(08,itype,cw_phase)      = p_co3_cw08
	    lptr_nh4_aer(08,itype,cw_phase)      = p_nh4_cw08
	    lptr_na_aer(08,itype,cw_phase)       = p_na_cw08
	    lptr_ca_aer(08,itype,cw_phase)       = p_ca_cw08
	    lptr_oin_aer(08,itype,cw_phase)      = p_oin_cw08
	    lptr_oc_aer(08,itype,cw_phase)       = p_oc_cw08
	    lptr_bc_aer(08,itype,cw_phase)       = p_bc_cw08

            lptr_pcg1_b_c_aer(08,itype,cw_phase) = p_pcg1_b_c_cw08
            lptr_opcg1_b_c_aer(08,itype,cw_phase) = p_opcg1_b_c_cw08
            lptr_pcg1_b_o_aer(08,itype,cw_phase) = p_pcg1_b_o_cw08
            lptr_opcg1_b_o_aer(08,itype,cw_phase) = p_opcg1_b_o_cw08
            lptr_pcg1_f_c_aer(08,itype,cw_phase) = p_pcg1_f_c_cw08
            lptr_opcg1_f_c_aer(08,itype,cw_phase) = p_opcg1_f_c_cw08
            lptr_pcg1_f_o_aer(08,itype,cw_phase) = p_pcg1_f_o_cw08
            lptr_opcg1_f_o_aer(08,itype,cw_phase) = p_opcg1_f_o_cw08
            lptr_ant1_c_aer(08,itype,cw_phase) = p_ant1_c_cw08
            lptr_biog1_c_aer(08,itype,cw_phase) = p_biog1_c_cw08

	    numptr_aer(08,itype,cw_phase)        = p_num_cw08
	  end if
	end if
#endif


!
!   define the massptr_aer and mastercompptr_aer pointers
!   and the name() species names
!

!   first initialize
	do l = 1, l2maxd
	    write( name(l), '(a,i4.4,15x)' ) 'r', l
	end do
	massptr_aer(:,:,:,:) = -999888777
	mastercompptr_aer(:,:) = -999888777

	do 2800 itype = 1, ntype_aer

	if (itype .eq. 1) then
	    typetxt = ' '
	    ntypetxt = 1
	    if (ntype_aer .gt. 1) then
		typetxt = '_t1'
		ntypetxt = 3
	    end if
	else if (itype .le. 9) then
	    write(typetxt,'(a,i1)') '_t', itype
	    ntypetxt = 3
	else if (itype .le. 99) then
	    write(typetxt,'(a,i2)') '_t', itype
	    ntypetxt = 4
	else
	    typetxt = '_t?'
	    ntypetxt = 3
	end if

	ncomp_dum(:,:) = 0
	ncomp_plustracer_dum(:,:) = 0

	do 2700 isize = 1, nsize_aer(itype)
	n =isize

	if (isize .le. 9) then
	    write(sizetxt,'(i1)') isize
	    nsizetxt = 1
	else if (isize .le. 99) then
	    write(sizetxt,'(i2)') isize
	    nsizetxt = 2
	else if (isize .le. 999) then
	    write(sizetxt,'(i3)') isize
	    nsizetxt = 3
	else
	    sizetxt = 's?'
	    nsizetxt = 2
	end if


	do 2600 iphase = 1, nphase_aer

	if (iphase .eq. ai_phase) then
	    phasetxt = 'a'
	    nphasetxt = 1
	else if (iphase .eq. cw_phase) then
	    phasetxt = 'cw'
	    nphasetxt = 2
	else 
	    phasetxt = 'p?'
	    nphasetxt = 2
	end if


	do 2500 l_mastercomp = -2, ntot_mastercomp_aer

	iaddto_ncomp = 1
	iaddto_ncomp_plustracer = 1

	if (l_mastercomp .eq. -2) then
	    iaddto_ncomp = 0
	    iaddto_ncomp_plustracer = 0
	    lptr_dum = numptr_aer(n,itype,iphase)
	    mcindx_dum = -2
	    spectxt = 'numb_'
	    nspectxt = 5

	else if (l_mastercomp .eq. -1) then
	    if (iphase .ne. ai_phase) goto 2500
	    iaddto_ncomp = 0
	    iaddto_ncomp_plustracer = 0
	    lptr_dum = waterptr_aer(n,itype)
	    mcindx_dum = -1
	    spectxt = 'water_'
	    nspectxt = 6

	else if (l_mastercomp .eq. 0) then
	    if (iphase .ne. ai_phase) goto 2500
	    iaddto_ncomp = 0
	    iaddto_ncomp_plustracer = 0
	    lptr_dum = hyswptr_aer(n,itype)
	    mcindx_dum = 0
	    spectxt = 'hysw_'
	    nspectxt = 5

	else if (l_mastercomp .eq. mastercompindx_so4_aer) then
	    lptr_dum = lptr_so4_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_so4_aer
	    spectxt = 'so4_'
	    nspectxt = 4

	else if (l_mastercomp .eq. mastercompindx_no3_aer) then
	    lptr_dum = lptr_no3_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_no3_aer
	    spectxt = 'no3_'
	    nspectxt = 4

	else if (l_mastercomp .eq. mastercompindx_cl_aer) then
	    lptr_dum = lptr_cl_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_cl_aer
	    spectxt = 'cl_'
	    nspectxt = 3

	else if (l_mastercomp .eq. mastercompindx_co3_aer) then
	    lptr_dum = lptr_co3_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_co3_aer
	    spectxt = 'co3_'
	    nspectxt = 4

	else if (l_mastercomp .eq. mastercompindx_nh4_aer) then
	    lptr_dum = lptr_nh4_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_nh4_aer
	    spectxt = 'nh4_'
	    nspectxt = 4

	else if (l_mastercomp .eq. mastercompindx_na_aer) then
	    lptr_dum = lptr_na_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_na_aer
	    spectxt = 'na_'
	    nspectxt = 3

	else if (l_mastercomp .eq. mastercompindx_ca_aer) then
	    lptr_dum = lptr_ca_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_ca_aer
	    spectxt = 'ca_'
	    nspectxt = 3

	else if (l_mastercomp .eq. mastercompindx_oin_aer) then
	    lptr_dum = lptr_oin_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_oin_aer
	    spectxt = 'oin_'
	    nspectxt = 4

	else if (l_mastercomp .eq. mastercompindx_oc_aer) then
	    lptr_dum = lptr_oc_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_oc_aer
	    spectxt = 'oc_'
	    nspectxt = 3

	else if (l_mastercomp .eq. mastercompindx_bc_aer) then
	    lptr_dum = lptr_bc_aer(n,itype,iphase)
	    mcindx_dum = mastercompindx_bc_aer
	    spectxt = 'bc_'
	    nspectxt = 3


        else if (l_mastercomp .eq. mastercompindx_pcg1_b_c_aer) then
            lptr_dum = lptr_pcg1_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg1_b_c_aer
            spectxt = 'pcg1_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg2_b_c_aer) then
            lptr_dum = lptr_pcg2_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg2_b_c_aer
            spectxt = 'pcg2_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg3_b_c_aer) then
            lptr_dum = lptr_pcg3_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg3_b_c_aer
            spectxt = 'pcg3_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg4_b_c_aer) then
            lptr_dum = lptr_pcg4_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg4_b_c_aer
            spectxt = 'pcg4_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg5_b_c_aer) then
            lptr_dum = lptr_pcg5_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg5_b_c_aer
            spectxt = 'pcg5_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg6_b_c_aer) then
            lptr_dum = lptr_pcg6_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg6_b_c_aer
            spectxt = 'pcg6_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg7_b_c_aer) then
            lptr_dum = lptr_pcg7_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg7_b_c_aer
            spectxt = 'pcg7_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg8_b_c_aer) then
            lptr_dum = lptr_pcg8_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg8_b_c_aer
            spectxt = 'pcg8_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg9_b_c_aer) then
            lptr_dum = lptr_pcg9_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg9_b_c_aer
            spectxt = 'pcg9_b_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg1_b_o_aer) then
            lptr_dum = lptr_pcg1_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg1_b_o_aer
            spectxt = 'pcg1_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg2_b_o_aer) then
            lptr_dum = lptr_pcg2_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg2_b_o_aer
            spectxt = 'pcg2_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg3_b_o_aer) then
            lptr_dum = lptr_pcg3_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg3_b_o_aer
            spectxt = 'pcg3_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg4_b_o_aer) then
            lptr_dum = lptr_pcg4_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg4_b_o_aer
            spectxt = 'pcg4_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg5_b_o_aer) then
            lptr_dum = lptr_pcg5_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg5_b_o_aer
            spectxt = 'pcg5_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg6_b_o_aer) then
            lptr_dum = lptr_pcg6_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg6_b_o_aer
            spectxt = 'pcg6_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg7_b_o_aer) then
            lptr_dum = lptr_pcg7_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg7_b_o_aer
            spectxt = 'pcg7_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg8_b_o_aer) then
            lptr_dum = lptr_pcg8_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg8_b_o_aer
            spectxt = 'pcg8_b_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg9_b_o_aer) then
            lptr_dum = lptr_pcg9_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg9_b_o_aer
            spectxt = 'pcg9_b_o_'
            nspectxt = 9
        else if (l_mastercomp .eq. mastercompindx_opcg1_b_c_aer) then
            lptr_dum = lptr_opcg1_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg1_b_c_aer
            spectxt = 'opcg1_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg2_b_c_aer) then
            lptr_dum = lptr_opcg2_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg2_b_c_aer
            spectxt = 'opcg2_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg3_b_c_aer) then
            lptr_dum = lptr_opcg3_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg3_b_c_aer
            spectxt = 'opcg3_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg4_b_c_aer) then
            lptr_dum = lptr_opcg4_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg4_b_c_aer
            spectxt = 'opcg4_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg5_b_c_aer) then
            lptr_dum = lptr_opcg5_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg5_b_c_aer
            spectxt = 'opcg5_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg6_b_c_aer) then
            lptr_dum = lptr_opcg6_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg6_b_c_aer
            spectxt = 'opcg6_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg7_b_c_aer) then
            lptr_dum = lptr_opcg7_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg7_b_c_aer
            spectxt = 'opcg7_b_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg8_b_c_aer) then
            lptr_dum = lptr_opcg8_b_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg8_b_c_aer
            spectxt = 'opcg8_b_c_'
            nspectxt = 10
        else if (l_mastercomp .eq. mastercompindx_opcg1_b_o_aer) then
            lptr_dum = lptr_opcg1_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg1_b_o_aer
            spectxt = 'opcg1_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg2_b_o_aer) then
            lptr_dum = lptr_opcg2_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg2_b_o_aer
            spectxt = 'opcg2_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg3_b_o_aer) then
            lptr_dum = lptr_opcg3_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg3_b_o_aer
            spectxt = 'opcg3_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg4_b_o_aer) then
            lptr_dum = lptr_opcg4_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg4_b_o_aer
            spectxt = 'opcg4_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg5_b_o_aer) then
            lptr_dum = lptr_opcg5_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg5_b_o_aer
            spectxt = 'opcg5_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg6_b_o_aer) then
            lptr_dum = lptr_opcg6_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg6_b_o_aer
            spectxt = 'opcg6_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg7_b_o_aer) then
            lptr_dum = lptr_opcg7_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg7_b_o_aer
            spectxt = 'opcg7_b_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg8_b_o_aer) then
            lptr_dum = lptr_opcg8_b_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg8_b_o_aer
            spectxt = 'opcg8_b_o_'
            nspectxt = 10
        else if (l_mastercomp .eq. mastercompindx_pcg1_f_c_aer) then
            lptr_dum = lptr_pcg1_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg1_f_c_aer
            spectxt = 'pcg1_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg2_f_c_aer) then
            lptr_dum = lptr_pcg2_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg2_f_c_aer
            spectxt = 'pcg2_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg3_f_c_aer) then
            lptr_dum = lptr_pcg3_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg3_f_c_aer
            spectxt = 'pcg3_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg4_f_c_aer) then
            lptr_dum = lptr_pcg4_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg4_f_c_aer
            spectxt = 'pcg4_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg5_f_c_aer) then
            lptr_dum = lptr_pcg5_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg5_f_c_aer
            spectxt = 'pcg5_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg6_f_c_aer) then
            lptr_dum = lptr_pcg6_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg6_f_c_aer
            spectxt = 'pcg6_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg7_f_c_aer) then
            lptr_dum = lptr_pcg7_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg7_f_c_aer
            spectxt = 'pcg7_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg8_f_c_aer) then
            lptr_dum = lptr_pcg8_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg8_f_c_aer
            spectxt = 'pcg8_f_c_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg9_f_c_aer) then
            lptr_dum = lptr_pcg9_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg9_f_c_aer
            spectxt = 'pcg9_f_c_'
            nspectxt = 9
        else if (l_mastercomp .eq. mastercompindx_pcg1_f_o_aer) then
            lptr_dum = lptr_pcg1_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg1_f_o_aer
            spectxt = 'pcg1_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg2_f_o_aer) then
            lptr_dum = lptr_pcg2_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg2_f_o_aer
            spectxt = 'pcg2_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg3_f_o_aer) then
            lptr_dum = lptr_pcg3_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg3_f_o_aer
            spectxt = 'pcg3_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg4_f_o_aer) then
            lptr_dum = lptr_pcg4_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg4_f_o_aer
            spectxt = 'pcg4_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg5_f_o_aer) then
            lptr_dum = lptr_pcg5_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg5_f_o_aer
            spectxt = 'pcg5_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg6_f_o_aer) then
            lptr_dum = lptr_pcg6_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg6_f_o_aer
            spectxt = 'pcg6_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg7_f_o_aer) then
            lptr_dum = lptr_pcg7_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg7_f_o_aer
            spectxt = 'pcg7_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg8_f_o_aer) then
            lptr_dum = lptr_pcg8_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg8_f_o_aer
            spectxt = 'pcg8_f_o_'
            nspectxt = 9

        else if (l_mastercomp .eq. mastercompindx_pcg9_f_o_aer) then
            lptr_dum = lptr_pcg9_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_pcg9_f_o_aer
            spectxt = 'pcg9_f_o_'
            nspectxt = 9
        else if (l_mastercomp .eq. mastercompindx_opcg1_f_c_aer) then
            lptr_dum = lptr_opcg1_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg1_f_c_aer
            spectxt = 'opcg1_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg2_f_c_aer) then
            lptr_dum = lptr_opcg2_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg2_f_c_aer
            spectxt = 'opcg2_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg3_f_c_aer) then
            lptr_dum = lptr_opcg3_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg3_f_c_aer
            spectxt = 'opcg3_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg4_f_c_aer) then
            lptr_dum = lptr_opcg4_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg4_f_c_aer
            spectxt = 'opcg4_f_c_'
            nspectxt = 10
        else if (l_mastercomp .eq. mastercompindx_opcg5_f_c_aer) then
            lptr_dum = lptr_opcg5_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg5_f_c_aer
            spectxt = 'opcg5_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg6_f_c_aer) then
            lptr_dum = lptr_opcg6_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg6_f_c_aer
            spectxt = 'opcg6_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg7_f_c_aer) then
            lptr_dum = lptr_opcg7_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg7_f_c_aer
            spectxt = 'opcg7_f_c_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg8_f_c_aer) then
            lptr_dum = lptr_opcg8_f_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg8_f_c_aer
            spectxt = 'opcg8_f_c_'
            nspectxt = 10
        else if (l_mastercomp .eq. mastercompindx_opcg1_f_o_aer) then
            lptr_dum = lptr_opcg1_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg1_f_o_aer
            spectxt = 'opcg1_f_o_'
            nspectxt = 10
        else if (l_mastercomp .eq. mastercompindx_opcg2_f_o_aer) then
            lptr_dum = lptr_opcg2_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg2_f_o_aer
            spectxt = 'opcg2_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg3_f_o_aer) then
            lptr_dum = lptr_opcg3_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg3_f_o_aer
            spectxt = 'opcg3_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg4_f_o_aer) then
            lptr_dum = lptr_opcg4_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg4_f_o_aer
            spectxt = 'opcg4_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg5_f_o_aer) then
            lptr_dum = lptr_opcg5_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg5_f_o_aer
            spectxt = 'opcg5_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg6_f_o_aer) then
            lptr_dum = lptr_opcg6_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg6_f_o_aer
            spectxt = 'opcg6_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg7_f_o_aer) then
            lptr_dum = lptr_opcg7_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg7_f_o_aer
            spectxt = 'opcg7_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_opcg8_f_o_aer) then
            lptr_dum = lptr_opcg8_f_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_opcg8_f_o_aer
            spectxt = 'opcg8_f_o_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_ant1_c_aer) then
            lptr_dum = lptr_ant1_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant1_c_aer
            spectxt = 'ant1_c_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant2_c_aer) then
            lptr_dum = lptr_ant2_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant2_c_aer
            spectxt = 'ant2_c_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant3_c_aer) then
            lptr_dum = lptr_ant3_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant3_c_aer
            spectxt = 'ant3_c_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant4_c_aer) then
            lptr_dum = lptr_ant4_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant4_c_aer
            spectxt = 'ant4_c_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant1_o_aer) then
            lptr_dum = lptr_ant1_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant1_o_aer
            spectxt = 'ant1_o_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant2_o_aer) then
            lptr_dum = lptr_ant2_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant2_o_aer
            spectxt = 'ant2_o_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant3_o_aer) then
            lptr_dum = lptr_ant3_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant3_o_aer
            spectxt = 'ant3_o_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_ant4_o_aer) then
            lptr_dum = lptr_ant4_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_ant4_o_aer
            spectxt = 'ant4_o_'
            nspectxt = 7
        else if (l_mastercomp .eq. mastercompindx_biog1_c_aer) then
            lptr_dum = lptr_biog1_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog1_c_aer
            spectxt = 'biog1_c_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog2_c_aer) then
            lptr_dum = lptr_biog2_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog2_c_aer
            spectxt = 'biog2_c_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog3_c_aer) then
            lptr_dum = lptr_biog3_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog3_c_aer
            spectxt = 'biog3_c_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog4_c_aer) then
            lptr_dum = lptr_biog4_c_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog4_c_aer
            spectxt = 'biog4_c_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog1_o_aer) then
            lptr_dum = lptr_biog1_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog1_o_aer
            spectxt = 'biog1_o_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog2_o_aer) then
            lptr_dum = lptr_biog2_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog2_o_aer
            spectxt = 'biog2_o_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog3_o_aer) then
            lptr_dum = lptr_biog3_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog3_o_aer
            spectxt = 'biog3_o_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_biog4_o_aer) then
            lptr_dum = lptr_biog4_o_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_biog4_o_aer
            spectxt = 'biog4_o_'
            nspectxt = 8
        else if (l_mastercomp .eq. mastercompindx_smpa_aer) then
            lptr_dum = lptr_smpa_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_smpa_aer
            spectxt = 'smpa_'
            nspectxt = 5

        else if (l_mastercomp .eq. mastercompindx_smpbb_aer) then
            lptr_dum = lptr_smpbb_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_smpbb_aer
            spectxt = 'smpbb_'
            nspectxt = 5

         else if (l_mastercomp .eq. mastercompindx_glysoa_r1_aer) then
            lptr_dum = lptr_glysoa_r1_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_glysoa_r1_aer
            spectxt = 'glysoa_r1_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_glysoa_r2_aer) then
            lptr_dum = lptr_glysoa_r2_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_glysoa_r2_aer
            spectxt = 'glysoa_r2_'
            nspectxt = 10

        else if (l_mastercomp .eq. mastercompindx_glysoa_sfc_aer) then
            lptr_dum = lptr_glysoa_sfc_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_glysoa_sfc_aer
            spectxt = 'glysoa_sfc_'
            nspectxt = 11

        else if (l_mastercomp .eq. mastercompindx_glysoa_nh4_aer) then
            lptr_dum = lptr_glysoa_nh4_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_glysoa_nh4_aer
            spectxt = 'glysoa_nh4_'
            nspectxt = 11

        else if (l_mastercomp .eq. mastercompindx_glysoa_oh_aer) then
            lptr_dum = lptr_glysoa_oh_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_glysoa_oh_aer
            spectxt = 'glysoa_oh_'
            nspectxt = 10


        else if (l_mastercomp .eq. mastercompindx_asoaX_aer) then
            lptr_dum = lptr_asoaX_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_asoaX_aer
            spectxt = 'asoaX_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_asoa1_aer) then
            lptr_dum = lptr_asoa1_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_asoa1_aer
            spectxt = 'asoa1_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_asoa2_aer) then
            lptr_dum = lptr_asoa2_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_asoa2_aer
            spectxt = 'asoa2_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_asoa3_aer) then
            lptr_dum = lptr_asoa3_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_asoa3_aer
            spectxt = 'asoa3_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_asoa4_aer) then
            lptr_dum = lptr_asoa4_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_asoa4_aer
            spectxt = 'asoa4_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_bsoaX_aer) then
            lptr_dum = lptr_bsoaX_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_bsoaX_aer
            spectxt = 'bsoaX_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_bsoa1_aer) then
            lptr_dum = lptr_bsoa1_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_bsoa1_aer
            spectxt = 'bsoa1_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_bsoa2_aer) then
            lptr_dum = lptr_bsoa2_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_bsoa2_aer
            spectxt = 'bsoa2_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_bsoa3_aer) then
            lptr_dum = lptr_bsoa3_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_bsoa3_aer
            spectxt = 'bsoa3_'
            nspectxt = 6

        else if (l_mastercomp .eq. mastercompindx_bsoa4_aer) then
            lptr_dum = lptr_bsoa4_aer(n,itype,iphase)
            mcindx_dum = mastercompindx_bsoa4_aer
            spectxt = 'bsoa4_'
            nspectxt = 6
	else
	    goto 2500
	end if
	
	    
	if (lptr_dum .gt. lmaxd) then
! rce 2005-mar-14 - added check for lptr_dum > lmaxd
	    write( msg, '(a,3(1x,i4))' ) 'itype, isize, iphase =',   &
		itype, isize, iphase
	    call peg_message( lunout, msg )
	    write( msg, '(a,3(1x,i4))' ) 'l_mastercomp, lptr_dum, lmaxd =',   &
		l_mastercomp, lptr_dum, lmaxd
	    call peg_message( lunout, msg )
	    msg = '*** subr init_data_mosaic_ptr error - lptr_dum > lmaxd'
	    call peg_error_fatal( lunerr, msg )

	else if (lptr_dum .ge. p1st) then

	    ncomp_dum(isize,iphase) = ncomp_dum(isize,iphase) + iaddto_ncomp
	    ncomp_plustracer_dum(isize,iphase) =   &
		ncomp_plustracer_dum(isize,iphase) + iaddto_ncomp_plustracer

	    name(lptr_dum) =   &
		spectxt(1:nspectxt) // phasetxt(1:nphasetxt) //   &
		sizetxt(1:nsizetxt) //  typetxt(1:ntypetxt)

	    if (l_mastercomp .eq. -2) then
!		(numptr_aer is already set)
		mprognum_aer(n,itype,iphase) = 1

	    else if (l_mastercomp .eq. -1) then
!		(waterptr_aer is already set)
		continue

	    else if (l_mastercomp .eq. 0) then
!		(hyswptr_aer is already set)
		continue

	    else if (l_mastercomp .gt. 0) then
		ll = ncomp_plustracer_dum(isize,iphase)
! Note massptr_aer holds the species type l1, size bin n and is used at many places in movesect and mosaic drydep code
		massptr_aer(ll,n,itype,iphase) = lptr_dum
		mastercompptr_aer(ll,itype) = mcindx_dum

		name_aer(ll,itype) = name_mastercomp_aer(mcindx_dum)
		dens_aer(ll,itype) = dens_mastercomp_aer(mcindx_dum)
		mw_aer(ll,itype) = mw_mastercomp_aer(mcindx_dum)
		hygro_aer(ll,itype) = hygro_mastercomp_aer(mcindx_dum)

	    end if

	end if

2500	continue	! l_mastercomp = -1, ntot_mastercomp_aer

2600	continue	! iphase = 1, nphase_aer

2700	continue	! isize = 1, nsize_aer(itype)


!   now set ncomp_aer and ncomp_plustracer_aer, 
!   *** and check that the values computed for each size and phase all match
	ncomp_aer(itype) = ncomp_dum(1,ai_phase)
	ncomp_plustracer_aer(itype) = ncomp_plustracer_dum(1,ai_phase)

	do iphase = 1, nphase_aer
	do isize = 1, nsize_aer(itype)
	    if (ncomp_aer(itype) .ne. ncomp_dum(isize,iphase)) then
	        msg =  '*** subr init_data_mosaic_ptr - ' //   &
		    'ncomp_aer .ne. ncomp_dum'
		call peg_message( lunerr, msg )
		write(msg,9350) 'isize, itype, iphase =', isize, itype, iphase
		call peg_message( lunerr, msg )
		write(msg,9350) 'ncomp_aer, ncomp_dum =',   &
		    ncomp_aer(itype), ncomp_dum(isize,iphase)
		call peg_error_fatal( lunerr, msg )
	    end if
	    if (ncomp_plustracer_aer(itype) .ne.   &
			ncomp_plustracer_dum(isize,iphase)) then
	        msg = '*** subr init_data_mosaic_ptr - ' //   &
		    'ncomp_plustracer_aer .ne. ncomp_plustracer_dum'
		call peg_message( lunerr, msg )
		write(msg,9350) 'isize, itype, iphase =', isize, itype, iphase
		call peg_message( lunerr, msg )
		write(msg,9350)   &
		    'ncomp_plustracer_aer, ncomp_plustracer_dum =',   &
		    ncomp_plustracer_aer(itype),   &
		    ncomp_plustracer_dum(isize,iphase)
		call peg_error_fatal( lunerr, msg )
	    end if
	end do
	end do


2800	continue	! itype = 1, ntype_aer


9320	format( a, i1, i1, a, 8x )

!
!   output wrfch pointers
!
9350	format( a, 32(1x,i4) )
	msg = ' '
	call peg_message( lunout, msg )
	msg = 'output from subr init_data_mosaic_ptr'
	call peg_message( lunout, msg )
	write(msg,9350) 'nphase_aer =     ', nphase_aer
	call peg_message( lunout, msg )

	do iphase=1,nphase_aer

	write(msg,9350) 'iphase =     ', iphase
	call peg_message( lunout, msg )
	write(msg,9350) 'ntype_aer =     ', ntype_aer
	call peg_message( lunout, msg )

        write(msg,9350) 'ncomp_aer =     ', ncomp_aer
        call peg_message( lunout, msg )

	do itype=1,ntype_aer

	write(msg,9350) 'itype =     ', itype
	call peg_message( lunout, msg )
	write(msg,9350) 'nsize_aer = ', nsize_aer(itype)
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_so4_aer ',   &
		(lptr_so4_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_no3_aer ',   &
		(lptr_no3_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_cl_aer  ',   &
		(lptr_cl_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_msa_aer ',   &
		(lptr_msa_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_co3_aer ',   &
		(lptr_co3_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_nh4_aer ',   &
		(lptr_nh4_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_na_aer  ',   &
		(lptr_na_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_ca_aer  ',   &
		(lptr_ca_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_oin_aer ',   &
		(lptr_oin_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_oc_aer  ',   &
		(lptr_oc_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'lptr_bc_aer  ',   &
		(lptr_bc_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'hyswptr_aer',   &
		(hyswptr_aer(n,itype), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'waterptr_aer  ',   &
		(waterptr_aer(n,itype), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	write(msg,9350) 'numptr_aer     ',   &
		(numptr_aer(n,itype,iphase), n=1,nsize_aer(itype))
	call peg_message( lunout, msg )
	
! for all "write(msg)..." lines below: added missing CALL peg_message to actually write out the message
        write(msg,9350) 'lptr_pcg1_b_c_aer ',   &
                (lptr_pcg1_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg2_b_c_aer ',   &
                (lptr_pcg2_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg3_b_c_aer ',   &
                (lptr_pcg3_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg4_b_c_aer ',   &
                (lptr_pcg4_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg5_b_c_aer ',   &
                (lptr_pcg5_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg6_b_c_aer ',   &
                (lptr_pcg6_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg7_b_c_aer ',   &
                (lptr_pcg7_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg8_b_c_aer ',   &
                (lptr_pcg8_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg9_b_c_aer ',   &
                (lptr_pcg9_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg1_b_o_aer ',   &
                (lptr_pcg1_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg2_b_o_aer ',   &
                (lptr_pcg2_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg3_b_o_aer ',   &
                (lptr_pcg3_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg4_b_o_aer ',   &
                (lptr_pcg4_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg5_b_o_aer ',   &
                (lptr_pcg5_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg6_b_o_aer ',   &
                (lptr_pcg6_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg7_b_o_aer ',   &
                (lptr_pcg7_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg8_b_o_aer ',   &
                (lptr_pcg8_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg9_b_o_aer ',   &
                (lptr_pcg9_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg1_b_c_aer ',   &
                (lptr_opcg1_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg2_b_c_aer ',   &
                (lptr_opcg2_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg3_b_c_aer ',   &
                (lptr_opcg3_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg4_b_c_aer ',   &
                (lptr_opcg4_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg5_b_c_aer ',   &
                (lptr_opcg5_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg6_b_c_aer ',   &
                (lptr_opcg6_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg7_b_c_aer ',   &
                (lptr_opcg7_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg8_b_c_aer ',   &
                (lptr_opcg8_b_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg1_b_o_aer ',   &
                (lptr_opcg1_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg2_b_o_aer ',   &
                (lptr_opcg2_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg3_b_o_aer ',   &
                (lptr_opcg3_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg4_b_o_aer ',   &
                (lptr_opcg4_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg5_b_o_aer ',   &
                (lptr_opcg5_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg6_b_o_aer ',   &
                (lptr_opcg6_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg7_b_o_aer ',   &
                (lptr_opcg7_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg8_b_o_aer ',   &
                (lptr_opcg8_b_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg1_f_c_aer ',   &
                (lptr_pcg1_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg2_f_c_aer ',   &
                (lptr_pcg2_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg3_f_c_aer ',   &
                (lptr_pcg3_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg4_f_c_aer ',   &
                (lptr_pcg4_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg5_f_c_aer ',   &
                (lptr_pcg5_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg6_f_c_aer ',   &
                (lptr_pcg6_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg7_f_c_aer ',   &
                (lptr_pcg7_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg8_f_c_aer ',   &
                (lptr_pcg8_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg9_f_c_aer ',   &
                (lptr_pcg9_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg1_f_o_aer ',   &
                (lptr_pcg1_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg2_f_o_aer ',   &
                (lptr_pcg2_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg3_f_o_aer ',   &
                (lptr_pcg3_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg4_f_o_aer ',   &
                (lptr_pcg4_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg5_f_o_aer ',   &
                (lptr_pcg5_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg6_f_o_aer ',   &
                (lptr_pcg6_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg7_f_o_aer ',   &
                (lptr_pcg7_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg8_f_o_aer ',   &
                (lptr_pcg8_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_pcg9_f_o_aer ',   &
                (lptr_pcg9_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg1_f_c_aer ',   &
                (lptr_opcg1_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg2_f_c_aer ',   &
                (lptr_opcg2_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg3_f_c_aer ',   &
                (lptr_opcg3_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg4_f_c_aer ',   &
                (lptr_opcg4_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg5_f_c_aer ',   &
                (lptr_opcg5_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg6_f_c_aer ',   &
                (lptr_opcg6_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg7_f_c_aer ',   &
                (lptr_opcg7_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg8_f_c_aer ',   &
                (lptr_opcg8_f_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg1_f_o_aer ',   &
                (lptr_opcg1_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg2_f_o_aer ',   &
                (lptr_opcg2_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg3_f_o_aer ',   &
                (lptr_opcg3_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg4_f_o_aer ',   &
                (lptr_opcg4_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg5_f_o_aer ',   &
                (lptr_opcg5_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg6_f_o_aer ',   &
                (lptr_opcg6_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg7_f_o_aer ',   &
                (lptr_opcg7_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'lptr_opcg8_f_o_aer ',   &
                (lptr_opcg8_f_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )

        write(msg,9350) 'ant1_c_aer ',   &
                (lptr_ant1_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant2_c_aer ',   &
                (lptr_ant2_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant3_c_aer ',   &
                (lptr_ant3_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant4_c_aer ',   &
                (lptr_ant4_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant1_o_aer ',   &
                (lptr_ant1_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant2_o_aer ',   &
                (lptr_ant2_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant3_o_aer ',   &
                (lptr_ant3_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'ant4_o_aer ',   &
                (lptr_ant4_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog1_c_aer ',   &
                (lptr_biog1_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog2_c_aer ',   &
                (lptr_biog2_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog3_c_aer ',   &
                (lptr_biog3_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog4_c_aer ',   &
                (lptr_biog4_c_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog1_o_aer ',   &
                (lptr_biog1_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog2_o_aer ',   &
                (lptr_biog2_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog3_o_aer ',   &
                (lptr_biog3_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'biog4_o_aer ',   &
                (lptr_biog4_o_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'smpa_aer ',   &
                (lptr_smpa_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'smpbb_aer ',   &
                (lptr_smpbb_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'glysoa_r1_aer ',   &
                (lptr_glysoa_r1_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'glysoa_r2_aer ',   &
                (lptr_glysoa_r2_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'glysoa_sfc_aer ',   &
                (lptr_glysoa_sfc_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'glysoa_nh4_aer ',   &
                (lptr_glysoa_nh4_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'glysoa_oh_aer ',   &
                (lptr_glysoa_oh_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'asoaX_aer ',   &
                (lptr_asoaX_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'asoa1_aer ',   &
                (lptr_asoa1_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'asoa2_aer ',   &
                (lptr_asoa2_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'asoa3_aer ',   &
                (lptr_asoa3_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'asoa4_aer ',   &
                (lptr_asoa4_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )

        write(msg,9350) 'bsoaX_aer ',   &
                (lptr_bsoaX_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'bsoa1_aer ',   &
                (lptr_bsoa1_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'bsoa2_aer ',   &
                (lptr_bsoa2_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'bsoa3_aer ',   &
                (lptr_bsoa3_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )
        write(msg,9350) 'bsoa4_aer ',   &
                (lptr_bsoa4_aer(n,itype,iphase), n=1,nsize_aer(itype))
        call peg_message( lunout, msg )

  write(*,*) " ---------------------- "




	do ll = 1, ncomp_plustracer_aer(itype)
! added writeout of name_aer to understand which mastercomps are used
!	    write(msg,9350) 'massptr_aer(), ll',   &
	    write(msg,9350) name_aer(ll,itype),   &
		(massptr_aer(ll,n,itype,iphase), n=1,nsize_aer(itype)), ll
	    call peg_message( lunout, msg )
	end do
	end do ! type
	end do ! phase

!
!   check aerosol species pointers for "validity"
!
	do iphase=1,nphase_aer
	do itype=1,ntype_aer
	y_so4 = 0
	y_no3 = 0
	y_cl = 0
	y_msa = 0
	y_co3 = 0
	y_nh4 = 0
	y_na = 0
	y_ca = 0
	y_oin = 0
	y_oc = 0
	y_bc = 0
	y_hysw = 0
	y_water = 0
	y_num = 0
        y_pcg1_b_c=0
        y_pcg2_b_c=0
        y_pcg3_b_c=0
        y_pcg4_b_c=0
        y_pcg5_b_c=0
        y_pcg6_b_c=0
        y_pcg7_b_c=0
        y_pcg8_b_c=0
        y_pcg9_b_c=0
        y_pcg1_b_o=0
        y_pcg2_b_o=0
        y_pcg3_b_o=0
        y_pcg4_b_o=0
        y_pcg5_b_o=0
        y_pcg6_b_o=0
        y_pcg7_b_o=0
        y_pcg8_b_o=0
        y_pcg9_b_o=0
        y_opcg1_b_c=0
        y_opcg2_b_c=0
        y_opcg3_b_c=0
        y_opcg4_b_c=0
        y_opcg5_b_c=0
        y_opcg6_b_c=0
        y_opcg7_b_c=0
        y_opcg8_b_c=0
        y_opcg1_b_o=0
        y_opcg2_b_o=0
        y_opcg3_b_o=0
        y_opcg4_b_o=0
        y_opcg5_b_o=0
        y_opcg6_b_o=0
        y_opcg7_b_o=0
        y_opcg8_b_o=0
        y_pcg1_f_c=0
        y_pcg2_f_c=0
        y_pcg3_f_c=0
        y_pcg4_f_c=0
        y_pcg5_f_c=0
        y_pcg6_f_c=0
        y_pcg7_f_c=0
        y_pcg8_f_c=0
        y_pcg9_f_c=0
        y_pcg1_f_o=0
        y_pcg2_f_o=0
        y_pcg3_f_o=0
        y_pcg4_f_o=0
        y_pcg5_f_o=0
        y_pcg6_f_o=0
        y_pcg7_f_o=0
        y_pcg8_f_o=0
        y_pcg9_f_o=0
        y_opcg1_f_c=0
        y_opcg2_f_c=0
        y_opcg3_f_c=0
        y_opcg4_f_c=0
        y_opcg5_f_c=0
        y_opcg6_f_c=0
        y_opcg7_f_c=0
        y_opcg8_f_c=0
        y_opcg1_f_o=0
        y_opcg2_f_o=0
        y_opcg3_f_o=0
        y_opcg4_f_o=0
        y_opcg5_f_o=0
        y_opcg6_f_o=0
        y_opcg7_f_o=0
        y_opcg8_f_o=0
        y_ant1_c=0
        y_ant2_c=0
        y_ant3_c=0
        y_ant4_c=0
        y_ant1_o=0
        y_ant2_o=0
        y_ant3_o=0
        y_ant4_o=0
        y_biog1_c=0
        y_biog2_c=0
        y_biog3_c=0
        y_biog4_c=0
        y_biog1_o=0
        y_biog2_o=0
        y_biog3_o=0
        y_biog4_o=0
        y_smpa=0
        y_smpbb=0
        y_glysoa_r1=0
        y_glysoa_r2=0
        y_glysoa_sfc=0
        y_glysoa_nh4=0
        y_glysoa_oh=0
        y_asoaX=0
        y_asoa1=0
        y_asoa2=0
        y_asoa3=0
        y_asoa4=0
        y_bsoaX=0
        y_bsoa1=0
        y_bsoa2=0
        y_bsoa3=0
        y_bsoa4=0

    

	do n = 1, nsize_aer(itype)
	    if (lptr_so4_aer(n,itype,iphase) .ge. p1st) y_so4 = y_so4 + 1
	    if (lptr_no3_aer(n,itype,iphase) .ge. p1st) y_no3 = y_no3 + 1
	    if (lptr_cl_aer(n,itype,iphase)  .ge. p1st) y_cl  = y_cl + 1
	    if (lptr_msa_aer(n,itype,iphase) .ge. p1st) y_msa = y_msa + 1
	    if (lptr_co3_aer(n,itype,iphase) .ge. p1st) y_co3 = y_co3 + 1
	    if (lptr_nh4_aer(n,itype,iphase) .ge. p1st) y_nh4 = y_nh4 + 1
	    if (lptr_na_aer(n,itype,iphase)  .ge. p1st) y_na  = y_na + 1
	    if (lptr_ca_aer(n,itype,iphase)  .ge. p1st) y_ca  = y_ca + 1
	    if (lptr_oin_aer(n,itype,iphase) .ge. p1st) y_oin = y_oin + 1
	    if (lptr_oc_aer(n,itype,iphase)  .ge. p1st) y_oc  = y_oc + 1
	    if (lptr_bc_aer(n,itype,iphase)  .ge. p1st) y_bc  = y_bc + 1
	    if (hyswptr_aer(n,itype)    .ge. p1st) y_hysw = y_hysw + 1
	    if (waterptr_aer(n,itype)        .ge. p1st) y_water = y_water + 1
	    if (numptr_aer(n,itype,iphase)   .ge. p1st) y_num = y_num + 1
            if (lptr_pcg1_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg1_b_c = y_pcg1_b_c + 1
            if (lptr_pcg2_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg2_b_c = y_pcg2_b_c + 1
            if (lptr_pcg3_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg3_b_c = y_pcg3_b_c + 1
            if (lptr_pcg4_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg4_b_c = y_pcg4_b_c + 1
            if (lptr_pcg5_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg5_b_c = y_pcg5_b_c + 1
            if (lptr_pcg6_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg6_b_c = y_pcg6_b_c + 1
            if (lptr_pcg7_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg7_b_c = y_pcg7_b_c + 1
            if (lptr_pcg8_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg8_b_c = y_pcg8_b_c + 1
            if (lptr_pcg9_b_c_aer(n,itype,iphase) .ge. p1st) y_pcg9_b_c = y_pcg9_b_c + 1
            if (lptr_pcg1_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg1_b_o = y_pcg1_b_o + 1
            if (lptr_pcg2_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg2_b_o = y_pcg2_b_o + 1
            if (lptr_pcg3_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg3_b_o = y_pcg3_b_o + 1
            if (lptr_pcg4_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg4_b_o = y_pcg4_b_o + 1
            if (lptr_pcg5_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg5_b_o = y_pcg5_b_o + 1
            if (lptr_pcg6_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg6_b_o = y_pcg6_b_o + 1
            if (lptr_pcg7_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg7_b_o = y_pcg7_b_o + 1
            if (lptr_pcg8_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg8_b_o = y_pcg8_b_o + 1
            if (lptr_pcg9_b_o_aer(n,itype,iphase) .ge. p1st) y_pcg9_b_o = y_pcg9_b_o + 1
            if (lptr_opcg1_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg1_b_c = y_opcg1_b_c + 1
            if (lptr_opcg2_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg2_b_c = y_opcg2_b_c + 1
            if (lptr_opcg3_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg3_b_c = y_opcg3_b_c + 1
            if (lptr_opcg4_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg4_b_c = y_opcg4_b_c + 1
            if (lptr_opcg5_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg5_b_c = y_opcg5_b_c + 1
            if (lptr_opcg6_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg6_b_c = y_opcg6_b_c + 1
            if (lptr_opcg7_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg7_b_c = y_opcg7_b_c + 1
            if (lptr_opcg8_b_c_aer(n,itype,iphase) .ge. p1st) y_opcg8_b_c = y_opcg8_b_c + 1
            if (lptr_opcg1_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg1_b_o = y_opcg1_b_o + 1
            if (lptr_opcg2_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg2_b_o = y_opcg2_b_o + 1
            if (lptr_opcg3_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg3_b_o = y_opcg3_b_o + 1
            if (lptr_opcg4_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg4_b_o = y_opcg4_b_o + 1
            if (lptr_opcg5_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg5_b_o = y_opcg5_b_o + 1
            if (lptr_opcg6_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg6_b_o = y_opcg6_b_o + 1
            if (lptr_opcg7_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg7_b_o = y_opcg7_b_o + 1
            if (lptr_opcg8_b_o_aer(n,itype,iphase) .ge. p1st) y_opcg8_b_o = y_opcg8_b_o + 1
            if (lptr_pcg1_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg1_f_c = y_pcg1_f_c + 1
            if (lptr_pcg2_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg2_f_c = y_pcg2_f_c + 1
            if (lptr_pcg3_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg3_f_c = y_pcg3_f_c + 1
            if (lptr_pcg4_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg4_f_c = y_pcg4_f_c + 1
            if (lptr_pcg5_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg5_f_c = y_pcg5_f_c + 1
            if (lptr_pcg6_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg6_f_c = y_pcg6_f_c + 1
            if (lptr_pcg7_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg7_f_c = y_pcg7_f_c + 1
            if (lptr_pcg8_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg8_f_c = y_pcg8_f_c + 1
            if (lptr_pcg9_f_c_aer(n,itype,iphase) .ge. p1st) y_pcg9_f_c = y_pcg9_f_c + 1
            if (lptr_pcg1_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg1_f_o = y_pcg1_f_o + 1
            if (lptr_pcg2_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg2_f_o = y_pcg2_f_o + 1
            if (lptr_pcg3_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg3_f_o = y_pcg3_f_o + 1
            if (lptr_pcg4_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg4_f_o = y_pcg4_f_o + 1
            if (lptr_pcg5_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg5_f_o = y_pcg5_f_o + 1
            if (lptr_pcg6_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg6_f_o = y_pcg6_f_o + 1
            if (lptr_pcg7_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg7_f_o = y_pcg7_f_o + 1
            if (lptr_pcg8_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg8_f_o = y_pcg8_f_o + 1
            if (lptr_pcg9_f_o_aer(n,itype,iphase) .ge. p1st) y_pcg9_f_o = y_pcg9_f_o + 1
            if (lptr_opcg1_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg1_f_c = y_opcg1_f_c + 1
            if (lptr_opcg2_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg2_f_c = y_opcg2_f_c + 1
            if (lptr_opcg3_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg3_f_c = y_opcg3_f_c + 1
            if (lptr_opcg4_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg4_f_c = y_opcg4_f_c + 1
            if (lptr_opcg5_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg5_f_c = y_opcg5_f_c + 1
            if (lptr_opcg6_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg6_f_c = y_opcg6_f_c + 1
            if (lptr_opcg7_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg7_f_c = y_opcg7_f_c + 1
            if (lptr_opcg8_f_c_aer(n,itype,iphase) .ge. p1st) y_opcg8_f_c = y_opcg8_f_c + 1
            if (lptr_opcg1_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg1_f_o = y_opcg1_f_o + 1
            if (lptr_opcg2_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg2_f_o = y_opcg2_f_o + 1
            if (lptr_opcg3_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg3_f_o = y_opcg3_f_o + 1
            if (lptr_opcg4_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg4_f_o = y_opcg4_f_o + 1
            if (lptr_opcg5_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg5_f_o = y_opcg5_f_o + 1
            if (lptr_opcg6_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg6_f_o = y_opcg6_f_o + 1
            if (lptr_opcg7_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg7_f_o = y_opcg7_f_o + 1
            if (lptr_opcg8_f_o_aer(n,itype,iphase) .ge. p1st) y_opcg8_f_o = y_opcg8_f_o + 1
            if (lptr_ant1_c_aer(n,itype,iphase) .ge. p1st) y_ant1_c = y_ant1_c + 1
            if (lptr_ant2_c_aer(n,itype,iphase) .ge. p1st) y_ant2_c = y_ant2_c + 1
            if (lptr_ant3_c_aer(n,itype,iphase) .ge. p1st) y_ant3_c = y_ant3_c + 1
            if (lptr_ant4_c_aer(n,itype,iphase) .ge. p1st) y_ant4_c = y_ant4_c + 1
            if (lptr_ant1_o_aer(n,itype,iphase) .ge. p1st) y_ant1_o = y_ant1_o + 1
            if (lptr_ant2_o_aer(n,itype,iphase) .ge. p1st) y_ant2_o = y_ant2_o + 1
            if (lptr_ant3_o_aer(n,itype,iphase) .ge. p1st) y_ant3_o = y_ant3_o + 1
            if (lptr_ant4_o_aer(n,itype,iphase) .ge. p1st) y_ant4_o = y_ant4_o + 1
            if (lptr_biog1_c_aer(n,itype,iphase) .ge. p1st) y_biog1_c = y_biog1_c + 1
            if (lptr_biog2_c_aer(n,itype,iphase) .ge. p1st) y_biog2_c = y_biog2_c + 1
            if (lptr_biog3_c_aer(n,itype,iphase) .ge. p1st) y_biog3_c = y_biog3_c + 1
            if (lptr_biog4_c_aer(n,itype,iphase) .ge. p1st) y_biog4_c = y_biog4_c + 1
            if (lptr_biog1_o_aer(n,itype,iphase) .ge. p1st) y_biog1_o = y_biog1_o + 1
            if (lptr_biog2_o_aer(n,itype,iphase) .ge. p1st) y_biog2_o = y_biog2_o + 1
            if (lptr_biog3_o_aer(n,itype,iphase) .ge. p1st) y_biog3_o = y_biog3_o + 1
            if (lptr_biog4_o_aer(n,itype,iphase) .ge. p1st) y_biog4_o = y_biog4_o + 1
            if (lptr_smpa_aer(n,itype,iphase) .ge. p1st) y_smpa = y_smpa + 1
            if (lptr_smpbb_aer(n,itype,iphase) .ge. p1st) y_smpbb = y_smpbb + 1
            if (lptr_glysoa_r1_aer(n,itype,iphase) .ge. p1st) y_glysoa_r1 = y_glysoa_r1 + 1
            if (lptr_glysoa_r2_aer(n,itype,iphase) .ge. p1st) y_glysoa_r2 = y_glysoa_r2 + 1
            if (lptr_glysoa_sfc_aer(n,itype,iphase) .ge. p1st) y_glysoa_sfc = y_glysoa_sfc + 1
            if (lptr_glysoa_nh4_aer(n,itype,iphase) .ge. p1st) y_glysoa_nh4 = y_glysoa_nh4 + 1
            if (lptr_glysoa_oh_aer(n,itype,iphase) .ge. p1st) y_glysoa_oh = y_glysoa_oh + 1
            if (lptr_asoaX_aer(n,itype,iphase) .ge. p1st) y_asoaX = y_asoaX + 1
            if (lptr_asoa1_aer(n,itype,iphase) .ge. p1st) y_asoa1 = y_asoa1 + 1
            if (lptr_asoa2_aer(n,itype,iphase) .ge. p1st) y_asoa2 = y_asoa2 + 1
            if (lptr_asoa3_aer(n,itype,iphase) .ge. p1st) y_asoa3 = y_asoa3 + 1
            if (lptr_asoa4_aer(n,itype,iphase) .ge. p1st) y_asoa4 = y_asoa4 + 1
            if (lptr_bsoaX_aer(n,itype,iphase) .ge. p1st) y_bsoaX = y_bsoaX + 1
            if (lptr_bsoa1_aer(n,itype,iphase) .ge. p1st) y_bsoa1 = y_bsoa1 + 1
            if (lptr_bsoa2_aer(n,itype,iphase) .ge. p1st) y_bsoa2 = y_bsoa2 + 1
            if (lptr_bsoa3_aer(n,itype,iphase) .ge. p1st) y_bsoa3 = y_bsoa3 + 1
            if (lptr_bsoa4_aer(n,itype,iphase) .ge. p1st) y_bsoa4 = y_bsoa4 + 1

	end do

!   these must be defined for all aerosol bins
	if (y_so4 .ne. nsize_aer(itype)) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for so4'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if (y_water .ne. nsize_aer(itype)) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for water'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if (y_num .ne. nsize_aer(itype)) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for num'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	end if


!   these must be defined for all aerosol bins
!       or else undefined for all aerosol bins
	if      ((y_no3 .ne. 0) .and.   &
	         (y_no3 .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for no3'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
        else if ((y_cl .ne. 0) .and.   &
                 (y_cl .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for cl'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_pcg1_b_c .ne. 0) .and.   &
                 (y_pcg1_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg1_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg2_b_c .ne. 0) .and.   &
                 (y_pcg2_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg2_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg3_b_c .ne. 0) .and.   &
                 (y_pcg3_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg3_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg4_b_c .ne. 0) .and.   &
                 (y_pcg4_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg4_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg5_b_c .ne. 0) .and.   &
                 (y_pcg5_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg5_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg6_b_c .ne. 0) .and.   &
                 (y_pcg6_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg6_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg7_b_c .ne. 0) .and.   &
                 (y_pcg7_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg7_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg8_b_c .ne. 0) .and.   &
                 (y_pcg8_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg8_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg9_b_c .ne. 0) .and.   &
                 (y_pcg9_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg9_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg1_b_o .ne. 0) .and.   &
                 (y_pcg1_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg1_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg2_b_o .ne. 0) .and.   &
                 (y_pcg2_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg2_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg3_b_o .ne. 0) .and.   &
                 (y_pcg3_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg3_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg4_b_o .ne. 0) .and.   &
                 (y_pcg4_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg4_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg5_b_o .ne. 0) .and.   &
                 (y_pcg5_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg5_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg6_b_o .ne. 0) .and.   &
                 (y_pcg6_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg6_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg7_b_o .ne. 0) .and.   &
                 (y_pcg7_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg7_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg8_b_o .ne. 0) .and.   &
                 (y_pcg8_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg8_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg9_b_o .ne. 0) .and.   &
                 (y_pcg9_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg9_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg1_b_o .ne. 0) .and.   &
                 (y_opcg1_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg1_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg2_b_o .ne. 0) .and.   &
                 (y_opcg2_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg2_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg3_b_o .ne. 0) .and.   &
                 (y_opcg3_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg3_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg4_b_o .ne. 0) .and.   &
                 (y_opcg4_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg4_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg5_b_o .ne. 0) .and.   &
                 (y_opcg5_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg5_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg6_b_o .ne. 0) .and.   &
                 (y_opcg6_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg6_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg7_b_o .ne. 0) .and.   &
                 (y_opcg7_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg7_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg8_b_o .ne. 0) .and.   &
                 (y_opcg8_b_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg8_b_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg1_b_c .ne. 0) .and.   &
                 (y_opcg1_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg1_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg2_b_c .ne. 0) .and.   &
                 (y_opcg2_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg2_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg3_b_c .ne. 0) .and.   &
                 (y_opcg3_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg3_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg4_b_c .ne. 0) .and.   &
                 (y_opcg4_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg4_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg5_b_c .ne. 0) .and.   &
                 (y_opcg5_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg5_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg6_b_c .ne. 0) .and.   &
                 (y_opcg6_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg6_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg7_b_c .ne. 0) .and.   &
                 (y_opcg7_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg7_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg8_b_c .ne. 0) .and.   &
                 (y_opcg8_b_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg8_b_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg1_f_c .ne. 0) .and.   &
                 (y_pcg1_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg1_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg2_f_c .ne. 0) .and.   &
                 (y_pcg2_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg2_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg3_f_c .ne. 0) .and.   &
                 (y_pcg3_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg3_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg4_f_c .ne. 0) .and.   &
                 (y_pcg4_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg4_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg5_f_c .ne. 0) .and.   &
                 (y_pcg5_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg5_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg6_f_c .ne. 0) .and.   &
                 (y_pcg6_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg6_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg7_f_c .ne. 0) .and.   &
                 (y_pcg7_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg7_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg8_f_c .ne. 0) .and.   &
                 (y_pcg8_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg8_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg9_f_c .ne. 0) .and.   &
                 (y_pcg9_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg9_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg1_f_o .ne. 0) .and.   &
                 (y_pcg1_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg1_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg2_f_o .ne. 0) .and.   &
                 (y_pcg2_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg2_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg3_f_o .ne. 0) .and.   &
                 (y_pcg3_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg3_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg4_f_o .ne. 0) .and.   &
                 (y_pcg4_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg4_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg5_f_o .ne. 0) .and.   &
                 (y_pcg5_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg5_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg6_f_o .ne. 0) .and.   &
                 (y_pcg6_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg6_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg7_f_o .ne. 0) .and.   &
                 (y_pcg7_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg7_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg8_f_o .ne. 0) .and.   &
                 (y_pcg8_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg8_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_pcg9_f_o .ne. 0) .and.   &
                 (y_pcg9_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for pcg9_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg1_f_o .ne. 0) .and.   &
                 (y_opcg1_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg1_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg2_f_o .ne. 0) .and.   &
                 (y_opcg2_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg2_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg3_f_o .ne. 0) .and.   &
                 (y_opcg3_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg3_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg4_f_o .ne. 0) .and.   &
                 (y_opcg4_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg4_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg5_f_o .ne. 0) .and.   &
                 (y_opcg5_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg5_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg6_f_o .ne. 0) .and.   &
                 (y_opcg6_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg6_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg7_f_o .ne. 0) .and.   &
                 (y_opcg7_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg7_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg8_f_o .ne. 0) .and.   &
                 (y_opcg8_f_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg8_f_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg1_f_c .ne. 0) .and.   &
                 (y_opcg1_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg1_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg2_f_c .ne. 0) .and.   &
                 (y_opcg2_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg2_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg3_f_c .ne. 0) .and.   &
                 (y_opcg3_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg3_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg4_f_c .ne. 0) .and.   &
                 (y_opcg4_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg4_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg5_f_c .ne. 0) .and.   &
                 (y_opcg5_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg5_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg6_f_c .ne. 0) .and.   &
                 (y_opcg6_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg6_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg7_f_c .ne. 0) .and.   &
                 (y_opcg7_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg7_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
        else if ((y_opcg8_f_c .ne. 0) .and.   &
                 (y_opcg8_f_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for opcg8_f_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant1_c .ne. 0) .and.   &
                 (y_ant1_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant1_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant2_c .ne. 0) .and.   &
                 (y_ant2_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant2_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant3_c .ne. 0) .and.   &
                 (y_ant3_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant3_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant4_c .ne. 0) .and.   &
                 (y_ant4_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant4_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )


        else if ((y_ant1_o .ne. 0) .and.   &
                 (y_ant1_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant1_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant2_o .ne. 0) .and.   &
                 (y_ant2_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant2_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant3_o .ne. 0) .and.   &
                 (y_ant3_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant3_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_ant4_o .ne. 0) .and.   &
                 (y_ant4_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for ant4_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog1_c .ne. 0) .and.   &
                 (y_biog1_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog1_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog2_c .ne. 0) .and.   &
                 (y_biog2_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog2_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog3_c .ne. 0) .and.   &
                 (y_biog3_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog3_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog4_c .ne. 0) .and.   &
                 (y_biog4_c .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog4_c'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )


        else if ((y_biog1_o .ne. 0) .and.   &
                 (y_biog1_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog1_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog2_o .ne. 0) .and.   &
                 (y_biog2_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog2_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog3_o .ne. 0) .and.   &
                 (y_biog3_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog3_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_biog4_o .ne. 0) .and.   &
                 (y_biog4_o .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for biog4_o'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_smpa .ne. 0) .and.   &
                 (y_smpa .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for smpa'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_smpbb .ne. 0) .and.   &
                 (y_smpbb .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for smpbb'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_glysoa_r1 .ne. 0) .and.   &
                 (y_glysoa_r1 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for glysoa_r1'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_glysoa_r2 .ne. 0) .and.   &
                 (y_glysoa_r2 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for glysoa_r2'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_glysoa_sfc .ne. 0) .and.   &
                 (y_glysoa_sfc .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for glysoa_sfc'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
            
        else if ((y_glysoa_nh4 .ne. 0) .and.   &
                 (y_glysoa_nh4 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for glysoa_nh4'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_glysoa_oh .ne. 0) .and.   &
                 (y_glysoa_oh .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for glysoa_oh'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )


        else if ((y_asoaX .ne. 0) .and.   &
                 (y_asoaX .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_asoaX'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_asoa1 .ne. 0) .and.   &
                 (y_asoa1 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_asoa1'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_asoa2 .ne. 0) .and.   &
                 (y_asoa2 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_asoa2'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_asoa3 .ne. 0) .and.   &
                 (y_asoa3 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_asoa3'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_asoa4 .ne. 0) .and.   &
                 (y_asoa4 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_asoa4'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_bsoaX .ne. 0) .and.   &
                 (y_bsoaX .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_bsoaX'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_bsoa1 .ne. 0) .and.   &
                 (y_bsoa1 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_bsoa1'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_bsoa2 .ne. 0) .and.   &
                 (y_bsoa2 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_bsoa2'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_bsoa3 .ne. 0) .and.   &
                 (y_bsoa3 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_bsoa3'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )

        else if ((y_bsoa4 .ne. 0) .and.   &
                 (y_bsoa4 .ne. nsize_aer(itype))) then
            msg = '*** subr init_data_mosaic_ptr - ptr error for y_bsoa4'
            call peg_message( lunerr, msg )
            write(msg,9350) 'phase, type=', iphase,itype
            call peg_error_fatal( lunerr, msg )
	else if ((y_msa .ne. 0) .and.   &
	         (y_msa .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for msa'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_co3 .ne. 0) .and.   &
	         (y_co3 .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for co3'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_nh4 .ne. 0) .and.   &
	         (y_nh4 .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for nh4'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_na .ne. 0) .and.   &
	         (y_na .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for na'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_ca .ne. 0) .and.   &
	         (y_ca .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for ca'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_oin .ne. 0) .and.   &
	         (y_oin .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for oin'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_oc .ne. 0) .and.   &
	         (y_oc .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for oc'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_bc .ne. 0) .and.   &
	         (y_bc .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for bc'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	else if ((y_hysw .ne. 0) .and.   &
	         (y_hysw .ne. nsize_aer(itype))) then
	    msg = '*** subr init_data_mosaic_ptr - ptr error for hysw'
	    call peg_message( lunerr, msg )
	    write(msg,9350) 'phase, type=', iphase,itype
	    call peg_error_fatal( lunerr, msg )
	end if

	enddo ! type
	enddo ! phase
!
!   set pointers for gases
!   rce 2004-dec-02 - gases not required to be present
!
        if (p_h2so4 .ge. p1st) then 
            kh2so4 = p_h2so4
       
       elseif (p_sulf .ge. p1st) then
            kh2so4 = p_sulf
       
 
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for h2so4'
!	    call peg_error_fatal( lunerr, msg )
	end if
	if (p_hno3 .ge. p1st) then
	    khno3 = p_hno3
      endif 
       if (p_pcg1_b_c .ge. p1st) then
            kpcg1_b_c = p_pcg1_b_c
        endif
       if (p_pcg2_b_c .ge. p1st) then
            kpcg2_b_c = p_pcg2_b_c
        endif
       if (p_pcg3_b_c .ge. p1st) then
            kpcg3_b_c = p_pcg3_b_c
        endif
       if (p_pcg4_b_c .ge. p1st) then
            kpcg4_b_c = p_pcg4_b_c
        endif
       if (p_pcg5_b_c .ge. p1st) then
            kpcg5_b_c = p_pcg5_b_c
        endif
       if (p_pcg6_b_c .ge. p1st) then
            kpcg6_b_c = p_pcg6_b_c
        endif
       if (p_pcg7_b_c .ge. p1st) then
            kpcg7_b_c = p_pcg7_b_c
        endif
       if (p_pcg8_b_c .ge. p1st) then
            kpcg8_b_c = p_pcg8_b_c
        endif
       if (p_pcg9_b_c .ge. p1st) then
            kpcg9_b_c = p_pcg9_b_c
        endif
       if (p_pcg1_b_o .ge. p1st) then
            kpcg1_b_o = p_pcg1_b_o
        endif
       if (p_pcg2_b_o .ge. p1st) then
            kpcg2_b_o = p_pcg2_b_o
        endif
       if (p_pcg3_b_o .ge. p1st) then
            kpcg3_b_o = p_pcg3_b_o
        endif
       if (p_pcg4_b_o .ge. p1st) then
            kpcg4_b_o = p_pcg4_b_o
        endif
       if (p_pcg5_b_o .ge. p1st) then
            kpcg5_b_o = p_pcg5_b_o
        endif
       if (p_pcg6_b_o .ge. p1st) then
            kpcg6_b_o = p_pcg6_b_o
        endif
       if (p_pcg7_b_o .ge. p1st) then
            kpcg7_b_o = p_pcg7_b_o
        endif
       if (p_pcg8_b_o .ge. p1st) then
            kpcg8_b_o = p_pcg8_b_o
        endif
       if (p_pcg9_b_o .ge. p1st) then
            kpcg9_b_o = p_pcg9_b_o
        endif
       if (p_opcg1_b_o .ge. p1st) then
            kopcg1_b_o = p_opcg1_b_o
        endif
       if (p_opcg2_b_o .ge. p1st) then
            kopcg2_b_o = p_opcg2_b_o
        endif
       if (p_opcg3_b_o .ge. p1st) then
            kopcg3_b_o = p_opcg3_b_o
        endif
       if (p_opcg4_b_o .ge. p1st) then
            kopcg4_b_o = p_opcg4_b_o
        endif
       if (p_opcg5_b_o .ge. p1st) then
            kopcg5_b_o = p_opcg5_b_o
        endif
       if (p_opcg6_b_o .ge. p1st) then
            kopcg6_b_o = p_opcg6_b_o
        endif
       if (p_opcg7_b_o .ge. p1st) then
            kopcg7_b_o = p_opcg7_b_o
        endif
       if (p_opcg8_b_o .ge. p1st) then
            kopcg8_b_o = p_opcg8_b_o
        endif
       if (p_opcg1_b_c .ge. p1st) then
            kopcg1_b_c = p_opcg1_b_c
        endif
       if (p_opcg2_b_c .ge. p1st) then
            kopcg2_b_c = p_opcg2_b_c
        endif
       if (p_opcg3_b_c .ge. p1st) then
            kopcg3_b_c = p_opcg3_b_c
        endif
       if (p_opcg4_b_c .ge. p1st) then
            kopcg4_b_c = p_opcg4_b_c
        endif
       if (p_opcg5_b_c .ge. p1st) then
            kopcg5_b_c = p_opcg5_b_c
        endif
       if (p_opcg6_b_c .ge. p1st) then
            kopcg6_b_c = p_opcg6_b_c
        endif
       if (p_opcg7_b_c .ge. p1st) then
            kopcg7_b_c = p_opcg7_b_c
        endif
       if (p_opcg8_b_c .ge. p1st) then
            kopcg8_b_c = p_opcg8_b_c
        endif
       if (p_pcg1_f_c .ge. p1st) then
            kpcg1_f_c = p_pcg1_f_c
        endif
       if (p_pcg2_f_c .ge. p1st) then
            kpcg2_f_c = p_pcg2_f_c
        endif
       if (p_pcg3_f_c .ge. p1st) then
            kpcg3_f_c = p_pcg3_f_c
        endif
       if (p_pcg4_f_c .ge. p1st) then
            kpcg4_f_c = p_pcg4_f_c
        endif
       if (p_pcg5_f_c .ge. p1st) then
            kpcg5_f_c = p_pcg5_f_c
        endif
       if (p_pcg6_f_c .ge. p1st) then
            kpcg6_f_c = p_pcg6_f_c
        endif
       if (p_pcg7_f_c .ge. p1st) then
            kpcg7_f_c = p_pcg7_f_c
        endif
       if (p_pcg8_f_c .ge. p1st) then
            kpcg8_f_c = p_pcg8_f_c
        endif
       if (p_pcg9_f_c .ge. p1st) then
            kpcg9_f_c = p_pcg9_f_c
        endif
       if (p_pcg1_f_o .ge. p1st) then
            kpcg1_f_o = p_pcg1_f_o
        endif
       if (p_pcg2_f_o .ge. p1st) then
            kpcg2_f_o = p_pcg2_f_o
        endif
       if (p_pcg3_f_o .ge. p1st) then
            kpcg3_f_o = p_pcg3_f_o
        endif
       if (p_pcg4_f_o .ge. p1st) then
            kpcg4_f_o = p_pcg4_f_o
        endif
       if (p_pcg5_f_o .ge. p1st) then
            kpcg5_f_o = p_pcg5_f_o
        endif
       if (p_pcg6_f_o .ge. p1st) then
            kpcg6_f_o = p_pcg6_f_o
        endif
       if (p_pcg7_f_o .ge. p1st) then
            kpcg7_f_o = p_pcg7_f_o
        endif
       if (p_pcg8_f_o .ge. p1st) then
            kpcg8_f_o = p_pcg8_f_o
        endif
       if (p_pcg9_f_o .ge. p1st) then
            kpcg9_f_o = p_pcg9_f_o
        endif
       if (p_opcg1_f_o .ge. p1st) then
            kopcg1_f_o = p_opcg1_f_o
        endif
       if (p_opcg2_f_o .ge. p1st) then
            kopcg2_f_o = p_opcg2_f_o
        endif
       if (p_opcg3_f_o .ge. p1st) then
            kopcg3_f_o = p_opcg3_f_o
        endif
       if (p_opcg4_f_o .ge. p1st) then
            kopcg4_f_o = p_opcg4_f_o
        endif
       if (p_opcg5_f_o .ge. p1st) then
            kopcg5_f_o = p_opcg5_f_o
        endif
       if (p_opcg6_f_o .ge. p1st) then
            kopcg6_f_o = p_opcg6_f_o
        endif
       if (p_opcg7_f_o .ge. p1st) then
            kopcg7_f_o = p_opcg7_f_o
        endif
       if (p_opcg8_f_o .ge. p1st) then
            kopcg8_f_o = p_opcg8_f_o
        endif
       if (p_opcg1_f_c .ge. p1st) then
            kopcg1_f_c = p_opcg1_f_c
        endif
       if (p_opcg2_f_c .ge. p1st) then
            kopcg2_f_c = p_opcg2_f_c
        endif
       if (p_opcg3_f_c .ge. p1st) then
            kopcg3_f_c = p_opcg3_f_c
        endif
       if (p_opcg4_f_c .ge. p1st) then
            kopcg4_f_c = p_opcg4_f_c
        endif
       if (p_opcg5_f_c .ge. p1st) then
            kopcg5_f_c = p_opcg5_f_c
        endif
       if (p_opcg6_f_c .ge. p1st) then
            kopcg6_f_c = p_opcg6_f_c
        endif
       if (p_opcg7_f_c .ge. p1st) then
            kopcg7_f_c = p_opcg7_f_c
        endif
       if (p_opcg8_f_c .ge. p1st) then
            kopcg8_f_c = p_opcg8_f_c
        endif

       if (p_smpa .ge. p1st) then
            ksmpa = p_smpa
        endif
       if (p_smpbb .ge. p1st) then
            ksmpbb = p_smpbb
        endif

       if (p_gly .ge. p1st) then
           kgly = p_gly
       endif

       if (p_cvasoaX .ge. p1st) then
           kasoaX = p_cvasoaX
       endif

       if (p_cvasoa1 .ge. p1st) then
           kasoa1 = p_cvasoa1
       endif

       if (p_cvasoa2 .ge. p1st) then
           kasoa2 = p_cvasoa2
       endif

       if (p_cvasoa3 .ge. p1st) then
           kasoa3 = p_cvasoa3
       endif

       if (p_cvasoa4 .ge. p1st) then
           kasoa4 = p_cvasoa4
       endif

       if (p_cvbsoaX .ge. p1st) then
           kbsoaX = p_cvbsoaX
       endif

       if (p_cvbsoa1 .ge. p1st) then
           kbsoa1 = p_cvbsoa1
       endif

       if (p_cvbsoa2 .ge. p1st) then
           kbsoa2 = p_cvbsoa2
       endif

       if (p_cvbsoa3 .ge. p1st) then
           kbsoa3 = p_cvbsoa3
       endif

       if (p_cvbsoa4 .ge. p1st) then
           kbsoa4 = p_cvbsoa4
       endif
       if (p_ant1_c .ge. p1st) then
            kant1_c = p_ant1_c
        endif

       if (p_ant2_c .ge. p1st) then
            kant2_c = p_ant2_c
        endif
       if (p_ant3_c .ge. p1st) then
            kant3_c = p_ant3_c
        endif
       if (p_ant4_c .ge. p1st) then
            kant4_c = p_ant4_c
        endif
       if (p_ant1_o .ge. p1st) then
            kant1_o = p_ant1_o
        endif

       if (p_ant2_o .ge. p1st) then
            kant2_o = p_ant2_o
        endif
       if (p_ant3_o .ge. p1st) then
            kant3_o = p_ant3_o
        endif
       if (p_ant4_o .ge. p1st) then
            kant4_o = p_ant4_o
        endif

       if (p_biog1_c .ge. p1st) then
            kbiog1_c = p_biog1_c
        endif

       if (p_biog2_c .ge. p1st) then
            kbiog2_c = p_biog2_c
        endif
       if (p_biog3_c .ge. p1st) then
            kbiog3_c = p_biog3_c
        endif
       if (p_biog4_c .ge. p1st) then
            kbiog4_c = p_biog4_c
        endif
       if (p_biog1_o .ge. p1st) then
            kbiog1_o = p_biog1_o
        endif

       if (p_biog2_o .ge. p1st) then
            kbiog2_o = p_biog2_o
        endif
       if (p_biog3_o .ge. p1st) then
            kbiog3_o = p_biog3_o
        endif
       if (p_biog4_o .ge. p1st) then
            kbiog4_o = p_biog4_o
        endif

!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for hno3'
!	    call peg_error_fatal( lunerr, msg )
	if (p_hcl .ge. p1st) then
	    khcl = p_hcl
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for hcl'
!	    call peg_error_fatal( lunerr, msg )
	end if
	if (p_nh3 .ge. p1st) then
	    knh3 = p_nh3
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for nh3'
!	    call peg_error_fatal( lunerr, msg )
	end if
	if (p_n2o5 .ge. p1st) then
	    kn2o5 = p_n2o5
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for n2o5'
!	    call peg_error_fatal( lunerr, msg )
	end if
	if (p_clno2 .ge. p1st) then
	    kclno2 = p_clno2
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for clno2'
!	    call peg_error_fatal( lunerr, msg )
	end if
	if (p_o3 .ge. p1st) then
	    ko3 = p_o3
!	else
!	    msg = '*** subr init_data_mosaic_ptr - ptr error for o3'
!	    call peg_error_fatal( lunerr, msg )
	end if

!   rce 2005-apr-12 - added following species for cldchem, here and below:
!   kso2, kh2o2, khcho, khcooh, koh, kho2, 
!   kno3, kno, kno2, khono, kpan, kch3o2, kch3oh, kch3ooh
	if (p_so2    .ge. p1st) kso2    = p_so2
	if (p_h2o2   .ge. p1st) kh2o2   = p_h2o2
	if (p_hcho   .ge. p1st) khcho   = p_hcho
	if (p_ho     .ge. p1st) koh     = p_ho
	if (p_ho2    .ge. p1st) kho2    = p_ho2
	if (p_no3    .ge. p1st) kno3    = p_no3
	if (p_no     .ge. p1st) kno     = p_no
	if (p_no2    .ge. p1st) kno2    = p_no2
	if (p_hono   .ge. p1st) khono   = p_hono
	if (p_pan    .ge. p1st) kpan    = p_pan

!
!   calc ltot, ltot2, kh2o, ktemp
!
	is_aerosol(:) = .false.
	ltot = 0
	ltot = max( ltot, kh2so4 )
	ltot = max( ltot, khno3 )
	ltot = max( ltot, khcl )
	ltot = max( ltot, knh3 )
	ltot = max( ltot, kn2o5 )
	ltot = max( ltot, kclno2 )
	ltot = max( ltot, ko3 )
	ltot = max( ltot, kso2    )
	ltot = max( ltot, kh2o2   )
	ltot = max( ltot, khcho   )
	ltot = max( ltot, koh     )
	ltot = max( ltot, kho2    )
	ltot = max( ltot, kno3    )
	ltot = max( ltot, kno     )
	ltot = max( ltot, kno2    )
	ltot = max( ltot, khono   )
	ltot = max( ltot, kpan    )
        ltot = max( ltot, kpcg1_b_c )
        ltot = max( ltot, kpcg2_b_c )
        ltot = max( ltot, kpcg3_b_c )
        ltot = max( ltot, kpcg4_b_c )
        ltot = max( ltot, kpcg5_b_c )
        ltot = max( ltot, kpcg6_b_c )
        ltot = max( ltot, kpcg7_b_c )
        ltot = max( ltot, kpcg8_b_c )
        ltot = max( ltot, kpcg9_b_c )
        ltot = max( ltot, kpcg1_b_o )
        ltot = max( ltot, kpcg2_b_o )
        ltot = max( ltot, kpcg3_b_o )
        ltot = max( ltot, kpcg4_b_o )
        ltot = max( ltot, kpcg5_b_o )
        ltot = max( ltot, kpcg6_b_o )
        ltot = max( ltot, kpcg7_b_o )
        ltot = max( ltot, kpcg8_b_o )
        ltot = max( ltot, kpcg9_b_o )
        ltot = max( ltot, kopcg1_b_c )
        ltot = max( ltot, kopcg2_b_c )
        ltot = max( ltot, kopcg3_b_c )
        ltot = max( ltot, kopcg4_b_c )
        ltot = max( ltot, kopcg5_b_c )
        ltot = max( ltot, kopcg6_b_c )
        ltot = max( ltot, kopcg7_b_c )
        ltot = max( ltot, kopcg8_b_c )
        ltot = max( ltot, kopcg1_b_o )
        ltot = max( ltot, kopcg2_b_o )
        ltot = max( ltot, kopcg3_b_o )
        ltot = max( ltot, kopcg4_b_o )
        ltot = max( ltot, kopcg5_b_o )
        ltot = max( ltot, kopcg6_b_o )
        ltot = max( ltot, kopcg7_b_o )
        ltot = max( ltot, kopcg8_b_o )
        ltot = max( ltot, kpcg1_f_c )
        ltot = max( ltot, kpcg2_f_c )
        ltot = max( ltot, kpcg3_f_c )
        ltot = max( ltot, kpcg4_f_c )
        ltot = max( ltot, kpcg5_f_c )
        ltot = max( ltot, kpcg6_f_c )
        ltot = max( ltot, kpcg7_f_c )
        ltot = max( ltot, kpcg8_f_c )
        ltot = max( ltot, kpcg9_f_c )
        ltot = max( ltot, kpcg1_f_o )
        ltot = max( ltot, kpcg2_f_o )
        ltot = max( ltot, kpcg3_f_o )
        ltot = max( ltot, kpcg4_f_o )
        ltot = max( ltot, kpcg5_f_o )
        ltot = max( ltot, kpcg6_f_o )
        ltot = max( ltot, kpcg7_f_o )
        ltot = max( ltot, kpcg8_f_o )
        ltot = max( ltot, kpcg9_f_o )
        ltot = max( ltot, kopcg1_f_c )
        ltot = max( ltot, kopcg2_f_c )
        ltot = max( ltot, kopcg3_f_c )
        ltot = max( ltot, kopcg4_f_c )
        ltot = max( ltot, kopcg5_f_c )
        ltot = max( ltot, kopcg6_f_c )
        ltot = max( ltot, kopcg7_f_c )
        ltot = max( ltot, kopcg8_f_c )
        ltot = max( ltot, kopcg1_f_o )
        ltot = max( ltot, kopcg2_f_o )
        ltot = max( ltot, kopcg3_f_o )
        ltot = max( ltot, kopcg4_f_o )
        ltot = max( ltot, kopcg5_f_o )
        ltot = max( ltot, kopcg6_f_o )
        ltot = max( ltot, kopcg7_f_o )
        ltot = max( ltot, kopcg8_f_o )
        ltot = max( ltot, ksmpa )
        ltot = max( ltot, ksmpbb )
        ltot = max( ltot, kgly    )
        ltot = max( ltot, kant1_c )
        ltot = max( ltot, kant2_c )
        ltot = max( ltot, kant3_c )
        ltot = max( ltot, kant4_c )
        ltot = max( ltot, kant1_o )
        ltot = max( ltot, kant2_o )
        ltot = max( ltot, kant3_o )
        ltot = max( ltot, kant4_o )
        ltot = max( ltot, kbiog1_c )
        ltot = max( ltot, kbiog2_c )
        ltot = max( ltot, kbiog3_c )
        ltot = max( ltot, kbiog4_c )
        ltot = max( ltot, kbiog1_o )
        ltot = max( ltot, kbiog2_o )
        ltot = max( ltot, kbiog3_o )
        ltot = max( ltot, kbiog4_o )
        ltot = max( ltot, kasoaX )
        ltot = max( ltot, kasoa1 )
        ltot = max( ltot, kasoa2 )
        ltot = max( ltot, kasoa3 )
        ltot = max( ltot, kasoa4 )
        ltot = max( ltot, kbsoaX )
        ltot = max( ltot, kbsoa1 )
        ltot = max( ltot, kbsoa2 )
        ltot = max( ltot, kbsoa3 )
        ltot = max( ltot, kbsoa4 )

	do iphase=1,nphase_aer
	    do itype=1,ntype_aer
		do n = 1, nsize_aer(itype)
		    do ll = 1, ncomp_plustracer_aer(itype)
		       ltot = max( ltot, massptr_aer(ll,n,itype,iphase) )
		       is_aerosol(massptr_aer(ll,n,itype,iphase))=.true.
		    end do
		    ltot = max( ltot, hyswptr_aer(n,itype) )
		    ltot = max( ltot, waterptr_aer(n,itype) )
		    ltot = max( ltot, numptr_aer(n,itype,iphase) )
		    l = hyswptr_aer(n,itype)
		    if (l .ge. p1st) is_aerosol(l)=.true.
		    l = waterptr_aer(n,itype)
		    if (l .ge. p1st) is_aerosol(l)=.true.
		    l = numptr_aer(n,itype,iphase)
		    if (l .ge. p1st) is_aerosol(l)=.true.
		end do
	    end do
	end do

	kh2o = ltot + 1
	ktemp = ltot + 2
	ltot2 = ktemp

	write( msg, '(a,4(1x,i4))' ) 'ltot, ltot2, lmaxd, l2maxd =',   &
		ltot, ltot2, lmaxd, l2maxd
	call peg_message( lunout, msg )
	if ((ltot .gt. lmaxd) .or. (ltot2 .gt. l2maxd)) then
	    msg = '*** subr init_data_mosaic_ptr - ltot/ltot2 too big'
	    call peg_error_fatal( lunerr, msg )
	end if

        if (p_h2so4   .ge. p1st)then 
           name(kh2so4 ) = 'h2so4'
        elseif (p_sulf   .ge. p1st) then
             name(kh2so4 ) = 'h2so4'
        endif
	if (p_hno3   .ge. p1st) name(khno3  ) = 'hno3'
        if (p_pcg1_b_c   .ge. p1st) name(kpcg1_b_c  ) = 'pcg1_b_c'
        if (p_pcg2_b_c   .ge. p1st) name(kpcg2_b_c  ) = 'pcg2_b_c'
        if (p_pcg3_b_c   .ge. p1st) name(kpcg3_b_c  ) = 'pcg3_b_c'
        if (p_pcg4_b_c   .ge. p1st) name(kpcg4_b_c  ) = 'pcg4_b_c'
        if (p_pcg5_b_c   .ge. p1st) name(kpcg5_b_c  ) = 'pcg5_b_c'
        if (p_pcg6_b_c   .ge. p1st) name(kpcg6_b_c  ) = 'pcg6_b_c'
        if (p_pcg7_b_c   .ge. p1st) name(kpcg7_b_c  ) = 'pcg7_b_c'
        if (p_pcg8_b_c   .ge. p1st) name(kpcg8_b_c  ) = 'pcg8_b_c'
        if (p_pcg9_b_c   .ge. p1st) name(kpcg9_b_c  ) = 'pcg9_b_c'
        if (p_pcg1_b_o   .ge. p1st) name(kpcg1_b_o  ) = 'pcg1_b_o'
        if (p_pcg2_b_o   .ge. p1st) name(kpcg2_b_o  ) = 'pcg2_b_o'
        if (p_pcg3_b_o   .ge. p1st) name(kpcg3_b_o  ) = 'pcg3_b_o'
        if (p_pcg4_b_o   .ge. p1st) name(kpcg4_b_o  ) = 'pcg4_b_o'
        if (p_pcg5_b_o   .ge. p1st) name(kpcg5_b_o  ) = 'pcg5_b_o'
        if (p_pcg6_b_o   .ge. p1st) name(kpcg6_b_o  ) = 'pcg6_b_o'
        if (p_pcg7_b_o   .ge. p1st) name(kpcg7_b_o  ) = 'pcg7_b_o'
        if (p_pcg8_b_o   .ge. p1st) name(kpcg8_b_o  ) = 'pcg8_b_o'
        if (p_pcg9_b_o   .ge. p1st) name(kpcg9_b_o  ) = 'pcg9_b_o'
        if (p_opcg1_b_c   .ge. p1st) name(kopcg1_b_c  ) = 'opcg1_b_c'
        if (p_opcg2_b_c   .ge. p1st) name(kopcg2_b_c  ) = 'opcg2_b_c'
        if (p_opcg3_b_c   .ge. p1st) name(kopcg3_b_c  ) = 'opcg3_b_c'
        if (p_opcg4_b_c   .ge. p1st) name(kopcg4_b_c  ) = 'opcg4_b_c'
        if (p_opcg5_b_c   .ge. p1st) name(kopcg5_b_c  ) = 'opcg5_b_c'
        if (p_opcg6_b_c   .ge. p1st) name(kopcg6_b_c  ) = 'opcg6_b_c'
        if (p_opcg7_b_c   .ge. p1st) name(kopcg7_b_c  ) = 'opcg7_b_c'
        if (p_opcg8_b_c   .ge. p1st) name(kopcg8_b_c  ) = 'opcg8_b_c'
        if (p_opcg1_b_o   .ge. p1st) name(kopcg1_b_o  ) = 'opcg1_b_o'
        if (p_opcg2_b_o   .ge. p1st) name(kopcg2_b_o  ) = 'opcg2_b_o'
        if (p_opcg3_b_o   .ge. p1st) name(kopcg3_b_o  ) = 'opcg3_b_o'
        if (p_opcg4_b_o   .ge. p1st) name(kopcg4_b_o  ) = 'opcg4_b_o'
        if (p_opcg5_b_o   .ge. p1st) name(kopcg5_b_o  ) = 'opcg5_b_o'
        if (p_opcg6_b_o   .ge. p1st) name(kopcg6_b_o  ) = 'opcg6_b_o'
        if (p_opcg7_b_o   .ge. p1st) name(kopcg7_b_o  ) = 'opcg7_b_o'
        if (p_opcg8_b_o   .ge. p1st) name(kopcg8_b_o  ) = 'opcg8_b_o'
        if (p_pcg1_f_c   .ge. p1st) name(kpcg1_f_c  ) = 'pcg1_f_c'
        if (p_pcg2_f_c   .ge. p1st) name(kpcg2_f_c  ) = 'pcg2_f_c'
        if (p_pcg3_f_c   .ge. p1st) name(kpcg3_f_c  ) = 'pcg3_f_c'
        if (p_pcg4_f_c   .ge. p1st) name(kpcg4_f_c  ) = 'pcg4_f_c'
        if (p_pcg5_f_c   .ge. p1st) name(kpcg5_f_c  ) = 'pcg5_f_c'
        if (p_pcg6_f_c   .ge. p1st) name(kpcg6_f_c  ) = 'pcg6_f_c'
        if (p_pcg7_f_c   .ge. p1st) name(kpcg7_f_c  ) = 'pcg7_f_c'
        if (p_pcg8_f_c   .ge. p1st) name(kpcg8_f_c  ) = 'pcg8_f_c'
        if (p_pcg9_f_c   .ge. p1st) name(kpcg9_f_c  ) = 'pcg9_f_c'
        if (p_pcg1_f_o   .ge. p1st) name(kpcg1_f_o  ) = 'pcg1_f_o'
        if (p_pcg2_f_o   .ge. p1st) name(kpcg2_f_o  ) = 'pcg2_f_o'
        if (p_pcg3_f_o   .ge. p1st) name(kpcg3_f_o  ) = 'pcg3_f_o'
        if (p_pcg4_f_o   .ge. p1st) name(kpcg4_f_o  ) = 'pcg4_f_o'
        if (p_pcg5_f_o   .ge. p1st) name(kpcg5_f_o  ) = 'pcg5_f_o'
        if (p_pcg6_f_o   .ge. p1st) name(kpcg6_f_o  ) = 'pcg6_f_o'
        if (p_pcg7_f_o   .ge. p1st) name(kpcg7_f_o  ) = 'pcg7_f_o'
        if (p_pcg8_f_o   .ge. p1st) name(kpcg8_f_o  ) = 'pcg8_f_o'
        if (p_pcg9_f_o   .ge. p1st) name(kpcg9_f_o  ) = 'pcg9_f_o'
        if (p_opcg1_f_c   .ge. p1st) name(kopcg1_f_c  ) = 'opcg1_f_c'
        if (p_opcg2_f_c   .ge. p1st) name(kopcg2_f_c  ) = 'opcg2_f_c'
        if (p_opcg3_f_c   .ge. p1st) name(kopcg3_f_c  ) = 'opcg3_f_c'
        if (p_opcg4_f_c   .ge. p1st) name(kopcg4_f_c  ) = 'opcg4_f_c'
        if (p_opcg5_f_c   .ge. p1st) name(kopcg5_f_c  ) = 'opcg5_f_c'
        if (p_opcg6_f_c   .ge. p1st) name(kopcg6_f_c  ) = 'opcg6_f_c'
        if (p_opcg7_f_c   .ge. p1st) name(kopcg7_f_c  ) = 'opcg7_f_c'
        if (p_opcg8_f_c   .ge. p1st) name(kopcg8_f_c  ) = 'opcg8_f_c'
        if (p_opcg1_f_o   .ge. p1st) name(kopcg1_f_o  ) = 'opcg1_f_o'
        if (p_opcg2_f_o   .ge. p1st) name(kopcg2_f_o  ) = 'opcg2_f_o'
        if (p_opcg3_f_o   .ge. p1st) name(kopcg3_f_o  ) = 'opcg3_f_o'
        if (p_opcg4_f_o   .ge. p1st) name(kopcg4_f_o  ) = 'opcg4_f_o'
        if (p_opcg5_f_o   .ge. p1st) name(kopcg5_f_o  ) = 'opcg5_f_o'
        if (p_opcg6_f_o   .ge. p1st) name(kopcg6_f_o  ) = 'opcg6_f_o'
        if (p_opcg7_f_o   .ge. p1st) name(kopcg7_f_o  ) = 'opcg7_f_o'
        if (p_opcg8_f_o   .ge. p1st) name(kopcg8_f_o  ) = 'opcg8_f_o'
        if (p_smpa   .ge. p1st) name(ksmpa  ) = 'smpa'
        if (p_smpbb   .ge. p1st) name(ksmpbb  ) = 'smpbb'
        if (p_gly    .ge. p1st) name(kgly   ) = 'gly'
        if (p_ant1_c   .ge. p1st) name(kant1_c  ) = 'ant1_c'
        if (p_ant2_c   .ge. p1st) name(kant2_c  ) = 'ant2_c'
        if (p_ant3_c   .ge. p1st) name(kant3_c  ) = 'ant3_c'
        if (p_ant4_c   .ge. p1st) name(kant4_c  ) = 'ant4_c'
        if (p_ant1_o   .ge. p1st) name(kant1_o  ) = 'ant1_o'
        if (p_ant2_o   .ge. p1st) name(kant2_o  ) = 'ant2_o'
        if (p_ant3_o   .ge. p1st) name(kant3_o  ) = 'ant3_o'
        if (p_ant4_o   .ge. p1st) name(kant4_o  ) = 'ant4_o'
        if (p_biog1_c   .ge. p1st) name(kbiog1_c  ) = 'biog1_c'
        if (p_biog2_c   .ge. p1st) name(kbiog2_c  ) = 'biog2_c'
        if (p_biog3_c   .ge. p1st) name(kbiog3_c  ) = 'biog3_c'
        if (p_biog4_c   .ge. p1st) name(kbiog4_c  ) = 'biog4_c'
        if (p_biog1_o   .ge. p1st) name(kbiog1_o  ) = 'biog1_o'
        if (p_biog2_o   .ge. p1st) name(kbiog2_o  ) = 'biog2_o'
        if (p_biog3_o   .ge. p1st) name(kbiog3_o  ) = 'biog3_o'
        if (p_biog4_o   .ge. p1st) name(kbiog4_o  ) = 'biog4_o'
        if (p_cvasoaX   .ge. p1st) name(kasoaX  ) = 'cvasoaX'
        if (p_cvasoa1   .ge. p1st) name(kasoa1  ) = 'cvasoa1'
        if (p_cvasoa2   .ge. p1st) name(kasoa2  ) = 'cvasoa2'
        if (p_cvasoa3   .ge. p1st) name(kasoa3  ) = 'cvasoa3'
        if (p_cvasoa4   .ge. p1st) name(kasoa4  ) = 'cvasoa4'
        if (p_cvbsoaX   .ge. p1st) name(kbsoaX  ) = 'cvbsoaX'
        if (p_cvbsoa1   .ge. p1st) name(kbsoa1  ) = 'cvbsoa1'
        if (p_cvbsoa2   .ge. p1st) name(kbsoa2  ) = 'cvbsoa2'
        if (p_cvbsoa3   .ge. p1st) name(kbsoa3  ) = 'cvbsoa3'
        if (p_cvbsoa4   .ge. p1st) name(kbsoa4  ) = 'cvbsoa4'
	if (p_hcl    .ge. p1st) name(khcl   ) = 'hcl'
	if (p_nh3    .ge. p1st) name(knh3   ) = 'nh3'
	if (p_n2o5   .ge. p1st) name(kn2o5  ) = 'n2o5'
	if (p_clno2  .ge. p1st) name(kclno2 ) = 'clno2'
	if (p_o3     .ge. p1st) name(ko3    ) = 'o3'
	if (p_so2    .ge. p1st) name(kso2   ) = 'so2'
	if (p_h2o2   .ge. p1st) name(kh2o2  ) = 'h2o2'
	if (p_hcho   .ge. p1st) name(khcho  ) = 'hcho'
	if (p_ho     .ge. p1st) name(koh    ) = 'oh'
	if (p_ho2    .ge. p1st) name(kho2   ) = 'ho2'
	if (p_no3    .ge. p1st) name(kno3   ) = 'no3'
	if (p_no     .ge. p1st) name(kno    ) = 'no'
	if (p_no2    .ge. p1st) name(kno2   ) = 'no2'
	if (p_hono   .ge. p1st) name(khono  ) = 'hono'
	if (p_pan    .ge. p1st) name(kpan   ) = 'pan'
	name(ktemp)  = 'temp'
	name(kh2o)   = 'h2o'

        call initwet(   &
	    ntype_aer, nsize_aer, ncomp_aer,   &
	    massptr_aer, dens_aer, numptr_aer,           &
	    maxd_acomp, maxd_asize,maxd_atype, maxd_aphase,   &
	    dcen_sect, sigmag_aer, &
	    waterptr_aer, dens_water_aer, &
	    scavimptblvol, scavimptblnum, nimptblgrow_mind,   &
	    nimptblgrow_maxd, dlndg_nimptblgrow)

	return
	end subroutine init_data_mosaic_ptr


!-----------------------------------------------------------------------
! aerchem_debug_dump is used to generate input for testing with Rahul's
! offline MOSAIC box model. To use it, first determine which cell and
! time is crashing. Then, turn on the calls to this routine for that
! cell and time only. The routine is called once before aerchemistry
! with iflag=1, and then again after with iflag=3, if the model makes it
! that far. ;-)
!-----------------------------------------------------------------------
	subroutine aerchem_debug_dump(   &
      		iflag, iclm, jclm, dtchem )

	use module_data_mosaic_asect
	use module_data_mosaic_other
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com9a'

	integer iflag, iclm, jclm
	real dtchem

!   local variables
	integer ientryno
	save ientryno
	integer iphase, itype, k, l, m, n

	real dtchem_sv1
	save dtchem_sv1
	real rsub_sv1(l2maxd,kmaxd,nsubareamaxd)

	data ientryno / -13579 /


!   check for bypass based on some control variable ?


!   do initial output when ientryno = -13579
	if (ientryno .ne. -13579) goto 1000

	ientryno = +1

95010	format( a )
95020	format( 8( 1x, i8 ) )
95030	format( 4( 1pe18.10 ) )

	print 95010, 'aerchem_debug_dump start -- first time'
	print 95020, ltot, ltot2, itot, jtot, ktot
	print 95010, (name(l), l=1,ltot2)

	print 95020, maerocoag, maerchem, maeroptical
	print 95020, msectional, maerosolincw
	do iphase = 1, nphase_aer
	do itype=1,ntype_aer
	print 95020, iphase, itype, nsize_aer(itype),   &
     		ncomp_plustracer_aer(itype)

	do n = 1, ncomp_plustracer_aer(itype)
	    print 95010,   &
      		name_aer(n,itype)
	    print 95030,   &
      		dens_aer(n,itype),     mw_aer(n,itype)
	end do

	do n = 1, nsize_aer(itype)
	    print 95020,   &
      		ncomp_plustracer_aer(n),       ncomp_aer(n),   &
      		waterptr_aer(n,itype),   numptr_aer(n,itype,iphase),    &
      		mprognum_aer(n,itype,iphase)
	    print 95020,   &
      		(mastercompptr_aer(l,itype), massptr_aer(l,n,itype,iphase),    &
      		l=1,ncomp_plustracer_aer(itype))
	    print 95030,   &
      		volumcen_sect(n,itype),     volumlo_sect(n,itype),   &
      		volumhi_sect(n,itype),      dcen_sect(n,itype),   &
      		dlo_sect(n,itype),          dhi_sect(n,itype)
	    print 95020,   &
      		lptr_so4_aer(n,itype,iphase),  lptr_msa_aer(n,itype,iphase),  &
      		lptr_no3_aer(n,itype,iphase),  lptr_cl_aer(n,itype,iphase),   &
      		lptr_co3_aer(n,itype,iphase),  lptr_nh4_aer(n,itype,iphase),  &
      		lptr_na_aer(n,itype,iphase),   lptr_ca_aer(n,itype,iphase),   &
      		lptr_oin_aer(n,itype,iphase),  lptr_oc_aer(n,itype,iphase),   &
      		lptr_bc_aer(n,itype,iphase),   hyswptr_aer(n,itype), &
                lptr_pcg1_b_c_aer(n,itype,iphase),  lptr_pcg2_b_c_aer(n,itype,iphase),&
                lptr_pcg3_b_c_aer(n,itype,iphase),  lptr_pcg4_b_c_aer(n,itype,iphase),&
                lptr_pcg5_b_c_aer(n,itype,iphase),  lptr_pcg6_b_c_aer(n,itype,iphase),&
                lptr_pcg7_b_c_aer(n,itype,iphase),  lptr_pcg8_b_c_aer(n,itype,iphase),&
                lptr_pcg9_b_c_aer(n,itype,iphase),  lptr_pcg1_b_o_aer(n,itype,iphase),&
                lptr_pcg2_b_o_aer(n,itype,iphase), lptr_pcg3_b_o_aer(n,itype,iphase), &
                lptr_pcg4_b_o_aer(n,itype,iphase), lptr_pcg5_b_o_aer(n,itype,iphase), &
                lptr_pcg6_b_o_aer(n,itype,iphase), lptr_pcg7_b_o_aer(n,itype,iphase), &
                lptr_pcg8_b_o_aer(n,itype,iphase), lptr_pcg9_b_o_aer(n,itype,iphase), &
                lptr_opcg1_b_c_aer(n,itype,iphase),&
                lptr_opcg2_b_c_aer(n,itype,iphase),  lptr_opcg3_b_c_aer(n,itype,iphase),&
                lptr_opcg4_b_c_aer(n,itype,iphase),  lptr_opcg5_b_c_aer(n,itype,iphase),&
                lptr_opcg6_b_c_aer(n,itype,iphase),  lptr_opcg7_b_c_aer(n,itype,iphase),&
                lptr_opcg8_b_c_aer(n,itype,iphase),  lptr_opcg1_b_o_aer(n,itype,iphase),&
                lptr_opcg2_b_o_aer(n,itype,iphase),  lptr_opcg3_b_o_aer(n,itype,iphase),&
                lptr_opcg4_b_o_aer(n,itype,iphase),  lptr_opcg5_b_o_aer(n,itype,iphase),&
                lptr_opcg6_b_o_aer(n,itype,iphase),  lptr_opcg7_b_o_aer(n,itype,iphase),&
                lptr_opcg8_b_o_aer(n,itype,iphase),  &
                lptr_pcg1_f_c_aer(n,itype,iphase),  lptr_pcg2_f_c_aer(n,itype,iphase),&
                lptr_pcg3_f_c_aer(n,itype,iphase),  lptr_pcg4_f_c_aer(n,itype,iphase),&
                lptr_pcg5_f_c_aer(n,itype,iphase),  lptr_pcg6_f_c_aer(n,itype,iphase),&
                lptr_pcg7_f_c_aer(n,itype,iphase),  lptr_pcg8_f_c_aer(n,itype,iphase),&
                lptr_pcg9_f_c_aer(n,itype,iphase),  lptr_pcg1_f_o_aer(n,itype,iphase),&
                lptr_pcg2_f_o_aer(n,itype,iphase), lptr_pcg3_f_o_aer(n,itype,iphase), &
                lptr_pcg4_f_o_aer(n,itype,iphase), lptr_pcg5_f_o_aer(n,itype,iphase), &
                lptr_pcg6_f_o_aer(n,itype,iphase), lptr_pcg7_f_o_aer(n,itype,iphase), &
                lptr_pcg8_f_o_aer(n,itype,iphase), lptr_pcg9_f_o_aer(n,itype,iphase), &
                lptr_opcg1_f_c_aer(n,itype,iphase),&
                lptr_opcg2_f_c_aer(n,itype,iphase),  lptr_opcg3_f_c_aer(n,itype,iphase),&
                lptr_opcg4_f_c_aer(n,itype,iphase),  lptr_opcg5_f_c_aer(n,itype,iphase),&
                lptr_opcg6_f_c_aer(n,itype,iphase),  lptr_opcg7_f_c_aer(n,itype,iphase),&
                lptr_opcg8_f_c_aer(n,itype,iphase),  lptr_opcg1_f_o_aer(n,itype,iphase),&
                lptr_opcg2_f_o_aer(n,itype,iphase),  lptr_opcg3_f_o_aer(n,itype,iphase),&
                lptr_opcg4_f_o_aer(n,itype,iphase),  lptr_opcg5_f_o_aer(n,itype,iphase),&
                lptr_opcg6_f_o_aer(n,itype,iphase),  lptr_opcg7_f_o_aer(n,itype,iphase),&
                lptr_opcg8_f_o_aer(n,itype,iphase),                                     &
                lptr_smpa_aer(n,itype,iphase),lptr_smpbb_aer(n,itype,iphase),        &
                lptr_glysoa_r1_aer(n,itype,iphase), &
                lptr_glysoa_r2_aer(n,itype,iphase), &
                lptr_glysoa_sfc_aer(n,itype,iphase), &
                lptr_glysoa_nh4_aer(n,itype,iphase), &
                lptr_glysoa_oh_aer(n,itype,iphase), &
                lptr_ant1_c_aer(n,itype,iphase),lptr_ant2_c_aer(n,itype,iphase),        &
                lptr_ant3_c_aer(n,itype,iphase),lptr_ant4_c_aer(n,itype,iphase),        &
                lptr_ant1_o_aer(n,itype,iphase),lptr_ant2_o_aer(n,itype,iphase),        &
                lptr_ant3_o_aer(n,itype,iphase),lptr_ant4_o_aer(n,itype,iphase),        &
                lptr_biog1_c_aer(n,itype,iphase),lptr_biog2_c_aer(n,itype,iphase),        &
                lptr_biog3_c_aer(n,itype,iphase),lptr_biog4_c_aer(n,itype,iphase),        &
                lptr_biog1_o_aer(n,itype,iphase),lptr_biog2_o_aer(n,itype,iphase),        &
                lptr_biog3_o_aer(n,itype,iphase),lptr_biog4_o_aer(n,itype,iphase),        &
                lptr_asoaX_aer(n,itype,iphase), &
                lptr_asoa1_aer(n,itype,iphase),lptr_asoa2_aer(n,itype,iphase),          &
                lptr_asoa3_aer(n,itype,iphase),lptr_asoa4_aer(n,itype,iphase),          &
                lptr_bsoaX_aer(n,itype,iphase), &
                lptr_bsoa1_aer(n,itype,iphase),lptr_bsoa2_aer(n,itype,iphase),          &
                lptr_bsoa3_aer(n,itype,iphase),lptr_bsoa4_aer(n,itype,iphase)
                         
	end do ! size
	end do ! type
	end do ! phase
	print 95010, 'aerchem_debug_dump end -- first time'

!
!   test iflag
!
1000	continue
	if (iflag .eq. 1) goto 1010
	if (iflag .eq. 2) goto 2000
	if (iflag .eq. 3) goto 3000
	return

!
!   iflag=1 -- save initial values
!              AND FOR NOW do output too
!
1010	continue
	dtchem_sv1 = dtchem
	do m = 1, nsubareas
	do k = 1, ktot
	do l = 1, ltot2
	    rsub_sv1(l,k,m) = rsub(l,k,m)
	end do
	end do
	end do

	print 95010, 'aerchem_debug_dump start -- iflag=1'
	do m = 1, nsubareas
	do k = 1, ktot
	    print 95020, iymdcur, ihmscur,   &
		iclm, jclm, k, m, nsubareas, iflag
	    print 95030, t, dtchem_sv1, cairclm(k), relhumclm(k),   &
      		ptotclm(k), afracsubarea(k,m)
	    print 95030, (rsub_sv1(l,k,m), rsub(l,k,m), l=1,ltot2)
	end do
	end do
	print 95010, 'aerchem_debug_dump end -- iflag=1'

	return

!
!   iflag=2 -- save intermediate values before doing move_sections
!   (this is deactivated for now)
!
2000	continue
	return


!
!   iflag=3 -- do output
!
3000	continue
	print 95010, 'aerchem_debug_dump start -- iflag=3'
	do m = 1, nsubareas
	do k = 1, ktot
	    print 95020, iymdcur, ihmscur,   &
		iclm, jclm, k, m, nsubareas, iflag
	    print 95030, t, dtchem_sv1, cairclm(k), relhumclm(k),   &
      		ptotclm(k), afracsubarea(k,m)
	    print 95030, (rsub_sv1(l,k,m), rsub(l,k,m), l=1,ltot2)
	end do
	end do
	print 95010, 'aerchem_debug_dump end -- iflag=3'


	return
	end subroutine aerchem_debug_dump 



!-----------------------------------------------------------------------
	end module module_mosaic_driver
