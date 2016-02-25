!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

	module module_sorgam_vbs_cloudchem



	integer, parameter :: l_so4_aqyy = 1
	integer, parameter :: l_no3_aqyy = 2
	integer, parameter :: l_cl_aqyy  = 3
	integer, parameter :: l_nh4_aqyy = 4
	integer, parameter :: l_na_aqyy  = 5
	integer, parameter :: l_oin_aqyy = 6
	integer, parameter :: l_bc_aqyy  = 7
	integer, parameter :: l_oc_aqyy  = 8

	integer, parameter :: nyyy = 8

! "negligible volume" = (1 #/kg ~= 1e-6 #/cm3) * ((dp=1e-6 cm)**3 * pi/6)
	real, parameter :: smallvolaa = 0.5e-18



	contains



!-----------------------------------------------------------------------
	subroutine sorgam_vbs_cloudchem_driver(   &
	    id, ktau, ktauc, dtstepc, config_flags,   &
	    p_phy, t_phy, rho_phy, alt,   &
	    cldfra, ph_no2,   &
	    moist, chem,   &
	    gas_aqfrac, numgas_aqfrac,   &
	    ids,ide, jds,jde, kds,kde,   &
	    ims,ime, jms,jme, kms,kme,   &
	    its,ite, jts,jte, kts,kte )

	use module_state_description, only:   &
		num_moist, num_chem, p_qc

	use module_configure, only:  grid_config_rec_type

	use module_data_sorgam_vbs, only:  cw_phase, nphase_aer


	implicit none

!   subr arguments
	integer, intent(in) ::   &
		id, ktau, ktauc,   &
		numgas_aqfrac,   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte
!   id - domain index
!   ktau - time step number
!   ktauc - gas and aerosol chemistry time step number
!   numgas_aqfrac - last dimension of gas_aqfrac

!   [ids:ide, kds:kde, jds:jde] - spatial (x,z,y) indices for 'domain'
!   [ims:ime, kms:kme, jms:jme] - spatial (x,z,y) indices for 'memory'
!       Most arrays that are arguments to chem_driver
!       are dimensioned with these spatial indices.
!   [its:ite, kts:kte, jts:jte] - spatial (x,z,y) indices for 'tile'
!       chem_driver and routines under it do calculations
!       over these spatial indices.

	type(grid_config_rec_type), intent(in) :: config_flags
!   config_flags - configuration and control parameters

	real, intent(in) ::   &
	    dtstepc
!   dtstepc - time step for gas and aerosol chemistry(s)

        real, intent(in),   &
                dimension( ims:ime, kms:kme, jms:jme ) :: &
                p_phy, t_phy, rho_phy, alt, cldfra, ph_no2
!   p_phy - air pressure (Pa)
!   t_phy - temperature (K)
!   rho_phy - moist air density (kg/m^3)
!   alt - dry air specific volume (m^3/kg)
!   cldfra - cloud fractional area (0-1)
!   ph_no2 - no2 photolysis rate (1/min)

        real, intent(in),   &
                dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
                moist
!   moist - mixing ratios of moisture species (water vapor,
!       cloud water, ...) (kg/kg for mass species, #/kg for number species)

        real, intent(inout),   &
                dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
                chem
!   chem - mixing ratios of trace gas and aerosol species (ppm for gases, 
!	ug/kg for aerosol mass species, #/kg for aerosol number species)

        real, intent(inout),   &
                dimension( ims:ime, kms:kme, jms:jme, numgas_aqfrac ) :: &
                gas_aqfrac
!   gas_aqfrac - fraction (0-1) of gas that is dissolved in cloud water


!   local variables
	integer :: it, jt, kt, l
	integer :: icase
	integer :: igaschem_onoff, iphotol_onoff, iradical_onoff

	real :: rbox(num_chem)
	real :: gas_aqfrac_box(numgas_aqfrac)
	real :: ph_aq_box
	real, parameter :: qcldwtr_cutoff = 1.0e-6
	real :: qcldwtr


!   check that cw_phase is active
        if ((cw_phase .le. 0) .or. (cw_phase .gt. nphase_aer)) then
            print *, '*** sorgam_vbs_cloudchem_driver - cw_phase not active'
            return
        end if

	print 93010, 'entering sorgam_vbs_cloudchem_driver - ktau =', ktau

	icase = 0

!   iphotol_onoff = 1 if photolysis rate calcs are on; 0 if off
	iphotol_onoff = 0
	if (config_flags%phot_opt .gt. 0) iphotol_onoff = 1
!   igaschem_onoff = 1 if gas-phase chemistry is on; 0 if off
	igaschem_onoff = 0
	if (config_flags%gaschem_onoff .gt. 0) igaschem_onoff = 1

!   iradical_onoff turns aqueous radical chemistry on/off
!   set iradical_onoff=0 if either photolysis or gas-phase chem are off
	if ((igaschem_onoff .le. 0) .or. (iphotol_onoff  .le. 0)) then
	    iradical_onoff = 0
	else
	    iradical_onoff = 1
	end if
!   following line turns aqueous radical chem off unconditionally
	iradical_onoff = 0


	do 3920 jt = jts, jte
	do 3910 it = its, ite

	do 3800 kt = kts, kte

	qcldwtr = moist(it,kt,jt,p_qc)
	if (qcldwtr .le. qcldwtr_cutoff) goto 3800


	icase = icase + 1

!   detailed dump for debugging 
	if (ktau .eq. -13579) then
!	if ((ktau .eq. 30) .and. (it .eq. 23) .and.   &
!	      (jt .eq.  1) .and. (kt .eq. 11)) then
	  call sorgam_cloudchem_dumpaa(   &
	    id, ktau, ktauc, dtstepc, config_flags,   &
	    p_phy, t_phy, rho_phy, alt,   &
	    cldfra, ph_no2,   &
	    moist, chem,   &
	    gas_aqfrac, numgas_aqfrac,   &
	    ids,ide, jds,jde, kds,kde,   &
	    ims,ime, jms,jme, kms,kme,   &
	    its,ite, jts,jte, kts,kte,   &
	    qcldwtr_cutoff,   &
	    it, jt, kt )
	end if

!   map from wrf-chem 3d arrays to pegasus clm & sub arrays
	rbox(1:num_chem) = chem(it,kt,jt,1:num_chem)

!   make call '1box' cloudchem routine
!	print 93010, 'calling sorgam_cloudchem_1 at ijk =', it, jt, kt
	call sorgam_cloudchem_1box(   &
	    id, ktau, ktauc, dtstepc,   &
	    iphotol_onoff, iradical_onoff,   &
	    ph_no2(it,kt,jt),   &
	    ph_aq_box, gas_aqfrac_box,   &
	    numgas_aqfrac, it, jt, kt, icase,   &
	    rbox, qcldwtr,   &
	    t_phy(it,kt,jt), p_phy(it,kt,jt), rho_phy(it,kt,jt),&
            config_flags)

!   map back to wrf-chem 3d arrays 
	chem(it,kt,jt,1:num_chem) = rbox(1:num_chem)
	gas_aqfrac(it,kt,jt,:) = gas_aqfrac_box(:) 


3800	continue

3910	continue
3920	continue

	print 93010, 'leaving  sorgam_vbs_cloudchem_driver - ktau =', ktau, icase
93010	format( a, 8(1x,i6) )

	return
	end subroutine sorgam_vbs_cloudchem_driver



!-----------------------------------------------------------------------
	subroutine sorgam_cloudchem_1box(   &
	    id, ktau, ktauc, dtstepc,   &
	    iphotol_onoff, iradical_onoff,   &
	    photol_no2_box,   &
	    ph_aq_box, gas_aqfrac_box,   &
	    numgas_aqfrac, it, jt, kt, icase,   &
	    rbox, qcw_box, temp_box, pres_box, rho_box, &
            config_flags )

        use module_configure, only: grid_config_rec_type

	use module_state_description, only:   &
		num_moist, num_chem

	use module_data_sorgam_vbs, only:   &
		msectional, maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer, do_cloudchem_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_nh4_aer,   &
		lptr_orgpa_aer, lptr_ec_aer, lptr_p25_aer, &
                lptr_cl_aer, lptr_na_aer

	use module_data_cmu_bulkaqchem, only:   &
	        meqn1max


	implicit none

!   subr arguments

        type(grid_config_rec_type), intent(in) :: config_flags

	integer, intent(in) ::   &
		id, ktau, ktauc,   &
		numgas_aqfrac, it, jt, kt,   &
		icase, iphotol_onoff, iradical_onoff

	real, intent(in) ::   &
	    dtstepc, photol_no2_box,   &
	    qcw_box,   &		! cloud water (kg/kg)
	    temp_box,   &		! air temp (K)
	    pres_box,   &		! air pres (Pa)
	    rho_box			! air dens (kg/m3)

	real, intent(inout) :: ph_aq_box

        real, intent(inout), dimension( num_chem ) :: rbox
!   rbox has same units as chem [gas = ppmv, AP mass = ug/kg, AP number = #/kg]

        real, intent(inout), dimension( numgas_aqfrac ) :: gas_aqfrac_box

!   local variables
	integer :: iphase
	integer :: icase_in, idecomp_hmsa_hso5,   &
		iradical_in, istat_aqop

	integer :: lptr_yyy_cwaer(maxd_asize,maxd_atype,nyyy)

	real :: co2_mixrat_in
	real :: ph_cmuaq_cur
	real :: photol_no2_in
	real :: xprescribe_ph

        real :: yaq_beg(meqn1max), yaq_end(meqn1max)
	real :: rbox_sv1(num_chem)
	real :: rbulk_cwaer(nyyy,2)

        real, dimension( maxd_asize, maxd_atype ) :: fr_partit_cw
        real, dimension( 2, 3 ) :: xvol_old


!
!   set the lptr_yyy_cwaer
!
	iphase = cw_phase
	lptr_yyy_cwaer(:,:,l_so4_aqyy) = lptr_so4_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_no3_aqyy) = lptr_no3_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_nh4_aqyy) = lptr_nh4_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_oin_aqyy) = lptr_p25_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_bc_aqyy ) = lptr_ec_aer( :,:,iphase)
	lptr_yyy_cwaer(:,:,l_oc_aqyy ) = lptr_orgpa_aer( :,:,iphase)
! na and cl added 
        lptr_yyy_cwaer(:,:,l_cl_aqyy ) = lptr_cl_aer(:,:,iphase)
        lptr_yyy_cwaer(:,:,l_na_aqyy ) = lptr_na_aer(:,:,iphase)
!	lptr_yyy_cwaer(:,:,l_cl_aqyy ) = -999888777
!	lptr_yyy_cwaer(:,:,l_na_aqyy ) = -999888777

!
!
!   do bulk cloud-water chemistry
!
!
	icase_in = icase
	iradical_in = 1
	idecomp_hmsa_hso5 = 1

	co2_mixrat_in = 350.0

	photol_no2_in = photol_no2_box

!   when xprescribe_ph >= 0, ph is forced to its value
	xprescribe_ph = -1.0e31

!   turn off aqueous phase photolytic and radical chemistry
!   if either of the iphotol_onoff and iradical_onoff flags are 0
	if ((iphotol_onoff .le. 0) .or. (iradical_onoff .le. 0)) then
	    photol_no2_in = 0.0
	    iradical_in = 0
	end if

#if defined ( ccboxtest_box_testing_active)
!   following is for off-line box testing only
	call ccboxtest_extra_args_aa( 'get',   &
		co2_mixrat_in, iradical_in,   &
		idecomp_hmsa_hso5, icase_in,   &
		xprescribe_ph )
#endif

	rbox_sv1(:) = rbox(:)
	gas_aqfrac_box(:) = 0.0


!   make call to interface_to_aqoperator1
	call sorgam_interface_to_aqoperator1(   &
	    istat_aqop,   &
	    dtstepc,   &
	    rbox, gas_aqfrac_box,   &
	    qcw_box, temp_box, pres_box, rho_box,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    co2_mixrat_in, photol_no2_in, xprescribe_ph,   &
	    iradical_in, idecomp_hmsa_hso5,   &
	    yaq_beg, yaq_end, ph_cmuaq_cur,   &
	    numgas_aqfrac, id, it, jt, kt, ktau, icase_in, &
            config_flags )

	ph_aq_box = ph_cmuaq_cur


#if defined ( ccboxtest_box_testing_active)
!   following is for off-line box testing only
	call ccboxtest_extra_args_bb( 'put',   &
		yaq_beg, yaq_end, ph_cmuaq_cur )
#endif


!
!
!   calculate fraction of cloud-water associated with each activated aerosol bin
!
!
	call sorgam_partition_cldwtr(   &
	    rbox, fr_partit_cw, xvol_old,   &
	    id, it, jt, kt, icase_in )

!
!
!   distribute changes in bulk cloud-water composition among size bins
!
!
	call sorgam_distribute_bulk_changes(   &
	    rbox, rbox_sv1, fr_partit_cw,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    id, it, jt, kt, icase_in )


!
!   do move-sections
!
	if (msectional .lt. 1000000000) then
	    call sorgam_cloudchem_apply_mode_transfer(   &
		rbox, rbox_sv1, xvol_old,   &
		id, it, jt, kt, icase_in )
	end if



	return
	end subroutine sorgam_cloudchem_1box



!-----------------------------------------------------------------------
	subroutine sorgam_interface_to_aqoperator1(   &
	    istat_aqop,   &
	    dtstepc,   &
	    rbox, gas_aqfrac_box,   &
	    qcw_box, temp_box, pres_box, rho_box,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    co2_mixrat_in, photol_no2_in, xprescribe_ph,   &
	    iradical_in, idecomp_hmsa_hso5,   &
	    yaq_beg, yaq_end, ph_cmuaq_cur,   &
	    numgas_aqfrac, id, it, jt, kt, ktau, icase, &
            config_flags )

        use module_configure, only: grid_config_rec_type

	use module_state_description, only:   &
		num_chem, param_first_scalar, p_qc,   &
		p_nh3, p_hno3, p_hcl, p_sulf, p_hcho,   &
		p_ora1, p_so2, p_h2o2, p_o3, p_ho,   &
		p_ho2, p_no3, p_no, p_no2, p_hono,   &
		p_pan, p_ch3o2, p_ch3oh, p_op1, &
                p_form, p_facd, p_oh, p_meo2, p_meoh, p_mepx, &
                CB05_SORG_VBS_AQ_KPP

	use module_data_cmu_bulkaqchem, only:   &
	        meqn1max, naers, ngas,   &
	        na4, naa, nac, nae, nah, nahmsa, nahso5,   &
	        nan, nao, nar, nas, naw,   &
	        ng4, nga, ngc, ngch3co3h, ngch3o2, ngch3o2h, ngch3oh,   &
	        ngh2o2, nghcho, nghcooh, nghno2, ngho2,   &
	        ngn, ngno, ngno2, ngno3, ngo3, ngoh, ngpan, ngso2

	use module_cmu_bulkaqchem, only:  aqoperator1

	use module_data_sorgam_vbs, only:   &
        	maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer, do_cloudchem_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_nh4_aer,   &
		lptr_orgpa_aer, lptr_ec_aer, lptr_p25_aer, &
                lptr_cl_aer, lptr_na_aer


	implicit none

!   subr arguments

        type(grid_config_rec_type), intent(in) :: config_flags

	integer, intent(in) ::   &
		iradical_in, idecomp_hmsa_hso5,   &
	        numgas_aqfrac, id, it, jt, kt, ktau, icase
	integer, intent(inout) ::   &
		istat_aqop

	integer, intent(in) :: lptr_yyy_cwaer(maxd_asize,maxd_atype,nyyy)

	real, intent(in) ::   &
	    dtstepc, co2_mixrat_in,   &
	    photol_no2_in, xprescribe_ph,   &
	    qcw_box, temp_box, pres_box, rho_box

	real, intent(inout) :: ph_cmuaq_cur

        real, intent(inout), dimension( num_chem ) :: rbox	! ppm or ug/kg

        real, intent(inout), dimension( numgas_aqfrac ) :: gas_aqfrac_box

        real, intent(inout), dimension( nyyy, 2 ) :: rbulk_cwaer

        real, intent(inout), dimension( meqn1max ) :: yaq_beg, yaq_end


!   local variables
	integer :: i, iphase, isize, itype
	integer :: iaq, istat_fatal, istat_warn
	integer :: l, lyyy
	integer :: p1st
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	integer :: lunxx
#endif

	real, parameter :: eps=0.622   ! (mw h2o)/(mw air)


	real :: cair_moleperm3
	real :: dum, dumb
	real :: factgas, factlwc, factpatm, factphoto
	real :: factaerbc, factaercl, factaerna, factaernh4,   &
		factaerno3, factaeroc, factaeroin, factaerso4
	real :: lwc
	real :: p_atm, photo_in
	real :: rh
	real :: temp, tstep_beg_sec, tstep_end_sec
	real :: totsulf_beg, totsulf_end
	real :: gas(ngas), aerosol(naers)
	real :: gas_aqfrac_cmu(ngas)

	double precision tstep_beg_sec_dp, tstep_end_sec_dp,   &
	  temp_dp, p_atm_dp, lwc_dp, rh_dp,   &
	  co2_mixrat_in_dp, photo_in_dp, ph_cmuaq_cur_dp,   &
	  xprescribe_ph_dp
	double precision gas_dp(ngas), gas_aqfrac_cmu_dp(ngas),   &
	  aerosol_dp(naers), yaq_beg_dp(meqn1max), yaq_end_dp(meqn1max)



	p1st = param_first_scalar

!
!   units conversion factors 
!   'cmuaq-bulk' value = pegasus value X factor
!
!   [pres in atmospheres] = [pres in pascals] * factpatm
	factpatm = 1.0/1.01325e5
!   [cldwtr in g-h2o/m3-air] = [cldwtr in kg-h2o/kg-air] * factlwc
	factlwc = 1.0e3*rho_box
!   [aq photolysis rate scaling factor in --] = [jno2 in 1/min] * factphoto
	factphoto = 1.6

!   [gas in ppm] = [gas in ppm] * factgas
	factgas = 1.0

!   [aerosol in ug/m3-air] = [aerosol in ug/kg-air] * factaer
	dum = rho_box
	factaerso4   = dum
	factaerno3   = dum
	factaercl    = dum
	factaernh4   = dum
	factaerna    = dum
	factaeroin   = dum
	factaeroc    = dum
	factaerbc    = dum

#if defined ( ccboxtest_box_testing_active)
!   If aboxtest_units_convert=10, turn off units conversions both here
!   and in module_mosaic.  This is for testing, to allow exact agreements.
	if (aboxtest_units_convert .eq. 10) then
	    factpatm = 1.0
	    factlwc = 1.0
	    factphoto = 1.0
	    factgas = 1.0
	    factaerso4   = 1.0
	    factaerno3   = 1.0
	    factaercl    = 1.0
	    factaernh4   = 1.0
	    factaerna    = 1.0
	    factaeroin   = 1.0
	    factaeroc    = 1.0
	    factaerbc    = 1.0
	end if
#endif

!
!   map from rbox to gas,aerosol
!
	temp = temp_box

	lwc = qcw_box * factlwc
	p_atm = pres_box * factpatm

!   rce 2005-jul-11 - set p_atm so that cmu code's cair will match cairclm
!	p_atm = cairclm(kpeg)*1.0e3*0.082058e0*temp
!   for made-sorgam, set p_atm so that cmu code's (cair*28.966e3) 
!	will match rho_box
	p_atm = (rho_box/28.966)*0.082058e0*temp

	photo_in = photol_no2_in * factphoto

	rh = 1.0
	iaq = 1

	tstep_beg_sec = 0.0
	tstep_end_sec = dtstepc

!   map gases and convert to ppm
	gas(:) = 0.0

	if (p_nh3   >= p1st) gas(nga     ) = rbox(p_nh3  )*factgas
	if (p_hno3  >= p1st) gas(ngn     ) = rbox(p_hno3 )*factgas
	if (p_hcl   >= p1st) gas(ngc     ) = rbox(p_hcl  )*factgas
	if (p_sulf  >= p1st) gas(ng4     ) = rbox(p_sulf )*factgas

!	if (p_hcho  >= p1st) gas(nghcho  ) = rbox(p_hcho )*factgas
!	if (p_ora1  >= p1st) gas(nghcooh ) = rbox(p_ora1 )*factgas
	if (p_so2   >= p1st) gas(ngso2   ) = rbox(p_so2  )*factgas
	if (p_h2o2  >= p1st) gas(ngh2o2  ) = rbox(p_h2o2 )*factgas
	if (p_o3    >= p1st) gas(ngo3    ) = rbox(p_o3   )*factgas
!	if (p_ho    >= p1st) gas(ngoh    ) = rbox(p_ho   )*factgas
	if (p_ho2   >= p1st) gas(ngho2   ) = rbox(p_ho2  )*factgas
	if (p_no3   >= p1st) gas(ngno3   ) = rbox(p_no3  )*factgas

	if (p_no    >= p1st) gas(ngno    ) = rbox(p_no   )*factgas
	if (p_no2   >= p1st) gas(ngno2   ) = rbox(p_no2  )*factgas
	if (p_hono  >= p1st) gas(nghno2  ) = rbox(p_hono )*factgas
	if (p_pan   >= p1st) gas(ngpan   ) = rbox(p_pan  )*factgas
!	if (p_ch3o2 >= p1st) gas(ngch3o2 ) = rbox(p_ch3o2)*factgas
!	if (p_ch3oh >= p1st) gas(ngch3oh ) = rbox(p_ch3oh)*factgas
!	if (p_op1   >= p1st) gas(ngch3o2h) = rbox(p_op1  )*factgas

! CB05 case added below
        if ((config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP)) then

          if (p_form  >= p1st) gas(nghcho  ) = rbox(p_form )*factgas
          if (p_facd  >= p1st) gas(nghcooh ) = rbox(p_facd )*factgas
          if (p_oh    >= p1st) gas(ngoh    ) = rbox(p_oh   )*factgas
          if (p_meo2  >= p1st) gas(ngch3o2 ) = rbox(p_meo2 )*factgas
          if (p_meoh  >= p1st) gas(ngch3oh ) = rbox(p_meoh )*factgas
          if (p_mepx  >= p1st) gas(ngch3o2h) = rbox(p_mepx )*factgas

        else

          if (p_hcho  >= p1st) gas(nghcho  ) = rbox(p_hcho )*factgas
          if (p_ora1  >= p1st) gas(nghcooh ) = rbox(p_ora1 )*factgas
          if (p_ho    >= p1st) gas(ngoh    ) = rbox(p_ho   )*factgas
          if (p_ch3o2 >= p1st) gas(ngch3o2 ) = rbox(p_ch3o2)*factgas
          if (p_ch3oh >= p1st) gas(ngch3oh ) = rbox(p_ch3oh)*factgas
          if (p_op1   >= p1st) gas(ngch3o2h) = rbox(p_op1  )*factgas

        endif


!   compute bulk activated-aerosol mixing ratios
	aerosol(:) = 0.0
	rbulk_cwaer(:,:) = 0.0

	iphase = cw_phase
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	if ( .not. do_cloudchem_aer(isize,itype) ) cycle

	do lyyy = 1, nyyy

	l = lptr_yyy_cwaer(isize,itype,lyyy)
	if (l .ge. p1st) rbulk_cwaer(lyyy,1) = rbulk_cwaer(lyyy,1) + rbox(l)

	end do

	end do
	end do

!   map them to 'aerosol' array and convert to ug/m3
	aerosol(na4) = rbulk_cwaer(l_so4_aqyy,1) * factaerso4
	aerosol(nan) = rbulk_cwaer(l_no3_aqyy,1) * factaerno3
	aerosol(nac) = rbulk_cwaer(l_cl_aqyy, 1) * factaercl
	aerosol(naa) = rbulk_cwaer(l_nh4_aqyy,1) * factaernh4
	aerosol(nas) = rbulk_cwaer(l_na_aqyy, 1) * factaerna
	aerosol(nar) = rbulk_cwaer(l_oin_aqyy,1) * factaeroin
	aerosol(nae) = rbulk_cwaer(l_bc_aqyy, 1) * factaerbc
	aerosol(nao) = rbulk_cwaer(l_oc_aqyy, 1) * factaeroc


!
!   make call to aqoperator1
!
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	lunxx = 87
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    write(lunxx,*)
	    write(lunxx,*)
	    write(lunxx,*) 'interface_to_aqoperator1 - icase, irad, idecomp'
	    write(lunxx,9870) icase, iradical_in, idecomp_hmsa_hso5
	    write(lunxx,*) 'it, jt, kt, ktau'
	    write(lunxx,9870) it, jt, kt, ktau
	    write(lunxx,*) 'temp, p_atm, lwc, photo, co2, xprescribe_ph'
	    write(lunxx,9875) temp, p_atm, lwc, photo_in, co2_mixrat_in, xprescribe_ph
	    write(lunxx,*) 'pres_box, rho_box, qcw_box, dt_sec'
	    write(lunxx,9875) pres_box, rho_box, qcw_box,   &
		(tstep_end_sec-tstep_beg_sec)
	    write(lunxx,*) 'gas (1=nh3, 2=hno3, 3=hcl, 4=h2so4, 11=so2, 12=h2o2, 18=o3)'
	    write(lunxx,9875) gas
	    write(lunxx,*) 'rbox(nh3, hno3, hcl, h2so4, so2, h2o2, o3)'
	    write(lunxx,9875) rbox(p_nh3), rbox(p_hno3), rbox(p_hcl),   &
		rbox(p_sulf), rbox(p_so2), rbox(p_h2o2), rbox(p_o3)
	    write(lunxx,*) 'aerosol (1=na, 3=nh4, 4=no3, 5=cl, 6=so4, 8=ec, 9=oc, 10=crus)'
	    write(lunxx,9875) aerosol
	    write(lunxx,*) 'rbulk_cwaer (1=so4, 2=no3, 3-cl, 4=nh4, 5=na, 6=oin, 7=bc, 8=oc)'
	    write(lunxx,9875) rbulk_cwaer(:,1)
	    if (icase .le. -5) then
		write(*,*)   &
		 '*** stopping in interface_to_aqop1 at icase =', icase
		call wrf_error_fatal('*** stopping in interface_to_aqop1')
	    end if
	end if
9870	format( 8i5 )
9875	format( 5(1pe14.6) )
#endif

#if 0
! Print outs for debugging of aqoperator1... wig, 26-Oct-2005
!!$    if( (id == 1 .and. ktau >=  207 ) .or. &
!!$        (id == 2 .and. ktau >=  610 ) .or. &
!!$        (id == 3 .and. ktau >= 1830 ) ) then
       write(6,'(a)') '---Begin input for aqoperator1---'
       write(6,'(a,4i)') 'id, it, jt, kt =', id, it, jt, kt
       write(6,'(a,1p,2e20.12)') 'tstep_beg_sec, tstep_end_sec = ',  &
           tstep_beg_sec, tstep_end_sec
       do l=1,ngas
          write(6,'("gas(",i2,") = ",1p,1e20.12)') l, gas(l)
       end do
       do l=1,naers
          write(6,'("aerosol(",i2,") = ",1p,1e20.12)') l, aerosol(l)
       end do
       write(6,'(a,1p,4e20.12)') "temp, p_atm, lwc, rh = ", temp, p_atm, lwc, rh
       write(6,'(a,1p,3e20.12)') "co2_mixrat_in, photo_in, xprescribe_ph = ",   &
           co2_mixrat_in, photo_in, xprescribe_ph
       write(6,'(a,3i)') " iradical_in, idecomp_hmsa_hso5, iaq = ",   &
           iradical_in, idecomp_hmsa_hso5, iaq
       write(6,'(a)') "---End input for aqoperator1---"
!!$    end if
#endif


!   convert arguments to double prec
	tstep_beg_sec_dp = 0.0d0
	if (tstep_beg_sec .ne. 0.0) tstep_beg_sec_dp = tstep_beg_sec
	tstep_end_sec_dp = 0.0d0
	if (tstep_end_sec .ne. 0.0) tstep_end_sec_dp = tstep_end_sec
	temp_dp = 0.0d0
	if (temp .ne. 0.0) temp_dp = temp
	p_atm_dp = 0.0d0
	if (p_atm .ne. 0.0) p_atm_dp = p_atm
	lwc_dp = 0.0d0
	if (lwc .ne. 0.0) lwc_dp = lwc
	rh_dp = 0.0d0
	if (rh .ne. 0.0) rh_dp = rh
	co2_mixrat_in_dp = 0.0d0
	if (co2_mixrat_in .ne. 0.0) co2_mixrat_in_dp = co2_mixrat_in
	photo_in_dp = 0.0d0
	if (photo_in .ne. 0.0) photo_in_dp = photo_in
	xprescribe_ph_dp = 0.0d0
	if (xprescribe_ph .ne. 0.0) xprescribe_ph_dp = xprescribe_ph
	ph_cmuaq_cur_dp = 0.0d0
	if (ph_cmuaq_cur .ne. 0.0) ph_cmuaq_cur_dp = ph_cmuaq_cur

	do i = 1, ngas
	    gas_dp(i) = 0.0d0
	    if (gas(i) .ne. 0.0) gas_dp(i) = gas(i)
	end do
	do i = 1, naers
	    aerosol_dp(i) = 0.0d0
	    if (aerosol(i) .ne. 0.0) aerosol_dp(i) = aerosol(i)
	end do
	do i = 1, ngas
	    gas_aqfrac_cmu_dp(i) = 0.0d0
	    if (gas_aqfrac_cmu(i) .ne. 0.0) gas_aqfrac_cmu_dp(i) = gas_aqfrac_cmu(i)
	end do
	do i = 1, meqn1max
	    yaq_beg_dp(i) = 0.0d0
	    if (yaq_beg(i) .ne. 0.0) yaq_beg_dp(i) = yaq_beg(i)
	end do
	do i = 1, meqn1max
	    yaq_end_dp(i) = 0.0d0
	    if (yaq_end(i) .ne. 0.0) yaq_end_dp(i) = yaq_end(i)
	end do


!   total sulfur species conc as sulfate (ug/m3)
	cair_moleperm3 = 1.0e3*p_atm_dp/(0.082058e0*temp_dp)
	totsulf_beg = ( aerosol_dp(na4)/96.   &
	              + aerosol_dp(nahso5)/113. + aerosol_dp(nahmsa)/111.   &
	              + (gas_dp(ngso2) + gas_dp(ng4))*cair_moleperm3 )*96.0

!	call aqoperator1(   &
!	    istat_fatal, istat_warn,   &
!	    tstep_beg_sec, tstep_end_sec,   &
!	    gas, aerosol, gas_aqfrac_cmu,   &
!	    temp, p_atm, lwc, rh,   &
!	    co2_mixrat_in, photo_in, xprescribe_ph,   &
!	    iradical_in, idecomp_hmsa_hso5, iaq,   &
!	    yaq_beg, yaq_end, ph_cmuaq_cur )

	call aqoperator1(   &
	    istat_fatal, istat_warn,   &
	    tstep_beg_sec_dp, tstep_end_sec_dp,   &
	    gas_dp, aerosol_dp, gas_aqfrac_cmu_dp,   &
	    temp_dp, p_atm_dp, lwc_dp, rh_dp,   &
	    co2_mixrat_in_dp, photo_in_dp, xprescribe_ph_dp,   &
	    iradical_in, idecomp_hmsa_hso5, iaq,   &
	    yaq_beg_dp, yaq_end_dp, ph_cmuaq_cur_dp )

	totsulf_end = ( aerosol_dp(na4)/96.   &
	              + aerosol_dp(nahso5)/113. + aerosol_dp(nahmsa)/111.   &
	              + (gas_dp(ngso2) + gas_dp(ng4))*cair_moleperm3 )*96.0


!   convert arguments back to single prec
	tstep_beg_sec = tstep_beg_sec_dp
	tstep_end_sec = tstep_end_sec_dp
	temp = temp_dp
	p_atm = p_atm_dp
	lwc = lwc_dp
	rh = rh_dp
!	co2_mixrat_in = co2_mixrat_in_dp	! this has intent(in)
!	photo_in = photo_in_dp			! this has intent(in)
!	xprescribe_ph = xprescribe_ph_dp	! this has intent(in)
	ph_cmuaq_cur = ph_cmuaq_cur_dp

	do i = 1, ngas
	    gas(i) = gas_dp(i)
	end do
	do i = 1, naers
	    aerosol(i) = aerosol_dp(i)
	end do
	do i = 1, ngas
	    gas_aqfrac_cmu(i) = gas_aqfrac_cmu_dp(i)
	end do
	do i = 1, meqn1max
	    yaq_beg(i) = yaq_beg_dp(i)
	end do
	do i = 1, meqn1max
	    yaq_end(i) = yaq_end_dp(i)
	end do


!
!   warning message when status flags are non-zero
!
	istat_aqop = 0
	if (istat_fatal .ne. 0) then
	    write(6,*)   &
		'*** sorgam_cloudchem_driver, subr interface_to_aqoperator1'
	    write(6,'(a,4i5,2i10)')   &
		'    id,it,jt,kt, istat_fatal, warn =',   &
		id, it, jt, kt, istat_fatal, istat_warn
	    istat_aqop = -10
	end if

!
!   warning message when sulfur mass balance error exceeds the greater
!	of (1.0e-3 ug/m3) OR (1.0e-3 X total sulfur mixing ratio)
!
	dum = totsulf_end - totsulf_beg
	dumb = max( totsulf_beg, totsulf_end )
	if (abs(dum) .gt. max(1.0e-3,1.0e-3*dumb)) then
	    write(6,*)   &
		'*** sorgam_cloudchem_driver, sulfur balance warning'
	    write(6,'(a,4i5,1p,3e12.4)')   &
		'    id,it,jt,kt, total_sulfur_beg, _end, _error =',   &
		id, it, jt, kt, totsulf_beg, totsulf_end, dum
	end if

!
!   map from [gas,aerosol,gas_aqfrac_box] to [rbox,gas_aqfrac_box]
!
	gas_aqfrac_box(:) = 0.0

	if (p_nh3   >= p1st) then
	    rbox(p_nh3  ) = gas(nga     )/factgas
	    if (p_nh3   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_nh3   ) = gas_aqfrac_cmu(nga     )
	end if
	if (p_hno3  >= p1st) then
	    rbox(p_hno3 ) = gas(ngn     )/factgas
	    if (p_hno3  <= numgas_aqfrac)   &
		gas_aqfrac_box(p_hno3  ) = gas_aqfrac_cmu(ngn     )
	end if
	if (p_hcl   >= p1st) then
	    rbox(p_hcl  ) = gas(ngc     )/factgas
	    if (p_hcl   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_hcl   ) = gas_aqfrac_cmu(ngc     )
	end if
	if (p_sulf  >= p1st) then
	    rbox(p_sulf ) = gas(ng4     )/factgas
	    if (p_sulf  <= numgas_aqfrac)   &
		gas_aqfrac_box(p_sulf  ) = gas_aqfrac_cmu(ng4     )
	end if

!	if (p_hcho  >= p1st) then
!	    rbox(p_hcho ) = gas(nghcho  )/factgas
!	    if (p_hcho  <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_hcho  ) = gas_aqfrac_cmu(nghcho  )
!	end if
!	if (p_ora1  >= p1st) then
!	    rbox(p_ora1 ) = gas(nghcooh )/factgas
!	    if (p_ora1  <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_ora1  ) = gas_aqfrac_cmu(nghcooh )
!	end if
	if (p_so2   >= p1st) then
	    rbox(p_so2  ) = gas(ngso2   )/factgas
	    if (p_so2   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_so2   ) = gas_aqfrac_cmu(ngso2   )
	end if
	if (p_h2o2  >= p1st) then
	    rbox(p_h2o2 ) = gas(ngh2o2  )/factgas
	    if (p_h2o2  <= numgas_aqfrac)   &
		gas_aqfrac_box(p_h2o2  ) = gas_aqfrac_cmu(ngh2o2  )
	end if
	if (p_o3    >= p1st) then
	    rbox(p_o3   ) = gas(ngo3    )/factgas
	    if (p_o3    <= numgas_aqfrac)   &
		gas_aqfrac_box(p_o3    ) = gas_aqfrac_cmu(ngo3    )
	end if
!	if (p_ho    >= p1st) then
!	    rbox(p_ho   ) = gas(ngoh    )/factgas
!	    if (p_ho    <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_ho    ) = gas_aqfrac_cmu(ngoh    )
!	end if
	if (p_ho2   >= p1st) then
	    rbox(p_ho2  ) = gas(ngho2   )/factgas
	    if (p_ho2   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_ho2   ) = gas_aqfrac_cmu(ngho2   )
	end if
	if (p_no3   >= p1st) then
	    rbox(p_no3  ) = gas(ngno3   )/factgas
	    if (p_no3   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_no3   ) = gas_aqfrac_cmu(ngno3   )
	end if

	if (p_no    >= p1st) then
	    rbox(p_no   ) = gas(ngno    )/factgas
	    if (p_no    <= numgas_aqfrac)   &
		gas_aqfrac_box(p_no    ) = gas_aqfrac_cmu(ngno    )
	end if
	if (p_no2   >= p1st) then
	    rbox(p_no2  ) = gas(ngno2   )/factgas
	    if (p_no2   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_no2   ) = gas_aqfrac_cmu(ngno2   )
	end if
	if (p_hono  >= p1st) then
	    rbox(p_hono ) = gas(nghno2  )/factgas
	    if (p_hono  <= numgas_aqfrac)   &
		gas_aqfrac_box(p_hono  ) = gas_aqfrac_cmu(nghno2  )
	end if
	if (p_pan   >= p1st) then
	    rbox(p_pan  ) = gas(ngpan   )/factgas
	    if (p_pan   <= numgas_aqfrac)   &
		gas_aqfrac_box(p_pan   ) = gas_aqfrac_cmu(ngpan   )
	end if
!	if (p_ch3o2 >= p1st) then
!	    rbox(p_ch3o2) = gas(ngch3o2 )/factgas
!	    if (p_ch3o2 <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_ch3o2 ) = gas_aqfrac_cmu(ngch3o2 )
!	end if
!	if (p_ch3oh >= p1st) then
!	    rbox(p_ch3oh) = gas(ngch3oh )/factgas
!	    if (p_ch3oh <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_ch3oh ) = gas_aqfrac_cmu(ngch3oh )
!	end if
!	if (p_op1   >= p1st) then
!	    rbox(p_op1  ) = gas(ngch3o2h)/factgas
!	    if (p_op1   <= numgas_aqfrac)   &
!		gas_aqfrac_box(p_op1   ) = gas_aqfrac_cmu(ngch3o2h)
!	end if

! CB05 case added 
       if ( (config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) ) then

          if (p_form  >= p1st) then
              rbox(p_form ) = gas(nghcho  )/factgas
              if (p_form  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_form  ) = gas_aqfrac_cmu(nghcho  )
          end if
          if (p_facd  >= p1st) then
              rbox(p_facd ) = gas(nghcooh )/factgas
              if (p_facd  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_facd  ) = gas_aqfrac_cmu(nghcooh )
          end if
          if (p_oh    >= p1st) then
              rbox(p_oh   ) = gas(ngoh    )/factgas
              if (p_oh    <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_oh    ) = gas_aqfrac_cmu(ngoh    )
          end if
          if (p_meo2  >= p1st) then
              rbox(p_meo2 ) = gas(ngch3o2 )/factgas
              if (p_meo2  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_meo2  ) = gas_aqfrac_cmu(ngch3o2 )
          end if

          if (p_meoh  >= p1st) then
              rbox(p_meoh ) = gas(ngch3oh )/factgas
              if (p_meoh  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_meoh  ) = gas_aqfrac_cmu(ngch3oh )
          end if
          if (p_mepx  >= p1st) then
              rbox(p_mepx ) = gas(ngch3o2h)/factgas
              if (p_mepx  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_mepx  ) = gas_aqfrac_cmu(ngch3o2h)
          end if

       else
          if (p_hcho  >= p1st) then
              rbox(p_hcho ) = gas(nghcho  )/factgas
              if (p_hcho  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_hcho  ) = gas_aqfrac_cmu(nghcho  )
          end if
          if (p_ora1  >= p1st) then
              rbox(p_ora1 ) = gas(nghcooh )/factgas
              if (p_ora1  <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_ora1  ) = gas_aqfrac_cmu(nghcooh )
          end if
          if (p_ho    >= p1st) then
              rbox(p_ho   ) = gas(ngoh    )/factgas
              if (p_ho    <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_ho    ) = gas_aqfrac_cmu(ngoh    )
          end if
          if (p_ch3o2 >= p1st) then
              rbox(p_ch3o2) = gas(ngch3o2 )/factgas
              if (p_ch3o2 <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_ch3o2 ) = gas_aqfrac_cmu(ngch3o2 )
          end if
          if (p_ch3oh >= p1st) then
              rbox(p_ch3oh) = gas(ngch3oh )/factgas
              if (p_ch3oh <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_ch3oh ) = gas_aqfrac_cmu(ngch3oh )
          end if
          if (p_op1   >= p1st) then
              rbox(p_op1  ) = gas(ngch3o2h)/factgas
              if (p_op1   <= numgas_aqfrac)   &
                  gas_aqfrac_box(p_op1   ) = gas_aqfrac_cmu(ngch3o2h)
          end if

       end if

! end of addition

	rbulk_cwaer(l_so4_aqyy,2) = aerosol(na4)/factaerso4
	rbulk_cwaer(l_no3_aqyy,2) = aerosol(nan)/factaerno3
	rbulk_cwaer(l_cl_aqyy, 2) = aerosol(nac)/factaercl
	rbulk_cwaer(l_nh4_aqyy,2) = aerosol(naa)/factaernh4
	rbulk_cwaer(l_na_aqyy, 2) = aerosol(nas)/factaerna
	rbulk_cwaer(l_oin_aqyy,2) = aerosol(nar)/factaeroin
	rbulk_cwaer(l_bc_aqyy, 2) = aerosol(nae)/factaerbc
	rbulk_cwaer(l_oc_aqyy, 2) = aerosol(nao)/factaeroc


#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	lunxx = 87
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    write(lunxx,*)
	    write(lunxx,*) 'interface_to_aqoperator1 - after call'
	    write(lunxx,*) 'gas (1=nh3, 2=hno3, 3=hcl, 4=h2so4, 11=so2, 12=h2o2, 18=o3)'
	    write(lunxx,9875) gas
	    write(lunxx,*) 'rbox(nh3, hno3, hcl, h2so4, so2, h2o2, o3)'
	    write(lunxx,9875) rbox(p_nh3), rbox(p_hno3), rbox(p_hcl),   &
		rbox(p_sulf), rbox(p_so2), rbox(p_h2o2), rbox(p_o3)
	    write(lunxx,*) 'aerosol (1=na, 3=nh4, 4=no3, 5=cl, 6=so4, 8=ec, 9=oc, 10=crus)'
	    write(lunxx,9875) aerosol
	    write(lunxx,*) 'rbulk_cwaer (1=so4, 2=no3, 3-cl, 4=nh4, 5=na, 6=oin, 7=bc, 8=oc)'
	    write(lunxx,9875) rbulk_cwaer(:,2)
	    write(lunxx,*) 'ph_cmuaq_cur'
	    write(lunxx,9875) ph_cmuaq_cur
	    if (icase .le. -5) then
		write(*,*)   &
		 '*** stopping in interface_to_aqop1 at icase =', icase
		call wrf_error_fatal('*** stopping in interface_to_aqop1')
	    end if
	end if
#endif


	return
	end subroutine sorgam_interface_to_aqoperator1



!-----------------------------------------------------------------------
	subroutine sorgam_partition_cldwtr(   &
	    rbox, fr_partit_cw, xvol_old,   &
	    id, it, jt, kt, icase )

	use module_state_description, only:   &
		param_first_scalar, num_chem

	use module_data_sorgam_vbs, only:   &
        	maxd_asize, maxd_atype,   &
		ai_phase, cw_phase, nsize_aer, ntype_aer, ncomp_aer,   &
		do_cloudchem_aer, massptr_aer, numptr_aer,   &
		dens_aer, sigmag_aer,   &
		dcen_sect, dlo_sect, dhi_sect,   &
		volumcen_sect, volumlo_sect, volumhi_sect


	implicit none

!   subr arguments
	integer, intent(in) :: id, it, jt, kt, icase

        real, intent(inout), dimension( 1:num_chem ) :: rbox

        real, intent(inout), dimension( maxd_asize, maxd_atype ) ::   &
		fr_partit_cw

        real, intent(inout), dimension( 2, 3 ) :: xvol_old

!   local variables
	integer :: isize, itype
	integer :: jdone_mass, jdone_numb, jpos, jpos_mass, jpos_numb
	integer :: l, ll
	integer :: p1st
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	integer :: lunxx
#endif

	real, parameter :: partit_wght_mass = 0.5

	real :: tmpa, tmpb, tmpc
	real :: tmp_cwvolfrac, tmp_lnsg
	real :: tmass, tnumb, umass, unumb, wmass, wnumb
	real :: xmass_c, xmass_a, xmass_t, xvolu_c, xvolu_a, xvolu_t
	real :: xnumb_c1, xnumb_a1, xnumb_t1, xnumb_c2, xnumb_a2, xnumb_t2
	real, dimension( maxd_asize, maxd_atype ) :: fmass, fnumb, xmass, xnumb, xnumbsv


	p1st = PARAM_FIRST_SCALAR

	tmass = 0.0
	tnumb = 0.0
	umass = 0.0
	unumb = 0.0

!   compute
!     xmass, xnumb = mass, number mixing ratio for a bin
!     tmass, tnumb = sum over all bins of xmass, xnumb
!     umass, unumb = max over all bins of xmass, xnumb
!   set xmass, xnumb = 0.0 if bin mass, numb < 1.0e-37
!   constrain xnumb so that mean particle volume is
!     within bin boundaries
!   for made-sorgam, x/t/umass are g/kg-air, x/t/unumb are #/kg-air
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    if ( .not. do_cloudchem_aer(isize,itype) ) cycle
	    xmass_c = 0.0
	    xvolu_c = 0.0
	    xvolu_a = 0.0
	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,cw_phase)
		if (l .ge. p1st) then
		    tmpa = max( 0.0, rbox(l) )*1.0e-6
		    xmass_c = xmass_c + tmpa
		    xvolu_c = xvolu_c + tmpa/dens_aer(ll,itype)
		end if
		l = massptr_aer(ll,isize,itype,ai_phase)
		if (l .ge. p1st) then
		    tmpa = max( 0.0, rbox(l) )*1.0e-6
		    xvolu_a = xvolu_a + tmpa/dens_aer(ll,itype)
		end if
	    end do

	    xnumb_c1 = max( 0.0, rbox(numptr_aer(isize,itype,cw_phase)) )
	    xnumb_a1 = max( 0.0, rbox(numptr_aer(isize,itype,ai_phase)) )
	    xnumbsv(isize,itype) = xnumb_c1
	    xnumb_t1 = xnumb_a1 + xnumb_c1
	    xvolu_t = xvolu_a + xvolu_c

!   do "bounding" activated+interstitial combined number
!   and calculate dgnum for activated+interstitial combined
	    if (xvolu_t < smallvolaa) then
		xnumb_t2 = xvolu_t/volumcen_sect(isize,itype)
	    else if (xnumb_t1 < xvolu_t/volumhi_sect(isize,itype)) then
		xnumb_t2 = xvolu_t/volumhi_sect(isize,itype)
	    else if (xnumb_t1 > xvolu_t/volumlo_sect(isize,itype)) then
		xnumb_t2 = xvolu_t/volumlo_sect(isize,itype)
	    else
		xnumb_t2 = xnumb_t1
	    end if

!   do "bounding" of activated number
!   tmp_cwvolfrac = (cw volume)/(ai + cw volume)
	    tmp_cwvolfrac = xvolu_c/max(xvolu_t,1.e-30)
	    tmp_lnsg = log(sigmag_aer(isize,itype))
	    if ((xvolu_c < smallvolaa) .or. (tmp_cwvolfrac < 1.0e-10)) then
!   for very small cw volume or volume fraction, 
!   use (ai+cw number)*(cw volume fraction)
		xnumb_c2 = xnumb_t2*tmp_cwvolfrac
		tmpa = -7.0 ; tmpb = -7.0 ; tmpc = -7.0
	    else
!   tmpa is value of (ln(dpcut)-ln(dgvol))/ln(sigmag) for which
!   "norm01 upper tail" cummulative pdf is equal to tmp_cwvolfrac
		tmpa = norm01_uptail_inv( tmp_cwvolfrac )
!   tmpb is corresponding value of (ln(dp)-ln(dgnum))/ln(sigmag)
		tmpb = tmpa + 3.0*tmp_lnsg
!   tmpc is corresponding "norm01 upper tail" cummulative pdf
		tmpc = norm01_uptail( tmpb )
!   minimum number of activated particles occurs when
!   activated particles are dp>dpcut and interstitial are dp<dpcut,
!   giving (cw number) == (ai+cw number)*tmpc
		xnumb_c2 = max( xnumb_c1,  xnumb_t2*tmpc )
!   assuming activated particles are at least as big as interstitial ones,
!   the minimum number of activated particles occurs when ai & cw have same sizes,
!   giving (cw number) == (ai+cw number)*(cw volume fraction)
		xnumb_c2 = min( xnumb_c2,  xnumb_t2*tmp_cwvolfrac )
	    end if
	    xnumb_a2 = xnumb_t2 - xnumb_c2
	    rbox(numptr_aer(isize,itype,cw_phase)) = xnumb_c2
	    rbox(numptr_aer(isize,itype,ai_phase)) = xnumb_a2

#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   diagnostics when lunxx > 0
	lunxx = 86
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx > 0) then
	    if ((isize == 1) .and. (itype == 1)) write(lunxx,'(a)')
	    write(lunxx,'(a,3i4,4x,2i4,2x,l2)') 'partition-cw  i,j,k,  is,it', &
		it, jt, kt, isize, itype
	    write(lunxx,'(a,1p,5e12.4)') 'cw  vol, num old/adj      ', &
		xvolu_c, xnumb_c1, xnumb_c2
	    write(lunxx,'(a,1p,5e12.4)') 'ai  vol, num old/adj      ', &
		xvolu_a, xnumb_a1, xnumb_a2
	    write(lunxx,'(a,1p,5e12.4)') 'a+c vol, num old/adj      ', &
		xvolu_t, xnumb_t1, xnumb_t2
	    write(lunxx,'(a,1p,5e12.4)') 'lnsg, cwvolfr, cwnumfr 1/2', &
		tmp_lnsg, tmp_cwvolfrac, &
		xnumb_c1/max(xnumb_t1,1.0e-30), &
		xnumb_c2/max(xnumb_t2,1.0e-30) 
	    write(lunxx,'(a,1p,5e12.4)') 'tmpa/b/c                  ', &
		tmpa, tmpb, tmpc
	    write(lunxx,'(a,1p,5e12.4)') 'dlo, dcen, dhi_sect       ', &
		dlo_sect(isize,itype), dcen_sect(isize,itype), &
		dhi_sect(isize,itype)
	    write(lunxx,'(a,1p,5e12.4)') 'vlo, vcen, vhi_sect       ', &
		volumlo_sect(isize,itype), volumcen_sect(isize,itype), &
		volumhi_sect(isize,itype)
	end if
#endif

	    if (xmass_c .lt. 1.0e-37) xmass_c = 0.0
	    xmass(isize,itype) = xmass_c
	    if (xnumb_c2 .lt. 1.0e-37) xnumb_c2 = 0.0
	    xnumb(isize,itype) = xnumb_c2
	    xnumbsv(isize,itype) = xnumb_c1

	    tmass = tmass + xmass(isize,itype)
	    tnumb = tnumb + xnumb(isize,itype)
	    umass = max( umass, xmass(isize,itype) )
	    unumb = max( unumb, xnumb(isize,itype) )

	    if ((itype == 1) .and. (isize <= 2)) then
		xvol_old(isize,1) = xvolu_c
		xvol_old(isize,2) = xvolu_a
		xvol_old(isize,3) = xvolu_t
	    end if
	end do
	end do

!   compute
!     fmass, fnumb = fraction of total mass, number that is in a bin
!   if tmass<1e-35 and umass>0, set fmass=1 for bin with largest xmass
!   if tmass<1e-35 and umass=0, set fmass=0 for all
	jdone_mass = 0
	jdone_numb = 0
	jpos_mass = 0
	jpos_numb = 0
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    if ( .not. do_cloudchem_aer(isize,itype) ) cycle
	    fmass(isize,itype) = 0.0
	    if (tmass .ge. 1.0e-35) then
		fmass(isize,itype) = xmass(isize,itype)/tmass
	    else if (umass .gt. 0.0) then
		if ( (jdone_mass .eq. 0) .and.   &
		     (xmass(isize,itype) .eq. umass) ) then
		    jdone_mass = 1
		    fmass(isize,itype) = 1.0
		end if
	    end if
	    if (fmass(isize,itype) .gt. 0) jpos_mass = jpos_mass + 1

	    fnumb(isize,itype) = 0.0
	    if (tnumb .ge. 1.0e-35) then
		fnumb(isize,itype) = xnumb(isize,itype)/tnumb
	    else if (unumb .gt. 0.0) then
		if ( (jdone_numb .eq. 0) .and.   &
		     (xnumb(isize,itype) .eq. unumb) ) then
		    jdone_numb = 1
		    fnumb(isize,itype) = 1.0
		end if
	    end if
	    if (fnumb(isize,itype) .gt. 0) jpos_numb = jpos_numb + 1
	end do
	end do

!   if only 1 bin has fmass or fnumb > 0, set value to 1.0 exactly
	if ((jpos_mass .eq. 1) .or. (jpos_numb .eq. 1)) then
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    if ( .not. do_cloudchem_aer(isize,itype) ) cycle
	    if (jpos_mass .eq. 1) then
		if (fmass(isize,itype) .gt. 0) fmass(isize,itype) = 1.0
	    end if
	    if (jpos_numb .eq. 1) then
		if (fnumb(isize,itype) .gt. 0) fnumb(isize,itype) = 1.0
	    end if
	end do
	end do
	end if

!
!   compute fr_partit_cw as weighted average of fmass & fnumb, except
!   if tmass<1e-35 and umass=0, use only fnumb
!   if tnumb<1e-35 and unumb=0, use only fmass
!   if tmass,tnumb<1e-35 and umass,unumb=0, 
!		set fr_partit_cw=1 for center bin of itype=1
!
	fr_partit_cw(:,:) = 0.0
	if ((jpos_mass .eq. 0) .and. (jpos_numb .eq. 0)) then
	    itype = 1
	    isize = (nsize_aer(itype)+1)/2
	    fr_partit_cw(isize,itype) = 1.0

	else if (jpos_mass .eq. 0) then
	    fr_partit_cw(:,:) = fnumb(:,:)

	else if (jpos_numb .eq. 0) then
	    fr_partit_cw(:,:) = fmass(:,:)

	else
	    wmass = max( 0.0, min( 1.0, partit_wght_mass ) )
	    wnumb = 1.0 - wmass
	    fr_partit_cw(:,:) =  wmass*fmass(:,:) + wnumb*fnumb(:,:)

	    jpos = 0
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if ( .not. do_cloudchem_aer(isize,itype) ) cycle
		if (fr_partit_cw(isize,itype) .gt. 0.0) jpos = jpos + 1
	    end do
	    end do

!   if only 1 bin has fr_partit_cw > 0, set value to 1.0 exactly
	    if (jpos .eq. 1) then
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if ( .not. do_cloudchem_aer(isize,itype) ) cycle
		if (fr_partit_cw(isize,itype) .gt. 0.0)   &
			fr_partit_cw(isize,itype) = 1.0
	    end do
	    end do
	    end if
	end if


#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   diagnostics when lunxx > 0
	lunxx = 86
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
!	if (icase .gt. 9) lunxx = -1
	if (lunxx .gt. 0) then
	    write(lunxx,9800)
	    write(lunxx,9800)   &
		'partition_cldwtr - icase, jpos, jpos_mass, jpos_numb'
	    write(lunxx,9810)  icase, jpos, jpos_mass, jpos_numb
	    write(lunxx,9800) 'tmass, umass, wmass'
	    write(lunxx,9820)  tmass, umass, wmass
	    write(lunxx,9800) 'tnumb, unumb, wnumb'
	    write(lunxx,9820)  tnumb, unumb, wnumb
	    write(lunxx,9800) 'xmass, fmass, xnumb_orig/adj, fnumb, fr_partit_cw'
	    tmpa = 0.0
	    tmpb = 0.0
	    tmpc = 0.0
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if ( .not. do_cloudchem_aer(isize,itype) ) cycle
	        write(lunxx,9820)  xmass(isize,itype), fmass(isize,itype),   &
			xnumbsv(isize,itype), xnumb(isize,itype),   &
			fnumb(isize,itype), fr_partit_cw(isize,itype)
	        tmpa = tmpa + fmass(isize,itype)
	        tmpb = tmpb + fnumb(isize,itype)
	        tmpc = tmpc + fr_partit_cw(isize,itype)
	    end do
	    end do
	    write(lunxx,9800)   &
		'sum_fmass-1.0,  sum_fnumb-1.0,  sum_fr_partit-1.0'
	    write(lunxx,9820)  (tmpa-1.0), (tmpb-1.0), (tmpc-1.0)
	    if (icase .le. -5) then
		write(*,*) '*** stopping in partition_cldwtr at icase =', icase
		call wrf_error_fatal('*** stopping in partition_cldwtr')
	    end if
9800	    format( a )
9810	    format( 5i10 )
9820	    format( 6(1pe10.2) )
	end if
#endif


	return
	end subroutine sorgam_partition_cldwtr



!-----------------------------------------------------------------------
	subroutine sorgam_distribute_bulk_changes(   &
		rbox, rbox_sv1, fr_partit_cw,   &
		rbulk_cwaer, lptr_yyy_cwaer,   &
		id, it, jt, kt, icase )

	use module_state_description, only:   &
        	param_first_scalar, num_chem

	use module_scalar_tables, only:  chem_dname_table

	use module_data_sorgam_vbs, only:   &
		maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer, do_cloudchem_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_nh4_aer,   &
		lptr_orgpa_aer, lptr_ec_aer, lptr_p25_aer


	implicit none

!   subr arguments
	integer, intent(in) :: id, it, jt, kt, icase

	integer, intent(in) :: lptr_yyy_cwaer(maxd_asize,maxd_atype,nyyy)

        real, intent(inout), dimension( 1:num_chem ) :: rbox, rbox_sv1

        real, intent(in), dimension( maxd_asize, maxd_atype ) ::   &
		fr_partit_cw

        real, intent(in), dimension( nyyy, 2 ) :: rbulk_cwaer


!   local variables
	integer :: iphase, isize, itype
	integer :: idone, icount, ncount
	integer :: jpos, jpos_sv
	integer :: l, lunxxaa, lunxxbb, lyyy
	integer :: p1st
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	integer :: lunxx
#endif

	real :: duma, dumb, dumc
	real :: fr, frsum_cur
        real :: fr_cur(maxd_asize,maxd_atype)
	real :: del_r_current, del_r_remain
	real :: del_rbulk_cwaer(nyyy)


	p1st = param_first_scalar

	do lyyy = 1, nyyy
	    del_rbulk_cwaer(lyyy) = rbulk_cwaer(lyyy,2) - rbulk_cwaer(lyyy,1)
	end do

	iphase = cw_phase


	jpos = 0
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    if ( .not. do_cloudchem_aer(isize,itype) ) cycle
	    if (fr_partit_cw(isize,itype) .gt. 0) jpos = jpos + 1
	end do
	end do
	jpos_sv = jpos

!
!   distribution is trivial when only 1 bin has fr_partit_cw > 0
!
	if (jpos_sv .eq. 1) then
	    do lyyy = 1, nyyy

	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if ( .not. do_cloudchem_aer(isize,itype) ) cycle
		fr = fr_partit_cw(isize,itype)
		if (fr .eq. 1.0) then
		    l = lptr_yyy_cwaer(isize,itype,lyyy)
		    if (l .ge. p1st) rbox(l) = rbulk_cwaer(lyyy,2)
		end if
	    end do
	    end do

	    end do
	    goto 7900
	end if


	do 3900 lyyy = 1, nyyy

!
!   distribution is simple when del_rbulk_cwaer(lyyy) >= 0
!
	if (del_rbulk_cwaer(lyyy) .eq. 0.0) then
	    goto 3900
	else if (del_rbulk_cwaer(lyyy) .gt. 0.0) then
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if ( .not. do_cloudchem_aer(isize,itype) ) cycle
		fr = fr_partit_cw(isize,itype)
		if (fr .gt. 0.0) then
		    l = lptr_yyy_cwaer(isize,itype,lyyy)
		    if (l .ge. p1st) then
			rbox(l) = rbox(l) + fr*del_rbulk_cwaer(lyyy)
		    end if
		end if
	    end do
	    end do

	    goto 3900
	end if

!
!   distribution is complicated when del_rbulk_cwaer(lyyy) < 0, 
!   because you cannot produce any negative mixrats
!
	del_r_remain = del_rbulk_cwaer(lyyy)
	fr_cur(:,:) = fr_partit_cw(:,:)

	ncount = max( 1, jpos_sv*2 )
	icount = 0

!   iteration loop
	do while (icount .le. ncount)

	icount = icount + 1
	del_r_current = del_r_remain
	jpos = 0
	frsum_cur = 0.0

	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	if ( .not. do_cloudchem_aer(isize,itype) ) cycle

	fr = fr_cur(isize,itype)

	if (fr .gt. 0.0) then
	    l = lptr_yyy_cwaer(isize,itype,lyyy)
	    if (l .ge. p1st) then
		duma = fr*del_r_current
		dumb = rbox(l) + duma
		if (dumb .gt. 0.0) then
		    jpos = jpos + 1
		else if (dumb .eq. 0.0) then
		    fr_cur(isize,itype) = 0.0
		else
		    duma = -rbox(l)
		    dumb = 0.0
		    fr_cur(isize,itype) = 0.0
		end if
		del_r_remain = del_r_remain - duma
		rbox(l) = dumb
		frsum_cur = frsum_cur + fr_cur(isize,itype)
	    else
		fr_cur(isize,itype) = 0.0
	    end if
	end if

	end do	! isize = 1, nsize_aer
	end do	! itype = 1, ntype_aer

!   done if jpos = jpos_sv, because bins reached zero mixrat
	if (jpos .eq. jpos_sv) then
	    idone = 1
!   del_r_remain starts as negative, so done if non-negative
	else if (del_r_remain .ge. 0.0) then
	    idone = 2
!   del_r_remain starts as negative, so done if non-negative
	else if (abs(del_r_remain) .le. 1.0e-7*abs(del_rbulk_cwaer(lyyy))) then
	    idone = 3
!   done if all bins have fr_cur = 0
	else if (frsum_cur .le. 0.0) then
	    idone = 4
!   same thing basically
	else if (jpos .le. 0) then
	    idone = 5
	else
	    idone = 0
	end if

!   check for done, and (conditionally) print message
	if (idone .gt. 0) then
	    lunxxaa = 6
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!	    lunxxaa = 86
	    if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxxaa = 171
#endif
	    if ((lunxxaa .gt. 0) .and. (icount .gt. (1+jpos_sv)/2)) then
		write(lunxxaa,9800)   &
		'distribute_bulk_changes - icount>jpos_sv/2 - i,j,k'
		write(lunxxaa,9810)  it, jt, kt
		write(lunxxaa,9800) 'icase, lyyy, idone, icount, jpos, jpos_sv'
		write(lunxxaa,9810)  icase, lyyy, idone, icount, jpos, jpos_sv
	    end if
	    goto 3900	
	end if

!   rescale fr_cur for next iteration
	fr_cur(:,:) = fr_cur(:,:)/frsum_cur

	end do	! while (icount .le. ncount)


!   icount > ncount, so print message
	lunxxbb = 6
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!	lunxxbb = 86
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxxbb = 171
#endif
	if (lunxxbb .gt. 0) then
	    write(lunxxbb,9800)
	    write(lunxxbb,9800)   &
		'distribute_bulk_changes - icount>ncount - i,j,k'
	    write(lunxxbb,9810)  it, jt, kt
	    write(lunxxbb,9800) 'icase, lyyy, icount, ncount, jpos_sv, jpos'
	    write(lunxxbb,9810)  icase, lyyy, icount, ncount, jpos_sv, jpos
	    write(lunxxbb,9800) 'rbulk_cwaer(1), del_rbulk_cwaer, del_r_remain, frsum_cur, (frsum_cur-1.0)'
	    write(lunxxbb,9820)  rbulk_cwaer(lyyy,1), del_rbulk_cwaer(lyyy),   &
		del_r_remain, frsum_cur, (frsum_cur-1.0)
	end if
9800	format( a )
9801	format( 3a )
9810	format( 7i10 )
9820	format( 7(1pe10.2) )
9840	format( 2i3, 5(1pe14.6) )


3900	continue

7900	continue


#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   diagnostics for testing
	lunxx = 88
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    icount = 0
	    do lyyy = 1, nyyy
	    duma = del_rbulk_cwaer(lyyy)
	    if ( abs(duma) .gt.   &
		    max( 1.0e-35, 1.0e-5*abs(rbulk_cwaer(lyyy,1)) ) ) then
		icount = icount + 1
		if (icount .eq. 1) write(lunxx,9800)
		if (icount .eq. 1) write(lunxx,9800)
		write(lunxx,9800)
		l = lptr_yyy_cwaer(1,1,lyyy)
		if (l .ge. p1st) then
		    write(lunxx,9801) 'distribute_bulk_changes - ',   &
			chem_dname_table(id,l)(1:12), ' - icase, lyyy, l11'
		else
		    write(lunxx,9801) 'distribute_bulk_changes - ',   &
			'name = ?????', ' - icase, lyyy, l11'
		end if
		write(lunxx,9810) icase, lyyy, l
		write(lunxx,9800) ' tp sz  rbox_sv1, rbox, del_rbox' //   &
			', del_rbox/del_rbulk_cwaer, (...-fr_partit_cw)'
		write(lunxx,9840) 0, 0, rbulk_cwaer(lyyy,1:2), duma
		do itype = 1, ntype_aer
		do isize = 1, nsize_aer(itype)
		    if ( .not. do_cloudchem_aer(isize,itype) ) cycle
		    l = lptr_yyy_cwaer(isize,itype,lyyy)
		    if (l .lt. p1st) cycle
		    dumb = rbox(l) - rbox_sv1(l)
		    dumc = dumb/max( abs(duma), 1.0e-35 )
		    if (duma .lt. 0.0) dumc = -dumc
		    write(lunxx,9840) itype, isize, rbox_sv1(l), rbox(l),   &
			dumb, dumc, (dumc-fr_partit_cw(isize,itype))
		end do
		end do
	    end if
	    end do
	    if (icase .le. -5) then
		write(*,*)   &
		'*** stop in distribute_bulk_changes diags, icase =', icase
		call wrf_error_fatal('*** stop in distribute_bulk_changes diags')
	    end if
	end if
#endif


	return
	end subroutine sorgam_distribute_bulk_changes



!-----------------------------------------------------------------------
	subroutine sorgam_cloudchem_apply_mode_transfer(   &
		rbox, rbox_sv1, xvol_old,   &
		id, it, jt, kt, icase )

	use module_state_description, only:   &
        	param_first_scalar, num_chem

	use module_scalar_tables, only:  chem_dname_table

	use module_data_sorgam_vbs, only:   &
		pirs,   &
		msectional,   &
		maxd_asize, maxd_atype,   &
		ai_phase, cw_phase, nsize_aer, ntype_aer, ncomp_aer,   &
		do_cloudchem_aer, massptr_aer, numptr_aer, dens_aer,   &
		sigmag_aer, dcen_sect, dlo_sect, dhi_sect,   &
		volumcen_sect, volumlo_sect, volumhi_sect,   &
		lptr_so4_aer, lptr_nh4_aer, lptr_p25_aer

	use module_aerosols_sorgam_vbs, only:  getaf


	implicit none

!   subr arguments
	integer, intent(in) :: id, it, jt, kt, icase

        real, intent(inout), dimension( 1:num_chem ) :: rbox, rbox_sv1

        real, intent(in), dimension( 2, 3 ) :: xvol_old


!   local variables
	integer :: idum_msect
	integer :: ii, isize, isize_ait, isize_acc, itype
	integer :: jj
	integer :: l, lfrm, ltoo, ll
	integer :: lptr_dum(maxd_asize,maxd_atype)
	integer :: p1st
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	integer :: lunxx
#endif

	logical :: skip_xfer

	real :: delvol(2)
	real :: fracrem_num, fracrem_vol, fracxfr_num, fracxfr_vol
	real :: rbox_sv2(1:num_chem)
	real :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf
	real :: tmp_cwnumfrac, tmp_cwvolfrac
	real :: tmp_dpmeanvol, tmp_lnsg
	real :: xcut_num, xcut_vol
	real :: xdgnum_aaa(2), xlnsg(2)
	real :: xnum_aaa(2,3)
	real :: xvol_aaa(2,3)


#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   diagnostics for testing
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
#endif

	p1st = param_first_scalar

!
!   initial calculations for aitken (ii=1) and accum (ii=2) modes
!
	skip_xfer = .false.

	do ii = 1, 2
	itype = 1
	isize = ii

!   calculate new volumes for activated (jj=1), interstitial (jj=2), and combined (jj=3)
	tmpa = 0.0
	do ll = 1, ncomp_aer(itype)
	    l = massptr_aer(ll,isize,itype,cw_phase)
	    if (l >= p1st) tmpa = tmpa + rbox(l)/dens_aer(ll,itype)
	end do
	xvol_aaa(ii,1) = tmpa*1.0e-6
	xvol_aaa(ii,2) = xvol_old(ii,2)
	xnum_aaa(ii,1) = rbox(numptr_aer(isize,itype,cw_phase))
	xnum_aaa(ii,2) = rbox(numptr_aer(isize,itype,ai_phase))

	xvol_aaa(ii,3) = xvol_aaa(ii,1) + xvol_aaa(ii,2)
	xnum_aaa(ii,3) = xnum_aaa(ii,1) + xnum_aaa(ii,2)
	delvol(ii) = xvol_aaa(ii,1) - xvol_old(ii,1)

!   check for negligible number or volume in aitken mode
	if (ii == 1) then
	    if (xvol_aaa(ii,3) < smallvolaa) then
		skip_xfer = .true.
		exit
	    end if
	end if

!   calculate dgnum for activated+interstitial combined using new volume
	if (xvol_aaa(ii,3) < smallvolaa) then
	    tmp_dpmeanvol = dcen_sect(isize,itype)
	else
	    tmp_dpmeanvol = xvol_aaa(ii,3)/xnum_aaa(ii,3)
	    tmp_dpmeanvol = (tmp_dpmeanvol*6.0/pirs)**0.33333333
	end if
	xlnsg(ii) = log(sigmag_aer(isize,itype))
	xdgnum_aaa(ii) = tmp_dpmeanvol*exp(-1.5*xlnsg(ii)*xlnsg(ii))
!   tmp_cwvolfrac = (cw volume)/(ai + cw volume)
	tmp_cwvolfrac = xvol_aaa(ii,1)/max(xvol_aaa(ii,3),1.e-30)

#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   diagnostics for testing
	if (lunxx > 0) then
	    if (ii == 1) write(lunxx,'(a)')
	    write(lunxx,'(a,3i4,i8,2x,l2)') 'merge i,j,k,  ii,skip', &
		it, jt, kt, ii, skip_xfer
	    write(lunxx,'(a,1p,5e12.4)') 'cw  vol old/aaa, num aaa        ', &
		xvol_old(ii,1), xvol_aaa(ii,1), xnum_aaa(ii,1)
	    write(lunxx,'(a,1p,5e12.4)') 'ai  vol old/aaa, num aaa        ', &
		xvol_old(ii,2), xvol_aaa(ii,2), xnum_aaa(ii,2)
	    write(lunxx,'(a,1p,5e12.4)') 'a+c vol old/aaa, num aaa        ', &
		xvol_old(ii,3), xvol_aaa(ii,3), xnum_aaa(ii,3)
	    write(lunxx,'(a,1p,5e12.4)') 'cwnum/volfrac, dpmeanvol, dgnum ', &
		(xnum_aaa(ii,1)/max(xnum_aaa(ii,3),1.e-30)), &
		tmp_cwvolfrac, tmp_dpmeanvol, xdgnum_aaa(ii)
	end if
#endif

	end do  ! ii = 1, 2


!   check for mode merging
	if ( skip_xfer ) return
	if (delvol(1) > delvol(2)) then
	    continue
	else if ( (xdgnum_aaa(1) > 0.03e-4) .and. (xnum_aaa(1,3) > xnum_aaa(2,3)) ) then
	    continue
	else
	    return
	end if
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	if (lunxx > 0) then
	  if (delvol(1) > delvol(2)) then
	    write(lunxx,'(a)') 'do merging - criterion 1'
	  else if ( (xdgnum_aaa(1) > 0.03e-4) .and. (xnum_aaa(1,3) > xnum_aaa(2,3)) ) then
	    write(lunxx,'(a)') 'do merging - criterion 2'
	  end if
	end if
#endif

!
!   calc transfer fractions for volume/mass and number
!   approach follows that in module_aerosols_sorgam (subr aerostep) except
!   >>	the first steps of the calculation are done using the total
!	(interstitial+activated) size distributions
!   >>  the number and volume (moment-3) transfer amounts are then limited
!	to the number/volume of the aitken activated distribution
!
!   xcut_num = [ln(dintsect/dgnuc)/xxlsgn], where dintsect is the diameter 
!   at which the aitken-mode and accum-mode number distribs intersect (overlap).
	tmpa = sqrt(2.0)
!	aaa = getaf( nu0, ac0, dgnuc, dgacc, xxlsgn, xxlsga, sqrt2 )
	xcut_num = tmpa * getaf( xnum_aaa(1,3), xnum_aaa(2,3), &
		xdgnum_aaa(1), xdgnum_aaa(2), xlnsg(1), xlnsg(2), tmpa )

!   forcing xcut_vol>0 means that no more than half of the aitken volume
!   will be transferred
	tmpd = xcut_num
	tmpc = 3.0*xlnsg(1)
	xcut_vol = max( xcut_num-tmpc, 0.0 )
	xcut_num = xcut_vol + tmpc
	fracxfr_vol = norm01_uptail( xcut_vol )
	fracxfr_num = norm01_uptail( xcut_num )
	tmpe = fracxfr_vol ; tmpf = fracxfr_num

	tmp_cwvolfrac = xvol_aaa(1,1)/max(xvol_aaa(1,3),1.e-30)
	tmp_cwnumfrac = xnum_aaa(1,1)/max(xnum_aaa(1,3),1.e-30)
	if ( (fracxfr_vol >= tmp_cwvolfrac) .or. &
	     (fracxfr_num >= tmp_cwnumfrac) ) then
!   limit volume fraction transferred to tmp_cwvolfrac
!   limit number fraction transferred to tmp_cwnumfrac
	    fracxfr_num = 1.0
	    fracxfr_vol = 1.0
	else
!   at this point, fracxfr_num/vol are fraction of 
!	interstitial+activated num/vol to be transferred
!   convert them to fraction of activated num/vol to be transferred
	    fracxfr_vol = fracxfr_vol/max(1.0e-10,tmp_cwvolfrac)
	    fracxfr_num = fracxfr_num/max(1.0e-10,tmp_cwnumfrac)
!   number fraction transferred cannot exceed volume fraction
	    fracxfr_num = min( fracxfr_num, fracxfr_vol )
	end if
	fracrem_vol = 1.0 - fracxfr_vol
	fracrem_num = 1.0 - fracxfr_num
#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
	if (lunxx > 0) then
	    write(lunxx,'(a,1p,5e12.4)') 'xcut_num1/num2/vol              ', &
		tmpd, xcut_num, xcut_vol
	    write(lunxx,'(a,1p,5e12.4)') 'fracxfr_num3/vol3/num1,vol1     ', &
		tmpf, tmpe, fracxfr_num, fracxfr_vol
	end if
#endif
	if ( skip_xfer ) return

!   do the transfer
	rbox_sv2(:) = rbox(:)
	itype = 1
	isize_ait = 1
	isize_acc = 2

	lfrm = numptr_aer(isize_ait,itype,cw_phase)
	ltoo = numptr_aer(isize_acc,itype,cw_phase)
	rbox(ltoo) = rbox(ltoo) + rbox(lfrm)*fracxfr_num
	rbox(lfrm) = rbox(lfrm)*fracrem_num

	do ll = 1, ncomp_aer(itype)
	    lfrm = massptr_aer(ll,isize_ait,itype,cw_phase)
	    ltoo = massptr_aer(ll,isize_acc,itype,cw_phase)
	    if (lfrm >= p1st) then
		if (ltoo >= p1st) rbox(ltoo) = rbox(ltoo) &
		                             + rbox(lfrm)*fracxfr_vol
		rbox(lfrm) = rbox(lfrm)*fracrem_vol
	    end if
	end do


#if defined ( ccboxtest_box_testing_active ) || defined ( cctemp_testing_active )
!   more diagnostics for testing
	if (lunxx .gt. 0) then
	    do ll = 1, 4
		if (ll .eq. 1) then
		    lptr_dum(:,:) = lptr_so4_aer(:,:,cw_phase)
		else if (ll .eq. 2) then
		    lptr_dum(:,:) = lptr_nh4_aer(:,:,cw_phase)
		else if (ll .eq. 3) then
		    lptr_dum(:,:) = lptr_p25_aer(:,:,cw_phase)
		else if (ll .eq. 4) then
		    lptr_dum(:,:) = numptr_aer(:,:,cw_phase)
		end if

		if (ll .eq. 1) write(lunxx,'(a)')
		write(lunxx,'(2a,i6,i3,2x,a)') 'sorgam_cloudchem_apply_mode_transfer',   &
		    ' - icase, ll', icase, ll,   &
		    chem_dname_table(id,lptr_dum(1,1))(1:12)
		write(lunxx,'(a)') ' ty sz  rbox_sv1, rbox, rsub'

		itype = 1
		do ii = 1, 2
		    if (ii == 1) then
			isize = isize_ait
		    else
			isize = isize_acc
		    end if
		    l = lptr_dum(isize,itype)
		    write(lunxx,'(2i3,1p,5e14.6)') &
			itype, isize, rbox_sv1(l), rbox_sv2(l), rbox(l)
		end do
	    end do

	    if (icase .le. -5) then
		write(*,*)   &
		'*** stop in sorgam_cloudchem_apply_mode_transfer diags, icase =',   &
		icase
		call wrf_error_fatal('*** stop in sorgam_cloudchem_apply_mode_transfer diags')
	    end if
	end if
#endif


	return
	end subroutine sorgam_cloudchem_apply_mode_transfer



!-----------------------------------------------------------------------
	real function norm01_uptail( x )
!
!   norm01_uptail = cummulative pdf complement of normal(0,1) pdf
!            = integral from x to +infinity of [normal(0,1) pdf]
!
!   erfc_num_recipes is from press et al, numerical recipes, 1990, page 164
!
	implicit none
	real x, xabs
	real*8 erfc_approx, tmpa, t, z

	xabs = abs(x)
	if (xabs >= 12.962359) then
	    if (x > 0.0) then
		norm01_uptail = 0.0
	    else
		norm01_uptail = 1.0
	    end if
	    return
	end if

	z = xabs / sqrt(2.0_8)
	t = 1.0_8/(1.0_8 + 0.5_8*z)

!	erfc_approx =
!     &	  t*exp( -z*z - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
!     &	  t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
!     &	                                   t*(-1.13520398 +
!     &	  t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

	tmpa =  ( -z*z - 1.26551223_8 + t*(1.00002368_8 + t*(0.37409196_8 +   &
      	  t*(0.09678418_8 + t*(-0.18628806_8 + t*(0.27886807_8 +   &
      	                                   t*(-1.13520398_8 +   &
      	  t*(1.48851587_8 + t*(-0.82215223_8 + t*0.17087277_8 )))))))))

	erfc_approx = t * exp(tmpa)
	if (x .lt. 0.0) erfc_approx = 2.0_8 - erfc_approx

	norm01_uptail = 0.5_8 * erfc_approx

	return
	end function norm01_uptail

!-----------------------------------------------------------------------
	real function norm01_uptail_inv( x )
!
!   norm01_uptail_inv = inverse of norm01_uptail
!	if y = norm01_uptail_inv( x ), then
!	{integral from y to +infinity of [normal(0,1) pdf]} = x
!   y is computed using newton's method
!
	implicit none

!   fn parameters
	real x

!   local variables
	integer niter
	real dfdyinv, f, pi, sqrt2pi, tmpa, y, ynew

	parameter (pi = 3.1415926535897932384626434)

	if (x .le. 1.0e-38) then
	    norm01_uptail_inv = 12.962359
	    return
	else if (x .ge. 1.0) then
	    norm01_uptail_inv = -12.962359
	    return
	end if

	sqrt2pi = sqrt( 2.0*pi )

!   initial guess
!   crude
!	y = 3.0*(0.5 - x)
!   better
	tmpa = x
	tmpa = max( 0.0, min( 1.0, tmpa ) )
	tmpa = 4.0*tmpa*(1.0 - tmpa)
	tmpa = max( 1.0e-38, min( 1.0, tmpa ) )
	y = sqrt( -(pi/2.0)*log(tmpa) )
	if (x > 0.5) y = -y

	f = norm01_uptail(y) - x
	do niter = 1, 100
!   iterate - dfdy is computed analytically
	    dfdyinv = -sqrt2pi * exp( 0.5*y*y )
	    ynew = y - f*dfdyinv
	    f = norm01_uptail(ynew) - x

	    if ( (ynew == y) .or.   &
		 (abs(f) <= abs(x)*1.0e-5) ) then
		exit
	    end if
	    y = ynew
	end do
9100	format( 'niter/x/f/y/ynew', i5, 4(1pe16.8) )

	norm01_uptail_inv = ynew
	return
	end function norm01_uptail_inv



!-----------------------------------------------------------------------
	subroutine sorgam_cloudchem_dumpaa(   &
	    id, ktau, ktauc, dtstepc, config_flags,   &
	    p_phy, t_phy, rho_phy, alt,   &
	    cldfra, ph_no2,   &
	    moist, chem,   &
	    gas_aqfrac, numgas_aqfrac,   &
	    ids,ide, jds,jde, kds,kde,   &
	    ims,ime, jms,jme, kms,kme,   &
	    its,ite, jts,jte, kts,kte,   &
	    qcldwtr_cutoff,   &
	    itcur, jtcur, ktcur )

	use module_state_description, only:   &
		num_moist, num_chem, p_qc
	use module_scalar_tables, only:  chem_dname_table
	use module_configure, only:  grid_config_rec_type
	use module_data_sorgam_vbs


	implicit none

!   subr arguments
	integer, intent(in) ::   &
		id, ktau, ktauc,   &
		numgas_aqfrac,   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte,   &
		itcur, jtcur, ktcur
!   id - domain index
!   ktau - time step number
!   ktauc - gas and aerosol chemistry time step number
!   numgas_aqfrac - last dimension of gas_aqfrac

!   [ids:ide, kds:kde, jds:jde] - spatial (x,z,y) indices for 'domain'
!   [ims:ime, kms:kme, jms:jme] - spatial (x,z,y) indices for 'memory'
!       Most arrays that are arguments to chem_driver
!       are dimensioned with these spatial indices.
!   [its:ite, kts:kte, jts:jte] - spatial (x,z,y) indices for 'tile'
!       chem_driver and routines under it do calculations
!       over these spatial indices.

	type(grid_config_rec_type), intent(in) :: config_flags
!   config_flags - configuration and control parameters

	real, intent(in) ::   &
	    dtstepc, qcldwtr_cutoff
!   dtstepc - time step for gas and aerosol chemistry(s)

        real, intent(in),   &
                dimension( ims:ime, kms:kme, jms:jme ) :: &
                p_phy, t_phy, rho_phy, alt, cldfra, ph_no2
!   p_phy - air pressure (Pa)
!   t_phy - temperature (K)
!   rho_phy - moist air density (kg/m^3)
!   alt - dry air specific volume (m^3/kg)
!   cldfra - cloud fractional area (0-1)
!   ph_no2 - no2 photolysis rate (1/min)

        real, intent(in),   &
                dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
                moist
!   moist - mixing ratios of moisture species (water vapor,
!       cloud water, ...) (kg/kg for mass species, #/kg for number species)

        real, intent(inout),   &
                dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
                chem
!   chem - mixing ratios of trace gas and aerosol species (ppm for gases, 
!	ug/kg for aerosol mass species, #/kg for aerosol number species)

        real, intent(inout),   &
                dimension( ims:ime, kms:kme, jms:jme, numgas_aqfrac ) :: &
                gas_aqfrac
!   gas_aqfrac - fraction (0-1) of gas that is dissolved in cloud water


!   local variables
	integer :: it, jt, kt, l, ll, n
	integer :: isize, itype

	real :: dumai, dumcw
	real :: qcldwtr


	it = itcur
	jt = jtcur
	kt = ktcur

	write(*,*)
	write(*,*)
	write(*,*)
	write(*,9100)
	write(*,9102) ktau, it, jt, kt
9100	format( 7('----------') )
9102	format(   &
	'sorgam_cloudchem_dumpaa - ktau, i, j, k =', 4i5 )

	do 2900 itype = 1, ntype_aer
	do 2900 isize = 1, nsize_aer(itype)
	if ( .not. do_cloudchem_aer(isize,itype) ) goto 2900

	write(*,9110) isize
9110	format( / 'isize, itype =', 2i3 /   &
	'  k    cldwtr      mass-ai   numb-ai      mass-cw   numb-cw' )

	do 2800 kt = kte, kts, -1

	dumai = 0.0
	dumcw = 0.0
	do ll = 1, ncomp_aer(itype)
	    l = massptr_aer(ll,isize,itype,1)
	    dumai = dumai + chem(it,kt,jt,l)
	    l = massptr_aer(ll,isize,itype,2)
	    dumcw = dumcw + chem(it,kt,jt,l)
	end do
	write(*,9120) kt,   &
		moist(it,kt,jt,p_qc),   &
		dumai, chem(it,kt,jt,numptr_aer(isize,itype,1)),   &
		dumcw, chem(it,kt,jt,numptr_aer(isize,itype,2))
9120	format( i3, 1p, e10.2, 2(3x, 2e10.2) )

2800	continue
2900	continue

	write(*,*)
	write(*,9100)
	write(*,*)

!   map from wrf-chem 3d arrays to pegasus clm & sub arrays
	kt = ktcur
	if ((ktau .eq. 30) .and. (it .eq. 23) .and.   &
	      (jt .eq.  1) .and. (kt .eq. 11)) then
	    qcldwtr = moist(it,kt,jt,p_qc)
	    write(*,*)
	    write(*,*)
	    write(*,9102) ktau, it, jt, kt
	    write(*,*)
	    write( *, '(3(1pe10.2,3x,a))' )   &
		(chem(it,kt,jt,l), chem_dname_table(id,l)(1:12), l=1,num_chem)
	    write(*,*)
	    write( *, '(3(1pe10.2,3x,a))' )   &
		  p_phy(it,kt,jt), 'p_phy     ',   &
		  t_phy(it,kt,jt), 't_phy     ',   &
		rho_phy(it,kt,jt), 'rho_phy   ',   &
		    alt(it,kt,jt), 'alt       ',   &
		          qcldwtr, 'qcldwtr   ',   &
		   qcldwtr_cutoff, 'qcldwtrcut'
	    write(*,*)
	    write(*,9100)
	    write(*,*)
	end if


	return
	end subroutine sorgam_cloudchem_dumpaa



!-----------------------------------------------------------------------
	end module module_sorgam_vbs_cloudchem
