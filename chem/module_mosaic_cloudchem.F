!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

	module module_mosaic_cloudchem



	integer, parameter :: l_so4_aqyy = 1
	integer, parameter :: l_no3_aqyy = 2
	integer, parameter :: l_cl_aqyy  = 3
	integer, parameter :: l_nh4_aqyy = 4
	integer, parameter :: l_na_aqyy  = 5
	integer, parameter :: l_oin_aqyy = 6
	integer, parameter :: l_bc_aqyy  = 7
	integer, parameter :: l_oc_aqyy  = 8

	integer, parameter :: nyyy = 8



	contains



!-----------------------------------------------------------------------
	subroutine mosaic_cloudchem_driver(   &
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

	use module_data_mosaic_asect, only:  cw_phase, nphase_aer

	use module_data_mosaic_other, only:  k_pegbegin, name

	use module_mosaic_driver, only:  mapaer_tofrom_host


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
	integer :: it, jt, kt, kpeg, k_pegshift, l, mpeg
	integer :: icase
	integer :: igaschem_onoff, iphotol_onoff, iradical_onoff

	real :: gas_aqfrac_box(numgas_aqfrac)
	real :: ph_aq_box
	real, parameter :: qcldwtr_cutoff = 1.0e-6
	real :: qcldwtr


!   check that cw_phase is active
        if ((cw_phase .le. 0) .or. (cw_phase .gt. nphase_aer)) then
            print *, '*** mosaic_cloudchem_driver - cw_phase not active'
            return
        end if

	print 93010, 'entering mosaic_cloudchem_driver - ktau =', ktau

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


	k_pegshift = k_pegbegin - kts
	kpeg = kt + k_pegshift
	mpeg = 1
	icase = icase + 1

!   detailed dump for debugging 
	if (ktau .eq. -13579) then
!	if ((ktau .eq. 30) .and. (it .eq. 23) .and.   &
!	      (jt .eq.  1) .and. (kt .eq. 11)) then
	  call mosaic_cloudchem_dumpaa(   &
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
	call mapaer_tofrom_host( 0,                           &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      kt, kt,                     &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )

!   make call '1box' cloudchem routine
!	print 93010, 'calling mosaic_cloudchem_1 at ijk =', it, jt, kt
	call mosaic_cloudchem_1box(   &
	    id, ktau, ktauc, dtstepc,   &
	    iphotol_onoff, iradical_onoff,   &
	    ph_no2(it,kt,jt),   &
	    ph_aq_box, gas_aqfrac_box,   &
	    numgas_aqfrac, it, jt, kt, kpeg, mpeg, icase )

!   map back to wrf-chem 3d arrays 
	call mapaer_tofrom_host( 1,                           &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      kt, kt,                     &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )

	gas_aqfrac(it,kt,jt,:) = gas_aqfrac_box(:) 


3800	continue

3910	continue
3920	continue

	print 93010, 'leaving  mosaic_cloudchem_driver - ktau =', ktau, icase
93010	format( a, 8(1x,i6) )

	return
	end subroutine mosaic_cloudchem_driver



!-----------------------------------------------------------------------
	subroutine mosaic_cloudchem_1box(   &
	    id, ktau, ktauc, dtstepc,   &
	    iphotol_onoff, iradical_onoff,   &
	    photol_no2_box,   &
	    ph_aq_box, gas_aqfrac_box,   &
	    numgas_aqfrac, it, jt, kt, kpeg, mpeg, icase )

	use module_state_description, only:   &
		num_moist, num_chem

	use module_data_mosaic_asect, only:   &
		msectional,   &
        	maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_cl_aer, lptr_co3_aer,   &
		lptr_msa_aer, lptr_nh4_aer, lptr_na_aer, lptr_ca_aer,   &
		lptr_oin_aer, lptr_bc_aer, lptr_oc_aer

	use module_data_mosaic_other, only:   &
		l2maxd, ltot2, rsub

	use module_data_cmu_bulkaqchem, only:   &
	        meqn1max


	implicit none

!   subr arguments
	integer, intent(in) ::   &
		id, ktau, ktauc,   &
		numgas_aqfrac, it, jt, kt, kpeg, mpeg,   &
		icase, iphotol_onoff, iradical_onoff

	real, intent(in) ::   &
	    dtstepc, photol_no2_box

	real, intent(inout) :: ph_aq_box

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
	real :: rbox(l2maxd), rbox_sv1(l2maxd)
	real :: rbulk_cwaer(nyyy,2)

        real, dimension( maxd_asize, maxd_atype ) :: fr_partit_cw


!
!   set the lptr_yyy_cwaer
!
	iphase = cw_phase
	lptr_yyy_cwaer(:,:,l_so4_aqyy) = lptr_so4_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_no3_aqyy) = lptr_no3_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_cl_aqyy ) = lptr_cl_aer( :,:,iphase)
	lptr_yyy_cwaer(:,:,l_nh4_aqyy) = lptr_nh4_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_na_aqyy ) = lptr_na_aer( :,:,iphase)
	lptr_yyy_cwaer(:,:,l_oin_aqyy) = lptr_oin_aer(:,:,iphase)
	lptr_yyy_cwaer(:,:,l_bc_aqyy ) = lptr_bc_aer( :,:,iphase)
	lptr_yyy_cwaer(:,:,l_oc_aqyy ) = lptr_oc_aer( :,:,iphase)

!
!   xfer from rsub to rbox
!
	rbox(1:ltot2) = max( 0.0, rsub(1:ltot2,kpeg,mpeg) )
	rbox_sv1(1:ltot2) = rbox(1:ltot2)

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

	gas_aqfrac_box(:) = 0.0


!   make call to interface_to_aqoperator1
	call interface_to_aqoperator1(   &
	    istat_aqop,   &
	    dtstepc,   &
	    rbox, gas_aqfrac_box,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    co2_mixrat_in, photol_no2_in, xprescribe_ph,   &
	    iradical_in, idecomp_hmsa_hso5,   &
	    yaq_beg, yaq_end, ph_cmuaq_cur,   &
	    numgas_aqfrac, id, it, jt, kt, kpeg, mpeg, ktau, icase_in )

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
	call partition_cldwtr(   &
	    rbox, fr_partit_cw,   &
	    it, jt, kt, kpeg, mpeg, icase_in )

!
!
!   distribute changes in bulk cloud-water composition among size bins
!
!
	call distribute_bulk_changes(   &
	    rbox, rbox_sv1, fr_partit_cw,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    it, jt, kt, kpeg, mpeg, icase_in )


!
!   xfer back to rsub
!
	rsub(1:ltot2,kpeg,mpeg) = max( 0.0, rbox(1:ltot2) )


!
!   do move-sections
!
	if (msectional .lt. 1000000000) then
	    call cloudchem_apply_move_sections(   &
		rbox, rbox_sv1,   &
		it, jt, kt, kpeg, mpeg, icase_in )
	end if



	return
	end subroutine mosaic_cloudchem_1box



!-----------------------------------------------------------------------
	subroutine interface_to_aqoperator1(   &
	    istat_aqop,   &
	    dtstepc,   &
	    rbox, gas_aqfrac_box,   &
	    rbulk_cwaer, lptr_yyy_cwaer,   &
	    co2_mixrat_in, photol_no2_in, xprescribe_ph,   &
	    iradical_in, idecomp_hmsa_hso5,   &
	    yaq_beg, yaq_end, ph_cmuaq_cur,   &
	    numgas_aqfrac, id, it, jt, kt, kpeg, mpeg, ktau, icase )

	use module_state_description, only:   &
		num_chem, param_first_scalar, p_qc,   &
		p_nh3, p_hno3, p_hcl, p_sulf, p_h2so4, p_hcho,   &
		p_ora1, p_so2, p_h2o2, p_o3, p_ho,   &
		p_ho2, p_no3, p_no, p_no2, p_hono,   &
!		p_pan, p_ch3o2, p_ch3oh, p_op1
		p_pan, p_ch3o2, p_ch3oh, p_op1,      &
		p_hcooh, p_ch3ooh

	use module_data_cmu_bulkaqchem, only:   &
	        meqn1max, naers, ngas,   &
	        na4, naa, nac, nae, nah, nahmsa, nahso5,   &
	        nan, nao, nar, nas, naw,   &
	        ng4, nga, ngc, ngch3co3h, ngch3o2, ngch3o2h, ngch3oh,   &
	        ngh2o2, nghcho, nghcooh, nghno2, ngho2,   &
	        ngn, ngno, ngno2, ngno3, ngo3, ngoh, ngpan, ngso2

	use module_cmu_bulkaqchem, only:  aqoperator1

	use module_data_mosaic_asect, only:   &
        	maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_cl_aer, lptr_co3_aer,   &
		lptr_msa_aer, lptr_nh4_aer, lptr_na_aer, lptr_ca_aer,   &
		lptr_oin_aer, lptr_bc_aer, lptr_oc_aer,   &
		mw_cl_aer, mw_na_aer, mw_nh4_aer, mw_no3_aer, mw_so4_aer

	use module_data_mosaic_other, only:   &
		aboxtest_units_convert, cairclm,   &
		ktemp, l2maxd, ptotclm, rcldwtr_sub


	implicit none

!   subr arguments
	integer, intent(in) ::   &
		iradical_in, idecomp_hmsa_hso5,   &
	        numgas_aqfrac, id, it, jt, kt, kpeg, mpeg, ktau, icase
	integer, intent(inout) ::   &
		istat_aqop

	integer, intent(in) :: lptr_yyy_cwaer(maxd_asize,maxd_atype,nyyy)

	real, intent(in) ::   &
	    dtstepc, co2_mixrat_in,   &
	    photol_no2_in, xprescribe_ph

	real, intent(inout) :: ph_cmuaq_cur

        real, intent(inout), dimension( 1:l2maxd ) :: rbox

        real, intent(inout), dimension( nyyy, 2 ) :: rbulk_cwaer

        real, intent(inout), dimension( 1:numgas_aqfrac ) :: gas_aqfrac_box

        real, intent(inout), dimension( meqn1max ) :: yaq_beg, yaq_end


!   local variables
	integer :: i, iphase, isize, itype
	integer :: iaq, istat_fatal, istat_warn
	integer :: l, lunxx, lyyy
	integer :: p1st

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
!   [pres in atmospheres] = [pres in dynes/cm2] * factpatm
	factpatm = 1.0/1.01325e6
!   [cldwtr in g-h2o/m3-air] = [cldwtr in mole-h2o/mole-air] * factlwc
	factlwc = 28.966*eps*1.0e6*cairclm(kpeg)
!   [aq photolysis rate scaling factor in --] = [jno2 in 1/min] * factphoto
	factphoto = 1.6

!   [gas in ppm] = [gas in mole/mole-air] * factgas
	factgas = 1.0e6

!   [aerosol in ug/m3-air] = [aerosol in mole/mole-air] * factaer
	dum = cairclm(kpeg)*1.0e12
	factaerso4   = dum*mw_so4_aer
	factaerno3   = dum*mw_no3_aer
	factaercl    = dum*mw_cl_aer
	factaernh4   = dum*mw_nh4_aer
	factaerna    = dum*mw_na_aer
	factaeroin   = dum
	factaeroc    = dum
	factaerbc    = dum

!   rce 2005-jul-11 - use same molecular weights here as in cmu code
!	factaerso4   = dum*96.0
!	factaerno3   = dum*62.0
!	factaercl    = dum*35.5
!	factaernh4   = dum*18.0
!	factaerna    = dum*23.0

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

!
!   map from rbox to gas,aerosol
!
	temp = rbox(ktemp)

	lwc = rcldwtr_sub(kpeg,mpeg) * factlwc
	p_atm = ptotclm(kpeg) * factpatm

!   rce 2005-jul-11 - set p_atm so that cmu code's cair will match cairclm
	p_atm = cairclm(kpeg)*1.0e3*0.082058e0*temp

	photo_in = photol_no2_in * factphoto

	rh = 1.0
	iaq = 1

	tstep_beg_sec = 0.0
	tstep_end_sec = dtstepc

!   map gases and convert to ppm
	gas(:) = 0.0

	gas(nga     ) = rbox(p_nh3   ) * factgas
	gas(ngn     ) = rbox(p_hno3  ) * factgas
	if(p_hcl > param_first_scalar )   gas(ngc ) = rbox(p_hcl   ) * factgas
	if(p_sulf > param_first_scalar )  gas(ng4 ) = rbox(p_sulf  ) * factgas
	if(p_h2so4 > param_first_scalar ) gas(ng4 ) = rbox(p_h2so4 ) * factgas


	gas(nghcho  ) = rbox(p_hcho  ) * factgas
	if(p_ora1 > param_first_scalar )  gas(nghcooh ) = rbox(p_ora1  ) * factgas
        if(p_hcooh > param_first_scalar ) gas(nghcooh ) = rbox(p_hcooh ) * factgas
	gas(ngso2   ) = rbox(p_so2   ) * factgas
	gas(ngh2o2  ) = rbox(p_h2o2  ) * factgas
	gas(ngo3    ) = rbox(p_o3    ) * factgas
	gas(ngoh    ) = rbox(p_ho    ) * factgas
	gas(ngho2   ) = rbox(p_ho2   ) * factgas
	gas(ngno3   ) = rbox(p_no3   ) * factgas

	gas(ngno    ) = rbox(p_no    ) * factgas
	gas(ngno2   ) = rbox(p_no2   ) * factgas
	gas(nghno2  ) = rbox(p_hono  ) * factgas
	gas(ngpan   ) = rbox(p_pan   ) * factgas
	gas(ngch3o2 ) = rbox(p_ch3o2 ) * factgas
	gas(ngch3oh ) = rbox(p_ch3oh ) * factgas
        if(p_op1 > param_first_scalar )    gas(ngch3o2h) = rbox(p_op1   ) * factgas
        if(p_ch3ooh > param_first_scalar ) gas(ngch3o2h) = rbox(p_ch3ooh) * factgas

!   compute bulk activated-aerosol mixing ratios
	aerosol(:) = 0.0
	rbulk_cwaer(:,:) = 0.0

	iphase = cw_phase
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)

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
!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
	lunxx = 87
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    write(lunxx,*)
	    write(lunxx,*)
	    write(lunxx,*) 'interface_to_aqoperator1 - icase, irad, idecomp'
	    write(lunxx,9870) icase, iradical_in, idecomp_hmsa_hso5
	    write(lunxx,*) 'it, jt, kt, kpeg, mpeg, ktau'
	    write(lunxx,9870) it, jt, kt, kpeg, mpeg, ktau
	    write(lunxx,*) 'temp, p_atm, lwc, photo, co2, xprescribe_ph'
	    write(lunxx,9875) temp, p_atm, lwc, photo_in, co2_mixrat_in, xprescribe_ph
	    write(lunxx,*) 'ptot, cair, rcldwtr_clm, dt_sec'
	    write(lunxx,9875) ptotclm(kpeg), cairclm(kpeg),   &
		rcldwtr_sub(kpeg,mpeg), (tstep_end_sec-tstep_beg_sec)
	    write(lunxx,*) 'gas (1=nh3, 2=hno3, 3=hcl, 4=h2so4, 11=so2, 12=h2o2, 18=o3)'
	    write(lunxx,9875) gas
	    write(lunxx,*) 'rbox(nh3, hno3, hcl, h2so4, so2, h2o2, o3)'
	    write(lunxx,9875) rbox(p_nh3), rbox(p_hno3), rbox(p_hcl),   &
		rbox(p_sulf),rbox(p_h2so4), rbox(p_so2), rbox(p_h2o2), rbox(p_o3)
	    write(lunxx,*) 'aerosol (1=na, 3=nh4, 4=no3, 5=cl, 6=so4, 8=ec, 9=oc, 10=crus)'
	    write(lunxx,9875) aerosol
	    write(lunxx,*) 'rbulk_cwaer (1=so4, 2=no3, 3-cl, 4=nh4, 5=na, 6=oin, 7=bc, 8=oc)'
	    write(lunxx,9875) rbulk_cwaer(:,1)
!	    if (icase .ge. 3) then
!		write(*,*)   &
!		 '*** stopping in interface_to_aqop1 at icase =', icase
!		stop
!	    end if
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
       write(6,'(a,1p,2e20.12)') 'tstep_beg_sec, tstep_end_sec = ',tstep_beg_sec, tstep_end_sec
       do l=1,ngas
          write(6,'("gas(",i2,") = ",1p,1e20.12)') l, gas(l)
       end do
       do l=1,naers
          write(6,'("aerosol(",i2,") = ",1p,1e20.12)') l, aerosol(l)
       end do
       write(6,'(a,1p,4e20.12)') "temp, p_atm, lwc, rh = ", temp, p_atm, lwc, rh
       write(6,'(a,1p,3e20.12)') "co2_mixrat_in, photo_in, xprescribe_ph = ",   &
           co2_mixrat_in, photo_in, xprescribe_ph
       write(6,'(a,3i)') " iradical_in, idecomp_hmsa_hso5, iaq = ", iradical_in, idecomp_hmsa_hso5, iaq
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
		'*** mosaic_cloudchem_driver, subr interface_to_aqoperator1'
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
		'*** mosaic_cloudchem_driver, sulfur balance warning'
	    write(6,'(a,4i5,1p,3e12.4)')   &
		'    id,it,jt,kt, total_sulfur_beg, _end, _error =',   &
		id, it, jt, kt, totsulf_beg, totsulf_end, dum
	end if

!
!   map from gas,aerosol to rbox
!
	rbox(p_nh3   ) = gas(nga     ) / factgas
	rbox(p_hno3  ) = gas(ngn     ) / factgas
	if(p_hcl .gt. param_first_scalar)   rbox(p_hcl   ) = gas(ngc     ) / factgas
	if(p_sulf .gt. param_first_scalar)  rbox(p_sulf  ) = gas(ng4) / factgas
	if(p_h2so4 .gt. param_first_scalar) rbox(p_h2so4 ) = gas(ng4) / factgas


	rbox(p_hcho  ) = gas(nghcho  ) / factgas
	if(p_ora1 > param_first_scalar ) rbox(p_ora1  ) = gas(nghcooh ) / factgas
        if(p_hcooh > param_first_scalar ) rbox(p_hcooh) = gas(nghcooh ) / factgas
	rbox(p_so2   ) = gas(ngso2   ) / factgas
	rbox(p_h2o2  ) = gas(ngh2o2  ) / factgas
	rbox(p_o3    ) = gas(ngo3    ) / factgas
	rbox(p_ho    ) = gas(ngoh    ) / factgas
	rbox(p_ho2   ) = gas(ngho2   ) / factgas
	rbox(p_no3   ) = gas(ngno3   ) / factgas

	rbox(p_no    ) = gas(ngno    ) / factgas
	rbox(p_no2   ) = gas(ngno2   ) / factgas
	rbox(p_hono  ) = gas(nghno2  ) / factgas
	rbox(p_pan   ) = gas(ngpan   ) / factgas
	rbox(p_ch3o2 ) = gas(ngch3o2 ) / factgas
	rbox(p_ch3oh ) = gas(ngch3oh ) / factgas
	if(p_op1 > param_first_scalar )    rbox(p_op1   ) = gas(ngch3o2h) / factgas
        if(p_ch3ooh > param_first_scalar ) rbox(p_ch3ooh) = gas(ngch3o2h) / factgas

	gas_aqfrac_box(:) = 0.0

	if (p_nh3   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_nh3   ) = gas_aqfrac_cmu(nga     )
	if (p_hno3  .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_hno3  ) = gas_aqfrac_cmu(ngn     )
	if (p_hcl   .le. numgas_aqfrac .and. p_hcl .gt. param_first_scalar)   &
		gas_aqfrac_box(p_hcl   ) = gas_aqfrac_cmu(ngc     )
	if (p_sulf  .le. numgas_aqfrac .and. p_sulf .gt. param_first_scalar)   &
		gas_aqfrac_box(p_sulf  ) = gas_aqfrac_cmu(ng4     )
	if (p_h2so4  .le. numgas_aqfrac .and. p_h2so4 .gt. param_first_scalar) &
		gas_aqfrac_box(p_h2so4 ) = gas_aqfrac_cmu(ng4     )

	if (p_hcho  .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_hcho  ) = gas_aqfrac_cmu(nghcho  )
	if (p_ora1  .le. numgas_aqfrac .and. p_ora1 .gt. param_first_scalar)   &
		gas_aqfrac_box(p_ora1  ) = gas_aqfrac_cmu(nghcooh )
        if (p_hcooh .le. numgas_aqfrac .and. p_hcooh .gt. param_first_scalar)   &
                gas_aqfrac_box(p_hcooh ) = gas_aqfrac_cmu(nghcooh )
	if (p_so2   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_so2   ) = gas_aqfrac_cmu(ngso2   )
	if (p_h2o2  .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_h2o2  ) = gas_aqfrac_cmu(ngh2o2  )
	if (p_o3    .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_o3    ) = gas_aqfrac_cmu(ngo3    )
	if (p_ho    .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_ho    ) = gas_aqfrac_cmu(ngoh    )
	if (p_ho2   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_ho2   ) = gas_aqfrac_cmu(ngho2   )
	if (p_no3   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_no3   ) = gas_aqfrac_cmu(ngno3   )

	if (p_no    .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_no    ) = gas_aqfrac_cmu(ngno    )
	if (p_no2   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_no2   ) = gas_aqfrac_cmu(ngno2   )
	if (p_hono  .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_hono  ) = gas_aqfrac_cmu(nghno2  )
	if (p_pan   .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_pan   ) = gas_aqfrac_cmu(ngpan   )
	if (p_ch3o2 .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_ch3o2 ) = gas_aqfrac_cmu(ngch3o2 )
	if (p_ch3oh .le. numgas_aqfrac)   &
		gas_aqfrac_box(p_ch3oh ) = gas_aqfrac_cmu(ngch3oh )
	if (p_op1   .le. numgas_aqfrac .and. p_op1 .gt. param_first_scalar)   &
		gas_aqfrac_box(p_op1   ) = gas_aqfrac_cmu(ngch3o2h)
        if (p_ch3ooh.le. numgas_aqfrac .and. p_ch3ooh .gt. param_first_scalar)   &
                gas_aqfrac_box(p_ch3ooh) = gas_aqfrac_cmu(ngch3o2h)

	rbulk_cwaer(l_so4_aqyy,2) = aerosol(na4) / factaerso4
	rbulk_cwaer(l_no3_aqyy,2) = aerosol(nan) / factaerno3
	rbulk_cwaer(l_cl_aqyy, 2) = aerosol(nac) / factaercl
	rbulk_cwaer(l_nh4_aqyy,2) = aerosol(naa) / factaernh4
	rbulk_cwaer(l_na_aqyy, 2) = aerosol(nas) / factaerna
	rbulk_cwaer(l_oin_aqyy,2) = aerosol(nar) / factaeroin
	rbulk_cwaer(l_bc_aqyy, 2) = aerosol(nae) / factaerbc
	rbulk_cwaer(l_oc_aqyy, 2) = aerosol(nao) / factaeroc


!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
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
		rbox(p_sulf), rbox(p_h2so4), rbox(p_so2), rbox(p_h2o2), rbox(p_o3)
	    write(lunxx,*) 'aerosol (1=na, 3=nh4, 4=no3, 5=cl, 6=so4, 8=ec, 9=oc, 10=crus)'
	    write(lunxx,9875) aerosol
	    write(lunxx,*) 'rbulk_cwaer (1=so4, 2=no3, 3-cl, 4=nh4, 5=na, 6=oin, 7=bc, 8=oc)'
	    write(lunxx,9875) rbulk_cwaer(:,2)
	    write(lunxx,*) 'ph_cmuaq_cur'
	    write(lunxx,9875) ph_cmuaq_cur
	    if (icase .ge. 10) then
		write(*,*)   &
		 '*** stopping in interface_to_aqop1 at icase =', icase
!		stop
	    end if
	end if
#endif


	return
	end subroutine interface_to_aqoperator1



!-----------------------------------------------------------------------
	subroutine partition_cldwtr(   &
	    rbox, fr_partit_cw,   &
	    it, jt, kt, kpeg, mpeg, icase )

	use module_state_description, only:   &
        param_first_scalar

	use module_data_mosaic_asect, only:   &
        	maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer, ncomp_aer,   &
		massptr_aer, numptr_aer,   &
		dens_aer, mw_aer, volumlo_sect, volumhi_sect

	use module_data_mosaic_other, only:   &
		aboxtest_units_convert, cairclm,   &
		ktemp, l2maxd, ptotclm, rcldwtr_sub


	implicit none

!   subr arguments
	integer, intent(in) :: it, jt, kt, kpeg, mpeg, icase

        real, intent(inout), dimension( 1:l2maxd ) :: rbox

        real, intent(inout), dimension( maxd_asize, maxd_atype ) ::   &
		fr_partit_cw

!   local variables
	integer :: iphase, isize, itype
	integer :: jdone_mass, jdone_numb, jpos, jpos_mass, jpos_numb
	integer :: l, ll, lunxx
	integer :: p1st

	real, parameter :: partit_wght_mass = 0.5

	real :: dum, duma, dumb, dumc, dummass, dumnumb, dumvolu
	real :: tmass, tnumb, umass, unumb, wmass, wnumb
    real, dimension( maxd_asize, maxd_atype ) :: fmass, fnumb, xmass, xnumb

    p1st = PARAM_FIRST_SCALAR

	iphase = cw_phase
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
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    dummass = 0.0
	    dumvolu = 0.0
	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		if (l .ge. p1st) then
		    dum = max( 0.0, rbox(l) )*mw_aer(ll,itype)
		    dummass = dummass + dum
		    dumvolu = dumvolu + dum/dens_aer(ll,itype)
		end if
	    end do

	    l = numptr_aer(isize,itype,iphase)
	    dumnumb = max( 0.0, rbox(l) )
	    if (dumnumb .gt. dumvolu/volumlo_sect(isize,itype)) then
	        dumnumb = dumvolu/volumlo_sect(isize,itype)
		rbox(l) = dumnumb
	    else if (dumnumb .lt. dumvolu/volumhi_sect(isize,itype)) then
	        dumnumb = dumvolu/volumhi_sect(isize,itype)
		rbox(l) = dumnumb
	    end if

	    if (dummass .lt. 1.0e-37) dummass = 0.0
	    xmass(isize,itype) = dummass
	    if (dumnumb .lt. 1.0e-37) dumnumb = 0.0
	    xnumb(isize,itype) = dumnumb

	    tmass = tmass + xmass(isize,itype)
	    tnumb = tnumb + xnumb(isize,itype)
	    umass = max( umass, xmass(isize,itype) )
	    unumb = max( unumb, xnumb(isize,itype) )
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
		if (fr_partit_cw(isize,itype) .gt. 0.0) jpos = jpos + 1
	    end do
	    end do

!   if only 1 bin has fr_partit_cw > 0, set value to 1.0 exactly
	    if (jpos .eq. 1) then
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
		if (fr_partit_cw(isize,itype) .gt. 0.0)   &
			fr_partit_cw(isize,itype) = 1.0
	    end do
	    end do
	    end if
	end if


!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
!   diagnostics when lunxx > 0
	lunxx = 86
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	if (icase .le. 9) then
	    write(lunxx,9800)
	    write(lunxx,9800)
	    write(lunxx,9800)   &
		'partition_cldwtr - icase, jpos, jpos_mass, jpos_numb'
	    write(lunxx,9810)  icase, jpos, jpos_mass, jpos_numb
	    write(lunxx,9800) 'tmass, umass, wmass'
	    write(lunxx,9820)  tmass, umass, wmass
	    write(lunxx,9800) 'tnumb, unumb, wnumb'
	    write(lunxx,9820)  tnumb, unumb, wnumb
	    write(lunxx,9800) 'xmass, fmass, xnumb, fnumb, fr_partit_cw'
	    duma = 0.0
	    dumb = 0.0
	    dumc = 0.0
	    do itype = 1, ntype_aer
	    do isize = 1, nsize_aer(itype)
	        write(lunxx,9820)  xmass(isize,itype), fmass(isize,itype),   &
			xnumb(isize,itype), fnumb(isize,itype),   &
			fr_partit_cw(isize,itype)
	        duma = duma + fmass(isize,itype)
	        dumb = dumb + fnumb(isize,itype)
	        dumc = dumc + fr_partit_cw(isize,itype)
	    end do
	    end do
	    write(lunxx,9800)   &
		'sum_fmass-1.0,  sum_fnumb-1.0,  sum_fr_partit-1.0'
	    write(lunxx,9820)  (duma-1.0), (dumb-1.0), (dumc-1.0)
	    if (icase .eq. -2) then
		write(*,*) '*** stopping in partition_cldwtr at icase =', icase
!		stop
	    end if
9800	    format( a )
9810	    format( 5i10 )
9820	    format( 5(1pe10.2) )
	end if
	end if
#endif


	return
	end subroutine partition_cldwtr



!-----------------------------------------------------------------------
	subroutine distribute_bulk_changes(   &
		rbox, rbox_sv1, fr_partit_cw,   &
		rbulk_cwaer, lptr_yyy_cwaer,   &
		it, jt, kt, kpeg, mpeg, icase )

	use module_state_description, only:   &
		param_first_scalar

	use module_data_mosaic_asect, only:   &
		maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_cl_aer, lptr_co3_aer,   &
		lptr_msa_aer, lptr_nh4_aer, lptr_na_aer, lptr_ca_aer,   &
		lptr_oin_aer, lptr_bc_aer, lptr_oc_aer

	use module_data_mosaic_other, only:  l2maxd, lunout, name


	implicit none

!   subr arguments
	integer, intent(in) :: it, jt, kt, kpeg, mpeg, icase

	integer, intent(in) :: lptr_yyy_cwaer(maxd_asize,maxd_atype,nyyy)

        real, intent(inout), dimension( 1:l2maxd ) :: rbox, rbox_sv1

        real, intent(in), dimension( maxd_asize, maxd_atype ) ::   &
		fr_partit_cw

        real, intent(in), dimension( nyyy, 2 ) :: rbulk_cwaer


!   local variables
	integer :: iphase, isize, itype
	integer :: idone, icount, ncount
	integer :: jpos, jpos_sv
	integer :: l, lunxx, lunxxaa, lunxxbb, lyyy
	integer :: p1st

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
!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
	    lunxxaa = 86
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
!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
	lunxxbb = 86
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


!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
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
		write(lunxx,9801) 'distribute_bulk_changes - ',name(lptr_yyy_cwaer(1,1,lyyy)), ' - icase, lyyy'
		write(lunxx,9810) icase, lyyy
		write(lunxx,9800) ' tp sz  rbox_sv1, rbox, del_rbox, del_rbox/del_rbulk_cwaer, (...-fr_partit_cw)'
		write(lunxx,9840) 0, 0, rbulk_cwaer(lyyy,1:2), duma
		do itype = 1, ntype_aer
		do isize = 1, nsize_aer(itype)
		    l = lptr_yyy_cwaer(isize,itype,lyyy)
		    dumb = rbox(l) - rbox_sv1(l)
		    dumc = dumb/max( abs(duma), 1.0e-35 )
		    if (duma .lt. 0.0) dumc = -dumc
		    write(lunxx,9840) itype, isize, rbox_sv1(l), rbox(l),   &
			dumb, dumc, (dumc-fr_partit_cw(isize,itype))
		end do
		end do
	    end if
	    end do
	    if (icase .ge. 5) then
		write(*,*)   &
		'*** stop in distribute_bulk_changes diags, icase =', icase
!		stop
	    end if
	end if
#endif


	return
	end subroutine distribute_bulk_changes



!-----------------------------------------------------------------------
	subroutine cloudchem_apply_move_sections(   &
		rbox, rbox_sv1,   &
		it, jt, kt, kpeg, mpeg, icase )

	use module_state_description, only:   &
		param_first_scalar

	use module_data_mosaic_asect, only:   &
		msectional,   &
		maxd_asize, maxd_atype,   &
		cw_phase, nsize_aer, ntype_aer, ncomp_aer,   &
		massptr_aer, numptr_aer, mw_aer, dens_aer,   &
		lptr_so4_aer, lptr_no3_aer, lptr_cl_aer, lptr_co3_aer,   &
		lptr_msa_aer, lptr_nh4_aer, lptr_na_aer, lptr_ca_aer,   &
		lptr_oin_aer, lptr_bc_aer, lptr_oc_aer,   &
		drymass_aftgrow, drymass_pregrow,   &
		drydens_aftgrow, drydens_pregrow

	use module_data_mosaic_other, only:  l2maxd, name, rsub

	use module_mosaic_movesect, only:  move_sections


	implicit none

!   subr arguments
	integer, intent(in) :: it, jt, kt, kpeg, mpeg, icase

        real, intent(inout), dimension( 1:l2maxd ) :: rbox, rbox_sv1


!   local variables
	integer :: idum_msect
	integer :: iphase, isize, itype
	integer :: l, ll, lunxx
	integer :: p1st
	integer :: lptr_dum(maxd_asize,maxd_atype)

	real :: densdefault
	real :: dmaft, dmpre, dvaft, dvpre
	real :: duma, dumb, dumc
	real :: smallmassbb


	p1st = param_first_scalar
	iphase = cw_phase

!
!   compute drymass before and after growth
!   set drydens = -1.0, so it will be calculated
!
	densdefault = 2.0
	smallmassbb = 1.0e-30

	do 1800 itype = 1, ntype_aer
	do 1800 isize = 1, nsize_aer(itype)
	    dmaft = 0.0
	    dmpre = 0.0
	    dvaft = 0.0
	    dvpre = 0.0

	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		if (l .ge. p1st) then
		    duma = mw_aer(ll,itype)
		    dmaft = dmaft + duma*rbox(l)
		    dmpre = dmpre + duma*rbox_sv1(l)

		    duma = duma/dens_aer(ll,itype)
		    dvaft = dvaft + duma*rbox(l)
		    dvpre = dvpre + duma*rbox_sv1(l)
		end if
	    end do

	    drymass_aftgrow(isize,itype) = dmaft
	    drymass_pregrow(isize,itype) = dmpre

	    if (min(dmaft,dvaft) .le. smallmassbb) then
		drydens_aftgrow(isize,itype) = densdefault
	    else
		drydens_aftgrow(isize,itype) = dmaft/dvaft
	    end if
	    if (min(dmpre,dvpre) .le. smallmassbb) then
		drydens_pregrow(isize,itype) = densdefault
	    else
		drydens_pregrow(isize,itype) = dmpre/dvpre
	    end if

1800	continue


!   apply move sections routine
!   (and conditionally turn on movesect diagnostics)
	idum_msect = msectional

!#if defined ( ccboxtest_box_testing_active )
#if defined (      temp_box_testing_active)
	lunxx = 88
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    if (msectional .lt. 7000) msectional = msectional + 7000
	end if
#endif

	call move_sections( 2, it, jt, kpeg, mpeg )

	msectional = idum_msect


!#if defined ( ccboxtest_box_testing_active)
#if defined (      temp_box_testing_active)
!   diagnostics for testing
	lunxx = 88
	lunxx = -1
	if ((it==23) .and. (jt==1) .and. (kt<=20)) lunxx = 171
	if (lunxx .gt. 0) then
	    do ll = 1, 4
		if (ll .eq. 1) then
		    lptr_dum(:,:) = lptr_so4_aer(:,:,iphase)
		else if (ll .eq. 2) then
		    lptr_dum(:,:) = lptr_nh4_aer(:,:,iphase)
		else if (ll .eq. 3) then
		    lptr_dum(:,:) = lptr_oin_aer(:,:,iphase)
		else if (ll .eq. 4) then
		    lptr_dum(:,:) = numptr_aer(:,:,iphase)
		end if

		if (ll .eq. 1) write(lunxx,9800)
		if (ll .eq. 1) write(lunxx,9800)
		write(lunxx,9800)
		write(lunxx,9800)
		write(lunxx,9801) 'cloudchem_apply_move_sections - ',   &
		    name(lptr_dum(1,1)), ' - icase, ll'
		write(lunxx,9810) icase, ll
		write(lunxx,9800) ' tp sz  rbox_sv1, rbox, rsub'

		do itype = 1, ntype_aer
		do isize = 1, nsize_aer(itype)
		    l = lptr_dum(isize,itype)
		    dumb = rbox(l) - rbox_sv1(l)
		    dumc = dumb/max( abs(duma), 1.0e-35 )
		    if (duma .lt. 0.0) dumc = -dumc
		    write(lunxx,9840) itype, isize, rbox_sv1(l), rbox(l),   &
			rsub(l,kpeg,mpeg)
		end do
		end do
	    end do

	    if (icase .ge. 5) then
		write(*,*)   &
		'*** stop in cloudchem_apply_move_sections diags, icase =',   &
		icase
!		stop
	    end if
	end if
9800	format( a )
9801	format( 3a )
9810	format( 7i10 )
9820	format( 7(1pe10.2) )
9840	format( 2i3, 5(1pe14.6) )
#endif


	return
	end subroutine cloudchem_apply_move_sections



!-----------------------------------------------------------------------
	subroutine mosaic_cloudchem_dumpaa(   &
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

	use module_configure, only:  grid_config_rec_type

	use module_data_mosaic_asect

	use module_data_mosaic_other, only:  k_pegbegin, name

	use module_mosaic_driver, only:  mapaer_tofrom_host


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
	'mosaic_cloudchem_dumpaa - ktau, i, j, k =', 4i5 )

	itype = 1
	do 2900 isize = 1, nsize_aer(itype)

	write(*,9110) isize
9110	format( / 'isize =', i3 /   &
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
		(chem(it,kt,jt,l), name(l)(1:10), l=1,num_chem)
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
	end subroutine mosaic_cloudchem_dumpaa



!-----------------------------------------------------------------------
	end module module_mosaic_cloudchem
