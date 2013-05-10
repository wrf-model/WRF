!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
	module module_mosaic_drydep


! rce 2005-feb-18 - several fixes for indices of dlo/hi_sect, volumlo/hi_sect,
!     which are now (isize,itype)

! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect


	contains


!-----------------------------------------------------------------------
	subroutine mosaic_drydep_driver(                                   &
		id, curr_secs, ktau, dtstep, config_flags,                     &
		gmt, julday,                                                   &
		t_phy, rho_phy, p_phy,                                         &
		ust, aer_res,                                                  &
		moist, chem, ddvel,                                            &
		ids,ide, jds,jde, kds,kde,                                     &
		ims,ime, jms,jme, kms,kme,                                     &
		its,ite, jts,jte, kts,kte                                      )

	use module_configure, only:  grid_config_rec_type, num_moist, num_chem
	use module_state_description, only:  param_first_scalar

	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_mosaic_driver, only:  mapaer_tofrom_host
	use module_peg_util, only:  peg_error_fatal

	implicit none

!   subr arguments
	integer, intent(in) ::   &
		id, ktau, julday

	integer, intent(in) ::   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

    real(kind=8), intent(in) :: curr_secs

	real, intent(in) :: dtstep, gmt

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
		t_phy, rho_phy, p_phy

	real, intent(in),   &
		dimension( ims:ime, jms:jme ) :: &
		ust

	real, intent(in),   &
		dimension( its:ite, jts:jte ) :: &
		aer_res

	real, intent(in),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_moist ) :: &
		moist
 
	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem

	real, intent(inout),   &
		dimension( its:ite, jts:jte, 1:num_chem ) :: &
		ddvel


	type(grid_config_rec_type), intent(in) :: config_flags


!   local variables
	integer idum, jdum
	integer it, jt, kt
	integer iphase, itype
	integer ktmaps, ktmape
	integer ll, l1, n
	integer levdbg_err, levdbg_info

	integer idiagaa_dum, ijcount_dum

	real dum
	real vdep_aer(maxd_asize,maxd_atype,maxd_aphase)

	character*100 msg


!!rcetestdd diagnostics --------------------------------------------------
!	print 93010, ' '
!	print 93010, 'rcetestdd diagnostics from mosaic_drydep_driver'
!	print 93010, 'id, chem_opt, ktau, julday           ',   &
!	              id, config_flags%chem_opt, ktau, julday
!	print 93010, 'ims/e, j, k', ims, ime, jms, jme, kms, kme
!	print 93010, 'its/e, j, k', its, ite, jts, jte, kts, kte
!
!	do jdum = 0, 2
!	do idum = 0, 2
!	jt = jts + ((jte-jts)/2)*jdum
!	it = its + ((ite-its)/2)*idum
!if (idum .eq. 2) it = ite
!if (jdum .eq. 2) jt = jte
!	kt = kts
!	print 93050, 'it, jt, t, p, ust, aer_res', it, jt,   &
!		t_phy(it,kt,jt), p_phy(it,kt,jt), ust(it,jt), aer_res(it,jt)
!	end do
!	end do
!
!93010	format( a, 8(1x,i6) )
!93020	format( a, 8(1p,e14.6) )
!93050	format( a, 2(1x,i4), 8(1p,e14.6) )
!!rcetestdd diagnostics --------------------------------------------------


!   ktmaps,ktmape = first/last wrf kt for which depvel is calculated
!   ktot = number of levels at which depvel is calculated
	ktmaps = kts
	ktmape = kts
	ktot = 1


!   set some variables to their wrf-chem "standard" values
	lunerr = -1
	lunout = -1
	levdbg_err = 0
        levdbg_info = 15

	iymdcur = 20030822
	ihmscur = 0
	ihmscur = nint( mod( real(gmt*3600.,8)+curr_secs, 86400.0_8 ) )

	t = curr_secs
	ncorecnt = ktau - 1

!   reset some variables to "box test" values
!   (*** aboxtest_get_extra_args is for "box testing" only
!   and should be not be called from wrf-chem ***)
!	call aboxtest_get_extra_args( 20,   &
!		iymdcur, ihmscur,   &
!		aboxtest_units_convert, aboxtest_rh_method,   &
!		aboxtest_map_method, aboxtest_gases_fixed,  &
!		lunerr, lunout, t, dum )


!   set "pegasus" grid size variables
	itot = ite
	jtot = jte
	nsubareas = 1

	ijcount_dum = 0


	do 2920 jt = jts, jte
	do 2910 it = its, ite

	ijcount_dum = ijcount_dum + 1


	call mapaer_tofrom_host( 0,                       &
		ims,ime, jms,jme, kms,kme,                    &
		its,ite, jts,jte, kts,kte,                    &
		it,      jt,      ktmaps,ktmape,              &
		num_moist, num_chem, moist, chem,             &
		t_phy, p_phy, rho_phy                         )


!   compute deposition velocities
	idiagaa_dum = 1
	idiagaa_dum = 0
	if ((jt.ne.jts) .and. (jt.ne.jte) .and.   &
			(jt.ne.(jts+jte)/2)) idiagaa_dum = 0
	if ((it.ne.its) .and. (it.ne.ite) .and.   &
			(it.ne.(its+ite)/2)) idiagaa_dum = 0

	call mosaic_drydep_1clm( idiagaa_dum, it, jt,   &
		ust(it,jt), aer_res(it,jt), vdep_aer )


! rce 23-nov-2004 - change to using the "..._aer" species pointers
	do iphase = 1, nphase_aer
	do itype = 1, ntype_aer
	do n = 1, nsize_aer(itype)
	    do ll = -2, ncomp_plustracer_aer(itype)
		if (ll .eq. -2) then
        	    l1 = numptr_aer(n,itype,iphase)
		else if (ll .eq. -1) then
		    l1 = -1
		    if (iphase .eq. ai_phase) l1 = waterptr_aer(n,itype)
		else if (ll .eq. 0) then
		    l1 = -1
		    if (iphase .eq. ai_phase) l1 = hyswptr_aer(n,itype)
		else
		    l1 = massptr_aer(ll,n,itype,iphase)
		end if
		if (l1 .ge. param_first_scalar) then
		    ddvel(it,jt,l1) = vdep_aer(n,itype,iphase)
		end if
	    end do
	end do
	end do
	end do


2910	continue
2920	continue
!	print 93010, 'leaving mosaic_drydep_driver'


	return
	end subroutine mosaic_drydep_driver


!-----------------------------------------------------------------------
	subroutine mosaic_drydep_1clm( idiagaa, it, jt,   &
		ustar_in, depresist_a_in, vdep_aer )

	use module_configure, only:  grid_config_rec_type

	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_mosaic_driver, only:  mapaer_tofrom_host
	use module_peg_util, only:  peg_error_fatal

	implicit none

!   subr arguments
	integer, intent(in) :: idiagaa, it, jt

!   friction velocity (m/s)
	real, intent(in) :: ustar_in
!   aerodynamic resistance (s/m)
	real, intent(in) :: depresist_a_in

!   deposition velocities (m/s)
	real, intent(inout) :: vdep_aer(maxd_asize,maxd_atype,maxd_aphase)

!   local variables
	real, parameter :: densdefault = 2.0
	real, parameter :: smallmassaa = 1.0e-20
	real, parameter :: smallmassbb = 1.0e-30
	real, parameter :: piover6 = pi/6.0
	real, parameter :: onethird = 1.0/3.0

	integer iphase, itype, k, ll, l1, m, n

	real airdens, airkinvisc
	real depresist_a, depresist_unstabpblfact
	real depresist_d0, depresist_d3
	real depvel_a0, depvel_a3
	real drydens, drydp, drymass, dryvol
	real dum, dumalnsg, dumfact, dummass
	real freepath
	real rnum
	real temp
	real ustar
	real vsettl_0, vsettl_3
	real wetdgnum, wetdens, wetdp, wetmass, wetvol


!       if (idiagaa>0) print *, ' '
	k = 1
	m = 1

!   temperature (K)
	temp = rsub(ktemp,k,m)
!   air density (g/cm^3)
!	airdens = cairclm(1)*xmwair
	airdens = cairclm(1)*28.966
!   air kinematic viscosity (cm^2/s)
	airkinvisc = ( 1.8325e-4 * (416.16/(temp+120.0)) *   &
      				((temp/296.16)**1.5) ) / airdens
!   air molecular freepath (cm)
	freepath = 7.39758e-4 * airkinvisc / sqrt(temp)
!   friction velocity (cm/s)
	ustar = ustar_in * 100.0
!   aerodynamic resistance (s/cm)
	depresist_a = depresist_a_in * 0.01

!   enhancement factor for unstable pbl
	depresist_unstabpblfact = 1.0


!   initialize vdep_aer
	vdep_aer(:,:,:) = 0.0

!   *** for now, just calc vdep_aer for iphase = ai_phase
	iphase = ai_phase

!   calculate vdep_aer
	do 2900 itype = 1, ntype_aer
	do 2800 n = 1, nsize_aer(itype)

	dryvol = 0.0
	drymass = 0.0
	do ll = 1, ncomp_aer(itype)
	    l1 = massptr_aer(ll,n,itype,iphase)
	    dummass = rsub(l1,k,m)*mw_aer(ll,itype)
	    drymass = drymass + dummass
	    dryvol = dryvol + dummass/dens_aer(ll,itype)
	end do

	l1 = waterptr_aer(n,itype)
	dummass = rsub(l1,k,m)*mw_water_aer
	wetmass = drymass + dummass
	wetvol = dryvol + dummass/dens_water_aer

	l1 = numptr_aer(n,itype,iphase)
	rnum = rsub(l1,k,m)

	if (drymass .le. smallmassbb) then
	    drydp = dcen_sect(n,itype)
	    drydens = densdefault
	    wetdp = drydp
	    wetdens = drydens
	    goto 1900
	end if

!jdf    if (drymass .le. smallmassbb) then
	if (drymass .le. smallmassaa) then
	    wetmass = drymass
	    wetvol = dryvol
	end if
	drydens = drymass/dryvol
	wetdens = wetmass/wetvol


	if (rnum .ge. dryvol/volumlo_sect(n,itype)) then
	    drydp = dlo_sect(n,itype)
	else if (rnum .le. dryvol/volumhi_sect(n,itype)) then
	    drydp = dhi_sect(n,itype)
	else
	    drydp = (dryvol/(piover6*rnum))**onethird
	end if

!jdf    dumfact = (wetvol/dryvol)**onethird
!jdf    dumfact = min( dumfact, 10.0 )
        if(abs(wetvol).gt.(1000.*abs(dryvol))) then
          dumfact=10.0
        else
          dumfact=abs(wetvol/dryvol)**onethird
          dumfact=max(1.0,min(dumfact,10.0))
        endif
!jdf
	wetdp = drydp*dumfact

1900	continue


!
!   get surface resistance and settling velocity for mass (moment 3)
!   and number (moment 0)
!
	dumalnsg = log( 1.0 )
	wetdgnum = wetdp * exp( -1.5*dumalnsg*dumalnsg )
	call aerosol_depvel_2(   &
      	    wetdgnum, dumalnsg, wetdens,   &
      	    temp, airdens, airkinvisc, freepath,   &
      	    ustar, depresist_unstabpblfact,   &
      	    depresist_d0, vsettl_0,   &
      	    depresist_d3, vsettl_3 )

!
!   compute overall deposition velocity (binkowski/shankar eqn a33)
!
	dum = depresist_a + depresist_d3 +   &
      		    depresist_a*depresist_d3*vsettl_3
	depvel_a3 = vsettl_3 + (1./dum)

	dum = depresist_a + depresist_d0 +   &
      		    depresist_a*depresist_d0*vsettl_0
	depvel_a0 = vsettl_0 + (1./dum)

!   cm/s --> m/s
	vdep_aer(n,itype,iphase) = depvel_a3 * 0.01


!
!   diagnostic output
!
	if (idiagaa>0) print 9310, it, jt, n, itype, iphase,   &
		dcen_sect(n,itype), drydp, wetdp,   &
		drydens, wetdens, vdep_aer(n,itype,iphase),   &
		vsettl_3, depresist_d3, depresist_a
9310	format( 'aerdep', 2i4, 3i3, 1p, 3e10.2,   &
		2x, 0p, 2f5.2, 2x, 1p, 4e10.2 )


2800	continue
2900	continue


	return
	end subroutine mosaic_drydep_1clm


!------------------------------------------------------------------------
!------------------------------------------------------------------------
	subroutine aerosol_depvel_2(   &
      	    dgnum, alnsg, aerodens,   &
      	    temp, airdens, airkinvisc, freepath,   &
      	    ustar, depresist_unstabpblfact,   &
      	    depresist_d0, vsettl_0,   &
      	    depresist_d3, vsettl_3 )
!
!   computes the surface layer resistance term and the
!   gravitational settling velocity term for the 3rd moment
!   of a log-normal aerosol mode
!
!   input parameters
!	dgnum - geometric mean diameter for aerosol number (cm)
!	alnsg - natural logarithm of the geometric standard deviation
!		for aerosol number
!	aerodens - aerosol density (dgnum and aerodens are for the
!		actual wet distribution)
!	temp - temperature (K)
!	airdens - air density (g/cm^3)
!	airkinvisc - air kinematic viscosity (cm^2/s)
!	freepath - air molecular freepath (cm)
!	ustar - friction velocity (cm/s)
!	depresist_unstabpblfact = weseley et al. 1985 factor for increasing
!	    depvel in unstable pbl -- either
!		1. + (-0.3*zi/L)**0.667 OR
!		1. + 0.24*((wstar/ustar)**2)
!   output parameters
!	depresist_d3 - surface layer resistance for 3rd (mass) moment (s/cm)
!	vsettl_3 - gravitational settling velocity for 3rd moment (cm/s)
!	depresist_d0 - surface layer resistance for 0th (number) moment (s/cm)
!	vsettl_0 - gravitational settling velocity for 0th moment (cm/s)
!	

	implicit none

	real dgnum, alnsg, aerodens,   &
      	    temp, airdens, airkinvisc, freepath,   &
      	    ustar, depresist_unstabpblfact,   &
      	    depresist_d0, vsettl_0,   &
      	    depresist_d3, vsettl_3

	real aerodiffus_0, schmidt_0, stokes_0, facdepresist_d0
	real aerodiffus_3, schmidt_3, stokes_3, facdepresist_d3
	common / aerosol_depvel_cmn01 /   &
      		aerodiffus_0, schmidt_0, stokes_0, facdepresist_d0,   &
      		aerodiffus_3, schmidt_3, stokes_3, facdepresist_d3

	real xknudsen, xknudsenfact, alnsg2, duma, dumb,   &
      	vsettl_dgnum, aerodiffus_dgnum

	real pi
	parameter (pi = 3.1415926536)
!   gravity = gravitational acceleration in cm/s^2
	real gravity
	parameter (gravity = 980.616)
!   boltzmann constant in erg/deg-K
	real boltzmann
	parameter (boltzmann = 1.3807e-16)

	xknudsen = 2.*freepath/dgnum
	xknudsenfact = xknudsen*1.246
	alnsg2 = alnsg*alnsg

	vsettl_dgnum = (gravity*aerodens*dgnum*dgnum)/   &
      		(18.*airkinvisc*airdens)
	vsettl_0 = vsettl_dgnum *   &
      		( exp(2.*alnsg2) + xknudsenfact*exp(0.5*alnsg2) )
	vsettl_3 = vsettl_dgnum *   &
      		( exp(8.*alnsg2) + xknudsenfact*exp(3.5*alnsg2) )

	aerodiffus_dgnum = (boltzmann*temp)/   &
      		(3.*pi*airkinvisc*airdens*dgnum)
	aerodiffus_0 = aerodiffus_dgnum *   &
      		( exp(+0.5*alnsg2) + xknudsenfact*exp(+2.*alnsg2) )
	aerodiffus_3 = aerodiffus_dgnum *   &
      		( exp(-2.5*alnsg2) + xknudsenfact*exp(-4.*alnsg2) )

	schmidt_0 = airkinvisc/aerodiffus_0
	schmidt_3 = airkinvisc/aerodiffus_3

	stokes_0 = ustar*ustar*vsettl_0/(gravity*airkinvisc)
	stokes_3 = ustar*ustar*vsettl_3/(gravity*airkinvisc)
	
	duma = (schmidt_0**(-0.66666666)) +   &
      		(10.**(-3./max(0.03,stokes_0)))
!	dumb = duma*ustar*(1. + 0.24*wstaroverustar*wstaroverustar)
	dumb = duma*ustar*depresist_unstabpblfact
	depresist_d0 = 1./max( dumb, 1.e-20 )
	facdepresist_d0 = duma

	duma = (schmidt_3**(-0.66666666)) +   &
      		(10.**(-3./max(0.03,stokes_3)))
!	dumb = duma*ustar*(1. + 0.24*wstaroverustar*wstaroverustar)
	dumb = duma*ustar*depresist_unstabpblfact
	depresist_d3 = 1./max( dumb, 1.e-20 )
	facdepresist_d3 = duma

	return
	end subroutine aerosol_depvel_2 


!-----------------------------------------------------------------------
	end module module_mosaic_drydep
