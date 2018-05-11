	module module_mosaic_movesect1d


	use module_data_mosaic_kind, only:  r8
	use module_data_mosaic_main, only:  mw_air, ntot_used, pi
	!use module_data_mosaic_aero, only:  it_mosaic, mmovesect_flag1 !BSINGH
	use module_data_mosaic_asecthp, only:  lunerr, lunout
	use module_peg_util


	implicit none


	contains


!-----------------------------------------------------------------------
!**********************************************************************************  
! following routines adapted from
!    fjaersky:/home/d37080/box/aqchem/pandis/ccboxwrf6/module_mosaic_movesect.F.saveaa
!**********************************************************************************  


!-----------------------------------------------------------------------
	subroutine move_sections_x3( iphase_flag, iclm, jclm, k, m, rbox,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam,   &
      	  drydens_aftgrow, drydens_pregrow,   &
      	  drymass_aftgrow, drymass_pregrow,   &
      	  adrydens_tmp, awetdens_tmp, adrydpav_tmp, awetdpav_tmp,   &
      	  adryqmas_tmp, it_mosaic, mmovesect_flag1, idiag_movesect )
!
!   routine transfers aerosol number and mass between sections
!	to account for continuous aerosol growth
!   this routine is called after the gas condensation module (MOSAIC) or
!	aqueous chemistry module has increased the mass within sections
!
!   moving-center algorithm or mass-number advection algorithm is used,
!   depending on value of mod(mmovesect_flag1,100)
!	section mean diameter is given by
!	    vtot = ntot * (pi/6) * (dmean**3)
!	where vtot and ntot are total dry-volume and number for the section
!	if dmean is outside the section boundaries (dlo_sect & dhi_sect), then
!	    all the mass and number in the section are transfered to the
!	    section with dlo_sect(nnew) < dmean < dhi_sect(nnew)
!
!   mass mixing ratios are in rbox(massptr_aer(ll,n,itype,iphase))
!       units such that rbox*fact_apmassmr gives (g-AP/g-air)
!   number mixing ratios are in rbox(numptr_aer(n,itype,iphase))
!       units such that rbox*fact_apnumbmr gives (#/g-air)
!   these values are over-written with new values

!   the following are also updated:  
!	adrydens_tmp(n,itype), adrydpav_tmp(n,itype),
!	awetdens_tmp(n,itype), awetdpav_tmp(n,itype)
!	adryqmas_tmp(n,itype)
!
!   the module_data_mosaic_asecthp densities are such that
!	dens_aer*fact_apdens give (g/cm^3)
!   the module_data_mosaic_asecthp diameters are such that
!	dlo/hi_sect*fact_apdiam give (cm)
!   the module_data_mosaic_asecthp volumes are such that
!	volumlo/hi_sect**fact_apdiam**3) give (cm^3)
!
!   input parameters
!	iphase_flag = 1 - do transfer after trace-gas condensation/evaporation
!	      = 2 - do transfer after aqueous chemistry
!	      = -1/-2 - do some "first entry" tasks for the iphase_flag=+1/+2 cases
!
!	iclm, jclm, k = current i,j,k indices
!	m = current subarea index
!
!	drymass_pregrow(n,itype) = dry-mass for section n before the growth
!	drymass_aftgrow(n,itype) = dry-mass for section n after the growth 
!	                           but before inter-section transfer
!           (units for both are same as rbox mass entries)
!	drydens_pregrow(n,itype) = dry-density for section n before the growth
!	drydens_aftgrow(n,itype) = dry-density for section n after the growth
!	                           but before inter-section transfer
!           (units for both are same as dens_aer)
!
!	(drymass_pregrow and drydens_pregrow are used by the linear-discrete
!	    algorithm but not the moving-center algorithm)
!
!       method_movesect (from first two digits of movesect_flag1, which
!	    is a "data_module" variable)
!	    10 - do moving-center algorithm
!	    20 - do linear-discrete algorithm
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize, maxd_atype,   &
	    ntype_aer, nsize_aer, nphase_aer, ai_phase, cw_phase,   &
	    dens_aer, dcen_sect,   &
	    smallmassaa_asecthp => smallmassaa, smallmassbb_asecthp => smallmassbb,   &
	    volumcen_sect, volumlo_sect, volumhi_sect

	implicit none

!   subr arguments
	integer, intent(in) :: iphase_flag, iclm, jclm, k, m, it_mosaic, &
	                       mmovesect_flag1, idiag_movesect

	real(r8), intent(inout) :: rbox(ntot_used)

	real(r8), intent(in) :: fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam

	real(r8), intent(in) :: drymass_pregrow(maxd_asize,maxd_atype)
        real(r8), intent(in) :: drydens_pregrow(maxd_asize,maxd_atype)
        real(r8), intent(in) :: drymass_aftgrow(maxd_asize,maxd_atype)
        real(r8), intent(in) :: drydens_aftgrow(maxd_asize,maxd_atype)

	real(r8), intent(inout) :: adrydens_tmp(maxd_asize,maxd_atype), awetdens_tmp(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: adrydpav_tmp(maxd_asize,maxd_atype), awetdpav_tmp(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: adryqmas_tmp(maxd_asize,maxd_atype)
! adrydens_tmp = aerosol dry density (units same as dens_aer)
! awetdens_tmp = aerosol wet density (units same as dens_aer)
! adrydpav_tmp = aerosol mean dry diameter (units same as dlo_sect)
! awetdpav_tmp = aerosol mean wet diameter (units same as dlo_sect)
! adryqmas_tmp = aerosol total-dry-mass mixing ratio (units same as rbox)

!   local variables
	integer iphase, isize, itype,   &
	  l, ll, llhysw, llwater, lnew, lold, l3,   &
      	  method_movesect, n, nnew, nold
	integer nnewsave(2,maxd_asize)

	real(r8) densdefault, densh2o
	real(r8) delta_water_conform1, delta_numb_conform1
	real(r8) fact_apvolu
	real(r8) smallmassaa, smallmassbb

	real(r8) dcen_stmp(maxd_asize,maxd_atype)
	real(r8) drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	    drydensxx(maxd_asize), drydensyy(maxd_asize)
	real(r8) drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	    drymassxx(maxd_asize), drymassyy(maxd_asize)
	real(r8) dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize),   &
      	    rmassyy(maxd_acomp+2,maxd_asize)
	real(r8) rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	    rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real(r8) specdensxx(maxd_acomp)
	real(r8) xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real(r8) volumcen_stmp(maxd_asize,maxd_atype), &
	    volumlo_stmp(maxd_asize,maxd_atype), volumhi_stmp(maxd_asize,maxd_atype)
	real(r8) wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real(r8) wetmassxx(maxd_asize), wetmassyy(maxd_asize)

	character*160 msg


!
!   check for valid inputs
!
	if (mmovesect_flag1 <= 0) return
	if (ntype_aer       <= 0) return
	if (nphase_aer      <= 0) return


!   get "method_movesect" from digits 1-2 of mmovesect_flag1 (treat 1-9 as 10)
	method_movesect = mod( mmovesect_flag1, 100 )
!   treat 1-9 as 10
	if (method_movesect .le. 10) method_movesect = 10

	if      ((method_movesect .eq. 10) .or.   &
      		 (method_movesect .eq. 20)) then
	    continue
	else
	    msg = '*** subr move_sections error - ' //   &
		'illegal value for mmovesect_flag1'
	    call peg_error_fatal( lunerr, msg )
	end if


!   check iphase_flag
	if (iabs(iphase_flag) .eq. 1) then
	    iphase = ai_phase
	else if (iabs(iphase_flag) .eq. 2) then
!	    iphase = cw_phase
!	    if (nphase_aer .lt. 2) then
!		msg = '*** subr move_sections error - ' //   &
!		    'iphase_flag=2 (after aqueous chemistry) but nphase_aer < 2'
!		call peg_error_fatal( lunerr, msg )
!	    else if (cw_phase .ne. 2) then
!		msg = '*** subr move_sections error - ' //   &
!		    'iphase_flag=2 (after aqueous chemistry) but cw_phase .ne. 2'
!		call peg_error_fatal( lunerr, msg )
!	    end if
	    msg = '*** subr move_sections error - ' //   &
		'iphase_flag=2 (after aqueous chemistry) is not implemented'
	    call peg_error_fatal( lunerr, msg )
	else
	    msg = '*** subr move_sections error - ' //   &
		'iabs(iphase_flag) must be 1 or 2'
	    call peg_error_fatal( lunerr, msg )
	end if


!   when iphase_flag=-1/-2, call move_sections_checkptrs then return
!	if ((ncorecnt .le. 0) .and. (k .le. 1)) then
	if (iphase_flag .le. 0) then
	    write(msg,9040) 'method', method_movesect
	    call peg_message( lunout, msg )
	    write(msg,9040) 'idiag ', idiag_movesect
	    call peg_message( lunout, msg )
	    call move_sections_checkptrs( iphase_flag, iclm, jclm, k, m )
	    return
	end if
9040	format( '*** subr move_sections - ', a, ' =', i6 )


	densdefault = 2.0
	densh2o = 1.0
	smallmassaa = smallmassaa_asecthp
	smallmassbb = smallmassbb_asecthp

!   smallmassaa & smallmassbb are now set in module_data_mosaic_asecthp
!
!   if bin mass mixrat < smallmassaa (1.e-22 g/g), then assume no growth
!   AND no water AND conform number so that size is within bin limits
!
!   if bin mass OR volume mixrat < smallmassbb (1.e-30 g/g), then
!   assume default density to avoid divide by zero


!   calc single particle sizes and volumes in (cm) and (cm^3)
	fact_apvolu = fact_apdiam*fact_apdiam*fact_apdiam
!	write(*,'(/a,1p,5e12.4/)') 'fact_ap...', &
!          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu
	do itype = 1, ntype_aer
	do isize = 1, nsize_aer(itype)
	    dcen_stmp(    isize,itype) = dcen_sect(    isize,itype)*fact_apdiam
	    volumcen_stmp(isize,itype) = volumcen_sect(isize,itype)*fact_apvolu
	    volumlo_stmp( isize,itype) = volumlo_sect( isize,itype)*fact_apvolu
	    volumhi_stmp( isize,itype) = volumhi_sect( isize,itype)*fact_apvolu
	end do
	end do


!   process each type, one at a time
	do 1900 itype = 1, ntype_aer

	densdefault = dens_aer(1,itype)*fact_apdens   ! use density of first component as default

	if (nsize_aer(itype) .le. 0) goto 1900

	if (idiag_movesect .ge. 70) then
	    msg = ' '
	    call peg_message( lunout, msg )
	    write(msg,9060) mmovesect_flag1, iclm, jclm, k, m, it_mosaic, itype
	    call peg_message( lunout, msg )
	end if
9060	format( '*** move_sections diags - ', &
	    'msflag, ijkm, itim, ityp =', i7, 3i4,i2, i7, i5 )


	do n = 1, nsize_aer(itype)
	    drydenspp(n) = drydens_pregrow(n,itype)*fact_apdens
	    drydensxx(n) = drydens_aftgrow(n,itype)*fact_apdens

	    drymasspp(n) = drymass_pregrow(n,itype)*fact_apmassmr
	    drymassxx(n) = drymass_aftgrow(n,itype)*fact_apmassmr
	end do

	call move_sections_initial_conform(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx, specdensxx,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )

	if (method_movesect .le. 19) then
	call move_sections_calc_movingcenter(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )
	else
	call move_sections_calc_masnumadvect(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )
	end if

	call move_sections_apply_moves(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum,   &
	  dcen_stmp, volumcen_stmp, volumlo_stmp, volumhi_stmp,   &
      	  adrydens_tmp, awetdens_tmp, adrydpav_tmp, awetdpav_tmp,   &
      	  adryqmas_tmp )


! *** wrf-chem code has call to move_sections_apply_n1_inflow here
!     this is not needed in the mosaic box model


!	call move_sections_final_conform(   &
!	  iphase_flag, iclm, jclm, k, m, iphase, itype )

1900	continue

	return
	end subroutine move_sections_x3


!-----------------------------------------------------------------------
	subroutine move_sections_initial_conform(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx, specdensxx,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )

!
!   routine does some initial tasks for the section movement
!	load rmassxx & rnumbxx from rbox
!	load specdensxx
!	set drymassxx & dryvolxx from drymass_aftgrow & drydens_aftgrow,
!	    OR compute them from rmassxx, specdensxx if need be
!	set wetmassxx & wetvolxx from dry values & water mass
!	conform rnumbxx so that the mean particle size of each section
!	    (= dryvolxx/rnumbxx) is within the section limits
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize, maxd_atype,   &
	    nsize_aer, ncomp_aer, ncomp_plustracer_aer, ai_phase,   &
	    massptr_aer, numptr_aer, waterptr_aer, hyswptr_aer,   &
	    dens_aer

	implicit none

!   subr arguments
	integer iphase_flag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	real(r8) densdefault, densh2o, smallmassaa, smallmassbb
	real(r8) fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu
	real(r8) delta_water_conform1, delta_numb_conform1
	real(r8) drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	     drydensxx(maxd_asize)
	real(r8) drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	     drymassxx(maxd_asize)
	real(r8) dryvolxx(maxd_asize)
	real(r8) rbox(ntot_used)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize)
	real(r8) rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	     rnumbxx(maxd_asize)
	real(r8) specdensxx(maxd_acomp)
	real(r8) volumcen_stmp(maxd_asize,maxd_atype), &
	    volumlo_stmp(maxd_asize,maxd_atype), volumhi_stmp(maxd_asize,maxd_atype)
	real(r8) wetvolxx(maxd_asize)
	real(r8) wetmassxx(maxd_asize)


!   local variables
	integer l, ll, lnew, lold, l3, n, nnew, nold

	real(r8) dummass, dumnum, dumnum_at_dhi, dumnum_at_dlo, dumr,   &
      	  dumvol, dumvol1p, dumwatrmass


!   assure positive definite
	do l = 1, ntot_used
	    rbox(l) = max( 0.0_r8, rbox(l) )
	end do

!   load mixrats into working arrays and assure positive definite
	llhysw = ncomp_plustracer_aer(itype) + 1
	llwater = ncomp_plustracer_aer(itype) + 2
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)
		l = massptr_aer(ll,n,itype,iphase)
		rmassxx(ll,n) = rbox(l)*fact_apmassmr
	    end do
	    rmassxx(llhysw,n) = 0.
	    l = 0
	    if (iphase .eq. ai_phase) l = hyswptr_aer(n,itype)
	    if (l .gt. 0) rmassxx(llhysw,n) = rbox(l)*fact_apmassmr
	    rmassxx(llwater,n) = 0.
	    l = 0
	    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
	    if (l .gt. 0) rmassxx(llwater,n) = rbox(l)*fact_apmassmr

	    rnumbxx(n)  = rbox(numptr_aer(n,itype,iphase))*fact_apnumbmr
	    rnumbxx0(n) = rnumbxx(n)
	    rnumbpp(n)  = rnumbxx(n)

	    drydensxx0(n) = drydensxx(n)
	    drymassxx0(n) = drymassxx(n)
	end do

!   load specdens also
	do ll = 1, ncomp_plustracer_aer(itype)
	    specdensxx(ll) = dens_aer(ll,itype)*fact_apdens
	end do

	delta_water_conform1 = 0.0
	delta_numb_conform1 = 0.0


	do 1390 n = 1, nsize_aer(itype)

!
!   if drydens_aftgrow < 0.1 g/cm^3, then bin had state="no_aerosol"
!   compute volume using default dry-densities, set water=0,
!	and conform the number
!   also do this if mass is extremely small (below smallmassaa)
!	OR if drydens_aftgrow > 20 g/cm^3 (which is unreal(r8))
!
	if ( (drydensxx(n) .lt.  0.1) .or.   &
	     (drydensxx(n) .gt. 20.0) .or.   &
      	     (drymassxx(n) .le. smallmassaa) ) then
	    dummass = 0.
	    dumvol = 0.
	    do ll = 1, ncomp_aer(itype)
		dumr = rmassxx(ll,n)
		dummass = dummass + dumr
		dumvol  = dumvol  + dumr/specdensxx(ll)
	    end do
	    drymassxx(n) = dummass
	    if (min(dummass,dumvol) .le. smallmassbb) then
		drydensxx(n) = densdefault
		dumvol = dummass/densdefault
		dumnum = dummass/(volumcen_stmp(n,itype)*densdefault)
	    else
		drydensxx(n) = dummass/dumvol
		dumnum = rnumbxx(n)
		dumnum_at_dhi = dumvol/volumhi_stmp(n,itype)
		dumnum_at_dlo = dumvol/volumlo_stmp(n,itype)
		dumnum = max( dumnum_at_dhi, min( dumnum_at_dlo, dumnum ) )
	    end if
	    delta_numb_conform1 = delta_numb_conform1 + dumnum - rnumbxx(n)
	    rnumbxx(n) = dumnum
	    rnumbpp(n) = rnumbxx(n)
	    delta_water_conform1 = delta_water_conform1 - rmassxx(llwater,n) 
	    rmassxx(llwater,n) = 0.
	end if

!   load dry/wet mass and volume into "xx" arrays
!   which hold values before inter-mode transferring
	dryvolxx(n) = drymassxx(n)/drydensxx(n)
	dumwatrmass = rmassxx(llwater,n)
	wetmassxx(n) = drymassxx(n) + dumwatrmass
	wetvolxx(n) = dryvolxx(n) + dumwatrmass/densh2o

1390	continue

	return
	end subroutine move_sections_initial_conform                          


!-----------------------------------------------------------------------
	subroutine move_sections_calc_movingcenter(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )
!
!   routine calculates section movements for the moving-center approach
!
!   material in section n will be transfered to section nnewsave(1,n)
!
!   the nnewsave are calculated here
!   the actual transfer is done in another routine
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize, maxd_atype, nsize_aer

	implicit none

!   subr arguments
	integer iphase_flag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)
	real(r8) densdefault, densh2o, smallmassaa, smallmassbb
	real(r8) drydensxx(maxd_asize)
	real(r8) drymassxx(maxd_asize)
	real(r8) dryvolxx(maxd_asize)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize)
	real(r8) rnumbxx(maxd_asize)
	real(r8) xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real(r8) volumcen_stmp(maxd_asize,maxd_atype), &
	    volumlo_stmp(maxd_asize,maxd_atype), volumhi_stmp(maxd_asize,maxd_atype)
	real(r8) wetmassxx(maxd_asize)
	real(r8) wetvolxx(maxd_asize)

!   local variables
	integer isize, itmpa, itmpb, ll, n, ndum, nnew, nold
	real(r8) dumnum, dumvol, dumvol1p, sixoverpi, third
	character*160 fmtaa, msg
	character*11 txt11


	sixoverpi = 6.0/pi
	third = 1.0/3.0

!
!   compute mean size after growth (and corresponding section)
!   particles in section n will be transferred to section nnewsave(1,n)
!
	do 1390 n = 1, nsize_aer(itype)

	nnew = n

!   don't bother to transfer bins whose mass is extremely small
	if (drymassxx(n) .le. smallmassaa) goto 1290

	dumvol = dryvolxx(n)
	dumnum = rnumbxx(n)

!   check for number so small that particle volume is
!   above that of largest section
	isize = nsize_aer(itype)
	if ( dumnum .le. dumvol/volumhi_stmp(isize,itype) ) then
	    nnew = nsize_aer(itype)
	    goto 1290
!   or below that of smallest section
	else if ( dumnum .ge. dumvol/volumlo_stmp(1,itype) ) then
	    nnew = 1
	    goto 1290
	end if

!   dumvol1p is mean particle volume (cm3) for the section
	dumvol1p = dumvol/dumnum
	if ( dumvol1p .gt. volumhi_stmp(n,itype) ) then
	    do while ( ( nnew .lt. nsize_aer(itype) ) .and.   &
      		       ( dumvol1p .gt. volumhi_stmp(nnew,itype) ) )
		nnew = nnew + 1
	    end do

	else if ( dumvol1p .lt. volumlo_stmp(n,itype) ) then
	    do while ( ( nnew .gt. 1 ) .and.   &
      		       ( dumvol1p .lt. volumlo_stmp(nnew,itype) ) )
		nnew = nnew - 1
	    end do

	end if

1290	nnewsave(1,n) = nnew
	nnewsave(2,n) = 0

	xferfracvol(1,n) = 1.0
	xferfracvol(2,n) = 0.0
	xferfracnum(1,n) = 1.0
	xferfracnum(2,n) = 0.0

1390	continue


!   diagnostic output
	if (idiag_movesect .ge. 70) then
	    ndum = 0
	    do n = 1, nsize_aer(itype)
		if (nnewsave(1,n) .ne. n) ndum = ndum + 1
	    end do
	    if (ndum .gt. 0) then
		txt11 = 'movesectYES'
	    else
		txt11 = 'movesectNO '
	    end if
	    do itmpa = 1, nsize_aer(itype), 24
	        itmpb = min( itmpa+23, nsize_aer(itype) )
		if (itmpa == 1) then
		    fmtaa = '( a, 4i3, i5, i6, 3x, 24i3 )'
		    write(msg,fmtaa) txt11, iclm, jclm, k, m, itype,   &
	      		ndum, (nnewsave(1,n), n=itmpa,itmpb)
		else
		    fmtaa = '( 37x, 24i3 )'
		    write(msg,fmtaa) (nnewsave(1,n), n=itmpa,itmpb)
		end if
		call peg_message( lunout, msg )
	    end do
	end if

	return
	end subroutine move_sections_calc_movingcenter                          


!-----------------------------------------------------------------------
	subroutine move_sections_calc_masnumadvect(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum,   &
	  volumcen_stmp, volumlo_stmp, volumhi_stmp )
!
!   routine calculates section movements for the mass-number-advection approach
!
!   material in section n will be transfered to sections
!	nnewsave(1,n) and nnewsave(2,n)
!   the fractions of mass/volume transfered to each are
!	xferfracvol(1,n) and xferfracvol(2,n)
!   the fractions of number transfered to each are
!	xferfracnum(1,n) and xferfracnum(2,n)
!
!   the nnewsave, xferfracvol, and xferfracnum are calculated here
!   the actual transfer is done in another routine
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize, maxd_atype, nsize_aer

	implicit none

!   subr arguments
	integer iphase_flag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)

	real(r8) densdefault, densh2o, smallmassaa, smallmassbb
	real(r8) drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	     drydensxx(maxd_asize)
	real(r8) drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	     drymassxx(maxd_asize)
	real(r8) dryvolxx(maxd_asize)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize)
	real(r8) rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	     rnumbxx(maxd_asize)
	real(r8) xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real(r8) volumcen_stmp(maxd_asize,maxd_atype), &
	    volumlo_stmp(maxd_asize,maxd_atype), volumhi_stmp(maxd_asize,maxd_atype)
	real(r8) wetvolxx(maxd_asize)
	real(r8) wetmassxx(maxd_asize)

!   local variables
	integer ierr, n, nnew, nnew2
	integer iforce_movecenter(maxd_asize)

	real(r8) dum1, dum2, dum3
	real(r8) dumaa, dumbb, dumgamma, dumratio
	real(r8) dumfracnum, dumfracvol
	real(r8) dumntot
	real(r8) dumv
	real(r8) dumvbar_aft, dumvbar_pre
	real(r8) dumvcutlo_nnew_pre, dumvcuthi_nnew_pre
	real(r8) dumvlo_pre, dumvhi_pre, dumvdel_pre
	real(r8) dumvtot_aft, dumvtot_pre
	real(r8) dumzlo, dumzhi
	real(r8) sixoverpi, third, tmpa

	character*4 dumch4
	character*1 dumch1
	character*160 msg


	sixoverpi = 6.0/pi
	third = 1.0/3.0

!
!   compute mean size after growth (and corresponding section)
!   some of the particles in section n will be transferred to section nnewsave(1,n)
!
!   if the aftgrow mass is extremely small,
!   OR if the aftgrow mean size is outside of
!       [dlo_sect(1,itype), dhi_sect(nsize_aer(itype),itype)]
!   then use the moving-center method_movesect for this bin
!   (don't try to compute the pregrow within-bin distribution)
!
	do 3900 n = 1, nsize_aer(itype)

	nnew = n
	iforce_movecenter(n) = 0

	xferfracvol(1,n) = 1.0
	xferfracvol(2,n) = 0.0
	xferfracnum(1,n) = 1.0
	xferfracnum(2,n) = 0.0

	dumvtot_aft = -1.0
	dumvtot_pre = -1.0
	dumvbar_aft = -1.0
	dumvbar_pre = -1.0
	dumvlo_pre = -1.0
	dumvhi_pre = -1.0
	dumgamma = -1.0
	dumratio = -1.0
	dumvcutlo_nnew_pre = volumlo_stmp(nnew,itype)*(dumvbar_pre/dumvbar_aft)
	dumvcuthi_nnew_pre = volumhi_stmp(nnew,itype)*(dumvbar_pre/dumvbar_aft)
	dumfracvol = -1.0
	dumfracnum = -1.0

!   don't bother to transfer bins whose mass is extremely small
	if (drymassxx(n) .le. smallmassaa) then
	    iforce_movecenter(n) = 1
	    goto 1290
	end if

	dumvtot_aft = dryvolxx(n)
	dumntot = rnumbxx(n)

!   check for particle volume above that of largest section
!   or below that of smallest section
	if (dumntot .le. dumvtot_aft/volumhi_stmp(nsize_aer(itype),itype)) then
	    nnew = nsize_aer(itype)
	    iforce_movecenter(n) = 2
	    goto 1290
	else if (dumntot .ge. dumvtot_aft/volumlo_stmp(1,itype)) then
	    nnew = 1
	    iforce_movecenter(n) = 3
	    goto 1290
	end if

!   dumvbar_aft is mean particle volume (cm3) for the section
!   find the section that encloses this volume
	dumvbar_aft = dumvtot_aft/dumntot
	if (dumvbar_aft .gt. volumhi_stmp(n,itype)) then
	    do while ( (nnew .lt. nsize_aer(itype)) .and.   &
      		       (dumvbar_aft .gt. volumhi_stmp(nnew,itype)) )
		nnew = nnew + 1
	    end do

	else if (dumvbar_aft .lt. volumlo_stmp(n,itype)) then
	    do while ( (nnew .gt. 1) .and.   &
      		       (dumvbar_aft .lt. volumlo_stmp(nnew,itype)) )
		nnew = nnew - 1
	    end do

	end if

1290	nnewsave(1,n) = nnew
	nnewsave(2,n) = 0

	if (iforce_movecenter(n) .gt. 0) goto 3700


!   if drydenspp (pregrow) < 0.1 (because bin had state="no_aerosol" before
!	growth was computed, so its initial mass was very small)
!   then use the moving-center method_movesect for this bin
!   (don't try to compute the pregrow within-bin distribution)
!   also do this if pregrow mass is extremely small (below smallmassaa)
!	OR if drydenspp > 20 g/cm^3 (unphysical)
	if ( (drydenspp(n) .lt.  0.1) .or.   &
	     (drydenspp(n) .gt. 20.0) .or.   &
      	     (drymasspp(n) .le. smallmassaa) ) then
	    iforce_movecenter(n) = 11
	    goto 3700
	end if

	dumvtot_pre = drymasspp(n)/drydenspp(n)

	dumvlo_pre = volumlo_stmp(n,itype)
	dumvhi_pre = volumhi_stmp(n,itype)
	dumvdel_pre = dumvhi_pre - dumvlo_pre

!   if the pregrow mean size is outside of OR very close to the bin limits,
!   then use moving-center approach for this bin
	dumv = dumvhi_pre - 0.01*dumvdel_pre
	if (dumntot .le. dumvtot_pre/dumv) then
	    iforce_movecenter(n) = 12
	    goto 3700
	end if
	dumv = dumvlo_pre + 0.01*dumvdel_pre
	if (dumntot .ge. dumvtot_pre/dumv) then
	    iforce_movecenter(n) = 13
	    goto 3700
	end if

!   calculate the pregrow within-section size distribution
	dumvbar_pre = dumvtot_pre/dumntot
	dumgamma = (dumvhi_pre/dumvlo_pre) - 1.0
	dumratio = dumvbar_pre/dumvlo_pre

	if (dumratio .le. (1.0001 + dumgamma/3.0)) then
	    dumv = dumvlo_pre + 3.0*(dumvbar_pre-dumvlo_pre)
	    dumvhi_pre = min( dumvhi_pre, dumv )
	    dumvdel_pre = dumvhi_pre - dumvlo_pre
	    dumgamma = (dumvhi_pre/dumvlo_pre) - 1.0
	    dumratio = dumvbar_pre/dumvlo_pre
	else if (dumratio .ge. (0.9999 + dumgamma*2.0/3.0)) then
	    dumv = dumvhi_pre + 3.0*(dumvbar_pre-dumvhi_pre)
	    dumvlo_pre = max( dumvlo_pre, dumv )
	    dumvdel_pre = dumvhi_pre - dumvlo_pre
	    dumgamma = (dumvhi_pre/dumvlo_pre) - 1.0
	    dumratio = dumvbar_pre/dumvlo_pre
	end if

	dumbb = (dumratio - 1.0 - 0.5*dumgamma)*12.0/dumgamma
	dumaa = 1.0 - 0.5*dumbb

!   calculate pregrow volumes corresponding to the nnew
!   section boundaries
	dumvcutlo_nnew_pre = volumlo_stmp(nnew,itype)*(dumvbar_pre/dumvbar_aft)
	dumvcuthi_nnew_pre = volumhi_stmp(nnew,itype)*(dumvbar_pre/dumvbar_aft)

!   if the [dumvlo_pre, dumvhi_pre] falls completely within
!   the [dumvcutlo_nnew_pre, dumvcuthi_nnew_pre] interval,
!   then all mass and number go to nnew
	if (nnew .eq. 1) then
	    if (dumvhi_pre .le. dumvcuthi_nnew_pre) then
		iforce_movecenter(n) = 21
	    else
		nnew2 = nnew + 1
	    end if
	else if (nnew .eq. nsize_aer(itype)) then
	    if (dumvlo_pre .ge. dumvcutlo_nnew_pre) then
		iforce_movecenter(n) = 22
	    else
		nnew2 = nnew - 1
	    end if
	else
	    if ((dumvlo_pre .ge. dumvcutlo_nnew_pre) .and.   &
      		(dumvhi_pre .le. dumvcuthi_nnew_pre)) then
		iforce_movecenter(n) = 23
	    else if (dumvlo_pre .lt. dumvcutlo_nnew_pre) then
		nnew2 = nnew - 1
	    else
		nnew2 = nnew + 1
	    end if
	end if
	if (iforce_movecenter(n) .gt. 0) goto 3700

!   calculate the fraction of ntot and vtot that are within
!   the [dumvcutlo_nnew_pre, dumvcuthi_nnew_pre] interval
	dumzlo = (dumvcutlo_nnew_pre - dumvlo_pre)/dumvdel_pre
	dumzhi = (dumvcuthi_nnew_pre - dumvlo_pre)/dumvdel_pre
	dumzlo = max( dumzlo, 0.0_r8 )
	dumzhi = min( dumzhi, 1.0_r8 )
	dum1 =  dumzhi    - dumzlo
	dum2 = (dumzhi**2 - dumzlo**2)*0.5
	dum3 = (dumzhi**3 - dumzlo**3)/3.0
	dumfracnum = dumaa*dum1 + dumbb*dum2
	dumfracvol = (dumvlo_pre/dumvbar_pre) * (dumaa*dum1 +   &
      		(dumaa*dumgamma + dumbb)*dum2 + (dumbb*dumgamma)*dum3)

	if ((dumfracnum .le. 0.0) .or. (dumfracvol .le. 0.0)) then
	    iforce_movecenter(n) = 31
	    nnewsave(1,n) = nnew2
	else if ((dumfracnum .ge. 1.0) .or. (dumfracvol .ge. 1.0)) then
	    iforce_movecenter(n) = 32
	end if
	if (iforce_movecenter(n) .gt. 0) goto 3700

	nnewsave(2,n) = nnew2

	xferfracvol(1,n) = dumfracvol
	xferfracvol(2,n) = 1.0 - dumfracvol
	xferfracnum(1,n) = dumfracnum
	xferfracnum(2,n) = 1.0 - dumfracnum

3700	continue

!   diagnostic output
	if (idiag_movesect .lt. 70) goto 3800

	if (nnewsave(2,n) .eq. 0) then
	    if (nnewsave(1,n) .eq. 0) then
		dumch4 = 'NO X'
	    else if (nnewsave(1,n) .eq. n) then
		dumch4 = 'NO A'
	    else
		dumch4 = 'YESA'
	    end if
	else if (nnewsave(1,n) .eq. 0) then
	    if (nnewsave(2,n) .eq. n) then
		dumch4 = 'NO B'
	    else
		dumch4 = 'YESB'
	    end if
	else if (nnewsave(2,n) .eq. n) then
	    if (nnewsave(1,n) .eq. n) then
		dumch4 = 'NO Y'
	    else
		dumch4 = 'YESC'
	    end if
	else if (nnewsave(1,n) .eq. n) then
	    dumch4 = 'YESD'
	else
	    dumch4 = 'YESE'
	end if

	dumch1 = '+'
	if (drymasspp(n) .gt. drymassxx(n)) dumch1 = '-'
		
	msg = ' '
	call peg_message( lunout, msg )
	write(msg,97010) dumch1, dumch4, iclm, jclm, k, m,   &
      		n, nnewsave(1,n), nnewsave(2,n), iforce_movecenter(n)
	call peg_message( lunout, msg )
	write(msg,97020) 'pre mass, dens      ',   &
      		drymasspp(n), drydenspp(n)
	call peg_message( lunout, msg )
	write(msg,97020) 'aft mass, dens, numb',   &
      		drymassxx(n), drydensxx(n), rnumbxx(n)
	call peg_message( lunout, msg )
	if ((drydensxx(n) .ne. drydensxx0(n)) .or.   &
      	    (drymassxx(n) .ne. drymassxx0(n)) .or.   &
      	    (rnumbxx(n)   .ne. rnumbxx0(n)  )) then
      	    write(msg,97020) 'aft0 mas, dens, numb',   &
      		drymassxx0(n), drydensxx0(n), rnumbxx0(n)
	    call peg_message( lunout, msg )
	end if
	write(msg,97020) 'vlop0, vbarp,  vhip0',   &
      		volumlo_stmp(n,itype), dumvbar_pre, volumhi_stmp(n,itype)
	call peg_message( lunout, msg )
	write(msg,97020) 'vlop , vbarp,  vhip ',   &
      		dumvlo_pre, dumvbar_pre, dumvhi_pre
	call peg_message( lunout, msg )
	write(msg,97020) 'vloax, vbarax, vhiax',   &
      		dumvcutlo_nnew_pre, dumvbar_pre, dumvcuthi_nnew_pre
	call peg_message( lunout, msg )
	write(msg,97020) 'vloa0, vbara,  vhia0',   &
      		volumlo_stmp(nnew,itype), dumvbar_aft, volumhi_stmp(nnew,itype)
	call peg_message( lunout, msg )
	write(msg,97020) 'dumfrvol, num, ratio',   &
      		dumfracvol, dumfracnum, dumratio
	call peg_message( lunout, msg )
	write(msg,97020) 'frvol,num1; vol,num2',   &
      		xferfracvol(1,n), xferfracnum(1,n),   &
      		xferfracvol(2,n), xferfracnum(2,n)
	call peg_message( lunout, msg )

97010	format( 'movesect', 2a, 7x, 4i3, 4x,   &
      		'n,nnews', 3i3, 4x, 'iforce', i3.2 )
97020	format( a, 1p, 4e13.4 )

3800	continue

!
!   check for legal combinations of nnewsave(1,n) & nnewsave(2,n)
!   error if
!     nnew1 == nnew2
!     both are non-zero AND iabs(nnew1-nnew2) != 1
	ierr = 0
	if (nnewsave(1,n) .eq. nnewsave(2,n)) then
	    ierr = 1
	else if (nnewsave(1,n)*nnewsave(2,n) .ne. 0) then
	    if (iabs(nnewsave(1,n)-nnewsave(2,n)) .ne. 1) ierr = 1
	end if
	if (ierr .gt. 0) then
	    write(msg,97010) 'E', 'RROR', iclm, jclm, k, m,   &
      		n, nnewsave(1,n), nnewsave(2,n), iforce_movecenter(n)
	    call peg_message( lunout, msg )
	end if


!   if method_movesect == 30-31 then force moving center
!   this is just for testing purposes
	if ((method_movesect .ge. 30) .and. (method_movesect .le. 39)) then
	    nnewsave(1,n) = nnew
	    nnewsave(2,n) = 0
	    xferfracvol(1,n) = 1.0
	    xferfracvol(2,n) = 0.0
	    xferfracnum(1,n) = 1.0
	    xferfracnum(2,n) = 0.0
	end if

3900	continue

	return
	end subroutine move_sections_calc_masnumadvect                          


!-----------------------------------------------------------------------
	subroutine move_sections_apply_moves(   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum,   &
	  dcen_stmp, volumcen_stmp, volumlo_stmp, volumhi_stmp,   &
      	  adrydens_tmp, awetdens_tmp, adrydpav_tmp, awetdpav_tmp,   &
      	  adryqmas_tmp )
!
!   routine performs the actual transfer of aerosol number and mass
!	between sections
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize, maxd_atype,   &
	    nsize_aer, ncomp_plustracer_aer, ai_phase,   &
	    massptr_aer, numptr_aer, waterptr_aer, hyswptr_aer

	implicit none

!   subr arguments
	integer iphase_flag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)

	real(r8) densdefault, densh2o, smallmassaa, smallmassbb
	real(r8) fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_apvolu
	real(r8) delta_water_conform1, delta_numb_conform1
	real(r8) drydenspp(maxd_asize)
	real(r8) drymasspp(maxd_asize)
	real(r8) drymassxx(maxd_asize), drymassyy(maxd_asize)
	real(r8) dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real(r8) rbox(ntot_used)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real(r8) rnumbpp(maxd_asize)
	real(r8) rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real(r8) xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real(r8) dcen_stmp(maxd_asize,maxd_atype), volumcen_stmp(maxd_asize,maxd_atype), &
	    volumlo_stmp(maxd_asize,maxd_atype), volumhi_stmp(maxd_asize,maxd_atype)
	real(r8) wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real(r8) wetmassxx(maxd_asize), wetmassyy(maxd_asize)
	real(r8) adrydens_tmp(maxd_asize,maxd_atype),  awetdens_tmp(maxd_asize,maxd_atype)
	real(r8) adrydpav_tmp(maxd_asize,maxd_atype), awetdpav_tmp(maxd_asize,maxd_atype)
	real(r8) adryqmas_tmp(maxd_asize,maxd_atype)

!   local variables
	integer jj, l, ll, n, ndum, nnew, nold
	integer jja, jjb, jjc

	real(r8) delta_numb_conform2,   &
	  dumbot, dumnum, dumnum_at_dhi, dumnum_at_dlo,   &
      	  dumvol, dumvol1p, dumxfvol, dumxfnum, sixoverpi, third
	real(r8) dumpp(maxd_asize), dumxx(maxd_asize), dumyy(maxd_asize),   &
	  dumout(maxd_asize)

	character*160 msg
	character*8 dumch8
	character*4 dumch4


	sixoverpi = 6.0/pi
	third = 1.0/3.0

!
!   initialize "yy" arrays that hold values after inter-mode transferring
!	"yy" = "xx" for sections that do not move at all
!	"yy" = 0.0  for sections that do move (partially or completely)
!
	do 1900 n = 1, nsize_aer(itype)

	if ( (nnewsave(1,n) .eq. n) .and.   &
      	     (nnewsave(2,n) .eq. 0) ) then
!   if nnew == n, then material in section n will not be transferred, and
!	section n will contain its initial material plus any material
!	transferred from other sections
!   so initialize "yy" arrays with "xx" values
	    drymassyy(n) = drymassxx(n)
	    dryvolyy(n) = dryvolxx(n)
	    wetmassyy(n) = wetmassxx(n)
	    wetvolyy(n) = wetvolxx(n)
	    rnumbyy(n) = rnumbxx(n)
	    do ll = 1, ncomp_plustracer_aer(itype) + 2
		rmassyy(ll,n) = rmassxx(ll,n)
	    end do

	else
!   if nnew .ne. n, then material in section n will be transferred, and
!	section n will only contain material that is transferred from
!	other sections
!   so initialize "yy" arrays to zero
	    drymassyy(n) = 0.0
	    dryvolyy(n) = 0.0
	    wetmassyy(n) = 0.0
	    wetvolyy(n) = 0.0
	    rnumbyy(n) = 0.0
	    do ll = 1, ncomp_plustracer_aer(itype) + 2
		rmassyy(ll,n) = 0.0
	    end do

	end if

1900	continue

!
!   do the transfer of mass and number
!
	do 2900 nold = 1, nsize_aer(itype)

	if ( (nnewsave(1,nold) .eq. nold) .and.   &
      	     (nnewsave(2,nold) .eq. 0   ) ) goto 2900

	do 2800 jj = 1, 2

	nnew = nnewsave(jj,nold)
	if (nnew .le. 0) goto 2800

	dumxfvol = xferfracvol(jj,nold)
	dumxfnum = xferfracnum(jj,nold)

	do ll = 1, ncomp_plustracer_aer(itype) + 2
	    rmassyy(ll,nnew) = rmassyy(ll,nnew) + rmassxx(ll,nold)*dumxfvol
	end do
	rnumbyy(nnew) = rnumbyy(nnew) + rnumbxx(nold)*dumxfnum

	drymassyy(nnew) = drymassyy(nnew) + drymassxx(nold)*dumxfvol
	dryvolyy(nnew)  = dryvolyy(nnew)  + dryvolxx(nold)*dumxfvol
	wetmassyy(nnew) = wetmassyy(nnew) + wetmassxx(nold)*dumxfvol
	wetvolyy(nnew)  = wetvolyy(nnew)  + wetvolxx(nold)*dumxfvol

2800	continue

2900	continue

!
!   transfer among sections is completed
!   - check for conservation of mass/volume/number
!   - conform number again
!   - compute/store densities and mean sizes
!   - if k=1, save values for use by dry deposition routine
!   - copy new mixrats back to rbox array
!
	call move_sections_conserve_check(   &
	  1, iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
	  fact_apmassmr, fact_apnumbmr,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy )

	delta_numb_conform2 = 0.0

	do 3900 n = 1, nsize_aer(itype)

	dumvol = dryvolyy(n)
	if (min(drymassyy(n),dumvol) .le. smallmassbb) then
	    dumvol = drymassyy(n)/densdefault
	    dumnum = drymassyy(n)/(volumcen_stmp(n,itype)*densdefault)
	    delta_numb_conform2 = delta_numb_conform2 + dumnum - rnumbyy(n)
	    rnumbyy(n) = dumnum
	    adrydens_tmp(n,itype) = densdefault/fact_apdens
	    awetdens_tmp(n,itype) = densdefault/fact_apdens
	    adrydpav_tmp(n,itype) = dcen_stmp(n,itype)/fact_apdiam
	    awetdpav_tmp(n,itype) = dcen_stmp(n,itype)/fact_apdiam
	else
	    dumnum = rnumbyy(n)
	    dumnum_at_dhi = dumvol/volumhi_stmp(n,itype)
	    dumnum_at_dlo = dumvol/volumlo_stmp(n,itype)
	    dumnum = max( dumnum_at_dhi, min( dumnum_at_dlo, dumnum ) )
	    delta_numb_conform2 = delta_numb_conform2 + dumnum - rnumbyy(n)
	    rnumbyy(n) = dumnum
	    adrydens_tmp(n,itype) = drymassyy(n)/dumvol/fact_apdens
	    dumvol1p = dumvol/dumnum
	    adrydpav_tmp(n,itype) = ((dumvol1p*sixoverpi)**third) / fact_apdiam
	    awetdens_tmp(n,itype) = wetmassyy(n)/wetvolyy(n)/fact_apdens
	    dumvol1p = wetvolyy(n)/dumnum
	    awetdpav_tmp(n,itype) = min( 100.0_r8*dcen_stmp(n,itype),   &
      			(dumvol1p*sixoverpi)**third ) / fact_apdiam
	end if
	adryqmas_tmp(n,itype) = drymassyy(n)/fact_apmassmr

!	if (k .eq. 1) then
!	    awetdens_sfc(n,itype,iclm,jclm) = awetdens_tmp(n,itype,k,m)
!	    awetdpav_sfc(n,itype,iclm,jclm) = awetdpav_tmp(n,itype,k,m)
!	end if

	do ll = 1, ncomp_plustracer_aer(itype)
	    l = massptr_aer(ll,n,itype,iphase)
	    rbox(l) = rmassyy(ll,n)/fact_apmassmr
	end do
	l = 0
	if (iphase .eq. ai_phase) then
	    l = waterptr_aer(n,itype)
	    if (l .gt. 0) rbox(l) = rmassyy(llwater,n)/fact_apmassmr
	    l = hyswptr_aer(n,itype)
	    if (l .gt. 0) rbox(l) = rmassyy(llhysw,n)/fact_apmassmr
	end if
	rbox(numptr_aer(n,itype,iphase)) = rnumbyy(n)/fact_apnumbmr

3900	continue

	delta_numb_conform1 = delta_numb_conform1 + delta_numb_conform2

	call move_sections_conserve_check(   &
	  2, iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
	  fact_apmassmr, fact_apnumbmr,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy )


!   diagnostic output
	if (idiag_movesect .lt. 70) goto 4900

	ndum = 0
	do n = 1, nsize_aer(itype)
	    if (nnewsave(1,n)+nnewsave(2,n) .ne. n) ndum = ndum + 1
	end do
!	if (ndum .gt. 0) then
!	    write(msg,97010) 'SOME', iclm, jclm, k, m,   &
!      		ndum, (nnewsave(1,n), nnewsave(2,n), n=1,nsize_aer(itype))
!	    call peg_message( lunout, msg )
!	else
!	    write(msg,97010) 'NONE', iclm, jclm, k, m,   &
!      		ndum, (nnewsave(1,n), nnewsave(2,n), n=1,nsize_aer(itype))
!	    call peg_message( lunout, msg )
!	end if

	dumch4 = 'NONE'
	if (ndum .gt. 0) dumch4 = 'SOME'
	msg = ' '
	call peg_message( lunout, msg )
	write(msg,97010) dumch4, iclm, jclm, k, m, ndum
	call peg_message( lunout, msg )
	do jjb = 1, nsize_aer(itype), 10
	    jjc = min( jjb+9, nsize_aer(itype) )
	    write(msg,97011) (nnewsave(1,n), nnewsave(2,n), n=jjb,jjc)
	    call peg_message( lunout, msg )
	end do

!	write(msg,97020) 'rnumbold', (rnumbxx(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )
!	write(msg,97020) 'rnumbnew', (rnumbyy(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )

!	write(msg,97020) 'drvolold', (dryvolxx(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )
!	write(msg,97020) 'drvolnew', (dryvolyy(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )

	dumbot = log( volumhi_stmp(1,itype)/volumlo_stmp(1,itype) )
	do n = 1, nsize_aer(itype)
	    dumpp(n) = -9.99
	    dumxx(n) = -9.99
	    dumyy(n) = -9.99
	    if ( (drydenspp(n) .gt. 0.5) .and.   &
      	         (drymasspp(n) .gt. smallmassaa) ) then
      		dumvol = drymasspp(n)/drydenspp(n)
		if ((rnumbpp(n) .ge. 1.0e-35) .and.   &
      		    (dumvol .ge. 1.0e-35)) then
		    dumvol1p = dumvol/rnumbpp(n)
		    dumpp(n) = 1.0 + log(dumvol1p/volumlo_stmp(1,itype))/dumbot
		end if
	    end if
	    if ((rnumbxx(n) .ge. 1.0e-35) .and.   &
      		(dryvolxx(n) .ge. 1.0e-35)) then
		dumvol1p = dryvolxx(n)/rnumbxx(n)
		dumxx(n) = 1.0 + log(dumvol1p/volumlo_stmp(1,itype))/dumbot
	    end if
	    if ((rnumbyy(n) .ge. 1.0e-35) .and.   &
      		(dryvolyy(n) .ge. 1.0e-35)) then
		dumvol1p = dryvolyy(n)/rnumbyy(n)
		dumyy(n) = 1.0 + log(dumvol1p/volumlo_stmp(1,itype))/dumbot
	    end if
	end do

!	write(msg,97030) 'lnvolold', (dumxx(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )
!	write(msg,97030) 'lnvolnew', (dumyy(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )

	do jja = 1, 7
!	do jja = 1, 13
	    if      (jja .eq. 1) then
		dumch8 = 'rnumbold'
		dumout(:) = rnumbxx(:)
	    else if (jja .eq. 2) then
		dumch8 = 'rnumbnew'
		dumout(:) = rnumbyy(:)
	    else if (jja .eq. 3) then
		dumch8 = 'drvolold'
		dumout(:) = dryvolxx(:)
	    else if (jja .eq. 4) then
		dumch8 = 'drvolnew'
		dumout(:) = dryvolyy(:)
	    else if (jja .eq. 5) then
		dumch8 = 'lnvolold'
		dumout(:) = dumxx(:)
	    else if (jja .eq. 6) then
		dumch8 = 'lnvolnew'
		dumout(:) = dumyy(:)
	    else if (jja .eq. 7) then
		dumch8 = 'lnvolpre'
		dumout(:) = dumpp(:)
	    else if (jja .eq. 8) then
		dumch8 = 'wtvolold'
		dumout(:) = wetvolxx(:)
	    else if (jja .eq. 9) then
		dumch8 = 'wtvolnew'
		dumout(:) = wetvolyy(:)
	    else if (jja .eq. 10) then
		dumch8 = 'wtmasold'
		dumout(:) = wetmassxx(:)
	    else if (jja .eq. 11) then
		dumch8 = 'wtmasnew'
		dumout(:) = wetmassyy(:)
	    else if (jja .eq. 12) then
		dumch8 = 'drmasold'
		dumout(:) = drymassxx(:)
	    else if (jja .eq. 13) then
		dumch8 = 'drmasnew'
		dumout(:) = drymassyy(:)
	    end if
	    do jjb = 1, nsize_aer(itype), 10
		jjc = min( jjb+9, nsize_aer(itype) )
		if ((jja .le. 4) .or. (jja .ge. 8)) then
		    if (jjc == nsize_aer(itype)) then
			write(msg,97020) dumch8, (dumout(n), n=jjb,jjc), &
			    sum( dumout(1:jjc) )
		    else
			write(msg,97020) dumch8, (dumout(n), n=jjb,jjc)
		    end if
		else
		    write(msg,97030) dumch8, (dumout(n), n=jjb,jjc)
		end if
		call peg_message( lunout, msg )
		dumch8 = ' '
	    end do
	end do

!97010	format( / 'movesectapply', a, 4i3, 3x, i3 / 5x, 10(3x,2i3) )
!97020	format( a, 1p, 10e9.1 / (( 8x, 1p, 10e9.1 )) )
!97030	format( a,     10f9.3 / (( 8x,     10f9.3 )) )
97010	format( 'movesectapply', a, 4i3, 3x, i3 )
!97011	format( 5x, 10(3x,2i3) )
!97020	format( a, 1p, 10e9.1 )
!97030	format( a,     10f9.3 )
97011	format( 5x, 10(5x,2i3) )
97020	format( a, 1p, 11e11.3 )
97030	format( a,     10f11.5 )

4900	continue
	return
	end subroutine move_sections_apply_moves                          


!-----------------------------------------------------------------------
	subroutine move_sections_conserve_check( ipass,   &
	  iphase_flag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
	  fact_apmassmr, fact_apnumbmr,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  rbox,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy )
!
!   routine checks for conservation of number, mass, and volume
!	by the move_sections algorithm
!
!   ipass = 1
!	initialize all thesum(jj,ll) to zero
!	computes thesum(1,ll) from rmassxx, rnumbxx, ...
!	computes thesum(2,ll) from rmassyy, rnumbyy, ...
!	compares thesum(1,ll) with thesum(2,ll)
!	computes thesum(3,ll) from rbox before section movement
!   ipass = 2
!	computes thesum(4,ll) from rbox after  section movement
!	compares thesum(3,ll) with thesum(4,ll)
!
!   currently only implemented for condensational growth (iphase_flag=1)
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_acomp, maxd_asize,   &
	    nsize_aer, ncomp_plustracer_aer, ai_phase,   &
	    massptr_aer, numptr_aer, waterptr_aer, hyswptr_aer,   &
	    name_aer

	implicit none

!   subr arguments
	integer ipass, iphase_flag, iclm, jclm, iphase, itype, k,   &
	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)
	real(r8) densdefault, densh2o, smallmassaa, smallmassbb
	real(r8) fact_apmassmr, fact_apnumbmr
	real(r8) delta_water_conform1, delta_numb_conform1
	real(r8) drymassxx(maxd_asize), drymassyy(maxd_asize)
	real(r8) dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real(r8) rbox(ntot_used)
	real(r8) rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real(r8) rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real(r8) wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real(r8) wetmassxx(maxd_asize), wetmassyy(maxd_asize)

!   local variables
	integer jj, l, ll, llworst, llworstb, n
	integer nerr, nerrmax
	save nerr, nerrmax
	data nerr, nerrmax / 0, 999 /

	real(r8) dumbot, dumtop, dumtoler, dumerr, dumworst, dumworstb
	real(r8) duma, dumb, dumc, dume
	real(r8) dumerrsv(maxd_acomp+7)
	real(r8) thesum(4,maxd_acomp+7)
	save thesum

	character*8 dumname(maxd_acomp+7)
	character*160 msg


	if (ipass .eq. 2) goto 2000

	do ll = 1, ncomp_plustracer_aer(itype)+7
	do jj = 1, 4
	    thesum(jj,ll) = 0.0
	end do
	end do

	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)+2
		thesum(1,ll) = thesum(1,ll) + rmassxx(ll,n)
		thesum(2,ll) = thesum(2,ll) + rmassyy(ll,n)
	    end do
	    ll = ncomp_plustracer_aer(itype)+3
	    thesum(1,ll) = thesum(1,ll) + rnumbxx(n)
	    thesum(2,ll) = thesum(2,ll) + rnumbyy(n)
	    ll = ncomp_plustracer_aer(itype)+4
	    thesum(1,ll) = thesum(1,ll) + drymassxx(n)
	    thesum(2,ll) = thesum(2,ll) + drymassyy(n)
	    ll = ncomp_plustracer_aer(itype)+5
	    thesum(1,ll) = thesum(1,ll) + dryvolxx(n)
	    thesum(2,ll) = thesum(2,ll) + dryvolyy(n)
	    ll = ncomp_plustracer_aer(itype)+6
	    thesum(1,ll) = thesum(1,ll) + wetmassxx(n)
	    thesum(2,ll) = thesum(2,ll) + wetmassyy(n)
	    ll = ncomp_plustracer_aer(itype)+7
	    thesum(1,ll) = thesum(1,ll) + wetvolxx(n)
	    thesum(2,ll) = thesum(2,ll) + wetvolyy(n)
	end do


2000	continue
!
!   calc sum over bins for each species
!   for water, account for loss in initial conform (delta_water_conform1)
!
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)+3
		duma = fact_apmassmr
		if (ll .le. ncomp_plustracer_aer(itype)) then
		    l = massptr_aer(ll,n,itype,iphase)
		else if (ll .eq. ncomp_plustracer_aer(itype)+1) then
		    l = 0
		    if (iphase .eq. ai_phase) l = hyswptr_aer(n,itype)
		else if (ll .eq. ncomp_plustracer_aer(itype)+2) then
		    l = 0
		    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
		else
		    l = numptr_aer(n,itype,iphase)
		    duma = fact_apnumbmr
		end if
		if (l .gt. 0)   &
		    thesum(ipass+2,ll) = thesum(ipass+2,ll) + rbox(l)*duma
	    end do
	end do
	if (ipass .eq. 2) then
	    ll = ncomp_plustracer_aer(itype)+2
	    thesum(3,ll) = thesum(3,ll) + delta_water_conform1
	    ll = ncomp_plustracer_aer(itype)+3
	    thesum(3,ll) = thesum(3,ll) + delta_numb_conform1
	end if

!
!   now compare either sum1-sum2 or sum3-sum4
!	on ipass=1, jj=1, so compare sum1 & sum2
!	on ipass=2, jj=3, so compare sum3 & sum4
!
	do ll = 1, ncomp_plustracer_aer(itype)+7
	    dumname(ll) = ' '
	    write(dumname(ll),'(i4.4)') ll
!	    if (ll .le. ncomp_plustracer_aer(itype)) dumname(ll) =   &
!	    		species(massptr_aer(ll,1,itype,iphase))(1:4)
	    if (ll .le. ncomp_plustracer_aer(itype))   dumname(ll) = name_aer(ll,1)(1:4)
	    if (ll .eq. ncomp_plustracer_aer(itype)+1) dumname(ll) = 'hysw'
	    if (ll .eq. ncomp_plustracer_aer(itype)+2) dumname(ll) = 'watr'
	    if (ll .eq. ncomp_plustracer_aer(itype)+3) dumname(ll) = 'numb'
	    if (ll .eq. ncomp_plustracer_aer(itype)+4) dumname(ll) = 'drymass'
	    if (ll .eq. ncomp_plustracer_aer(itype)+5) dumname(ll) = 'dryvol'
	    if (ll .eq. ncomp_plustracer_aer(itype)+6) dumname(ll) = 'wetmass'
	    if (ll .eq. ncomp_plustracer_aer(itype)+7) dumname(ll) = 'wetvol'
	end do

	jj = 2*ipass - 1
	dumworst = 0.0
	dumworstb = 0.0
	llworst = 0
	llworstb = 0
	dumerrsv(:) = 0.0
	do ll = 1, ncomp_plustracer_aer(itype)+7
	    dumtop = thesum(jj+1,ll) - thesum(jj,ll)
	    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-35_r8 )
	    dumerr = dumtop/dumbot

! rce 21-jul-2006 - encountered some cases when delta_*_conform1 is negative 
!   and large in magnitude relative to thesum, which causes the mass
!   conservation to be less accurate due to roundoff
! following section recomputes relative error with delta_*_conform1
!   added onto each of thesum
! also increased dumtoler slightly
	    if (ipass .eq. 2) then
		dumc = 1.0
		if (ll .eq. ncomp_plustracer_aer(itype)+2) then
		    dumc = delta_water_conform1
		else if (ll .eq. ncomp_plustracer_aer(itype)+3) then
		    dumc = delta_numb_conform1
		end if
		if (dumc .lt. 0.0) then
		    duma = thesum(3,ll) - dumc
		    dumb = thesum(4,ll) - dumc
		    dumtop = dumb - duma
		    dumbot = max( abs(duma), abs(dumb), 1.0e-35_r8 )
		    dume = dumtop/dumbot
		    if (abs(dume) .lt. abs(dumerr)) dumerr = dume
		end if
	    end if

	    dumerrsv(ll) = dumerr
	    if (abs(dumerr) .gt. abs(dumworst)) then
		llworstb = llworst
		dumworstb = dumworst
		llworst = ll
		dumworst = dumerr
	    end if
	end do

	dumtoler = 1.0e-6
	if (abs(dumworst) .gt. dumtoler) then
	    nerr = nerr + 1
	    if (nerr .le. nerrmax) then
		msg = ' '
		call peg_message( lunout, msg )
		write(msg,97110) iclm, jclm, k, m, ipass, llworst
		call peg_message( lunout, msg )
		write(msg,97120) '    nnew(1,n)',   &
      			(nnewsave(1,n), n=1,nsize_aer(itype))
		call peg_message( lunout, msg )
		write(msg,97120) '    nnew(2,n)',   &
      			(nnewsave(2,n), n=1,nsize_aer(itype))
		call peg_message( lunout, msg )

		ll = llworst
		if (ll .eq. 0) ll = ncomp_plustracer_aer(itype)+2
		write(msg,97130) 'name/relerrA/thesum', jj, '/thesum', jj+1,   &
      			dumname(ll), dumworst, thesum(jj,ll), thesum(jj+1,ll)
		call peg_message( lunout, msg )

		if ( (ll .eq. ncomp_plustracer_aer(itype)+3) .and.   &
		     (abs(dumworstb) .gt. dumtoler) ) then
		    ll = max( 1, llworstb )
		    dumtop = thesum(jj+1,ll) - thesum(jj,ll)
		    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-35_r8 )
		    dumerr = dumtop/dumbot
		    write(msg,97130) 'name/relerrB/thesum', jj, '/thesum', jj+1,   &
      			dumname(ll), dumerr, thesum(jj,ll), thesum(jj+1,ll)
		    call peg_message( lunout, msg )
		end if
	    end if
	end if

!   following added on 10-mar-2010
	do ll = 1, ncomp_plustracer_aer(itype)+7
	    exit   ! this deactivates it
	    if (abs(dumerrsv(ll)) .le. dumtoler) cycle
	    nerr = nerr + 1
	    if (nerr .le. nerrmax) then
		write(msg,97130) 'name/relerrC/thesum', jj, '/thesum', jj+1,   &
      			dumname(ll), dumworst, thesum(jj,ll), thesum(jj+1,ll)
		call peg_message( lunout, msg )
	    end if
	end do ! ll

97110	format( 'movesect conserve ERROR - i/j/k/m/pass/llworst',   &
		4i3, 2x, 2i3 )
97120	format( a, 64i3 )
97130	format( a, i1, a, i1, 2x, a, 1p, 3e16.7 )

	return
	end subroutine move_sections_conserve_check        


!-----------------------------------------------------------------------
	subroutine move_sections_checkptrs( iphase_flag, iclm, jclm, k, m )
!
!   checks for valid number and water pointers
!
	use module_data_mosaic_asecthp, only:  &
	    ntype_aer, nsize_aer, nphase_aer, ai_phase,   &
	    numptr_aer, waterptr_aer

	implicit none

!   subr parameters
	integer iphase_flag, iclm, jclm, k, m

!   local variables
	integer l, itype, iphase, n, ndum
	character*160 msg

	do 1900 itype = 1, ntype_aer
	do 1800 iphase = 1, nphase_aer

	ndum = 0
	do n = 1, nsize_aer(itype)
	    l = numptr_aer(n,itype,iphase)
	    if ((l .lt. 1) .or. (l .gt. ntot_used)) then
		msg = '*** subr move_sections error - ' //   &
			'numptr_amode not defined'
		call peg_message( lunerr, msg )
		write(msg,9030) 'mode, numptr =', n, l
		call peg_message( lunerr, msg )
		write(msg,9030) 'iphase, itype =', iphase, itype
		call peg_message( lunerr, msg )
		call peg_error_fatal( lunerr, msg )
	    end if
!	checks involving nspec_amode and nspec_amode_nontracer
!	being the same for all sections are no longer needed
	    l = 0
	    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
	    if ((l .ge. 1) .and. (l .le. ntot_used)) ndum = ndum + 1
	end do
	if ((ndum .ne. 0) .and. (ndum .ne. nsize_aer(itype))) then
	    msg = '*** subr move_sections error - ' //   &
      		'waterptr_aer must be on/off for all modes'
	    call peg_message( lunerr, msg )
	    write(msg,9030) 'iphase, itype =', iphase, itype
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if
9030	format( a, 2(1x,i6) )

1800	continue
1900	continue

	return
	end subroutine move_sections_checkptrs                           


!-----------------------------------------------------------------------
	subroutine test_move_sections( iphase_flag_inp, iclm, jclm, k, m,it_mosaic, &
	                               mmovesect_flag1, idiag_movesect )
!
!   routine runs tests on move_sections, using a matrix of
!	pregrow and aftgrow masses for each section
!
	use module_data_mosaic_asecthp, only:  &
	    maxd_atype, maxd_asize,   &
	    ntype_aer, nsize_aer, ncomp_plustracer_aer,   &
	    ai_phase, cw_phase,   &
	    massptr_aer, numptr_aer, waterptr_aer,   &
	    dens_aer, volumlo_sect

	implicit none

!   subr arguments
	integer, intent(in) :: iphase_flag_inp, iclm, jclm, k, m, it_mosaic, &
	                       mmovesect_flag1, idiag_movesect

!   local variables
	integer iphase_flag, ii, iphase, itype, jj,   &
	  l, ll, n, nn
	integer, save :: ientryno = 0

	real(r8) dumnumb, dumvolpre, dumvolaft
	real(r8) adrydens_tmp(maxd_asize,maxd_atype),  awetdens_tmp(maxd_asize,maxd_atype)
	real(r8) adrydpav_tmp(maxd_asize,maxd_atype), awetdpav_tmp(maxd_asize,maxd_atype)
	real(r8) adryqmas_tmp(maxd_asize,maxd_atype)
	real(r8) fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam
	real(r8) tmp_drymass_pregrow(maxd_asize,maxd_atype)
	real(r8) tmp_drymass_aftgrow(maxd_asize,maxd_atype)
	real(r8) tmp_drydens_pregrow(maxd_asize,maxd_atype)
	real(r8) tmp_drydens_aftgrow(maxd_asize,maxd_atype)
	real(r8) rbox(ntot_used)

	character*160 msg

	integer maxvolfactpre, maxvolfactaft
	parameter (maxvolfactpre=15, maxvolfactaft=23)

	real(r8) dumvolfactpre(maxvolfactpre)
	data dumvolfactpre /   &
	2.0, 0.0, 1.0e-20, 0.5, 0.9,   &
	1.0, 1.01, 1.1, 2.0, 4.0, 7.9, 7.99, 8.0,   &
	8.1, 16.0 /

	real(r8) dumvolfactaft(maxvolfactaft)
	data dumvolfactaft /   &
	4.0, 0.0, 1.0e-20, 0.01, 0.02, 0.05, 0.1, 0.5, 0.9,   &
	1.0, 1.01, 1.1, 2.0, 4.0, 7.9, 7.99, 8.0,   &
	8.1, 16.0, 32., 64., 128., 256. /
!	7.9, 0.0, 1.0e-20, 0.01, 0.02, 0.05, 0.1, 0.5, 0.9,   &
!	1.0, 1.01, 1.1, 2.0, 4.0, 7.9, 7.99, 8.0,   &
!	8.1, 16.0, 32., 64., 128., 256. /


!
!   check for valid inputs
!   and first entry
!
	if (mmovesect_flag1 .le. 0) return
	if (nsize_aer(1) .le. 0) return

	if (idiag_movesect .ne. 80) return
	
	ientryno = ientryno + 1
	if (ientryno .gt. 1) return

	write(*,*) '*** executing test_move_sections ***'


!
!   make test calls to move_sections
!
	do 3900 iphase_flag = 1, 1

	iphase = ai_phase
	if (iabs(iphase_flag) .eq. 2) iphase = cw_phase

	do 3800 itype = 1, ntype_aer

	do 2900 nn = 1, nsize_aer(itype)

	do 2800 ii = 1, maxvolfactpre

	do 2700 jj = 1, maxvolfactaft

!----------------------------------------------------------
!   following lines produce a small subset of the ii/jj/nn tests
!
!   following limits ii/jj to 1 and "matching dumvolfact"
!	if (dumvolfactpre(ii) /= dumvolfactpre(1)) goto 2700
!	if (dumvolfactaft(jj) /= dumvolfactaft(1)) goto 2700
!   following limits dumvolfactpre to 4.0
!	if (abs(dumvolfactpre(ii) - 4.0) > 1.0e-4) goto 2800
!   following limits nn
!	if (nn /= 4) goto 2900
!----------------------------------------------------------

!   zero out rbox and dryxxx_yyygrow arrays
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)
		rbox(massptr_aer(ll,n,itype,iphase)) = 0.0
	    end do
	    l = 0
	    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
	    if (l .gt. 0) rbox(l) = 0.0
	    l = numptr_aer(n,itype,iphase)
	    if (l .gt. 0) rbox(l) = 0.0
	    tmp_drymass_pregrow(n,itype) = 0.0
	    tmp_drymass_aftgrow(n,itype) = 0.0
	    tmp_drydens_pregrow(n,itype) = -1.0
	    tmp_drydens_aftgrow(n,itype) = -1.0
	end do

!   fill in values for section nn
	n = nn
	dumnumb = 1.0e7/mw_air   ! 1.0e7 #/mol-air = 3.45e5 #/g-air = 345 #/mg-air
	rbox(numptr_aer(n,itype,iphase)) = dumnumb
	ll = 1
	l = massptr_aer(ll,n,itype,iphase)

	dumvolpre = volumlo_sect(n,itype)*dumvolfactpre(ii)*dumnumb
	tmp_drydens_pregrow(n,itype) = dens_aer(ll,itype)
	tmp_drymass_pregrow(n,itype) = dumvolpre*tmp_drydens_pregrow(n,itype)
	if (ii .eq. 1) tmp_drydens_pregrow(n,itype) = -1.0
	
	dumvolaft = volumlo_sect(n,itype)*dumvolfactaft(jj)*dumnumb
	tmp_drydens_aftgrow(n,itype) = dens_aer(ll,itype)
	tmp_drymass_aftgrow(n,itype) = dumvolaft*tmp_drydens_aftgrow(n,itype)
	if (jj .eq. 1) tmp_drydens_aftgrow(n,itype) = -1.0
	
	rbox(l) = tmp_drymass_aftgrow(n,itype)   ! g/g-air
	
	msg = ' '
	call peg_message( lunout, msg )
	write(msg,98010) nn, ii, jj
	call peg_message( lunout, msg )
	write(msg,98011) dumvolfactpre(ii), dumvolfactaft(jj)
	call peg_message( lunout, msg )

	fact_apmassmr = 1.0   ! units are g-AP/g-air already
	fact_apnumbmr = 1.0   ! units are    #/g-air already
	fact_apdens = 1.0     ! units are    g/cm3   already
	fact_apdiam = 1.0     ! units are   cm       already

!   make test call to move_sections
	call move_sections_x3( iphase_flag, iclm, jclm, k, m, rbox,   &
          fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam,   &
      	  tmp_drydens_aftgrow, tmp_drydens_pregrow,   &
      	  tmp_drymass_aftgrow, tmp_drymass_pregrow,   &
      	  adrydens_tmp, awetdens_tmp, adrydpav_tmp, awetdpav_tmp,   &
      	  adryqmas_tmp, it_mosaic, mmovesect_flag1, idiag_movesect )

	msg = ' '
	call peg_message( lunout, msg )
	write(msg,98010) nn, ii, jj
	call peg_message( lunout, msg )
	write(msg,98011) dumvolfactpre(ii), dumvolfactaft(jj)
	call peg_message( lunout, msg )

2700	continue
2800	continue
2900	continue

3800	continue
3900	continue

98010	format( 'test_move_sections output - nn, ii, jj =', 3i3 )
98011	format( 'volfactpre, volfactaft =', 1p, 2e12.4 )


	write(*,*) '*** leaving   test_move_sections at end ***'
	return
	end subroutine test_move_sections                           


!-----------------------------------------------------------------------


	end module module_mosaic_movesect1d

