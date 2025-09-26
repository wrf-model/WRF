!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
	module module_mosaic_movesect


	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_peg_util


!
!   if apply_n1_inflow = 1, then subr move_sections_apply_n1_inflow
!	applies an ad_hoc correction to bin 1 to account for growth of 
!	smaller particles (which are not simulated when aernucnew_onoff=0)
!       growing into bin 1
!   if apply_n1_inflow = other, this correction is not applied
!
	integer, parameter :: apply_n1_inflow = 0

	contains



!-----------------------------------------------------------------------
!
!   zz08movesect.f - created 24-nov-01 from scm movebin code
!   04-dec-01 rce - added code to treat bins that had state="no_aerosol"
!   10-dec-01 rce - diagnostic writes to fort.96 deleted
!   08-aug-02 rce - this is latest version from freshair scaqs-aerosol code
!   03-aug-02 rce - bypass this routine when msectional=701
!	output nnewsave values to lunout when msectional=702
!   29-aug-03 rce - use nspec_amode_nontracer in first "do ll" loop
!   12-nov-03 rce - two approaches now available
!	jacobson moving-center (old)        - when msectional=10
!	tzivion mass-number advection (new) - when msectional=20
!   28-jan-04 rce - eliminated the move_sections_old code 
!	(no reason to keep it)
!
! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect
! rce 2005-feb-17 - fixes involving drydens_pre/aftgrow, drymass_...,
!	& mw_aer.  All are dimensioned (isize,itype) but were being used
!	as (itype).  In old compiler, this was OK, and treated as (itype,1).
!	In new compiler, this caused an error.
!	Also, in subr test_move_sections, set iphase based on iflag.
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
	subroutine move_sections( iflag, iclm, jclm, k, m)
!
!   routine transfers aerosol number and mass between sections
!	to account for continuous aerosol growth
!   this routine is called after the gas condensation module (MOSAIC) or
!	aqueous chemistry module has increased the mass within sections
!
!   moving-center algorithm or mass-number advection algorithm is used,
!   depending on value of mod(msectional,100)
!	section mean diameter is given by
!	    vtot = ntot * (pi/6) * (dmean**3)
!	where vtot and ntot are total dry-volume and number for the section
!	if dmean is outside the section boundaries (dlo_sect & dhi_sect), then
!	    all the mass and number in the section are transfered to the
!	    section with dlo_sect(nnew) < dmean < dhi_sect(nnew)
!
!   mass mixing ratios (mole/mole-air or g/mole-air) are in
!	rsub(massptr_aer(ll,n,itype,iphase),k,m)
!   number mixing ratios (particles/mole-air) are in 
!	rsub(numptr_aer(n,itype,iphase),k,m)
!   these values are over-written with new values
!   the following are also updated:  
!	adrydens_sub(n,itype,k,m), admeandry_sub(n,itype,k,m),
!	awetdens_sub(n,itype,k,m), admeanwet_sub(n,itype,k,m)
!
!   particle sizes are in cm
!
!   input parameters
!	iflag = 1 - do transfer after trace-gas condensation/evaporation
!	      = 2 - do transfer after aqueous chemistry
!	      = -1/-2 - do some "first entry" tasks for the iflag=+1/+2 cases
!	iclm, jclm, k = current i,j,k indices
!	m = current subarea index
!	drymass_pregrow(n,itype) = dry-mass (g/mole-air) for section n
!			before the growth
!	drymass_aftgrow(n,itype) = dry-mass (g/mole-air) for section n
!			after the growth but before inter-section transfer
!	drydens_pregrow(n,itype) = dry-density (g/cm3) for section n
!			before the growth
!	drydens_aftgrow(n,itype) = dry-density (g/cm3) for section n
!			after the growth but before inter-section transfer
!	(drymass_pregrow and drydens_pregrow are not used by the moving-center
!	algorithm, but would be needed for other sectional algorithms)
!
!   this routine is the top level module for the post-october-2003 code
!	and will do either moving-center or mass-number advection
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, k, m

!   local variables
	integer idiag_movesect, iphase, itype,   &
	  l, ll, llhysw, llwater, lnew, lold, l3,   &
      	  method_movesect, n, nnew, nold
	integer nnewsave(2,maxd_asize)

	real densdefault, densh2o, smallmassaa, smallmassbb
	real delta_water_conform1, delta_numb_conform1

	real drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	     drydensxx(maxd_asize), drydensyy(maxd_asize)
	real drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	     drymassxx(maxd_asize), drymassyy(maxd_asize)
	real dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	     rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real specdensxx(maxd_acomp), specmwxx(maxd_acomp)
	real xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real wetmassxx(maxd_asize), wetmassyy(maxd_asize)

	character*160 msg


!
!   check for valid inputs
!
	if (msectional .le. 0) return
	if (ntype_aer .le. 0) return
	if (nphase_aer .le. 0) return

 
!
!   run diagnostic tests
!   (these will only be run for certain values of idiag_movesect
!   rce 2004-nov-29 - to avoid recursion, test_move_sections 
!	is now called from mosaic_driver
!
!	call test_move_sections( iflag, iclm, jclm, k, m )


!   get "method_movesect" from digits 1-2 of msectional (treat 1-9 as 10)
	method_movesect = mod( msectional, 100 )
	if (method_movesect .le. 10) method_movesect = 10

!   get "idiag_movesect"  from digits 3-4 of msectional
	idiag_movesect  = mod( msectional, 10000 )/100

	if      ((method_movesect .eq. 10) .or.   &
      		 (method_movesect .eq. 11) .or.   &
      		 (method_movesect .eq. 20) .or.   &
      		 (method_movesect .eq. 21) .or.   &
      		 (method_movesect .eq. 30) .or.   &
      		 (method_movesect .eq. 31)) then
	    continue
	else if ((method_movesect .eq. 19) .or.   &
      		 (method_movesect .eq. 29) .or.   &
      		 (method_movesect .eq. 39)) then
	    return
	else
	    msg = '*** subr move_sections error - ' //   &
		'illegal value for msectional'
	    call peg_error_fatal( lunerr, msg )
	end if

	if (iabs(iflag) .eq. 1) then
	    iphase = ai_phase
	else if (iabs(iflag) .eq. 2) then
	    iphase = cw_phase
	    if (nphase_aer .lt. 2) then
		msg = '*** subr move_sections error - ' //   &
		    'iflag=2 (after aqueous chemistry) but nphase_aer < 2'
		call peg_error_fatal( lunerr, msg )
	    else if (cw_phase .ne. 2) then
		msg = '*** subr move_sections error - ' //   &
		    'iflag=2 (after aqueous chemistry) but cw_phase .ne. 2'
		call peg_error_fatal( lunerr, msg )
	    end if
	else
	    msg = '*** subr move_sections error - ' //   &
		'iabs(iflag) must be 1 or 2'
	    call peg_error_fatal( lunerr, msg )
	end if


!   when iflag=-1/-2, call move_sections_checkptrs then return
!	if ((ncorecnt .le. 0) .and. (k .le. 1)) then
	if (iflag .le. 0) then
	    write(msg,9040) 'method', method_movesect
	    call peg_message( lunout, msg )
	    write(msg,9040) 'idiag ', idiag_movesect
	    call peg_message( lunout, msg )
	    call move_sections_checkptrs( iflag, iclm, jclm, k, m )
	    return
	end if
9040	format( '*** subr move_sections - ', a, ' =', i6 )

!   diagnostics
	if (idiag_movesect .eq. 70) then
	    msg = ' '
	    call peg_message( lunout, msg )
	    write(msg,9060) iclm, jclm, k, m, msectional
	    call peg_message( lunout, msg )
	end if
9060	format( '*** subr move_sections diagnostics i,j,k,m,msect =', 4i4, i6 )


	densdefault = 2.0
	densh2o = 1.0

!   if bin mass mixrat < smallmassaa (1.e-20 g/mol), then assume no growth
!   AND no water AND conform number so that size is within bin limits
	smallmassaa = 1.0e-20
!   if bin mass OR volume mixrat < smallmassbb (1.e-30 g/mol), then
!   assume default density to avoid divide by zero
	smallmassbb = 1.0e-30


!   process each type, one at a time
	do 1900 itype = 1, ntype_aer

	if (nsize_aer(itype) .le. 0) goto 1900

	call move_sections_initial_conform(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  specmwxx, specdensxx )

	if (method_movesect .le. 19) then
	call move_sections_calc_movingcenter(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum )
	else
	call move_sections_calc_masnumadvect(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum )
	end if

	call move_sections_apply_moves(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum )

! rce 08-nov-2004 - this call is new (and perhaps temporary)
! rce 05-jul-2005 - do n1_inflow for aerchem growth but not for cldchem
	if ((apply_n1_inflow .eq. 1) .and.   &
	    (iphase .eq. ai_phase)) then
	call move_sections_apply_n1_inflow(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum,   &
      	  specmwxx, specdensxx )
	end if

!	call move_sections_final_conform(   &
!	  iflag, iclm, jclm, k, m, iphase, itype )

1900	continue

	return
	end subroutine move_sections                          


!-----------------------------------------------------------------------
	subroutine move_sections_initial_conform(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  specmwxx, specdensxx )

!
!   routine does some initial tasks for the section movement
!	load rmassxx & rnumbxx from rsub
!	load specmwxx & specdensxx
!	set drymassxx & dryvolxx from drymass_aftgrow & drydens_aftgrow,
!	    OR compute them from rmassxx, specmwxx, specdensxx if need be
!	set wetmassxx & wetvolxx from dry values & water mass
!	conform rnumbxx so that the mean particle size of each section
!	    (= dryvolxx/rnumbxx) is within the section limits
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	real densdefault, densh2o, smallmassaa, smallmassbb
	real delta_water_conform1, delta_numb_conform1
	real drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	     drydensxx(maxd_asize)
	real drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	     drymassxx(maxd_asize)
	real dryvolxx(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize)
	real rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	     rnumbxx(maxd_asize)
	real specdensxx(maxd_acomp), specmwxx(maxd_acomp)
	real wetvolxx(maxd_asize)
	real wetmassxx(maxd_asize)


!   local variables
	integer l, ll, lnew, lold, l3, n, nnew, nold

	real dummass, dumnum, dumnum_at_dhi, dumnum_at_dlo, dumr,   &
      	  dumvol, dumvol1p, dumwatrmass


!   assure positive definite
	do l = 1, ltot2
	    rsub(l,k,m) = max( 0., rsub(l,k,m) )
	end do

!   load mixrats into working arrays and assure positive definite
	llhysw = ncomp_plustracer_aer(itype) + 1
	llwater = ncomp_plustracer_aer(itype) + 2
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)
		l = massptr_aer(ll,n,itype,iphase)
		rmassxx(ll,n) = rsub(l,k,m)
	    end do
	    rmassxx(llhysw,n) = 0.
	    l = 0
	    if (iphase .eq. ai_phase) l = hyswptr_aer(n,itype)
	    if (l .gt. 0) rmassxx(llhysw,n) = rsub(l,k,m)
	    rmassxx(llwater,n) = 0.
	    l = 0
	    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
	    if (l .gt. 0) rmassxx(llwater,n) = rsub(l,k,m)

	    rnumbxx(n)  = rsub(numptr_aer(n,itype,iphase),k,m)
	    rnumbxx0(n) = rnumbxx(n)
	    rnumbpp(n)  = rnumbxx(n)

	    drydenspp(n) = drydens_pregrow(n,itype)
	    drymasspp(n) = drymass_pregrow(n,itype)

	    drydensxx(n) = drydens_aftgrow(n,itype)
	    drymassxx(n) = drymass_aftgrow(n,itype)
	    drydensxx0(n) = drydensxx(n)
	    drymassxx0(n) = drymassxx(n)
	end do

!   load specmw and specdens also
	do ll = 1, ncomp_plustracer_aer(itype)
	    specmwxx(ll) = mw_aer(ll,itype)
	    specdensxx(ll) = dens_aer(ll,itype)
	end do

	delta_water_conform1 = 0.0
	delta_numb_conform1 = 0.0


	do 1390 n = 1, nsize_aer(itype)

!
!   if drydens_aftgrow < 0.1, then bin had state="no_aerosol"
!   compute volume using default dry-densities, set water=0,
!	and conform the number
!   also do this if mass is extremely small (below smallmassaa)
!	OR if drydens_aftgrow > 20 (which is unrealistic)
!
	if ( (drydensxx(n) .lt.  0.1) .or.   &
	     (drydensxx(n) .gt. 20.0) .or.   &
      	     (drymassxx(n) .le. smallmassaa) ) then
	    dummass = 0.
	    dumvol = 0.
	    do ll = 1, ncomp_aer(itype)
		dumr = rmassxx(ll,n)*specmwxx(ll)
		dummass = dummass + dumr
		dumvol  = dumvol  + dumr/specdensxx(ll)
	    end do
	    drymassxx(n) = dummass
	    if (min(dummass,dumvol) .le. smallmassbb) then
		drydensxx(n) = densdefault
		dumvol = dummass/densdefault
		dumnum = dummass/(volumcen_sect(n,itype)*densdefault)
	    else
		drydensxx(n) = dummass/dumvol
		dumnum = rnumbxx(n)
		dumnum_at_dhi = dumvol/volumhi_sect(n,itype)
		dumnum_at_dlo = dumvol/volumlo_sect(n,itype)
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
	dumwatrmass = rmassxx(llwater,n)*mw_water_aer
	wetmassxx(n) = drymassxx(n) + dumwatrmass
	wetvolxx(n) = dryvolxx(n) + dumwatrmass/densh2o

1390	continue

	return
	end subroutine move_sections_initial_conform                          


!-----------------------------------------------------------------------
	subroutine move_sections_calc_movingcenter(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum )
!
!   routine calculates section movements for the moving-center approach
!
!   material in section n will be transfered to section nnewsave(1,n)
!
!   the nnewsave are calculated here
!   the actual transfer is done in another routine
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)
	real densdefault, densh2o, smallmassaa, smallmassbb
	real drydensxx(maxd_asize)
	real drymassxx(maxd_asize)
	real dryvolxx(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize)
	real rnumbxx(maxd_asize)
	real xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real wetmassxx(maxd_asize)
	real wetvolxx(maxd_asize)

!   local variables
	integer ll, n, ndum, nnew, nold
	real dumnum, dumvol, dumvol1p, sixoverpi, third
	character*160 msg


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
	if (dumnum .le. dumvol/volumhi_sect(nsize_aer(itype),itype)) then
	    nnew = nsize_aer(itype)
	    goto 1290
!   or below that of smallest section
	else if (dumnum .ge. dumvol/volumlo_sect(1,itype)) then
	    nnew = 1
	    goto 1290
	end if

!   dumvol1p is mean particle volume (cm3) for the section
	dumvol1p = dumvol/dumnum
	if (dumvol1p .gt. volumhi_sect(n,itype)) then
	    do while ( (nnew .lt. nsize_aer(itype)) .and.   &
      		       (dumvol1p .gt. volumhi_sect(nnew,itype)) )
		nnew = nnew + 1
	    end do

	else if (dumvol1p .lt. volumlo_sect(n,itype)) then
	    do while ( (nnew .gt. 1) .and.   &
      		       (dumvol1p .lt. volumlo_sect(nnew,itype)) )
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
!   output nnewsave values when msectional=7xxx
	if (idiag_movesect .eq. 70) then
	    ndum = 0
	    do n = 1, nsize_aer(itype)
		if (nnewsave(1,n) .ne. n) ndum = ndum + 1
	    end do
	    if (ndum .gt. 0) then
		write(msg,97751) 'YES', iclm, jclm, k, m,   &
      		ndum, (nnewsave(1,n), n=1,nsize_aer(itype))
		call peg_message( lunout, msg )
	    else
		write(msg,97751) 'NO ', iclm, jclm, k, m,   &
      		ndum, (nnewsave(1,n), n=1,nsize_aer(itype))
		call peg_message( lunout, msg )
	    end if
	end if
97751	format( 'movesect', a, 4i3, 3x, i3, 3x, 14i3 )

	return
	end subroutine move_sections_calc_movingcenter                          


!-----------------------------------------------------------------------
	subroutine move_sections_calc_masnumadvect(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drydensxx0, drymassxx0, rnumbxx0,   &
      	  drydensxx, drymassxx, dryvolxx, rmassxx, rnumbxx,   &
      	  wetmassxx, wetvolxx,   &
      	  xferfracvol, xferfracnum )
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
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)

	real densdefault, densh2o, smallmassaa, smallmassbb
	real drydenspp(maxd_asize), drydensxx0(maxd_asize),   &
      	     drydensxx(maxd_asize)
	real drymasspp(maxd_asize), drymassxx0(maxd_asize),   &
      	     drymassxx(maxd_asize)
	real dryvolxx(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize)
	real rnumbpp(maxd_asize), rnumbxx0(maxd_asize),   &
      	     rnumbxx(maxd_asize)
	real xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real wetvolxx(maxd_asize)
	real wetmassxx(maxd_asize)

!   local variables
	integer ierr, n, nnew, nnew2
	integer iforce_movecenter(maxd_asize)

	real dum1, dum2, dum3
	real dumaa, dumbb, dumgamma, dumratio
	real dumfracnum, dumfracvol
	real dumntot
	real dumv
	real dumvbar_aft, dumvbar_pre
	real dumvcutlo_nnew_pre, dumvcuthi_nnew_pre
	real dumvlo_pre, dumvhi_pre, dumvdel_pre
	real dumvtot_aft, dumvtot_pre
	real dumzlo, dumzhi
	real sixoverpi, third

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
	dumvcutlo_nnew_pre = volumlo_sect(nnew,itype)*(dumvbar_pre/dumvbar_aft)
	dumvcuthi_nnew_pre = volumhi_sect(nnew,itype)*(dumvbar_pre/dumvbar_aft)
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
	if (dumntot .le. dumvtot_aft/volumhi_sect(nsize_aer(itype),itype)) then
	    nnew = nsize_aer(itype)
	    iforce_movecenter(n) = 2
	    goto 1290
	else if (dumntot .ge. dumvtot_aft/volumlo_sect(1,itype)) then
	    nnew = 1
	    iforce_movecenter(n) = 3
	    goto 1290
	end if

!   dumvbar_aft is mean particle volume (cm3) for the section
!   find the section that encloses this volume
	dumvbar_aft = dumvtot_aft/dumntot
	if (dumvbar_aft .gt. volumhi_sect(n,itype)) then
	    do while ( (nnew .lt. nsize_aer(itype)) .and.   &
      		       (dumvbar_aft .gt. volumhi_sect(nnew,itype)) )
		nnew = nnew + 1
	    end do

	else if (dumvbar_aft .lt. volumlo_sect(n,itype)) then
	    do while ( (nnew .gt. 1) .and.   &
      		       (dumvbar_aft .lt. volumlo_sect(nnew,itype)) )
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
!	OR if drydenspp > 20 (unphysical)
	if ( (drydenspp(n) .lt.  0.1) .or.   &
	     (drydenspp(n) .gt. 20.0) .or.   &
      	     (drymasspp(n) .le. smallmassaa) ) then
	    iforce_movecenter(n) = 11
	    goto 3700
	end if

	dumvtot_pre = drymasspp(n)/drydenspp(n)

	dumvlo_pre = volumlo_sect(n,itype)
	dumvhi_pre = volumhi_sect(n,itype)
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
	dumvcutlo_nnew_pre = volumlo_sect(nnew,itype)*(dumvbar_pre/dumvbar_aft)
	dumvcuthi_nnew_pre = volumhi_sect(nnew,itype)*(dumvbar_pre/dumvbar_aft)

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
	dumzlo = max( dumzlo, 0.0 )
	dumzhi = min( dumzhi, 1.0 )
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

!   output nnewsave values when msectional=7xxx
	if (idiag_movesect .ne. 70) goto 3800

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
      		volumlo_sect(n,itype), dumvbar_pre, volumhi_sect(n,itype)
	call peg_message( lunout, msg )
	write(msg,97020) 'vlop , vbarp,  vhip ',   &
      		dumvlo_pre, dumvbar_pre, dumvhi_pre
	call peg_message( lunout, msg )
	write(msg,97020) 'vloax, vbarax, vhiax',   &
      		dumvcutlo_nnew_pre, dumvbar_pre, dumvcuthi_nnew_pre
	call peg_message( lunout, msg )
	write(msg,97020) 'vloa0, vbara,  vhia0',   &
      		volumlo_sect(nnew,itype), dumvbar_aft, volumhi_sect(nnew,itype)
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
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drydenspp, drymasspp, rnumbpp,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum )
!
!   routine performs the actual transfer of aerosol number and mass
!	between sections
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)

	real densdefault, densh2o, smallmassaa, smallmassbb
	real delta_water_conform1, delta_numb_conform1
	real drydenspp(maxd_asize)
	real drymasspp(maxd_asize)
	real drymassxx(maxd_asize), drymassyy(maxd_asize)
	real dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real rnumbpp(maxd_asize)
	real rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real wetmassxx(maxd_asize), wetmassyy(maxd_asize)

!   local variables
	integer jj, l, ll, n, ndum, nnew, nold
	integer jja, jjb, jjc

	real delta_numb_conform2,   &
	  dumbot, dumnum, dumnum_at_dhi, dumnum_at_dlo,   &
      	  dumvol, dumvol1p, dumxfvol, dumxfnum, sixoverpi, third
	real dumpp(maxd_asize), dumxx(maxd_asize), dumyy(maxd_asize),   &
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
!   - copy new mixrats back to rsub array
!
	call move_sections_conserve_check(   &
	  1, iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy )

	delta_numb_conform2 = 0.0

	do 3900 n = 1, nsize_aer(itype)

	dumvol = dryvolyy(n)
	if (min(drymassyy(n),dumvol) .le. smallmassbb) then
	    dumvol = drymassyy(n)/densdefault
	    dumnum = drymassyy(n)/(volumlo_sect(n,itype)*densdefault)
	    delta_numb_conform2 = delta_numb_conform2 + dumnum - rnumbyy(n)
	    rnumbyy(n) = dumnum
	    adrydens_sub(n,itype,k,m) = densdefault
	    awetdens_sub(n,itype,k,m) = densdefault
	    admeandry_sub(n,itype,k,m) = dcen_sect(n,itype)
	    admeanwet_sub(n,itype,k,m) = dcen_sect(n,itype)
	else
	    dumnum = rnumbyy(n)
	    dumnum_at_dhi = dumvol/volumhi_sect(n,itype)
	    dumnum_at_dlo = dumvol/volumlo_sect(n,itype)
	    dumnum = max( dumnum_at_dhi, min( dumnum_at_dlo, dumnum ) )
	    delta_numb_conform2 = delta_numb_conform2 + dumnum - rnumbyy(n)
	    rnumbyy(n) = dumnum
	    adrydens_sub(n,itype,k,m) = drymassyy(n)/dumvol
	    dumvol1p = dumvol/dumnum
	    admeandry_sub(n,itype,k,m) = (dumvol1p*sixoverpi)**third
	    awetdens_sub(n,itype,k,m) = wetmassyy(n)/wetvolyy(n)
	    dumvol1p = wetvolyy(n)/dumnum
	    admeanwet_sub(n,itype,k,m) = min( 100.*dcen_sect(n,itype),   &
      			(dumvol1p*sixoverpi)**third )
	end if
	aqvoldry_sub(n,itype,k,m) = dumvol
	aqmassdry_sub(n,itype,k,m) = drymassyy(n)

!	if (k .eq. 1) then
!	    awetdens_sfc(n,itype,iclm,jclm) = awetdens_sub(n,itype,k,m)
!	    admeanwet_sfc(n,itype,iclm,jclm) = admeanwet_sub(n,itype,k,m)
!	end if

	do ll = 1, ncomp_plustracer_aer(itype)
	    l = massptr_aer(ll,n,itype,iphase)
	    rsub(l,k,m) = rmassyy(ll,n)
	end do
	l = 0
	if (iphase .eq. ai_phase) then
	    l = waterptr_aer(n,itype)
	    if (l .gt. 0) rsub(l,k,m) = rmassyy(llwater,n)
	    l = hyswptr_aer(n,itype)
	    if (l .gt. 0) rsub(l,k,m) = rmassyy(llhysw,n)
	end if
	rsub(numptr_aer(n,itype,iphase),k,m) = rnumbyy(n)

3900	continue

	delta_numb_conform1 = delta_numb_conform1 + delta_numb_conform2

	call move_sections_conserve_check(   &
	  2, iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy )


!   diagnostic output
!   output nnewsave values when msectional=7xxx
	if (idiag_movesect .ne. 70) goto 4900

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

	dumbot = log( volumhi_sect(1,itype)/volumlo_sect(1,itype) )
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
		    dumpp(n) = 1.0 + log(dumvol1p/volumlo_sect(1,itype))/dumbot
		end if
	    end if
	    if ((rnumbxx(n) .ge. 1.0e-35) .and.   &
      		(dryvolxx(n) .ge. 1.0e-35)) then
		dumvol1p = dryvolxx(n)/rnumbxx(n)
		dumxx(n) = 1.0 + log(dumvol1p/volumlo_sect(1,itype))/dumbot
	    end if
	    if ((rnumbyy(n) .ge. 1.0e-35) .and.   &
      		(dryvolyy(n) .ge. 1.0e-35)) then
		dumvol1p = dryvolyy(n)/rnumbyy(n)
		dumyy(n) = 1.0 + log(dumvol1p/volumlo_sect(1,itype))/dumbot
	    end if
	end do

!	write(msg,97030) 'lnvolold', (dumxx(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )
!	write(msg,97030) 'lnvolnew', (dumyy(n), n=1,nsize_aer(itype))
!	call peg_message( lunout, msg )

	do jja = 1, 7
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
	    end if
	    do jjb = 1, nsize_aer(itype), 10
		jjc = min( jjb+9, nsize_aer(itype) )
		if (jja .le. 4) then
		    write(msg,97020) dumch8, (dumout(n), n=jjb,jjc)
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
97011	format( 5x, 10(3x,2i3) )
97020	format( a, 1p, 10e9.1 )
97030	format( a,     10f9.3 )

4900	continue
	return
	end subroutine move_sections_apply_moves                          


!-----------------------------------------------------------------------
! rce 08-nov-2004 - this routine is new (and perhaps temporary)
!
	subroutine move_sections_apply_n1_inflow(   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
      	  drymassxx, dryvolxx, rmassxx, rnumbxx, wetmassxx, wetvolxx,   &
      	  drymassyy, dryvolyy, rmassyy, rnumbyy, wetmassyy, wetvolyy,   &
      	  xferfracvol, xferfracnum,   &
      	  specmwxx, specdensxx )
!
!   routine applies an ad_hoc correction to bin 1 to account for
!	growth of smaller particles (which are not simulated) growing into
!	bin 1
!   the correction to particle number balances the loss of particles from
!	bin 1 to larger bins by growth
!   the correction to mass assumes that the particles coming into bin 1
!	are slightly larger than dlo_sect(1)
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)

	real densdefault, densh2o, smallmassaa, smallmassbb
	real delta_water_conform1, delta_numb_conform1
	real drymassxx(maxd_asize), drymassyy(maxd_asize)
	real dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real xferfracvol(2,maxd_asize), xferfracnum(2,maxd_asize)
	real wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real wetmassxx(maxd_asize), wetmassyy(maxd_asize)
	real specdensxx(maxd_acomp), specmwxx(maxd_acomp)


!   local variables
	integer jj, l, ll, n, nnew, nold

	real deltanum, deltavol, dumvol1p, dumxfvol, dumxfnum


!
!   compute fraction of number transferred out of bin 1 by growth
!
	nold = 1
	n = nold
	if ( (nnewsave(1,nold) .eq. nold) .and.   &
      	     (nnewsave(2,nold) .eq. 0   ) ) goto 3900

	dumxfnum = 0.0
	do 2800 jj = 1, 2
	    nnew = nnewsave(jj,nold)
	    if (nnew .le. 0) goto 2800
	    if (nnew .eq. nold) goto 2800
	    dumxfnum = dumxfnum + xferfracnum(jj,nold)
2800	continue

!
!   compute "inflow" change to number and volume
!     number change matches that lost by growth
!     volume change assume inflow particles slightly bigger then dlo_sect
!
	dumvol1p = 0.95*volumlo_sect(n,itype) + 0.05*volumhi_sect(n,itype)
	deltanum = dumxfnum*rnumbxx(n)
	deltavol = deltanum*dumvol1p
	if (dumxfnum .le. 0.0) goto 3900
	if (deltanum .le. 0.0) goto 3900
	if (deltavol .le. 0.0) goto 3900

!
!   increment the number and masses
!   if the old dryvol (dryvolxx) > smallmassbb, then compute mass increment for 
!	each species from the old masses (rmassxx)
!   otherwise only increment the first mass species
!
	if (dryvolxx(n) .gt. smallmassbb) then
	    dumxfvol = deltavol/dryvolxx(n)
	    do ll = 1, ncomp_plustracer_aer(itype) + 2
		rmassyy(ll,n) = rmassyy(ll,n) + dumxfvol*rmassxx(ll,n)
	    end do
	else
	    ll = 1
	    rmassyy(ll,n) = rmassyy(ll,n) + deltavol*specdensxx(ll)/specmwxx(ll)
	end if
	rnumbyy(n) = rnumbyy(n) + deltanum

!
!   transfer results into rsub
!
	do ll = 1, ncomp_plustracer_aer(itype)
	    l = massptr_aer(ll,n,itype,iphase)
	    rsub(l,k,m) = rmassyy(ll,n)
	end do
	if (iphase .eq. ai_phase) then
	    l = waterptr_aer(n,itype)
	    if (l .gt. 0) rsub(l,k,m) = rmassyy(llwater,n)
	    l = hyswptr_aer(n,itype)
	    if (l .gt. 0) rsub(l,k,m) = rmassyy(llhysw,n)
	end if
	rsub(numptr_aer(n,itype,iphase),k,m) = rnumbyy(n)


3900	continue
	return
	end subroutine move_sections_apply_n1_inflow


!-----------------------------------------------------------------------
	subroutine move_sections_conserve_check( ipass,   &
	  iflag, iclm, jclm, k, m, iphase, itype,   &
      	  method_movesect, idiag_movesect, llhysw, llwater, nnewsave,   &
      	  densdefault, densh2o, smallmassaa, smallmassbb,   &
      	  delta_water_conform1, delta_numb_conform1,   &
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
!	computes thesum(3,ll) from rsub before section movement
!   ipass = 2
!	computes thesum(4,ll) from rsub after  section movement
!	compares thesum(3,ll) with thesum(4,ll)
!
!   currently only implemented for condensational growth (iflag=1)
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer ipass, iflag, iclm, jclm, iphase, itype, k,   &
      	  m, method_movesect, idiag_movesect, llhysw, llwater
	integer nnewsave(2,maxd_asize)
	real densdefault, densh2o, smallmassaa, smallmassbb
	real delta_water_conform1, delta_numb_conform1
	real drymassxx(maxd_asize), drymassyy(maxd_asize)
	real dryvolxx(maxd_asize), dryvolyy(maxd_asize)
	real rmassxx(maxd_acomp+2,maxd_asize),   &
      	     rmassyy(maxd_acomp+2,maxd_asize)
	real rnumbxx(maxd_asize), rnumbyy(maxd_asize)
	real wetvolxx(maxd_asize), wetvolyy(maxd_asize)
	real wetmassxx(maxd_asize), wetmassyy(maxd_asize)

!   local variables
	integer jj, l, ll, llworst, llworstb, n
	integer nerr, nerrmax
	save nerr, nerrmax
	data nerr, nerrmax / 0, 999 /

	real dumbot, dumtop, dumtoler, dumerr, dumworst, dumworstb
	real duma, dumb, dumc, dume
	real thesum(4,maxd_acomp+7)
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
		end if
		if (l .gt. 0)   &
		    thesum(ipass+2,ll) = thesum(ipass+2,ll) + rsub(l,k,m)
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
	    if (ll .le. ncomp_plustracer_aer(itype)) dumname(ll) =   &
      	    		name(massptr_aer(ll,1,itype,iphase))(1:4)
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
	do ll = 1, ncomp_plustracer_aer(itype)+7
	    dumtop = thesum(jj+1,ll) - thesum(jj,ll)
!	    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-35 )
! change minimum dumbot to 1.0e-30 to reduce unimportant error messages
	    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-30 )
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
!		    dumbot = max( abs(duma), abs(dumb), 1.0e-35 )
		    dumbot = max( abs(duma), abs(dumb), 1.0e-30 )
		    dume = dumtop/dumbot
		    if (abs(dume) .lt. abs(dumerr)) dumerr = dume
		end if
	    end if

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
		write(msg,97130) 'name/relerr/thesum', jj, '/thesum', jj+1,   &
      			dumname(ll), dumworst, thesum(jj,ll), thesum(jj+1,ll)
		call peg_message( lunout, msg )

		if ( (ll .eq. ncomp_plustracer_aer(itype)+3) .and.   &
		     (abs(dumworstb) .gt. dumtoler) ) then
		    ll = max( 1, llworstb )
		    dumtop = thesum(jj+1,ll) - thesum(jj,ll)
!		    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-35 )
		    dumbot = max( abs(thesum(jj,ll)), abs(thesum(jj+1,ll)), 1.0e-30 )
		    dumerr = dumtop/dumbot
		    write(msg,97130) 'name/relerr/thesum', jj, '/thesum', jj+1,   &
      			dumname(ll), dumerr, thesum(jj,ll), thesum(jj+1,ll)
		    call peg_message( lunout, msg )
		end if
	    end if
	end if

97110	format( 'movesect conserve ERROR - i/j/k/m/pass/llworst',   &
		4i3, 2x, 2i3 )
97120	format( a, 64i3 )
97130	format( a, i1, a, i1, 2x, a, 1p, 3e16.7 )

	return
	end subroutine move_sections_conserve_check        


!-----------------------------------------------------------------------
	subroutine test_move_sections( iflag_test, iclm, jclm, k, m )
!
!   routine runs tests on move_sections, using a matrix of
!	pregrow and aftgrow masses for each section
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr arguments
	integer iflag_test, iclm, jclm, k, m

!   local variables
	integer idiag_movesect, iflag, ii, iphase, itype, jj,   &
	  l, ll, n, nn
	integer ientryno
	save ientryno
	data ientryno / 0 /

	real dumnumb, dumvolpre, dumvolaft
	real dumsv_rsub(l2maxd)
	real dumsv_drymass_pregrow(maxd_asize,maxd_atype)
	real dumsv_drymass_aftgrow(maxd_asize,maxd_atype)
	real dumsv_drydens_pregrow(maxd_asize,maxd_atype)
	real dumsv_drydens_aftgrow(maxd_asize,maxd_atype)

	character*160 msg

	integer maxvolfactpre, maxvolfactaft
	parameter (maxvolfactpre=15, maxvolfactaft=23)

	real dumvolfactpre(maxvolfactpre)
	data dumvolfactpre /   &
      	2.0, 0.0, 1.0e-20, 0.5, 0.9,   &
      	1.0, 1.01, 1.1, 2.0, 4.0, 7.9, 7.99, 8.0,   &
      	8.1, 16.0 /

	real dumvolfactaft(maxvolfactaft)
	data dumvolfactaft /   &
      	4.0, 0.0, 1.0e-20, 0.01, 0.02, 0.05, 0.1, 0.5, 0.9,   &
      	1.0, 1.01, 1.1, 2.0, 4.0, 7.9, 7.99, 8.0,   &
      	8.1, 16.0, 32., 64., 128., 256. /


!
!   check for valid inputs
!   and first entry
!
	if (msectional .le. 0) return
	if (nsize_aer(1) .le. 0) return

	idiag_movesect = mod( msectional, 10000 )/100
	if (idiag_movesect .ne. 70) return
	
	ientryno = ientryno + 1
	if (ientryno .gt. 2) return

!
!   save rsub and drymass/dens_aft/pregrow
!
	do l = 1, ltot2
	    dumsv_rsub(l) = rsub(l,k,m)
	end do
	do itype = 1, ntype_aer
	    do n = 1, nsize_aer(itype)
		dumsv_drymass_pregrow(n,itype) = drymass_pregrow(n,itype)
		dumsv_drymass_aftgrow(n,itype) = drymass_aftgrow(n,itype)
		dumsv_drydens_pregrow(n,itype) = drydens_pregrow(n,itype)
		dumsv_drydens_aftgrow(n,itype) = drydens_aftgrow(n,itype)
	    end do
	end do

!
!   make test calls to move_sections
!
	do 3900 iflag = 1, 1

	iphase = ai_phase
	if (iabs(iflag) .eq. 2) iphase = cw_phase

	do 3800 itype = 1, ntype_aer

	do 2900 nn = 1, nsize_aer(itype)

	do 2800 ii = 1, maxvolfactpre

	do 2700 jj = 1, maxvolfactaft

!   zero out rsub and dryxxx_yyygrow arrays
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_plustracer_aer(itype)
		rsub(massptr_aer(ll,n,itype,iphase),k,m) = 0.0
	    end do
	    l = 0
	    if (iphase .eq. ai_phase) l = waterptr_aer(n,itype)
	    if (l .gt. 0) rsub(l,k,m) = 0.0
	    l = numptr_aer(n,itype,iphase)
	    if (l .gt. 0) rsub(l,k,m) = 0.0
	    drymass_pregrow(n,itype) = 0.0
	    drymass_aftgrow(n,itype) = 0.0
	    drydens_pregrow(n,itype) = -1.0
	    drydens_aftgrow(n,itype) = -1.0
	end do

!   fill in values for section nn
	n = nn
	dumnumb = 1.0e7
	rsub(numptr_aer(n,itype,iphase),k,m) = dumnumb
	ll = 1
	l = massptr_aer(ll,n,itype,iphase)

	dumvolpre = volumlo_sect(n,itype)*dumvolfactpre(ii)*dumnumb
	drydens_pregrow(n,itype) = dens_aer(ll,itype)
	drymass_pregrow(n,itype) = dumvolpre*drydens_pregrow(n,itype)
	if (ii .eq. 1) drydens_pregrow(n,itype) = -1.0
	
	dumvolaft = volumlo_sect(n,itype)*dumvolfactaft(jj)*dumnumb
	drydens_aftgrow(n,itype) = dens_aer(ll,itype)
	drymass_aftgrow(n,itype) = dumvolaft*drydens_aftgrow(n,itype)
	if (jj .eq. 1) drydens_aftgrow(n,itype) = -1.0
	
	rsub(l,k,m) = drymass_aftgrow(n,itype)/mw_aer(ll,itype)
	
	msg = ' '
	call peg_message( lunout, msg )
	write(msg,98010) nn, ii, jj
	call peg_message( lunout, msg )
	write(msg,98011) dumvolfactpre(ii), dumvolfactaft(jj)
	call peg_message( lunout, msg )

!   make test call to move_sections
	call move_sections( iflag, iclm, jclm, k, m )

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

!
!   restore rsub and drymass/dens_aft/pregrow
!
	do l = 1, ltot2
	    rsub(l,k,m) = dumsv_rsub(l)
	end do
	do itype = 1, ntype_aer
	do n = 1, nsize_aer(itype)
	    drymass_pregrow(n,itype) = dumsv_drymass_pregrow(n,itype)
	    drymass_aftgrow(n,itype) = dumsv_drymass_aftgrow(n,itype)
	    drydens_pregrow(n,itype) = dumsv_drydens_pregrow(n,itype)
	    drydens_aftgrow(n,itype) = dumsv_drydens_aftgrow(n,itype)
	end do
	end do

	return
	end subroutine test_move_sections                           


!-----------------------------------------------------------------------
	subroutine move_sections_checkptrs( iflag, iclm, jclm, k, m )
!
!   checks for valid number and water pointers
!
	implicit none

!	include 'v33com'
!	include 'v33com2'
!	include 'v33com3'
!	include 'v33com9a'
!	include 'v33com9b'

!   subr parameters
	integer iflag, iclm, jclm, k, m

!   local variables
	integer l, itype, iphase, n, ndum
	character*160 msg

	do 1900 itype = 1, ntype_aer
	do 1800 iphase = 1, nphase_aer

	ndum = 0
	do n = 1, nsize_aer(itype)
	    l = numptr_aer(n,itype,iphase)
	    if ((l .le. 0) .or. (l .gt. ltot2)) then
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
	    if ((l .gt. 0) .and. (l .le. ltot2)) ndum = ndum + 1
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



	end module module_mosaic_movesect
