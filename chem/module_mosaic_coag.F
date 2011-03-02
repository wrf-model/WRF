!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
	module module_mosaic_coag



	use module_peg_util



	implicit none



	contains



!-----------------------------------------------------------------------
	subroutine mosaic_coag_1clm( istat_coag,   &
		it, jt, kclm_calcbgn, kclm_calcend,   &
		idiagbb_in,   &
		dtchem, dtcoag_in,   &
		id, ktau, ktauc, its, ite, jts, jte, kts, kte )
!
!   calculates aerosol particle coagulation for grid points in
!	the i=it, j=jt vertical column 
!	over timestep dtcoag_in
!
!   uses a version of subr. coagsolv (see additional disclaimer below)
!	that was obtained from mark jacobson in june 2003,
!	and then modified to work with mosaic aerosol routines
!
	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_state_description, only:  param_first_scalar

!   subr arguments
	integer, intent(inout) :: istat_coag    ! =0 if no problems
	integer, intent(in) ::   &
		it, jt, kclm_calcbgn, kclm_calcend,   &
		idiagbb_in,   &
		id, ktau, ktauc, its, ite, jts, jte, kts, kte
	real, intent(in) :: dtchem, dtcoag_in

!   NOTE - much information is passed via arrays in 
!		module_data_mosaic_asect and module_data_mosaic_other
!
!   rsub (inout) - aerosol mixing ratios
!   aqmassdry_sub, aqvoldry_sub (inout) - 
!		aerosol dry-mass and dry-volume mixing ratios
!   adrydens_sub (inout) - aerosol dry density
!   rsub(ktemp,:,:), ptotclm, cairclm (in) - 
!		air temperature, pressure, and molar density


!   local variables
	integer, parameter :: coag_method = 1
	integer, parameter :: ncomp_coag_maxd = maxd_acomp + 3

	integer :: k, l, ll, lla, llb, llx, m
	integer :: isize, itype, iphase
	integer :: iconform_numb
	integer :: idiagbb, idiag_coag_onoff, iok
	integer :: lunout_coag
	integer :: ncomp_coag, nsize, nsubstep_coag, ntau_coag
	integer,save :: ncountaa(10), ncountbb(0:ncomp_coag_maxd)
	integer :: p1st

	real, parameter :: densdefault = 2.0
	real, parameter :: factsafe = 1.00001
	real, parameter :: onethird = 1.0/3.0
	real, parameter :: piover6 = pi/6.0
	real, parameter :: smallmassbb = 1.0e-30

	real :: cair_box
	real :: dtcoag
	real :: duma, dumb, dumc
	real :: patm_box
	real :: temp_box
	real :: xxdens, xxmass, xxnumb, xxvolu
	real :: xxmasswet, xxvoluwet

	real :: dpdry(maxd_asize), dpwet(maxd_asize), denswet(maxd_asize)
	real :: num_distrib(maxd_asize)
	real :: sumnew(ncomp_coag_maxd), sumold(ncomp_coag_maxd)
	real :: vol_distrib(maxd_asize,ncomp_coag_maxd)
	real :: xxvolubb(maxd_asize)

	character(len=120) :: msg



	istat_coag = 0

	lunout_coag = 6
	if (lunout .gt. 0) lunout_coag = lunout


!   when dtcoag_in > dtchem, do not perform coagulation calcs 
!   on every chemistry time step
	ntau_coag = nint( dtcoag_in/dtchem )
	ntau_coag = max( 1, ntau_coag )
	if (mod(ktau,ntau_coag) .ne. 0) return
	dtcoag = dtchem*ntau_coag


!   set variables that do not change
    p1st = PARAM_FIRST_SCALAR
	idiagbb = idiagbb_in

!   NOTE - code currently just does 1 type
	itype = 1
	iphase = ai_phase
	nsize = nsize_aer(itype)
	ncomp_coag = ncomp_plustracer_aer(itype) + 3


!   loop over subareas (currently only 1) and vertical levels
	do 2900 m = 1, nsubareas

	do 2800 k = kclm_calcbgn, kclm_calcend


!   temporary diagnostics
	if ((it .eq. its) .and.   &
	    (jt .eq. jts) .and. (k .eq. kclm_calcbgn)) then
	    ncountaa(:) = 0
	    ncountbb(:) = 0
	end if


	ncountaa(1) = ncountaa(1) + 1
	if (afracsubarea(k,m) .lt. 1.e-4) goto 2700

	cair_box = cairclm(k)
	temp_box = rsub(ktemp,k,m)
	patm_box = ptotclm(k)/1.01325e6

	nsubstep_coag = 1
	idiag_coag_onoff = -1

!   do initial calculation of dry mass, volume, and density,
!   and initial number conforming (as needed)
	vol_distrib(:,:) = 0.0
	sumold(:) = 0.0
	do isize = 1, nsize
	    l = numptr_aer(isize,itype,iphase)
	    xxnumb = rsub(l,k,m)
	    xxmass = aqmassdry_sub(isize,itype,k,m)
	    xxvolu = aqvoldry_sub( isize,itype,k,m)
	    xxdens = adrydens_sub( isize,itype,k,m)
	    iconform_numb = 1

	    duma = max( abs(xxmass), abs(xxvolu*xxdens), 0.1*smallmassbb )
	    dumb = abs(xxmass - xxvolu*xxdens)/duma
	    if ( (xxdens .lt. 0.1) .or. (xxdens .gt. 20.0)   &
                                   .or. (dumb .gt. 1.0e-4) ) then
!   (exception) case of drydensity not valid, or mass /= volu*dens
!   so compute dry mass and volume from rsub
		ncountaa(2) = ncountaa(2) + 1
		if (idiagbb .ge. 200)   &
		    write(*,'(a,i10,4i4,1p,4e10.2)') 'coagaa-2a',   &
		    ktau, it, jt, k, isize, xxmass, xxvolu, xxdens
		xxmass = 0.0
		xxvolu = 0.0
		do ll = 1, ncomp_aer(itype)
		    l = massptr_aer(ll,isize,itype,iphase)
		    if (l .ge. p1st) then
			duma = max( 0.0, rsub(l,k,m) )*mw_aer(ll,itype)
			xxmass = xxmass + duma
			xxvolu = xxvolu + duma/dens_aer(ll,itype)
		    end if
		end do
		if (xxmass .gt. 0.99*smallmassbb) then
		    xxdens = xxmass/xxvolu
		    xxvolu = xxmass/xxdens
		    if (idiagbb .ge. 200)   &
			write(*,'(a,i10,4i4,1p,4e10.2)') 'coagaa-2b',   &
			ktau, it, jt, k, isize, xxmass, xxvolu, xxdens
		end if
	    end if

	    if (xxmass .le. 1.01*smallmassbb) then
!   when drymass extremely small, use default density and bin center size,
!   and zero out water
		ncountaa(3) = ncountaa(3) + 1
		xxdens = densdefault
		xxvolu = xxmass/xxdens
		xxnumb = xxmass/(volumcen_sect(isize,itype)*xxdens)
		l = hyswptr_aer(isize,itype)
		if (l .ge. p1st) rsub(l,k,m) = 0.0
		l = waterptr_aer(isize,itype)
		if (l .ge. p1st) rsub(l,k,m) = 0.0
		iconform_numb = 0
		if (idiagbb .ge. 200)   &
		    write(*,'(a,i10,4i4,1p,4e10.2)') 'coagaa-2c',   &
		    ktau, it, jt, k, isize, xxmass, xxvolu, xxdens
	    else
		xxvolu = xxmass/xxdens
	    end if

!   check for mean dry-size 'safely' within bounds, and conform number if not
	    if (iconform_numb .gt. 0) then
		if (xxnumb .gt.   &
			xxvolu/(factsafe*volumlo_sect(isize,itype))) then
		    ncountaa(4) = ncountaa(4) + 1
		    duma = xxnumb
		    xxnumb = xxvolu/(factsafe*volumlo_sect(isize,itype))
		    if (idiagbb .ge. 200)   &
			write(*,'(a,i10,4i4,1p,3e12.4)') 'coagcc-4 ',   &
			ktau, it, jt, k, isize, xxvolu, duma, xxnumb
		else if (xxnumb .lt.   &
			xxvolu*factsafe/volumhi_sect(isize,itype)) then
		    ncountaa(5) = ncountaa(5) + 1
		    duma = xxnumb
		    xxnumb = xxvolu*factsafe/volumhi_sect(isize,itype)
		    if (idiagbb .ge. 200)   &
			write(*,'(a,i10,4i4,1p,3e12.4)') 'coagcc-5 ',   &
			ktau, it, jt, k, isize, xxvolu, duma, xxnumb
		end if
	    end if

!   load numb, mass, volu, dens back into mosaic_asect arrays
	    l = numptr_aer(isize,itype,iphase)
	    rsub(l,k,m) = xxnumb
	    adrydens_sub( isize,itype,k,m) = xxdens
	    aqmassdry_sub(isize,itype,k,m) = xxmass
	    aqvoldry_sub( isize,itype,k,m) = xxvolu

!
!   load coagsolv arrays with number, mass, and volume mixing ratios
!
!   *** NOTE ***
!     num_distrib must be (#/cm3)
!     vol_distrib units do not matter - they can be either masses or volumes,
!        and either mixing ratios or concentrations
!	 (e.g., g/cm3-air, ug/kg-air, cm3/cm3-air, cm3/kg-air, ...)
!
	    num_distrib(isize) = xxnumb*cair_box

	    do ll = 1, ncomp_plustracer_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		duma = 0.0
		if (l .ge. p1st) duma = max( 0.0, rsub(l,k,m) )
		vol_distrib(isize,ll)   = duma
		sumold(ll) = sumold(ll) + duma
	    end do

	    do llx = 1, 3
		ll = (ncomp_coag-3) + llx
		duma = 0.0
		if (llx .eq. 1) then
		    l = hyswptr_aer(isize,itype)
		    if (l .ge. p1st) duma = max( 0.0, rsub(l,k,m) )
		else if (llx .eq. 2) then
		    l = waterptr_aer(isize,itype)
		    if (l .ge. p1st) duma = max( 0.0, rsub(l,k,m) )
		else
		    duma = max( 0.0, aqvoldry_sub( isize,itype,k,m) )
		end if
		vol_distrib(isize,ll)   = duma
		sumold(ll) = sumold(ll) + duma
	    end do

!   calculate dry and wet diameters and wet density
	    if (xxmass .le. 1.01*smallmassbb) then
		dpdry(isize) = dcen_sect(isize,itype)
		dpwet(isize) = dpdry(isize)
		denswet(isize) = xxdens
	    else
		dpdry(isize) = (xxvolu/(xxnumb*piover6))**onethird
		dpdry(isize) = max( dpdry(isize), dlo_sect(isize,itype) )
		l = waterptr_aer(isize,itype)
		if (l .ge. p1st) then
		    duma = max( 0.0, rsub(l,k,m) )*mw_water_aer
		    xxmasswet = xxmass + duma
		    xxvoluwet = xxvolu + duma/dens_water_aer
		    dpwet(isize) = (xxvoluwet/(xxnumb*piover6))**onethird
		    dpwet(isize) = min( dpwet(isize), 30.0*dhi_sect(isize,itype) )
		    denswet(isize) = (xxmasswet/xxvoluwet)
		else
		    dpwet(isize) = dpdry(isize)
		    denswet(isize) = xxdens
		end if
	    end if
	end do


!
!   make call to coagulation routine
!
	call coagsolv(   &
	    nsize, maxd_asize, ncomp_coag, ncomp_coag_maxd,   &
	    temp_box, patm_box, dtcoag, nsubstep_coag,   &
	    lunout_coag, idiag_coag_onoff, iok,   &
	    dpdry, dpwet, denswet, num_distrib, vol_distrib )

	if (iok .lt. 0) then
	    msg = '*** subr mosaic_coag_1clm -- fatal error in coagsolv'
	    call peg_message( lunout, msg )
	    call peg_error_fatal( lunout, msg )
	else if (iok .gt. 0) then
	    ncountaa(6) = ncountaa(6) + 1
	    goto 2700
	end if


!
!   unload coagsolv arrays
!
	sumnew(:) = 0.0
	do isize = 1, nsize
	    do ll = 1, ncomp_coag
		sumnew(ll) = sumnew(ll) + max( 0.0, vol_distrib(isize,ll) )
	    end do

	    l = numptr_aer(isize,itype,iphase)
	    rsub(l,k,m) = max( 0.0, num_distrib(isize)/cair_box )

!   unload mass mixing ratios into rsub; also calculate dry mass and volume
	    xxmass = 0.0
	    xxvolu = 0.0
	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		if (l .ge. p1st) then
		    duma = max( 0.0, vol_distrib(isize,ll) )
		    rsub(l,k,m) = duma
		    duma = duma*mw_aer(ll,itype)
		    xxmass = xxmass + duma
		    xxvolu = xxvolu + duma/dens_aer(ll,itype)
		end if
	    end do
	    aqmassdry_sub(isize,itype,k,m) = xxmass
	    xxvolubb(isize) = xxvolu

	    ll = (ncomp_coag-3) + 1
	    l = hyswptr_aer(isize,itype)
	    if (l .ge. p1st) rsub(l,k,m) = max( 0.0, vol_distrib(isize,ll) )

	    ll = (ncomp_coag-3) + 2
	    l = waterptr_aer(isize,itype)
	    if (l .ge. p1st) rsub(l,k,m) = max( 0.0, vol_distrib(isize,ll) )

	    ll = (ncomp_coag-3) + 3
	    aqvoldry_sub( isize,itype,k,m) = max( 0.0, vol_distrib(isize,ll) )
	end do


!   check for mass and volume conservation
	do ll = 1, ncomp_coag
	    duma = max( sumold(ll), sumnew(ll), 1.0e-35 )
	    if (abs(sumold(ll)-sumnew(ll)) .gt. 1.0e-6*duma) then
		ncountbb(ll) = ncountbb(ll) + 1
		ncountbb(0) = ncountbb(0) + 1
	    end if
	end do


!
!   calculate new dry density,
!   and check for new mean dry-size within bounds
!
	do isize = 1, nsize

	    xxmass = aqmassdry_sub(isize,itype,k,m)
	    xxvolu = aqvoldry_sub( isize,itype,k,m)
	    l = numptr_aer(isize,itype,iphase)
	    xxnumb = rsub(l,k,m)
	    iconform_numb = 1

!   do a cautious calculation of density, using volume from coagsolv
	    if (xxmass .le. smallmassbb) then
		ncountaa(8) = ncountaa(8) + 1
		xxdens = densdefault
		xxvolu = xxmass/xxdens
		xxnumb = xxmass/(volumcen_sect(isize,itype)*xxdens)
		l = hyswptr_aer(isize,itype)
		if (l .ge. p1st) rsub(l,k,m) = 0.0
		l = waterptr_aer(isize,itype)
		if (l .ge. p1st) rsub(l,k,m) = 0.0
		iconform_numb = 0
		if (idiagbb .ge. 200)   &
		    write(*,'(a,i10,4i4,1p,4e10.2)') 'coagaa-7a',   &
		    ktau, it, jt, k, isize, xxmass, xxvolu, xxdens
	    else if (xxmass .gt. 1000.0*xxvolu) then
!   in this case, density is too large.  setting density=1000 forces
!   next IF block while avoiding potential divide by zero or overflow
		xxdens = 1000.0
	    else 
		xxdens = xxmass/xxvolu
	    end if

	    if ((xxdens .lt. 0.1) .or. (xxdens .gt. 20.0)) then
!   (exception) case -- new dry density is unrealistic,
!   so use dry volume from rsub instead of that from coagsolv
		ncountaa(7) = ncountaa(7) + 1
		if (idiagbb .ge. 200)   &
		    write(*,'(a,i10,4i4,1p,4e10.2)') 'coagaa-7b',   &
		    ktau, it, jt, k, isize, xxmass, xxvolu, xxdens
		xxvolu = xxvolubb(isize)
		xxdens = xxmass/xxvolu
		if (idiagbb .ge. 200)   &
		    write(*,'(a,26x,1p,4e10.2)') 'coagaa-7c',   &
		    xxmass, xxvolu, xxdens
	    end if

!   check for mean size ok, and conform number if not
	    if (iconform_numb .gt. 0) then
		if (xxnumb .gt. xxvolu/volumlo_sect(isize,itype)) then
		    ncountaa(9) = ncountaa(9) + 1
		    duma = xxnumb
		    xxnumb = xxvolu/volumlo_sect(isize,itype)
		    if (idiagbb .ge. 200)   &
			write(*,'(a,i10,4i4,1p,3e12.4)') 'coagcc-9 ',   &
			ktau, it, jt, k, isize, xxvolu, duma, xxnumb
		else if (xxnumb .lt. xxvolu/volumhi_sect(isize,itype)) then
		    ncountaa(10) = ncountaa(10) + 1
		    duma = xxnumb
		    xxnumb = xxvolu/volumhi_sect(isize,itype)
		    if (idiagbb .ge. 200)   &
			write(*,'(a,i10,4i4,1p,3e12.4)') 'coagcc-10',   &
			ktau, it, jt, k, isize, xxvolu, duma, xxnumb
		end if
	    end if

!   load number, mass, volume, dry-density back into arrays
	    l = numptr_aer(isize,itype,iphase)
	    rsub(l,k,m) = xxnumb
	    adrydens_sub( isize,itype,k,m) = xxdens
	    aqmassdry_sub(isize,itype,k,m) = xxmass
	    aqvoldry_sub( isize,itype,k,m) = xxvolu

	end do


2700	continue

!   temporary diagnostics
	if ((idiagbb .ge. 100) .and.   &
	    (it .eq. ite) .and.    &
	    (jt .eq. jte) .and. (k .eq. kclm_calcend)) then
	    write(msg,93030) 'coagbb ncntaa ', ncountaa(1:10)
	    call peg_message( lunerr, msg )
	    if (ncountbb(0) .gt. 0) then
		do llx = 1, (ncomp_coag+9)/10
		    llb = llx*10
		    lla = llb - 9
		    llb = min( llb, ncomp_coag)
		    write(msg,93032) 'coagbb ncntbb',   &
			mod(llx,10), ncountbb(lla:llb)
		    call peg_message( lunerr, msg )
		end do
	    end if
	end if
93020	format( a, 1p, 10e10.2 )
93030	format( a, 1p, 10i10 )
93032	format( a, 1p, i1, 10i10 )


2800	continue	! k levels

2900	continue	! subareas


	return
	end subroutine mosaic_coag_1clm


!----------------------------------------------------------------------
      subroutine coagsolv(   &
        nbin, nbin_maxd, ncomp, ncomp_maxd,   &
        tkelvin_inp, patm_inp, deltat_inp, nsubstep,   &
        lunout, idiag_onoff, iok,   &
        dpdry_inp, dpwet_inp, denswet_inp,   &
        num_distrib_inp, vol_distrib_inp )

      implicit none

!
! *********************************************************************
! ***************** written by mark jacobson (1993) *******************
! ***           (c) copyright, 1993 by mark z. jacobson             ***
! ***              this version modified december, 2001             ***
! ***                       (650) 723-6836                          ***
! *********************************************************************
!
!   cccccc   ooooo      a      gggggg   ssssss  ooooo   l     v      v
!  c        o     o    a a    g        s       o     o  l      v    v
!  c        o     o   a   a   g   ggg   sssss  o     o  l       v  v
!  c        o     o  aaaaaaa  g     g        s o     o  l        v v
!   cccccc   ooooo  a       a  gggggg  ssssss   ooooo   lllllll   v
!
! *********************************************************************
! the use of this module implies that the user agrees not to sell the
! module or any portion of it, not to remove the copyright notice from it,
! and not to change the name of the module, "coagsolv". users may
! modify the module as needed or incorporate it into a larger model.
! any special requests with regard to this module may be directed to
! jacobson@stanford.edu.
! *********************************************************************
!
! *********************************************************************
! * this is a box-model version of "coagsolv," a semi-implicit        *
! * aerosol coagulation solver. the solver is exactly volume          *
! * conserving, unconditionally stable (regardless of time step),     *
! * and noniterative.                                                 *
! *                                                                   *
! * this program calculates brownian coagulation kernels and solves   *
! * self-coagulation among any number of size bins, one               *
! * particle type and any number of volume fractions.                 *
! *                                                                   *
! * the program is set up as a box-model.                             *
! *                                                                   *
! * the volume ratio of adjacent size bins can be any number > 1      *
! *                                                                   *
! * the scheme can be used to solve coagulation with any size bin     *
! * structure.                                                        *
! *                                                                   *
! * the initial size distribution here is monomer or lognormal        *
! * (ifsmoluc = 1 or 0) with a fixed size bin structure               *
! *                                                                   *
! *********************************************************************
! *                         references                                *
! *********************************************************************
! * semi-implicit scheme using movable or variable bins and with      *
! * any volume ratio  (vrat) > 1                                      *
! *                                                                   *
! * jacobson m. z., turco r. p., jensen, e. j. and toon o. b. (1994)  *
! *  modeling coagulation among particles of different compostion     *
! *  and size. atmos. environ. 28a, 1327 - 1338.                      *
! *                                                                   *
! * jacobson m. z. (1999) fundamentals of atmospheric modeling.       *
! *  cambridge university press, new york, 656 pp.                    *
! *                                                                   *
! *********************************************************************
! * semi-implicit scheme using fixed bins with volume ratio >,= 2     *
! *                                                                   *
! * toon o. b., turco r. p., westphal d., malone r. and liu m. s.     *
! *  (1988) a multidimensional model for aerosols: description of     *
! *  computational analogs. j. atmos. sci. 45, 2123 - 2143            *
! *                                                                   *
! *********************************************************************
! * orig semi-implicit scheme using fixed bins with volume ratio = 2  *
! *                                                                   *
! * turco r. p., hamill p., toon o. b., whitten r. c. and kiang c. s. *
! *  (1979) the nasa-ames research center stratospheric aerosol       *
! *  model: i physical processes and computational analogs. nasa      *
! *  tech. publ. (tp) 1362, iii-94.                                   *
! *********************************************************************
!
!   modified by y. zhang for incorporation into pnnl's mirage
!   june-july, 2003
!
! *********************************************************************
!
!   modified by r.c. easter for incorporation into pnnl's wrf-chem
!   feb 2005 (a)
!	added "_inp" to all subr arguments
!	added iradmaxd_inp & lunout
!	define iradmaxd, iaertymaxd, iaeromaxd locally
!	no include statements
!	changed real*16 to real*8
!   feb 2005 (b)
!	in coagsolv, distrib_inp is 2-d array that holds both number and
!	    volume concentrations; distrib is initialized from it;
!	    the "fracnaer" stuff is gone
!   feb 2005 (c)
!	change distrib & cc2 to be 2-d arrays (which simplifies indexing!);
!	iaeromaxd and iaero are gone;
!	deleted many commented-out lines in coagsolv
!   feb 2005 (d)
!       changed cc2(i,1) to be number instead of all-species volume;
!	    prod term for number distrib now follows jgr-2002 article
!	    [multiply by volume(j), divide by volume(k)]
!   apr 2006 (a)
!	considerable cleanup (mostly cosmetic)
!	pass in the bin sizes directly, rather than vrat & dmin_um
!       pass in dry and wet sizes separately
!       pass in/out number and volume distributions in separate arrays
!
! *********************************************************************


!   IN arguments
      integer nbin            ! actual number of size bins
      integer nbin_maxd       ! size-bin dimension for input (argument) arrays
      integer ncomp           ! actual number of aerosol volume components
      integer ncomp_maxd      ! volume-component dimension for input (argument) arrays
      integer lunout          ! logical unit for warning & diagnostic output
      integer idiag_onoff     ! if positive, write some mass-conservation diagnostics
      integer nsubstep        ! number of time sub-steps for the integration

      real tkelvin_inp             ! air temperature (K)
      real patm_inp                ! air pressure (atm)
      real deltat_inp              ! integration time (s)
      real dpdry_inp(nbin_maxd)    ! bin dry diameter (cm)
      real dpwet_inp(nbin_maxd)    ! bin wet (ambient) diameter (cm)
      real denswet_inp(nbin_maxd)  ! bin wet (ambient) density (g/cm3)

!   IN-OUT arguments
      real num_distrib_inp(nbin_maxd)
!          num_distrib_inp(i)   = number concentration for bin i (#/cm3)
      real vol_distrib_inp(nbin_maxd,ncomp_maxd)
!          vol_distrib_inp(i,j) = volume concentration for bin i,
!                                                component j (cm3/cm3)

!   OUT arguments
      integer iok             ! status flag (0=success, anything else=failure)

!
! NOTE -- The sectional representation is based on dry particle size.
! Dry sizes are used to calculate the receiving bin indices and product fractions
!   for each (bin-i1, bin-i2) coagulation combination.
! Wet sizes and densities are used to calculate the coagulation rate coefficients.
!
! NOTE -- Units for num_distrib and vol_distrib
!    num_distrib units MUST BE (#/cm3)
!    vol_distrib units do not really matter.  They can be either masses 
!        or volumes, and either mixing ratios or concentrations,
!        (e.g., g/cm3-air, ug/kg-air, cm3/m3-air, cm3/kg-air, ...).
!        Use whatever is convenient.
!

!
!   local variables
!
      integer iradmaxd_wrk, iaertymaxd_wrk
      parameter (iradmaxd_wrk=16)
      parameter (iaertymaxd_wrk=150)

      integer irad
      integer iaerty
! irad == nbin = actual number of size bins
! iradmaxd_wrk = size-bin dimension for local (working) arrays
!
! (iaerty-1) == ncomp = actual number of aerosol volume components
! iaertymaxd_wrk = volume-component dimension for local (working) arrays
!
! iaerty = 1 --> calc number concentration only
!        > 1 --> calc number concentration + (iaerty-1) volume concentrations

      integer i, isubstep, j, jaer, jb, k, kout

      integer jbinik(iradmaxd_wrk,iradmaxd_wrk,iradmaxd_wrk)
      integer jbins(iradmaxd_wrk,iradmaxd_wrk)

!wig, 11-Dec-2006: real*8 causing core dumps on IBM's (i.e. bluesky)
!      real*8 aknud, aloss, amu, avg,   &
      real aknud, aloss, amu, avg,   &
             boltg, bpm,   &
             cbr, consmu, cpi,   &
             deltat, deltp1, deltr,   &
             divis, dti1, dti2,   &
             finkhi, finklow, fourpi, fourrsq,   &
             ggr, gmfp,   &
             onepi,   &
             patm, pmfp, prod,   &
             radi, radiust, radj, ratior,   &
             rgas2, rho3, rsuma, rsumsq,   &
             sumdc, sumnaft, sumnbef,   &
             term1, term2, third, tk, tkelvin, tworad,   &
             viscosk, vk1, voltota, vthermg,   &
             wtair

!wig, 11-Dec-2006: real*8 causing core dumps on IBM's (i.e. bluesky)
!      real*8 cc2(iradmaxd_wrk,iaertymaxd_wrk),   &
      real cc2(iradmaxd_wrk,iaertymaxd_wrk),   &
             conc(iradmaxd_wrk),   &
             denav(iradmaxd_wrk) ,   &
             difcof(iradmaxd_wrk),   &
             distrib(iradmaxd_wrk,iaertymaxd_wrk),   &
             fink(iradmaxd_wrk,iradmaxd_wrk,iradmaxd_wrk),   &
             floss(iradmaxd_wrk,iradmaxd_wrk),   &
             radius(iradmaxd_wrk),   &
             radwet(iradmaxd_wrk),   &
             rrate(iradmaxd_wrk,iradmaxd_wrk),   &
             sumdp(iradmaxd_wrk),   &
             sumvt(iradmaxd_wrk),   &
             tvolfin(iaertymaxd_wrk),   &
             tvolinit(iaertymaxd_wrk),   &
             volume(iradmaxd_wrk),   &
             volwet(iradmaxd_wrk),   &
             vthermp(iradmaxd_wrk)


! *********************************************************************
!                    set some parameters
! *********************************************************************
      irad   = nbin
      iaerty = ncomp + 1

      kout = lunout

      if (irad .gt. iradmaxd_wrk) then
        write(lunout,*) '*** coagsolv: irad > iradmaxd_wrk: ',   &
          irad, iradmaxd_wrk
        iok = -1
        return
      endif
      if (iaerty .gt. iaertymaxd_wrk) then
        write(lunout,*) '*** coagsolv: iaerty > iaertymaxd_wrk: ',   &
          iaerty, iaertymaxd_wrk
        iok = -2
        return
      endif
!
! tkelvin = temperature (k)
! denav   = particle density (g cm-3)
! patm    = air pressure (atm)
!
      tkelvin   = tkelvin_inp
      patm      = patm_inp
      do i    = 1, irad
        denav(i)   = denswet_inp(i)
      end do

!
! deltat_inp = total integration time (s)
! deltat     = individual time step (s)
! nsubstep   = number of time steps
!
      deltat = deltat_inp/nsubstep

!
! boltg   = boltzmann's 1.38054e-16        (erg k-1 = g cm**2 sec-1 k-1)
! wtair   = molecular weight of air (g mol-1)
! avg     = avogadro's number (molec. mol-1)
! rgas2   = gas constant (l-atm mol-1 k-1)
! amu     = dynamic viscosity of air (g cm-1 sec-1)
!           est value at 20 c = 1.815e-04
! rho3    = air density (g cm-3)
! viscosk = kinematic viscosity = amu / denair = (cm**2 sec-1)
! vthermg = mean thermal velocity of air molecules (cm sec-1)
! gmfp    = mean free path of an air molecule  = 2 x viscosk /
!           thermal velocity of air molecules (gmfp units of cm)
!
      tk        = tkelvin
      third     = 1. / 3.
      onepi     = 3.14159265358979
      fourpi    = 4. * onepi
      boltg     = 1.38054e-16
      wtair     = 28.966
      avg       = 6.02252e+23
      rgas2     = 0.08206
      consmu    = 1.8325e-04 * (296.16 + 120.0)
      amu       = (consmu / (tk + 120.)) * (tk / 296.16)**1.5
      rho3      = patm * wtair * 0.001 / (rgas2 * tk)
      viscosk   = amu / rho3
      vthermg   = sqrt(8. * boltg * tk * avg / (onepi * wtair))
      gmfp      = 2.0 * viscosk / vthermg

!
! *********************************************************************
! *                     set volume ratio size bin grid                *
! *********************************************************************
!
      cpi          = fourpi / 3.

      do 30 j      = 1, irad
       radius( j)  = dpdry_inp(j) * 0.5
       volume( j)  = cpi * radius(j) * radius(j) * radius(j)
       radwet( j)  = dpwet_inp(j) * 0.5
       volwet( j)  = cpi * radwet(j) * radwet(j) * radwet(j)
 30   continue

!
! *********************************************************************
! *              determine bins where coagulated terms go             *
! *********************************************************************
! finklow        = volume fraction of i+j going to lower  (k  ) bin
! finkhi         = volume fraction of i+j going to higher (k+1) bin
! fink(i,j,k)    = volume fraction of i+j going to bin k
! floss          = simplified fink term used in loss rates
!                  no self-coagulation loss out of largest bin
! jbins(i,k)     = number of production terms into bin k from bin i
! jbinik(i,k,jb) = identifies each bin j that coagulates with bin i
!                  to produce bin k
!
      do 35 i             = 1, irad
        do 34 j            = 1, irad
          jbins(i,j)        = 0
          floss(i,j)        = 0.
          do 33 k           = 1, irad
            jbinik(i,j,k)    = 0
            fink(  i,j,k)    = 0.
 33       continue
 34     continue
 35   continue

      do 40 i             = 1, irad
        do 39 j            = 1, irad
          voltota           = volume(i) + volume(j)
          if (voltota.ge.volume(irad)) then

            if (i.eq.irad) then
              floss(i,j)      = 0.0
            else
              floss(i,j)      = 1.0
            endif

            if (j.lt.irad) then
              jbins(i,irad)     = jbins(i,irad) + 1
              jb                = jbins(i,irad)
              jbinik(i,irad,jb) = j
              fink(  i,jb,irad) = 1.0
            endif

          else
            do 45 k          = 1, irad - 1
              vk1             = volume(k+1)
              if (voltota.ge.volume(k).and.voltota.lt.vk1) then
                finklow        = ((vk1 - voltota)/(vk1 - volume(k)))   &
                               * volume(k) / voltota
                finkhi         = 1. - finklow

                if (i.lt.k) then
                  floss(i,j)      = 1.
                elseif (i.eq.k) then
                  floss(i,j)      = finkhi
                endif

                if (j.lt.k) then
                  jbins(i,k)      = jbins(i,k) + 1
                  jb              = jbins(i,k)
                  jbinik(i,k,jb)  = j
                  fink(  i,jb,k)  = finklow
                endif

                jbins(i,k+1)     = jbins(i,k+1) + 1
                jb               = jbins(i,k+1)
                jbinik(i,k+1,jb) = j
                fink(  i,jb,k+1) = finkhi

              endif
 45         continue
          endif

          do 38 k             = 1, irad
            if (jbins(i,k).gt.irad) then
              write(21,*)'coagsolv: jbins > irad: ',jbins(i,k),i,k
              iok = -3
              return
            endif
 38       continue

 39     continue
 40   continue

!
! ***********************************************************************
!                      initialize size distribution
!
! copy initial number concentration (#/cm3-air) and volume concentrations
!   (cm3/cm3-air) from num/vol_distrib_inp (real*4) to distrib (real*8)
! ***********************************************************************
       do i = 1, irad
         distrib(i,1) = num_distrib_inp(i)
       end do

       do jaer = 2, iaerty
         do i = 1, irad
           distrib(i,jaer) = vol_distrib_inp(i,jaer-1)
         end do
       end do


!
! *********************************************************************
!              coagulation kernel from fuchs equations
! *********************************************************************
! difcof  = brownian particle diffus coef  (cm**2 sec-1)
!         = boltg * t * bpm / (6 * pi * mu * r(i))
!         = 5.25e-04 (diam = 0.01um); 6.23e-7 (diam = 0.5 um) seinfeld p.325.
! vthermp = avg thermal vel of particle    (cm sec-1)
!         = (8 * boltg * t / (pi * masmolec)**0.5
! pmfp    = mean free path of particle     (cm)
!         = 8 * di / (pi * vthermp)
! bpm     = correction for particle shape and for effects of low mean
!           free path of air. (toon et al., 1989, physical processes in
!           polar stratospheric ice clouds, jgr 94, 11,359. f1 and f2 are
!           included in the expression below (shape effects correction)
!         = 1 + kn[a + bexp(-c/kn)]
! deltp1  = if particles with mean free path pmfp leave the surface of
!           an absorbing sphere in all directions, each being probably
!           equal, then deltp1 is the mean distance from the surface of the
!           sphere reached by particles after covering a distance pmfp. (cm).
! cbr     = coag kernel (cm3 partic-1 s-1) due to brownian motion (fuchs, 1964)
! rrate   = coag kernal (cm3 partic-1 s-1) * time step (s)
!
! *** use the wet (ambient) radius and volume here ***
       do 145 i    = 1, irad
         radiust    = radwet(i)
         tworad     = radiust  + radiust
         fourrsq    = 4.       * radiust * radiust
         aknud      = gmfp / radiust
         bpm        = 1. + aknud * (1.257 + 0.42*exp(-1.1/aknud))
         difcof(i)  = boltg * tk * bpm  / (6. * onepi * radiust * amu)

! use bin-varied aerosol density from mirage
!        vthermp(i) = sqrt(8. * boltg * tk / (onepi*denav * volume(i)))
         vthermp(i) = sqrt(8. * boltg * tk / (onepi*denav(i) *   &
                      volwet(i)))
         sumvt(i)   = vthermp(i)  * vthermp(i)
         pmfp       = 8. * difcof(i) / (onepi * vthermp(i))
         dti1       = tworad   + pmfp
         dti2       = (fourrsq + pmfp * pmfp)**1.5
         divis      = 0.166667 / (radiust * pmfp)
         deltp1     = divis    * (dti1 * dti1 * dti1 - dti2) - tworad
         sumdp(i)   = deltp1   * deltp1
 145   continue

!yy    write(kout,9165)
       do 155 i     = 1, irad
         do 154 j    = 1, irad
           radi       = radwet(i)
           radj       = radwet(j)
           rsuma      = radi  + radj
           rsumsq     = rsuma * rsuma
           ratior     = radi  / radj
           sumdc      =     difcof(i) + difcof(j)
           ggr        = sqrt(sumvt(i) + sumvt(j))
           deltr      = sqrt(sumdp(i) + sumdp(j))
           term1      = rsuma /         (rsuma + deltr)
           term2      = 4.    * sumdc / (rsuma * ggr)
           cbr        = fourpi * rsuma * sumdc / (term1 + term2)
           rrate(i,j) = cbr * deltat
!yy        write(kout,9190) (2.0e4*radius(i)), (2.0e4*radius(j)), cbr
 154     continue
 155   continue

9165  format(16x,'coagulation coefficients (cm**3 #-1 sec-1)'/   &
                 'diam1_um diam2_um brownian ')
9190  format(1pe15.7,7(1x,1pe15.7))


!
! *********************************************************************
! ****************** solve coagulation equations **********************
! *********************************************************************
!
! *********************************************************************
!                       inialize new arrays
! *********************************************************************
! distrib  = initial conc (# cm-3 for num conc.; cm3 cm-3 for volume fractions)
! cc2      = initial, final volume conc (cm3 cm-3) for all particle types.
! conc     = initial, final number conc (# cm-3) for number distribution
! volume   = volume (cm3 #-1) of particles in bin
!
!

      do i = 1, irad
!       cc2( i,1) = distrib(i,1) * volume(i)
        cc2( i,1) = distrib(i,1)
        conc(i)   = distrib(i,1)
      end do

      do jaer = 2, iaerty
        do i  = 1, irad
          cc2(i,jaer) = distrib(i,jaer)
        end do
      end do

! *********************************************************************
!             solve coagulation over several time steps
! *********************************************************************
!
      do 700 isubstep  = 1, nsubstep
!
        do 485 jaer  = 1, iaerty
          do 484 k    = 1, irad
!
! production terms
!
            prod        = 0.
            if (k.gt.1) then
              if (jaer .eq. 1) then
                do 465 i = 1, k
                  do 464 jb = 1, jbins(i,k)
                    j       = jbinik(i,k,jb)
                    prod    = prod + fink(i,jb,k)*rrate(i,j)*   &
                              cc2(j,jaer)*volume(j)*conc(i)
 464              continue
 465            continue
                prod     = prod/volume(k)
              else
                do 469 i = 1, k
                  do 468 jb = 1, jbins(i,k)
                    j       = jbinik(i,k,jb)
                    prod    = prod +   &
                             fink(i,jb,k)*rrate(i,j)*cc2(j,jaer)*conc(i)
 468              continue
 469            continue
              endif
            endif
!
! loss terms
!
            aloss       = 0.
            if (k.lt.irad) then
              do 475 j = 1, irad
                aloss  = aloss + floss(k,j) * rrate(k,j) * conc(j)
 475          continue
            endif
!
! final concentrations
!
            cc2(k,jaer) = (cc2(k,jaer) + prod) / (1. + aloss)
 484      continue
 485    continue
!
! put updated number concentration into conc array
        do i = 1, irad
          conc(i) = cc2(i,1)
        end do

 700  continue


!
! *********************************************************
! **    put the advanced concentration back on the grid  **
!       (copy them from the real*8 working array to the
!        real*4 caller arrays)
! *********************************************************
!
       do i = 1, irad
         num_distrib_inp(i) = cc2(i,1)
       end do
       do jaer = 2, iaerty
         do i = 1, irad
           vol_distrib_inp(i,jaer-1) = cc2(i,jaer)
         end do
       end do

!
! if no diagnostics, then return
!
      iok = 0
      if (idiag_onoff .le. 0) return

!
! *********************************************************************
! *************         check whether mass is conserved     ***********
! *********************************************************************
! tvolinit = initial volume conc (cm3 cm-3), summed over all bins
! tvolfin  = final   volume conc (cm3 cm-3), summed over all bins
! sumnbef = summed number conc (# cm-3) before coagulation
! sumnaft = summed number conc (# cm-3) after coagulation
!
      do jaer = 1, iaerty
        tvolinit(jaer) = 0.
        tvolfin( jaer) = 0.
      end do

      sumnbef        = 0.
      sumnaft        = 0.
!
! distrib(jaer=1, i=1:nrad) = initial total number conc in # cm-3
! cc2    (jaer=1, i=1:nrad) = final   total number conc in # cm-3
!
      do i = 1, irad
        tvolinit(1)   = tvolinit(1) + distrib(i,1) * volume(i)
        tvolfin( 1)   = tvolfin( 1) + cc2(    i,1) * volume(i)
        sumnbef       = sumnbef     + distrib(i,1)
        sumnaft       = sumnaft     + cc2(    i,1)
      end do

!
! distrib(jaer=2:iaerty, i=1:nrad) = initial component volume conc in cm3 cm-3
! cc2    (jaer=2:iaerty, i=1:nrad) = initial component volume conc in cm3 cm-3
!
      do jaer = 2, iaerty
      do i    = 1, irad
        tvolinit(jaer) = tvolinit(jaer) + distrib(i,jaer)
        tvolfin( jaer) = tvolfin( jaer) + cc2(    i,jaer)
      end do
      end do

      write(kout,*)
      write(kout,9434) sumnbef, sumnaft,   &
                      tvolinit(1)*1.0e+12,tvolfin(1)*1.0e+12

      write(kout,9435) sumnaft-sumnbef
      write(kout,9439) tvolinit(1) / tvolfin(1)

      do jaer = 2, iaerty
        if (abs(tvolfin(jaer)) .gt. 0.0) then
           write(kout,9440) jaer-1, tvolinit(jaer) / tvolfin(jaer)
        else
           write(kout,9441) jaer-1, tvolinit(jaer),  tvolfin(jaer)
        end if
      end do

9434  format('# bef, aft; vol bef, aft =',/4(1pe16.10,1x)/)
9435  format('total partic change in  # cm-3 = ',1pe17.10)
9439  format('total partic vol bef / vol aft = ',1pe17.10,   &
             ': if 1.0 -> exact vol conserv')
9440  format('vol comp',i4,' vol bef / vol aft = ',1pe17.10,   &
             ': if 1.0 -> exact vol conserv')
9441  format('vol comp',i4,' vol bef,  vol aft = ',2(1pe17.10))


!
! ************************** print results ****************************
! distrib = initial conc in # cm-3 for total particle (jaer=1, i=1,nrad)
! conc    = final   conc in # cm-3 for total particle (jaer=1, i=1,nrad)
!


!
! *********************************************************************
!                      end of program coagsolv.f
! *********************************************************************
!
      return
      end subroutine coagsolv





!-----------------------------------------------------------------------



	end module module_mosaic_coag
