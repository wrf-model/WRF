	module module_mosaic_coag1d



	use module_data_mosaic_kind
	use module_peg_util



	implicit none



	contains



!-----------------------------------------------------------------------
	subroutine mosaic_coag_1box( istat_coag,   &
	    idiagbb_in, itstep,   &
	    dtcoag_in,   &
	    temp_box, patm_box, rhoair_box, rbox,   &
	    fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam,   &
	    adrydens_box, awetdens_box,   &
	    adrydpav_box, awetdpav_box, adryqmas_box, mcoag_flag1 )
!
!   calculates aerosol particle coagulation for a single grid box
!	over timestep dtcoag_in
!
!   uses the single-type coagulation algorithm of jacobson (2002)
!
	use module_data_mosaic_main, only:  ntot_used, pi, piover6, third
	use module_data_mosaic_aero, only:  ifreq_coag
	use module_data_mosaic_asecthp, only: &
	    ai_phase, hyswptr_aer, &
	    lunerr, lunout, &
	    maxd_acomp, maxd_asize, maxd_atype, &
	    ncomp_aer, ncomp_plustracer_aer, nsize_aer, ntype_aer, &
	    massptr_aer, numptr_aer, waterptr_aer, &
	    dens_aer, dens_water_aer, &
	    dcen_sect, dhi_sect, dlo_sect, &
	    smallmassbb, &
	    volumcen_sect, volumcut_sect, volumhi_sect, volumlo_sect


!   subr arguments
	integer, intent(inout)  :: istat_coag  ! =0 if no problems
	integer, intent(in)     :: idiagbb_in  ! controls diagnostics
	integer, intent(in)     :: itstep      ! host code time step index
        integer, intent(in)     :: mcoag_flag1
	real(r8), intent(in)    :: dtcoag_in   ! time step (s)
	real(r8), intent(in)    :: temp_box    ! air temp (K)
	real(r8), intent(in)    :: patm_box    ! air pressure (atm)
	real(r8), intent(in)    :: rhoair_box    ! air density (g/cm^3)

	real(r8), intent(inout) :: rbox(ntot_used)
! rbox - holds aerosol number and component mass mixing ratios
!	units for number mr are such that (rbox*fact_apnumbmr) gives (#/g-air) 
!	units for mass   mr are such that (rbox*fact_apmassmr) gives (g/g-air) 

	real(r8), intent(in) :: fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam
! these are units conversion factors
!   the module_data_mosaic_asecthp densities are such that
!       dens_aer*fact_apdens gives (g/cm^3)
!   the module_data_mosaic_asecthp diameters are such that
!       d[lo,cen,hi]_sect*fact_apdiam gives (cm)
!   the module_data_mosaic_asecthp volumes are such that
!       volum[lo,cen,hi]_sect**fact_apdiam**3) gives (cm^3)

	real(r8), intent(inout) :: adrydens_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: awetdens_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: adrydpav_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: awetdpav_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: adryqmas_box(maxd_asize,maxd_atype)
! adryqmas_box = aerosol total-dry-mass mixing ratio (g/mol-air)
! adrydens_box = aerosol dry density (units same as dens_aer)
! awetdens_box = aerosol wet density (units same as dens_aer)
! adrydpav_box = aerosol mean dry diameter (units same as dlo_sect)
! awetdpav_box = aerosol mean wet diameter (units same as dlo_sect)
! adryqmas_box = aerosol total-dry-mass mixing ratio (units same as rbox)


!   local variables
	integer, parameter :: coag_method = 1
	integer, parameter :: ncomp_coag_maxd = maxd_acomp + 3

	integer :: l, ll, lla, llb, llx
	integer :: isize, itype, iphase
	integer :: iconform_numb
	integer :: idiagbb, idiag_coag_onoff
	integer :: lunout_coag
	integer :: ncomp_coag, nsize, nsubstep_coag, ntau_coag
	integer,save :: ncountaa(10), ncountbb(0:ncomp_coag_maxd)

	real(r8), parameter :: factsafe = 1.00001

! units for local working varibles:
!    size = cm
!    density = g/cm^3
!    number mixing ratio = #/g-air
!       EXCEPT num_distrib = #/cm^3-air
!    mass   mixing ratio = g/g-air
!    volume mixing ratio = cm^3/g-air
!    
	real(r8) :: densdefault, dtcoag
	real(r8) :: fact_apdia3, fact_apvolumr
	real(r8) :: press_cgs   ! air pressure (dyne/cm2)
	real(r8) :: tmpa, tmpb
	real(r8) :: wwdens, wwdpav, wwmass, wwvolu
	real(r8) :: xxdens, xxdpav, xxmass, xxnumb, xxvolu
	real(r8) :: xxmasswet, xxvoluwet

	real(r8) :: adryqvol_box(maxd_asize,maxd_atype) ! aerosol total-dry-volume mixing ratio (cm3/mol-air)
	real(r8) :: dpdry(maxd_asize), dpwet(maxd_asize), denswet(maxd_asize)
	real(r8) :: num_distrib(maxd_asize)
	real(r8) :: sumnew(ncomp_coag_maxd), sumold(ncomp_coag_maxd)
	real(r8) :: sum_num(2), sum_vol(0:ncomp_coag_maxd,2)
	real(r8) :: vol_distrib(maxd_asize,ncomp_coag_maxd)
	real(r8) :: vpcut(0:maxd_asize), vpdry(maxd_asize)
	real(r8) :: xxvolubb(maxd_asize)

	character(len=120) :: msg



	if (mcoag_flag1 <= 0) return
	if (ifreq_coag > 1) then
	   if (mod(itstep,ifreq_coag) /= 0) return
	   dtcoag = dtcoag_in*ifreq_coag
	else
	   dtcoag = dtcoag_in
	end if

	istat_coag = 0

	lunout_coag = 6
	if (lunout .gt. 0) lunout_coag = lunout


!   set variables that do not change
	idiagbb = idiagbb_in
	idiag_coag_onoff = +1
	if (idiagbb <= 0) idiag_coag_onoff = 0

	iphase = ai_phase
	fact_apdia3 = fact_apdiam*fact_apdiam*fact_apdiam
	fact_apvolumr = fact_apmassmr/fact_apdens

	nsubstep_coag = 1
        press_cgs = patm_box*1.01325e6_r8


!   temporary diagnostics
	ncountaa(:) = 0
	ncountbb(:) = 0


	do 2800 itype = 1, ntype_aer
!	do 2800 itype = 1, 1

	nsize = nsize_aer(itype)
	ncomp_coag = ncomp_plustracer_aer(itype) + 3

	densdefault = dens_aer(1,itype)*fact_apdens
	vpcut(0:nsize) = volumcut_sect(0:nsize,itype)*fact_apdia3

	ncountaa(1) = ncountaa(1) + 1

!   do initial calculation of dry mass, volume, and density,
!   and initial number conforming (as needed)
	vol_distrib(:,:) = 0.0
	sumold(:) = 0.0

isize_loop_aa: &
	do isize = 1, nsize
	    l = numptr_aer(isize,itype,iphase)
	    xxnumb = rbox(l)*fact_apnumbmr
	    xxmass = adryqmas_box(isize,itype)*fact_apmassmr
	    xxdens = adrydens_box(isize,itype)*fact_apdens
	    iconform_numb = 1

	    if ( (xxdens .lt. 0.1) .or. (xxdens .gt. 20.0) ) then
!   (exception) case of drydensity not valid
!   so compute dry mass and volume from rbox
		ncountaa(2) = ncountaa(2) + 1
		xxvolu = 0.0
		if (idiagbb .ge. 200)   &
		    write(lunout_coag,'(a,i10,2i4,1p,4e10.2)') 'coagaa-2a',   &
		    itstep, itype, isize, xxmass, xxvolu, xxdens
		xxmass = 0.0
		do ll = 1, ncomp_aer(itype)
		    l = massptr_aer(ll,isize,itype,iphase)
		    if (l .ge. 1) then
			tmpa = max( 0.0_r8, rbox(l) )
			xxmass = xxmass + tmpa
			xxvolu = xxvolu + tmpa/dens_aer(ll,itype)
		    end if
		end do
		xxmass = xxmass*fact_apmassmr
		xxvolu = xxvolu*fact_apvolumr
		if (xxmass .gt. 0.99*smallmassbb) then
		    xxdens = xxmass/xxvolu
		    xxvolu = xxmass/xxdens
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,i10,2i4,1p,4e10.2)') 'coagaa-2b',   &
			itstep, itype, isize, xxmass, xxvolu, xxdens
		end if
	    else
		xxvolu = xxmass/xxdens
	    end if

	    if (xxmass .le. 1.01*smallmassbb) then
!   when drymass extremely small, use default density and bin center size,
!   and zero out water
		ncountaa(3) = ncountaa(3) + 1
		xxdens = densdefault
		xxvolu = xxmass/xxdens
		xxnumb = xxmass/(volumcen_sect(isize,itype)*fact_apdia3*xxdens)
		l = hyswptr_aer(isize,itype)
		if (l .ge. 1) rbox(l) = 0.0
		l = waterptr_aer(isize,itype)
		if (l .ge. 1) rbox(l) = 0.0
		iconform_numb = 0
		if (idiagbb .ge. 200)   &
		    write(lunout_coag,'(a,i10,2i4,1p,4e10.2)') 'coagaa-2c',   &
		    itstep, itype, isize, xxmass, xxvolu, xxdens
	    else
		xxvolu = xxmass/xxdens
	    end if

!   check for mean dry-size 'safely' within bounds, and conform number if not
	    if (iconform_numb .gt. 0) then
		if (xxnumb .gt.   &
			xxvolu/(factsafe*volumlo_sect(isize,itype)*fact_apdia3)) then
		    ncountaa(4) = ncountaa(4) + 1
		    tmpa = xxnumb
		    xxnumb = xxvolu/(factsafe*volumlo_sect(isize,itype)*fact_apdia3)
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,i10,2i4,1p,3e12.4)') 'coagcc-4 ',   &
			itstep, itype, isize, xxvolu, tmpa, xxnumb
		else if (xxnumb .lt.   &
			xxvolu*factsafe/(volumhi_sect(isize,itype)*fact_apdia3)) then
		    ncountaa(5) = ncountaa(5) + 1
		    tmpa = xxnumb
		    xxnumb = xxvolu*factsafe/(volumhi_sect(isize,itype)*fact_apdia3)
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,i10,2i4,1p,3e12.4)') 'coagcc-5 ',   &
			itstep, itype, isize, xxvolu, tmpa, xxnumb
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,10x, 8x,1p,4e12.4)') 'coagcc-5 ',   &
			dlo_sect(isize,itype), dlo_sect(isize,itype)*fact_apdiam, &
			volumlo_sect(isize,itype), volumlo_sect(isize,itype)*fact_apdia3
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,10x, 8x,1p,4e12.4)') 'coagcc-5 ',   &
			dhi_sect(isize,itype), dhi_sect(isize,itype)*fact_apdiam, &
			volumhi_sect(isize,itype), volumhi_sect(isize,itype)*fact_apdia3
		end if
	    end if

!   load numb, mass, volu, dens back into mosaic_asecthp arrays
	    l = numptr_aer(isize,itype,iphase)
	    rbox(l) = xxnumb/fact_apnumbmr
	    adrydens_box(isize,itype) = xxdens/fact_apdens
	    adryqmas_box(isize,itype) = xxmass/fact_apmassmr
	    adryqvol_box(isize,itype) = xxvolu/fact_apvolumr

!
!   load number/mass/volume into working arrays
!
!   *** NOTE ***
!     num_distrib must be (#/cm3)
!     vol_distrib units do not matter - they can be either masses or volumes,
!        and either mixing ratios or concentrations
!	 (e.g., g/cm3-air, ug/kg-air, cm3/cm3-air, cm3/kg-air, ...)
!
	    num_distrib(isize) = xxnumb*rhoair_box

	    do ll = 1, ncomp_plustracer_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		tmpa = 0.0
		if (l .ge. 1) tmpa = max( 0.0_r8, rbox(l) )*fact_apmassmr
		vol_distrib(isize,ll)   = tmpa
		sumold(ll) = sumold(ll) + tmpa
	    end do

	    do llx = 1, 3
		ll = (ncomp_coag-3) + llx
		tmpa = 0.0
		if (llx .eq. 1) then
		    l = hyswptr_aer(isize,itype)
		    if (l .ge. 1) tmpa = max( 0.0_r8, rbox(l) )*fact_apmassmr
		else if (llx .eq. 2) then
		    l = waterptr_aer(isize,itype)
		    if (l .ge. 1) tmpa = max( 0.0_r8, rbox(l) )*fact_apmassmr
		else
		    tmpa = max( 0.0_r8, adryqvol_box(isize,itype) )*fact_apvolumr
		end if
		vol_distrib(isize,ll)   = tmpa
		sumold(ll) = sumold(ll) + tmpa
	    end do

!   calculate dry and wet diameters and wet density
	    if (xxmass .le. 1.01*smallmassbb) then
		dpdry(isize) = dcen_sect(isize,itype)*fact_apdiam
		dpwet(isize) = dpdry(isize)
		denswet(isize) = xxdens
	    else
		dpdry(isize) = (xxvolu/(xxnumb*piover6))**third
		dpdry(isize) = max( dpdry(isize), dlo_sect(isize,itype)*fact_apdiam )
		l = waterptr_aer(isize,itype)
		if (l .ge. 1) then
		    tmpa = max( 0.0_r8, rbox(l) )*fact_apmassmr
		    xxmasswet = xxmass + tmpa
		    xxvoluwet = xxvolu + tmpa/(dens_water_aer*fact_apdens)
		    dpwet(isize) = (xxvoluwet/(xxnumb*piover6))**third
		    dpwet(isize) = min( dpwet(isize), 30.0_r8*dhi_sect(isize,itype)*fact_apdiam )
		    denswet(isize) = (xxmasswet/xxvoluwet)
		else
		    dpwet(isize) = dpdry(isize)
		    denswet(isize) = xxdens
		end if
	    end if

	    vpdry(isize) = piover6 * (dpdry(isize)**3)

	end do isize_loop_aa


!
!   make call to coagulation routine
!
	if (idiag_coag_onoff > 0) call coag_conserve_check(   &
	    nsize, maxd_asize, ncomp_coag, ncomp_coag_maxd, 1, lunout,   &
	    dpdry, num_distrib, vol_distrib, sum_num, sum_vol )

	if ((mcoag_flag1 >= 10) .and. (mcoag_flag1 <= 19)) then
	    call movingcenter_singletype_coag(   &
		idiagbb, lunout_coag, nsize, maxd_asize, ncomp_coag, ncomp_coag_maxd,   &
		temp_box, press_cgs, dtcoag, nsubstep_coag,   &
		vpcut, vpdry, dpdry, dpwet, denswet, num_distrib, vol_distrib )

	else
	    call jacobson2002_singletype_coag(   &
		nsize, maxd_asize, ncomp_coag, ncomp_coag_maxd,   &
		temp_box, press_cgs, dtcoag, nsubstep_coag,   &
		dpdry, dpwet, denswet, num_distrib, vol_distrib )

	end if

	if (idiag_coag_onoff > 0) call coag_conserve_check(   &
	    nsize, maxd_asize, ncomp_coag, ncomp_coag_maxd, 2, lunout,   &
	    dpdry, num_distrib, vol_distrib, sum_num, sum_vol )


!
!   unload number/mass/volume from working arrays
!
	sumnew(:) = 0.0
isize_loop_xx: &
	do isize = 1, nsize
	    do ll = 1, ncomp_coag
		sumnew(ll) = sumnew(ll) + max( 0.0_r8, vol_distrib(isize,ll) )
	    end do

	    l = numptr_aer(isize,itype,iphase)
	    rbox(l) = max( 0.0_r8, num_distrib(isize)/(rhoair_box*fact_apnumbmr) )

!   unload mass mixing ratios into rbox; also calculate dry mass and volume
	    xxmass = 0.0
	    xxvolu = 0.0
	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		if (l .ge. 1) then
		    tmpa = max( 0.0_r8, vol_distrib(isize,ll) )
		    rbox(l) = tmpa/fact_apmassmr
		    xxmass = xxmass + tmpa
		    xxvolu = xxvolu + tmpa/(dens_aer(ll,itype)*fact_apdens)
		end if
	    end do
	    adryqmas_box(isize,itype) = xxmass/fact_apmassmr
	    xxvolubb(isize) = xxvolu

	    ll = (ncomp_coag-3) + 1
	    l = hyswptr_aer(isize,itype)
	    if (l .ge. 1) rbox(l) = max( 0.0_r8, vol_distrib(isize,ll) )/fact_apmassmr

	    ll = (ncomp_coag-3) + 2
	    l = waterptr_aer(isize,itype)
	    if (l .ge. 1) rbox(l) = max( 0.0_r8, vol_distrib(isize,ll) )/fact_apmassmr

	    ll = (ncomp_coag-3) + 3
	    adryqvol_box(isize,itype) = max( 0.0_r8, vol_distrib(isize,ll) )/fact_apvolumr
	end do isize_loop_xx


!   check for mass and volume conservation
	do ll = 1, ncomp_coag
	    tmpa = max( sumold(ll), sumnew(ll), 1.0e-35_r8 )
	    if (abs(sumold(ll)-sumnew(ll)) .gt. 1.0e-6*tmpa) then
		ncountbb(ll) = ncountbb(ll) + 1
		ncountbb(0) = ncountbb(0) + 1
	    end if
	end do


!
!   calculate new dry density,
!   and check for new mean dry-size within bounds
!
isize_loop_yy: &
	do isize = 1, nsize

	    xxmass = adryqmas_box(isize,itype)*fact_apmassmr
	    xxvolu = adryqvol_box(isize,itype)*fact_apvolumr
	    l = numptr_aer(isize,itype,iphase)
	    xxnumb = rbox(l)*fact_apnumbmr
	    iconform_numb = 1

!   do a cautious calculation of density, using volume from coag routine
	    if (xxmass .le. smallmassbb) then
		ncountaa(8) = ncountaa(8) + 1
		xxdens = densdefault
		xxvolu = xxmass/xxdens
		xxnumb = xxmass/(volumcen_sect(isize,itype)*fact_apdia3*xxdens)
		xxdpav = dcen_sect(isize,itype)*fact_apdiam
		wwdens = xxdens
		wwdpav = xxdpav
		l = hyswptr_aer(isize,itype)
		if (l .ge. 1) rbox(l) = 0.0
		l = waterptr_aer(isize,itype)
		if (l .ge. 1) rbox(l) = 0.0
		iconform_numb = 0
		if (idiagbb .ge. 200)   &
		    write(lunout_coag,'(a,i10,2i4,1p,4e10.2)') 'coagaa-7a',   &
		    itstep, itype, isize, xxmass, xxvolu, xxdens
	    else if (xxmass .gt. 1000.0*xxvolu) then
!   in this case, density is too large.  setting density=1000 forces
!   next IF block while avoiding potential divide by zero or overflow
		xxdens = 1000.0
	    else 
		xxdens = xxmass/xxvolu
	    end if

	    if ((xxdens .lt. 0.1) .or. (xxdens .gt. 20.0)) then
!   (exception) case -- new dry density is unrealistic,
!   so use dry volume from rbox instead of that from coag routine
		ncountaa(7) = ncountaa(7) + 1
		if (idiagbb .ge. 200)   &
		    write(lunout_coag,'(a,i10,2i4,1p,4e10.2)') 'coagaa-7b',   &
		    itstep, itype, isize, xxmass, xxvolu, xxdens
		xxvolu = xxvolubb(isize)
		xxdens = xxmass/xxvolu
		if (idiagbb .ge. 200)   &
		    write(lunout_coag,'(a,18x,1p,4e10.2)') 'coagaa-7c',   &
		    xxmass, xxvolu, xxdens
	    end if

!   check for mean size ok, and conform number if not
	    if (iconform_numb .gt. 0) then
		if (xxnumb .gt. xxvolu/(volumlo_sect(isize,itype)*fact_apdia3)) then
		    ncountaa(9) = ncountaa(9) + 1
		    tmpa = xxnumb
		    xxnumb = xxvolu/(volumlo_sect(isize,itype)*fact_apdia3)
		    xxdpav = dlo_sect(isize,itype)*fact_apdiam
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,i10,2i4,1p,3e12.4)') 'coagcc-9 ',   &
			itstep, itype, isize, xxvolu, tmpa, xxnumb
		else if (xxnumb .lt. xxvolu/(volumhi_sect(isize,itype)*fact_apdia3)) then
		    ncountaa(10) = ncountaa(10) + 1
		    tmpa = xxnumb
		    xxnumb = xxvolu/(volumhi_sect(isize,itype)*fact_apdia3)
		    xxdpav = dhi_sect(isize,itype)*fact_apdiam
		    if (idiagbb .ge. 200)   &
			write(lunout_coag,'(a,i10,2i4,1p,3e12.4)') 'coagcc-10',   &
			itstep, itype, isize, xxvolu, tmpa, xxnumb
		else
		    xxdpav = (xxvolu/(xxnumb*piover6))**third
		end if

		tmpb = 0.0
		l = waterptr_aer(isize,itype)
		if (l .ge. 1) tmpb = max(0.0_r8,rbox(l))*fact_apmassmr
		wwmass = xxmass + tmpb
		wwvolu = xxvolu + tmpb/(dens_water_aer*fact_apdens)
		wwdens = wwmass/wwvolu
		wwdpav = xxdpav*((wwvolu/xxvolu)**third)
	    end if

!   load number, mass, volume, dry-density back into arrays
	    l = numptr_aer(isize,itype,iphase)
	    rbox(l) = xxnumb/fact_apnumbmr
	    adrydens_box(isize,itype) = xxdens/fact_apdens
	    adrydpav_box(isize,itype) = xxdpav/fact_apdiam
	    adryqmas_box(isize,itype) = xxmass/fact_apmassmr
	    adryqvol_box(isize,itype) = xxvolu/fact_apvolumr
	    awetdens_box(isize,itype) = wwdens/fact_apdens
	    awetdpav_box(isize,itype) = wwdpav/fact_apdiam

	end do isize_loop_yy


!   temporary diagnostics
	if (idiagbb .ge. 100) then
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


2800	continue	! itype = 1, ntype_aer


	return
	end subroutine mosaic_coag_1box


!----------------------------------------------------------------------
      subroutine coag_conserve_check(   &
        nbin, nbin_maxd, ncomp, ncomp_maxd, iflagaa, lunout,   &
        dpdry, num_distrib, vol_distrib, sum_num, sum_vol )

      implicit none
!
! checks on conservation of volume before/after coag routine
!
      integer, intent(in) :: nbin            ! actual number of size bins
      integer, intent(in) :: nbin_maxd       ! size-bin dimension for input (argument) arrays
      integer, intent(in) :: ncomp           ! actual number of aerosol volume components
      integer, intent(in) :: ncomp_maxd      ! volume-component dimension for input (argument) arrays
      integer, intent(in) :: lunout          ! logical unit for warning & diagnostic output
      integer, intent(in) :: iflagaa         !

      real(r8), intent(in) :: dpdry(nbin_maxd)
      real(r8), intent(in) :: num_distrib(nbin_maxd)
      real(r8), intent(in) :: vol_distrib(nbin_maxd,ncomp_maxd)
      real(r8), intent(inout) :: sum_num(2)
      real(r8), intent(inout) :: sum_vol(0:ncomp_maxd,2)

! local variables
      integer :: i, l
      real(r8), parameter :: pi = 3.14159265358979323846_r8
      real(r8) :: voldry(nbin)


      if (iflagaa == 1) then
         i = 1
      else if (iflagaa == 2) then
         i = 2
      else
         return
      end if


! sum initial or number and volumes
      voldry(1:nbin) = (pi/6.0)*(dpdry(1:nbin)**3)
      sum_num(i) = sum( num_distrib(1:nbin) )
      sum_vol(0,i) = sum( num_distrib(1:nbin)*voldry(1:nbin) )
      do l = 1, ncomp
         sum_vol(l,i) = sum( vol_distrib(1:nbin,l) )
      end do

! write diagnostics
      if (iflagaa /= 2) return
      write(lunout,'(a)')
      write(lunout,'(2a,1p,2e14.6)')   &
               'number total ', '    initial, final   =',   &
               sum_num(1:2)
      write(lunout,'(2a,1p,2e14.6)')   &
               'number total ', '   (final/initial)   =',   &
               sum_num(2)/sum_num(1)
      write(lunout,'(2a,1p,2e14.6)')   &
               'volume total ', '    initial, final   =',   &
               sum_vol(0,1:2)
      write(lunout,'(2a,1p,2e14.6)')   &
               'volume total ', '   (initial/final)-1 =',   &
               (sum_vol(0,1)/sum_vol(0,2))-1.0

      do l = 1, ncomp
         if (abs(sum_vol(l,2)) > 0.0) then
            write(lunout,'(a,i4,a,1p,2e14.6)')   &
               'volume l=',  l, '   (initial/final)-1 =',   &
               (sum_vol(l,1)/sum_vol(l,2))-1.0
         else
            write(lunout,'(a,i4,a,1p,2e14.6)')   &
               'volume l=',  l, '    initial, final   =',   &
               sum_vol(l,1), sum_vol(l,2)
         end if
      end do

      return
      end subroutine coag_conserve_check



!----------------------------------------------------------------------
      subroutine movingcenter_singletype_coag(   &
        idiagbb, lunout, nbin, nbin_maxd, ncomp, ncomp_maxd,   &
        tempk, press, deltat, nsubstep_in,   &
        vpcut, vpdry_in, dpdry, dpwet, denswet,   &
        num_distrib, vol_distrib )

      implicit none
!
! calculates aerosol coagulation for sectional representation
!    and a single "particle type" using the single-type algorithm of
!    jacobson (2002)
!
! reference
!    jacobson, m. z., 2002, analysis of aerosol interactions with numerical
!       techniques for solving coagulation, nucleation, condensation,
!       dissolution, and reversible chemistry among multiple size
!       distributions, j. geophys. res., 107, d19, 4366
!

!   IN arguments
      integer, intent(in) :: idiagbb
      integer, intent(in) :: lunout
      integer, intent(in) :: nbin            ! actual number of size bins
      integer, intent(in) :: nbin_maxd       ! size-bin dimension for input (argument) arrays
      integer, intent(in) :: ncomp           ! actual number of aerosol volume components
      integer, intent(in) :: ncomp_maxd      ! volume-component dimension for input (argument) arrays
      integer, intent(in) :: nsubstep_in     ! number of time sub-steps for the integration

      real(r8), intent(in) :: tempk               ! air temperature (K)
      real(r8), intent(in) :: press               ! air pressure (dyne/cm2)
      real(r8), intent(in) :: deltat              ! integration time (s)
      real(r8), intent(in) :: dpdry(nbin_maxd)    ! bin current volume-mean dry diameter (cm)
      real(r8), intent(in) :: dpwet(nbin_maxd)    ! bin wet (ambient) diameter (cm)
      real(r8), intent(in) :: denswet(nbin_maxd)  ! bin wet (ambient) density (g/cm3)
      real(r8), intent(in) :: vpdry_in(nbin_maxd) ! bin current mean 1-particle dry volume (cm^3)
      real(r8), intent(in) :: vpcut(0:nbin_maxd)  ! 1-particle dry volumes at bin boundaries (cm^3)

!   INOUT arguments
      real(r8), intent(inout) :: num_distrib(nbin_maxd)
!          num_distrib(i)   = number concentration for bin i (#/cm3)
      real(r8), intent(inout) :: vol_distrib(nbin_maxd,ncomp_maxd)
!          vol_distrib(i,l) = volume concentration for bin i, component l (cm3/cm3)

!
! NOTE -- The sectional representation is based on dry particle size.
! Dry sizes are used to calculate the receiving bin indices and product fractions
!   for each (bin-i1, bin-i2) coagulation combination.
! Wet sizes and densities are used to calculate the brownian coagulation kernel.
!
! NOTE -- Units for num_distrib and vol_distrib
!    num_distrib units MUST BE (#/cm3)
!    vol_distrib 
!        (cm3/cm3) units are most consistent with the num_distrib units
!        However, the algorithm works with almost any units!  So vol_distrib can 
!        be either masses or volumes, and either mixing ratios or concentrations
!        (e.g., g/cm3-air, ug/kg-air, um3/cm3-air, cm3/kg-air, ...).
!        Use whatever is convenient.
!

!
!   local variables
!
      integer :: i, isubstep, j, k, l, nsubstep

      real(r8), parameter :: frac_loss_limit = 0.5_r8
      real(r8), parameter :: pi = 3.14159265358979323846_r8

      real(r8) ::   &
             del_num, del_voli, del_volj, deltat_sub,   &
             tmpa, tmpb, tmp_beta,   &
             vpdry_ipj

      real(r8) ::   &
             betaxh(nbin,nbin),   &
             cnum(nbin), cnum_old(nbin),   &
             cvol(nbin,ncomp), cvol_old(nbin,ncomp),   &
             vpdry(nbin)


!
! calculate brownian coagulation kernel 
!
      call brownian_kernel(   &
         nbin, tempk, press, dpwet, denswet, betaxh )

!
! check that fractional number loss from any bin over deltat_sub does
! not exceed frac_loss_limit
!
      nsubstep = nsubstep_in
      deltat_sub = deltat/nsubstep   ! time substep (s)
      tmpa = 0.0
      do j = 1, nbin
         tmpb = 0.0
         do i = 1, nbin
            tmpb = tmpb + betaxh(i,j)*num_distrib(i)
         end do
         tmpb = tmpb - 0.5*betaxh(j,j)*num_distrib(j)
         tmpa = max( tmpa, tmpb )
      end do
      if (idiagbb > 0) write(lunout,'(/a,i10,1p,4e12.4)') 'coag aa nsub, dtsub, fracloss', &
            nsubstep, deltat_sub, tmpa*deltat_sub, frac_loss_limit
      if (tmpa*deltat_sub > frac_loss_limit) then
         tmpb = tmpa*deltat/frac_loss_limit
         isubstep = int(tmpb) + 1
         tmpb = deltat/isubstep
         if (idiagbb > 0) write(lunout,'( a,i10,1p,4e12.4)') 'coag bb nsub, dtsub, fracloss', &
            isubstep, tmpb, tmpa*tmpb
         nsubstep = isubstep
         deltat_sub = deltat/nsubstep
      end if

! multiply coag kernel by deltat_sub
!     write(92,'(a)')   &
!        '   dpdry_i (um)    dpdry_j (um)    coag coef (cm3/#/s)'
      do j = 1, nbin
      do i = 1, nbin
!        write(92,'(1p,3e16.7)')   &
!           (1.0e4*dpwet(i)), (1.0e4*dpwet(j)), betaxh(i,j)
         betaxh(i,j) = betaxh(i,j) * deltat_sub
      end do
      end do


! initialize working arrays
!   cnum(i)   = current number conc (#/cm3)   for bin i
!   cvol(i,l) = current volume conc (cm3/cm3) for bin i, species l
      cnum(1:nbin) = num_distrib(1:nbin)
      cvol(1:nbin,1:ncomp) = vol_distrib(1:nbin,1:ncomp)

! 
! solve coagulation over multiple time substeps
!
solve_isubstep_loop:   &
      do isubstep = 1, nsubstep
!        write(91,*) 'maa =', 2

         cnum_old(1:nbin) = cnum(1:nbin) 
         cvol_old(1:nbin,1:ncomp) = cvol(1:nbin,1:ncomp)
         if (isubstep == 1) then
             vpdry(1:nbin) = vpdry_in(1:nbin)
!        else
! ***
! *** should be updating vpdry(:) after first substep
! ***
         end if

         do j = 1, nbin
         do i = 1, j

            ! determine the bin that receives the "i+j" particles
            vpdry_ipj = vpdry(i) + vpdry(j)
            k = max( i, j )
            do while (k > -99)
               if (vpdry_ipj > vpcut(k)) then
                  k = k+1
                  if (k >= nbin) exit   ! do while ...
               else if (vpdry_ipj < vpcut(k-1)) then
                  k = k-1
                  if (k <= 1) exit   ! do while ...
               else
                  exit   ! do while ...
               end if
            end do   ! while ...
            k = max( 1, min( nbin, k ) )
!           write(183,'(3i4,1p,5e10.2)') i, j, k, vpdry(i), vpdry(j), vpdry_ipj
!           write(183,'(12x,1p,5e10.2)') vpcut(i), vpcut(j), vpcut(k-1:k)

            tmp_beta = betaxh(i,j)

            ! now calc the coagulation changes to number and volume 
            if (i == j) then
               del_num = 0.5*tmp_beta*cnum_old(i)*cnum_old(i)
               if (k == i) then
                  ! here i + i --> i
                  ! each coag event consumes 2 and produces 1 "i" particle
                  cnum(i) = cnum(i) - del_num
                  cycle
               else
                  ! here i + i --> k /= i
                  ! each coag event consumes 2 "i" and produces 1 "k" particle
                  ! and there is transfer of mass" and produces 1 "k" particle
                  cnum(i) = cnum(i) - del_num*2.0
                  cnum(k) = cnum(k) + del_num
                  do l = 1, ncomp
                     del_voli = tmp_beta*cnum_old(i)*cvol_old(i,l)
                     cvol(i,l) = cvol(i,l) - del_voli
                     cvol(k,l) = cvol(k,l) + del_voli
                  end do
               end if

            else   ! here i < j
               del_num = tmp_beta*cnum_old(i)*cnum_old(j)
               cnum(i) = cnum(i) - del_num
               if (k == j) then
                  ! here i and j are different but j = k
                  ! i loses number and transfers mass to k=j
                  ! j=k does not lose any of its own number or mass
                  do l = 1, ncomp
                     del_voli = tmp_beta*cnum_old(j)*cvol_old(i,l)
                     cvol(i,l) = cvol(i,l) - del_voli
                     cvol(k,l) = cvol(k,l) + del_voli
                  end do
               else
                  ! here i, j, and k are all different
                  ! both i and j lose number and transfer mass to k
                  cnum(j) = cnum(j) - del_num
                  cnum(k) = cnum(k) + del_num
                  do l = 1, ncomp
                     del_voli = tmp_beta*cnum_old(j)*cvol_old(i,l)
                     del_volj = tmp_beta*cnum_old(i)*cvol_old(j,l)
                     cvol(i,l) = cvol(i,l) - del_voli
                     cvol(j,l) = cvol(j,l) - del_volj
                     cvol(k,l) = cvol(k,l) + del_voli + del_volj
                  end do
               end if

            end if

         end do   ! i
         end do   ! j

      end do solve_isubstep_loop


! transfer new concentrations from working arrays
      num_distrib(1:nbin) = cnum(1:nbin)
      do l = 1, ncomp
         vol_distrib(1:nbin,l) = cvol(1:nbin,l)
      end do


! all done
      return
      end subroutine movingcenter_singletype_coag



!----------------------------------------------------------------------
      subroutine jacobson2002_singletype_coag(   &
        nbin, nbin_maxd, ncomp, ncomp_maxd,   &
        tempk, press, deltat, nsubstep,   &
        dpdry, dpwet, denswet,   &
        num_distrib, vol_distrib )

      implicit none
!
! calculates aerosol coagulation for sectional representation
!    and a single "particle type" using the single-type algorithm of
!    jacobson (2002)
!
! reference
!    jacobson, m. z., 2002, analysis of aerosol interactions with numerical
!       techniques for solving coagulation, nucleation, condensation,
!       dissolution, and reversible chemistry among multiple size
!       distributions, j. geophys. res., 107, d19, 4366
!

!   IN arguments
      integer, intent(in) :: nbin            ! actual number of size bins
      integer, intent(in) :: nbin_maxd       ! size-bin dimension for input (argument) arrays
      integer, intent(in) :: ncomp           ! actual number of aerosol volume components
      integer, intent(in) :: ncomp_maxd      ! volume-component dimension for input (argument) arrays
      integer, intent(in) :: nsubstep        ! number of time sub-steps for the integration

      real(r8), intent(in) :: tempk               ! air temperature (K)
      real(r8), intent(in) :: press               ! air pressure (dyne/cm2)
      real(r8), intent(in) :: deltat              ! integration time (s)
      real(r8), intent(in) :: dpdry(nbin_maxd)    ! bin dry diameter (cm)
      real(r8), intent(in) :: dpwet(nbin_maxd)    ! bin wet (ambient) diameter (cm)
      real(r8), intent(in) :: denswet(nbin_maxd)  ! bin wet (ambient) density (g/cm3)

!   INOUT arguments
      real(r8), intent(inout) :: num_distrib(nbin_maxd)
!          num_distrib(i)   = number concentration for bin i (#/cm3)
      real(r8), intent(inout) :: vol_distrib(nbin_maxd,ncomp_maxd)
!          vol_distrib(i,l) = volume concentration for bin i, component l (cm3/cm3)

!
! NOTE -- The sectional representation is based on dry particle size.
! Dry sizes are used to calculate the receiving bin indices and product fractions
!   for each (bin-i1, bin-i2) coagulation combination.
! Wet sizes and densities are used to calculate the brownian coagulation kernel.
!
! NOTE -- Units for num_distrib and vol_distrib
!    num_distrib units MUST BE (#/cm3)
!    vol_distrib 
!        (cm3/cm3) units are most consistent with the num_distrib units
!        However, the algorithm works with almost any units!  So vol_distrib can 
!        be either masses or volumes, and either mixing ratios or concentrations
!        (e.g., g/cm3-air, ug/kg-air, um3/cm3-air, cm3/kg-air, ...).
!        Use whatever is convenient.
!

!
!   local variables
!
      integer :: i, isubstep, j, k, kp1, l

      integer ::   &
             ilo_of_jk(nbin,nbin),   &
             ihi_of_jk(nbin,nbin),   &
             klo_of_ij(nbin,nbin),   &
             khi_of_ij(nbin,nbin)

      real(r8), parameter :: pi = 3.14159265358979323846_r8

      real(r8) ::   &
             deltat_sub,   &
             fijk_tmp,   &
             t1xh_num, t3xh_num,   &
             tmpa,   &
             voldry_ipj

      real(r8) ::   &
             betaxh(nbin,nbin),   &
             cvol(nbin,ncomp),   &
             cnum(nbin), cnum_old(nbin),   &
             fijk_ieqk(nbin,nbin),   &
             fijk_keqklo(nbin,nbin),   &
             fijk_keqkhi(nbin,nbin),   &
             t1xh_vol(ncomp), t3xh_vol(ncomp),   &
             voldry(nbin)


!
! calculate brownian coagulation kernel 
!
      call brownian_kernel(   &
         nbin, tempk, press, dpwet, denswet, betaxh )

!     write(92,'(a)')   &
!        '   dpdry_i (um)    dpdry_j (um)    coag coef (cm3/#/s)'
      deltat_sub = deltat/nsubstep   ! time substep (s)
      do i = 1, nbin
      do j = 1, nbin
!        write(92,'(1p,3e16.7)')   &
!           (1.0e4*dpwet(i)), (1.0e4*dpwet(j)), betaxh(i,j)
         betaxh(i,j) = betaxh(i,j) * deltat_sub
      end do
      end do


!
! calculate the fijk of eqn 8
!
! for each (i,j) pair, there are at most two k values for which fijk > 0
! rather than calculating (and using) the full (3d) fijk array,
!    only calculate the non-zero portions
!
! klo_of_ij(i,j) is the lowest  k for which i + j --> k
! khi_of_ij(i,j) is the highest k for which i + j --> k
!    for most (i,j), khi_of_ij = klo_of_ij + 1
! fijk_keqklo(i,j) = fijk with k=klo_of_ij(i,j)
! fijk_keqkhi(i,j) = fijk with k=khi_of_ij(i,j)
! fijk_ieqk(i,j) = fijk with k=i
!
      fijk_ieqk(:,:) = 0.0
      fijk_keqklo(:,:) = 0.0
      fijk_keqkhi(:,:) = 0.0
      klo_of_ij(:,:) = 0
      khi_of_ij(:,:) = 0

      do i = 1, nbin
         voldry(i) = (pi/6.0)*(dpdry(i)**3)
      end do

      do i = 1, nbin
      do j = 1, nbin
         voldry_ipj = voldry(i) + voldry(j)
         if (voldry_ipj >= voldry(nbin)) then
            if (i == nbin) fijk_ieqk(i,j) = 1.0
            klo_of_ij(i,j) = nbin
            fijk_keqklo(i,j) = 1.0
         else
            do k = 1, nbin - 1
               kp1 = k+1
               if ((voldry_ipj >= voldry(k)) .and. (voldry_ipj < voldry(kp1))) then

                  fijk_tmp = ((voldry(kp1) - voldry_ipj)/(voldry(kp1) - voldry(k)))   &
                              * voldry(k) / voldry_ipj
                  if (i == k) fijk_ieqk(i,j) = fijk_tmp
                  klo_of_ij(i,j) = k
                  fijk_keqklo(i,j) = fijk_tmp
                  khi_of_ij(i,j) = kp1
                  fijk_keqkhi(i,j) = 1.0 - fijk_tmp
               endif
            end do
          endif
      end do   ! j
      end do   ! i

! for each j and k, determine the lowest and highest i
!    for which i + j --> k is possible
      do k = 1, nbin
      do j = 1, k
         ilo_of_jk(j,k) = nbin+1
         ihi_of_jk(j,k) = 0
         do i = 1, k-1
            if ((klo_of_ij(i,j) == k) .or. (khi_of_ij(i,j) == k)) then
               ilo_of_jk(j,k) = min( ilo_of_jk(j,k), i )
               ihi_of_jk(j,k) = max( ihi_of_jk(j,k), i )
            end if
         end do
      end do   ! j
      end do   ! k


! initialize working arrays
!   cnum(i)   = current number conc (#/cm3)   for bin i
!   cvol(i,l) = current volume conc (cm3/cm3) for bin i, species l
      cnum(1:nbin) = num_distrib(1:nbin)
      do l = 1, ncomp
         cvol(1:nbin,l) = vol_distrib(1:nbin,l)
      end do

! 
! solve coagulation over multiple time substeps
!
solve_isubstep_loop:   &
      do isubstep = 1, nsubstep
!        write(91,*) 'maa =', 2

         cnum_old(1:nbin) = cnum(1:nbin) 

solve_k_loop:   &
         do k = 1, nbin
! T3*h of eqns 7 and 9
            t3xh_num = 0.
            if (k < nbin) then
               do j = 1, nbin
                  t3xh_num = t3xh_num + (1.0-fijk_ieqk(k,j))*betaxh(k,j)*cnum_old(j)
               end do
            endif
            t3xh_vol(:) = t3xh_num

! T1*h of eqn 7 (for number) and eqn 9 (for volume)
            t1xh_num = 0.
            t1xh_vol(:) = 0.
            if (k > 1) then
               do j = 1, k
               do i = ilo_of_jk(j,k), ihi_of_jk(j,k)
                  if (klo_of_ij(i,j) == k) then
                     fijk_tmp = fijk_keqklo(i,j)
                  else if (khi_of_ij(i,j) == k) then
                     fijk_tmp = fijk_keqkhi(i,j)
                  else
                     cycle
                  end if
                  tmpa = fijk_tmp*betaxh(j,i)*cnum_old(j)
                  t1xh_num = t1xh_num + tmpa*cnum(i)*voldry(i)
!                 write(91,'(a,3i4,1p,2e12.4)')   &
!                       'num source', i, j, k, fijk_tmp, t1xh_num
                  do l = 1, ncomp
                     t1xh_vol(l) = t1xh_vol(l) + tmpa*cvol(i,l)
!                    write(91,'(a,4i4,1p,2e12.4)')   &
!                       'vol so', l, i, j, k, fijk_tmp, t1xh_vol(l)
                  end do
               end do
               end do
               t1xh_num = t1xh_num/voldry(k)
            endif

! new concentrations
            cnum(k) = (cnum(k) + t1xh_num) / (1.0 + t3xh_num)
            cvol(k,:) = (cvol(k,:) + t1xh_vol(:))/(1.0 + t3xh_vol(:))

         end do solve_k_loop

      end do solve_isubstep_loop


! transfer new concentrations from working arrays
      num_distrib(1:nbin) = cnum(1:nbin)
      do l = 1, ncomp
         vol_distrib(1:nbin,l) = cvol(1:nbin,l)
      end do


! all done
      return
      end subroutine jacobson2002_singletype_coag



!-----------------------------------------------------------------------
      subroutine brownian_kernel(   &
        nbin, tempk, press, dpwet, denswet, bckernel )
!
! this routine calculates brownian coagulation kernel
! using on eqn 16.28 of   
!    jacobson,  m. z. (1999) fundamentals of atmospheric modeling.
!       cambridge university press, new york, 656 pp.
!
      implicit none

! arguments
      integer, intent(in) :: nbin            ! actual number of size bins

      real(r8), intent(in)  :: tempk            ! air temperature (K)
      real(r8), intent(in)  :: press            ! air pressure (dyne/cm2)
      real(r8), intent(in)  :: dpwet(nbin)      ! bin wet (ambient) diameter (cm)
      real(r8), intent(in)  :: denswet(nbin)    ! bin wet (ambient) density (g/cm3)
      real(r8), intent(out) :: bckernel(nbin,nbin)  ! brownian coag kernel (cm3/#/s)

! local variables
      integer i, j

      real(r8), parameter :: pi = 3.14159265358979323846_r8

      real(r8)   &
             apfreepath, avogad,   &
             boltz,   &
             cunning,   &
             deltasq_i, deltasq_j,   &
             diffus_i, diffus_j, diffus_ipj,   &
             dpwet_i, dpwet_j, dpwet_ipj,   &
             gasfreepathx2, gasspeed,   &
             knud,   &
             mass_i, mass_j, mwair,   &
             pi6,   &
             rgas, rhoair,   &
             speedsq_i, speedsq_j,   &
             tmp1,   &
             viscosd, viscosk

! boltz   = boltzmann's constant (erg/K = g*cm2/s/K)
! avogad  = avogadro's number (molecules/mol)
! mwair   = molecular weight of air (g/mol)
! rgas    = gas constant ((dyne/cm2)/(mol/cm3)/K)
! rhoair  = air density (g/cm3)
! viscosd = air dynamic viscosity (g/cm/s)
! viscosk = air kinematic viscosity (cm2/s)
! gasspeed      = air molecule mean thermal velocity (cm/s)
! gasfreepathx2 = air molecule mean free path (cm) x 2
      pi6 = pi/6.0

      boltz = 1.38065e-16
      avogad = 6.02214e+23
      mwair = 28.966
      rgas = 8.31447e7

      rhoair = press*mwair/(rgas*tempk)

      viscosd = 1.8325e-04*(416.16/(tempk+120.0)) * (tempk/296.16)**1.5
      viscosk = viscosd/rhoair
      gasspeed = sqrt(8.0*boltz*tempk*avogad/(pi*mwair))
      gasfreepathx2 = 4.0*viscosk/gasspeed

! coagulation kernel from eqn 16.28 of jacobson (1999) text
!
! diffus_i/j  = particle brownian diffusion coefficient  (cm2/s)
! speedsq_i/j = square of particle mean thermal velocity (cm/s)
! apfreepath  = particle mean free path (cm)
! cunning     = cunningham slip-flow correction factor
! deltasq_i/j = square of "delta_i" in eqn 16.29
       do i = 1, nbin

          dpwet_i = dpwet(i)                    ! particle wet diameter (cm)
          mass_i = pi6*denswet(i)*(dpwet_i**3)  ! particle wet mass (g)
          knud = gasfreepathx2/dpwet_i  
          cunning = 1.0 + knud*(1.249 + 0.42*exp(-0.87/knud))
          diffus_i = boltz*tempk*cunning/(3.0*pi*dpwet_i*viscosd)
          speedsq_i = 8.0*boltz*tempk/(pi*mass_i)
          apfreepath = 8.0*diffus_i/(pi*sqrt(speedsq_i))
          tmp1 = (dpwet_i + apfreepath)**3   &
               - (dpwet_i*dpwet_i + apfreepath*apfreepath)**1.5
          deltasq_i = ( tmp1/(3.0*dpwet_i*apfreepath) - dpwet_i )**2

          do j = 1, nbin

             if (j >= i) then

             dpwet_j = dpwet(j)                    ! particle wet diameter (cm)
             mass_j = pi6*denswet(j)*(dpwet_j**3)  ! particle wet mass (g)
             knud = gasfreepathx2/dpwet_j  
             cunning = 1.0 + knud*(1.249 + 0.42*exp(-0.87/knud))
             diffus_j = boltz*tempk*cunning/(3.0*pi*dpwet_j*viscosd)
             speedsq_j = 8.0*boltz*tempk/(pi*mass_j)
             apfreepath = 8.0*diffus_j/(pi*sqrt(speedsq_j))
             tmp1 = (dpwet_j + apfreepath)**3   &
                  - (dpwet_j*dpwet_j + apfreepath*apfreepath)**1.5
             deltasq_j = ( tmp1/(3.0*dpwet_j*apfreepath) - dpwet_j )**2

             dpwet_ipj = dpwet_i + dpwet_j
             diffus_ipj = diffus_i + diffus_j 
             tmp1 = (dpwet_ipj/(dpwet_ipj + 2.0*sqrt(deltasq_i + deltasq_j)))   &
                  + (8.0*diffus_ipj/(dpwet_ipj*sqrt(speedsq_i + speedsq_j)))
             bckernel(i,j) = 2.0*pi*dpwet_ipj*diffus_ipj/tmp1

           else
             bckernel(i,j) = bckernel(j,i) 
           end if

          end do   ! j
       end do   ! i

       return
       end subroutine brownian_kernel



!-----------------------------------------------------------------------



	end module module_mosaic_coag1d
