	module module_mosaic_newnucb

!-----------------------------------------------------------------------
! 23-may-2008 - created
!	subr mosaic_newnuc_1box & wexler_nuc_mosaic_1box taken
!	    from wrfchem (amt version) module_mosaic_newnuc.F
!	subr subr mer07_veh02_nuc_mosaic_1box, binary_nuc_vehk2002,
!	    & ternary_nuc_merik2007 taken from cam3 (modal_aero version)
!	    modal_aero_newnuc.F90
!       mosaic_newnuc_1box required significant changes
!	other routines required no or minor changes
!-----------------------------------------------------------------------



	use module_data_mosaic_kind, only:  r8
	use module_data_mosaic_main, only:  pi !BSINGH - 05/28/2013(RCE updates)
	use module_peg_util



	implicit none
        !BSINGH - 05/28/2013(RCE updates)
        ! adjustment factors
	real(r8), parameter, public :: adjust_factor_dnaitdt = 1.0_r8  ! applied to final dnait/dt
	real(r8), parameter, public :: adjust_factor_bin_tern_ratenucl = 1.0_r8  ! applied to binary/ternary nucleation rate
	real(r8), parameter, public :: adjust_factor_pbl_ratenucl = 1.0_r8  ! applied to boundary layer nucleation rate
        !BSINGH - 05/28/2013(RCE updates ENDS)

	contains



!-----------------------------------------------------------------------
	subroutine mosaic_newnuc_1box(   &
		istat_newnuc, idiagbb_in,  &
		itstep, itype_newnuc, dtchem,   &
		temp_box, patm_box, cair_box, rh_box,   &
		dens_nh4so4a_in, mw_so4a, mw_nh4a, mw_h2o, mw_air,   &
		fact_apmassmr, fact_apnumbmr, fact_apdens, fact_apdiam, fact_gasmr,   &
		rbox0, rbox,   &
		adrydens_box, awetdens_box,   &
		adrydpav_box, awetdpav_box, adryqmas_box )
!
!   calculates new particle nucleation for a single grid box
!	over timestep dtchem
!   works with mosaic sectional aerosol packages
!
!   uses the following nucleation parameterizations
!	mnewnuc_flag1 = 1 -- h2so4-nh3-h2o ternary nuc. of merikanto et al. (2007)
!	mnewnuc_flag1 = 2 -- h2so4-h2o binary nuc. of vehkamaki et al. (2002)
!	mnewnuc_flag1 = 3 -- h2so4-h2o binary nuc. of wexler et al. (1994)

!BSINGH - 05/28/2013(RCE updates)
!	mnewnuc_flag1 = 11 -- empirical first  order boundary layer nucleation
!	mnewnuc_flag1 = 12 -- empirical second order boundary layer nucleation
!BSINGH - 05/28/2013(RCE updates ENDS)
	use module_data_mosaic_main, only:  ntot_used, piover6, third
	use module_data_mosaic_boxmod, only:  kh2so4, knh3, kso2
	use module_data_mosaic_aero, only:  mnewnuc_flag1
	use module_data_mosaic_asecthp, only:   &
		ai_phase, dens_aer, dens_water_aer,   &
		dcen_sect, dlo_sect, dhi_sect,   &
		hyswptr_aer, lptr_nh4_aer, lptr_so4_aer, lunerr,   &
		massptr_aer, maxd_asize, maxd_atype,   &
		ncomp_aer, nsize_aer, numptr_aer, smallmassbb,   &
		volumcen_sect, volumlo_sect, volumhi_sect, waterptr_aer


!   subr arguments
	integer,  intent(inout) :: istat_newnuc   ! =0 if no problems
	integer,  intent(in)    :: idiagbb_in     ! controls diagnostics
	integer,  intent(in)    :: itstep         ! host code time step index
	integer,  intent(in)    :: itype_newnuc   ! "type" that gets the new particles
	real(r8), intent(in)    :: dtchem         ! time step for nucleation
	real(r8), intent(in)    :: temp_box       ! air temp (K)
	real(r8), intent(in)    :: patm_box       ! air pressure (atm)
	real(r8), intent(in)    :: cair_box       ! air molar density (mol/cm^3)
	real(r8), intent(in)    :: rh_box         ! relative humidity (0-1)

	real(r8), intent(in) :: dens_nh4so4a_in   ! nh4so4 density (units same as dens_aer)
	real(r8), intent(in) :: mw_so4a, mw_nh4a, mw_h2o, mw_air   ! molec wghts (g/mol)

	real(r8), intent(in) :: fact_apmassmr, fact_apnumbmr, &
                                fact_apdens, fact_apdiam, fact_gasmr
!   the module_data_mosaic_asecthp densities are such that
!	dens_aer*fact_apdens give (g/cm^3)
!   the module_data_mosaic_asecthp diameters are such that
!	dlo/hi_sect*fact_apdiam give (cm)
!   the module_data_mosaic_asecthp volumes are such that
!	volumlo/hi_sect**fact_apdiam**3) give (cm^3)
!   aerosol mass mixing ratios in rbox(massptr_aer(ll,n,itype,iphase))
!       have units for which rbox*fact_apmassmr gives (g-AP/g-air)
!   aerosol number mixing ratios in rbox(numptr_aer(n,itype,iphase))
!       have units for which rbox*fact_apnumbmr gives (#/g-air)
!   trace gas mixing ratios in rbox(...)
!       have units for which rbox*fact_gasmr gives (mol/mol-air)

! rbox0 holds mixrat values before gas-aerosol mass-transfer
! rbox  holds mixrat values after  gas-aerosol mass-transfer and movesect 
	real(r8), intent(in)    :: rbox0(ntot_used)
	real(r8), intent(inout) :: rbox(ntot_used)

! adrydens_box = aerosol dry density (units same as dens_aer)
! awetdens_box = aerosol wet density (units same as dens_aer)
	real(r8), intent(inout) :: adrydens_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: awetdens_box(maxd_asize,maxd_atype)
! adrydpav_box = aerosol mean dry diameter (units same as dlo_sect)
! awetdpav_box = aerosol mean wet diameter (units same as dlo_sect)
	real(r8), intent(inout) :: adrydpav_box(maxd_asize,maxd_atype)
	real(r8), intent(inout) :: awetdpav_box(maxd_asize,maxd_atype)
! adryqmas_box = aerosol total-dry-mass mixing ratio (units same as rbox)
	real(r8), intent(inout) :: adryqmas_box(maxd_asize,maxd_atype)


!   local variables
	integer, parameter :: p1st = 1

	integer :: it=1, jt=1, k=1
	integer :: l, ll
	integer :: isize, itype, iphase
	integer :: iconform_numb
	integer :: idiagbb
	integer :: ldiagaa, lundiagaa
	integer :: nsize
	integer, save :: ncount(10)

! min h2so4 vapor for nuc calcs = 4.0e-16 mol/mol-air ~= 1.0e4 molecules/cm3, 
	real(r8), parameter :: qh2so4_cutoff = 4.0e-16_r8

! nh4hso4 values for a_zsr and b_zsr
        real(r8), parameter :: a_zsr_xx1 =  1.15510
        real(r8), parameter :: a_zsr_xx2 = -3.20815
        real(r8), parameter :: a_zsr_xx3 =  2.71141
        real(r8), parameter :: a_zsr_xx4 =  2.01155
        real(r8), parameter :: a_zsr_xx5 = -4.71014
        real(r8), parameter :: a_zsr_xx6 =  2.04616
        real(r8), parameter :: b_zsr_xx  = 29.4779

	real(r8) :: aw
	real(r8) :: densdefault
	real(r8) :: dens_nh4so4a, dens_nh4so4a_kgm3
	real(r8) :: dtnuc
	real(r8) :: duma, dumb, dumc
	real(r8) :: fact_apvolu
	real(r8) :: h2so4_uptkrate
	real(r8) :: press_pa
	real(r8) :: qh2so4_avg, qh2so4_cur, qh2so4_del  ! [mol/mol-air]
	real(r8) :: qnh3_avg, qnh3_cur, qnh3_del        ! [mol/mol-air]
	real(r8) :: qso4a_del, qnh4a_del                ! [mol/mol-air]
	real(r8) :: qnuma_del                           ! [#/mol-air]
	real(r8) :: tmpa, tmpb, tmpc, tmp_q2, tmp_q3
	real(r8) :: wwdens, wwdpav, wwmass, wwvolu
	real(r8) :: xxdens, xxdpav, xxmass, xxnumb, xxvolu

	real(r8),save :: tmpveca(10), tmpvecb(10), tmpvecc(10), tmpvecd(10), tmpvece(10), tmpvecf(10) !BSINGH - 05/28/2013(RCE updates - changed 'dum' to 'tmp')
	real(r8) :: dplom_nuc(maxd_asize), dphim_nuc(maxd_asize)
	real(r8) :: volumlo_nuc(maxd_asize), volumhi_nuc(maxd_asize)

	character(len=100) :: msg


        if ( kh2so4 < 1 .or. kh2so4 > ntot_used .or. &
             knh3   < 1 .or. knh3   > ntot_used .or. &
             kso2   < 1 .or. kso2   > ntot_used ) then
           write(msg,'(a,3i15)') &
              'module_mosaic_newnucb bad kh2so4/knh3/kso2', & 
              kh2so4, knh3, kso2
           call peg_message( lunerr, msg )
           call peg_error_fatal( lunerr, msg )
           call wrf_error_fatal(trim(adjustl(msg)))
        end if

!   check mnewnuc_flag1
	istat_newnuc = 0
	if (mnewnuc_flag1 <= 0) then !BSINGH - 05/28/2013(RCE updates)
           !BSINGH - 05/28/2013(RCE updates)
           return
	else if ( mnewnuc_flag1 ==  1 .or. &
             mnewnuc_flag1 ==  2 .or. &
             mnewnuc_flag1 ==  3 .or. &
             mnewnuc_flag1 == 11 .or. &
             mnewnuc_flag1 == 12 ) then
           continue
	else
           !BSINGH - 05/28/2013(RCE updates ENDS)
!	    if ((it .eq. its) .and. (jt .eq. jts))   &
		call peg_message( lunerr,   &
		'*** mosaic_newnuc_1box -- illegal mnewnuc_flag1' )
	    istat_newnuc = -1
	    return
            !BSINGH - 05/28/2013(RCE updates- got rid of an 'elseif' here)
	end if


!   set variables that do not change
	dtnuc = dtchem
	idiagbb = idiagbb_in
        lundiagaa = 93!BSINGH - 05/28/2013(RCE updates)

	itype = itype_newnuc
	iphase = ai_phase
	nsize = nsize_aer(itype)
	fact_apvolu = fact_apdiam*fact_apdiam*fact_apdiam
	densdefault = dens_aer(1,itype)*fact_apdens
	volumlo_nuc(1:nsize) = volumlo_sect(1:nsize,itype)*fact_apvolu
	volumhi_nuc(1:nsize) = volumhi_sect(1:nsize,itype)*fact_apvolu
	dplom_nuc(1:nsize) = dlo_sect(1:nsize,itype)*0.01*fact_apdiam   ! convert cm to m
	dphim_nuc(1:nsize) = dhi_sect(1:nsize,itype)*0.01*fact_apdiam   ! convert cm to m


!   loop over subareas (currently only 1) and vertical levels
!	do 2900 m = 1, nsubareas
!	do 2800 k = kclm_calcbgn, kclm_calcend


!   initialize diagnostics
!	if ((it .eq. its) .and.   &
!	    (jt .eq. jts) .and. (k .eq. kclm_calcbgn)) then
	    tmpveca(:) = 0.0         ! current grid param values
	    tmpvecb(:) = +1.0e35     ! param minimums
	    tmpvecc(:) = -1.0e35     ! param maximums
	    tmpvecd(:) = 0.0         ! param averages
	    tmpvece(:) = 0.0         ! param values for highest qnuma_del
	    ncount(:) = 0
!	end if


	ncount(1) = ncount(1) + 1

	qh2so4_cur = max(0.0_r8,rbox(kh2so4))*fact_gasmr
!   skip if h2so4 vapor < qh2so4_cutoff
	if (qh2so4_cur <= qh2so4_cutoff) goto 2700

	qnh3_cur   = max(0.0_r8,rbox(knh3))*fact_gasmr
	qh2so4_avg = 0.5*( qh2so4_cur + max(0.0_r8,rbox0(kh2so4))*fact_gasmr )
	qnh3_avg   = 0.5*( qnh3_cur   + max(0.0_r8,rbox0(knh3))*fact_gasmr )


! calc h2so4_uptkrate = h2so4 first-order uptake rate to existing aerosol
!                     = -d[ln(qh2so4)]/dt  (units = 1/s)
	tmp_q3 = qh2so4_cur
!   tmp_q2 = qh2so4 before aerosol uptake
	tmp_q2 = max(0.0_r8,rbox0(kh2so4))*fact_gasmr
!   following gets tmpb = log( max( 1, tmp_q2/tmp_q3 ) ) 
!   but with some checks added to avoid floating point exceptions
	if (tmp_q2 > tmp_q3) then
	   tmpc = tmp_q2 * exp( -20.0_r8 )
	   if (tmp_q3 <= tmpc) then
	      tmpb = 20.0_r8
	   else
	      tmpb = log( tmp_q2/tmp_q3 )
	   end if
	else
	   tmpb = 0.0
	end if
	h2so4_uptkrate = tmpb/dtchem


	qh2so4_del = 0.0
	qnh3_del = 0.0
	qnuma_del = 0.0
	qso4a_del = 0.0
	qnh4a_del = 0.0

!	dens_nh4so4a = dens_so4_aer
!	dens_nh4so4a = dens_aer_mac(iso4_a)
	dens_nh4so4a = dens_nh4so4a_in*fact_apdens

	isize = 0

!   make call to nucleation routine
	if (mnewnuc_flag1 /= 3) then!BSINGH - 05/28/2013(RCE updates)

!  new ternary (from cam3 code)
            ldiagaa = idiagbb!BSINGH - 05/28/2013(RCE updates)
            press_pa = patm_box*1.01325e5   ! convert atmos to pa
            dens_nh4so4a_kgm3 = dens_nh4so4a*1.0e3

            call mer07_veh02_nuc_mosaic_1box(   &
               mnewnuc_flag1, dtnuc, temp_box, rh_box, press_pa,   &
               qh2so4_cur, qh2so4_avg, qnh3_cur, qnh3_avg, h2so4_uptkrate,   &
               nsize, maxd_asize, dplom_nuc, dphim_nuc,   &
               isize, qnuma_del, qso4a_del, qnh4a_del,   &
               qh2so4_del, qnh3_del, dens_nh4so4a_kgm3, &
               itstep, ldiagaa, lundiagaa )!BSINGH - 05/28/2013(RCE updates)
!           subr mer07_veh02_nuc_mosaic_1box(   &
!              newnuc_method_flagaa, dtnuc, temp_in, rh_in, press_in,   &
!              qh2so4_cur, qh2so4_avg, qnh3_cur, h2so4_uptkrate,   &
!              nsize, maxd_asize, dplom_sect, dphim_sect,   &
!              isize_nuc, qnuma_del, qso4a_del, qnh4a_del,   &
!              qh2so4_del, qnh3_del, dens_nh4so4a, &
!              itstep, ldiagaa, lundiagaa )!BSINGH - 05/28/2013(RCE updates)

            dens_nh4so4a = dens_nh4so4a_kgm3/1.0e3

	else! (mnewnuc_flag1 == 3)!BSINGH - 05/28/2013(RCE updates)

            call wexler_nuc_mosaic_1box(   &
               dtnuc, temp_box, rh_box, cair_box,   &
               qh2so4_avg, qh2so4_cur, qnh3_avg, qnh3_cur,   &
               nsize, maxd_asize, volumlo_nuc, volumhi_nuc,   &
               isize, qnuma_del, qso4a_del, qnh4a_del,   &
               qh2so4_del, qnh3_del, dens_nh4so4a )

	!BSINGH - 05/28/2013(RCE updates- got rid of an 'else' here)
	end if


!   temporary diagnostics
	tmpveca(1) = temp_box
	tmpveca(2) = rh_box
	tmpveca(3) = rbox(kso2)*fact_gasmr
	tmpveca(4) = qh2so4_avg
	tmpveca(5) = qnh3_avg
	tmpveca(6) = qnuma_del
	do l = 1, 6
	    tmpvecb(l) = min( tmpvecb(l), tmpveca(l) )
	    tmpvecc(l) = max( tmpvecc(l), tmpveca(l) )
	    tmpvecd(l) = tmpvecd(l) + tmpveca(l)
	    if (qnuma_del .gt. tmpvece(6)) tmpvece(l) = tmpveca(l)
	end do


!   check for zero new particles
	if (qnuma_del .le. 0.0) goto 2700

!   check for valid isize
	if (isize .ne. 1) ncount(3) = ncount(3) + 1
	if ((isize .lt. 1) .or. (isize .gt. nsize)) then
	    write(msg,93010) 'newnucxx bad isize_nuc' , it, jt, k,   &
		isize, nsize
	    call peg_message( lunerr, msg )
	    goto 2700
	end if
93010	format( a, 3i3, 1p, 9e10.2 )


	ncount(2) = ncount(2) + 1

!   update gas and aerosol so4 and nh3/nh4 mixing ratios
	rbox(kh2so4) = max( 0.0_r8, rbox(kh2so4) + qh2so4_del/fact_gasmr )
	rbox(knh3  ) = max( 0.0_r8, rbox(knh3  ) + qnh3_del/fact_gasmr )

	l = lptr_so4_aer(isize,itype,iphase)
	if (l .ge. p1st) then
!	    rbox(l) = rbox(l) + qso4a_del
	    tmpa = qso4a_del*mw_so4a/mw_air   ! g/g-air
            tmpvecf(1) = rbox(l)!BSINGH - 05/28/2013(RCE updates)
	    rbox(l) = rbox(l) + tmpa/fact_apmassmr
            !BSINGH - 05/28/2013(RCE updates)
            tmpvecf(2) = rbox(l)
	    !              (convert whatever to g/g-air) * (then to ug/g-air) * (then to ug/m3-air)
	    tmpvecf(1:2) = tmpvecf(1:2) * fact_apmassmr  * 1.0e6              * (1.0e6_r8*cair_box*mw_air)
	    tmpvecf(3) = tmpvecf(2) - tmpvecf(1)
	    if (idiagbb >= 10) &
                 write(lundiagaa,'(a,1p,3e12.4)') 'so4 (ug/m3) o/n/d', tmpvecf(1:3)
            !BSINGH - 05/28/2013(RCE updates ENDS)
	end if
	l = lptr_nh4_aer(isize,itype,iphase)
	if (l .ge. p1st) then
!	    rbox(l) = rbox(l) + qnh4a_del
	    tmpa = qnh4a_del*mw_nh4a/mw_air   ! g/g-air
            tmpvecf(1) = rbox(l)!BSINGH - 05/28/2013(RCE updates)
	    rbox(l) = rbox(l) + tmpa/fact_apmassmr
            !BSINGH - 05/28/2013(RCE updates)
            tmpvecf(2) = rbox(l)
	    !              (convert whatever to g/g-air) * (then to ug/g-air) * (then to ug/m3-air)
	    tmpvecf(1:2) = tmpvecf(1:2) * fact_apmassmr  * 1.0e6              * (1.0e6_r8*cair_box*mw_air)
	    tmpvecf(3) = tmpvecf(2) - tmpvecf(1)
	    if (idiagbb >= 10) &
	    write(lundiagaa,'(a,1p,3e12.4)') 'nh4 (ug/m3) o/n/d', tmpvecf(1:3)
            !BSINGH - 05/28/2013(RCE updates ENDS)
	end if
	l = numptr_aer(isize,itype,iphase)
!	rbox(l) = rbox(l) + qnuma_del
	tmpa = qnuma_del/mw_air   ! #/g-air
        tmpvecf(1) = rbox(l)!BSINGH - 05/28/2013(RCE updates)
	rbox(l) = rbox(l) + tmpa/fact_apnumbmr
        !BSINGH - 05/28/2013(RCE updates)
        tmpvecf(2) = rbox(l)
	!              (convert whatever to #/g-air) * (then to #/cm3-air)
	tmpvecf(1:2) = tmpvecf(1:2) * fact_apnumbmr  * (cair_box*mw_air)
	tmpvecf(3) = tmpvecf(2) - tmpvecf(1)
	if (idiagbb >= 10) &
	write(lundiagaa,'(a,1p,3e12.4)') 'num (#/cm3) o/n/d', tmpvecf(1:3)
        !BSINGH - 05/28/2013(RCE updates ENDS)
	xxnumb = rbox(l)*fact_apnumbmr

!   update aerosol water, using mosaic parameterizations for nh4hso4
!     duma = (mole-salt)/(mole-salt+water)
!     dumb = (mole-salt)/(kg-water)
!     dumc = (mole-water)/(mole-salt)
	l = waterptr_aer(isize,itype)
	if ((rh_box .gt. 0.10) .and. (l .ge. p1st)) then
	    aw = min( rh_box, 0.98_r8 )
	    if (aw .lt. 0.97) then
		duma =       a_zsr_xx1 +   &
		        aw*( a_zsr_xx2 +   &
		        aw*( a_zsr_xx3 +   &
		        aw*( a_zsr_xx4 +   &
		        aw*( a_zsr_xx5 +   &
		        aw*  a_zsr_xx6 ))))
	    else
		dumb = -b_zsr_xx*log(aw)
	        dumb = max( dumb, 0.5_r8 )
		duma = 1.0/(1.0 + 55.509/dumb)
	    end if
	    duma = max( duma, 0.01_r8 )
	    dumc = (1.0 - duma)/duma
!	    rbox(l) = rbox(l) + qso4a_del*dumc
	    tmpa = qso4a_del*dumc*mw_h2o/mw_air
	    rbox(l) = rbox(l) + tmpa/fact_apmassmr
	end if


!
!   update dry mass, density, and volume,
!   and check for mean dry-size within bounds
!
	xxmass = adryqmas_box(isize,itype)*fact_apmassmr
	xxdens = adrydens_box( isize,itype)*fact_apdens
	iconform_numb = 1

	if ((xxdens .lt. 0.1) .or. (xxdens .gt. 20.0)) then
!   (exception) case of drydensity not valid
	    continue
	else
!   (normal) case of drydensity valid (which means drymass is valid also)
!   so increment mass and volume with the so4 & nh4 deltas, then calc density
	    xxvolu = xxmass/xxdens
!	    duma = qso4a_del*mw_aer_mac(iso4_a)   &
!	         + qnh4a_del*mw_aer_mac(inh4_a)
	    duma = (qso4a_del*mw_so4a + qnh4a_del*mw_nh4a)/mw_air   ! g/g-air
	    xxmass = xxmass + duma
	    xxvolu  = xxvolu  + duma/dens_nh4so4a
	    if (xxmass .le. smallmassbb) then
!   do this to force calc of dry mass, volume from rbox
		xxdens = 0.001
	    else if (xxmass .gt. 1000.0*xxvolu) then
!   in this case, density is too large.  setting density=1000 forces
!   next IF block while avoiding potential divide by zero or overflow
		xxdens = 1000.0
	    else 
		xxdens = xxmass/xxvolu
	    end if
	end if

	if ((xxdens .lt. 0.1) .or. (xxdens .gt. 20.0)) then
!   (exception) case of drydensity not valid (or drymass extremely small), 
!   so compute from dry mass, volume from rbox
	    ncount(4) = ncount(4) + 1
	    xxmass = 0.0
	    xxvolu  = 0.0
	    do ll = 1, ncomp_aer(itype)
		l = massptr_aer(ll,isize,itype,iphase)
		if (l .ge. p1st) then
!		    duma = max( 0.0_r8, rbox(l) )*mw_aer(ll,itype)
!		    xxmass = xxmass + duma
!		    xxvolu = xxvolu + duma/dens_aer(ll,itype)
		    duma = max( 0.0_r8, rbox(l) )*fact_apmassmr
		    xxmass = xxmass + duma
		    xxvolu = xxvolu + duma/(dens_aer(ll,itype)*fact_apdens)
		end if
	    end do
	end if

	if (xxmass .le. smallmassbb) then
!   when drymass extremely small, use default density and bin center size,
!   and zero out water
	    ncount(5) = ncount(5) + 1
	    xxdens = densdefault
	    xxvolu = xxmass/xxdens
	    xxnumb = xxmass/(volumcen_sect(isize,itype)*fact_apvolu*xxdens)
	    xxdpav = dcen_sect(isize,itype)*fact_apdiam
	    wwdens = xxdens
	    wwdpav = xxdpav
	    iconform_numb = 0
	    l = waterptr_aer(isize,itype)
	    if (l .ge. p1st) rbox(l) = 0.0
	    l = hyswptr_aer(isize,itype)
	    if (l .ge. p1st) rbox(l) = 0.0
	else
	    xxdens = xxmass/xxvolu
	end if

	if (iconform_numb .gt. 0) then
!   check for mean dry-size within bounds, and conform number if not
	    if ( xxnumb .gt. xxvolu/volumlo_nuc(isize) ) then
		ncount(6) = ncount(6) + 1
		xxnumb = xxvolu/volumlo_nuc(isize)
		xxdpav = dlo_sect(isize,itype)*fact_apdiam
	    else if ( xxnumb .lt. xxvolu/volumhi_nuc(isize) ) then
		ncount(7) = ncount(7) + 1
		xxnumb = xxvolu/volumhi_nuc(isize)
		xxdpav = dhi_sect(isize,itype)*fact_apdiam
	    else
		xxdpav = (xxvolu/(xxnumb*piover6))**third
	    end if

	    tmpb = 0.0
	    l = waterptr_aer(isize,itype)
!	    if (l .ge. p1st) tmpb = max(0.0_r8,rbox(l))*mw_water_aer
	    if (l .ge. p1st) tmpb = max(0.0_r8,rbox(l))*fact_apmassmr
	    wwmass = xxmass + tmpb
	    wwvolu = xxvolu + tmpb/(dens_water_aer*fact_apdens)
	    wwdens = wwmass/wwvolu
	    wwdpav = xxdpav*((wwvolu/xxvolu)**third)
	end if

!   load dry mass, density, volume, and (possibly conformed) number
	l = numptr_aer(isize,itype,iphase)
	rbox(l) = xxnumb/fact_apnumbmr
	adrydens_box(isize,itype) = xxdens/fact_apdens
	adrydpav_box(isize,itype) = xxdpav/fact_apdiam
	adryqmas_box(isize,itype) = xxmass/fact_apmassmr
	awetdens_box(isize,itype) = wwdens/fact_apdens
	awetdpav_box(isize,itype) = wwdpav/fact_apdiam


2700	continue

!   temporary diagnostics
	if (idiagbb .ge. 100) then
!	if ((idiagbb .ge. 100) .and.   &
!	    (it .eq. ite) .and.    &
!	    (jt .eq. jte) .and. (k .eq. kclm_calcend)) then
            !BSINGH - 05/28/2013(RCE updates - got rid of lundiagaa)
	    write(lundiagaa,'(2a)')   &
		'newnucbb names  temp      rh        ',   &
		'so2       h2so4_avg nh3_avg   numa_del'
	    if (idiagbb .ge. 110) then
	      write(lundiagaa,93020) 'newnucbb mins ', tmpvecb(1:6)
	      write(lundiagaa,93020) 'newnucbb maxs ', tmpvecc(1:6)
	      duma = max( 1, ncount(1) ) 
	      write(lundiagaa,93020) 'newnucbb avgs ', tmpvecd(1:6)/duma
	      write(lundiagaa,93020) 'newnucbb hinuc', tmpvece(1:6)
	      write(lundiagaa,93020) 'newnucbb dtnuc', dtnuc
	    end if
	    write(lundiagaa,93030) 'newnucbb ncnt ', ncount(1:7)
	end if
93020	format( a, 1p, 10e10.2 )
93030	format( a, 1p, 10i10 )


!2800	continue	! k levels
!2900	continue	! subareas


	return
	end subroutine mosaic_newnuc_1box



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine mer07_veh02_nuc_mosaic_1box(   &
           newnuc_method_flagaa, dtnuc, temp_in, rh_in, press_in,   &
           qh2so4_cur, qh2so4_avg, qnh3_cur, qnh3_avg, h2so4_uptkrate,   &
           nsize, maxd_asize, dplom_sect, dphim_sect,   &
           isize_nuc, qnuma_del, qso4a_del, qnh4a_del,   &
           qh2so4_del, qnh3_del, dens_nh4so4a, &
           itstep, ldiagaa, lundiagaa ) !BSINGH - 05/28/2013(RCE updates)
!.......................................................................
!
! calculates new particle production from homogeneous nucleation
!    over timestep dtnuc, using nucleation rates from either
!    merikanto et al. (2007) h2so4-nh3-h2o ternary parameterization
!    vehkamaki et al. (2002) h2so4-h2o binary parameterization
!
! the new particles are "grown" to the lower-bound size of the host code's 
!    smallest size bin.  (this "growth" is somewhat ad hoc, and would not be
!    necessary if the host code's size bins extend down to ~1 nm.)
!
!    if the h2so4 and nh3 mass mixing ratios (mixrats) of the grown new 
!    particles exceed the current gas mixrats, the new particle production
!    is reduced so that the new particle mass mixrats match the gas mixrats.
!
!    the correction of kerminen and kulmala (2002) is applied to account
!    for loss of the new particles by coagulation as they are
!    growing to the "host code mininum size"
!
! revision history
!    coded by rc easter, pnnl, xx-apr-2007
!
! key routines called: subr ternary_nuc_napari
!
! references:
!    merikanto, j., i. napari, h. vehkamaki, t. anttila,
!     and m. kulmala, 2007, new parameterization of
!     sulfuric acid-ammonia-water ternary nucleation
!     rates at tropospheric conditions,
!       j. geophys. res., 112, d15207, doi:10.1029/2006jd0027977
!
!    vehkamäki, h., m. kulmala, i. napari, k.e.j. lehtinen,
!       c. timmreck, m. noppel and a. laaksonen, 2002,
!       an improved parameterization for sulfuric acid-water nucleation
!       rates for tropospheric and stratospheric conditions,
!       j. geophys. res., 107, 4622, doi:10.1029/2002jd002184
!
!    kerminen, v., and m. kulmala, 2002,
!	analytical formulae connecting the "real" and the "apparent"
!	nucleation rate and the nuclei number concentration
!	for atmospheric nucleation events
!
!.......................................................................
      implicit none

! subr arguments (in)
        real(r8), intent(in) :: dtnuc             ! nucleation time step (s)
        real(r8), intent(in) :: temp_in           ! temperature, in k
        real(r8), intent(in) :: rh_in             ! relative humidity, as fraction
        real(r8), intent(in) :: press_in          ! air pressure (pa)

        real(r8), intent(in) :: qh2so4_cur, qh2so4_avg
        real(r8), intent(in) :: qnh3_cur, qnh3_avg
             ! above 4 variables are gas h2so4 and gas nh3 mixing ratios (mol/mol-air)
             ! qxxx_cur = current value (after gas chem and condensation)
             ! qxxx_avg = estimated average value (for simultaneous source/sink calcs)
        real(r8), intent(in) :: h2so4_uptkrate    ! h2so4 uptake rate to aerosol (1/s)

        integer,  intent(in) :: newnuc_method_flagaa    ! 1=merikanto et al (2007) ternary
                                                        ! 2=vehkamaki et al (2002) binary
        integer,  intent(in) :: nsize                   ! number of aerosol size bins
        integer,  intent(in) :: maxd_asize              ! dimension for dplom_sect, ...
        real(r8), intent(in) :: dplom_sect(maxd_asize)  ! dry diameter at lower bnd of bin (m)
        real(r8), intent(in) :: dphim_sect(maxd_asize)  ! dry diameter at upper bnd of bin (m)
        integer,  intent(in) :: itstep !BSINGH - 05/28/2013(RCE updates)
        integer,  intent(in) :: ldiagaa, lundiagaa

! subr arguments (inout & out)
        integer,  intent(out) :: isize_nuc        ! size bin into which new particles go
        real(r8), intent(out) :: qnuma_del        ! change to aerosol number mixing ratio (#/mol-air)
        real(r8), intent(out) :: qso4a_del        ! change to aerosol so4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qnh4a_del        ! change to aerosol nh4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qh2so4_del       ! change to gas h2so4 mixing ratio (mol/mol-air)
        real(r8), intent(out) :: qnh3_del         ! change to gas nh3 mixing ratio (mol/mol-air)
                                                  ! aerosol changes are > 0; gas changes are < 0
        real(r8), intent(inout) :: dens_nh4so4a   ! dry-density of the new nh4-so4 aerosol mass (kg/m3)
                                                  ! use 'in' value only if it is between 1500-2000 kg/m3

! subr arguments (out) passed via common block  
!    these are used to duplicate the outputs of yang zhang's original test driver
!    they are not really needed in wrf-chem
        real(r8) :: ratenuclt        ! j = ternary nucleation rate from napari param. (cm-3 s-1)
        real(r8) :: rateloge         ! ln (j)
        real(r8) :: cnum_h2so4       ! number of h2so4 molecules in the critical nucleus
        real(r8) :: cnum_nh3         ! number of nh3   molecules in the critical nucleus
        real(r8) :: cnum_tot         ! total number of molecules in the critical nucleus
        real(r8) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        integer :: i
        integer :: igrow
        integer, save :: icase = 0, icase_reldiffmax = 0
!       integer, parameter :: ldiagaa = -1
        integer :: lun
        integer :: newnuc_method_flagaa2

        real(r8), parameter :: onethird = 1.0/3.0
        real(r8), parameter :: avogad = 6.022e23   ! avogadro number (molecules/mol)
        real(r8), parameter :: mw_air = 28.966     ! dry-air mean molecular weight (g/mol)

        real(r8), parameter :: accom_coef_h2so4 = 0.65   ! accomodation coef for h2so4 conden

! dry densities (kg/m3) molecular weights of aerosol 
! ammsulf, ammbisulf, and sulfacid (from mosaic  dens_electrolyte values)
!       real(r8), parameter :: dens_ammsulf   = 1.769e3
!       real(r8), parameter :: dens_ammbisulf = 1.78e3
!       real(r8), parameter :: dens_sulfacid  = 1.841e3
! use following to match cam3 modal_aero densities
        real(r8), parameter :: dens_ammsulf   = 1.770e3
        real(r8), parameter :: dens_ammbisulf = 1.770e3
        real(r8), parameter :: dens_sulfacid  = 1.770e3
        real(r8), parameter :: dens_water     = 1.0e3

! molecular weights (g/mol) of aerosol ammsulf, ammbisulf, and sulfacid
!    for ammbisulf and sulfacid, use 114 & 96 here rather than 115 & 98
!    because we don't keep track of aerosol hion mass
        real(r8), parameter :: mw_ammsulf   = 132.0
        real(r8), parameter :: mw_ammbisulf = 114.0
        real(r8), parameter :: mw_sulfacid  =  96.0
! molecular weights of aerosol sulfate and ammonium
        real(r8), parameter :: mw_so4a      =  96.0
        real(r8), parameter :: mw_nh4a      =  18.0
        real(r8), parameter :: mw_water     =  18.0

        real(r8), save :: reldiffmax = 0.0

        real(r8) cair                     ! dry-air molar density (mol/m3)
        real(r8) cs_prime_kk              ! kk2002 "cs_prime" parameter (1/m2)
        real(r8) cs_kk                    ! kk2002 "cs" parameter (1/s)
        real(r8) dens_part                ! "grown" single-particle dry density (kg/m3)
        real(r8) dfin_kk, dnuc_kk         ! kk2002 final/initial new particle wet diameter (nm)
        real(r8) dpdry_clus               ! critical cluster diameter (m)
        real(r8) dpdry_part               ! "grown" single-particle dry diameter (m)
        real(r8) tmpa, tmpb, tmpc, tmpe, tmpq
        real(r8) tmpa1, tmpb1
        real(r8) tmp_m1, tmp_m2, tmp_m3, tmp_n1, tmp_n2, tmp_n3
        real(r8) tmp_spd                  ! h2so4 vapor molecular speed (m/s)
        real(r8) factor_kk
        real(r8) fogas, foso4a, fonh4a, fonuma
        real(r8) freduce                  ! reduction factor applied to nucleation rate
                                          ! due to limited availability of h2so4 & nh3 gases
        real(r8) freducea, freduceb
        real(r8) gamma_kk                 ! kk2002 "gamma" parameter (nm2*m2/h)
        real(r8) gr_kk                    ! kk2002 "gr" parameter (nm/h)
        real(r8) kgaero_per_moleso4a      ! (kg dry aerosol)/(mol aerosol so4)
        real(r8) mass_part                ! "grown" single-particle dry mass (kg)
        real(r8) molenh4a_per_moleso4a    ! (mol aerosol nh4)/(mol aerosol so4)
        real(r8) nh3ppt, nh3ppt_bb        ! actual and bounded nh3 (ppt)
        real(r8) nu_kk                    ! kk2002 "nu" parameter (nm)
        real(r8) qmolnh4a_del_max         ! max production of aerosol nh4 over dtnuc (mol/mol-air)
        real(r8) qmolso4a_del_max         ! max production of aerosol so4 over dtnuc (mol/mol-air)
        real(r8) ratenuclt_bb             ! nucleation rate (#/m3/s)
        real(r8) ratenuclt_kk             ! nucleation rate after kk2002 adjustment (#/m3/s)
        real(r8) rh_bb                    ! bounded value of rh_in
        real(r8) so4vol_in                ! concentration of h2so4 for nucl. calc., molecules cm-3
        real(r8) so4vol_bb                ! bounded value of so4vol_in
        real(r8) temp_bb                  ! bounded value of temp_in
        real(r8) voldry_clus              ! critical-cluster dry volume (m3)
        real(r8) voldry_part              ! "grown" single-particle dry volume (m3)
        real(r8) wetvol_dryvol            ! grown particle (wet-volume)/(dry-volume)
        real(r8) wet_volfrac_so4a         ! grown particle (dry-volume-from-so4)/(wet-volume)



!
! if h2so4 vapor < qh2so4_cutoff
! exit with new particle formation = 0
!
        isize_nuc = 1
        qnuma_del = 0.0
        qso4a_del = 0.0
        qnh4a_del = 0.0
        qh2so4_del = 0.0
        qnh3_del = 0.0
!       if (qh2so4_avg .le. qh2so4_cutoff) return   ! this no longer needed
!       if (qh2so4_cur .le. qh2so4_cutoff) return   ! this no longer needed


!
! make call to vehkamaki parameterization routine
!

! calc h2so4 in molecules/cm3 and nh3 in ppt
        cair = press_in/(temp_in*8.3144)
        so4vol_in = qh2so4_avg * cair * avogad * 1.0e-6
        nh3ppt    = qnh3_avg * 1.0e12
        newnuc_method_flagaa2 = 0!BSINGH - 05/28/2013(RCE updates)

        if ((newnuc_method_flagaa == 1) .and. (nh3ppt >= 0.1)) then
            if (so4vol_in < 5.0e4) return
! make call to merikanto ternary parameterization routine
! (when nh3ppt < 0.1, use binary param instead)
            temp_bb = max( 235.0_r8, min( 295.0_r8, temp_in ) )
            rh_bb = max( 0.05_r8, min( 0.95_r8, rh_in ) )
            so4vol_bb = max( 5.0e4_r8, min( 1.0e9_r8, so4vol_in ) )
            nh3ppt_bb = max( 0.1_r8, min( 1.0e3_r8, nh3ppt ) )
            call ternary_nuc_merik2007(   &
                temp_bb, rh_bb, so4vol_bb, nh3ppt_bb,   &
                rateloge,   &
                cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
            newnuc_method_flagaa2 = 1

        else if ((newnuc_method_flagaa == 1) .or. &
                 (newnuc_method_flagaa == 2)) then

            if (so4vol_in < 1.0e4) return
! make call to vehkamaki binary parameterization routine
            temp_bb = max( 230.15_r8, min( 305.15_r8, temp_in ) )
            rh_bb = max( 1.0e-4_r8, min( 1.0_r8, rh_in ) )
            so4vol_bb = max( 1.0e4_r8, min( 1.0e11_r8, so4vol_in ) )
            call binary_nuc_vehk2002(   &
                temp_bb, rh_bb, so4vol_bb,   &
                ratenuclt, rateloge,   &
                cnum_h2so4, cnum_tot, radius_cluster )
            cnum_nh3 = 0.0
            newnuc_method_flagaa2 = 2
            !BSINGH - 05/28/2013(RCE updates)
         else if ((newnuc_method_flagaa == 11) .or. &
              (newnuc_method_flagaa == 12)) then
            call pbl_nuc_wang2008( so4vol_in,   &
                 newnuc_method_flagaa, newnuc_method_flagaa2,   &
                 ratenuclt, rateloge,   &
                 cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
            !BSINGH - 05/28/2013(RCE updates ENDS)   
        else
            return
        end if
        if (ldiagaa > 0) &
        write(lundiagaa,'(a,i8,1p,e10.2)') 'newnuc - itstep, ratenucl =', itstep, exp( rateloge )!BSINGH - 05/28/2013(RCE updates)
! if nucleation rate is less than 1e-6 #/m3/s ~= 0.1 #/cm3/day,
! exit with new particle formation = 0
        if (rateloge  .le. -13.82_r8) return
!       if (ratenuclt .le. 1.0e-6) return
        ratenuclt = exp( rateloge )
        ratenuclt_bb = ratenuclt*1.0e6_r8


! wet/dry volume ratio - use simple kohler approx for ammsulf/ammbisulf
        tmpa = max( 0.10_r8, min( 0.95_r8, rh_in ) )
        wetvol_dryvol = 1.0 - 0.56/log(tmpa)


! determine size bin into which the new particles go
! (probably it will always be bin #1, but ...)
        voldry_clus = ( max(cnum_h2so4,1.0_r8)*mw_so4a + cnum_nh3*mw_nh4a ) /   &
                      (1.0e3*dens_sulfacid*avogad)
        dpdry_clus = (voldry_clus*6.0/pi)**onethird

        isize_nuc = 1
        dpdry_part = dplom_sect(1)
        if (dpdry_clus <= dplom_sect(1)) then
           igrow = 1   ! need to clusters to larger size
        else if (dpdry_clus >= dphim_sect(nsize)) then
           igrow = 0
           isize_nuc = nsize
           dpdry_part = dphim_sect(nsize)
        else
           igrow = 0
           do i = 1, nsize
              if (dpdry_clus < dphim_sect(i)) then
                 isize_nuc = i
                 dpdry_part = dpdry_clus
                 dpdry_part = min( dpdry_part, dphim_sect(i) )
                 dpdry_part = max( dpdry_part, dplom_sect(i) )
                 exit
              end if
           end do
        end if
        voldry_part = (pi/6.0)*(dpdry_part**3)
        if (ldiagaa > 0) &
             write(lundiagaa,'(a,i8,i4)') 'newnuc - itstep, igrow    =', itstep, igrow!BSINGH - 05/28/2013(RCE updates)


!
! determine composition and density of the "grown particles"
! the grown particles are assumed to be liquid
!    (since critical clusters contain water)
!    so any (nh4/so4) molar ratio between 0 and 2 is allowed
! assume that the grown particles will have 
!    (nh4/so4 molar ratio) = min( 2, (nh3/h2so4 gas molar ratio) )
!
        if (igrow .le. 0) then
! no "growing" so pure sulfuric acid
           tmp_n1 = 0.0
           tmp_n2 = 0.0
           tmp_n3 = 1.0
        else if (qnh3_cur .ge. qh2so4_cur) then
! combination of ammonium sulfate and ammonium bisulfate
! tmp_n1 & tmp_n2 = mole fractions of the ammsulf & ammbisulf
           tmp_n1 = (qnh3_cur/qh2so4_cur) - 1.0
           tmp_n1 = max( 0.0_r8, min( 1.0_r8, tmp_n1 ) )
           tmp_n2 = 1.0 - tmp_n1
           tmp_n3 = 0.0
        else
! combination of ammonium bisulfate and sulfuric acid
! tmp_n2 & tmp_n3 = mole fractions of the ammbisulf & sulfacid
           tmp_n1 = 0.0
           tmp_n2 = (qnh3_cur/qh2so4_cur)
           tmp_n2 = max( 0.0_r8, min( 1.0_r8, tmp_n2 ) )
           tmp_n3 = 1.0 - tmp_n2
	end if

        tmp_m1 = tmp_n1*mw_ammsulf
        tmp_m2 = tmp_n2*mw_ammbisulf
        tmp_m3 = tmp_n3*mw_sulfacid
        dens_part = (tmp_m1 + tmp_m2 + tmp_m3)/   &
           ((tmp_m1/dens_ammsulf) + (tmp_m2/dens_ammbisulf)   &
                                  + (tmp_m3/dens_sulfacid))
! use 'in' value only if it is between 1500-2000 kg/m3
	if (abs(dens_nh4so4a-1750.0) .le. 250.0) then
	    dens_part = dens_nh4so4a
	else
            dens_nh4so4a = dens_part
	end if

        mass_part  = voldry_part*dens_part 
! (mol aerosol nh4)/(mol aerosol so4)
        molenh4a_per_moleso4a = 2.0*tmp_n1 + tmp_n2  
! (kg dry aerosol)/(mol aerosol so4)
        kgaero_per_moleso4a = 1.0e-3*(tmp_m1 + tmp_m2 + tmp_m3)  

! fraction of wet volume due to so4a
        tmpb = 1.0 + molenh4a_per_moleso4a*17.0/98.0
        wet_volfrac_so4a = 1.0 / ( wetvol_dryvol * tmpb )


!
! calc kerminen & kulmala (2002) correction
!
        if (igrow <=  0) then
            factor_kk = 1.0

        else
! "gr" parameter (nm/h) = condensation growth rate of new particles
! use kk2002 eqn 21 for h2so4 uptake, and correct for nh3 & h2o uptake
            tmp_spd = 14.7*sqrt(temp_in)   ! h2so4 molecular speed (m/s)
            gr_kk = 3.0e-9*tmp_spd*mw_sulfacid*so4vol_in/   &
                    (dens_part*wet_volfrac_so4a)

! "gamma" parameter (nm2/m2/h)
! use kk2002 eqn 22
!
! dfin_kk = wet diam (nm) of grown particle having dry dia = dpdry_part (m)
            dfin_kk = 1.0e9 * dpdry_part * (wetvol_dryvol**onethird)
! dnuc_kk = wet diam (nm) of cluster
            dnuc_kk = 2.0*radius_cluster
            dnuc_kk = max( dnuc_kk, 1.0_r8 )
! neglect (dmean/150)**0.048 factor, 
! which should be very close to 1.0 because of small exponent
            gamma_kk = 0.23 * (dnuc_kk)**0.2   &
                     * (dfin_kk/3.0)**0.075   &
                     * (dens_part*1.0e-3)**(-0.33)   &
                     * (temp_in/293.0)**(-0.75)

! "cs_prime parameter" (1/m2) 
! instead kk2002 eqn 3, use
!     cs_prime ~= tmpa / (4*pi*tmpb * h2so4_accom_coef)
! where
!     tmpa = -d(ln(h2so4))/dt by conden to particles   (1/h units)
!     tmpb = h2so4 vapor diffusivity (m2/h units)
! this approx is generally within a few percent of the cs_prime
!     calculated directly from eqn 2, 
!     which is acceptable, given overall uncertainties
! tmpa = -d(ln(h2so4))/dt by conden to particles   (1/h units)
            tmpa = h2so4_uptkrate * 3600.0
            tmpa1 = tmpa
            tmpa = max( tmpa, 0.0_r8 )
! tmpb = h2so4 gas diffusivity (m2/s, then m2/h)
            tmpb = 6.7037e-6 * (temp_in**0.75) / cair
            tmpb1 = tmpb         ! m2/s
            tmpb = tmpb*3600.0   ! m2/h
            cs_prime_kk = tmpa/(4.0*pi*tmpb*accom_coef_h2so4)
            cs_kk = cs_prime_kk*4.0*pi*tmpb1

! "nu" parameter (nm) -- kk2002 eqn 11
            nu_kk = gamma_kk*cs_prime_kk/gr_kk
! nucleation rate adjustment factor (--) -- kk2002 eqn 13
            factor_kk = exp( (nu_kk/dfin_kk) - (nu_kk/dnuc_kk) )

        end if
        ratenuclt_kk = ratenuclt_bb*factor_kk


! max production of aerosol dry mass (kg-aero/m3-air)
        tmpa = max( 0.0_r8, (ratenuclt_kk*dtnuc*mass_part) )
! max production of aerosol so4 (mol-so4a/mol-air)
        tmpe = tmpa/(kgaero_per_moleso4a*cair)
! max production of aerosol so4 (mol/mol-air)
! based on ratenuclt_kk and mass_part
        qmolso4a_del_max = tmpe

! check if max production exceeds available h2so4 vapor
        freducea = 1.0
        if (qmolso4a_del_max .gt. qh2so4_cur) then
           freducea = qh2so4_cur/qmolso4a_del_max
        end if

! check if max production exceeds available nh3 vapor
        freduceb = 1.0
        if (molenh4a_per_moleso4a .ge. 1.0e-10) then
! max production of aerosol nh4 (ppm) based on ratenuclt_kk and mass_part
           qmolnh4a_del_max = qmolso4a_del_max*molenh4a_per_moleso4a
           if (qmolnh4a_del_max .gt. qnh3_cur) then
              freduceb = qnh3_cur/qmolnh4a_del_max
           end if
        end if
        freduce = min( freducea, freduceb )

! if adjusted nucleation rate is less than 1e-12 #/m3/s ~= 0.1 #/cm3/day,
! exit with new particle formation = 0
        if (freduce*ratenuclt_kk .le. 1.0e-12) return


! note:  suppose that at this point, freduce < 1.0 (no gas-available 
!    constraints) and molenh4a_per_moleso4a < 2.0
! if the gas-available constraints is do to h2so4 availability,
!    then it would be possible to condense "additional" nh3 and have
!    (nh3/h2so4 gas molar ratio) < (nh4/so4 aerosol molar ratio) <= 2 
! one could do some additional calculations of 
!    dens_part & molenh4a_per_moleso4a to realize this
! however, the particle "growing" is a crude approximate way to get
!    the new particles to the host code's minimum particle size,
! are such refinements worth the effort?


! changes to h2so4 & nh3 gas (in mol/mol-air), limited by amounts available
        tmpa = 0.9999
        qh2so4_del = min( tmpa*qh2so4_cur, freduce*qmolso4a_del_max )
        qnh3_del   = min( tmpa*qnh3_cur, qh2so4_del*molenh4a_per_moleso4a )
        qh2so4_del = -qh2so4_del
        qnh3_del   = -qnh3_del

! changes to so4 & nh4 aerosol (in mol/mol-air)
        qso4a_del = -qh2so4_del
        qnh4a_del =   -qnh3_del
! change to aerosol number (in #/mol-air)
        qnuma_del = 1.0e-3*(qso4a_del*mw_so4a + qnh4a_del*mw_nh4a)/mass_part

! do the following (tmpa, tmpb, tmpc) calculations as a check
! max production of aerosol number (#/mol-air)
        tmpa = max( 0.0_r8, (ratenuclt_kk*dtnuc/cair) )
! adjusted production of aerosol number (#/mol-air)
        tmpb = tmpa*freduce
! relative difference from qnuma_del
        tmpc = (tmpb - qnuma_del)/max(tmpb, qnuma_del, 1.0e-35_r8)


!
! diagnostic output to fort.41
! (this should be commented-out or deleted in the wrf-chem version)
!
        if (ldiagaa <= 99) return!BSINGH - 05/28/2013(RCE updates)
        lun = lundiagaa

        icase = icase + 1
        if (abs(tmpc) .gt. abs(reldiffmax)) then
           reldiffmax = tmpc
           icase_reldiffmax = icase
        end if
!       do lun = 41, 51, 10
!       do lun = 6, 6
!          write(lun,'(/)')
           write(lun,'(a,2i9,1p,e10.2)')   &
               'vehkam bin-nuc icase, icase_rdmax =',   &
               icase, icase_reldiffmax, reldiffmax
           if (freduceb .lt. freducea) then
              if (abs(freducea-freduceb) .gt.   &
                   3.0e-7*max(freduceb,freducea)) write(lun,'(a,1p,2e15.7)')   &
                 'freducea, b =', freducea, freduceb
           end if
!       end do

! output factors so that output matches that of ternucl03
!       fogas  = 1.0e6                     ! convert mol/mol-air to ppm
!       foso4a = 1.0e9*mw_so4a/mw_air      ! convert mol-so4a/mol-air to ug/kg-air
!       fonh4a = 1.0e9*mw_nh4a/mw_air      ! convert mol-nh4a/mol-air to ug/kg-air
!       fonuma = 1.0e3/mw_air              ! convert #/mol-air to #/kg-air
        fogas  = 1.0
        foso4a = 1.0
        fonh4a = 1.0
        fonuma = 1.0

!       do lun = 41, 51, 10
!       do lun = 6, 6

        write(lun,'(a,2i5)') 'newnuc_method_flagaa/aa2',   &
           newnuc_method_flagaa, newnuc_method_flagaa2

        write(lun,9210)
        write(lun,9201) temp_in, rh_in,   &
           ratenuclt, 2.0*radius_cluster*1.0e-7, dpdry_part*1.0e2,   &
           voldry_part*1.0e6, float(igrow)
        write(lun,9215)
        write(lun,9201)   &
           qh2so4_avg*fogas, qnh3_avg*fogas,  &
           qh2so4_cur*fogas, qnh3_cur*fogas,  &
           qh2so4_del*fogas, qnh3_del*fogas,  &
           qso4a_del*foso4a, qnh4a_del*fonh4a

        write(lun,9220)
        write(lun,9201)   &
           dtnuc, dens_nh4so4a*1.0e-3,   &
           (qnh3_cur/qh2so4_cur), molenh4a_per_moleso4a,   &
           qnuma_del*fonuma, tmpb*fonuma, tmpc, freduce

!       end do

!       lun = 51
!       lun = 6
        write(lun,9230)
        write(lun,9201)   &
           press_in, cair*1.0e-6, so4vol_in,   &
           wet_volfrac_so4a, wetvol_dryvol, dens_part*1.0e-3

        if (igrow > 0) then
        write(lun,9240)
        write(lun,9201)   &
           tmp_spd, gr_kk, dnuc_kk, dfin_kk,   &
           gamma_kk, tmpa1, tmpb1, cs_kk

        write(lun,9250)
        write(lun,9201)   &
           cs_prime_kk, nu_kk, factor_kk, ratenuclt,   &
           ratenuclt_kk*1.0e-6
        end if

9201    format ( 1p, 40e10.2  )
9210    format (   &
        '      temp        rh',   &
        '   ratenuc  dia_clus ddry_part',   &
        ' vdry_part     igrow' )
9215    format (   &
        '  h2so4avg  h2so4pre',   &
        '  h2so4cur   nh3_cur',   &
        '  h2so4del   nh3_del',   &
        '  so4a_del  nh4a_del' )
9220    format (    &
        '     dtnuc    dens_a   nh/so g   nh/so a',   &
        '  numa_del  numa_dl2   reldiff   freduce' )
9230    format (   &
        '  press_in      cair so4_volin',   &
        ' wet_volfr wetv_dryv dens_part' )
9240    format (   &
        '   tmp_spd     gr_kk   dnuc_kk   dfin_kk',   &
        '  gamma_kk     tmpa1     tmpb1     cs_kk' )
9250    format (   &
        ' cs_pri_kk     nu_kk factor_kk ratenuclt',   &
        ' ratenu_kk' )


        return
        end subroutine mer07_veh02_nuc_mosaic_1box


!BSINGH - 05/28/2013(RCE updates)

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine pbl_nuc_wang2008( so4vol,   &
            newnuc_method_flagaa, newnuc_method_flagaa2,   &
            ratenucl, rateloge,   &
            cnum_tot, cnum_h2so4, cnum_nh3, radius_cluster )
!
! calculates boundary nucleation nucleation rate
! using the first or second-order parameterization in  
!     wang, m., and j.e. penner, 2008,
!        aerosol indirect forcing in a global model with particle nucleation,
!        atmos. chem. phys. discuss., 8, 13943-13998
!
        implicit none

! subr arguments (in)
        real(r8), intent(in) :: so4vol            ! concentration of h2so4 (molecules cm-3)
        integer, intent(in)  :: newnuc_method_flagaa  
                                ! [11,12] value selects [first,second]-order parameterization

! subr arguments (inout)
        integer, intent(inout)  :: newnuc_method_flagaa2
        real(r8), intent(inout) :: ratenucl         ! binary nucleation rate, j (# cm-3 s-1)
        real(r8), intent(inout) :: rateloge         ! log( ratenucl )

        real(r8), intent(inout) :: cnum_tot         ! total number of molecules
                                                    ! in the critical nucleus
        real(r8), intent(inout) :: cnum_h2so4       ! number of h2so4 molecules
        real(r8), intent(inout) :: cnum_nh3         ! number of nh3 molecules
        real(r8), intent(inout) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        real(r8) :: tmp_diam, tmp_mass, tmp_volu
        real(r8) :: tmp_rateloge, tmp_ratenucl

! executable


! nucleation rate
        if (newnuc_method_flagaa == 11) then
           tmp_ratenucl = 1.0e-6_r8 * so4vol
        else if (newnuc_method_flagaa == 12) then
           tmp_ratenucl = 1.0e-12_r8 * (so4vol**2)
        else
           return
        end if
        tmp_ratenucl = tmp_ratenucl * adjust_factor_pbl_ratenucl
        tmp_rateloge = log( tmp_ratenucl )

! exit if pbl nuc rate is lower than (incoming) ternary/binary rate
!       if (tmp_rateloge <= rateloge) return

        rateloge = tmp_rateloge
        ratenucl = tmp_ratenucl
        newnuc_method_flagaa2 = newnuc_method_flagaa

! following wang 2002, assume fresh nuclei are 1 nm diameter
!    subsequent code will "grow" them to aitken mode size
        radius_cluster = 0.5_r8

! assume fresh nuclei are pure h2so4
!    since aitken size >> initial size, the initial composition 
!    has very little impact on the results
        tmp_diam = radius_cluster * 2.0e-7_r8   ! diameter in cm
        tmp_volu = (tmp_diam**3) * (pi/6.0_r8)  ! volume in cm^3
        tmp_mass = tmp_volu * 1.8_r8            ! mass in g
        cnum_h2so4 = (tmp_mass / 98.0_r8) * 6.023e23_r8   ! no. of h2so4 molec assuming pure h2so4
        cnum_tot = cnum_h2so4
        cnum_nh3 = 0.0_r8


        return
        end subroutine pbl_nuc_wang2008

!BSINGH - 05/28/2013(RCE updates ENDS)


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine binary_nuc_vehk2002( temp, rh, so4vol,   &
            ratenucl, rateloge,   &
            cnum_h2so4, cnum_tot, radius_cluster )
!
! calculates binary nucleation rate and critical cluster size
! using the parameterization in  
!     vehkamäki, h., m. kulmala, i. napari, k.e.j. lehtinen,
!        c. timmreck, m. noppel and a. laaksonen, 2002,
!        an improved parameterization for sulfuric acid-water nucleation
!        rates for tropospheric and stratospheric conditions,
!        j. geophys. res., 107, 4622, doi:10.1029/2002jd002184
!
        implicit none

! subr arguments (in)
        real(r8), intent(in) :: temp              ! temperature (k)  
        real(r8), intent(in) :: rh                ! relative humidity (0-1)
        real(r8), intent(in) :: so4vol            ! concentration of h2so4 (molecules cm-3)

! subr arguments (out)
        real(r8), intent(out) :: ratenucl         ! binary nucleation rate, j (# cm-3 s-1)
        real(r8), intent(out) :: rateloge         ! log( ratenucl )

        real(r8), intent(out) :: cnum_h2so4       ! number of h2so4 molecules
                                                  ! in the critical nucleus
        real(r8), intent(out) :: cnum_tot         ! total number of molecules
                                                  ! in the critical nucleus
        real(r8), intent(out) :: radius_cluster   ! the radius of cluster (nm)


! local variables
        real(r8) :: crit_x
        real(r8) :: acoe, bcoe, ccoe, dcoe, ecoe, fcoe, gcoe, hcoe, icoe, jcoe
        real(r8) :: tmpa, tmpb

! executable


! calc sulfuric acid mole fraction in critical cluster
        crit_x = 0.740997 - 0.00266379 * temp   &
               - 0.00349998 * log (so4vol)   &
               + 0.0000504022 * temp * log (so4vol)   &
               + 0.00201048 * log (rh)   &
               - 0.000183289 * temp * log (rh)   &
               + 0.00157407 * (log (rh)) ** 2.0   &
               - 0.0000179059 * temp * (log (rh)) ** 2.0   &
               + 0.000184403 * (log (rh)) ** 3.0   &
               - 1.50345e-6 * temp * (log (rh)) ** 3.0


! calc nucleation rate
        acoe    = 0.14309+2.21956*temp   &
                - 0.0273911 * temp**2.0   &
                + 0.0000722811 * temp**3.0 + 5.91822/crit_x

        bcoe    = 0.117489 + 0.462532 *temp   &
                - 0.0118059 * temp**2.0   &
                + 0.0000404196 * temp**3.0 + 15.7963/crit_x

        ccoe    = -0.215554-0.0810269 * temp   &
                + 0.00143581 * temp**2.0   &
                - 4.7758e-6 * temp**3.0   &
                - 2.91297/crit_x

        dcoe    = -3.58856+0.049508 * temp   &
                - 0.00021382 * temp**2.0   &
                + 3.10801e-7 * temp**3.0   &
                - 0.0293333/crit_x

        ecoe    = 1.14598 - 0.600796 * temp   &
                + 0.00864245 * temp**2.0   &
                - 0.0000228947 * temp**3.0   &
                - 8.44985/crit_x

        fcoe    = 2.15855 + 0.0808121 * temp   &
                -0.000407382 * temp**2.0   &
                -4.01957e-7 * temp**3.0   &
                + 0.721326/crit_x

        gcoe    = 1.6241 - 0.0160106 * temp   &
                + 0.0000377124 * temp**2.0   &
                + 3.21794e-8 * temp**3.0   &
                - 0.0113255/crit_x

        hcoe    = 9.71682 - 0.115048 * temp   &
                + 0.000157098 * temp**2.0   &
                + 4.00914e-7 * temp**3.0   &
                + 0.71186/crit_x

        icoe    = -1.05611 + 0.00903378 * temp   &
                - 0.0000198417 * temp**2.0   &
                + 2.46048e-8  * temp**3.0   &
                - 0.0579087/crit_x

        jcoe    = -0.148712 + 0.00283508 * temp   &
                - 9.24619e-6  * temp**2.0   &
                + 5.00427e-9 * temp**3.0   &
                - 0.0127081/crit_x

        tmpa     =     (   &
                  acoe   &
                + bcoe * log (rh)   &
                + ccoe * ( log (rh))**2.0   &
                + dcoe * ( log (rh))**3.0   &
                + ecoe * log (so4vol)   &
                + fcoe * (log (rh)) * (log (so4vol))   &
                + gcoe * ((log (rh) ) **2.0)   &
                       * (log (so4vol))   &
                + hcoe * (log (so4vol)) **2.0   &
                + icoe * log (rh)   &
                       * ((log (so4vol)) **2.0)   &
                + jcoe * (log (so4vol)) **3.0   &
                )
        rateloge = tmpa
        tmpa = min( tmpa, log(1.0e38_r8) )
        ratenucl = exp ( tmpa )
!       write(*,*) 'tmpa, ratenucl =', tmpa, ratenucl



! calc number of molecules in critical cluster
        acoe    = -0.00295413 - 0.0976834*temp   &
                + 0.00102485 * temp**2.0   &
                - 2.18646e-6 * temp**3.0 - 0.101717/crit_x

        bcoe    = -0.00205064 - 0.00758504*temp   &
                + 0.000192654 * temp**2.0   &
                - 6.7043e-7 * temp**3.0 - 0.255774/crit_x

        ccoe    = +0.00322308 + 0.000852637 * temp   &
                - 0.0000154757 * temp**2.0   &
                + 5.66661e-8 * temp**3.0   &
                + 0.0338444/crit_x

        dcoe    = +0.0474323 - 0.000625104 * temp   &
                + 2.65066e-6 * temp**2.0   &
                - 3.67471e-9 * temp**3.0   &
                - 0.000267251/crit_x

        ecoe    = -0.0125211 + 0.00580655 * temp   &
                - 0.000101674 * temp**2.0   &
                + 2.88195e-7 * temp**3.0   &
                + 0.0942243/crit_x

        fcoe    = -0.038546 - 0.000672316 * temp   &
                + 2.60288e-6 * temp**2.0   &
                + 1.19416e-8 * temp**3.0   &
                - 0.00851515/crit_x

        gcoe    = -0.0183749 + 0.000172072 * temp   &
                - 3.71766e-7 * temp**2.0   &
                - 5.14875e-10 * temp**3.0   &
                + 0.00026866/crit_x

        hcoe    = -0.0619974 + 0.000906958 * temp   &
                - 9.11728e-7 * temp**2.0   &
                - 5.36796e-9 * temp**3.0   &
                - 0.00774234/crit_x

        icoe    = +0.0121827 - 0.00010665 * temp   &
                + 2.5346e-7 * temp**2.0   &
                - 3.63519e-10 * temp**3.0   &
                + 0.000610065/crit_x

        jcoe    = +0.000320184 - 0.0000174762 * temp   &
                + 6.06504e-8 * temp**2.0   &
                - 1.4177e-11 * temp**3.0   &
                + 0.000135751/crit_x

        cnum_tot = exp (   &
                  acoe   &
                + bcoe * log (rh)   &
                + ccoe * ( log (rh))**2.0   &
                + dcoe * ( log (rh))**3.0   &
                + ecoe * log (so4vol)   &
                + fcoe * (log (rh)) * (log (so4vol))   &
                + gcoe * ((log (rh) ) **2.0)   &
                       * (log (so4vol))   &
                + hcoe * (log (so4vol)) **2.0   &
                + icoe * log (rh)   &
                       * ((log (so4vol)) **2.0)   &
                + jcoe * (log (so4vol)) **3.0   &
                )

        cnum_h2so4 = cnum_tot * crit_x

!   calc radius (nm) of critical cluster
        radius_cluster = exp( -1.6524245 + 0.42316402*crit_x   &
                              + 0.3346648*log(cnum_tot) )
      

      return
      end subroutine binary_nuc_vehk2002



!----------------------------------------------------------------------
!----------------------------------------------------------------------
subroutine ternary_nuc_merik2007( t, rh, c2, c3, j_log, ntot, nacid, namm, r )
!subroutine ternary_fit(          t, rh, c2, c3, j_log, ntot, nacid, namm, r )
! *************************** ternary_fit.f90 ********************************
! joonas merikanto, 2006
!
! fortran 90 routine that calculates the parameterized composition 
! and nucleation rate of critical clusters in h2o-h2so4-nh3 vapor
!
! warning: the fit should not be used outside its limits of validity
! (limits indicated below)
!
! in:
! t:     temperature (k), limits 235-295 k
! rh:    relative humidity as fraction (eg. 0.5=50%) limits 0.05-0.95
! c2:    sulfuric acid concentration (molecules/cm3) limits 5x10^4 - 10^9 molecules/cm3
! c3:    ammonia mixing ratio (ppt) limits 0.1 - 1000 ppt
!
! out:
! j_log: logarithm of nucleation rate (1/(s cm3))
! ntot:  total number of molecules in the critical cluster
! nacid: number of sulfuric acid molecules in the critical cluster
! namm:  number of ammonia molecules in the critical cluster
! r:     radius of the critical cluster (nm)
!  ****************************************************************************
implicit none

real(r8), intent(in) :: t, rh, c2, c3
real(r8), intent(out) :: j_log, ntot, nacid, namm, r
real(r8) :: j, t_onset

t_onset=143.6002929064716 + 1.0178856665693992*rh + &
   10.196398812974294*log(c2) - &
   0.1849879416839113*log(c2)**2 - 17.161783213150173*log(c3) + &
   (109.92469248546053*log(c3))/log(c2) + &
   0.7734119613144357*log(c2)*log(c3) - 0.15576469879527022*log(c3)**2

if(t_onset.gt.t) then 

   j_log=-12.861848898625231 + 4.905527742256349*c3 - 358.2337705052991*rh -& 
   0.05463019231872484*c3*t + 4.8630382337426985*rh*t + &
   0.00020258394697064567*c3*t**2 - 0.02175548069741675*rh*t**2 - &
   2.502406532869512e-7*c3*t**3 + 0.00003212869941055865*rh*t**3 - &
   4.39129415725234e6/log(c2)**2 + (56383.93843154586*t)/log(c2)**2 -& 
   (239.835990963361*t**2)/log(c2)**2 + &
   (0.33765136625580167*t**3)/log(c2)**2 - &
   (629.7882041830943*rh)/(c3**3*log(c2)) + &
   (7.772806552631709*rh*t)/(c3**3*log(c2)) - &
   (0.031974053936299256*rh*t**2)/(c3**3*log(c2)) + &
   (0.00004383764128775082*rh*t**3)/(c3**3*log(c2)) + &
   1200.472096232311*log(c2) - 17.37107890065621*t*log(c2) + &
   0.08170681335921742*t**2*log(c2) - 0.00012534476159729881*t**3*log(c2) - &
   14.833042158178936*log(c2)**2 + 0.2932631303555295*t*log(c2)**2 - &
   0.0016497524241142845*t**2*log(c2)**2 + &
   2.844074805239367e-6*t**3*log(c2)**2 - 231375.56676032578*log(c3) - &
   100.21645273730675*rh*log(c3) + 2919.2852552424706*t*log(c3) + &
   0.977886555834732*rh*t*log(c3) - 12.286497122264588*t**2*log(c3) - &
   0.0030511783284506377*rh*t**2*log(c3) + &
   0.017249301826661612*t**3*log(c3) + 2.967320346100855e-6*rh*t**3*log(c3) + &
   (2.360931724951942e6*log(c3))/log(c2) - &
   (29752.130254319443*t*log(c3))/log(c2) + &
   (125.04965118142027*t**2*log(c3))/log(c2) - &
   (0.1752996881934318*t**3*log(c3))/log(c2) + &
   5599.912337254629*log(c2)*log(c3) - 70.70896612937771*t*log(c2)*log(c3) + &
   0.2978801613269466*t**2*log(c2)*log(c3) - &
   0.00041866525019504*t**3*log(c2)*log(c3) + 75061.15281456841*log(c3)**2 - &
   931.8802278173565*t*log(c3)**2 + 3.863266220840964*t**2*log(c3)**2 - &
   0.005349472062284983*t**3*log(c3)**2 - &
   (732006.8180571689*log(c3)**2)/log(c2) + &
   (9100.06398573816*t*log(c3)**2)/log(c2) - &
   (37.771091915932004*t**2*log(c3)**2)/log(c2) + &
   (0.05235455395566905*t**3*log(c3)**2)/log(c2) - &
   1911.0303773001353*log(c2)*log(c3)**2 + &
   23.6903969622286*t*log(c2)*log(c3)**2 - &
   0.09807872005428583*t**2*log(c2)*log(c3)**2 + &
   0.00013564560238552576*t**3*log(c2)*log(c3)**2 - &
   3180.5610833308*log(c3)**3 + 39.08268568672095*t*log(c3)**3 - &
   0.16048521066690752*t**2*log(c3)**3 + &
   0.00022031380023793877*t**3*log(c3)**3 + &
   (40751.075322248245*log(c3)**3)/log(c2) - &
   (501.66977622013934*t*log(c3)**3)/log(c2) + &
   (2.063469732254135*t**2*log(c3)**3)/log(c2) - &
   (0.002836873785758324*t**3*log(c3)**3)/log(c2) + &
   2.792313345723013*log(c2)**2*log(c3)**3 - &
   0.03422552111802899*t*log(c2)**2*log(c3)**3 + &
   0.00014019195277521142*t**2*log(c2)**2*log(c3)**3 - &
   1.9201227328396297e-7*t**3*log(c2)**2*log(c3)**3 - &
   980.923146020468*log(rh) + 10.054155220444462*t*log(rh) - &
   0.03306644502023841*t**2*log(rh) + 0.000034274041225891804*t**3*log(rh) + &
   (16597.75554295064*log(rh))/log(c2) - &
   (175.2365504237746*t*log(rh))/log(c2) + &
   (0.6033215603167458*t**2*log(rh))/log(c2) - &
   (0.0006731787599587544*t**3*log(rh))/log(c2) - &
   89.38961120336789*log(c3)*log(rh) + 1.153344219304926*t*log(c3)*log(rh) - &
   0.004954549700267233*t**2*log(c3)*log(rh) + &
   7.096309866238719e-6*t**3*log(c3)*log(rh) + &
   3.1712136610383244*log(c3)**3*log(rh) - &
   0.037822330602328806*t*log(c3)**3*log(rh) + &
   0.0001500555743561457*t**2*log(c3)**3*log(rh) - &
   1.9828365865570703e-7*t**3*log(c3)**3*log(rh)

   j=exp(j_log)

   ntot=57.40091052369212 - 0.2996341884645408*t + &
   0.0007395477768531926*t**2 - &
   5.090604835032423*log(c2) + 0.011016634044531128*t*log(c2) + &
   0.06750032251225707*log(c2)**2 - 0.8102831333223962*log(c3) + &
   0.015905081275952426*t*log(c3) - 0.2044174683159531*log(c2)*log(c3) + &
   0.08918159167625832*log(c3)**2 - 0.0004969033586666147*t*log(c3)**2 + &
   0.005704394549007816*log(c3)**3 + 3.4098703903474368*log(j) - &
   0.014916956508210809*t*log(j) + 0.08459090011666293*log(c3)*log(j) - &
   0.00014800625143907616*t*log(c3)*log(j) + 0.00503804694656905*log(j)**2
 
   r=3.2888553966535506e-10 - 3.374171768439839e-12*t + &
   1.8347359507774313e-14*t**2 + 2.5419844298881856e-12*log(c2) - &
   9.498107643050827e-14*t*log(c2) + 7.446266520834559e-13*log(c2)**2 + &
   2.4303397746137294e-11*log(c3) + 1.589324325956633e-14*t*log(c3) - &
   2.034596219775266e-12*log(c2)*log(c3) - 5.59303954457172e-13*log(c3)**2 - &
   4.889507104645867e-16*t*log(c3)**2 + 1.3847024107506764e-13*log(c3)**3 + &
   4.141077193427042e-15*log(j) - 2.6813110884009767e-14*t*log(j) + &
   1.2879071621313094e-12*log(c3)*log(j) - &
   3.80352446061867e-15*t*log(c3)*log(j) - 1.8790172502456827e-14*log(j)**2
 
   nacid=-4.7154180661803595 + 0.13436423483953885*t - & 
   0.00047184686478816176*t**2 - & 
   2.564010713640308*log(c2) + 0.011353312899114723*t*log(c2) + &
   0.0010801941974317014*log(c2)**2 + 0.5171368624197119*log(c3) - &
   0.0027882479896204665*t*log(c3) + 0.8066971907026886*log(c3)**2 - & 
   0.0031849094214409335*t*log(c3)**2 - 0.09951184152927882*log(c3)**3 + &
   0.00040072788891745513*t*log(c3)**3 + 1.3276469271073974*log(j) - &
   0.006167654171986281*t*log(j) - 0.11061390967822708*log(c3)*log(j) + &
   0.0004367575329273496*t*log(c3)*log(j) + 0.000916366357266258*log(j)**2
 
   namm=71.20073903979772 - 0.8409600103431923*t + &
   0.0024803006590334922*t**2 + &
   2.7798606841602607*log(c2) - 0.01475023348171676*t*log(c2) + &
   0.012264508212031405*log(c2)**2 - 2.009926050440182*log(c3) + &
   0.008689123511431527*t*log(c3) - 0.009141180198955415*log(c2)*log(c3) + &
   0.1374122553905617*log(c3)**2 - 0.0006253227821679215*t*log(c3)**2 + &
   0.00009377332742098946*log(c3)**3 + 0.5202974341687757*log(j) - &
   0.002419872323052805*t*log(j) + 0.07916392322884074*log(c3)*log(j) - &
   0.0003021586030317366*t*log(c3)*log(j) + 0.0046977006608603395*log(j)**2

else
! nucleation rate less that 5e-6, setting j_log arbitrary small
   j_log=-300.
end if

return

end  subroutine ternary_nuc_merik2007



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
        subroutine wexler_nuc_mosaic_1box(   &
           dtnuc, temp_in, rh_in, cair,   &
           qh2so4_avg, qh2so4_cur, qnh3_avg, qnh3_cur,   &
           nsize, maxd_asize, volumlo_sect, volumhi_sect,   &
           isize_nuc, qnuma_del, qso4a_del, qnh4a_del,   &
           qh2so4_del, qnh3_del, dens_nh4so4a )
!.......................................................................
!
! calculates new particle production from h2so4-h2o binary nucleation
!    over timestep dtnuc, using the wexler et al. (1994) parameterization
!
! the size of new particles is the lower-bound size of the host code's 
!    smallest size bin.  their composition is so4 and nh4, since the nuclei
!    would incorporate nh3 as they grow from ~1 nm to the lower-bound size.
!    (the new particle composition can be forced to pure so4 by setting
!    the qnh3_avg & qnh3_cur input arguments to 0.0).
!
! revision history
!    coded by rc easter, pnnl, 20-mar-2006
!
! key routines called: none
!
! references:
!    wexler, a. s., f. w. lurmann, and j. h. seinfeld,
!       modelling urban and regional aerosols -- i.  model development,
!       atmos. environ., 28, 531-546, 1994.
!
!.......................................................................
        implicit none

! subr arguments (in)
        real(r8), intent(in) :: dtnuc             ! nucleation time step (s)
        real(r8), intent(in) :: temp_in           ! temperature, in k
        real(r8), intent(in) :: rh_in             ! relative humidity, as fraction
        real(r8), intent(in) :: cair              ! dry-air molar density (mole-air/cm3)

        real(r8), intent(in) :: qh2so4_avg, qh2so4_cur   ! gas h2so4 mixing ratios (ppm)
        real(r8), intent(in) :: qnh3_avg, qnh3_cur       ! gas nh3 mixing ratios (ppm)
             ! qxxx_cur = current value (at end of condensation)
             ! qxxx_avg = average value (from start to end of condensation)

        integer, intent(in) :: nsize                    ! number of aerosol size bins
        integer, intent(in) :: maxd_asize               ! dimension for volumlo_sect, ...
        real(r8), intent(in) :: volumlo_sect(maxd_asize)    ! dry volume at lower bnd of bin (cm3)
        real(r8), intent(in) :: volumhi_sect(maxd_asize)    ! dry volume at upper bnd of bin (cm3)

! subr arguments (out)
        integer, intent(out) :: isize_nuc     ! size bin into which new particles go
        real(r8), intent(out) :: qnuma_del        ! change to aerosol number mixing ratio (#/kg)
        real(r8), intent(out) :: qso4a_del        ! change to aerosol so4 mixing ratio (ug/kg)
        real(r8), intent(out) :: qnh4a_del        ! change to aerosol nh4 mixing ratio (ug/kg)
        real(r8), intent(out) :: qh2so4_del       ! change to gas h2so4 mixing ratio (ppm)
        real(r8), intent(out) :: qnh3_del         ! change to gas nh3 mixing ratio (ppm)
                                              ! aerosol changes are > 0; gas changes are < 0

! subr arguments (inout)
        real(r8), intent(inout) :: dens_nh4so4a   ! dry-density of the new nh4-so4 aerosol mass (g/cm3)
                                              ! use 'in' value only if it is between 1.5-2.0 g/cm3

! local variables
        integer i
        integer, save :: icase = 0, icase_reldiffmax = 0

        real(r8), parameter :: pi = 3.1415926536
        real(r8), parameter :: avogad = 6.022e23   ! avogadro number (molecules/mole)
        real(r8), parameter :: mw_air = 28.966     ! dry-air mean molecular weight (g/mole)

! dry densities (g/cm3) molecular weights of aerosol 
! ammsulf, ammbisulf, and sulfacid (from mosaic  dens_electrolyte values)
        real(r8), parameter :: dens_ammsulf   = 1.769
        real(r8), parameter :: dens_ammbisulf = 1.78
        real(r8), parameter :: dens_sulfacid  = 1.841

! molecular weights (g/mole) of aerosol ammsulf, ammbisulf, and sulfacid
!    for ammbisulf and sulfacid, use 114 & 96 here rather than 115 & 98
!    because we don't keep track of aerosol hion mass
        real(r8), parameter :: mw_ammsulf   = 132.0
        real(r8), parameter :: mw_ammbisulf = 114.0
        real(r8), parameter :: mw_sulfacid  =  96.0
! molecular weights of aerosol sulfate and ammonium
        real(r8), parameter :: mw_so4a      =  96.0
        real(r8), parameter :: mw_nh4a      =  18.0

        real(r8), save :: reldiffmax = 0.0

        real(r8) ch2so4_crit              ! critical h2so4 conc (ug/m3)
        real(r8) dens_part                ! "grown" single-particle dry density (g/cm3)
        real(r8) duma, dumb, dumc, dume
        real(r8) dum_m1, dum_m2, dum_m3, dum_n1, dum_n2, dum_n3
        real(r8) fogas, foso4a, fonh4a, fonuma
        real(r8) mass_part                ! "grown" single-particle mass (g)
        real(r8) molenh4a_per_moleso4a    ! (mole aerosol nh4)/(mole aerosol so4)
        real(r8) qh2so4_crit              ! critical h2so4 mixrat (ppm)
        real(r8) qh2so4_avail             ! amount of h2so4 available for new particles (ppm)
        real(r8) vol_part                 ! "grown" single-particle volume (cm3)


!
! initialization output arguments with "zero nucleation" values
!
        isize_nuc = 1
        qnuma_del = 0.0
        qso4a_del = 0.0
        qnh4a_del = 0.0
        qh2so4_del = 0.0
        qnh3_del = 0.0

!
! calculate critical h2so4 concentration (ug/m3) and mixing ratio (mole/mole-air)
!
        ch2so4_crit = 0.16 * exp( 0.1*temp_in - 3.5*rh_in - 27.7 )
! ch2so4 = (ug-h2so4/m3-air)
! ch2so4*1.0e-12/mwh2so4 = (mole-h2so4/cm3-air)
        qh2so4_crit = (ch2so4_crit*1.0e-12/98.0)/cair
        qh2so4_avail = qh2so4_cur - qh2so4_crit

! if "available" h2so4 vapor < 4.0e-18 mole/mole-air ~= 1.0e2 molecules/cm3, 
! exit with new particle formation = 0
        if (qh2so4_avail .le. 4.0e-18) then
           return
        end if

! determine size bin into which the new particles go
        isize_nuc = 1
        vol_part = volumlo_sect(1)

!
! determine composition and density of the "grown particles"
! the grown particles are assumed to be liquid 
!    (since critical clusters contain water)
!    so any (nh4/so4) molar ratio between 0 and 2 is allowed
! assume that the grown particles will have 
!    (nh4/so4 molar ratio) = min( 2, (nh3/h2so4 gas molar ratio) )
!
        if (qnh3_cur .ge. qh2so4_avail) then
! combination of ammonium sulfate and ammonium bisulfate
! dum_n1 & dum_n2 = mole fractions of the ammsulf & ammbisulf
           dum_n1 = (qnh3_cur/qh2so4_avail) - 1.0
           dum_n1 = max( 0.0_r8, min( 1.0_r8, dum_n1 ) )
           dum_n2 = 1.0 - dum_n1
           dum_n3 = 0.0
        else
! combination of ammonium bisulfate and sulfuric acid
! dum_n2 & dum_n3 = mole fractions of the ammbisulf & sulfacid
           dum_n1 = 0.0
           dum_n2 = (qnh3_cur/qh2so4_avail)
           dum_n2 = max( 0.0_r8, min( 1.0_r8, dum_n2 ) )
           dum_n3 = 1.0 - dum_n2
	end if

        dum_m1 = dum_n1*mw_ammsulf
        dum_m2 = dum_n2*mw_ammbisulf
        dum_m3 = dum_n3*mw_sulfacid
        dens_part = (dum_m1 + dum_m2 + dum_m3)/   &
           ((dum_m1/dens_ammsulf) + (dum_m2/dens_ammbisulf)   &
                                  + (dum_m3/dens_sulfacid))
! 25-jul-2006 - use 'in' value only if it is between 1.5-2.0 g/cm3
	if (abs(dens_nh4so4a-1.75) .le. 0.25) then
	    dens_part = dens_nh4so4a
	else
            dens_nh4so4a = dens_part
	end if
        mass_part  = vol_part*dens_part 
        molenh4a_per_moleso4a = 2.0*dum_n1 + dum_n2


! changes to h2so4 & nh3 gas (in mole/mole-air), limited by amounts available
        duma = 0.9999
        qh2so4_del = min( duma*qh2so4_cur, qh2so4_avail )
        qnh3_del   = min( duma*qnh3_cur, qh2so4_del*molenh4a_per_moleso4a )
        qh2so4_del = -qh2so4_del
        qnh3_del   = -qnh3_del

! changes to so4 & nh4 aerosol (in mole/mole-air)
        qso4a_del = -qh2so4_del
        qnh4a_del =   -qnh3_del
! change to aerosol number (in #/mole-air)
        qnuma_del = (qso4a_del*mw_so4a + qnh4a_del*mw_nh4a)/mass_part


        return
        end subroutine wexler_nuc_mosaic_1box




!-----------------------------------------------------------------------



	end module module_mosaic_newnucb
