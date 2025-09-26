!************************************************************************
! This computer software was prepared by Battelle Memorial Institute,
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Chemistry Option:  CBMZ (Carbon Bond Mechanism IV - Zaveri)
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
! of the code, or Jerome Fast, the WRF-chem implementation team leader.
!
! Terms of Use:
!  1) Users are requested to consult the primary author prior to
!     modifying the CBMZ code or incorporating it or its submodules in
!     another code. This is meant to ensure that the any linkages and/or
!     assumptions will not adversely affect the operation of CBMZ.
!  2) The CBMZ source code is intended for research and educational
!     purposes. Users are requested to contact the primary author
!     regarding the use of CBMZ code for any commercial application.
!  3) Users preparing publications resulting from the usage of CBMZ are
!     requested to cite one or more of the references below (depending on
!     the application) for proper acknowledgement.
!
! References: 
! 1) Zaveri R.A., and L.K. Peters (1999), A new lumped structure
!    photochemical mechanism for large-scale applications, J. Geophys.
!    Res., 104, 30387-30415.
! 2) Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!    Barnard, E.G., Chapman, G.A. Grell, and S.E. Peckham (2005),
!    Evolution of ozone, particulates, and aerosol direct radiative
!    forcing in the vicinity of Houston using a fully-coupled
!    meteorology- chemistry-aerosol model, J. Geophys. Res., 111, D21305,
!    doi:10.1029/2005JD006721.
!
! Additional information:
! * www.pnl.gov/atmospheric/research/wrf-chem
!
! Support: 
! Funding for developing and evaluating CBMZ was provided by the U.S.
! Department of Energy under the auspices of Atmospheric Science Program
! of the Office of Biological and Environmental Research the the PNNL
! Laboratory Research and Directed Research and Development program.
!************************************************************************
      module module_cbmz



      use module_peg_util

      contains


!***********************************************************************
! < 1.> subr cbmz_driver
!
! purpose: serves as an interface between subr. gas_chemistry and
!          the actual solver subr such as lsodes, rodas, etc.
!
! grid   : fixed i,j,k  (box-model)
!
! author : Rahul A. Zaveri
! date   : November 1998
!
!-----------------------------------------------------------------------

      subroutine cbmz_driver( &
               id, curr_secs, ktau, dtstep, ktauc, dtstepc, &
               config_flags, gmt, julday, t_phy, moist, p8w, t8w, &
               p_phy, chem, rho_phy, dz8w, z, z_at_w, vdrog3, &
               ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, ph_hno2, &
               ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, ph_ch2om, &
               ph_ch3o2h, ph_n2o5, &
               ids,ide, jds,jde, kds,kde, &
               ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte  )

   USE module_configure, only:  grid_config_rec_type, num_moist, num_chem,  &
!	p_qv, p_so2, p_ho2, p_so4aj, p_corn, p_hcl, p_mtf
!jdf
	p_qv, p_so2, p_ho2, p_so4aj, p_corn, p_hcl, p_mtf, &
        p_xyl, p_tol, p_csl, p_oli, p_olt, p_ho, p_o3, p_no
!jdf
   USE module_data_sorgam, only:  ldrog
   USE module_data_cbmz
   IMPLICIT NONE


!-----------------------------------------------------------------------
! subr arguments

   INTEGER, INTENT(IN ) :: id, julday, &
                           ids,ide, jds,jde, kds,kde, &
                           ims,ime, jms,jme, kms,kme, &
                           its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN ) :: ktau, ktauc

   REAL(KIND=8), INTENT(IN) :: curr_secs

   REAL, INTENT(IN ) :: dtstep, dtstepc, gmt
!
! advected moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), &
         INTENT(IN ) :: moist
!
! advected chemical tracers
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
         INTENT(INOUT ) :: chem
!
! arrays that hold photolysis rates
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), &
         INTENT(INOUT ) :: &
           ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, ph_hno2, &
           ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, ph_ch2om, &
           ph_ch3o2h, ph_n2o5
!
! on input from met model
!
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
          INTENT(IN ) :: &
                         t_phy, &	! temperature
                         rho_phy, &	! air density (kg/m3)
                         p_phy, &	! NOT USED
                         z, z_at_w, &	! NOT USED
                         dz8w, &	! NOT USED
                         t8w, p8w	! NOT USED
!
! for interaction with aerosols (really is output)
!
   REAL, DIMENSION( ims:ime , kms:kme-0 , jms:jme , ldrog ) , &
          INTENT(INOUT ) :: &
                         vdrog3		! NOT USED

   TYPE(grid_config_rec_type), INTENT(IN ) :: config_flags


!-----------------------------------------------------------------------


!   local variables
	integer :: idum, iok,iprog
	integer :: iregime
	integer :: igaschem_allowed_m1, igaschem_allowed_m2,   &
                   igaschem_allowed_m3, igaschem_allowed_m4
	integer :: igas_solver, iregime_forced
	integer :: i_boxtest_units_convert
	integer :: i_print_gasode_stats
	integer :: i_force_dump, mode_force_dump
	integer :: it, jt, kt
	integer :: jsolver
	integer :: lunerr, lunout, levdbg_err, levdbg_info
	integer :: mgaschem

    real(kind=8) :: trun
	real :: abs_error, rel_error
	real :: dtchem, tchem, tstart, tstop
	real :: airdenbox, pressbox, tempbox
	real :: cair_mlc
	real :: h2o, ch4, oxygen, nitrogen, hydrogen
	real :: cboxnew(ngas_z), cboxold(ngas_z),prodrog(ldrog) !MS
	real :: Aperox(nperox,nperox), Bperox(nperox,nperox)
	real :: rk_param(nperox), rk_photo(nphoto)
	real :: rk_m1(nrxn_m1), rk_m2(nrxn_m2), rk_m3(nrxn_m3), rk_m4(nrxn_m4)
        real factorprog
	integer, dimension(2,6), save :: inforodas=0
	integer, dimension(6), save :: iodestatus_count=0, ioderegime_count=0

#ifdef CHEM_DBG_I
!rcetestb diagnostics --------------------------------------------------
	print 93010, ' '
	print 93010, 'rcetestb diagnostics from cbmz_driver'
	print 93010, 'id, chem_opt, ktau, ktauc, julday    ',   &
	              id, config_flags%chem_opt, ktau, ktauc, julday
	print 93020, 'dtstep, dtstepc, gmt                 ',   &
	              dtstep, dtstepc, gmt
	print 93010, 'ids/e, j, k', ids, ide, jds, jde, kds, kde
	print 93010, 'ims/e, j, k', ims, ime, jms, jme, kms, kme
	print 93010, 'its/e, j, k', its, ite, jts, jte, kts, kte
	print 93010, 'num_moist, p_qv              ', num_moist, p_qv
	print 93010, 'num_chem, p_so2, p_ho2       ', num_chem, p_so2, p_ho2
	print 93010, 'p_so4aj, p_corn, p_hcl, p_mtf', p_so4aj, p_corn, p_hcl, p_mtf
93010	format( a, 8(1x,i6) )
93020	format( a, 8(1p,e14.6) )
!rcetestb diagnostics --------------------------------------------------
#endif


!   set some control variables to their "standard for wrf-chem" values
	igas_solver = 1
	iregime_forced = -1
	mgaschem = +1
	i_boxtest_units_convert = 0

	i_print_gasode_stats = 1
	mode_force_dump = 0
	lunerr = -1
	lunout = -1
	levdbg_err = 0
	levdbg_info = 15

	abs_error = 1.0e1	! solver absolute tolerance (molecules/cm3)
	rel_error = 1.0e-3	! solver relative tolerance

!   set some control variables to non-standard values for testing
!   force dumps for center column, every 3rd level
!	mode_force_dump = +77
!   force dumps for center column, 1st level
!	mode_force_dump = +7
!   do levdbg_info output always
	levdbg_info = 0


!   following call is for boxwrf testing only
!   it must be commented out for actual wrf applications
!	call boxtest_get_extra_args( &
!	    igas_solver, iregime_forced, &
!	    i_boxtest_units_convert, lunerr, lunout, &
!	    abs_error, rel_error, trun   )


!   currently nothing is done with vdrog3
!	vdrog3(its:ite,kts:kte,jts:jte,:) = 0.0  !This is already set to zero in chem_driver.

!   initialize rate constant arrays to 0.0 since not all elements are set
!   to valid values before some were used in loops, wig 16-Nov-2007
    rk_m1(:) = 0.
    rk_m2(:) = 0.
    rk_m3(:) = 0.
    rk_m4(:) = 0.

!   determine which regimes are allowed
!   based on which gas species are "active"
	call set_gaschem_allowed_regimes( lunerr,   &
		igaschem_allowed_m1, igaschem_allowed_m2,   &
		igaschem_allowed_m3, igaschem_allowed_m4 )

!
!   main loop -- do gas chemistry at each i,j,k
!
	do 2900 jt = jts, jte
	do 2900 kt = kts, kte
	do 2900 it = its, ite

	trun   = curr_secs              ! run time in s
	tchem  = mod( real(gmt*3600.0,8)+trun, 86400._8 ) ! time from 00 UTC in s
	dtchem = dtstepc
	tstart = tchem                  ! s
	tstop  = tstart + dtchem        ! s

!   skip integration for very small dtchem
	if ((tstop-tstart) .le. 1.0e-5) goto 2900


!   initial species mapping from host array
	call mapgas_tofrom_host( 0,                    &
		i_boxtest_units_convert,               &
		it,jt,kt, ims,ime, jms,jme, kms,kme,   &
		num_moist, num_chem, moist, chem,      &
		t_phy, p_phy, rho_phy,                 &
		cboxold, tempbox, pressbox, airdenbox, &
		cair_mlc,                              &
		h2o, ch4, oxygen, nitrogen, hydrogen   )
	cboxnew(:) = cboxold(:)

!   determine regime
	call selectgasregime( iregime, iregime_forced, cboxold,   &
		igaschem_allowed_m1, igaschem_allowed_m2,   &
		igaschem_allowed_m3, igaschem_allowed_m4 )
	idum = iregime
	if ((idum .lt. 1) .or. (idum .ge. 6)) idum = 6
	ioderegime_count(idum) = ioderegime_count(idum) + 1
	iodestatus_count(6) = iodestatus_count(6) + 1

!   compute rate constants
!   transfer/map incoming photolysis rate contants to local array
      call gasphotoconstants( rk_photo,   &
	    i_boxtest_units_convert,               &
	    it,jt,kt, ims,ime, jms,jme, kms,kme,   &
	    ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, ph_hno2, &
	    ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, ph_ch2om, &
	    ph_ch3o2h, ph_n2o5 )
!   loads Aperox and Bperox
	call loadperoxyparameters( Aperox, Bperox )
!   calculate parameterized rate constants
	call peroxyrateconstants( tempbox, cboxold,   &
		 Aperox, Bperox, rk_param )
!   calculate thermal rate constants
	call gasrateconstants( iregime, tempbox, cair_mlc,   &
		rk_photo, rk_param, rk_m1, rk_m2, rk_m3, rk_m4 )

!   mode_force_dump selects a detailed dump of gaschem at either
!   first ijk grid, first ij column, all ijk, or no ijk
	i_force_dump = 0
	if (mode_force_dump .eq. 1) then
	    if ((it.eq.its) .and. (jt.eq.jts)   &
	                    .and. (kt.eq.kts)) i_force_dump = 1
	else if (mode_force_dump .eq. 10) then
	    if ((it.eq.its) .and. (jt.eq.jts)) i_force_dump = 1
	else if (mode_force_dump .eq. 100) then
	    i_force_dump = 1
	else if (mode_force_dump .eq. 7) then
	    if ( (it .eq.  (its+ite)/2) .and.   &
	         (jt .eq.  (jts+jte)/2) .and.   &
	         (kt .eq.          kts) ) i_force_dump = 1
	else if (mode_force_dump .eq. 77) then
	    if ( (it .eq.  (its+ite)/2) .and.   &
	         (jt .eq.  (jts+jte)/2) .and.   &
	         (mod(kt-kts,3) .eq. 0) ) i_force_dump = 1
	end if


!   rodas
	iok = 0
	jsolver = 0
	if (igas_solver .eq. 1) then
	    jsolver = 1
	    call gasodesolver_rodas( tstart, tstop, iok,   &
      		it, jt, kt, iregime,   &
      		mgaschem, lunerr, lunout, levdbg_err, levdbg_info,   &
		i_force_dump, inforodas, iodestatus_count,   &
      		abs_error, rel_error, trun,   &
      		tempbox, pressbox, airdenbox, cboxnew, cboxold,   &
		rk_m1, rk_m2, rk_m3, rk_m4 )
        endif

!   lsodes
	if (igas_solver.eq.2 .or. iok.le.0) then
	    jsolver = 2
	    call gasodesolver_lsodes( tstart, tstop, iok,   &
      		it, jt, kt, iregime,   &
      		mgaschem, lunerr, lunout, levdbg_err, levdbg_info,   &
		i_force_dump, iodestatus_count,   &
      		abs_error, rel_error, trun,   &
      		tempbox, pressbox, airdenbox, cboxnew, cboxold,   &
		rk_m1, rk_m2, rk_m3, rk_m4 )
	endif

!   final species mapping back to host array -- only when iok > 0
	if (iok .gt. 0) then
	    call mapgas_tofrom_host( 1,                &
		i_boxtest_units_convert,               &
		it,jt,kt, ims,ime, jms,jme, kms,kme,   &
		num_moist, num_chem, moist, chem,      &
		t_phy, p_phy, rho_phy,                 &
		cboxnew, tempbox, pressbox, airdenbox, &
		cair_mlc,                              &
		h2o, ch4, oxygen, nitrogen, hydrogen   )
	end if

!   following call is for boxwrf testing only
!   it must be commented out for actual wrf applications
!	call boxtest_set_extra_args( iregime, it, jt, kt )

         if (iok .gt. 0) then
        call update_vdrog3_cbmz(rk_m2,rk_m3,dtstepc,cboxnew, cboxold, &
                                prodrog)
!prodrog units are in molecules/cm3
!   chem units = (ppm)
        airdenbox = rho_phy(it,kt,jt)/28.966e3
        if (i_boxtest_units_convert .eq. 10) then
            airdenbox = rho_phy(it,kt,jt)
        end if


        factorprog = airdenbox*1.0e-6
        if (i_boxtest_units_convert .eq. 10) factorprog = airdenbox
        factorprog = factorprog*avognumkpp


!  convert prodrog from units of molecules/cm3 to ppmv        
         do iprog=1,ldrog
           prodrog(iprog)=prodrog(iprog)/factorprog
          enddo
 

!  Now assign the prodrog values to vdrog3 array
          do iprog=1,ldrog
          vdrog3(it,kt,jt,iprog)=prodrog(iprog)
          vdrog3(it,kt,jt,iprog)=max(0.,vdrog3(it,kt,jt,iprog))
          enddo

        endif ! iok .gt. 0
!MS
2900	continue

	if (i_print_gasode_stats .gt. 0)   &
	   call print_gasode_stats( lunout, levdbg_info,   &
		inforodas, iodestatus_count, ioderegime_count )
	return
	end subroutine cbmz_driver                      
 

!***********************************************************************
! < xx.> subr update_vdrog3_cbmz
!
! purpose: updates vdrog3 concentrations for MADE-SORGAM-CBMZ package
! Written by Manish Shrivastava on 12/20/2011
!-----------------------------------------------------------------------

      subroutine update_vdrog3_cbmz(rk_m2,rk_m3,dtstepc,cboxnew, cboxold, &
                                prodrog)
   USE module_data_sorgam, only:  ldrog
   USE module_data_cbmz

      implicit none
     
      real,intent(in) :: rk_m2(nrxn_m2),cboxnew(ngas_z), cboxold(ngas_z)
      real,intent(in) :: rk_m3(nrxn_m3)
      real,intent(in) :: dtstepc
      real,intent(inout) :: prodrog(ldrog)
      integer i
      real temp_par_new,temp_par_old,f_api,f_lim

!  f_api is fraction of isoprene attributed to API
!  f_lim is fraction of isoprene attributed to LIM

      f_api=0.292
      f_lim=0.064

! First initialize prodrog

        temp_par_new=0.0
        temp_par_old=0.0
        do i=1,ldrog
        prodrog(i)=0.0
        enddo
         
        temp_par_new=temp_par_new+cboxnew(ipar_z)/7.9
!since hc8 emissions are multiplied by 7.9 to calculate par, we divide by this number
 
       temp_par_old=temp_par_old+cboxold(ipar_z)/7.9
!since hc8 emissions are multiplied by 7.9 to calculate par, we divide by this number



! The order of following reactions is from module_data_sorgam.F
! prodrog(1) is rxn of XYL+OH rk_m2(19)
      prodrog(1)=0.5*(cboxnew(ixyl_z)+cboxold(ixyl_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(19)*dtstepc
!prodrog(2) is rxn of TOL+OH rk_m2(18)
     prodrog(2)=0.5*(cboxnew(itol_z)+cboxold(itol_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(18)*dtstepc
!prodrog(3) is rxn of CRES+OH rk_m2(21)
     prodrog(3)=0.5*(cboxnew(icres_z)+cboxold(icres_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(21)*dtstepc
!prodrog(4) is rxn of CRES+NO3 rk_m2(22)
     prodrog(4)=0.5*(cboxnew(icres_z)+cboxold(icres_z))*0.5*(cboxnew(ino3_z)+cboxold(ino3_z)) &
                    *rk_m2(22)*dtstepc
!prodrog(5) is rxn of HC+OH rk_m2(1)
     prodrog(5)=0.5*(temp_par_new+temp_par_old)*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(1)*dtstepc !Set to 0.0 as PAR should be replaced by HC8 or higher alkanes
!prodrog(6) is rxn of OLEI+OH rk_m2(15)
     prodrog(6)=0.5*(cboxnew(iolei_z)+cboxold(iolei_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(15)*dtstepc
!prodrog(7) is rxn of OLEI+NO3 rk_m2(17)
     prodrog(7)=0.5*(cboxnew(iolei_z)+cboxold(iolei_z))*0.5*(cboxnew(ino3_z)+cboxold(ino3_z)) &
                    *rk_m2(17)*dtstepc
!prodrog(8) is rxn of OLEI+O3 rk_m2(13)
     prodrog(8)=0.5*(cboxnew(iolei_z)+cboxold(iolei_z))*0.5*(cboxnew(io3_z)+cboxold(io3_z)) &
                    *rk_m2(13)*dtstepc
!prodrog(9) is rxn of OLET+OH rk_m2(14)
     prodrog(9)=0.5*(cboxnew(iolet_z)+cboxold(iolet_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m2(14)*dtstepc
!prodrog(10) is rxn of OLET+NO3 rk_m2(16)
     prodrog(10)=0.5*(cboxnew(iolet_z)+cboxold(iolet_z))*0.5*(cboxnew(ino3_z)+cboxold(ino3_z)) &
                    *rk_m2(16)*dtstepc
!prodrog(11) is rxn of OLET+O3 rk_m2(12)
     prodrog(11)=0.5*(cboxnew(iolet_z)+cboxold(iolet_z))*0.5*(cboxnew(io3_z)+cboxold(io3_z)) &
                    *rk_m2(12)*dtstepc
    
     prodrog(12)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m3(1)*dtstepc*f_api
     prodrog(13)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(ino3_z)+cboxold(ino3_z)) &
                    *rk_m3(3)*dtstepc*f_api
     prodrog(14)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(io3_z)+cboxold(io3_z)) &
                    *rk_m3(2)*dtstepc*f_api
     prodrog(15)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(ioh_z)+cboxold(ioh_z)) &
                    *rk_m3(1)*dtstepc*f_lim
     prodrog(16)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(ino3_z)+cboxold(ino3_z)) &
                    *rk_m3(3)*dtstepc*f_lim
     prodrog(17)=0.5*(cboxnew(iisop_z)+cboxold(iisop_z))*0.5*(cboxnew(io3_z)+cboxold(io3_z)) &
                    *rk_m3(2)*dtstepc*f_lim


 
! prodrog(12) to prodrog(17) are for biogenic species API and LIM
! Since these species are not available in CBMZ prodrog(12) to prodrog(17) are not calculated

        return
        end subroutine update_vdrog3_cbmz
 
!***********************************************************************
! < xx.> subr print_gasode_stats
!
! purpose: writes some statistics on ode solver performance to unit lunout
!
!-----------------------------------------------------------------------

	subroutine print_gasode_stats( lunout, levdbg,   &
		inforodas, iodestatus_count, ioderegime_count )

	implicit none

!   subr arguments
	integer lunout, levdbg
	integer inforodas(2,6), iodestatus_count(6), ioderegime_count(6)

!   local variables
	integer i, j
	character*80 msg


	msg = ' '
	call peg_debugmsg( lunout, levdbg, msg )
	msg = 'output from dump_cbmz_gasodeinfo'
	call peg_debugmsg( lunout, levdbg, msg )
	write(msg,9100) 'oderegime(1-6)', (ioderegime_count(i), i=1,6)
	call peg_debugmsg( lunout, levdbg, msg )
	write(msg,9100) 'odestatus(1-6)', (iodestatus_count(i), i=1,6)
	call peg_debugmsg( lunout, levdbg, msg )

	write(msg,9200)   &
      		'inforodas(1-3)', ((inforodas(j,i), j=1,2), i=1,3)
	call peg_debugmsg( lunout, levdbg, msg )
	write(msg,9200)   &
      		'inforodas(4-6)', ((inforodas(j,i), j=1,2), i=4,6)
	call peg_debugmsg( lunout, levdbg, msg )

9100	format( a, 6i11 )
9200	format( a, 3( i11, '--', i9.9 ) )

	return
	end subroutine print_gasode_stats
 
 
 
!***********************************************************************
! < xx.> subr gasodesolver_rodas
!
! purpose: interfaces to rodas ode solver
!
!-----------------------------------------------------------------------

	subroutine gasodesolver_rodas( tstart, tstop, iok,   &
      		isvode, jsvode, ksvode, iregime,   &
      		mgaschem, lunerr, lunout, levdbg_err, levdbg_info,   &
		i_force_dump, inforodas, iodestatus_count, &
      		abs_error, rel_error, trun,   &
      		tempbox, pressbox, airdenbox, cboxnew, cboxold,   &
		rk_m1, rk_m2, rk_m3, rk_m4 )

	use module_data_cbmz
	use module_cbmz_rodas_prep, only:                                    &
	    cbmz_v02r01_mapconcs, cbmz_v02r01_maprates, cbmz_v02r01_torodas, &
	    cbmz_v02r02_mapconcs, cbmz_v02r02_maprates, cbmz_v02r02_torodas, &
	    cbmz_v02r03_mapconcs, cbmz_v02r03_maprates, cbmz_v02r03_torodas, &
	    cbmz_v02r04_mapconcs, cbmz_v02r04_maprates, cbmz_v02r04_torodas, &
	    cbmz_v02r05_mapconcs, cbmz_v02r05_maprates, cbmz_v02r05_torodas, &
	    cbmz_v02r06_mapconcs, cbmz_v02r06_maprates, cbmz_v02r06_torodas

	implicit none

!   subr arguments 
    real(kind=8) :: trun
	integer iok, isvode, jsvode, ksvode, i_force_dump, iregime,   &
              mgaschem, lunerr, lunout, levdbg_err, levdbg_info
	integer inforodas(2,6), iodestatus_count(6)
	real tstart, tstop, abs_error, rel_error
	real tempbox, pressbox, airdenbox
	real cboxnew(ngas_z), cboxold(ngas_z)
	real rk_m1(nrxn_m1), rk_m2(nrxn_m2), rk_m3(nrxn_m3), rk_m4(nrxn_m4)

!   local variables
	integer               :: ia, idum, idydt_sngldble, ig, l, ntot
	integer, save         :: nrodas_failures = 0
	integer, dimension(6) :: inforodas_cur

        real hmin, hstart, taa, tzz
	real atolvec(ngas_z), rtolvec(ngas_z),   &
      		stot(ngas_z),   &
      		yposlimit(ngas_z), yneglimit(ngas_z)
	real sfixedkpp(nfixed_kppmax), rconstkpp(nreact_kppmax)

	character*80 msg

!   map reaction rate constants (pegasus --> kpp)
!   map concentrations (cboxold --> stot)
!   dump rates (for debugging)
	if (iregime .eq. 1) then
	    call cbmz_v02r01_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r01_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )

	else if (iregime .eq. 2) then
	    call cbmz_v02r02_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r02_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )

	else if (iregime .eq. 3) then
	    call cbmz_v02r03_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r03_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )

	else if (iregime .eq. 4) then
	    call cbmz_v02r04_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r04_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )

	else if (iregime .eq. 5) then
	    call cbmz_v02r05_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r05_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )

	else
	    call cbmz_v02r06_maprates( rk_m1, rk_m2, rk_m3, rk_m4,   &
      		rconstkpp )
	    call cbmz_v02r06_mapconcs( 0, ntot, stot, sfixedkpp, cboxold )
	end if

!   set parameters for rodas call
	do l = 1, ntot
	    atolvec(l) = abs_error
	    rtolvec(l) = rel_error
	    yposlimit(l) = 1.0e20
	    yneglimit(l) = -1.0e8
	end do

	taa = tstart
	tzz = tstop
	hmin = 1.0e-5
	hstart = 60.0
	idydt_sngldble = 1

!   call rodas integrator
!	subr cbmz_v02r06_torodas(
!    +	  ngas, taa, tzz,
!    +	  stot, atol, rtol, yposlimit, yneglimit,
!    +	  hmin, hstart,
!    +	  inforodas_cur, iok, lunerr, idydt_sngldble )

	if (iregime .eq. 1) then
	    call cbmz_v02r01_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )

	else if (iregime .eq. 2) then
	    call cbmz_v02r02_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )

	else if (iregime .eq. 3) then
	    call cbmz_v02r03_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )

	else if (iregime .eq. 4) then
	    call cbmz_v02r04_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )

	else if (iregime .eq. 5) then
	    call cbmz_v02r05_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )

	else
	    call cbmz_v02r06_torodas(   &
      		ntot, taa, tzz,   &
      		stot, atolvec, rtolvec, yposlimit, yneglimit,   &
      		sfixedkpp, rconstkpp,   &
      		hmin, hstart,   &
      		inforodas_cur, iok, lunerr, idydt_sngldble )
	end if


!   increment odeinfo counters
	if (iok .gt. 0) then
	    if (inforodas_cur(6) .le. 0) then
		ia = 1
	    else
		ia = 2
	    end if
	else
	    ia = 3
	end if
	iodestatus_count(ia) = iodestatus_count(ia) + 1
!   do following to avoid overflow of the "inforodas" numbers
!       inforodas(2,i) contains rightmost 9 digits of each inforodas number
!       inforodas(1,i) contains any higher  digits of each inforodas number
	do ia = 1, 6
	    idum = inforodas(2,ia) + inforodas_cur(ia)
	    inforodas(1,ia) = inforodas(1,ia) + (idum/1000000000)
	    inforodas(2,ia) = mod(idum, 1000000000)
	end do


!   map concentrations (stot --> cboxnew)
	if (iregime .eq. 1) then
	    call cbmz_v02r01_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	else if (iregime .eq. 2) then
	    call cbmz_v02r02_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	else if (iregime .eq. 3) then
	    call cbmz_v02r03_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	else if (iregime .eq. 4) then
	    call cbmz_v02r04_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	else if (iregime .eq. 5) then
	    call cbmz_v02r05_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	else
	    call cbmz_v02r06_mapconcs( 1, ntot, stot, sfixedkpp, cboxnew )
	end if


!   diagnostic output if integration fails OR if i_force_dump > 0
	if (iok .gt. 0) then
	    if (i_force_dump .le. 0) goto 20000
	else
	    nrodas_failures = nrodas_failures + 1
	    if (nrodas_failures .gt. 100) goto 20000
	end if

	msg = ' '
	call peg_debugmsg( lunout, levdbg_err, msg )
	if (iok .gt. 0) then
	    msg = '*** gasodesolver_rodas forced dump'
	else
	    write(msg,*) '*** gasodesolver_rodas failure no.',   &
      		nrodas_failures
	end if
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg = 'iregime, iok, i, j, k / t'
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97010) iregime, iok, isvode, jsvode, ksvode
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) trun
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg = 'inforodas_cur(1-6) ='
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97010) inforodas_cur
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg =   &
	'tstart, tstop, abs_error, rel_error / temp, press, cair, cos_sza ='
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) tstart, tstop, abs_error, rel_error
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) tempbox, pressbox, airdenbox, -99.0
	call peg_debugmsg( lunout, levdbg_err, msg )

	idum = 0
	do ig = nreact_kppmax, 1, -1
	    if ((idum .eq. 0) .and. (rconstkpp(ig) .ne. 0.0)) idum = ig
	end do
	msg = 'ngas_z, nrconst_nonzero ='
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97010) ngas_z, idum
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg = 'l, name, cboxold, cboxnew for l=1,ngas_z'
	call peg_debugmsg( lunout, levdbg_err, msg )
	do l = 1, ngas_z
	    write(msg,97030) l, name_z(l), cboxold(l), cboxnew(l)
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do
	msg = 'rconst for i=1,nrconst_nonzero'
	call peg_debugmsg( lunout, levdbg_err, msg )
	do ia = 1, idum, 4
	    write(msg,97020) ( rconstkpp(ig), ig = ia, min(ia+3,idum) )
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do

97010	format( 6i12 )
97020	format( 4(1pe18.10) )
97030	format(( i3, 1x, a, 2(1pe18.10) ))


!   force non-negative values
20000	do l = 1, ngas_z
	    cboxnew(l) = max( cboxnew(l), 0.0 )
	end do

	return
	end subroutine gasodesolver_rodas                     

 
 
!***********************************************************************
! < xx.> subr gasodesolver_lsodes
!
! purpose: interface to lsodes ode solver
!
! author : Rahul A. Zaveri
! date   : May, 2000
!
!-----------------------------------------------------------------------

      subroutine gasodesolver_lsodes( tstart, tstop, iok,   &
      		isvode, jsvode, ksvode, iregime,   &
      		mgaschem, lunerr, lunout, levdbg_err, levdbg_info,   &
		i_force_dump, iodestatus_count, &
      		abs_error, rel_error, trun,   &
      		tempbox, pressbox, airdenbox, cboxnew, cboxold,   &
		rk_m1, rk_m2, rk_m3, rk_m4 )

      use module_data_cbmz
      use module_cbmz_lsodes_solver, only:  lsodes_solver, xsetf,   &
                                            set_lsodes_common_vars
      implicit none

!   subr arguments 
      real(kind=8) :: trun
      integer i, iok, isvode, jsvode, ksvode, i_force_dump, iregime,   &
              mgaschem, lunerr, lunout, levdbg_err, levdbg_info
      integer iodestatus_count(6)
      real tstart, tstop, abs_error, rel_error
      real tempbox, pressbox, airdenbox
      real cboxnew(ngas_z), cboxold(ngas_z)
      real rk_m1(nrxn_m1), rk_m2(nrxn_m2), rk_m3(nrxn_m3), rk_m4(nrxn_m4)

! lsodes parameters and local variables
      integer itoler, itask, iopt, mf, lwm, nrdim, nidim
      integer nruserpar, niuserpar
      parameter( itoler = 1, itask = 1, iopt = 1, mf= 222 )
      parameter( lwm = 3*ngas_tot*ngas_tot + 12*ngas_tot )
      parameter( nrdim = 20 + 9*ngas_tot + lwm )
      parameter( nidim = 31 + ngas_tot + ngas_tot*ngas_tot )
      parameter( nruserpar = 5 + nrxn_m1 + nrxn_m2 + nrxn_m3 + nrxn_m4)
      parameter( niuserpar = ngas_z + 1 )

      integer ia, idum, ig, ioffset, istate, iwork(nidim), l
      integer ntotvec(1), iuserpar(niuserpar)
      integer indx(ngas_z)
      integer, save :: iflagout = 0
      integer, save :: nlsodes_failures = 0

      real dtchem, rwork(nrdim), stot(ngas_tot)
      real atolvec(1), rtolvec(1), ruserpar(nruserpar)

      character*80 msg



      iok = 1				! reset

      call set_lsodes_common_vars()

! sets gas species indices for iregime
      call setgasindices( iregime, indx )

! map cboxold --> stot
      call mapgasspecies( cboxold, stot, 0, iregime, indx )

!----------------------------------------------------------------------
! set number of species (ntot) for the selected regime for LSODES
      if      (iregime .eq. 1) then
          ntotvec(1) = ngas_r1
      else if (iregime .eq. 2) then
          ntotvec(1) = ngas_r2
      else if (iregime .eq. 3) then
          ntotvec(1) = ngas_r3
      else if (iregime .eq. 4) then
          ntotvec(1) = ngas_r4
      else if (iregime .eq. 5) then
          ntotvec(1) = ngas_r5
      else
          ntotvec(1) = ngas_r6
      end if

100   continue

! set other LSODES parameters...
      iwork(6) = 1000		! max iterations for a time step
      iwork(7) = 1
      istate   = 1
      rwork(6) = dtchem
      if(iflagout.eq.0)then
          call xsetf(iflagout)
      endif

      atolvec(1) = abs_error
      rtolvec(1) = rel_error

      do ig = 1, 5
          ruserpar(ig) = ig*7.0
      end do
      ruserpar(1) = cboxold(ih2o_z)
      ruserpar(2) = cboxold(ich4_z)
      ruserpar(3) = cboxold(io2_z)
      ruserpar(4) = cboxold(in2_z)
      ruserpar(5) = cboxold(ih2_z)
      ioffset = 5
      do ig = 1, nrxn_m1
          ruserpar(ioffset+ig) = rk_m1(ig)
      end do
      ioffset = ioffset + nrxn_m1
      do ig = 1, nrxn_m2
          ruserpar(ioffset+ig) = rk_m2(ig)
      end do
      ioffset = ioffset + nrxn_m2
      do ig = 1, nrxn_m3
          ruserpar(ioffset+ig) = rk_m3(ig)
      end do
      ioffset = ioffset + nrxn_m3
      do ig = 1, nrxn_m4
          ruserpar(ioffset+ig) = rk_m4(ig)
      end do

      iuserpar(1) = iregime
      do ig = 1, ngas_z
          iuserpar(1+ig) = indx(ig)
      end do

      call lsodes_solver(   &
                gasode_cbmz, ntotvec, stot, tstart, tstop,   &
      		itoler, rtolvec, atolvec, itask, istate, iopt,   &
      		rwork, nrdim, iwork, nidim, jcs, mf,   &
      		ruserpar, nruserpar, iuserpar, niuserpar )

      if (istate .le. 0) iok = -1


!   increment odeinfo counters
      if (iok .gt. 0) then
          ia = 4
      else
          ia = 5
      end if
      iodestatus_count(ia) = iodestatus_count(ia) + 1


! map stot --> cboxnew
	call mapgasspecies( cboxnew, stot, 1, iregime, indx )


! do diagnostic output if integration fails OR if i_force_dump > 0
	if (iok .gt. 0) then
	    if (i_force_dump .le. 0) goto 20000
	else
	    nlsodes_failures = nlsodes_failures + 1
	end if

	msg = ' '
	call peg_debugmsg( lunout, levdbg_err, msg )
	if (iok .gt. 0) then
	    msg = '*** gasodesolver_lsodes forced dump'
	else
	    write(msg,*) '*** gasodesolver_lsodes failure no.',   &
		nlsodes_failures
	end if
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg = 'iregime, iok, i, j, k / t'
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97010) iregime, iok, isvode, jsvode, ksvode
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) trun
	call peg_debugmsg( lunout, levdbg_err, msg )
	if (nlsodes_failures .gt. 1000) then
	    write(msg,*) '*** exceeded lsodes failure limit =', 1000
	    call peg_debugmsg( lunout, levdbg_err, msg )
	    call peg_error_fatal( lunerr, msg )
	end if
	if (nlsodes_failures .gt. 100) goto 20000

	write(msg,*) 'istate -', istate
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg =   &
	'tstart, tstop, abs_error, rel_error / temp, press, cair, cos_sza ='
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) tstart, tstop, abs_error, rel_error
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97020) tempbox, pressbox, airdenbox, -99.0
	call peg_debugmsg( lunout, levdbg_err, msg )

	idum = nrxn_m1 + nrxn_m2 + nrxn_m3 + nrxn_m4
	msg = 'ngas_z, nrconst_m1+m2+m3+m4 ='
	call peg_debugmsg( lunout, levdbg_err, msg )
	write(msg,97010) ngas_z, idum
	call peg_debugmsg( lunout, levdbg_err, msg )
	msg = 'l, name, cboxold, cboxnew for l=1,ngas_z'
	call peg_debugmsg( lunout, levdbg_err, msg )
	do l = 1, ngas_z
	    write(msg,97030) l, name_z(l), cboxold(l), cboxnew(l)
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do
	msg = 'rconst for i=1,nrconst_nonzero'
	call peg_debugmsg( lunout, levdbg_err, msg )
	do ia = 1, nrxn_m1, 4
	    write(msg,97020) ( rk_m1(ig), ig = ia, min(ia+3,nrxn_m1) )
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do
	do ia = 1, nrxn_m2, 4
	    write(msg,97020) ( rk_m2(ig), ig = ia, min(ia+3,nrxn_m2) )
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do
	do ia = 1, nrxn_m3, 4
	    write(msg,97020) ( rk_m3(ig), ig = ia, min(ia+3,nrxn_m3) )
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do
	do ia = 1, nrxn_m4, 4
	    write(msg,97020) ( rk_m4(ig), ig = ia, min(ia+3,nrxn_m4) )
	    call peg_debugmsg( lunout, levdbg_err, msg )
	end do

97010	format( 6i12 )
97020	format( 4(1pe18.10) )
97030	format(( i3, 1x, a, 2(1pe18.10) ))


!   force non-negative values
20000	do l = 1, ngas_z
	    cboxnew(l) = max( cboxnew(l), 0.0 )
	end do

      return
      end subroutine gasodesolver_lsodes                     
 
 
 
!***********************************************************************
! < 2.> subr selectgasregime
!
! purpose: selects an optimum combination of gas-phase
!          mechanisms based on sensitivity of [OH]
!          concentrations to some lumped structure
!          hydrocarbon groups concentrations and [DMS]
!          concentration.
!
! input : cbox      = full species concentrations array (mol/cc)
!
! output: iregime   = 1     : com
!                   = 2     : com + urb
!                   = 3     : com + urb + bio
!                   = 4     : com + mar
!                   = 5     : com + urb + mar
!                   = 6     : com + urb + bio + mar
!         ntot      = number of gas-phase species in the selected mechanism
!
! author: Rahul A. Zaveri
! date  : April 2000
!
!---------------------------------------------------------------------

      subroutine selectgasregime( iregime, iregime_forced, cbox,   &
              igaschem_allowed_m1, igaschem_allowed_m2,   &
              igaschem_allowed_m3, igaschem_allowed_m4 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer iregime, iregime_forced
      integer igaschem_allowed_m1, igaschem_allowed_m2,   &
              igaschem_allowed_m3, igaschem_allowed_m4
      real cbox(ngas_z)

!   local variables
      integer iwork(6)
      integer m_m1, m_m2, m_m3, m_m4
      real cut_molecpcc


      cut_molecpcc = 5.e+6		! molecules/cc

! initialize mechanism flags
      m_m1 = 1	! 1 (always)
      m_m2 = 0	! 0 or 1
      m_m3 = 0	! 0 or 2
      m_m4 = 0	! 0 or 3

      if (igaschem_allowed_m2 .gt. 0) then
          if (cbox(ipar_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iaone_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(imgly_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ieth_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iolet_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iolei_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ixyl_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(icres_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ito2_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(icro_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iopen_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ionit_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(irooh_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iro2_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(iano2_z    ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(inap_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ixo2_z     ) .gt. cut_molecpcc) m_m2 = 1
          if (cbox(ixpar_z    ) .gt. cut_molecpcc) m_m2 = 1
      end if

      if (igaschem_allowed_m3 .gt. 0) then
          if (cbox(iisop_z    ) .gt. cut_molecpcc) m_m3 = 2
          if (cbox(iisoprd_z  ) .gt. cut_molecpcc) m_m3 = 2
          if (cbox(iisopp_z   ) .gt. cut_molecpcc) m_m3 = 2
          if (cbox(iisopn_z   ) .gt. cut_molecpcc) m_m3 = 2
          if (cbox(iisopo2_z  ) .gt. cut_molecpcc) m_m3 = 2
      end if

      if (igaschem_allowed_m4 .gt. 0) then
          if (cbox(idms_z        ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(imsa_z        ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(idmso_z       ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(idmso2_z      ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3so2h_z    ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3sch2oo_z  ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3so2_z     ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3so3_z     ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3so2oo_z   ) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(ich3so2ch2oo_z) .gt. cut_molecpcc) m_m4 = 3
          if (cbox(imtf_z        ) .gt. cut_molecpcc) m_m4 = 3
      end if

      iregime = m_m1 + m_m2*((2-m_m3)/2) + m_m3 + m_m4

!   force iregime = iregime_forced
      if ((iregime_forced .ge. 1) .and. (iregime_forced .le. 6))   &
          iregime = iregime_forced

      return
      end subroutine selectgasregime                        
 
 
 
!***********************************************************************
! < 3.> subr setgasindices
!
! purpose: sets gas species indices
!
! author : Rahul A. Zaveri
! date   : May, 2000
!
!-----------------------------------------------------------------------

      subroutine setgasindices( iregime, indx )

      use module_data_cbmz
      implicit none

!   subr arguments
      integer iregime, indx(ngas_z)

!   local variables
      integer ilast

      ilast = 0
      indx(:) = -999888777

      goto (1,2,3,4,5,6), iregime

1     call setgasindex_m1( ilast, indx )		! regime 1
      return

2     call setgasindex_m1( ilast, indx )		! regime 2
      call setgasindex_m2( ilast, indx )
      return

3     call setgasindex_m1( ilast, indx )		! regime 3
      call setgasindex_m2( ilast, indx )
      call setgasindex_m3( ilast, indx )
      return

4     call setgasindex_m1( ilast, indx )		! regime 4
      call setgasindex_m4( ilast, indx )
      return

5     call setgasindex_m1( ilast, indx )		! regime 5
      call setgasindex_m2( ilast, indx )
      call setgasindex_m4( ilast, indx )
      return

6     call setgasindex_m1( ilast, indx )		! regime 6
      call setgasindex_m2( ilast, indx )
      call setgasindex_m3( ilast, indx )
      call setgasindex_m4( ilast, indx )
      return

      end subroutine setgasindices

 
 
 
!***********************************************************************
! < 4.> subr gasrateconstants
!
! purpose: calls regime-dependent subrs for calculating
!          gas-phase thermal reaction rate constants
!
! author : Rahul A. Zaveri
! date   : May, 2000
!
!-----------------------------------------------------------------------

      subroutine gasrateconstants( iregime, tempbox, cair_mlc,   &
		rk_photo, rk_param, rk_m1, rk_m2, rk_m3, rk_m4 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer iregime
      real tempbox, cair_mlc
      real rk_photo(nphoto), rk_param(nperox)
      real rk_m1(nrxn_m1), rk_m2(nrxn_m2), rk_m3(nrxn_m3), rk_m4(nrxn_m4)


!   iregime=1 --> do m1
!   iregime=2 --> do m1, m2
!   iregime=3 --> do m1, m2, m3
!   iregime=4 --> do m1, --, --, m4
!   iregime=5 --> do m1, m2, --, m4
!   iregime=6 --> do m1, m2, m3, m4

      call gasthermrk_m1( tempbox, cair_mlc,   &
                          rk_photo, rk_param, rk_m1, rk_m2 )

      if ((iregime .eq. 2) .or.   &
          (iregime .eq. 3) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasthermrk_m2( tempbox, cair_mlc,   &
                              rk_photo, rk_param, rk_m2 )

      if ((iregime .eq. 3) .or.   &
          (iregime .eq. 6))       &
          call gasthermrk_m3( tempbox, cair_mlc,   &
                              rk_photo, rk_param, rk_m3 )

      if ((iregime .eq. 4) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasthermrk_m4( tempbox, cair_mlc,   &
                              rk_photo, rk_param, rk_m4 )

      return
      end subroutine gasrateconstants           
 
 
 
!***********************************************************************
! < 3.> subr setgasindex_m1
!
! purpose: defines gas species indices for regime 1
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
!-----------------------------------------------------------------------

      subroutine setgasindex_m1( ilast, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer ilast, indx(ngas_z)


      indx(ino_z)	= 1
      indx(ino2_z)	= 2
      indx(ino3_z)	= 3
      indx(in2o5_z)	= 4
      indx(ihono_z)	= 5
      indx(ihno3_z)	= 6
      indx(ihno4_z)	= 7
      indx(io3_z)	= 8
      indx(io1d_z)	= 9
      indx(io3p_z)	= 10
      indx(ioh_z)	= 11
      indx(iho2_z)	= 12
      indx(ih2o2_z)	= 13
      indx(ico_z)	= 14
      indx(iso2_z)	= 15
      indx(ih2so4_z)	= 16
      indx(inh3_z)	= 17
      indx(ihcl_z)	= 18
      indx(ic2h6_z)	= 19
      indx(ich3o2_z)	= 20
      indx(iethp_z)	= 21
      indx(ihcho_z)	= 22
      indx(ich3oh_z)	= 23
      indx(ic2h5oh_z)	= 24
      indx(ich3ooh_z)	= 25
      indx(iethooh_z)	= 26
      indx(iald2_z)	= 27
      indx(ihcooh_z)	= 28
      indx(ircooh_z)	= 29
      indx(ic2o3_z)	= 30
      indx(ipan_z)	= 31

      ilast = indx(ipan_z)
 
      return
      end subroutine setgasindex_m1       
 
 
 
!***********************************************************************
! < 4.> subr setgasindex_m2
!
! purpose: defines gas species indices for regime 2
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
!-----------------------------------------------------------------------
      subroutine setgasindex_m2( ilast, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer ilast, indx(ngas_z)


      indx(ipar_z)	= ilast + 1
      indx(iaone_z)	= ilast + 2
      indx(imgly_z)	= ilast + 3
      indx(ieth_z)	= ilast + 4
      indx(iolet_z)	= ilast + 5
      indx(iolei_z)	= ilast + 6
      indx(itol_z)	= ilast + 7
      indx(ixyl_z)	= ilast + 8
      indx(icres_z)	= ilast + 9
      indx(ito2_z)	= ilast + 10
      indx(icro_z)	= ilast + 11
      indx(iopen_z)	= ilast + 12
      indx(ionit_z)	= ilast + 13
!     indx(ipan_z)	= ilast + 14
!     indx(ircooh_z)	= ilast + 15
      indx(irooh_z)	= ilast + 14
!     indx(ic2o3_z)	= ilast + 17
      indx(iro2_z)	= ilast + 15
      indx(iano2_z)	= ilast + 16
      indx(inap_z)	= ilast + 17
      indx(ixo2_z)	= ilast + 18
      indx(ixpar_z)	= ilast + 19

      ilast = indx(ixpar_z)
 
      return
      end subroutine setgasindex_m2       
 
 
 
!***********************************************************************
! < 5.> subr setgasindex_m3
!
! purpose: defines gas species indices for regime 3
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
!-----------------------------------------------------------------------
      subroutine setgasindex_m3( ilast, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer ilast, indx(ngas_z)


      indx(iisop_z)	= ilast + 1
      indx(iisoprd_z)	= ilast + 2
      indx(iisopp_z)	= ilast + 3
      indx(iisopn_z)	= ilast + 4
      indx(iisopo2_z)	= ilast + 5

      ilast = indx(iisopo2_z)
 
      return
      end subroutine setgasindex_m3       
 
 
 
!***********************************************************************
! < 6.> subr setgasindex_m4
!
! purpose: defines gas species indices for regime 4
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
!-----------------------------------------------------------------------

      subroutine setgasindex_m4( ilast, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer ilast, indx(ngas_z)


!
      indx(idms_z)		= ilast + 1
      indx(imsa_z)		= ilast + 2
      indx(idmso_z)		= ilast + 3
      indx(idmso2_z)		= ilast + 4
      indx(ich3so2h_z)		= ilast + 5
      indx(ich3sch2oo_z)	= ilast + 6
      indx(ich3so2_z)		= ilast + 7
      indx(ich3so3_z)		= ilast + 8
      indx(ich3so2oo_z)	= ilast + 9
      indx(ich3so2ch2oo_z)	= ilast + 10
      indx(imtf_z)		= ilast + 11

      ilast = indx(imtf_z)
 
      return
      end subroutine setgasindex_m4       
 
 
 
!***********************************************************************
! < 9.> subr mapgasspecies
!
! purpose: map gas species between stot and cbox arrays
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine mapgasspecies( cbox, stot, imap, iregime, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer imap, iregime, indx(ngas_z)
      real cbox(ngas_z)
      real stot(ngas_tot)
!
!
      goto (1,2,3,4,5,6), iregime
!
!
1     call mapgas_m1( cbox, stot, imap, indx )
      return
!
!
2     call mapgas_m1( cbox, stot, imap, indx )
      call mapgas_m2( cbox, stot, imap, indx )
      return
!
!
3     call mapgas_m1( cbox, stot, imap, indx )
      call mapgas_m2( cbox, stot, imap, indx )
      call mapgas_m3( cbox, stot, imap, indx )
      return
!
!
4     call mapgas_m1( cbox, stot, imap, indx )
      call mapgas_m4( cbox, stot, imap, indx )
      return
!
!
5     call mapgas_m1( cbox, stot, imap, indx )
      call mapgas_m2( cbox, stot, imap, indx )
      call mapgas_m4( cbox, stot, imap, indx )
      return
!
!
6     call mapgas_m1( cbox, stot, imap, indx )
      call mapgas_m2( cbox, stot, imap, indx )
      call mapgas_m3( cbox, stot, imap, indx )
      call mapgas_m4( cbox, stot, imap, indx )
      return
 
      end subroutine mapgasspecies                    
 
 
 
!***********************************************************************
! <10.> subr mapgas_m1
!
! purpose: maps gas species between stot and cbox arrays for mechanism 1
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine mapgas_m1( cbox, stot, imap, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer imap, indx(ngas_z)
      real cbox(ngas_z)
      real stot(ngas_tot)

      if(imap.eq.0)then    ! map cbox --> stot (both molec/cc)
      stot(indx(ino_z))	= cbox(ino_z)
      stot(indx(ino2_z))	= cbox(ino2_z)
      stot(indx(ino3_z))	= cbox(ino3_z)
      stot(indx(in2o5_z))	= cbox(in2o5_z)
      stot(indx(ihono_z))	= cbox(ihono_z)
      stot(indx(ihno3_z))	= cbox(ihno3_z)
      stot(indx(ihno4_z))	= cbox(ihno4_z)
      stot(indx(io3_z))	= cbox(io3_z)
      stot(indx(io1d_z))	= cbox(io1d_z)
      stot(indx(io3p_z))	= cbox(io3p_z)
      stot(indx(ioh_z))	= cbox(ioh_z)
      stot(indx(iho2_z))	= cbox(iho2_z)
      stot(indx(ih2o2_z))	= cbox(ih2o2_z)
      stot(indx(ico_z))	= cbox(ico_z)
      stot(indx(iso2_z))	= cbox(iso2_z)
      stot(indx(ih2so4_z))	= cbox(ih2so4_z)
      stot(indx(inh3_z))	= cbox(inh3_z)
      stot(indx(ihcl_z))	= cbox(ihcl_z)
      stot(indx(ic2h6_z))	= cbox(ic2h6_z)
      stot(indx(ich3o2_z))	= cbox(ich3o2_z)
      stot(indx(iethp_z))	= cbox(iethp_z)
      stot(indx(ihcho_z))	= cbox(ihcho_z)
      stot(indx(ich3oh_z))	= cbox(ich3oh_z)
      stot(indx(ic2h5oh_z))	= cbox(ic2h5oh_z)
      stot(indx(ich3ooh_z))	= cbox(ich3ooh_z)
      stot(indx(iethooh_z))	= cbox(iethooh_z)
      stot(indx(iald2_z))	= cbox(iald2_z)
      stot(indx(ihcooh_z))	= cbox(ihcooh_z)
      stot(indx(ircooh_z))	= cbox(ircooh_z)
      stot(indx(ic2o3_z))	= cbox(ic2o3_z)
      stot(indx(ipan_z))	= cbox(ipan_z)
!
      else                 ! map stot --> cbox (both molec/cc)
      cbox(ino_z)	= stot(indx(ino_z))
      cbox(ino2_z)	= stot(indx(ino2_z))
      cbox(ino3_z)	= stot(indx(ino3_z))
      cbox(in2o5_z)	= stot(indx(in2o5_z))
      cbox(ihono_z)	= stot(indx(ihono_z))
      cbox(ihno3_z)	= stot(indx(ihno3_z))
      cbox(ihno4_z)	= stot(indx(ihno4_z))
      cbox(io3_z)	= stot(indx(io3_z))
      cbox(io1d_z)	= stot(indx(io1d_z))
      cbox(io3p_z)	= stot(indx(io3p_z))
      cbox(ioh_z)	= stot(indx(ioh_z))
      cbox(iho2_z)	= stot(indx(iho2_z))
      cbox(ih2o2_z)	= stot(indx(ih2o2_z))
      cbox(ico_z)	= stot(indx(ico_z))
      cbox(iso2_z)	= stot(indx(iso2_z))
      cbox(ih2so4_z)	= stot(indx(ih2so4_z))
      cbox(inh3_z)	= stot(indx(inh3_z))
      cbox(ihcl_z)	= stot(indx(ihcl_z))
      cbox(ic2h6_z)	= stot(indx(ic2h6_z))
      cbox(ich3o2_z)	= stot(indx(ich3o2_z))
      cbox(iethp_z)	= stot(indx(iethp_z))
      cbox(ihcho_z)	= stot(indx(ihcho_z))
      cbox(ich3oh_z)	= stot(indx(ich3oh_z))
      cbox(ic2h5oh_z)	= stot(indx(ic2h5oh_z))
      cbox(ich3ooh_z)	= stot(indx(ich3ooh_z))
      cbox(iethooh_z)	= stot(indx(iethooh_z))
      cbox(iald2_z)	= stot(indx(iald2_z))
      cbox(ihcooh_z)	= stot(indx(ihcooh_z))
      cbox(ircooh_z)	= stot(indx(ircooh_z))
      cbox(ic2o3_z)	= stot(indx(ic2o3_z))
      cbox(ipan_z)	= stot(indx(ipan_z))
      endif
 
      return
      end subroutine mapgas_m1                    
 
 
 
!***********************************************************************
! <11.> subr mapgas_m2
!
! purpose: maps gas species between stot and cbox arrays for mechanism 2
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine mapgas_m2( cbox, stot, imap, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer imap, indx(ngas_z)
      real cbox(ngas_z)
      real stot(ngas_tot)

      if(imap.eq.0)then    ! map cbox --> stot (both molec/cc)
      stot(indx(ipar_z))	= cbox(ipar_z)
      stot(indx(iaone_z))	= cbox(iaone_z)
      stot(indx(imgly_z))	= cbox(imgly_z)
      stot(indx(ieth_z))	= cbox(ieth_z)
      stot(indx(iolet_z))	= cbox(iolet_z)
      stot(indx(iolei_z))	= cbox(iolei_z)
      stot(indx(itol_z))	= cbox(itol_z)
      stot(indx(ixyl_z))	= cbox(ixyl_z)
      stot(indx(icres_z))	= cbox(icres_z)
      stot(indx(ito2_z))	= cbox(ito2_z)
      stot(indx(icro_z))	= cbox(icro_z)
      stot(indx(iopen_z))	= cbox(iopen_z)
      stot(indx(ionit_z))	= cbox(ionit_z)
!     stot(indx(ipan_z))	= cbox(ipan_z)
!     stot(indx(ircooh_z))	= cbox(ircooh_z)
      stot(indx(irooh_z))	= cbox(irooh_z)
!     stot(indx(ic2o3_z))	= cbox(ic2o3_z)
      stot(indx(iro2_z))	= cbox(iro2_z)
      stot(indx(iano2_z))	= cbox(iano2_z)
      stot(indx(inap_z))	= cbox(inap_z)
      stot(indx(ixo2_z))	= cbox(ixo2_z)
      stot(indx(ixpar_z))	= cbox(ixpar_z)
!
      else                 ! map stot --> cbox (both molec/cc)
      cbox(ipar_z)	= stot(indx(ipar_z))
      cbox(iaone_z)	= stot(indx(iaone_z))
      cbox(imgly_z)	= stot(indx(imgly_z))
      cbox(ieth_z)	= stot(indx(ieth_z))
      cbox(iolet_z)	= stot(indx(iolet_z))
      cbox(iolei_z)	= stot(indx(iolei_z))
      cbox(itol_z)	= stot(indx(itol_z))
      cbox(ixyl_z)	= stot(indx(ixyl_z))
      cbox(icres_z)	= stot(indx(icres_z))
      cbox(ito2_z)	= stot(indx(ito2_z))
      cbox(icro_z)	= stot(indx(icro_z))
      cbox(iopen_z)	= stot(indx(iopen_z))
      cbox(ionit_z)	= stot(indx(ionit_z))
!     cbox(ipan_z)	= stot(indx(ipan_z))
!     cbox(ircooh_z)	= stot(indx(ircooh_z))
      cbox(irooh_z)	= stot(indx(irooh_z))
!     cbox(ic2o3_z)	= stot(indx(ic2o3_z))
      cbox(iro2_z)	= stot(indx(iro2_z))
      cbox(iano2_z)	= stot(indx(iano2_z))
      cbox(inap_z)	= stot(indx(inap_z))
      cbox(ixo2_z)	= stot(indx(ixo2_z))
      cbox(ixpar_z)	= stot(indx(ixpar_z))
      endif

      return
      end subroutine mapgas_m2                    
 
 
 
!***********************************************************************
! <12.> subr mapgas_m3
!
! purpose: maps gas species between stot and cbox arrays for mechanism 3
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine mapgas_m3( cbox, stot, imap, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer imap, indx(ngas_z)
      real cbox(ngas_z)
      real stot(ngas_tot)

      if(imap.eq.0)then    ! map cbox --> stot (both molec/cc)
      stot(indx(iisop_z))	= cbox(iisop_z)
      stot(indx(iisoprd_z))	= cbox(iisoprd_z)
      stot(indx(iisopp_z))	= cbox(iisopp_z)
      stot(indx(iisopn_z))	= cbox(iisopn_z)
      stot(indx(iisopo2_z))	= cbox(iisopo2_z)
!
      else                 ! map stot --> cbox (both molec/cc)
      cbox(iisop_z)	= stot(indx(iisop_z))
      cbox(iisoprd_z)	= stot(indx(iisoprd_z))
      cbox(iisopp_z)	= stot(indx(iisopp_z))
      cbox(iisopn_z)	= stot(indx(iisopn_z))
      cbox(iisopo2_z)	= stot(indx(iisopo2_z))
      endif
 
      return
      end subroutine mapgas_m3                    
 
 
 
!***********************************************************************
! <13.> subr mapgas_m4
!
! purpose: maps gas species between stot and cbox arrays for mechanism 4
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine mapgas_m4( cbox, stot, imap, indx )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer imap, indx(ngas_z)
      real cbox(ngas_z)
      real stot(ngas_tot)

      if(imap.eq.0)then    ! map cbox --> stot (both molec/cc)
      stot(indx(idms_z))	= cbox(idms_z)
      stot(indx(imsa_z))	= cbox(imsa_z)
      stot(indx(idmso_z))	= cbox(idmso_z)
      stot(indx(idmso2_z))	= cbox(idmso2_z)
      stot(indx(ich3so2h_z))	= cbox(ich3so2h_z)
      stot(indx(ich3sch2oo_z))	= cbox(ich3sch2oo_z)
      stot(indx(ich3so2_z))	= cbox(ich3so2_z)
      stot(indx(ich3so3_z))	= cbox(ich3so3_z)
      stot(indx(ich3so2oo_z))	= cbox(ich3so2oo_z)
      stot(indx(ich3so2ch2oo_z))= cbox(ich3so2ch2oo_z)
      stot(indx(imtf_z))	= cbox(imtf_z)
!
      else                 ! map stot --> cbox (both molec/cc)
      cbox(idms_z)	   = stot(indx(idms_z))
      cbox(imsa_z)	   = stot(indx(imsa_z))
      cbox(idmso_z)	   = stot(indx(idmso_z))
      cbox(idmso2_z)	   = stot(indx(idmso2_z))
      cbox(ich3so2h_z)	   = stot(indx(ich3so2h_z))
      cbox(ich3sch2oo_z)  = stot(indx(ich3sch2oo_z))
      cbox(ich3so2_z)	   = stot(indx(ich3so2_z))
      cbox(ich3so3_z)	   = stot(indx(ich3so3_z))
      cbox(ich3so2oo_z)   = stot(indx(ich3so2oo_z))
      cbox(ich3so2ch2oo_z)= stot(indx(ich3so2ch2oo_z))
      cbox(imtf_z)	   = stot(indx(imtf_z))
      endif
 
      return
      end subroutine mapgas_m4                    
 
 
 
!***********************************************************************
! <xx.> subr check_userpar
!
! purpose: called by lsodes (external)
!          computes time derivatives of species concentrations ds/dt.
!          by calling subr. gasrate and ode
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!---------------------------------------------------------------------------

      subroutine check_userpar( ruserpar, nruserpar, iuserpar, niuserpar )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer nruserpar, niuserpar
      integer iuserpar(niuserpar)
      real ruserpar(nruserpar)

!   local variables
      integer i
      real dum
      character*80 msg

      if (nruserpar .ne. (5 + nrxn_m1 + nrxn_m2 + nrxn_m3 + nrxn_m4)) then
          write(msg,9010) 'nruserpar', -1, nruserpar
          call wrf_error_fatal( msg )
      end if

      if (niuserpar .ne. (ngas_z + 1)) then
          write(msg,9010) 'niuserpar', -1, niuserpar
          call wrf_error_fatal( msg )
      end if

9010  format( '*** check_userpar error -- ', a, 1x, i8, 1x, i8 )

 
      return
      end subroutine check_userpar                
 
 
 
!***********************************************************************
! <14.> subr gasode_cbmz
!
! purpose: called by lsodes (external)
!          computes time derivatives of species concentrations ds/dt.
!          by calling subr. gasrate and ode
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!---------------------------------------------------------------------------

      subroutine gasode_cbmz( ntot, tt, s, sdot,   &
          ruserpar, nruserpar, iuserpar, niuserpar )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer ntot, nruserpar, niuserpar
      integer iuserpar(niuserpar)
      real tt
      real s(ngas_tot), sdot(ngas_tot), ruserpar(nruserpar)

!   local variables
      integer ig, ioffset, iregime, irxn
      integer indx(ngas_z)
      real h2o, ch4, oxygen, nitrogen, hydrogen
      real rk_m1(nrxn_m1), r_m1(nrxn_m1)
      real rk_m2(nrxn_m2), r_m2(nrxn_m2)
      real rk_m3(nrxn_m3), r_m3(nrxn_m3)
      real rk_m4(nrxn_m4), r_m4(nrxn_m4)
      real p_m1(ngas_tot), d_m1(ngas_tot)
      real p_m2(ngas_tot), d_m2(ngas_tot)
      real p_m3(ngas_tot), d_m3(ngas_tot)
      real p_m4(ngas_tot), d_m4(ngas_tot)


! test on userpar
      call check_userpar( ruserpar, nruserpar, iuserpar, niuserpar )

      iregime = iuserpar(1)
      do ig = 1, ngas_z
          indx(ig) = iuserpar(ig+1)
      end do

      h2o =      ruserpar(1)
      ch4 =      ruserpar(2)
      oxygen   = ruserpar(3)
      nitrogen = ruserpar(4)
      hydrogen = ruserpar(5)
      ioffset = 5
      do ig = 1, nrxn_m1
          rk_m1(ig) = ruserpar(ioffset+ig)
      end do
      ioffset = ioffset + nrxn_m1
      do ig = 1, nrxn_m2
          rk_m2(ig) = ruserpar(ioffset+ig)
      end do
      ioffset = ioffset + nrxn_m2
      do ig = 1, nrxn_m3
          rk_m3(ig) = ruserpar(ioffset+ig)
      end do
      ioffset = ioffset + nrxn_m3
      do ig = 1, nrxn_m4
          rk_m4(ig) = ruserpar(ioffset+ig)
      end do


! initialize to zero
      do irxn=1,nrxn_m1
      r_m1(irxn) = 0.
      enddo

      do irxn=1,nrxn_m2
      r_m2(irxn) = 0.
      enddo

      do irxn=1,nrxn_m3
      r_m3(irxn) = 0.
      enddo

      do irxn=1,nrxn_m4
      r_m4(irxn) = 0.
      enddo
!
!
! initialize to zero
      do ig=1,ngas_tot
      p_m1(ig) = 0.
      p_m2(ig) = 0.
      p_m3(ig) = 0.
      p_m4(ig) = 0.
!
      d_m1(ig) = 0.
      d_m2(ig) = 0.
      d_m3(ig) = 0.
      d_m4(ig) = 0.
      enddo


!   iregime=1 --> do m1
!   iregime=2 --> do m1, m2
!   iregime=3 --> do m1, m2, m3
!   iregime=4 --> do m1, --, --, m4
!   iregime=5 --> do m1, m2, --, m4
!   iregime=6 --> do m1, m2, m3, m4

      call gasrate_m1( indx, s, r_m1, r_m2, rk_m1, rk_m2, &
                           h2o, ch4, oxygen, nitrogen, hydrogen )

      if ((iregime .eq. 2) .or.   &
          (iregime .eq. 3) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasrate_m2( indx, s, r_m2, rk_m2 )

      if ((iregime .eq. 3) .or.   &
          (iregime .eq. 6))       &
          call gasrate_m3( indx, s, r_m3, rk_m3 )

      if ((iregime .eq. 4) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasrate_m4( indx, s, r_m4, rk_m4, oxygen )

      call gasode_m1( indx, r_m1, p_m1, d_m1, r_m2 )

      if ((iregime .eq. 2) .or.   &
          (iregime .eq. 3) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasode_m2( indx, r_m2, p_m2, d_m2 )

      if ((iregime .eq. 3) .or.   &
          (iregime .eq. 6))       &
          call gasode_m3( indx, r_m3, p_m3, d_m3 )

      if ((iregime .eq. 4) .or.   &
          (iregime .eq. 5) .or.   &
          (iregime .eq. 6))       &
          call gasode_m4( indx, r_m4, p_m4, d_m4 )


      if (iregime .eq. 1) then
! regime = 1
      do ig = 1, ngas_r1
      sdot(ig) = real( dble(p_m1(ig)) -   &
                       dble(d_m1(ig)) )
      end do

      else if (iregime .eq. 2) then
! regime = 2
      do ig = 1, ngas_r2
      sdot(ig) = real( dble(p_m1(ig)+p_m2(ig)) -   &
                       dble(d_m1(ig)+d_m2(ig)) )
      end do

      else if (iregime .eq. 3) then
! regime = 3
      do ig = 1, ngas_r3
      sdot(ig) = real( dble(p_m1(ig)+p_m2(ig)+p_m3(ig)) -   &
                       dble(d_m1(ig)+d_m2(ig)+d_m3(ig)) )
      end do

      else if (iregime .eq. 4) then
! regime = 4
      do ig = 1, ngas_r4
      sdot(ig) = real( dble(p_m1(ig)+p_m4(ig)) -   &
                       dble(d_m1(ig)+d_m4(ig)) )
      end do
      
      else if (iregime .eq. 5) then
! regime = 5
      do ig = 1, ngas_r5
      sdot(ig) = real( dble(p_m1(ig)+p_m2(ig)+p_m4(ig)) -   &
                       dble(d_m1(ig)+d_m2(ig)+d_m4(ig)) )
      end do

      else if (iregime .eq. 6) then
! regime = 6
      do ig = 1, ngas_r6
      sdot(ig) = real( dble(p_m1(ig)+p_m2(ig)+p_m3(ig)+p_m4(ig)) -   &
                       dble(d_m1(ig)+d_m2(ig)+d_m3(ig)+d_m4(ig)) )
      end do
 
      end if

      return
      end subroutine gasode_cbmz
 
 
 
!***********************************************************************
! <15.> subr jcs
!
! purpose: external dummy jacobian evaluation for LSODES (when mf=222)
!
!-----------------------------------------------------------------------

      subroutine jcs( ngas, tt, s, j, ian, jan, pdj,   &
          ruserpar, nruserpar, iuserpar, niuserpar )

      implicit none
      integer ngas, j, ian(*), jan(*), nruserpar, niuserpar
      integer iuserpar(niuserpar)
      real tt, s(*), pdj(*)
      real ruserpar(nruserpar)

! test on userpar
      call check_userpar( ruserpar, nruserpar, iuserpar, niuserpar )

      return
      end subroutine jcs                                 
 
 
 
!***********************************************************************
! <16.> subr gasode_m1
!
! purpose: updates production and destruction rates for mechanism 1
!          background troposphere
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine gasode_m1( indx, r_m1, p_m1, d_m1, r_m2 )

      use module_data_cbmz
      implicit none

!   subr arguments
      integer indx(ngas_z)
      real r_m1(nrxn_m1), p_m1(ngas_tot), d_m1(ngas_tot)
      real r_m2(nrxn_m2)


      p_m1(indx(ino_z))= r_m1(1)+0.11*r_m1(2)   &
             +r_m1(3)+r_m1(15)+r_m1(38)
      d_m1(indx(ino_z))= r_m1(17)+r_m1(18)+r_m1(23)   &
             +r_m1(33)+r_m1(37)+r_m1(57)   &
             +r_m1(58)   &
             +r_m2(34)
!
      p_m1(indx(ino2_z))= 0.89*r_m1(2)+r_m1(4)   &
              +r_m1(5)+r_m1(6)+r_m1(17)   &
              +r_m1(18)+r_m1(25)   &
              +r_m1(26)+r_m1(28)   &
              +r_m1(33)+r_m1(36)   &
              +r_m1(37)+r_m1(37)   &
              +r_m1(38)+r_m1(40)   &
              +r_m1(40)+.7*r_m1(41)   &
              +r_m1(43)+r_m1(57)   &
              +r_m1(58)+r_m1(59)   &
              +r_m1(60)   &
              +r_m2(32)+r_m2(34)+r_m2(39)
      d_m1(indx(ino2_z))= r_m1(1)+r_m1(15)+r_m1(16)   &
              +r_m1(19)+r_m1(24)   &
              +r_m1(34)+r_m1(35)   &
              +r_m1(38)+r_m1(39)   &
              +r_m2(31)
!
      p_m1(indx(ino3_z))= r_m1(6)+r_m1(16)+r_m1(19)   &
              +r_m1(27)+r_m1(43)
      d_m1(indx(ino3_z))= r_m1(2)+r_m1(25)+r_m1(37)   &
              +r_m1(38)+r_m1(39)   &
              +r_m1(40)+r_m1(40)   &
              +r_m1(41)+r_m1(52)   &
              +r_m1(59)+r_m1(60)   &
              +r_m2(4)+r_m2(39)
!
      p_m1(indx(in2o5_z))= r_m1(39)
      d_m1(indx(in2o5_z))= r_m1(6)+r_m1(42)   &
               +r_m1(43)
!
      p_m1(indx(ihono_z))= r_m1(23)+r_m1(35)
      d_m1(indx(ihono_z))= r_m1(3)+r_m1(26)
!
      p_m1(indx(ihno3_z))= r_m1(24)+.3*r_m1(41)   &
               +r_m1(42)+r_m1(42)   &
               +r_m1(52)   &
               +r_m2(4)
      d_m1(indx(ihno3_z))= r_m1(4)+r_m1(27)
!
      p_m1(indx(ihno4_z))= r_m1(34)
      d_m1(indx(ihno4_z))= r_m1(5)+r_m1(28)   &
               +r_m1(36)
!
      p_m1(indx(io3_z))= r_m1(13)   &
               +.4*r_m2(44)
      d_m1(indx(io3_z))= r_m1(7)+r_m1(8)+r_m1(14)   &
             +r_m1(18)+r_m1(19)+r_m1(20)   &
             +r_m1(21)
!
      p_m1(indx(io1d_z))= r_m1(8)
      d_m1(indx(io1d_z))= r_m1(10)+r_m1(11)   &
              +r_m1(12)
!
      p_m1(indx(io3p_z))= r_m1(1)+0.89*r_m1(2)   &
              +r_m1(7)+r_m1(10)+r_m1(11)
      d_m1(indx(io3p_z))= r_m1(13)+r_m1(14)   &
              +r_m1(15)+r_m1(16)   &
              +r_m1(17)
!
      p_m1(indx(ioh_z))= r_m1(3)+r_m1(4)+2*r_m1(9)   &
             +2*r_m1(12)+r_m1(21)   &
             +r_m1(33)+.7*r_m1(41)   &
             +r_m1(53)+r_m1(54)+.3*r_m1(55)   &
             +.5*r_m1(56)
      d_m1(indx(ioh_z))= r_m1(20)+r_m1(22)+r_m1(23)   &
             +r_m1(24)+r_m1(25)+r_m1(26)   &
             +r_m1(27)+r_m1(28)+r_m1(29)   &
             +r_m1(30)+r_m1(44)+r_m1(45)   &
             +r_m1(46)+r_m1(47)+r_m1(48)   &
             +r_m1(51)+r_m1(55)+r_m1(56)   &
             +r_m1(65)   &
             +r_m2(3)
!
      p_m1(indx(iho2_z))= r_m1(5)+r_m1(20)+r_m1(22)   &
              +r_m1(25)+r_m1(30)   &
              +r_m1(36)+r_m1(44)   &
              +r_m1(45)+r_m1(48)   &
              +2*r_m1(49)+r_m1(51)   &
              +r_m1(52)+r_m1(53)   &
              +r_m1(54)+r_m1(57)   &
              +r_m1(58)+r_m1(59)   &
              +r_m1(60)+.32*r_m1(63)   &
              +.6*r_m1(64)+r_m1(65)   &
              +r_m2(2)
      d_m1(indx(iho2_z))= r_m1(21)+r_m1(29)   &
              +r_m1(31)+r_m1(31)   &
              +r_m1(32)+r_m1(32)   &
              +r_m1(33)+r_m1(34)   &
              +r_m1(35)+r_m1(41)   &
              +r_m1(61)+r_m1(62)   &
              +r_m2(44)
!
      p_m1(indx(ih2o2_z))= r_m1(31)+r_m1(32)
      d_m1(indx(ih2o2_z))= r_m1(9)+r_m1(30)
!
      p_m1(indx(ico_z))= r_m1(49)+r_m1(50)+r_m1(51)   &
             +r_m1(52)   &
             +r_m2(2)
      d_m1(indx(ico_z))= r_m1(44)
!
      p_m1(indx(iso2_z))= 0.0
      d_m1(indx(iso2_z))= r_m1(45)
!
      p_m1(indx(ih2so4_z))= r_m1(45)
      d_m1(indx(ih2so4_z))= 0.0
!
      p_m1(indx(inh3_z))= 0.0
      d_m1(indx(inh3_z))= 0.0
!
      p_m1(indx(ihcl_z))= 0.0
      d_m1(indx(ihcl_z))= 0.0
!
      p_m1(indx(ic2h6_z))= .2*r_m1(64)
      d_m1(indx(ic2h6_z))= r_m1(47)
!
      p_m1(indx(ich3o2_z))= r_m1(46)+.7*r_m1(55)   &
                +r_m2(2)+r_m2(34)+r_m2(39)+r_m2(49)
      d_m1(indx(ich3o2_z))= r_m1(57)+r_m1(59)   &
                +r_m1(61)+r_m1(63)
!
      p_m1(indx(iethp_z))= r_m1(47)+.5*r_m1(56)
      d_m1(indx(iethp_z))= r_m1(58)+r_m1(60)   &
               +r_m1(62)+r_m1(64)
!
      p_m1(indx(ihcho_z))= r_m1(48)+r_m1(53)   &
               +.3*r_m1(55)+r_m1(57)   &
               +r_m1(59)+.66*r_m1(63)
      d_m1(indx(ihcho_z))= r_m1(49)+r_m1(50)   &
               +r_m1(51)+r_m1(52)
!
      p_m1(indx(ich3oh_z))= .34*r_m1(63)
      d_m1(indx(ich3oh_z))= r_m1(48)
!
      p_m1(indx(ic2h5oh_z))= 0.0
      d_m1(indx(ic2h5oh_z))= r_m1(65)
!
      p_m1(indx(ich3ooh_z))= r_m1(61)
      d_m1(indx(ich3ooh_z))= r_m1(53)+r_m1(55)
!
      p_m1(indx(iethooh_z))= r_m1(62)
      d_m1(indx(iethooh_z))= r_m1(54)+r_m1(56)
!
      p_m1(indx(iald2_z))= r_m1(54)+.5*r_m1(56)   &
               +r_m1(58)+r_m1(60)   &
               +.8*r_m1(64)+r_m1(65)
      d_m1(indx(iald2_z))= r_m2(2)+r_m2(3)+r_m2(4)
!
      p_m1(indx(ihcooh_z))= 0.0
      d_m1(indx(ihcooh_z))= 0.0
!
      p_m1(indx(ircooh_z))= .4*r_m2(44)
      d_m1(indx(ircooh_z))= 0.0
!
      p_m1(indx(ic2o3_z))= r_m2(3)+r_m2(4)+r_m2(32)
      d_m1(indx(ic2o3_z))= r_m2(31)+r_m2(34)+r_m2(39)+r_m2(44)+r_m2(49)
!
      p_m1(indx(ipan_z))= r_m2(31)
      d_m1(indx(ipan_z))= r_m2(32)
 
      return
      end subroutine gasode_m1
 
 
 
!***********************************************************************
! <17.> subr gasode_m2
!
! purpose: updates production and destruction rates for mechanism 2
!          anthropogenic hydrocarbons
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine gasode_m2( indx, r_m2, p_m2, d_m2 )

      use module_data_cbmz
      implicit none

!   subr arguments
      integer indx(ngas_z)
      real r_m2(nrxn_m2), p_m2(ngas_tot), d_m2(ngas_tot)


      p_m2(indx(ino_z))= 0.0
      d_m2(indx(ino_z))= r_m2(20)+r_m2(33)         +r_m2(35)   &
              +r_m2(36)+r_m2(37)
!
      p_m2(indx(ino2_z))= .95*r_m2(20)+r_m2(30)         +.84*r_m2(33)   &
                       +r_m2(35)+1.5*r_m2(36)+r_m2(37)   &
              +r_m2(38)         +r_m2(40)+1.5*r_m2(41)+r_m2(42)   &
              +.5*r_m2(51)
      d_m2(indx(ino2_z))= r_m2(23)
!
      p_m2(indx(ino3_z))= 0.0
      d_m2(indx(ino3_z))=        +r_m2(9)+r_m2(16)+r_m2(17)+r_m2(22)   &
              +r_m2(38)         +r_m2(40)+r_m2(41)+r_m2(42)
!
      p_m2(indx(ihno3_z))=        +r_m2(9)+r_m2(22)
      d_m2(indx(ihno3_z))= 0.0
!
      p_m2(indx(io3_z))= 0.0
      d_m2(indx(io3_z))= r_m2(10)+r_m2(12)+r_m2(13)+r_m2(26)
!
      p_m2(indx(ioh_z))= .12*r_m2(10)+.33*r_m2(12)+.60*r_m2(13)   &
             +.08*r_m2(26)+r_m2(27)+.23*r_m2(28)
      d_m2(indx(ioh_z))= r_m2(1)        +r_m2(6)+r_m2(8)+r_m2(11)   &
             +r_m2(14)+r_m2(15)+r_m2(18)+r_m2(19)+r_m2(21)   &
             +r_m2(24)+r_m2(28)+r_m2(29)
!
      p_m2(indx(iho2_z))=        +r_m2(7)+.22*r_m2(10)+r_m2(11)   &
             +.26*r_m2(12)+.22*r_m2(13)+r_m2(14)+r_m2(15)   &
             +.2*r_m2(18)+.55*r_m2(19)+.95*r_m2(20)   &
             +.6*r_m2(21)+2*r_m2(24)+r_m2(25)+.76*r_m2(26)   &
             +.9*r_m2(27)+.9*r_m2(30)+.76*r_m2(33)+.5*r_m2(36)   &
             +.9*r_m2(38)+.5*r_m2(41)+.54*r_m2(48)
      d_m2(indx(iho2_z))= r_m2(43)         +r_m2(45)+r_m2(46)+r_m2(47)
!
      p_m2(indx(ico_z))=        +r_m2(7)+r_m2(9)+.24*r_m2(10)   &
             +.31*r_m2(12)+.30*r_m2(13)+2*r_m2(24)+r_m2(25)   &
             +.69*r_m2(26)
      d_m2(indx(ico_z))= 0.0
!
      p_m2(indx(ipar_z))= 1.1*r_m2(19)
      d_m2(indx(ipar_z))= r_m2(1) + r_m2(53)
!
      p_m2(indx(ich3oh_z))= .03*r_m2(12)+.04*r_m2(13)
      d_m2(indx(ich3oh_z))= 0.0
!
      p_m2(indx(ihcho_z))= r_m2(10)+1.56*r_m2(11)+.57*r_m2(12)+r_m2(14)   &
               +r_m2(24)+.7*r_m2(26)+r_m2(35)+.5*r_m2(36)   &
               +r_m2(40)+.5*r_m2(41)+.7*r_m2(50)+.5*r_m2(51)
      d_m2(indx(ihcho_z))= 0.0
!
      p_m2(indx(iald2_z))= .22*r_m2(11)+.47*r_m2(12)+1.03*r_m2(13)   &
               +r_m2(14)+1.77*r_m2(15)+.03*r_m2(26)+.3*r_m2(27)   &
               +.04*r_m2(28)+.3*r_m2(30)+.25*r_m2(33)+.5*r_m2(36)   &
               +.3*r_m2(38)+.5*r_m2(41)+.21*r_m2(48)+.5*r_m2(51)
      d_m2(indx(iald2_z))= 0.0
!
      p_m2(indx(ihcooh_z))= .52*r_m2(10)+.22*r_m2(12)
      d_m2(indx(ihcooh_z))= 0.0
!
      p_m2(indx(iaone_z))= .07*r_m2(13)+.23*r_m2(15)+.74*r_m2(27)   &
               +.74*r_m2(30)+.62*r_m2(33)+.74*r_m2(38)   &
               +.57*r_m2(48)+.15*r_m2(50)
      d_m2(indx(iaone_z))= r_m2(5)+r_m2(6)
!
      p_m2(indx(imgly_z))= .04*r_m2(12)+.07*r_m2(13)+.8*r_m2(19)   &
               +.2*r_m2(26)+.19*r_m2(28)+.15*r_m2(50)
      d_m2(indx(imgly_z))= r_m2(7)+r_m2(8)+r_m2(9)
!
      p_m2(indx(ieth_z))= 0.0
      d_m2(indx(ieth_z))= r_m2(10)+r_m2(11)
!
      p_m2(indx(iolet_z))= 0.0
      d_m2(indx(iolet_z))= r_m2(12)+r_m2(14)+r_m2(16)
!
      p_m2(indx(iolei_z))= 0.0
      d_m2(indx(iolei_z))= r_m2(13)+r_m2(15)+r_m2(17)
!
      p_m2(indx(itol_z))= 0.0
      d_m2(indx(itol_z))= r_m2(18)
!
      p_m2(indx(ixyl_z))= 0.0
      d_m2(indx(ixyl_z))= r_m2(19)
!
      p_m2(indx(icres_z))= .12*r_m2(18)+.05*r_m2(19)
      d_m2(indx(icres_z))= r_m2(21)+r_m2(22)
!
      p_m2(indx(ito2_z))= .8*r_m2(18)+.45*r_m2(19)
      d_m2(indx(ito2_z))= r_m2(20)
!
      p_m2(indx(icro_z))= .4*r_m2(21)+r_m2(22)
      d_m2(indx(icro_z))= r_m2(23)
!
      p_m2(indx(iopen_z))= .95*r_m2(20)+.3*r_m2(21)
      d_m2(indx(iopen_z))= r_m2(24)+r_m2(25)+r_m2(26)
!
      p_m2(indx(ionit_z))= .05*r_m2(20)+r_m2(23)+.16*r_m2(33)   &
               +.5*r_m2(36)+.5*r_m2(41)+r_m2(46)+.5*r_m2(51)
      d_m2(indx(ionit_z))= r_m2(29)+r_m2(30)
!
      p_m2(indx(ipan_z))= 0.0
      d_m2(indx(ipan_z))= 0.0
!
      p_m2(indx(ircooh_z))= .09*r_m2(12)+.16*r_m2(13)
      d_m2(indx(ircooh_z))= 0.0
!
      p_m2(indx(irooh_z))= r_m2(43)+r_m2(45)
      d_m2(indx(irooh_z))= r_m2(27)+r_m2(28)
!
      p_m2(indx(ich3o2_z))=        +r_m2(5)+.07*r_m2(12)+.10*r_m2(13)
      d_m2(indx(ich3o2_z))= 0.0
!
      p_m2(indx(iethp_z))= .06*r_m2(12)+.05*r_m2(13)+.1*r_m2(27)   &
               +.1*r_m2(30)+.08*r_m2(33)+.1*r_m2(38)+.06*r_m2(48)
      d_m2(indx(iethp_z))= 0.0
!
      p_m2(indx(ic2o3_z))=                +r_m2(5)+r_m2(7)+r_m2(8)   &
               +r_m2(9)+.13*r_m2(12)+.19*r_m2(13)+r_m2(24)   &
               +r_m2(25)+.62*r_m2(26)         +r_m2(35)   &
               +r_m2(40)+.7*r_m2(50)
      d_m2(indx(ic2o3_z))= 0.0
!
      p_m2(indx(iro2_z))= r_m2(1)+.03*r_m2(12)+.09*r_m2(13)+.77*r_m2(28)
      d_m2(indx(iro2_z))= r_m2(33)+r_m2(38)+r_m2(43)+r_m2(48)
!
      p_m2(indx(iano2_z))= r_m2(6)+.11*r_m2(13)
      d_m2(indx(iano2_z))= r_m2(35)+r_m2(40)+r_m2(45)+r_m2(50)
!
      p_m2(indx(inap_z))= r_m2(16)+r_m2(17)+r_m2(29)
      d_m2(indx(inap_z))= r_m2(36)+r_m2(41)+r_m2(46)+r_m2(51)
!
      p_m2(indx(ixo2_z))= r_m2(8)+r_m2(11)+r_m2(14)+r_m2(15)   &
              +.08*r_m2(18)+.5*r_m2(19)+.6*r_m2(21)   &
              +r_m2(24)+.03*r_m2(26)+.4*r_m2(27)+.4*r_m2(30)   &
              +.34*r_m2(33)+.4*r_m2(38)+.24*r_m2(48)
      d_m2(indx(ixo2_z))= r_m2(37)+r_m2(42)+r_m2(47)+r_m2(52)

      p_m2(indx(ixpar_z))= 1.06*r_m2(12)+2.26*r_m2(13)+r_m2(14)   &
              +2.23*r_m2(15)+1.98*r_m2(27)+.42*r_m2(28)   &
              +1.98*r_m2(30)+1.68*r_m2(33)+r_m2(36)   &
              +1.98*r_m2(38)+r_m2(41)+1.25*r_m2(48)+r_m2(51)
      d_m2(indx(ixpar_z))= r_m2(53)
!
      return
      end subroutine gasode_m2
 
 
 
!***********************************************************************
! <18.> subr gasode_m3
!
! purpose: updates production and destruction rates for mechanism 3
!          isoprene
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine gasode_m3( indx, r_m3, p_m3, d_m3 )

      use module_data_cbmz
      implicit none

!   subr arguments
      integer indx(ngas_z)
      real r_m3(nrxn_m3), p_m3(ngas_tot), d_m3(ngas_tot)


      p_m3(indx(ino_z))= 0.0
      d_m3(indx(ino_z))= r_m3(8)+r_m3(9)+r_m3(10)
!
      p_m3(indx(ino2_z))= .91*r_m3(8)+1.2*r_m3(9)+r_m3(10)
      d_m3(indx(ino2_z))= 0.0
!
      p_m3(indx(ino3_z))= 0.0
      d_m3(indx(ino3_z))= r_m3(3)+r_m3(7)
!
      p_m3(indx(ihno3_z))= .07*r_m3(7)
      d_m3(indx(ihno3_z))= 0.0
!
      p_m3(indx(io3_z))= 0.0
      d_m3(indx(io3_z))= r_m3(2)+r_m3(6)
!
      p_m3(indx(ioh_z))= .27*r_m3(2)+.27*r_m3(6)
      d_m3(indx(ioh_z))= r_m3(1)+r_m3(5)
!
      p_m3(indx(iho2_z))= .07*r_m3(2)+.33*r_m3(4)+.1*r_m3(6)+.93*r_m3(7)   &
              +.91*r_m3(8)+.8*r_m3(9)+r_m3(10)
      d_m3(indx(iho2_z))= r_m3(11)+r_m3(12)+r_m3(13)
!
      p_m3(indx(ico_z))= .07*r_m3(2)+.33*r_m3(4)+.16*r_m3(6)+.64*r_m3(7)   &
             +.59*r_m3(10)
      d_m3(indx(ico_z))= 0.0
!
      p_m3(indx(ipar_z))= 1.86*r_m3(7)+0.18*r_m3(8)+1.6*r_m3(9)+2*r_m3(12)   &
              +2*r_m3(15)
      d_m3(indx(ipar_z))= 0.0
!
      p_m3(indx(ihcho_z))= .6*r_m3(2)+.2*r_m3(4)+.15*r_m3(6)+.28*r_m3(7)   &
               +.63*r_m3(8)+.25*r_m3(10)
      d_m3(indx(ihcho_z))= 0.0
!
      p_m3(indx(iald2_z))= .15*r_m3(2)+.07*r_m3(4)+.02*r_m3(6)+.28*r_m3(7)   &
               +.8*r_m3(9)+.55*r_m3(10)+r_m3(15)+.5*r_m3(16)
      d_m3(indx(iald2_z))= 0.0
!
      p_m3(indx(iaone_z))= .03*r_m3(4)+.09*r_m3(6)+.63*r_m3(10)+.5*r_m3(16)
      d_m3(indx(iaone_z))= 0.0
!
      p_m3(indx(imgly_z))= .85*r_m3(6)+.34*r_m3(10)
      d_m3(indx(imgly_z))= 0.0
!
      p_m3(indx(ionit_z))= .93*r_m3(7)+.09*r_m3(8)+.8*r_m3(9)+r_m3(12)   &
               +r_m3(15)
      d_m3(indx(ionit_z))= 0.0
!
      p_m3(indx(ihcooh_z))= .39*r_m3(2)+.46*r_m3(6)
      d_m3(indx(ihcooh_z))= 0.0
!
      p_m3(indx(irooh_z))= r_m3(11)+r_m3(13)
      d_m3(indx(irooh_z))= 0.0
!
      p_m3(indx(ich3o2_z))= .7*r_m3(4)+.05*r_m3(6)
      d_m3(indx(ich3o2_z))= 0.0
!
      p_m3(indx(ic2o3_z))= .2*r_m3(2)+.97*r_m3(4)+.5*r_m3(5)+.11*r_m3(6)   &
               +.07*r_m3(7)
      d_m3(indx(ic2o3_z))= 0.0
!
      p_m3(indx(ixo2_z))= .08*r_m3(1)+.2*r_m3(2)+.2*r_m3(5)+.07*r_m3(6)   &
              +.93*r_m3(7)
      d_m3(indx(ixo2_z))= 0.0
!
      p_m3(indx(iisop_z))= 0.0
      d_m3(indx(iisop_z))= r_m3(1)+r_m3(2)+r_m3(3)
!
      p_m3(indx(iisoprd_z))= .65*r_m3(2)+.91*r_m3(8)+.2*r_m3(9)+r_m3(14)
      d_m3(indx(iisoprd_z))= r_m3(4)+r_m3(5)+r_m3(6)+r_m3(7)
!
      p_m3(indx(iisopp_z))= r_m3(1)
      d_m3(indx(iisopp_z))= r_m3(8)+r_m3(11)+r_m3(14)
!
      p_m3(indx(iisopn_z))= r_m3(3)
      d_m3(indx(iisopn_z))= r_m3(9)+r_m3(12)+r_m3(15)
!
      p_m3(indx(iisopo2_z))= .5*r_m3(5)
      d_m3(indx(iisopo2_z))= r_m3(10)+r_m3(13)+r_m3(16)
!
      return
      end subroutine gasode_m3

 
 
!***********************************************************************
! <19.> subr gasode_m4
!
! purpose: updates production and destruction rates for mechanism 4
!          dimethylsulfide
!
! author : Rahul A. Zaveri
! date   : December, 1998
!
! ----------------------------------------------------------------------

      subroutine gasode_m4( indx, r_m4, p_m4, d_m4 )

      use module_data_cbmz
      implicit none

!   subr arguments
      integer indx(ngas_z)
      real r_m4(nrxn_m4), p_m4(ngas_tot), d_m4(ngas_tot)


      p_m4(indx(ino_z))= r_m4(19)
      d_m4(indx(ino_z))= r_m4(5)+r_m4(11)+r_m4(26)+r_m4(30)
!
      p_m4(indx(ino2_z))= r_m4(5)+r_m4(11)+r_m4(26)
      d_m4(indx(ino2_z))= r_m4(19)+r_m4(29)
!
      p_m4(indx(ino3_z))= 0.0
      d_m4(indx(ino3_z))= r_m4(2)+r_m4(14)
!
      p_m4(indx(ihono_z))= r_m4(30)
      d_m4(indx(ihono_z))= 0.0
!
      p_m4(indx(ihno3_z))= r_m4(2)+r_m4(14)+r_m4(29)
      d_m4(indx(ihno3_z))= 0.0
!
      p_m4(indx(io3_z))= 0.0
      d_m4(indx(io3_z))= r_m4(20)
!
      p_m4(indx(io3p_z))= 0.0
      d_m4(indx(io3p_z))= r_m4(3)
!
      p_m4(indx(ioh_z))= r_m4(21)
      d_m4(indx(ioh_z))= r_m4(1)+r_m4(4)+r_m4(9)+r_m4(10)+r_m4(16)+r_m4(23)
!
      p_m4(indx(iho2_z))= .965*r_m4(4)+r_m4(6)+.27*r_m4(9)+r_m4(12)+r_m4(22)   &
              +r_m4(27)+r_m4(32)
      d_m4(indx(iho2_z))= r_m4(13)+r_m4(21)+r_m4(31)
!
      p_m4(indx(ih2o2_z))= r_m4(13)
      d_m4(indx(ih2o2_z))= 0.0
!
      p_m4(indx(ico_z))= r_m4(32)
      d_m4(indx(ico_z))= 0.0
!
      p_m4(indx(iso2_z))= r_m4(18)
      d_m4(indx(iso2_z))= 0.0
!
      p_m4(indx(ih2so4_z))= r_m4(28)
      d_m4(indx(ih2so4_z))= 0.0
!
      p_m4(indx(ihcho_z))= r_m4(5)+2*r_m4(6)+r_m4(7)+r_m4(11)+2*r_m4(12)   &
               +r_m4(22)+r_m4(27)
      d_m4(indx(ihcho_z))= r_m4(32)
!
      p_m4(indx(ich3o2_z))= r_m4(3)+.035*r_m4(4)+.73*r_m4(9)+r_m4(18)+r_m4(28)
      d_m4(indx(ich3o2_z))= r_m4(6)+r_m4(12)+r_m4(15)+r_m4(22)+r_m4(27)
!
      p_m4(indx(ich3ooh_z))= r_m4(15)
      d_m4(indx(ich3ooh_z))= 0.0
!
      p_m4(indx(idms_z))= 0.0
      d_m4(indx(idms_z))= r_m4(1)+r_m4(2)+r_m4(3)+r_m4(4)
!
      p_m4(indx(imsa_z))= r_m4(17)+r_m4(23)+r_m4(29)+r_m4(30)+r_m4(31)+r_m4(32)
      d_m4(indx(imsa_z))= 0.0
!
      p_m4(indx(idmso_z))= .965*r_m4(4)
      d_m4(indx(idmso_z))= r_m4(9)
!
      p_m4(indx(idmso2_z))= .27*r_m4(9)
      d_m4(indx(idmso2_z))= r_m4(10)
!
      p_m4(indx(ich3so2h_z))= .73*r_m4(9)
      d_m4(indx(ich3so2h_z))= r_m4(13)+r_m4(14)+r_m4(15)+r_m4(16)+r_m4(17)
!
      p_m4(indx(ich3sch2oo_z))= r_m4(1)+r_m4(2)
      d_m4(indx(ich3sch2oo_z))= r_m4(5)+r_m4(6)+r_m4(7)+r_m4(8)+r_m4(8)
!
      p_m4(indx(ich3so2_z))= r_m4(3)+.035*r_m4(4)+r_m4(5)+r_m4(6)+r_m4(7)   &
                 +1.85*r_m4(8)   &
                 +r_m4(11)+r_m4(12)+r_m4(13)+r_m4(14)+r_m4(15)   &
                 +r_m4(16)+r_m4(17)+r_m4(25)
      d_m4(indx(ich3so2_z))= r_m4(7)+r_m4(18)+r_m4(19)+r_m4(20)+r_m4(21)   &
                 +r_m4(22)+r_m4(23)+r_m4(24)
!
      p_m4(indx(ich3so3_z))= r_m4(7)+r_m4(19)+r_m4(20)+r_m4(21)+r_m4(22)   &
                 +r_m4(26)+r_m4(27)
      d_m4(indx(ich3so3_z))= r_m4(17)+r_m4(28)+r_m4(29)+r_m4(30)+r_m4(31)   &
                 +r_m4(32)
!
      p_m4(indx(ich3so2oo_z))= r_m4(24)
      d_m4(indx(ich3so2oo_z))= r_m4(25)+r_m4(26)+r_m4(27)
!
      p_m4(indx(ich3so2ch2oo_z))= r_m4(10)
      d_m4(indx(ich3so2ch2oo_z))= r_m4(11)+r_m4(12)
!
      p_m4(indx(imtf_z))= .15*r_m4(8)
      d_m4(indx(imtf_z))= 0.0
!
      return
      end subroutine gasode_m4
 
 
 
!***********************************************************************
! <20.> subr gasrate_m1
!
! purpose: computes reaction rates for mechanism 1
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------

      subroutine gasrate_m1( indx, s, r_m1, r_m2, rk_m1, rk_m2,   &
                             h2o, ch4, oxygen, nitrogen, hydrogen )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer indx(ngas_z)
      real s(ngas_tot), r_m1(nrxn_m1), r_m2(nrxn_m2)
      real rk_m1(nrxn_m1), rk_m2(nrxn_m2)
      real h2o, ch4, oxygen, nitrogen, hydrogen

      r_m1(1)  = rk_m1(1)*s(indx(ino2_z))
      r_m1(2)  = rk_m1(2)*s(indx(ino3_z))
      r_m1(3)  = rk_m1(3)*s(indx(ihono_z))
      r_m1(4)  = rk_m1(4)*s(indx(ihno3_z))
      r_m1(5)  = rk_m1(5)*s(indx(ihno4_z))
      r_m1(6)  = rk_m1(6)*s(indx(in2o5_z))
      r_m1(7)  = rk_m1(7)*s(indx(io3_z))
      r_m1(8)  = rk_m1(8)*s(indx(io3_z))
      r_m1(9)  = rk_m1(9)*s(indx(ih2o2_z))
      r_m1(10) = rk_m1(10)*s(indx(io1d_z))*oxygen
      r_m1(11) = rk_m1(11)*s(indx(io1d_z))*nitrogen
      r_m1(12) = rk_m1(12)*s(indx(io1d_z))*h2o
      r_m1(13) = rk_m1(13)*s(indx(io3p_z))*oxygen
      r_m1(14) = rk_m1(14)*s(indx(io3p_z))*s(indx(io3_z))
      r_m1(15) = rk_m1(15)*s(indx(io3p_z))*s(indx(ino2_z))
      r_m1(16) = rk_m1(16)*s(indx(io3p_z))*s(indx(ino2_z))
      r_m1(17) = rk_m1(17)*s(indx(io3p_z))*s(indx(ino_z))
      r_m1(18) = rk_m1(18)*s(indx(io3_z))*s(indx(ino_z))
      r_m1(19) = rk_m1(19)*s(indx(io3_z))*s(indx(ino2_z))
      r_m1(20) = rk_m1(20)*s(indx(io3_z))*s(indx(ioh_z))
      r_m1(21) = rk_m1(21)*s(indx(io3_z))*s(indx(iho2_z))
      r_m1(22) = rk_m1(22)*s(indx(ioh_z))*hydrogen
      r_m1(23) = rk_m1(23)*s(indx(ioh_z))*s(indx(ino_z))
      r_m1(24) = rk_m1(24)*s(indx(ioh_z))*s(indx(ino2_z))
      r_m1(25) = rk_m1(25)*s(indx(ioh_z))*s(indx(ino3_z))
      r_m1(26) = rk_m1(26)*s(indx(ioh_z))*s(indx(ihono_z))
      r_m1(27) = rk_m1(27)*s(indx(ioh_z))*s(indx(ihno3_z))
      r_m1(28) = rk_m1(28)*s(indx(ioh_z))*s(indx(ihno4_z))
      r_m1(29) = rk_m1(29)*s(indx(ioh_z))*s(indx(iho2_z))
      r_m1(30) = rk_m1(30)*s(indx(ioh_z))*s(indx(ih2o2_z))
      r_m1(31) = rk_m1(31)*s(indx(iho2_z))*s(indx(iho2_z))
      r_m1(32) = rk_m1(32)*s(indx(iho2_z))*s(indx(iho2_z))*h2o
      r_m1(33) = rk_m1(33)*s(indx(iho2_z))*s(indx(ino_z))
      r_m1(34) = rk_m1(34)*s(indx(iho2_z))*s(indx(ino2_z))
      r_m1(35) = rk_m1(35)*s(indx(iho2_z))*s(indx(ino2_z))
      r_m1(36) = rk_m1(36)*s(indx(ihno4_z))
      r_m1(37) = rk_m1(37)*s(indx(ino3_z))*s(indx(ino_z))
      r_m1(38) = rk_m1(38)*s(indx(ino3_z))*s(indx(ino2_z))
      r_m1(39) = rk_m1(39)*s(indx(ino3_z))*s(indx(ino2_z))
      r_m1(40) = rk_m1(40)*s(indx(ino3_z))*s(indx(ino3_z))
      r_m1(41) = rk_m1(41)*s(indx(ino3_z))*s(indx(iho2_z))
      r_m1(42) = rk_m1(42)*s(indx(in2o5_z))*h2o
      r_m1(43) = rk_m1(43)*s(indx(in2o5_z))
      r_m1(44) = rk_m1(44)*s(indx(ico_z))*s(indx(ioh_z))
      r_m1(45) = rk_m1(45)*s(indx(iso2_z))*s(indx(ioh_z))
      r_m1(46) = rk_m1(46)*s(indx(ioh_z))*ch4
      r_m1(47) = rk_m1(47)*s(indx(ic2h6_z))*s(indx(ioh_z))
      r_m1(48) = rk_m1(48)*s(indx(ich3oh_z))*s(indx(ioh_z))
      r_m1(49) = rk_m1(49)*s(indx(ihcho_z))
      r_m1(50) = rk_m1(50)*s(indx(ihcho_z))
      r_m1(51) = rk_m1(51)*s(indx(ihcho_z))*s(indx(ioh_z))
      r_m1(52) = rk_m1(52)*s(indx(ihcho_z))*s(indx(ino3_z))
      r_m1(53) = rk_m1(53)*s(indx(ich3ooh_z))
      r_m1(54) = rk_m1(54)*s(indx(iethooh_z))
      r_m1(55) = rk_m1(55)*s(indx(ich3ooh_z))*s(indx(ioh_z))
      r_m1(56) = rk_m1(56)*s(indx(iethooh_z))*s(indx(ioh_z))
      r_m1(57) = rk_m1(57)*s(indx(ich3o2_z))*s(indx(ino_z))
      r_m1(58) = rk_m1(58)*s(indx(iethp_z))*s(indx(ino_z))
      r_m1(59) = rk_m1(59)*s(indx(ich3o2_z))*s(indx(ino3_z))
      r_m1(60) = rk_m1(60)*s(indx(iethp_z))*s(indx(ino3_z))
      r_m1(61) = rk_m1(61)*s(indx(ich3o2_z))*s(indx(iho2_z))
      r_m1(62) = rk_m1(62)*s(indx(iethp_z))*s(indx(iho2_z))
      r_m1(63) = rk_m1(63)*s(indx(ich3o2_z))
      r_m1(64) = rk_m1(64)*s(indx(iethp_z))
      r_m1(65) = rk_m1(65)*s(indx(ic2h5oh_z))*s(indx(ioh_z))

      r_m2(2)  = rk_m2(2)*s(indx(iald2_z))
      r_m2(3)  = rk_m2(3)*s(indx(iald2_z))*s(indx(ioh_z))
      r_m2(4)  = rk_m2(4)*s(indx(iald2_z))*s(indx(ino3_z))
      r_m2(31) = rk_m2(31)*s(indx(ic2o3_z))*s(indx(ino2_z))
      r_m2(32) = rk_m2(32)*s(indx(ipan_z))
      r_m2(34) = rk_m2(34)*s(indx(ic2o3_z))*s(indx(ino_z))
      r_m2(39) = rk_m2(39)*s(indx(ic2o3_z))*s(indx(ino3_z))
      r_m2(44) = rk_m2(44)*s(indx(ic2o3_z))*s(indx(iho2_z))
      r_m2(49) = rk_m2(49)*s(indx(ic2o3_z))

      return
      end subroutine gasrate_m1     



!***********************************************************************
! <21.> subr gasrate_m2
!
! purpose: computes reaction rates for mechanism 2
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------
!
      subroutine gasrate_m2( indx, s, r_m2, rk_m2 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer indx(ngas_z)
      real s(ngas_tot), r_m2(nrxn_m2), rk_m2(nrxn_m2)

      r_m2(1)  = rk_m2(1)*s(indx(ipar_z))*s(indx(ioh_z))

      r_m2(5)  = rk_m2(5)*s(indx(iaone_z))
      r_m2(6)  = rk_m2(6)*s(indx(iaone_z))*s(indx(ioh_z))
      r_m2(7)  = rk_m2(7)*s(indx(imgly_z))
      r_m2(8)  = rk_m2(8)*s(indx(imgly_z))*s(indx(ioh_z))
      r_m2(9)  = rk_m2(9)*s(indx(imgly_z))*s(indx(ino3_z))
      r_m2(10) = rk_m2(10)*s(indx(ieth_z))*s(indx(io3_z))
      r_m2(11) = rk_m2(11)*s(indx(ieth_z))*s(indx(ioh_z))
      r_m2(12) = rk_m2(12)*s(indx(iolet_z))*s(indx(io3_z))
      r_m2(13) = rk_m2(13)*s(indx(iolei_z))*s(indx(io3_z))
      r_m2(14) = rk_m2(14)*s(indx(iolet_z))*s(indx(ioh_z))
      r_m2(15) = rk_m2(15)*s(indx(iolei_z))*s(indx(ioh_z))
      r_m2(16) = rk_m2(16)*s(indx(iolet_z))*s(indx(ino3_z))
      r_m2(17) = rk_m2(17)*s(indx(iolei_z))*s(indx(ino3_z))
      r_m2(18) = rk_m2(18)*s(indx(itol_z))*s(indx(ioh_z))
      r_m2(19) = rk_m2(19)*s(indx(ixyl_z))*s(indx(ioh_z))
      r_m2(20) = rk_m2(20)*s(indx(ito2_z))*s(indx(ino_z))
      r_m2(21) = rk_m2(21)*s(indx(icres_z))*s(indx(ioh_z))
      r_m2(22) = rk_m2(22)*s(indx(icres_z))*s(indx(ino3_z))
      r_m2(23) = rk_m2(23)*s(indx(icro_z))*s(indx(ino2_z))
      r_m2(24) = rk_m2(24)*s(indx(iopen_z))*s(indx(ioh_z))
      r_m2(25) = rk_m2(25)*s(indx(iopen_z))
      r_m2(26) = rk_m2(26)*s(indx(iopen_z))*s(indx(io3_z))
      r_m2(27) = rk_m2(27)*s(indx(irooh_z))
      r_m2(28) = rk_m2(28)*s(indx(irooh_z))*s(indx(ioh_z))
      r_m2(29) = rk_m2(29)*s(indx(ionit_z))*s(indx(ioh_z))
      r_m2(30) = rk_m2(30)*s(indx(ionit_z))

      r_m2(33) = rk_m2(33)*s(indx(iro2_z))*s(indx(ino_z))

      r_m2(35) = rk_m2(35)*s(indx(iano2_z))*s(indx(ino_z))
      r_m2(36) = rk_m2(36)*s(indx(inap_z))*s(indx(ino_z))
      r_m2(37) = rk_m2(37)*s(indx(ixo2_z))*s(indx(ino_z))
      r_m2(38) = rk_m2(38)*s(indx(iro2_z))*s(indx(ino3_z))

      r_m2(40) = rk_m2(40)*s(indx(iano2_z))*s(indx(ino3_z))
      r_m2(41) = rk_m2(41)*s(indx(inap_z))*s(indx(ino3_z))
      r_m2(42) = rk_m2(42)*s(indx(ixo2_z))*s(indx(ino3_z))
      r_m2(43) = rk_m2(43)*s(indx(iro2_z))*s(indx(iho2_z))

      r_m2(45) = rk_m2(45)*s(indx(iano2_z))*s(indx(iho2_z))
      r_m2(46) = rk_m2(46)*s(indx(inap_z))*s(indx(iho2_z))
      r_m2(47) = rk_m2(47)*s(indx(ixo2_z))*s(indx(iho2_z))
      r_m2(48) = rk_m2(48)*s(indx(iro2_z))

      r_m2(50) = rk_m2(50)*s(indx(iano2_z))
      r_m2(51) = rk_m2(51)*s(indx(inap_z))
      r_m2(52) = rk_m2(52)*s(indx(ixo2_z))
      r_m2(53) = rk_m2(53)*s(indx(ipar_z))*s(indx(ixpar_z))
!
      return
      end subroutine gasrate_m2     
 
 
 
!***********************************************************************
! <22.> subr gasrate_m3
!
! purpose: computes reaction rates for mechanism 3
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------
!
      subroutine gasrate_m3( indx, s, r_m3, rk_m3 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer indx(ngas_z)
      real s(ngas_tot), r_m3(nrxn_m3), rk_m3(nrxn_m3)

      r_m3(1)  = rk_m3(1)*s(indx(iisop_z))*s(indx(ioh_z))
      r_m3(2)  = rk_m3(2)*s(indx(iisop_z))*s(indx(io3_z))
      r_m3(3)  = rk_m3(3)*s(indx(iisop_z))*s(indx(ino3_z))
      r_m3(4)  = rk_m3(4)*s(indx(iisoprd_z))
      r_m3(5)  = rk_m3(5)*s(indx(iisoprd_z))*s(indx(ioh_z))
      r_m3(6)  = rk_m3(6)*s(indx(iisoprd_z))*s(indx(io3_z))
      r_m3(7)  = rk_m3(7)*s(indx(iisoprd_z))*s(indx(ino3_z))
      r_m3(8)  = rk_m3(8)*s(indx(iisopp_z))*s(indx(ino_z))
      r_m3(9)  = rk_m3(9)*s(indx(iisopn_z))*s(indx(ino_z))
      r_m3(10) = rk_m3(10)*s(indx(iisopo2_z))*s(indx(ino_z))
      r_m3(11) = rk_m3(11)*s(indx(iisopp_z))*s(indx(iho2_z))
      r_m3(12) = rk_m3(12)*s(indx(iisopn_z))*s(indx(iho2_z))
      r_m3(13) = rk_m3(13)*s(indx(iisopo2_z))*s(indx(iho2_z))
      r_m3(14) = rk_m3(14)*s(indx(iisopp_z))
      r_m3(15) = rk_m3(15)*s(indx(iisopn_z))
      r_m3(16) = rk_m3(16)*s(indx(iisopo2_z))
 
      return
      end subroutine gasrate_m3     
 
 
 
!***********************************************************************
! <23.> subr gasrate_m4
!
! purpose: computes reaction rates for mechanism 4
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------
!
      subroutine gasrate_m4( indx, s, r_m4, rk_m4, oxygen )

      use module_data_cbmz
      implicit none

!   subr arguments 
      integer indx(ngas_z)
      real s(ngas_tot), r_m4(nrxn_m4), rk_m4(nrxn_m4)
      real oxygen

      r_m4(1)  = rk_m4(1)*s(indx(idms_z))*s(indx(ioh_z))
      r_m4(2)  = rk_m4(2)*s(indx(idms_z))*s(indx(ino3_z))
      r_m4(3)  = rk_m4(3)*s(indx(idms_z))*s(indx(io3p_z))
      r_m4(4)  = rk_m4(4)*s(indx(idms_z))*s(indx(ioh_z))
      r_m4(5)  = rk_m4(5)*s(indx(ich3sch2oo_z))*s(indx(ino_z))
      r_m4(6)  = rk_m4(6)*s(indx(ich3sch2oo_z))*s(indx(ich3o2_z))
      r_m4(7)  = rk_m4(7)*s(indx(ich3sch2oo_z))*s(indx(ich3so2_z))
      r_m4(8)  = rk_m4(8)*s(indx(ich3sch2oo_z))*s(indx(ich3sch2oo_z))
      r_m4(9)  = rk_m4(9)*s(indx(idmso_z))*s(indx(ioh_z))
      r_m4(10) = rk_m4(10)*s(indx(idmso2_z))*s(indx(ioh_z))
      r_m4(11) = rk_m4(11)*s(indx(ich3so2ch2oo_z))*s(indx(ino_z))
      r_m4(12) = rk_m4(12)*s(indx(ich3so2ch2oo_z))*s(indx(ich3o2_z))
      r_m4(13) = rk_m4(13)*s(indx(ich3so2h_z))*s(indx(iho2_z))
      r_m4(14) = rk_m4(14)*s(indx(ich3so2h_z))*s(indx(ino3_z))
      r_m4(15) = rk_m4(15)*s(indx(ich3so2h_z))*s(indx(ich3o2_z))
      r_m4(16) = rk_m4(16)*s(indx(ich3so2h_z))*s(indx(ioh_z))
      r_m4(17) = rk_m4(17)*s(indx(ich3so2h_z))*s(indx(ich3so3_z))
      r_m4(18) = rk_m4(18)*s(indx(ich3so2_z))
      r_m4(19) = rk_m4(19)*s(indx(ich3so2_z))*s(indx(ino2_z))
      r_m4(20) = rk_m4(20)*s(indx(ich3so2_z))*s(indx(io3_z))
      r_m4(21) = rk_m4(21)*s(indx(ich3so2_z))*s(indx(iho2_z))
      r_m4(22) = rk_m4(22)*s(indx(ich3so2_z))*s(indx(ich3o2_z))
      r_m4(23) = rk_m4(23)*s(indx(ich3so2_z))*s(indx(ioh_z))
      r_m4(24) = rk_m4(24)*s(indx(ich3so2_z))*oxygen
      r_m4(25) = rk_m4(25)*s(indx(ich3so2oo_z))
      r_m4(26) = rk_m4(26)*s(indx(ich3so2oo_z))*s(indx(ino_z))
      r_m4(27) = rk_m4(27)*s(indx(ich3so2oo_z))*s(indx(ich3o2_z))
      r_m4(28) = rk_m4(28)*s(indx(ich3so3_z))
      r_m4(29) = rk_m4(29)*s(indx(ich3so3_z))*s(indx(ino2_z))
      r_m4(30) = rk_m4(30)*s(indx(ich3so3_z))*s(indx(ino_z))
      r_m4(31) = rk_m4(31)*s(indx(ich3so3_z))*s(indx(iho2_z))
      r_m4(32) = rk_m4(32)*s(indx(ich3so3_z))*s(indx(ihcho_z))

      return
      end subroutine gasrate_m4     
 
 
 
!**************************************************************************
! <24.> subr loadperoxyparameters
!
! purpose: loads thermal rate coefficients for peroxy-peroxy
!          permutation reactions
!
! author : Rahul A. Zaveri
! date   : June 1998
!
! nomenclature:
! Aperox  = Pre-exponential factor (molec-cc-s)
! Bperox  = activation energy (-E/R)  (K)
!
!-------------------------------------------------------------------------
      subroutine loadperoxyparameters( Aperox, Bperox )

      use module_data_cbmz
      implicit none

!   subr arguments
      real Aperox(nperox,nperox), Bperox(nperox,nperox)

!   local variables
      integer i, j

      Aperox(jch3o2,jch3o2)   = 2.5e-13
      Aperox(jethp,jethp)     = 6.8e-14
      Aperox(jc2o3,jc2o3)     = 2.9e-12
      Aperox(jano2,jano2)     = 8.0e-12
      Aperox(jnap,jnap)       = 1.0e-12
      Aperox(jro2,jro2)       = 5.3e-16
      Aperox(jisopp,jisopp)   = 3.1e-14
      Aperox(jisopn,jisopn)   = 3.1e-14
      Aperox(jisopo2,jisopo2) = 3.1e-14
      Aperox(jxo2,jxo2)       = 3.1e-14

      Bperox(jch3o2,jch3o2)   = 190.
      Bperox(jethp,jethp)     = 0.0
      Bperox(jc2o3,jc2o3)     = 500.
      Bperox(jano2,jano2)     = 0.0
      Bperox(jnap,jnap)       = 0.0
      Bperox(jro2,jro2)       = 1980.
      Bperox(jisopp,jisopp)   = 1000.
      Bperox(jisopn,jisopn)   = 1000.
      Bperox(jisopo2,jisopo2) = 1000.
      Bperox(jxo2,jxo2)       = 1000.

      do i = 1, nperox
      do j = 1, nperox
      if(i.ne.j)then
      Aperox(i,j) = 2.0*sqrt(Aperox(i,i)*Aperox(j,j))
      Bperox(i,j) = 0.5*(Bperox(i,i) + Bperox(j,j))
      endif
      enddo
      enddo

! except for
      Aperox(jc2o3,jch3o2) = 1.3e-12
      Aperox(jch3o2,jc2o3) = 1.3e-12
      Bperox(jc2o3,jch3o2) = 640.
      Bperox(jch3o2,jc2o3) = 640.
 
      return
      end subroutine loadperoxyparameters




!**************************************************************************
! <25.> subr peroxyrateconstants
!
! purpose: computes parameterized thermal rate coefficients
!          for the alkylperoxy radical permutation reactions
!          for the entire mechanism.
!
! author : Rahul A. Zaveri
! date   : June 1998
!
! nomenclature:
! rk_param  = parameterized reaction rate constants (1/s)
! rk_perox  = individual permutation reaction rate constants (molec-cc-s)
! te        = ambient atmospheric temperature (K)
!
!-------------------------------------------------------------------------
      subroutine peroxyrateconstants( tempbox, cbox,   &
		Aperox, Bperox, rk_param )

      use module_data_cbmz
      implicit none

!   subr arguments 
      real tempbox, cbox(ngas_z)
      real Aperox(nperox,nperox), Bperox(nperox,nperox), rk_param(nperox)

!   local variables
      integer i, j
      real te
      real sperox(nperox), rk_perox(nperox,nperox)


      te = tempbox

      sperox(jch3o2)  = cbox(ich3o2_z)
      sperox(jethp)   = cbox(iethp_z)
      sperox(jro2)    = cbox(iro2_z)
      sperox(jc2o3)   = cbox(ic2o3_z)
      sperox(jano2)   = cbox(iano2_z)
      sperox(jnap)    = cbox(inap_z)
      sperox(jisopp)  = cbox(iisopp_z)
      sperox(jisopn)  = cbox(iisopn_z)
      sperox(jisopo2) = cbox(iisopo2_z)
      sperox(jxo2)    = cbox(ixo2_z)

!
! initialize to zero
      do i = 1, nperox
      rk_param(i) = 0.0
      enddo

      do i = 1, nperox
      do j = 1, nperox
      rk_perox(i,j) = arr( Aperox(i,j), Bperox(i,j), te )
      rk_param(i) = rk_param(i) + rk_perox(i,j)*sperox(j)
      enddo
      enddo
 
      return
      end subroutine peroxyrateconstants                 
 
 
 
!***********************************************************************
! <26.> subr gasthermrk_m1
!
! purpose: computes thermal reaction rate coefficients for
!          mechanism 1
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------

      subroutine gasthermrk_m1( tempbox, cair_mlc,   &
		 rk_photo, rk_param, rk_m1, rk_m2 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      real tempbox, cair_mlc
      real rk_photo(nphoto), rk_param(nperox)
      real rk_m1(nrxn_m1), rk_m2(nrxn_m2)
!   local variables
      integer i
      real rk0, rk2, rk3, rki, rko, rmm, rnn, te
!     real arr, troe


      te = tempbox

      rk_m1(1)  = rk_photo(jphoto_no2)
      rk_m1(2)  = rk_photo(jphoto_no3)
      rk_m1(3)  = rk_photo(jphoto_hono)
      rk_m1(4)  = rk_photo(jphoto_hno3)
      rk_m1(5)  = rk_photo(jphoto_hno4)
      rk_m1(6)  = rk_photo(jphoto_n2o5)
      rk_m1(7)  = rk_photo(jphoto_o3a)
      rk_m1(8)  = rk_photo(jphoto_o3b)
      rk_m1(9)  = rk_photo(jphoto_h2o2)
      rk_m1(10) = arr(3.2e-11, 70., te)
      rk_m1(11) = arr(1.8e-11, 110., te)
      rk_m1(12) = 2.2e-10
      rk_m1(13) = cair_mlc*6.e-34*(te/300.)**(-2.3)
      rk_m1(14) = arr(8.0e-12, -2060., te)
      rk_m1(15) = arr(6.5e-12, -120., te)
!
      rk0 = 9.0e-32
      rnn = 2.0
      rki = 2.2e-11
      rmm = 0.0
      rk_m1(16) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk0 = 9.0e-32
      rnn = 1.5
      rki = 3.0e-11
      rmm = 0.0
      rk_m1(17) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m1(18) = arr(2.0e-12, -1400., te)
      rk_m1(19) = arr(1.2e-13, -2450., te)
      rk_m1(20) = arr(1.6e-12, -940., te)
      rk_m1(21) = arr(1.1e-14, -500., te)
      rk_m1(22) = arr(5.5e-12, -2000., te)
!
      rk0 = 7.0e-31
      rnn = 2.6
      rki = 3.6e-11
      rmm = 0.1
      rk_m1(23) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk0 = 2.5e-30
      rnn = 4.4
      rki = 1.6e-11
      rmm = 1.7
      rk_m1(24) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m1(25) = 2.2e-11
      rk_m1(26) = arr(1.8e-11, -390., te)
            rko = 7.2e-15 * exp(785./te)
            rk2 = 4.1e-16 * exp(1440./te)
            rk3 = 1.9e-33 * exp(725./te)*cair_mlc
      rk_m1(27) = rko + rk3/(1.+rk3/rk2)
      rk_m1(28) = arr(1.3e-12, 380., te)
      rk_m1(29) = arr(4.8e-11, 250., te)
      rk_m1(30) = arr(2.9e-12, -160., te)
      rk_m1(31) = 2.3e-13 * exp(600./te) + 	 & 
                  1.7e-33 * exp(1000./te)*cair_mlc  ! ho2 + ho2 --> h2o2
      rk_m1(32) = rk_m1(31)*1.4e-21*exp(2200./te)   ! ho2 + ho2 + h2o --> h2o2
      rk_m1(33) = arr(3.5e-12, 250., te)
!
      rk0 = 1.8e-31
      rnn = 3.2
      rki = 4.7e-12
      rmm = 1.4
      rk_m1(34) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m1(35) = 5.0e-16
      rk_m1(36) = rk_m1(34)*arr(4.8e26, -10900., te)
      rk_m1(37) = arr(1.5e-11, 170., te)
      rk_m1(38) = arr(4.5e-14, -1260., te)
!
      rk0 = 2.2e-30
      rnn = 3.9
      rki = 1.5e-12
      rmm = 0.7
      rk_m1(39) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m1(40) = arr(8.5e-13, -2450., te)
      rk_m1(41) = 3.5e-12
      rk_m1(42) = 2.0e-21
      rk_m1(43) = rk_m1(39)*arr(3.7e26, -11000., te)
      rk_m1(44) = 1.5e-13 * (1.+8.18e-23*te*cair_mlc) ! co + oh --> ho2

      rk0 = 3.0e-31
      rnn = 3.3
      rki = 1.5e-12
      rmm = 0.0
      rk_m1(45) = Troe(cair_mlc,te,rk0,rnn,rki,rmm)

      rk_m1(46) = te**.667*arr(2.8e-14, -1575., te)
      rk_m1(47) = te**2*arr(1.5e-17, -492., te)
      rk_m1(48) = arr(6.7e-12, -600., te)
      rk_m1(49) = rk_photo(jphoto_hchoa)	! hcho + hv --> 2ho2 + co
      rk_m1(50) = rk_photo(jphoto_hchob)	! hcho + hv --> co
      rk_m1(51) = 1.0e-11
      rk_m1(52) = arr(3.4e-13, -1900., te)
      rk_m1(53) = rk_photo(jphoto_ch3ooh)
      rk_m1(54) = rk_photo(jphoto_ethooh)
      rk_m1(55) = arr(3.8e-12, 200., te)
      rk_m1(56) = arr(3.8e-12, 200., te)
      rk_m1(57) = arr(3.0e-12, 280., te)
      rk_m1(58) = arr(2.6e-12, 365., te)
      rk_m1(59) = 1.1e-12
      rk_m1(60) = 2.5e-12
      rk_m1(61) = arr(3.8e-13, 800., te)
      rk_m1(62) = arr(7.5e-13, 700., te)
      rk_m1(63) = rk_param(jch3o2)
      rk_m1(64) = rk_param(jethp)
      rk_m1(65) = arr(7.0e-12, -235.,te)

      rk_m2(2)  = rk_photo(jphoto_ald2)
      rk_m2(3)  = arr(5.6e-12, 270., te)
      rk_m2(4)  = arr(1.4e-12, -1900., te)
!
      rk0 = 9.7e-29
      rnn = 5.6
      rki = 9.3e-12
      rmm = 1.5
      rk_m2(31) = troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m2(32) = rk_m2(31)*arr(1.1e28, -14000., te)
      rk_m2(34) = arr(5.3e-12, 360., te)
      rk_m2(39) = 4.0e-12
      rk_m2(44) = arr(4.5e-13, 1000., te)
      rk_m2(49) = rk_param(jc2o3)

! Heterogeneous reactions
!      rk_m1(65) = rk_het(1)	! O3 -->
!      rk_m1(66) = rk_het(2)	! HO2 --> 0.5H2O2
!      rk_m1(67) = rk_het(3)	! NO2 --> 0.5HONO + 0.5HNO3
!      rk_m1(68) = rk_het(4)	! N2O5 --> 2HNO3
!      rk_m1(69) = rk_het(5)	! HNO3 --> NO2
!      rk_m1(70) = rk_het(6)	! HNO3 --> NO
!      rk_m1(71) = rk_het(7)	! NO3 --> NO + O2

!!$Turn off this check since initializing to 0's; wig 16-Nov-2007
!!$! all rate constants but be >= 0
!!$      do i = 1, nrxn_m1
!!$          rk_m1(i) = max( rk_m1(i), 0.0 )
!!$      end do
!!$      do i = 1, nrxn_m2
!!$          rk_m2(i) = max( rk_m2(i), 0.0 )
!!$      end do

      return
      end subroutine gasthermrk_m1                             
 
 
 
!***********************************************************************
! <27.> subr gasthermrk_m2
!
! purpose: computes thermal reaction rate coefficients for
!          mechanism 2
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------

      subroutine gasthermrk_m2( tempbox, cair_mlc,   &
		 rk_photo, rk_param, rk_m2 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      real tempbox, cair_mlc
      real rk_photo(nphoto), rk_param(nperox), rk_m2(nrxn_m2)
!   local variables
      integer i
      real rk0, rki, rmm, rnn, te
!     real arr, troe


      te = tempbox

      rk_m2(1)  = 8.1e-13

      rk_m2(5)  = rk_photo(jphoto_aone)
      rk_m2(6)  = te**2*arr(5.3e-18, -230., te)
      rk_m2(7)  = rk_photo(jphoto_mgly)
      rk_m2(8)  = 1.7e-11
      rk_m2(9)  = arr(1.4e-12, -1900., te)
      rk_m2(10) = arr(1.2e-14, -2630., te)
!
      rk0 = 1.0e-28
      rnn = 0.8
      rki = 8.8e-12
      rmm = 0.0
      rk_m2(11) = troe(cair_mlc,te,rk0,rnn,rki,rmm)
!
      rk_m2(12) = arr(4.2e-15, -1800., te)
      rk_m2(13) = arr(8.9e-16, -392., te)
      rk_m2(14) = arr(5.8e-12, 478., te)
      rk_m2(15) = arr(2.9e-11, 255., te)
      rk_m2(16) = arr(3.1e-13, -1010., te)
      rk_m2(17) = 2.5e-12
      rk_m2(18) = arr(2.1e-12, 322., te)
      rk_m2(19) = arr(1.7e-11, 116., te)
      rk_m2(20) = 8.1e-12
      rk_m2(21) = 4.1e-11
      rk_m2(22) = 2.2e-11
      rk_m2(23) = 1.4e-11
      rk_m2(24) = 3.0e-11
      rk_m2(25) = rk_photo(jphoto_open)
      rk_m2(26) = arr(5.4e-17, -500., te)
      rk_m2(27) = rk_photo(jphoto_rooh)
      rk_m2(28) = arr(3.8e-12, 200., te)
      rk_m2(29) = arr(1.6e-11, -540., te)
      rk_m2(30) = rk_photo(jphoto_onit)

      rk_m2(33) = 4.0e-12

      rk_m2(35) = 4.0e-12
      rk_m2(36) = 4.0e-12
      rk_m2(37) = 4.0e-12
      rk_m2(38) = 2.5e-12

      rk_m2(40) = 1.2e-12
      rk_m2(41) = 4.0e-12
      rk_m2(42) = 2.5e-12
      rk_m2(43) = arr(1.7e-13, 1300., te)

      rk_m2(45) = arr(1.2e-13, 1300., te)
      rk_m2(46) = arr(1.7e-13, 1300., te)
      rk_m2(47) = arr(1.7e-13, 1300., te)
      rk_m2(48) = rk_param(jro2)

      rk_m2(50) = rk_param(jano2)
      rk_m2(51) = rk_param(jnap)
      rk_m2(52) = rk_param(jxo2)
      rk_m2(53) = 1.0e-11		! XPAR + PAR -->

!!$Turn off this check since initializing to 0's; wig 16-Nov-2007
!!$! all rate constants but be >= 0
!!$      do i = 1, nrxn_m2
!!$          rk_m2(i) = max( rk_m2(i), 0.0 )
!!$      end do

      return
      end subroutine gasthermrk_m2                             
 
 
 
!***********************************************************************
! <28.> subr gasthermrk_m3
!
! purpose: computes thermal reaction rate coefficients for
!          mechanism 3
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------

      subroutine gasthermrk_m3( tempbox, cair_mlc,   &
		 rk_photo, rk_param, rk_m3 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      real tempbox, cair_mlc
      real rk_photo(nphoto), rk_param(nperox), rk_m3(nrxn_m3)
!   local variables
      integer i
      real te
!     real arr


      te = tempbox
!
      rk_m3(1)  = arr(2.6e-11, 409., te)
      rk_m3(2)  = arr(1.2e-14, -2013., te)
      rk_m3(3)  = arr(3.0e-12, -446., te)
      rk_m3(4)  = rk_photo(jphoto_isoprd)
      rk_m3(5)  = 3.3e-11
      rk_m3(6)  = 7.0e-18
      rk_m3(7)  = 1.0e-15
      rk_m3(8)  = 4.0e-12
      rk_m3(9)  = 4.0e-12
      rk_m3(10) = 4.0e-12
      rk_m3(11) = arr(1.7e-13, 1300., te)
      rk_m3(12) = arr(1.7e-13, 1300., te)
      rk_m3(13) = arr(1.7e-13, 1300., te)
      rk_m3(14) = rk_param(jisopp)
      rk_m3(15) = rk_param(jisopn)
      rk_m3(16) = rk_param(jisopo2)
 
! all rate constants but be >= 0
      do i = 1, nrxn_m3
          rk_m3(i) = max( rk_m3(i), 0.0 )
      end do

      return
      end subroutine gasthermrk_m3                             
 
 
 
!***********************************************************************
! <29.> subr gasthermrk_m4
!
! purpose: computes thermal reaction rate coefficients for
!          mechanism 4
!
! author : Rahul A. Zaveri
! date   : December 1998
!
!-------------------------------------------------------------------------

      subroutine gasthermrk_m4( tempbox, cair_mlc,   &
		 rk_photo, rk_param, rk_m4 )

      use module_data_cbmz
      implicit none

!   subr arguments 
      real tempbox, cair_mlc
      real rk_photo(nphoto), rk_param(nperox), rk_m4(nrxn_m4)
!   local variables
      integer i
      real B_abs, B_add, rk_tot, rk_tot_den, rk_tot_num, te
!     real arr


      te = tempbox
!
      rk_m4(1) = arr(9.6e-12, -234., te)	! ch3sch3 + oh --> ch3sch2
      rk_m4(2) = arr(1.4e-13, 500., te)
      rk_m4(3) = arr(1.3e-11, 409., te)

! Hynes et al. (1986)
      rk_tot_num =       te * exp(-234./te) +   &
                   8.46e-10 * exp(7230./te) +   &
                   2.68e-10 * exp(7810./te)
      rk_tot_den = 1.04e+11 * te + 88.1 * exp(7460./te)
      rk_tot	 = rk_tot_num/rk_tot_den
      B_abs      = rk_m4(1)/rk_tot
      B_add	 = 1. - B_abs

      rk_m4(4)  = B_add*rk_tot			! ch3sch3 + oh --> ch3s(oh)ch3
      rk_m4(5)  = 8.0e-12
      rk_m4(6)  = 1.8e-13
      rk_m4(7)  = 2.5e-13
      rk_m4(8)  = 8.6e-14
      rk_m4(9)  = 5.8e-11
      rk_m4(10) = 1.0e-14
      rk_m4(11) = 5.0e-12
      rk_m4(12) = 1.8e-13
      rk_m4(13) = 1.0e-15
      rk_m4(14) = 1.0e-13
      rk_m4(15) = 1.0e-15
      rk_m4(16) = 1.6e-11
      rk_m4(17) = 1.0e-13
!Bug fixing, the coefficient changed from 2.5e-13 to 2.5e13 by Qing.Yang@pnl.gov
      rk_m4(18) = arr(2.5e13, -8686., te)
      rk_m4(19) = 1.0e-14
      rk_m4(20) = 5.0e-15
      rk_m4(21) = 2.5e-13
      rk_m4(22) = 2.5e-13
      rk_m4(23) = 5.0e-11
      rk_m4(24) = 2.6e-18
      rk_m4(25) = 3.3
      rk_m4(26) = 1.0e-11
      rk_m4(27) = 5.5e-12
      rk_m4(28) = arr(2.0e17, -12626., te)
      rk_m4(29) = 3.0e-15
      rk_m4(30) = 3.0e-15
      rk_m4(31) = 5.0e-11
      rk_m4(32) = 1.6e-15
 
! all rate constants but be >= 0
      do i = 1, nrxn_m4
          rk_m4(i) = max( rk_m4(i), 0.0 )
      end do

      return
      end subroutine gasthermrk_m4                             
 
 
 
 
!***********************************************************************
! <26.> subr hetrateconstants
!
! purpose: computes heterogeneous reaction rate coefficients
!
! author : Rahul A. Zaveri
! date   : May 2000
!
!-------------------------------------------------------------------------

      subroutine hetrateconstants
      implicit none

      return
      end subroutine hetrateconstants
 
 
 
!***********************************************************************
! <31.> func troe
!
! purpose: calculates Troe reaction rate coefficient
!
! author : Rahul A. Zaveri
! date   : December 1998
!-----------------------------------------------------------------------

      real function troe( cairmlc, te, rk0, rnn, rki, rmm )
      implicit none
!   func parameters
      real cairmlc, te, rk0, rnn, rki, rmm
!   local variables
      real expo

      rk0 = rk0*cairmlc*(te/300.)**(-rnn)
      rki = rki*(te/300.)**(-rmm)
      expo= 1./(1. + (ALOG10(rk0/rki))**2)
      troe  = (rk0*rki/(rk0+rki))*.6**expo
      return
      end function troe                                   
 
 
 
!***********************************************************************
! <32.> func arr
!
! purpose: calculates arrhenius rate coefficient
!
! author : Rahul A. Zaveri
! date   : December 1998
!-----------------------------------------------------------------------

      real function arr( aa, bb, te )
      implicit none
!   func parameters
      real aa, bb, te

      arr = aa*exp(bb/te)
      return
      end function arr              



!***********************************************************************
! <xx.> subr mapgas_tofrom_host
!
! purpose: maps gas species between cboxold/new and host arrays
!
! author : R. C. Easter
! date   : November, 2003
!
! ----------------------------------------------------------------------

	subroutine mapgas_tofrom_host( imap,          &
		i_boxtest_units_convert,              &
		it,jt,kt, ims,ime, jms,jme, kms,kme,  &
		num_moist, num_chem, moist, chem,     &
		t_phy, p_phy, rho_phy,                &
		cbox, tempbox, pressbox, airdenbox,   &
		cair_mlc,                             &
		h2o, ch4, oxygen, nitrogen, hydrogen  )

        use module_configure, only:                             &
		p_qv,                                           &
		p_so2, p_sulf, p_no2, p_no, p_o3,               &
		p_hno3, p_h2o2, p_ald, p_hcho, p_op1,           &
		p_op2, p_paa, p_ora1, p_ora2, p_nh3,            &
		p_n2o5, p_no3, p_pan, p_hc3, p_hc5,             &
		p_hc8, p_eth, p_co, p_ol2, p_olt,               &
		p_oli, p_tol, p_xyl, p_aco3, p_tpan,            &
		p_hono, p_hno4, p_ket, p_gly, p_mgly,           &
		p_dcb, p_onit, p_csl, p_iso, p_ho,              &
		p_ho2,                                          &
		p_hcl, p_ch3o2, p_ethp, p_ch3oh, p_c2h5oh,      &
		p_par, p_to2, p_cro, p_open, p_op3,             &
		p_c2o3, p_ro2, p_ano2, p_nap, p_xo2,            &
		p_xpar, p_isoprd, p_isopp, p_isopn, p_isopo2,   &
		p_dms, p_msa, p_dmso, p_dmso2, p_ch3so2h,       &
		p_ch3sch2oo, p_ch3so2, p_ch3so3, p_ch3so2oo, p_ch3so2ch2oo, &
		p_mtf
	use module_data_cbmz
	implicit none

!   subr arguments 
	INTEGER, INTENT(IN) :: imap, it,jt,kt, ims,ime, jms,jme, kms,kme, &
		num_moist, num_chem, i_boxtest_units_convert
	REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), &
	    INTENT(IN) :: moist
	REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), &
	    INTENT(INOUT) :: chem
	REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) , &
	    INTENT(IN) :: t_phy, &	! temperature
		          p_phy, &	! air pressure (Pa)
		          rho_phy	! air density (kg/m3)
	REAL, INTENT(INOUT) :: cbox(ngas_z)
	REAL, INTENT(INOUT) :: tempbox, pressbox, airdenbox
	REAL, INTENT(INOUT) :: cair_mlc
	REAL, INTENT(INOUT) :: h2o, ch4, oxygen, nitrogen, hydrogen

!   local variables
	integer l
	real factoraa
	real, parameter :: eps=0.622


	tempbox = t_phy(it,kt,jt)
!   p_phy = (Pa);  pressbox = (dynes/cm2)
	pressbox = p_phy(it,kt,jt)*10.0
!   rho_phy = (kg_air/m3);  airdenbox = (mole_air/cm3)
	airdenbox = rho_phy(it,kt,jt)/28.966e3
	if (i_boxtest_units_convert .eq. 10) then
	    airdenbox = rho_phy(it,kt,jt)
	end if

	if (imap .gt. 0) goto 2000

!
!   imap==0 -- initial species mapping from host array to cboxold
!              chem --> czz --> cbox
!
!   note:  do not map nh3, hcl
!
	cbox(:) = 0.0

!   cair_mlc = (molecules_air/cm3)
	cair_mlc = airdenbox*avognumkpp

!  moist = (kg_h2o/kg_air);  czz = (mole_h2o/cm3);  h2o = (molecules_h2o/cm3)
!	czz(ih2o_z) = (moist(it,kt,jt,p_qv)/eps)*airdenbox
!	if (i_boxtest_units_convert .eq. 10) then
!	    czz(ih2o_z) = moist(it,kt,jt,p_qv)*airdenbox
!	end if
!	h2o      = czz(ih2o_z)*avognumkpp
	h2o = (moist(it,kt,jt,p_qv)/eps)*airdenbox
	if (i_boxtest_units_convert .eq. 10) then
	    h2o = moist(it,kt,jt,p_qv)*airdenbox
	end if
	h2o = h2o*avognumkpp

!	czz(ich4_z) = 1.7e-6*airdenbox           ! ch4 conc. in mol/cc
!	ch4      = czz(ich4_z)*avognumkpp        ! ch4 conc. in molec/cc
	ch4      = 1.7e-6*airdenbox*avognumkpp   ! ch4 conc. in molec/cc

	oxygen   = 0.21*cair_mlc                 ! o2 conc. in molec/cc
	nitrogen = 0.79*cair_mlc                 ! n2 conc. in molec/cc
	hydrogen = 0.58e-6*cair_mlc              ! h2 conc. in molec/cc

!   chem units = (ppm);  czz units = (mole/cm3);  cbox units = (molecules/cm3)
	factoraa = airdenbox*1.0e-6
	if (i_boxtest_units_convert .eq. 10) factoraa = airdenbox
	factoraa = factoraa*avognumkpp

	cbox(iso2_z)	     = chem(it,kt,jt,p_so2)*factoraa
	cbox(ih2so4_z)	     = chem(it,kt,jt,p_sulf)*factoraa
	cbox(ino2_z)	     = chem(it,kt,jt,p_no2)*factoraa
	cbox(ino_z)	     = chem(it,kt,jt,p_no)*factoraa
	cbox(io3_z)	     = chem(it,kt,jt,p_o3)*factoraa
	cbox(ihno3_z)	     = chem(it,kt,jt,p_hno3)*factoraa
	cbox(ih2o2_z)	     = chem(it,kt,jt,p_h2o2)*factoraa
	cbox(iald2_z)	     = chem(it,kt,jt,p_ald)*factoraa
	cbox(ihcho_z)	     = chem(it,kt,jt,p_hcho)*factoraa
	cbox(ich3ooh_z)	     = chem(it,kt,jt,p_op1)*factoraa
	cbox(iethooh_z)	     = chem(it,kt,jt,p_op2)*factoraa
	cbox(ihcooh_z)	     = chem(it,kt,jt,p_ora1)*factoraa
	cbox(ircooh_z)	     = chem(it,kt,jt,p_ora2)*factoraa
	cbox(inh3_z)	     = chem(it,kt,jt,p_nh3)*factoraa
	cbox(in2o5_z)	     = chem(it,kt,jt,p_n2o5)*factoraa
	cbox(ino3_z)	     = chem(it,kt,jt,p_no3)*factoraa
	cbox(ipan_z)	     = chem(it,kt,jt,p_pan)*factoraa
	cbox(ic2h6_z)	     = chem(it,kt,jt,p_eth)*factoraa
	cbox(ico_z)	     = chem(it,kt,jt,p_co)*factoraa
	cbox(ieth_z)	     = chem(it,kt,jt,p_ol2)*factoraa
	cbox(iolet_z)	     = chem(it,kt,jt,p_olt)*factoraa
	cbox(iolei_z)	     = chem(it,kt,jt,p_oli)*factoraa
	cbox(itol_z)	     = chem(it,kt,jt,p_tol)*factoraa
	cbox(ixyl_z)	     = chem(it,kt,jt,p_xyl)*factoraa
	cbox(ihono_z)	     = chem(it,kt,jt,p_hono)*factoraa
	cbox(ihno4_z)	     = chem(it,kt,jt,p_hno4)*factoraa
	cbox(iaone_z)	     = chem(it,kt,jt,p_ket)*factoraa
	cbox(imgly_z)	     = chem(it,kt,jt,p_mgly)*factoraa
	cbox(ionit_z)	     = chem(it,kt,jt,p_onit)*factoraa
	cbox(icres_z)	     = chem(it,kt,jt,p_csl)*factoraa
	cbox(iisop_z)	     = chem(it,kt,jt,p_iso)*factoraa
	cbox(ioh_z)	     = chem(it,kt,jt,p_ho)*factoraa
	cbox(iho2_z)	     = chem(it,kt,jt,p_ho2)*factoraa

	cbox(ihcl_z)	     = chem(it,kt,jt,p_hcl)*factoraa
	cbox(ich3o2_z)	     = chem(it,kt,jt,p_ch3o2)*factoraa
	cbox(iethp_z)	     = chem(it,kt,jt,p_ethp)*factoraa
	cbox(ich3oh_z)	     = chem(it,kt,jt,p_ch3oh)*factoraa
	cbox(ic2h5oh_z)	     = chem(it,kt,jt,p_c2h5oh)*factoraa
	cbox(ipar_z)	     = chem(it,kt,jt,p_par)*factoraa
	cbox(ito2_z)	     = chem(it,kt,jt,p_to2)*factoraa
	cbox(icro_z)	     = chem(it,kt,jt,p_cro)*factoraa
	cbox(iopen_z)	     = chem(it,kt,jt,p_open)*factoraa
	cbox(irooh_z)	     = chem(it,kt,jt,p_op3)*factoraa
	cbox(ic2o3_z)	     = chem(it,kt,jt,p_c2o3)*factoraa
	cbox(iro2_z)	     = chem(it,kt,jt,p_ro2)*factoraa
	cbox(iano2_z)	     = chem(it,kt,jt,p_ano2)*factoraa
	cbox(inap_z)	     = chem(it,kt,jt,p_nap)*factoraa
	cbox(ixo2_z)	     = chem(it,kt,jt,p_xo2)*factoraa
	cbox(ixpar_z)	     = chem(it,kt,jt,p_xpar)*factoraa
	cbox(iisoprd_z)	     = chem(it,kt,jt,p_isoprd)*factoraa
	cbox(iisopp_z)	     = chem(it,kt,jt,p_isopp)*factoraa
	cbox(iisopn_z)	     = chem(it,kt,jt,p_isopn)*factoraa
	cbox(iisopo2_z)	     = chem(it,kt,jt,p_isopo2)*factoraa
	cbox(idms_z)	     = chem(it,kt,jt,p_dms)*factoraa
	cbox(imsa_z)	     = chem(it,kt,jt,p_msa)*factoraa
	cbox(idmso_z)	     = chem(it,kt,jt,p_dmso)*factoraa
	cbox(idmso2_z)	     = chem(it,kt,jt,p_dmso2)*factoraa
	cbox(ich3so2h_z)     = chem(it,kt,jt,p_ch3so2h)*factoraa
	cbox(ich3sch2oo_z)   = chem(it,kt,jt,p_ch3sch2oo)*factoraa
	cbox(ich3so2_z)	     = chem(it,kt,jt,p_ch3so2)*factoraa
	cbox(ich3so3_z)	     = chem(it,kt,jt,p_ch3so3)*factoraa
	cbox(ich3so2oo_z)    = chem(it,kt,jt,p_ch3so2oo)*factoraa
	cbox(ich3so2ch2oo_z) = chem(it,kt,jt,p_ch3so2ch2oo)*factoraa
	cbox(imtf_z)	     = chem(it,kt,jt,p_mtf)*factoraa

	cbox(ih2o_z)	     = h2o
	cbox(ich4_z)	     = ch4
	cbox(io2_z)	     = oxygen
	cbox(in2_z)	     = nitrogen
	cbox(ih2_z)	     = hydrogen

	return

!
!   imap==1 -- final species mapping from cbox back to host array
!              cbox --> czz --> chem
!
!   note1:  do not map nh3, hcl, ch4
!
2000	continue
!   chem = (ppm);  czz = (mole/cm3);  cbox = (molecules/cm3)
	factoraa = airdenbox*1.0e-6
	if (i_boxtest_units_convert .eq. 10) factoraa = airdenbox
	factoraa = factoraa*avognumkpp

	chem(it,kt,jt,p_so2)	     = cbox(iso2_z)/factoraa
	chem(it,kt,jt,p_sulf)	     = cbox(ih2so4_z)/factoraa
	chem(it,kt,jt,p_no2)	     = cbox(ino2_z)/factoraa
	chem(it,kt,jt,p_no)	     = cbox(ino_z)/factoraa
	chem(it,kt,jt,p_o3)	     = cbox(io3_z)/factoraa
	chem(it,kt,jt,p_hno3)	     = cbox(ihno3_z)/factoraa
	chem(it,kt,jt,p_h2o2)	     = cbox(ih2o2_z)/factoraa
	chem(it,kt,jt,p_ald)	     = cbox(iald2_z)/factoraa
	chem(it,kt,jt,p_hcho)	     = cbox(ihcho_z)/factoraa
	chem(it,kt,jt,p_op1)	     = cbox(ich3ooh_z)/factoraa
	chem(it,kt,jt,p_op2)	     = cbox(iethooh_z)/factoraa
	chem(it,kt,jt,p_ora1)	     = cbox(ihcooh_z)/factoraa
	chem(it,kt,jt,p_ora2)	     = cbox(ircooh_z)/factoraa
	chem(it,kt,jt,p_nh3)	     = cbox(inh3_z)/factoraa
	chem(it,kt,jt,p_n2o5)	     = cbox(in2o5_z)/factoraa
	chem(it,kt,jt,p_no3)	     = cbox(ino3_z)/factoraa
	chem(it,kt,jt,p_pan)	     = cbox(ipan_z)/factoraa
	chem(it,kt,jt,p_eth)	     = cbox(ic2h6_z)/factoraa
	chem(it,kt,jt,p_co)	     = cbox(ico_z)/factoraa
	chem(it,kt,jt,p_ol2)	     = cbox(ieth_z)/factoraa
	chem(it,kt,jt,p_olt)	     = cbox(iolet_z)/factoraa
	chem(it,kt,jt,p_oli)	     = cbox(iolei_z)/factoraa
	chem(it,kt,jt,p_tol)	     = cbox(itol_z)/factoraa
	chem(it,kt,jt,p_xyl)	     = cbox(ixyl_z)/factoraa
	chem(it,kt,jt,p_hono)	     = cbox(ihono_z)/factoraa
	chem(it,kt,jt,p_hno4)	     = cbox(ihno4_z)/factoraa
	chem(it,kt,jt,p_ket)	     = cbox(iaone_z)/factoraa
	chem(it,kt,jt,p_mgly)	     = cbox(imgly_z)/factoraa
	chem(it,kt,jt,p_onit)	     = cbox(ionit_z)/factoraa
	chem(it,kt,jt,p_csl)	     = cbox(icres_z)/factoraa
	chem(it,kt,jt,p_iso)	     = cbox(iisop_z)/factoraa
	chem(it,kt,jt,p_ho)	     = cbox(ioh_z)/factoraa
	chem(it,kt,jt,p_ho2)	     = cbox(iho2_z)/factoraa

	chem(it,kt,jt,p_hcl)	     = cbox(ihcl_z)/factoraa
	chem(it,kt,jt,p_ch3o2)	     = cbox(ich3o2_z)/factoraa
	chem(it,kt,jt,p_ethp)	     = cbox(iethp_z)/factoraa
	chem(it,kt,jt,p_ch3oh)	     = cbox(ich3oh_z)/factoraa
	chem(it,kt,jt,p_c2h5oh)	     = cbox(ic2h5oh_z)/factoraa
	chem(it,kt,jt,p_par)	     = cbox(ipar_z)/factoraa
	chem(it,kt,jt,p_to2)	     = cbox(ito2_z)/factoraa
	chem(it,kt,jt,p_cro)	     = cbox(icro_z)/factoraa
	chem(it,kt,jt,p_open)	     = cbox(iopen_z)/factoraa
	chem(it,kt,jt,p_op3)	     = cbox(irooh_z)/factoraa
	chem(it,kt,jt,p_c2o3)	     = cbox(ic2o3_z)/factoraa
	chem(it,kt,jt,p_ro2)	     = cbox(iro2_z)/factoraa
	chem(it,kt,jt,p_ano2)	     = cbox(iano2_z)/factoraa
	chem(it,kt,jt,p_nap)	     = cbox(inap_z)/factoraa
	chem(it,kt,jt,p_xo2)	     = cbox(ixo2_z)/factoraa
	chem(it,kt,jt,p_xpar)	     = cbox(ixpar_z)/factoraa
	chem(it,kt,jt,p_isoprd)	     = cbox(iisoprd_z)/factoraa
	chem(it,kt,jt,p_isopp)	     = cbox(iisopp_z)/factoraa
	chem(it,kt,jt,p_isopn)	     = cbox(iisopn_z)/factoraa
	chem(it,kt,jt,p_isopo2)	     = cbox(iisopo2_z)/factoraa
	chem(it,kt,jt,p_dms)	     = cbox(idms_z)/factoraa
	chem(it,kt,jt,p_msa)	     = cbox(imsa_z)/factoraa
	chem(it,kt,jt,p_dmso)	     = cbox(idmso_z)/factoraa
	chem(it,kt,jt,p_dmso2)	     = cbox(idmso2_z)/factoraa
	chem(it,kt,jt,p_ch3so2h)     = cbox(ich3so2h_z)/factoraa
	chem(it,kt,jt,p_ch3sch2oo)   = cbox(ich3sch2oo_z)/factoraa
	chem(it,kt,jt,p_ch3so2)	     = cbox(ich3so2_z)/factoraa
	chem(it,kt,jt,p_ch3so3)	     = cbox(ich3so3_z)/factoraa
	chem(it,kt,jt,p_ch3so2oo)    = cbox(ich3so2oo_z)/factoraa
	chem(it,kt,jt,p_ch3so2ch2oo) = cbox(ich3so2ch2oo_z)/factoraa
	chem(it,kt,jt,p_mtf)	     = cbox(imtf_z)/factoraa

	return
	end subroutine mapgas_tofrom_host 



!***********************************************************************
! <xx.> subr set_gaschem_allowed_regimes
!
! purpose: determines which gas-phase chemistry regimes are allowed based
!          on which species are active in the simulation
!
! author :
! date   :
!
! ----------------------------------------------------------------------

	subroutine set_gaschem_allowed_regimes( lunerr,   &
		igaschem_allowed_m1, igaschem_allowed_m2,   &
		igaschem_allowed_m3, igaschem_allowed_m4 )
!
!   determines which gas-phase chemistry regimes are allowed based
!   on which species are active in the simulation
!
        use module_configure, only:                             &
		p_qv,                                           &
		p_so2, p_sulf, p_no2, p_no, p_o3,               &
		p_hno3, p_h2o2, p_ald, p_hcho, p_op1,           &
		p_op2, p_paa, p_ora1, p_ora2, p_nh3,            &
		p_n2o5, p_no3, p_pan, p_hc3, p_hc5,             &
		p_hc8, p_eth, p_co, p_ol2, p_olt,               &
		p_oli, p_tol, p_xyl, p_aco3, p_tpan,            &
		p_hono, p_hno4, p_ket, p_gly, p_mgly,           &
		p_dcb, p_onit, p_csl, p_iso, p_ho,              &
		p_ho2,                                          &
		p_hcl, p_ch3o2, p_ethp, p_ch3oh, p_c2h5oh,      &
		p_par, p_to2, p_cro, p_open, p_op3,             &
		p_c2o3, p_ro2, p_ano2, p_nap, p_xo2,            &
		p_xpar, p_isoprd, p_isopp, p_isopn, p_isopo2,   &
		p_dms, p_msa, p_dmso, p_dmso2, p_ch3so2h,       &
		p_ch3sch2oo, p_ch3so2, p_ch3so3, p_ch3so2oo, p_ch3so2ch2oo, &
		p_mtf
        use module_state_description, only:  param_first_scalar
	use module_data_cbmz
	implicit none

!   subr arguments
	integer lunerr
	integer igaschem_allowed_m1, igaschem_allowed_m2,   &
		igaschem_allowed_m3, igaschem_allowed_m4

!   local variables
	integer nactive, ndum, p1st
	character*80 msg


!   index for first "active" scalar (= 2)
	p1st = param_first_scalar

!   determine if regime 1 is allowed
!   (note:  p_xxx>1 if xxx is active, p_xxx=1 if inactive)
	if (p_qv .lt. p1st) then
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    msg = '*** water vapor IS NOT ACTIVE'
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if

!   determine if regime 1 is allowed
!   (note:  p_xxx>1 if xxx is active, p_xxx=1 if inactive)
	nactive = 0
	ndum = 27
	if (p_no          .ge. p1st) nactive = nactive + 1
	if (p_no2         .ge. p1st) nactive = nactive + 1
	if (p_no3         .ge. p1st) nactive = nactive + 1
	if (p_n2o5        .ge. p1st) nactive = nactive + 1
	if (p_hono        .ge. p1st) nactive = nactive + 1
	if (p_hno3        .ge. p1st) nactive = nactive + 1
	if (p_hno4        .ge. p1st) nactive = nactive + 1
	if (p_o3          .ge. p1st) nactive = nactive + 1
!	if (p_o1d         .ge. p1st) nactive = nactive + 1
!	if (p_o3p         .ge. p1st) nactive = nactive + 1
	if (p_ho          .ge. p1st) nactive = nactive + 1
	if (p_ho2         .ge. p1st) nactive = nactive + 1
	if (p_h2o2        .ge. p1st) nactive = nactive + 1
	if (p_co          .ge. p1st) nactive = nactive + 1
	if (p_so2         .ge. p1st) nactive = nactive + 1
	if (p_sulf        .ge. p1st) nactive = nactive + 1
!	if (p_nh3         .ge. p1st) nactive = nactive + 1
!	if (p_hcl         .ge. p1st) nactive = nactive + 1
	if (p_eth         .ge. p1st) nactive = nactive + 1
	if (p_ch3o2       .ge. p1st) nactive = nactive + 1
	if (p_ethp        .ge. p1st) nactive = nactive + 1
	if (p_hcho        .ge. p1st) nactive = nactive + 1
	if (p_ch3oh       .ge. p1st) nactive = nactive + 1
	if (p_c2h5oh      .ge. p1st) nactive = nactive + 1
	if (p_op1         .ge. p1st) nactive = nactive + 1
	if (p_op2         .ge. p1st) nactive = nactive + 1
	if (p_ald         .ge. p1st) nactive = nactive + 1
	if (p_ora1        .ge. p1st) nactive = nactive + 1
	if (p_pan         .ge. p1st) nactive = nactive + 1
	if (p_ora2        .ge. p1st) nactive = nactive + 1
	if (p_c2o3        .ge. p1st) nactive = nactive + 1

	if (nactive .le. 0) then
	    igaschem_allowed_m1 = 0
	else if (nactive .eq. ndum) then
	    igaschem_allowed_m1 = 1
	else
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    write(msg,90200) 1, nactive, ndum
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if
90200	format( '    error for regime ', i1, ', nactive, nexpected = ', 2i5 )

!   determine if regime 2 is allowed
	nactive = 0
	ndum = 19
	if (p_par         .ge. p1st) nactive = nactive + 1
	if (p_ket         .ge. p1st) nactive = nactive + 1
	if (p_mgly        .ge. p1st) nactive = nactive + 1
	if (p_ol2         .ge. p1st) nactive = nactive + 1
	if (p_olt         .ge. p1st) nactive = nactive + 1
	if (p_oli         .ge. p1st) nactive = nactive + 1
	if (p_tol         .ge. p1st) nactive = nactive + 1
	if (p_xyl         .ge. p1st) nactive = nactive + 1
	if (p_csl         .ge. p1st) nactive = nactive + 1
	if (p_to2         .ge. p1st) nactive = nactive + 1
	if (p_cro         .ge. p1st) nactive = nactive + 1
	if (p_open        .ge. p1st) nactive = nactive + 1
	if (p_onit        .ge. p1st) nactive = nactive + 1
	if (p_op3         .ge. p1st) nactive = nactive + 1
	if (p_ro2         .ge. p1st) nactive = nactive + 1
	if (p_ano2        .ge. p1st) nactive = nactive + 1
	if (p_nap         .ge. p1st) nactive = nactive + 1
	if (p_xo2         .ge. p1st) nactive = nactive + 1
	if (p_xpar        .ge. p1st) nactive = nactive + 1
	if (nactive .le. 0) then
	    igaschem_allowed_m2 = 0
	else if (nactive .eq. ndum) then
	    igaschem_allowed_m2 = 2
	else
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    write(msg,90200) 2, nactive, ndum
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if

!   determine if regime 3 is allowed
	nactive = 0
	ndum = 5
	if (p_iso         .ge. p1st) nactive = nactive + 1
	if (p_isoprd      .ge. p1st) nactive = nactive + 1
	if (p_isopp       .ge. p1st) nactive = nactive + 1
	if (p_isopn       .ge. p1st) nactive = nactive + 1
	if (p_isopo2      .ge. p1st) nactive = nactive + 1
	if (nactive .le. 0) then
	    igaschem_allowed_m3 = 0
	else if (nactive .eq. ndum) then
	    igaschem_allowed_m3 = 3
	else
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    write(msg,90200) 3, nactive, ndum
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if

!   determine if regime 4 is allowed
	nactive = 0
	ndum = 11
	if (p_dms         .ge. p1st) nactive = nactive + 1
	if (p_msa         .ge. p1st) nactive = nactive + 1
	if (p_dmso        .ge. p1st) nactive = nactive + 1
	if (p_dmso2       .ge. p1st) nactive = nactive + 1
	if (p_ch3so2h     .ge. p1st) nactive = nactive + 1
	if (p_ch3sch2oo   .ge. p1st) nactive = nactive + 1
	if (p_ch3so2      .ge. p1st) nactive = nactive + 1
	if (p_ch3so3      .ge. p1st) nactive = nactive + 1
	if (p_ch3so2oo    .ge. p1st) nactive = nactive + 1
	if (p_ch3so2ch2oo .ge. p1st) nactive = nactive + 1
	if (p_mtf         .ge. p1st) nactive = nactive + 1
	if (nactive .le. 0) then
	    igaschem_allowed_m4 = 0
	else if (nactive .eq. ndum) then
	    igaschem_allowed_m4 = 4
	else
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    write(msg,90200) 4, nactive, ndum
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if

!   regime 1 must always be allowed
	if (igaschem_allowed_m1 .le. 0) then
	    msg = '*** subr set_gaschem_allowed_regimes'
	    call peg_message( lunerr, msg )
	    write(msg,90300) 1
	    call peg_message( lunerr, msg )
	    call peg_error_fatal( lunerr, msg )
	end if
90300	format( '    regime ', i1, ' must always be allowed' )

!   if regime 2 is allowed, then regime 1 must be allowed
	if (igaschem_allowed_m2 .gt. 0) then
	    if (igaschem_allowed_m1 .le. 0) then
		msg = '*** subr set_gaschem_allowed_regimes'
		call peg_message( lunerr, msg )
		write(msg,90400) 2, 1
		call peg_message( lunerr, msg )
		call peg_error_fatal( lunerr, msg )
	    end if
	end if
90400	format( '    regime ', i1, ' allowed BUT regime ', i1, ' unallowed' )

!   if regime 3 is allowed, then regimes 1&2 must be allowed
	if (igaschem_allowed_m3 .gt. 0) then
	    if (igaschem_allowed_m1 .le. 0) then
		msg = '*** subr set_gaschem_allowed_regimes'
		call peg_message( lunerr, msg )
		write(msg,90400) 3, 1
		call peg_message( lunerr, msg )
		call peg_error_fatal( lunerr, msg )
	    else if (igaschem_allowed_m2 .le. 0) then
		msg = '*** subr set_gaschem_allowed_regimes'
		call peg_message( lunerr, msg )
		write(msg,90400) 3, 2
		call peg_message( lunerr, msg )
		call peg_error_fatal( lunerr, msg )
	    end if
	end if

!   if regime 4 is allowed, then regime 1 must be allowed
	if (igaschem_allowed_m4 .gt. 0) then
	    if (igaschem_allowed_m1 .le. 0) then
		msg = '*** subr set_gaschem_allowed_regimes'
		call peg_message( lunerr, msg )
		write(msg,90400) 4, 1
		call peg_message( lunerr, msg )
		call peg_error_fatal( lunerr, msg )
	    end if
	end if

	return
	end subroutine set_gaschem_allowed_regimes



!***********************************************************************
! <xx.> subr gasphotoconstants
!
! purpose: copy photolytic rate constants from host arrays to local array
!
!-----------------------------------------------------------------------
	subroutine gasphotoconstants( rk_photo,   &
	    i_boxtest_units_convert,               &
	    it,jt,kt, ims,ime, jms,jme, kms,kme,   &
	    ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, ph_hno2, &
	    ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, ph_ch2om, &
	    ph_ch3o2h, ph_n2o5 )
!
!   copies photolytic rate constants from host arrays to local arrays
!   note1:   currently 8 rate constants are scaled to other rate constants
!            as is done in zz06gasphotolysis.f
!   note2:   currently the n2o5 rate is set to zero
!
	use module_data_cbmz
	implicit none

!   subr arguments 
	integer it,jt,kt, ims,ime, jms,jme, kms,kme
	integer i_boxtest_units_convert
	real rk_photo(nphoto)
	real, dimension( ims:ime, kms:kme, jms:jme ) :: &
               ph_o31d, ph_o33p, ph_no2, ph_no3o2, ph_no3o, ph_hno2, &
               ph_hno3, ph_hno4, ph_h2o2, ph_ch2or, ph_ch2om, &
               ph_ch3o2h, ph_n2o5

!   local variables
	real ft


	rk_photo(:) = 0.0

!   these from wrf/madronnich rate constants
	rk_photo(jphoto_no2)    = ph_no2(it,kt,jt)       
	rk_photo(jphoto_no3)    = ph_no3o(it,kt,jt)   &
                                + ph_no3o2(it,kt,jt)     
	rk_photo(jphoto_o3a)    = ph_o33p(it,kt,jt)      
	rk_photo(jphoto_o3b)    = ph_o31d(it,kt,jt)      
	rk_photo(jphoto_hono)   = ph_hno2(it,kt,jt)      
	rk_photo(jphoto_hno3)   = ph_hno3(it,kt,jt)      
	rk_photo(jphoto_hno4)   = ph_hno4(it,kt,jt)      
	rk_photo(jphoto_h2o2)   = ph_h2o2(it,kt,jt)      
	rk_photo(jphoto_ch3ooh) = ph_ch3o2h(it,kt,jt)    
	rk_photo(jphoto_hchoa)  = ph_ch2or(it,kt,jt)     
	rk_photo(jphoto_hchob)  = ph_ch2om(it,kt,jt)     
	rk_photo(jphoto_n2o5)   = ph_n2o5(it,kt,jt)

!   these scaled to other rate constants
	rk_photo(jphoto_ethooh) = 0.7   *rk_photo(jphoto_h2o2)
	rk_photo(jphoto_ald2)   = 4.6e-4*rk_photo(jphoto_no2)
	rk_photo(jphoto_aone)   = 7.8e-5*rk_photo(jphoto_no2)
	rk_photo(jphoto_mgly)   = 9.64  *rk_photo(jphoto_hchoa)
	rk_photo(jphoto_open)   = 9.04  *rk_photo(jphoto_hchoa)
	rk_photo(jphoto_rooh)   = 0.7   *rk_photo(jphoto_h2o2)
	rk_photo(jphoto_onit)   = 1.0e-4*rk_photo(jphoto_no2)
	rk_photo(jphoto_isoprd) = .025  *rk_photo(jphoto_hchob)

!   convert from (1/min) to (1/s)
!   (except when i_boxtest_units_convert = 10 or 20)
	ft = 60.0
	if (i_boxtest_units_convert .eq. 10) ft = 1.0
	if (i_boxtest_units_convert .eq. 20) ft = 1.0
	if (ft .ne. 1.0) then
	    rk_photo(:) = rk_photo(:)/ft
	end if


	return
	end subroutine gasphotoconstants  



!-----------------------------------------------------------------------
	end module module_cbmz
