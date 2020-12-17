!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
!CPP directives to control ic/bc conditions...
!(The directive in module_input_chem_data also needs to be set.)
!  CASENAME = 0   Uses Texas August 2004 case values and profiles
!             1   Uses same concentrations as TX, but uses different
!                 profiles depending on the species. (NEAQS2004 case)
!             2   Mexico City
!             3   Mexico City for Testbed, to be consistent with MADE/SORGAM too
!             4   bc,ic values from wrfinput and wrfbdy files
#define CASENAME 4

	module module_mosaic_initmixrats

! rce 2005-feb-18 - several fixes for indices of dlo/hi_sect, volumlo/hi_sect,
!     which are now (isize,itype)

! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect

	USE module_state_description

	integer, parameter :: mosaic_init_wrf_mixrats_flagaa = 1
                            ! turns subr mosaic_init_wrf_mixrats on/off

	contains


!-----------------------------------------------------------------------
	subroutine mosaic_init_wrf_mixrats(  &
		iflagaa, config_flags,       &
		chem, alt, z_at_w, g,        &
		ids,ide, jds,jde, kds,kde,   &
		ims,ime, jms,jme, kms,kme,   &
		its,ite, jts,jte, kts,kte    )

!
!   initializes the species and number mixing ratios for each section
!
!   this top level routine simply calls other routines depending on value
!	of config_flags%aer_ic_opt
!
	use module_configure, only:  grid_config_rec_type
	use module_state_description, only:  num_chem, param_first_scalar,   &
			aer_ic_pnnl
	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_peg_util, only:  peg_message, peg_error_fatal

	implicit none


!   subr arguments
	type(grid_config_rec_type), intent(in) :: config_flags

	integer, intent(in) ::   &
		iflagaa,   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem

	real, intent(in),      &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
        alt, z_at_w
    real, intent(in) :: g

!   local variables
        integer :: i, ic, j, k

#if (CASENAME == 0) || (CASENAME == 1) || (CASENAME == 2) || (CASENAME == 3)
	if (config_flags%aer_ic_opt == aer_ic_pnnl) then
	    call mosaic_init_wrf_mixrats_opt2(   &
		iflagaa, config_flags,           &
		chem, z_at_w, g,                 &
		ids,ide, jds,jde, kds,kde,       &
		ims,ime, jms,jme, kms,kme,       &
		its,ite, jts,jte, kts,kte        )

	else
	    call mosaic_init_wrf_mixrats_opt1(   &
		iflagaa, config_flags,   &
		chem,   &
		ids,ide, jds,jde, kds,kde,   &
		ims,ime, jms,jme, kms,kme,   &
		its,ite, jts,jte, kts,kte    )

	end if

! Aerosol species are returned from above in concentration units. Convert
! them to mixing ratio for use in advection, etc.
    do ic = p_so4_a01,num_chem
       do j = jts,jte
          do k = kts,kte-1
             do i = its,ite
                chem(i,k,j,ic) = chem(i,k,j,ic)*alt(i,k,j)
             end do
          end do
       end do
    end do

! Fill the top z-staggered location to prevent trouble during advection.
    do ic = p_so4_a01,num_chem
       do j = jts,jte
          do i = its,ite
             chem(i,kte,j,ic) = chem(i,kte-1,j,ic)
          end do
       end do
    end do
#endif

	return
	end subroutine mosaic_init_wrf_mixrats


!-----------------------------------------------------------------------
	subroutine mosaic_init_wrf_mixrats_opt1(   &
		iflagaa, config_flags,   &
		chem,   &
		ids,ide, jds,jde, kds,kde,   &
		ims,ime, jms,jme, kms,kme,   &
		its,ite, jts,jte, kts,kte    )

!
!   initializes the species and number mixing ratios for each section
!   based on user-specified lognormal modes that span the size distribution
!
! rce 11-sep-2004 - eliminated use of the _wrfch pointers (lptr_xxx_a_wrfch, 
!     lwaterptr_wrfch, numptr_wrfch); use only the _amode pointers now; 
!     added l1dum
!
	use module_configure, only:  grid_config_rec_type
	use module_state_description, only:  num_chem, param_first_scalar
	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_peg_util, only:  peg_message, peg_error_fatal

	implicit none

!   subr arguments
	type(grid_config_rec_type), intent(in) :: config_flags

	integer, intent(in) ::   &
		iflagaa,   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(inout),   &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem

!   local variables
	integer i, j, k, l, ll, l1, l3, l1dum, m, mdum, nsm
	integer it, jt, kt

	real dum, dumdp, dumrsfc, dumvol,   &
      	  xlo, xhi,   &
	  dumvol1p,  &
      	  pdummb, zdumkm, zscalekm, zfactor

	real vtot_nsm_ofmode(maxd_asize)
	real dumarr(maxd_acomp+5)

	real erfc

!	double precision fracnum, fracvol, tlo, thi
	real fracvol, tlo, thi

	integer nmaxd_nsm
	parameter (nmaxd_nsm = 4)

	integer iphase, itype, ntot_nsm
	integer iiprof_nsm(nmaxd_nsm)
	integer lldum_so4, lldum_nh4, lldum_oc, lldum_bc,   &
		lldum_oin, lldum_na, lldum_cl, lldum_hysw

	real sx_nsm(nmaxd_nsm), sxr2_nsm(nmaxd_nsm),   &
      	  x0_nsm(nmaxd_nsm), x3_nsm(nmaxd_nsm),   &
      	  rtot_nsm(maxd_acomp,nmaxd_nsm),   &
      	  vtot_nsm(nmaxd_nsm), xntot_nsm(nmaxd_nsm)

	real dgnum_nsm(nmaxd_nsm), sigmag_nsm(nmaxd_nsm)
	real aaprof_nsm(maxd_acomp+1,nmaxd_nsm)
	real bbprof_nsm(nmaxd_nsm)

	character*80 msg
	character*10 dumname


!   check for on/off
	if (mosaic_init_wrf_mixrats_flagaa .le. 0) return


!   *** currently only works for ntype_aer = 1
	itype = 1
	iphase = ai_phase
	m = 1

!   set values for initialization modes
	iiprof_nsm(:) = 1
	aaprof_nsm(:,:) = 0.0
	bbprof_nsm(:) = 0.0

	ntot_nsm = 4
	ntot_nsm = min( ntot_nsm, nsize_aer(itype) )

	lldum_so4 = 0
	lldum_nh4 = 0
	lldum_oc  = 0
	lldum_bc  = 0
	lldum_oin = 0
	lldum_na  = 0
	lldum_cl  = 0
	lldum_hysw = 0

	do ll = 1, ncomp_plustracer_aer(itype)
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_so4_aer(m,itype,iphase)) lldum_so4 = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_nh4_aer(m,itype,iphase)) lldum_nh4 = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_oc_aer(m,itype,iphase))  lldum_oc  = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_bc_aer(m,itype,iphase))  lldum_bc  = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_oin_aer(m,itype,iphase)) lldum_oin = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_na_aer(m,itype,iphase))  lldum_na  = ll
	    if (massptr_aer(ll,m,itype,iphase) .eq.   &
		lptr_cl_aer(m,itype,iphase))  lldum_cl  = ll
	end do
	if (hyswptr_aer(m,itype) .gt. 0)   &
		lldum_hysw = ncomp_plustracer_aer(itype) + 1

	msg = ' '
	if (lldum_so4 .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_so4 = 0'
	if (lldum_nh4 .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_nh4 = 0'
	if (lldum_oc .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_oc = 0'
	if (lldum_bc .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_bc = 0'
	if (lldum_oin .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_oin = 0'
	if (lldum_na .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_na = 0'
	if (lldum_cl .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_cl = 0'
	if (lldum_hysw .le. 0)   &
		msg = '*** subr mosaic_init_wrf_mixrats - lldum_hysw = 0'
	if (msg .ne. ' ') call peg_error_fatal( lunerr, msg )


	do nsm = 1, ntot_nsm

	if (nsm .eq. 1) then
!   accum mode with so4, nh4, oc, bc
	    dgnum_nsm( nsm) = 0.15e-4
	    sigmag_nsm(nsm) = 2.0
	    aaprof_nsm(lldum_so4,nsm) = 0.50
	    aaprof_nsm(lldum_nh4,nsm) = aaprof_nsm(lldum_so4,nsm)   &
					* (mw_nh4_aer/mw_so4_aer)
	    aaprof_nsm(lldum_oc,nsm) = 0.25
	    aaprof_nsm(lldum_bc,nsm) = 0.05
	    aaprof_nsm(lldum_hysw,nsm) = aaprof_nsm(lldum_so4,nsm) * 0.2

	else if (nsm .eq. 2) then
!   aitken mode with so4, nh4, oc, bc
	    dgnum_nsm( nsm) = 0.03e-4
	    sigmag_nsm(nsm) = 2.0
	    aaprof_nsm(lldum_so4,nsm) = 0.50 * 0.020
	    aaprof_nsm(lldum_nh4,nsm) = aaprof_nsm(lldum_so4,nsm)   &
					* (mw_nh4_aer/mw_so4_aer)
	    aaprof_nsm(lldum_oc,nsm) = 0.25 * 0.020
	    aaprof_nsm(lldum_bc,nsm) = 0.05 * 0.020
	    aaprof_nsm(lldum_hysw,nsm) = aaprof_nsm(lldum_so4,nsm) * 0.2

	else if (nsm .eq. 3) then
!   coarse dust mode with oin
	    dgnum_nsm( nsm) = 1.0e-4
	    sigmag_nsm(nsm) = 2.0
	    aaprof_nsm(lldum_oin,nsm) = 0.5
	    aaprof_nsm(lldum_hysw,nsm) = aaprof_nsm( 9,nsm) * 1.0e-3

	else if (nsm .eq. 4) then
!   coarse seasalt mode with na, cl
	    dgnum_nsm( nsm) = 2.0e-4
	    sigmag_nsm(nsm) = 2.0
	    aaprof_nsm(lldum_cl,nsm) = 0.1
	    aaprof_nsm(lldum_na,nsm) = aaprof_nsm(lldum_cl,nsm)   &
					* (mw_na_aer/mw_cl_aer)
	    aaprof_nsm(lldum_hysw,nsm) = aaprof_nsm(lldum_cl,nsm) * 0.2

	end if

	end do

!   when iflagaa = 1/2/3/4, use only the nsm-mode = iflagaa
	if (iflagaa .gt. 0) then
	    do nsm = 1, ntot_nsm
		if (nsm .ne. iflagaa) aaprof_nsm(:,nsm) = 0.0
	    end do
	end if



!
!   do the initialization now
!

!   calculate mode parameters
	do nsm = 1, ntot_nsm
	    sx_nsm(nsm) = alog( sigmag_nsm(nsm) )
	    sxr2_nsm(nsm) = sx_nsm(nsm) * sqrt(2.0)
	    x0_nsm(nsm) = alog( dgnum_nsm(nsm) )
	    x3_nsm(nsm) = x0_nsm(nsm) + 3.0*sx_nsm(nsm)*sx_nsm(nsm)
	end do

!   initialize rclm array to zero
	rclm(:,:) = 0.
!	rclmsvaa(:,:) = 0.

!
!   loop over all vertical levels
!
!	do 12900 k = 1, ktot
	do 12900 k = 1, 1

!	pdummb = 1013.*scord(k)
!	zdumkm = ptoz( pdummb ) * 1.e-3
	zdumkm = 0.0


!   for each species and nsm mode, define total mixing ratio
!	(for all sizes) at level k
!
!   iiprof_nsm =  +1 gives constant mixing ratio
!	aaprof_nsm(l,nsm) = constant mixing ratio (ppb)
!   iiprof_nsm =  +2 gives exponential profile
!	aaprof_nsm(l,nsm) = surface mixing ratio (ppb)
!	bbprof_nsm(l) = scale height (km)
!   iiprof_nsm =  +3 gives linear profile (then zero at z > zscalekm)
!	aaprof_nsm(l,nsm) = surface mixing ratio (ppb)
!	bbprof_nsm(l) = height (km) at which mixing ratio = 0

	do nsm = 1, ntot_nsm

	if (iiprof_nsm(nsm) .eq. 2) then
	    zscalekm = bbprof_nsm(nsm)
	    zfactor = exp( -zdumkm/zscalekm )
	else if (iiprof_nsm(nsm) .eq. 3) then
	    zscalekm = bbprof_nsm(nsm)
	    zfactor = max( 0., (1. - zdumkm/zscalekm) )
	else
	    zfactor = 1.0
	end if

	    do ll = 1, ncomp_plustracer_aer(itype) + 1
		rtot_nsm(ll,nsm) = aaprof_nsm(ll,nsm) * zfactor
	    end do

	end do

!   compute total volume and number mixing ratios for each nsm mode
!   rtot_nsm is ug/m3,  1.0e-6*rtot is g/m3,  vtot_nsm is cm3/m3
	do nsm = 1, ntot_nsm
	    dumvol = 0.
	    do ll = 1, ncomp_aer(itype)
		dum = 1.0e-6*rtot_nsm(ll,nsm)/dens_aer(ll,itype)
		dumvol = dumvol + max( 0., dum )
	    end do
	    vtot_nsm(nsm) = dumvol
	end do

!   now compute species and number mixing ratios for each bin
	do 12700 m = 1, nsize_aer(itype)

	vtot_nsm_ofmode(m) = 0.0

	do 12500 nsm = 1, ntot_nsm

!   for nsm_mode = n, compute fraction of number and volume
!   that is in bin m
	xlo = alog( dlo_sect(m,itype) )
	xhi = alog( dhi_sect(m,itype) )

	tlo = (xlo - x3_nsm(nsm))/sxr2_nsm(nsm)
	thi = (xhi - x3_nsm(nsm))/sxr2_nsm(nsm)
	if (tlo .ge. 0.) then
!	    fracvol = 0.5*( erfc(tlo) - erfc(thi) )
	    fracvol = 0.5*( erfc_num_recipes(tlo) - erfc_num_recipes(thi) )
	else
!	    fracvol = 0.5*( erfc(-thi) - erfc(-tlo) )
	    fracvol = 0.5*( erfc_num_recipes(-thi) - erfc_num_recipes(-tlo) )
	end if
	fracvol = max( fracvol, 0.0 )

	vtot_nsm_ofmode(m) = vtot_nsm_ofmode(m)  + vtot_nsm(nsm)*fracvol

!   now load that fraction of species-mixing-ratio
!   into the appropriate rclm location
	do ll = 1, ncomp_plustracer_aer(itype)
	    rclm( k, massptr_aer(ll,m,itype,iphase) ) =   &
      	    rclm( k, massptr_aer(ll,m,itype,iphase) ) +   &
      		fracvol*rtot_nsm(ll,nsm)
	end do

	if ((iphase .eq. ai_phase) .and.   &
	    (lldum_hysw .gt. 0) .and.   &
	    (hyswptr_aer(m,itype) .gt. 0)) then

	    rclm( k, hyswptr_aer(m,itype) ) =   &
      	    rclm( k, hyswptr_aer(m,itype) ) +   &
      		fracvol*rtot_nsm(lldum_hysw,nsm)
	end if

12500	continue

!   now compute number from volume
	dum = sqrt( dlo_sect(m,itype)*dhi_sect(m,itype) )
	dumvol1p = (pi/6.0)*(dum**3)
	rclm( k, numptr_aer(m,itype,iphase) ) = vtot_nsm_ofmode(m)/dumvol1p

!   set water = hyswatr
	if ((iphase .eq. ai_phase) .and.   &
	    (lldum_hysw .gt. 0) .and.   &
	    (hyswptr_aer(m,itype) .gt. 0) .and.   &
	    (waterptr_aer(m,itype) .gt. 0)) then

      	    rclm( k, waterptr_aer(m,itype) ) =    &
	    rclm( k, hyswptr_aer(m,itype) )
	end if

12700	continue

12900	continue


!
!   do diagnostic output
!

! temporary
! temporary

	dumarr(:) = 0.0
	msg = ' '
	call peg_message( lunout, msg )
	msg = '*** subr mosaic_init_wrf_mixrats_opt1 results'
	call peg_message( lunout, msg )
	msg = '    mass in ug/m3     number in #/m3     volume in cm3/m3'
	call peg_message( lunout, msg )
	msg = ' '
	call peg_message( lunout, msg )
	msg = ' mode  l  l1  species      conc'
	call peg_message( lunout, msg )

        do 14390 mdum = 1, nsize_aer(itype)+1
	m = min( mdum, nsize_aer(itype) )
	msg = ' '
	call peg_message( lunout, msg )
        do 14350 l = 1, ncomp_plustracer_aer(itype)+4

            if (l .le. ncomp_plustracer_aer(itype)) then
                l1 = massptr_aer(l,m,itype,iphase)
		dumname = name_aer(l,itype)
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+1) then
                l1 = hyswptr_aer(m,itype)
		dumname = 'hystwatr'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+2) then
                l1 = waterptr_aer(m,itype)
		dumname = 'water'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+3) then
                l1 = numptr_aer(m,itype,iphase)
		dumname = 'number'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+4) then
                l1 = 0
		dumname = 'volume'
		dum = vtot_nsm_ofmode(m)
	    else
		dumname = '=BADBAD='
		l1 = -1
		dum = -1.0
            end if

	    l1dum = l1
	    if (aboxtest_testmode .gt. 0) l1dum = max( l1-1, 0 )

	    if (mdum .le. nsize_aer(itype)) then
		dumarr(l) = dumarr(l) + dum
		write(msg,9620) m, l, l1dum, dumname, dum
	    else
		write(msg,9625) l, dumname, dumarr(l)
	    end if
	    call peg_message( lunout, msg )

14350   continue
14390   continue

9620    format( 3i4, 2x, a, 3(1pe12.3) )
9625    format( ' sum', i4, '   -', 2x, a, 3(1pe12.3) )


!
!   load the chem array
!
        do 16390 m = 1, nsize_aer(itype)
        do 16350 l = 1, 15

	    if (l .eq. 1) then
		l1 = lptr_so4_aer(m,itype,iphase)
	    else if (l .eq. 2) then
		l1 = lptr_no3_aer(m,itype,iphase)
	    else if (l .eq. 3) then
		l1 = lptr_cl_aer(m,itype,iphase)
	    else if (l .eq. 4) then
		l1 = lptr_msa_aer(m,itype,iphase)
	    else if (l .eq. 5) then
		l1 = lptr_co3_aer(m,itype,iphase)
	    else if (l .eq. 6) then
		l1 = lptr_nh4_aer(m,itype,iphase)
	    else if (l .eq. 7) then
		l1 = lptr_na_aer(m,itype,iphase)
	    else if (l .eq. 8) then
		l1 = lptr_ca_aer(m,itype,iphase)
	    else if (l .eq. 9) then
		l1 = lptr_oin_aer(m,itype,iphase)
	    else if (l .eq. 10) then
		l1 = lptr_oc_aer(m,itype,iphase)
	    else if (l .eq. 11) then
		l1 = lptr_bc_aer(m,itype,iphase)
	    else if (l .eq. 12) then
		l1 = hyswptr_aer(m,itype)
	    else if (l .eq. 13) then
		l1 = waterptr_aer(m,itype)
	    else if (l .eq. 14) then
		l1 = numptr_aer(m,itype,iphase)
	    else
		goto 16350
	    end if
	    l3 = l1

	    if ((l1 .gt. 0) .and. (l1 .le. lmaxd) .and.   &
	        (l3 .ge. param_first_scalar)) then
		do it = its, ite
!   *** note:  not sure what the kt limits should be here 
		do kt = kts, kte-1
		do jt = jts, jte
		    chem(it,kt,jt,l3) = rclm(1,l1)
		end do
		end do
		end do
	    end if

16350   continue
16390   continue


!   all done
	return
	end subroutine mosaic_init_wrf_mixrats_opt1


!-----------------------------------------------------------------------
!wig 10-May-2004, added phb, ph, g
	subroutine mosaic_init_wrf_mixrats_opt2(   &
		iflagaa, config_flags,             &
		chem, z_at_w, g,                   &
		ids,ide, jds,jde, kds,kde,         &
		ims,ime, jms,jme, kms,kme,         &
		its,ite, jts,jte, kts,kte          )

!
!   provides initial values for mosaic aerosol species (mass and number
!	mixing ratio) for "Texas August 2000" simulations
!   modified to use different profiles for different aerosols for NEAQS case, egc 7/2005
!   currently all the initial values are uniform in x, y, AND z
!
! rce 11-sep-2004 - eliminated use of the _wrfch pointers (lptr_xxx_a_wrfch, 
!     lwaterptr_wrfch, numptr_wrfch); use only the _amode pointers now
!
	use module_configure, only:  grid_config_rec_type
	use module_state_description, only:  num_chem, param_first_scalar
	use module_data_mosaic_asect
	use module_data_mosaic_other
	use module_peg_util, only:  peg_message, peg_error_fatal

	implicit none

!   subr arguments
	type(grid_config_rec_type), intent(in) :: config_flags

	integer, intent(in) ::   &
		iflagaa,   &
		ids, ide, jds, jde, kds, kde,   &
		ims, ime, jms, jme, kms, kme,   &
		its, ite, jts, jte, kts, kte

	real, intent(inout),     &
		dimension( ims:ime, kms:kme, jms:jme, 1:num_chem ) :: &
		chem

        real, intent(in),        &
		dimension( ims:ime, kms:kme, jms:jme ) :: &
                z_at_w
        real :: g

!   local variables
	integer l, l1, l3, m, mdum
	integer iphase, itype
	integer it, jt, kt

!wig 10-May-2004, added mult
	real dum, dumvol1p, mult
	real qcoar, qfine, qval

	real :: vtot_ofmode(maxd_asize)
	real :: dumarr(maxd_acomp+5)
	real :: fr_coar(8), fr_fine(8)

!wig 01-Apr-2005, Updated fractional breakdown between bins. (See also
!                 bdy_chem_value_mosaic in this file and mosaic_addemiss in 
!                 module_mosaic_addemiss.F) Note that the values here no
!                 longer match those in mosaic_addemiss.
!rce 10-May-2005, changed fr8b_coar to values determined by jdf
	real, save :: fr8b_coar(8) =   &
        (/ 0.0,  0.0,  0.0,  0.019,  0.0212,  0.1101,  0.2751, 0.7018 /) ! jdf Testbed
!jdf    (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.300, 0.700 /) ! 10-May-2005
!       (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.933, 0.067 /) ! 01-Apr-2005
!       (/ 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.16,  0.84  /) ! "old"

	real, save :: fr8b_fine(8) =   &
        (/ 0.0245, 0.1472, 0.3501, 0.3317, 0.1251, 0.0186, 0.0011, 0.0/) !jdf Testbed
!jdf    (/ 0.006, 0.024, 0.231, 0.341, 0.140, 0.258, 0., 0./) !5-Apr-2005 values
!       (/ 0.0275, 0.0426, 0.2303, 0.3885, 0.1100, 0.2011, 0., 0./) !15-Nov-2004 values
!	(/ 0.01, 0.05, 0.145, 0.60, 0.145, 0.05, 0.00, 0.00 /)
!	(/ 0.04, 0.10, 0.35, 0.29, 0.15, 0.07, 0.00, 0.00 /)

	real :: qfine_so4, qfine_no3, qfine_cl, qfine_msa,   &
		qfine_co3, qfine_nh4, qfine_na, qfine_ca, qfine_oin,   &
		qfine_oc, qfine_bc, qfine_hysw, qfine_watr, qfine_vol
	real :: qcoar_so4, qcoar_no3, qcoar_cl, qcoar_msa,   &
		qcoar_co3, qcoar_nh4, qcoar_na, qcoar_ca, qcoar_oin,   &
		qcoar_oc, qcoar_bc, qcoar_hysw, qcoar_watr, qcoar_vol

!wig 10-May-2004, added z
        real, dimension( ims:ime, kms:kme, jms:jme ) :: z

	character*80 msg
	character*10 dumname


!   *** currently only works for ntype_aer = 1
	itype = 1
	iphase = ai_phase
	m = 1

!wig 10-May-2004, block begin...
! calculate the mid-level heights
    do jt = jts, min(jte,jde-1)
       do kt = kts, kte-1
          do it = its, min(ite,ide-1)
             z(it,kt,jt) = (z_at_w(it,kt,jt)+z_at_w(it,kt+1,jt))*0.5
          end do
       end do
    end do
!wig 10-May-2004, ...end block

!   set the partitioning fractions for either 8 or 4 bins
	if (nsize_aer(itype) == 8) then
	    fr_coar(:) = fr8b_coar(:)
	    fr_fine(:) = fr8b_fine(:)
	else if (nsize_aer(itype) == 4) then
	    do m = 1, nsize_aer(itype)
		fr_coar(m) = fr8b_coar(2*m) + fr8b_coar(2*m-1)
		fr_fine(m) = fr8b_fine(2*m) + fr8b_fine(2*m-1)
	    end do
	else
	    write(msg,'(a,i5)')   &
		'subr mosaic_init_wrf_mixrats_opt2' //   &
		' - nsize_aer(itype) must be 4 or 8 but = ', nsize_aer(itype)
	    call peg_error_fatal( lunout, msg )
	end if

!
!   compute initial values (currently uniform in x, y, AND z)
!   load them into the chem array
!   also load them into the rclm array for diagnostic output
!
	rclm(:,:) = 0.0
	vtot_ofmode(:) = 0.0

!   Set values for fine and coarse mass concentrations, and
!   compute fine/coarse volume concentrations. These are also set in
!   bdy_chem_value_mosaic, below.
!   (these are latest values from 1-Apr-2005 discussions)
!
!   rce 10-may-2005 - changed qfine_cl, _na, _oin to values determined by jdf
	qfine_so4 = 2.14
	qcoar_so4 = 0.242
	qfine_no3 = 0.11
	qcoar_no3 = 0.03
!	qfine_cl  = 0.3
	qfine_cl  = 0.14     ! 10-May-2005
	qcoar_cl  = 0.139
	qfine_msa = 0.0
	qcoar_msa = 0.0
	qfine_co3 = 0.0
	qcoar_co3 = 0.0
	qfine_nh4 = 0.83
	qcoar_nh4 = 0.10
!	qfine_na  = 0.2
	qfine_na  = 0.1      ! 10-May-2005
	qcoar_na  = 0.09
	qfine_ca  = 0.0
	qcoar_ca  = 0.0
!	qfine_oin = 6.2
	qfine_oin = 3.48     ! 10-May-2005
	qcoar_oin = 0.35
	qfine_oc  = 1.00
	qcoar_oc  = 1.50
	qfine_bc  = 0.2
	qcoar_bc  = 0.075
	qfine_hysw = 0.0
	qcoar_hysw = 0.0
	qfine_watr = 0.0
	qcoar_watr = 0.0
#if (CASENAME == 2)
!       qfine_so4 = 0.50
!       qcoar_so4 = 0.50/2.0
!       qfine_no3 = 0.75
!       qcoar_no3 = 0.75/2.0
!       qfine_cl  = 0.10
!       qcoar_cl  = 0.10/2.0
!       qfine_nh4 = 0.30
!       qcoar_nh4 = 0.30/2.0
!       qfine_na  = 0.20
!       qcoar_na  = 0.20/2.0
!       qfine_oin = 4.00
!       qcoar_oin = 4.00/2.0
!       qfine_oc  = 3.00
!       qcoar_oc  = 3.00/2.0
!       qfine_bc  = 0.50
!       qcoar_bc  = 0.50/2.0
        qfine_so4 = 0.300
        qcoar_so4 = 0.300/2.0
        qfine_no3 = 0.001
        qcoar_no3 = 0.001/2.0
        qfine_cl  = 1.050
        qcoar_cl  = 1.05/02.0
        qfine_nh4 = 0.094
        qcoar_nh4 = 0.094/2.0
        qfine_na  = 0.700
        qcoar_na  = 0.700/2.0
        qfine_oin = 4.500
        qcoar_oin = 4.500/2.0
        qfine_oc  = 0.088
        qcoar_oc  = 0.088/2.0
        qfine_bc  = 0.013
        qcoar_bc  = 0.013/2.0
#endif
#if (CASENAME == 3)
        qfine_so4 = 0.300
        qcoar_so4 = 0.0
        qfine_nh4 = 0.094
        qcoar_nh4 = 0.0
        qfine_no3 = 0.001
        qcoar_no3 = 0.0
        qfine_cl  = 0.0
        qcoar_cl  = 1.050
        qfine_na  = 0.000
        qcoar_na  = 0.700
        qfine_oin = 4.500
        qcoar_oin = 4.500/2.0
        qfine_oc  = 0.088
        qcoar_oc  = 0.0
        qfine_bc  = 0.013
        qcoar_bc  = 0.0
#endif

!!$! old values from 23-Apr-2004:
!!$	qfine_so4 = 2.554
!!$	qcoar_so4 = 0.242
!!$	qfine_no3 = 0.07
!!$	qcoar_no3 = 0.03
!!$	qfine_cl  = 0.324
!!$	qcoar_cl  = 0.139
!!$	qfine_msa = 0.0
!!$	qcoar_msa = 0.0
!!$	qfine_co3 = 0.0
!!$	qcoar_co3 = 0.0
!!$	qfine_nh4 = 0.98
!!$	qcoar_nh4 = 0.10
!!$	qfine_na  = 0.21
!!$	qcoar_na  = 0.09
!!$	qfine_ca  = 0.0
!!$	qcoar_ca  = 0.0
!!$	qfine_oin = 0.15
!!$	qcoar_oin = 0.35
!!$	qfine_oc  = 1.00
!!$	qcoar_oc  = 1.50
!!$	qfine_bc  = 0.175
!!$	qcoar_bc  = 0.075
!!$	qfine_hysw = 0.0
!!$	qcoar_hysw = 0.0
!!$	qfine_watr = 0.0
!!$	qcoar_watr = 0.0


! qfine_so4 ... are ug/m3 so 1.0e-6 factor gives g/m3
! dens_so4  ... are g/cm3;  qfine_vol ... are cm3/m3
	qfine_vol = 1.0e-6 * (   &
	    (qfine_so4/dens_so4_aer) + (qfine_no3/dens_no3_aer) +   &
	    (qfine_cl /dens_cl_aer ) + (qfine_msa/dens_msa_aer) +   &
	    (qfine_co3/dens_co3_aer) + (qfine_nh4/dens_nh4_aer) +   &
	    (qfine_na /dens_na_aer ) + (qfine_ca /dens_ca_aer ) +   &
	    (qfine_oin/dens_oin_aer) + (qfine_oc /dens_oc_aer ) +   &
	    (qfine_bc /dens_bc_aer ) )
	qcoar_vol =  1.0e-6 * (  &
	    (qcoar_so4/dens_so4_aer) + (qcoar_no3/dens_no3_aer) +   &
	    (qcoar_cl /dens_cl_aer ) + (qcoar_msa/dens_msa_aer) +   &
	    (qcoar_co3/dens_co3_aer) + (qcoar_nh4/dens_nh4_aer) +   &
	    (qcoar_na /dens_na_aer ) + (qcoar_ca /dens_ca_aer ) +   &
	    (qcoar_oin/dens_oin_aer) + (qcoar_oc /dens_oc_aer ) +   &
	    (qcoar_bc /dens_bc_aer ) )

        do 2900 m = 1, nsize_aer(itype)
        do 2800 l = 1, 15

	    if (l .eq. 1) then
		l1 = lptr_so4_aer(m,itype,iphase)
		qfine = qfine_so4
		qcoar = qcoar_so4
	    else if (l .eq. 2) then
		l1 = lptr_no3_aer(m,itype,iphase)
		qfine = qfine_no3
		qcoar = qcoar_no3
	    else if (l .eq. 3) then
		l1 = lptr_cl_aer(m,itype,iphase)
		qfine = qfine_cl
		qcoar = qcoar_cl
	    else if (l .eq. 4) then
		l1 = lptr_msa_aer(m,itype,iphase)
		qfine = qfine_msa
		qcoar = qcoar_msa
	    else if (l .eq. 5) then
		l1 = lptr_co3_aer(m,itype,iphase)
		qfine = qfine_co3
		qcoar = qcoar_co3
	    else if (l .eq. 6) then
		l1 = lptr_nh4_aer(m,itype,iphase)
		qfine = qfine_nh4
		qcoar = qcoar_nh4
	    else if (l .eq. 7) then
		l1 = lptr_na_aer(m,itype,iphase)
		qfine = qfine_na
		qcoar = qcoar_na
	    else if (l .eq. 8) then
		l1 = lptr_ca_aer(m,itype,iphase)
		qfine = qfine_ca
		qcoar = qcoar_ca
	    else if (l .eq. 9) then
		l1 = lptr_oin_aer(m,itype,iphase)
		qfine = qfine_oin
		qcoar = qcoar_oin
	    else if (l .eq. 10) then
		l1 = lptr_oc_aer(m,itype,iphase)
		qfine = qfine_oc
		qcoar = qcoar_oc
	    else if (l .eq. 11) then
		l1 = lptr_bc_aer(m,itype,iphase)
		qfine = qfine_bc
		qcoar = qcoar_bc
	    else if (l .eq. 12) then
		l1 = hyswptr_aer(m,itype)
		qfine = qfine_hysw
		qcoar = qcoar_hysw
	    else if (l .eq. 13) then
		l1 = waterptr_aer(m,itype)
		qfine = qfine_watr
		qcoar = qcoar_watr
	    else if (l .eq. 14) then
		l1 = numptr_aer(m,itype,iphase)
		dumvol1p = sqrt(volumlo_sect(m,itype)*volumhi_sect(m,itype))
		qfine = qfine_vol/dumvol1p
		qcoar = qcoar_vol/dumvol1p
		vtot_ofmode(m) =   &
			qfine_vol*fr_fine(m) + qcoar_vol*fr_coar(m)
	    else
		goto 2800
	    end if
	    l3 = l1

	    if ((l1 .gt. 0) .and. (l1 .le. lmaxd) .and.   &
	        (l3 .ge. param_first_scalar)) then
		qval = qfine*fr_fine(m) + qcoar*fr_coar(m)
		rclm(1,l1) = qval

		do jt = jts, min(jte,jde-1)
		do kt = kts, kte-1
		do it = its, min(ite,ide-1)

!wig 28-May-2004, begin block...
! Determine height multiplier...
! This should mimic the calculation in sorgam_set_aer_bc_pnnl,
! sorgam_init_aer_ic_pnnl, bdy_chem_value_mosaic
!!$!    Height(m)     Multiplier
!!$!    ---------     ----------
!!$!    <=2000        1.0
!!$!    2000<z<3000   linear transition zone to 0.5
!!$!    3000<z<5000   linear transition zone to 0.25
!!$!    >=5000        0.25
!!$!
!!$! which translates to:
!!$!    2000<z<3000   mult = 1.0 + (z-2000.)*(0.5-1.0)/(3000.-2000.)
!!$!    3000<z<5000   mult = 0.5 + (z-3000.)*(0.25-0.5)/(5000.-3000.)
!!$! or in reduced form:
!!$		   if( z(it,kt,jt) <= 2000. ) then
!!$		      mult = 1.0
!!$		   elseif( z(it,kt,jt) > 2000. &
!!$                        .and. z(it,kt,jt) <= 3000. ) then
!!$		      mult = 1.0 - 0.0005*(z(it,kt,jt)-2000.)
!!$		   elseif( z(it,kt,jt) > 3000. &
!!$                        .and. z(it,kt,jt) <= 5000. ) then
!!$		      mult = 0.5 - 1.25e-4*(z(it,kt,jt)-3000.)
!!$		   else
!!$		      mult = 0.25
!!$           end if
! Updated 1-Apr-2005:
#if (CASENAME == 1)
! further updated 7-20-05 egc:   species specific profiles based on MIRAG2 output
         mult = 1.0
         if ( (l3 == 1) .or. (l3 == 2) .or. (l3 == 6) ) then
! so4, no3 and nh4 aerosol profiles
	     if ( z(it,kt,jt) <= 1000. ) then
	              mult = 1.0
		  elseif( z(it,kt,jt) > 1000. &
                        .and. z(it,kt,jt) <= 4000. ) then
		      mult = 1.0 - 2.367e-4*(z(it,kt,jt)-1000.)
		   elseif( z(it,kt,jt) > 4000. &
                        .and. z(it,kt,jt) <= 5500. ) then
		      mult = 0.29 - 4.0e-5*(z(it,kt,jt)-4000.)
		   else
		      mult = 0.23
               end if
           else if ( ( l3 == 3) .or. (l3 ==7) ) then
! na and cl aerosol profiles
	     if ( z(it,kt,jt) <= 100. ) then
	               mult = 1.0
		  elseif( z(it,kt,jt) > 100. &
                        .and. z(it,kt,jt) <= 265. ) then
		      mult = 1.0 - 2.9e-3*(z(it,kt,jt)-100.)
		   elseif( z(it,kt,jt) > 265. &
                        .and. z(it,kt,jt) <= 2000. ) then
		      mult = 0.52 - 2.94e-4*(z(it,kt,jt)-265.)
                   elseif( z(it,kt,jt) > 2000. &
                        .and. z(it,kt,jt) <= 7000. ) then
		       mult = 0.01
		   else
		      mult = 1.e-10
               end if
          else if  ( l3 == 10)  then
! oc aerosol profile
	     if ( z(it,kt,jt) <= 600. ) then
	               mult = 1.0
		  elseif( z(it,kt,jt) > 600. &
                        .and. z(it,kt,jt) <= 3389. ) then
		      mult = 1.0 - 2.04e-4*(z(it,kt,jt)-600.)
		   elseif( z(it,kt,jt) > 3389. &
                        .and. z(it,kt,jt) <= 8840. ) then
		      mult = 0.43 - 7.045e-5*(z(it,kt,jt)-3389.)
		   else
		      mult = 0.046
               end if
          else if  ( l3 == 11)  then
! bc aerosol profile
	     if ( z(it,kt,jt) <= 100. ) then
	               mult = 1.0
		  elseif( z(it,kt,jt) > 100. &
                        .and. z(it,kt,jt) <= 3400. ) then
		      mult = 1.0 - 2.51e-4*(z(it,kt,jt)-100.)
		   elseif( z(it,kt,jt) > 3400. &
                        .and. z(it,kt,jt) <= 8400. ) then
		      mult = 0.172 -2.64e-5*(z(it,kt,jt)-3400.)
		   else
		      mult = 0.04
               end if
         else if  ( l3 == 9)  then
! oin aerosol profile
	     if ( z(it,kt,jt) <= 1580. ) then
	               mult = 1.0 + 1.77e-4 *z(it,kt,jt)
		  elseif( z(it,kt,jt) > 1580. &
                        .and. z(it,kt,jt) <= 5280. ) then
		      mult = 1.28 - 2.65e-4*(z(it,kt,jt)-1580.)
		   else
		      mult = 0.30
               end if
	else
! generic profile for other other species (which should have groundlevel values=0)
#endif
!    Height(m)     Multiplier
!    ---------     ----------
!    <=2000        1.0
!    2000<z<3000   linear transition zone to 0.25
!    3000<z<5000   linear transision zone to 0.125
!    >=5000        0.125
!
! which translates to:
!    2000<z<3000   mult = 1.00 + (z-2000.)*(0.25-1.0)/(3000.-2000.)
!    3000<z<5000   mult = 0.25 + (z-3000.)*(0.125-0.25)/(5000.-3000.)
! or in reduced form:
		   if( z(it,kt,jt) <= 2000. ) then
		      mult = 1.0
		   elseif( z(it,kt,jt) > 2000. &
                        .and. z(it,kt,jt) <= 3000. ) then
		      mult = 1.0 - 0.00075*(z(it,kt,jt)-2000.)
		   elseif( z(it,kt,jt) > 3000. &
                        .and. z(it,kt,jt) <= 5000. ) then
		      mult = 0.25 - 4.166666667e-5*(z(it,kt,jt)-3000.)
		   else
		      mult = 0.125
           end if
#if (CASENAME == 1)
        end if				!close l3 comparison block
#endif
#if (CASENAME == 2)
!     if( z(it,kt,jt) <= 3000. ) then
!       mult = 1.0
!     elseif( z(it,kt,jt) > 3000. &
!       .and. z(it,kt,jt) <= 5000. ) then
!       mult = 1.0 - 0.0004375*(z(it,kt,jt)-3000.)
!     else
!        mult = 0.125
!     end if
!
!     if( z(it,kt,jt) <= 5000. ) then
!       mult = 1.0
!     elseif( z(it,kt,jt) > 5000. &
!       .and. z(it,kt,jt) <= 7000. ) then
!       mult = 1.0 - 0.0004375*(z(it,kt,jt)-5000.)
!     else
!        mult = 0.125
!     end if
      mult=1.0
      if ( (l3 == lptr_so4_aer(m,itype,iphase)) .or. &
           (l3 == lptr_no3_aer(m,itype,iphase)) .or. &
           (l3 == lptr_nh4_aer(m,itype,iphase)) ) then
! so4, no3 and nh4 aerosol profiles
        if( z(it,kt,jt) <= 500. ) then
          mult = 1.000
        elseif( z(it,kt,jt) > 500. &
          .and. z(it,kt,jt) <= 1000. ) then
          mult = 1.000 - 0.0010740*(z(it,kt,jt)-500.)
        elseif( z(it,kt,jt) > 1000. &
          .and. z(it,kt,jt) <= 5000. ) then
          mult = 0.463 - 0.0001110*(z(it,kt,jt)-1000.)
        elseif( z(it,kt,jt) > 5000. &
          .and. z(it,kt,jt) <= 20000. ) then
          mult = 0.019
        else
          mult = 0.019
        end if
      else if ( (l3 == lptr_na_aer(m,itype,iphase)) .or. &
                (l3 == lptr_cl_aer(m,itype,iphase)) ) then
! na and cl aerosol profiles
        if( z(it,kt,jt) <= 500. ) then
          mult = 1.000
        elseif( z(it,kt,jt) > 500. &
          .and. z(it,kt,jt) <= 1000. ) then
          mult = 1.000 - 0.0014260*(z(it,kt,jt)-500.)
        elseif( z(it,kt,jt) > 1000. &
          .and. z(it,kt,jt) <= 5000. ) then
          mult = 0.287 - 0.0000643*(z(it,kt,jt)-1000.)
        elseif( z(it,kt,jt) > 5000. &
          .and. z(it,kt,jt) <= 10000. ) then
          mult = 0.030
        elseif( z(it,kt,jt) > 10000. &
          .and. z(it,kt,jt) <= 20000. ) then
          mult = 0.030 - 0.0000028*(z(it,kt,jt)-10000.)
        else
          mult = 0.002
        end if
      else if ( (l3 == lptr_oc_aer(m,itype,iphase)) .or. &
                (l3 == lptr_bc_aer(m,itype,iphase)) ) then
! oc and bc aerosol profile
        if( z(it,kt,jt) <= 500. ) then
          mult = 1.000
        elseif( z(it,kt,jt) > 500. &
          .and. z(it,kt,jt) <= 1000. ) then
          mult = 1.000 - 0.0002500*(z(it,kt,jt)-500.)
        elseif( z(it,kt,jt) > 1000. &
          .and. z(it,kt,jt) <= 5000. ) then
          mult = 0.875 - 0.0001843*(z(it,kt,jt)-1000.)
        elseif( z(it,kt,jt) > 5000. &
          .and. z(it,kt,jt) <= 20000. ) then
          mult = 0.138
        else
          mult = 0.138
        end if
      else if ( l3 == lptr_oin_aer(m,itype,iphase) ) then
! oin aerosol profile
        if( z(it,kt,jt) <= 500. ) then
          mult = 1.000
        elseif( z(it,kt,jt) > 500. &
          .and. z(it,kt,jt) <= 1000. ) then
          mult = 1.000 - 0.0007340*(z(it,kt,jt)-500.)
        elseif( z(it,kt,jt) > 1000. &
          .and. z(it,kt,jt) <= 5000. ) then
          mult = 0.633 - 0.0001103*(z(it,kt,jt)-1000.)
        elseif( z(it,kt,jt) > 5000. &
          .and. z(it,kt,jt) <= 10000. ) then
          mult = 0.192 - 0.0000080*(z(it,kt,jt)-5000.)
        elseif( z(it,kt,jt) > 10000. &
          .and. z(it,kt,jt) <= 20000. ) then
          mult = 0.152 - 0.0000137*(z(it,kt,jt)-10000.)
        else
          mult = 0.015
        end if
      else
        if( z(it,kt,jt) <= 5000. ) then
          mult = 1.0
        elseif( z(it,kt,jt) > 5000. &
          .and. z(it,kt,jt) <= 7000. ) then
          mult = 1.0 - 0.0004375*(z(it,kt,jt)-5000.)
        else
           mult = 0.125
        end if
      end if
#endif
#if (CASENAME == 3)
      mult = 1.000
      if( z(it,kt,jt) <= 500. ) then
        mult = 1.000
      elseif( z(it,kt,jt) > 500. &
        .and. z(it,kt,jt) <= 1000. ) then
        mult = 1.000 - 0.0010740*(z(it,kt,jt)-500.)
      elseif( z(it,kt,jt) > 1000. &
        .and. z(it,kt,jt) <= 5000. ) then
        mult = 0.463 - 0.0001110*(z(it,kt,jt)-1000.)
      else
        mult = 0.019
      end if
#endif

		    chem(it,kt,jt,l3) = mult*rclm(1,l1)
!wig 28-May-2004, ...end block
!		    chem(it,kt,jt,l3) = rclm(1,l1)
		end do
		end do
		end do
	    end if


2800   continue
2900   continue

!
!   do diagnostic output
!
	dumarr(:) = 0.0
	msg = ' '
	call peg_message( lunout, msg )
	msg = '*** subr mosaic_init_wrf_mixrats_opt2 results'
	call peg_message( lunout, msg )
	msg = '    mass in ug/m3     number in #/m3     volume in cm3/m3'
	call peg_message( lunout, msg )
	msg = ' '
	call peg_message( lunout, msg )
	msg = ' mode  l  l1  species      conc'
	call peg_message( lunout, msg )

        do 3190 mdum = 1, nsize_aer(itype)+1
	m = min( mdum, nsize_aer(itype) )
	msg = ' '
	call peg_message( lunout, msg )
        do 3150 l = 1, ncomp_plustracer_aer(itype)+4

            if (l .le. ncomp_plustracer_aer(itype)) then
                l1 = massptr_aer(l,m,itype,iphase)
		dumname = name_aer(l,itype)
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+1) then
                l1 = hyswptr_aer(m,itype)
		dumname = 'hystwatr'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+2) then
                l1 = waterptr_aer(m,itype)
		dumname = 'water'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+3) then
                l1 = numptr_aer(m,itype,iphase)
		dumname = 'number'
		dum = rclm(1,l1)
            else if (l .eq. ncomp_plustracer_aer(itype)+4) then
                l1 = 0
		dumname = 'volume'
		dum = vtot_ofmode(m)
	    else
		dumname = '=BADBAD='
		l1 = -1
		dum = -1.0
            end if

	    if (mdum .le. nsize_aer(itype)) then
		dumarr(l) = dumarr(l) + dum
		write(msg,9620) m, l, l1, dumname, dum
	    else
		write(msg,9625) l, dumname, dumarr(l)
	    end if
	    call peg_message( lunout, msg )

3150   continue
3190   continue

9620    format( 3i4, 2x, a, 3(1pe12.3) )
9625    format( ' sum', i4, '   -', 2x, a, 3(1pe12.3) )


!   all done
	return
	end subroutine mosaic_init_wrf_mixrats_opt2


!-----------------------------------------------------------------------
	real function erfc_num_recipes( x )
!
!   from press et al, numerical recipes, 1990, page 164
!
	implicit none
	real x
	double precision erfc_dbl, dum, t, zz

	zz = abs(x)
	t = 1.0/(1.0 + 0.5*zz)

!	erfc_num_recipes =
!     &	  t*exp( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +
!     &	  t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +
!     &	                                   t*(-1.13520398 +
!     &	  t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

	dum =  ( -zz*zz - 1.26551223 + t*(1.00002368 + t*(0.37409196 +   &
      	  t*(0.09678418 + t*(-0.18628806 + t*(0.27886807 +   &
      	                                   t*(-1.13520398 +   &
      	  t*(1.48851587 + t*(-0.82215223 + t*0.17087277 )))))))))

	erfc_dbl = t * exp(dum)
	if (x .lt. 0.0) erfc_dbl = 2.0d0 - erfc_dbl

	erfc_num_recipes = erfc_dbl

	return
	end function erfc_num_recipes     


!-----------------------------------------------------------------------
	end module module_mosaic_initmixrats




!-----------------------------------------------------------------------
 	subroutine bdy_chem_value_mosaic ( chem_bv, alt, zz, nch, config_flags )
!
! provides boundary values for the mosaic aerosol species
!
! it is outside of the module declaration because of potential
!    module1 --> module2 --> module1 use conflicts
!
! rce 11-sep-2004 - eliminated use of the _wrfch pointers (lptr_xxx_a_wrfch, 
!     lwaterptr_wrfch, numptr_wrfch); use only the _amode pointers now
!
	use module_configure, only:  grid_config_rec_type
	use module_state_description, only:  param_first_scalar,   &
			aer_bc_pnnl
	use module_data_mosaic_asect
	use module_data_mosaic_other
	implicit none

! arguments
	REAL,    intent(OUT)  :: chem_bv    ! boundary value for chem(-,-,-,nch)
	REAL,    intent(IN)   :: alt        ! inverse density
	REAL,    intent(IN)   :: zz         ! height
	INTEGER, intent(IN)   :: nch        ! index number of chemical species
	TYPE (grid_config_rec_type), intent(in) :: config_flags

! local variables
	integer :: iphase, itype, m
    logical :: foundit

	real, parameter :: chem_bv_def = 1.0e-20
!wig 10-May-2004, added mult
	real :: dumvol1p, mult
        integer :: iflg
	real :: qcoar, qfine, qval

	real :: fr_coar(8), fr_fine(8)

!wig 1-Apr-2005,  Updated fractional breakdown between bins. (See also
!                 mosaic_init_wrf_mixrats_opt2, above, and mosaic_addemiss
!                 in module_mosaic_addemiss.F). Note that these values no
!                 longer match those in mosaic_addemiss.
	real, save :: fr8b_coar(8) =   &
        (/ 0.0,  0.0,  0.0,  0.019,  0.0212,  0.1101,  0.2751, 0.7018 /) ! jdf Testbed
!jdf    (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.300, 0.700 /) ! 10-May-2005
!       (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.933, 0.067 /) ! 01-Apr-2005
!		(/ 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.16, 0.84 /)
	real, save :: fr8b_fine(8) =   &
        (/ 0.0245, 0.1472, 0.3501, 0.3317, 0.1251, 0.0186, 0.0011, 0.0/) !jdf Testbed
!jdf    (/ 0.006, 0.024, 0.231, 0.341, 0.140, 0.258, 0., 0./) !5-Apr-2005 values
!       (/ 0.0275, 0.0426, 0.2303, 0.3885, 0.1100, 0.2011, 0., 0./) !15-Nov-2004 values
!		(/ 0.01, 0.05, 0.145, 0.60, 0.145, 0.05, 0.00, 0.00 /)
!		(/ 0.04, 0.10, 0.35, 0.29, 0.15, 0.07, 0.00, 0.00 /)

	real :: qfine_so4, qfine_no3, qfine_cl, qfine_msa,   &
		qfine_co3, qfine_nh4, qfine_na, qfine_ca, qfine_oin,   &
		qfine_oc, qfine_bc, qfine_hysw, qfine_watr, qfine_vol
	real :: qcoar_so4, qcoar_no3, qcoar_cl, qcoar_msa,   &
		qcoar_co3, qcoar_nh4, qcoar_na, qcoar_ca, qcoar_oin,   &
		qcoar_oc, qcoar_bc, qcoar_hysw, qcoar_watr, qcoar_vol

	character*80 msg



! when aer_bc_opt /= aer_bc_pnnl,
! just chem_bv=near-zero for all species
	chem_bv = chem_bv_def
	if (config_flags%aer_bc_opt /= aer_bc_pnnl) return
	if (nch < param_first_scalar) return


!   *** currently only works for ntype_aer = 1
	itype = 1
	iphase = ai_phase
	m = 1 !This is here for size, but is overridden by loop below.


!
!   following is for aer_bc_opt == aer_bc_pnnl
!       when boundary values are set for Texas August 2000 simulations
!
!   set the partitioning fractions for either 8 or 4 bins
	if (nsize_aer(itype) == 8) then
	    fr_coar(:) = fr8b_coar(:)
	    fr_fine(:) = fr8b_fine(:)
	else if (nsize_aer(itype) == 4) then
	    do m = 1, nsize_aer(itype)
		fr_coar(m) = fr8b_coar(2*m) + fr8b_coar(2*m-1)
		fr_fine(m) = fr8b_fine(2*m) + fr8b_fine(2*m-1)
	    end do
	else
	    write(msg,'(a,i5)')   &
		'subr bdy_chem_value_mosaic' //   &
		' - nsize_aer(itype) must be 4 or 8 but = ', nsize_aer(itype)
	    call wrf_error_fatal( msg )
	end if

! Determine height multiplier...
! This should mimic the calculation in sorgam_set_aer_bc_pnnl,
! sorgam_init_aer_ic_pnnl, and mosaic_init_wrf_mixrats_opt2
!!$!    Height(m)     Multiplier
!!$!    ---------     ----------
!!$!    <=2000        1.0
!!$!    2000<z<3000   linear transition zone to 0.5
!!$!    3000<z<5000   linear transision zone to 0.25
!!$!    >=5000        0.25
!!$!
!!$! which translates to:
!!$!    2000<z<3000   mult = 1.0 + (z-2000.)*(0.5-1.0)/(3000.-2000.)
!!$!    3000<z<5000   mult = 0.5 + (z-3000.)*(0.25-0.5)/(5000.-3000.)
!!$! or in reduced form:
!!$        if( zz <= 2000. ) then
!!$           mult = 1.0
!!$        elseif( zz > 2000. &
!!$             .and. zz <= 3000. ) then
!!$           mult = 1.0 - 0.0005*(zz-2000.)
!!$        elseif( zz > 3000. &
!!$             .and. zz <= 5000. ) then
!!$           mult = 0.5 - 1.25e-4*(zz-3000.)
!!$        else
!!$           mult = 0.25
!!$        end if
#if (CASENAME == 1)
    mult = 1.0
    SIZE_LOOP : do m=1,nsize_aer(itype)
       if( ( nch .eq. lptr_so4_aer(m,itype,iphase) ) .or.  &
            (nch .eq. lptr_no3_aer(m,itype,iphase) ) .or.  &
            (nch .eq. lptr_nh4_aer(m,itype,iphase) )  )then
! so4, no3 and nh4 aerosol profiles
          if ( zz <= 1000. ) then
             mult = 1.0
		  elseif( zz > 1000. &
                 .and. zz <= 4000. ) then
             mult = 1.0 - 2.367e-4*(zz-1000.)
          elseif( zz > 4000. &
                 .and. zz <= 5500. ) then
             mult = 0.29 - 4.0e-5*(zz-4000.)
          else
             mult = 0.23
          end if
          exit SIZE_LOOP
       else if ( (nch .eq. lptr_na_aer(m,itype,iphase) ) .or.   &
	             (nch .eq. lptr_cl_aer(m,itype,iphase) ) ) then
! na and cl aerosol profiles
          if ( zz <= 100. ) then
             mult = 1.0
		  elseif( zz > 100. &
                  .and. zz <= 265. ) then
             mult = 1.0 - 2.9e-3*(zz-100.)
          elseif( zz > 265. &
                  .and. zz <= 2000. ) then
             mult = 0.52 - 2.94e-4*(zz-265.)
          elseif( zz > 2000. &
                  .and. zz <= 7000. ) then
             mult = 0.01
          else
             mult = 1.e-10
          end if
          exit SIZE_LOOP
       else if (nch .eq. lptr_oc_aer(m,itype,iphase) )  then
! oc aerosol profile
          if ( zz <= 600. ) then
             mult = 1.0
		  elseif( zz > 600. &
                  .and. zz <= 3389. ) then
             mult = 1.0 - 2.04e-4*(zz-600.)
          elseif( zz > 3389. &
                  .and. zz <= 8840. ) then
             mult = 0.43 - 7.045e-5*(zz-3389.)
          else
             mult = 0.046
          end if
          exit SIZE_LOOP
       else if  (nch .eq. lptr_bc_aer(m,itype,iphase) )   then
! bc aerosol profile
          if ( zz <= 100. ) then
             mult = 1.0
		  elseif( zz > 100. &
                  .and. zz <= 3400. ) then
             mult = 1.0 - 2.51e-4*(zz-100.)
          elseif( zz > 3400. &
                  .and. zz <= 8400. ) then
             mult = 0.172 -2.64e-5*(zz-3400.)
          else
             mult = 0.04
          end if
          exit SIZE_LOOP
       else if  (nch .eq. lptr_oin_aer(m,itype,iphase))  then
! oin aerosol profile
          if ( zz <= 1580. ) then
             mult = 1.0 + 1.77e-4 *zz
		  elseif( zz > 1580. &
                  .and. zz <= 5280. ) then
             mult = 1.28 - 2.65e-4*(zz-1580.)
          else
             mult = 0.30
          end if
          exit SIZE_LOOP
       else
! generic profile
#endif
! Updated aerosol profile multiplier 1-Apr-2005:
!    Height(m)     Multiplier
!    ---------     ----------
!    <=2000        1.0
!    2000<z<3000   linear transition zone to 0.25
!    3000<z<5000   linear transision zone to 0.125
!    >=5000        0.125
!
! which translates to:
!    2000<z<3000   mult = 1.00 + (z-2000.)*(0.25-1.0)/(3000.-2000.)
!    3000<z<5000   mult = 0.25 + (z-3000.)*(0.125-0.25)/(5000.-3000.)
! or in reduced form:
        if( zz <= 2000. ) then
           mult = 1.0
        elseif( zz > 2000. &
             .and. zz <= 3000. ) then
           mult = 1.0 - 0.00075*(zz-2000.)
        elseif( zz > 3000. &
             .and. zz <= 5000. ) then
           mult = 0.25 - 4.166666667e-5*(zz-3000.)
        else
           mult = 0.125
        end if
#if (CASENAME == 1)
      end if			! end nc block comparison
    end do SIZE_LOOP
#endif
#if (CASENAME == 2)
!     if( zz <= 3000. ) then
!       mult = 1.0
!     elseif( zz > 3000. &
!       .and. zz <= 5000. ) then
!       mult = 1.0 - 0.0004375*(zz-3000.)
!     else
!        mult = 0.125
!     end if
!     if( zz <= 5000. ) then
!       mult = 1.0
!     elseif( zz > 5000. &
!       .and. zz <= 7000. ) then
!       mult = 1.0 - 0.0004375*(zz-5000.)
!     else
!        mult = 0.125
!     end if
      mult=1.0
      iflg=0
      do 2901 m = 1, nsize_aer(itype)
      if(iflg.eq.1) goto 2902
      if( ( nch .eq. lptr_so4_aer(m,itype,iphase) ) .or.  &
        (nch .eq. lptr_no3_aer(m,itype,iphase) ) .or.  &
        (nch .eq. lptr_nh4_aer(m,itype,iphase) )  )then
! so4, no3 and nh4 aerosol profiles
        if( zz <= 500. ) then
          mult = 1.000
        elseif( zz > 500. &
          .and. zz <= 1000. ) then
          mult = 1.000 - 0.0010740*(zz-500.)
        elseif( zz > 1000. &
          .and. zz <= 5000. ) then
          mult = 0.463 - 0.0001110*(zz-1000.)
        elseif( zz > 5000. &
          .and. zz <= 20000. ) then
          mult = 0.019
        else
          mult = 0.019
        end if
        iflg=1
      else if ( (nch .eq. lptr_na_aer(m,itype,iphase) ) .or.   &
                (nch .eq. lptr_cl_aer(m,itype,iphase) ) ) then
! na and cl aerosol profiles
        if( zz <= 500. ) then
          mult = 1.000
        elseif( zz > 500. &
          .and. zz <= 1000. ) then
          mult = 1.000 - 0.0014260*(zz-500.)
        elseif( zz > 1000. &
          .and. zz <= 5000. ) then
          mult = 0.287 - 0.0000643*(zz-1000.)
        elseif( zz > 5000. &
          .and. zz <= 10000. ) then
          mult = 0.030
        elseif( zz > 10000. &
          .and. zz <= 20000. ) then
          mult = 0.030 - 0.0000028*(zz-10000.)
        else
          mult = 0.002
        end if
        iflg=1
      else if ( (nch .eq. lptr_oc_aer(m,itype,iphase) ) .or.   &
                (nch .eq. lptr_bc_aer(m,itype,iphase) ) ) then
! oc and bc aerosol profile
        if( zz <= 500. ) then
          mult = 1.000
        elseif( zz > 500. &
          .and. zz <= 1000. ) then
          mult = 1.000 - 0.0002500*(zz-500.)
        elseif( zz > 1000. &
          .and. zz <= 5000. ) then
          mult = 0.875 - 0.0001843*(zz-1000.)
        elseif( zz > 5000. &
          .and. zz <= 20000. ) then
          mult = 0.138
        else
          mult = 0.138
        end if
        iflg=1
      else if  (nch .eq. lptr_oin_aer(m,itype,iphase))  then
! oin aerosol profile
        if( zz <= 500. ) then
          mult = 1.000
        elseif( zz > 500. &
          .and. zz <= 1000. ) then
          mult = 1.000 - 0.0007340*(zz-500.)
        elseif( zz > 1000. &
          .and. zz <= 5000. ) then
          mult = 0.633 - 0.0001103*(zz-1000.)
        elseif( zz > 5000. &
          .and. zz <= 10000. ) then
          mult = 0.192 - 0.0000080*(zz-5000.)
        elseif( zz > 10000. &
          .and. zz <= 20000. ) then
          mult = 0.152 - 0.0000137*(zz-10000.)
        else
          mult = 0.015
        end if
        iflg=1
      else
        if( zz <= 5000. ) then
          mult = 1.0
        elseif( zz > 5000. &
          .and. zz <= 7000. ) then
          mult = 1.0 - 0.0004375*(zz-5000.)
        else
           mult = 0.125
        end if
      end if
2901  continue
2902  continue
#endif
#if (CASENAME == 3)
      if( zz <= 500. ) then
        mult = 1.000
      elseif( zz > 500. &
        .and. zz <= 1000. ) then
        mult = 1.000 - 0.0010740*(zz-500.)
      elseif( zz > 1000. &
        .and. zz <= 5000. ) then
        mult = 0.463 - 0.0001110*(zz-1000.)
      else
        mult = 0.019
      end if
#endif
!   Set values for fine and coarse mass concentrations, and
!   compute fine/coarse volume concentrations. These are also set in
!   mosaic_init_wrf_mixrats_opt2.
!   (these are latest values from 1-Apr-2005 discussions)
!wig 10-May-2004, added height multiplier (mult*) to each species...
	qfine_so4 = mult*2.14
	qcoar_so4 = mult*0.242
	qfine_no3 = mult*0.11
	qcoar_no3 = mult*0.03
!	qfine_cl  = mult*0.3
	qfine_cl  = mult*0.14     ! 10-May-2005
	qcoar_cl  = mult*0.139
	qfine_msa = mult*0.0
	qcoar_msa = mult*0.0
	qfine_co3 = mult*0.0
	qcoar_co3 = mult*0.0
	qfine_nh4 = mult*0.83
	qcoar_nh4 = mult*0.10
!	qfine_na  = mult*0.2
	qfine_na  = mult*0.1      ! 10-May-2005
	qcoar_na  = mult*0.09
	qfine_ca  = mult*0.0
	qcoar_ca  = mult*0.0
!	qfine_oin = mult*6.2
!	qfine_oin = 3.48     ! 10-May-2005
	qfine_oin = mult*3.48     ! not sure why mult was dropped from qfine_oin
	qcoar_oin = mult*0.35
	qfine_oc  = mult*1.00
	qcoar_oc  = mult*1.50
	qfine_bc  = mult*0.2
	qcoar_bc  = mult*0.075
	qfine_hysw = mult*0.0
	qcoar_hysw = mult*0.0
	qfine_watr = mult*0.0
	qcoar_watr = mult*0.0
#if (CASENAME == 2)
!       qfine_so4 = mult*0.50
!       qcoar_so4 = mult*0.50/2.0
!       qfine_no3 = mult*0.75
!       qcoar_no3 = mult*0.75/2.0
!       qfine_cl  = mult*0.10
!       qcoar_cl  = mult*0.10/2.0
!       qfine_nh4 = mult*0.30
!       qcoar_nh4 = mult*0.30/2.0
!       qfine_na  = mult*0.20
!       qcoar_na  = mult*0.20/2.0
!       qfine_oin = mult*4.00
!       qcoar_oin = mult*4.00/2.0
!       qfine_oc  = mult*3.00
!       qcoar_oc  = mult*3.00/2.0
!       qfine_bc  = mult*0.50
!       qcoar_bc  = mult*0.50/2.0
        qfine_so4 = mult*0.300
        qcoar_so4 = mult*0.300/2.0
        qfine_no3 = mult*0.001
        qcoar_no3 = mult*0.001/2.0
        qfine_cl  = mult*1.050
        qcoar_cl  = mult*1.05/02.0
        qfine_nh4 = mult*0.094
        qcoar_nh4 = mult*0.094/2.0
        qfine_na  = mult*0.700
        qcoar_na  = mult*0.700/2.0
        qfine_oin = mult*4.500
        qcoar_oin = mult*4.500/2.0
        qfine_oc  = mult*0.088
        qcoar_oc  = mult*0.088/2.0
        qfine_bc  = mult*0.013
        qcoar_bc  = mult*0.013/2.0
#endif
#if (CASENAME == 3)
        qfine_so4 = mult*0.300
        qcoar_so4 = mult*0.0
        qfine_nh4 = mult*0.094
        qcoar_nh4 = mult*0.0
        qfine_no3 = mult*0.001
        qcoar_no3 = mult*0.0
        qfine_cl  = mult*0.0
        qcoar_cl  = mult*1.050
        qfine_na  = mult*0.000
        qcoar_na  = mult*0.700
        qfine_oin = mult*4.500
        qcoar_oin = mult*4.500/2.0
        qfine_oc  = mult*0.088
        qcoar_oc  = mult*0.0
        qfine_bc  = mult*0.013
        qcoar_bc  = mult*0.0
#endif
!!$! old values from 23-Apr-2004:
!!$	qfine_so4 = mult*2.554
!!$	qcoar_so4 = mult*0.242
!!$	qfine_no3 = mult*0.07
!!$	qcoar_no3 = mult*0.03
!!$	qfine_cl  = mult*0.324
!!$	qcoar_cl  = mult*0.139
!!$	qfine_msa = mult*0.0
!!$	qcoar_msa = mult*0.0
!!$	qfine_co3 = mult*0.0
!!$	qcoar_co3 = mult*0.0
!!$	qfine_nh4 = mult*0.98
!!$	qcoar_nh4 = mult*0.10
!!$	qfine_na  = mult*0.21
!!$	qcoar_na  = mult*0.09
!!$	qfine_ca  = mult*0.0
!!$	qcoar_ca  = mult*0.0
!!$	qfine_oin = mult*0.15
!!$	qcoar_oin = mult*0.35
!!$	qfine_oc  = mult*1.00
!!$	qcoar_oc  = mult*1.50
!!$	qfine_bc  = mult*0.175
!!$	qcoar_bc  = mult*0.075
!!$	qfine_hysw = mult*0.0
!!$	qcoar_hysw = mult*0.0
!!$	qfine_watr = mult*0.0
!!$	qcoar_watr = mult*0.0


! qfine_so4 ... are ug/m3 so 1.0e-6 factor gives g/m3
! dens_so4  ... are g/cm3;  qfine_vol ... are cm3/m3
	qfine_vol = 1.0e-6 * (   &
	    (qfine_so4/dens_so4_aer) + (qfine_no3/dens_no3_aer) +   &
	    (qfine_cl /dens_cl_aer ) + (qfine_msa/dens_msa_aer) +   &
	    (qfine_co3/dens_co3_aer) + (qfine_nh4/dens_nh4_aer) +   &
	    (qfine_na /dens_na_aer ) + (qfine_ca /dens_ca_aer ) +   &
	    (qfine_oin/dens_oin_aer) + (qfine_oc /dens_oc_aer ) +   &
	    (qfine_bc /dens_bc_aer ) )
	qcoar_vol = 1.0e-6 * (   &
	    (qcoar_so4/dens_so4_aer) + (qcoar_no3/dens_no3_aer) +   &
	    (qcoar_cl /dens_cl_aer ) + (qcoar_msa/dens_msa_aer) +   &
	    (qcoar_co3/dens_co3_aer) + (qcoar_nh4/dens_nh4_aer) +   &
	    (qcoar_na /dens_na_aer ) + (qcoar_ca /dens_ca_aer ) +   &
	    (qcoar_oin/dens_oin_aer) + (qcoar_oc /dens_oc_aer ) +   &
	    (qcoar_bc /dens_bc_aer ) )

	qfine = -1.0e30
	qcoar = -1.0e30

!   identify the species by checking "nch" against the "lptr_xxx_a_amode(m)"
        do 2900 m = 1, nsize_aer(itype)

	    if (nch .eq. lptr_so4_aer(m,itype,iphase)) then
		qfine = qfine_so4
		qcoar = qcoar_so4
	    else if (nch .eq. lptr_no3_aer(m,itype,iphase)) then
		qfine = qfine_no3
		qcoar = qcoar_no3
	    else if (nch .eq. lptr_cl_aer(m,itype,iphase)) then
		qfine = qfine_cl
		qcoar = qcoar_cl
	    else if (nch .eq. lptr_msa_aer(m,itype,iphase)) then
		qfine = qfine_msa
		qcoar = qcoar_msa
	    else if (nch .eq. lptr_co3_aer(m,itype,iphase)) then
		qfine = qfine_co3
		qcoar = qcoar_co3
	    else if (nch .eq. lptr_nh4_aer(m,itype,iphase)) then
		qfine = qfine_nh4
		qcoar = qcoar_nh4
	    else if (nch .eq. lptr_na_aer(m,itype,iphase)) then
		qfine = qfine_na
		qcoar = qcoar_na
	    else if (nch .eq. lptr_ca_aer(m,itype,iphase)) then
		qfine = qfine_ca
		qcoar = qcoar_ca
	    else if (nch .eq. lptr_oin_aer(m,itype,iphase)) then
		qfine = qfine_oin
		qcoar = qcoar_oin
	    else if (nch .eq. lptr_oc_aer(m,itype,iphase)) then
		qfine = qfine_oc
		qcoar = qcoar_oc
	    else if (nch .eq. lptr_bc_aer(m,itype,iphase)) then
		qfine = qfine_bc
		qcoar = qcoar_bc
	    else if (nch .eq. hyswptr_aer(m,itype)) then
		qfine = qfine_hysw
		qcoar = qcoar_hysw
	    else if (nch .eq. waterptr_aer(m,itype)) then
		qfine = qfine_watr
		qcoar = qcoar_watr
	    else if (nch .eq. numptr_aer(m,itype,iphase)) then
		dumvol1p = sqrt(volumlo_sect(m,itype)*volumhi_sect(m,itype))
		qfine = qfine_vol/dumvol1p
		qcoar = qcoar_vol/dumvol1p
	    end if

	    if ((qfine >= 0.0) .and. (qcoar >= 0.0)) then
		qval = qfine*fr_fine(m) + qcoar*fr_coar(m)
		chem_bv = qval*alt
		goto 2910
	    end if

2900   continue
2910   continue

	return
 	end subroutine bdy_chem_value_mosaic


