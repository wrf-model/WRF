MODULE module_prep_wetscav_sorgam 

USE module_state_description
USE module_configure
USE module_mosaic_wetscav,only:  initwet,wetscav

IMPLICIT NONE

#define cw_species_are_in_registry

CONTAINS

SUBROUTINE aerosols_sorgam_init_aercld_ptrs(   &
         num_chem, is_aerosol, config_flags )
!
! initialize pointers used by aerosol-cloud-interaction routines
!
     USE module_data_sorgam
!    USE module_configure,only:  grid_config_rec_type
!    USE module_mosaic_wetscav,only:  initwet

    implicit none
    INTEGER, INTENT(IN) :: num_chem
    LOGICAL, INTENT(OUT) :: is_aerosol(num_chem)
    TYPE (grid_config_rec_type) , INTENT (in) :: config_flags


    integer iphase, isize, itype, l, ll, n, p1st
    REAL dp_meanvol_tmp


        nphase_aer = 1
	if(p_so4cwj.ge. param_first_scalar) then
           nphase_aer = 2
	endif
	ai_phase=-999888777
	cw_phase=-999888777
	ci_phase=-999888777
	cr_phase=-999888777
	cs_phase=-999888777
	cg_phase=-999888777
	if(nphase_aer>=1)ai_phase=1
	if(nphase_aer>=2)cw_phase=2
	if(nphase_aer>=3)cr_phase=3
	if(nphase_aer>=4)ci_phase=4
	if(nphase_aer>=5)cw_phase=5
	if(nphase_aer>=6)cg_phase=6

! aitken and accum mode have same set of species 
!    so are treated as isize=1,2 of itype=1
! coarse mode has different set of species
!    so is  treated as isize=1 of itype=2
        ntype_aer = 2
	nsize_aer(1)=2
	nsize_aer(2)=1

	msectional = 0
	maerosolincw = 0
#if defined ( cw_species_are_in_registry )
	maerosolincw = 1
#endif
	name_mastercomp_aer( 1) = 'sulfate'
	dens_mastercomp_aer( 1) = dens_so4_aer
	mw_mastercomp_aer(   1) =   mw_so4_aer
	hygro_mastercomp_aer(1) = hygro_so4_aer

	name_mastercomp_aer( 2) = 'nitrate'
	dens_mastercomp_aer( 2) = dens_no3_aer
	mw_mastercomp_aer(   2) =   mw_no3_aer
	hygro_mastercomp_aer(2) = hygro_no3_aer

	name_mastercomp_aer( 3) = 'ammonium'
	dens_mastercomp_aer( 3) = dens_nh4_aer
	mw_mastercomp_aer(   3) =   mw_nh4_aer
	hygro_mastercomp_aer(3) = hygro_nh4_aer

	name_mastercomp_aer( 4) = 'orgaro1'
	dens_mastercomp_aer( 4) = dens_oc_aer
	mw_mastercomp_aer(   4) =   mw_oc_aer
	hygro_mastercomp_aer(4) = hygro_oc_aer

	name_mastercomp_aer( 5) = 'orgaro2'
	dens_mastercomp_aer( 5) = dens_oc_aer
	mw_mastercomp_aer(   5) =   mw_oc_aer
	hygro_mastercomp_aer(5) = hygro_oc_aer

	name_mastercomp_aer( 6) = 'orgalk'
	dens_mastercomp_aer( 6) = dens_oc_aer
	mw_mastercomp_aer(   6) =   mw_oc_aer
	hygro_mastercomp_aer(6) = hygro_oc_aer

	name_mastercomp_aer( 7) = 'orgole'
	dens_mastercomp_aer( 7) = dens_oc_aer
	mw_mastercomp_aer(   7) =   mw_oc_aer
	hygro_mastercomp_aer(7) = hygro_oc_aer

	name_mastercomp_aer( 8) = 'orgba1'
	dens_mastercomp_aer( 8) = dens_oc_aer
	mw_mastercomp_aer(   8) =   mw_oc_aer
	hygro_mastercomp_aer(8) = hygro_oc_aer

	name_mastercomp_aer( 9) = 'orgba2'
	dens_mastercomp_aer( 9) = dens_oc_aer
	mw_mastercomp_aer(   9) =   mw_oc_aer
	hygro_mastercomp_aer(9) = hygro_oc_aer

	name_mastercomp_aer( 10) = 'orgba3'
	dens_mastercomp_aer( 10) = dens_oc_aer
	mw_mastercomp_aer(   10) =   mw_oc_aer
	hygro_mastercomp_aer(10) = hygro_oc_aer

	name_mastercomp_aer( 11) = 'orgba4'
	dens_mastercomp_aer( 11) = dens_oc_aer
	mw_mastercomp_aer(   11) =   mw_oc_aer
	hygro_mastercomp_aer(11) = hygro_oc_aer

	name_mastercomp_aer( 12) = 'orgpa'
	dens_mastercomp_aer( 12) = dens_oc_aer
	mw_mastercomp_aer(   12) =   mw_oc_aer
	hygro_mastercomp_aer(12) = hygro_oc_aer

	name_mastercomp_aer( 13) = 'ec'
	dens_mastercomp_aer( 13) = dens_ec_aer
	mw_mastercomp_aer(   13) =   mw_ec_aer
	hygro_mastercomp_aer(13) = hygro_ec_aer
	name_mastercomp_aer( 14) = 'p25'
	dens_mastercomp_aer( 14) = dens_oin_aer
	mw_mastercomp_aer(   14) =   mw_oin_aer
	hygro_mastercomp_aer(14) = hygro_oin_aer

	name_mastercomp_aer( 15) = 'anth'
	dens_mastercomp_aer( 15) = dens_oin_aer
	mw_mastercomp_aer(   15) =   mw_oin_aer
	hygro_mastercomp_aer(15) = hygro_oin_aer

	name_mastercomp_aer( 16) = 'seas'
	dens_mastercomp_aer( 16) = dens_seas_aer
	mw_mastercomp_aer(   16) =   mw_seas_aer
	hygro_mastercomp_aer(16) = hygro_seas_aer

	name_mastercomp_aer( 17) = 'soil'
	dens_mastercomp_aer( 17) = dens_dust_aer
	mw_mastercomp_aer(   17) =  mw_dust_aer
	hygro_mastercomp_aer(17) = hygro_dust_aer

  	name_mastercomp_aer(18)  = 'sodium'
  	dens_mastercomp_aer(18)  = dens_na_aer
  	mw_mastercomp_aer(  18)  =   mw_na_aer
  	hygro_mastercomp_aer(18) = hygro_na_aer
  
  	name_mastercomp_aer(19)  = 'chloride'
  	dens_mastercomp_aer(19)  = dens_cl_aer
  	mw_mastercomp_aer(  19)  =   mw_cl_aer
  	hygro_mastercomp_aer(19) = hygro_cl_aer

	lptr_so4_aer(    :,:,:) = 1
	lptr_nh4_aer(    :,:,:) = 1
	lptr_no3_aer(    :,:,:) = 1
	lptr_na_aer(     :,:,:) = 1
	lptr_cl_aer(     :,:,:) = 1
	lptr_orgaro1_aer(:,:,:) = 1
	lptr_orgaro2_aer(:,:,:) = 1
	lptr_orgalk_aer( :,:,:) = 1
	lptr_orgole_aer( :,:,:) = 1
	lptr_orgba1_aer( :,:,:) = 1
	lptr_orgba2_aer( :,:,:) = 1
	lptr_orgba3_aer( :,:,:) = 1
	lptr_orgba4_aer( :,:,:) = 1
	lptr_orgpa_aer(  :,:,:) = 1
	lptr_ec_aer(     :,:,:) = 1
	lptr_p25_aer(    :,:,:) = 1
	lptr_anth_aer(   :,:,:) = 1
	lptr_seas_aer(   :,:,:) = 1
	lptr_soil_aer(   :,:,:) = 1
	numptr_aer(      :,:,:) = 1

	do_cloudchem_aer(:,:) = .false.

! Aitken mode
	itype = 1
	isize = 1
	ncomp_aer(itype) = 16
	numptr_aer(      isize,itype,ai_phase) = p_nu0
      	lptr_so4_aer(    isize,itype,ai_phase) = p_so4ai
      	lptr_nh4_aer(    isize,itype,ai_phase) = p_nh4ai
      	lptr_no3_aer(    isize,itype,ai_phase) = p_no3ai
        lptr_na_aer(     isize,itype,ai_phase) = p_naai
        lptr_cl_aer(     isize,itype,ai_phase) = p_clai
      	lptr_orgaro1_aer(isize,itype,ai_phase) = p_orgaro1i
      	lptr_orgaro2_aer(isize,itype,ai_phase) = p_orgaro2i
      	lptr_orgalk_aer( isize,itype,ai_phase) = p_orgalk1i
      	lptr_orgole_aer( isize,itype,ai_phase) = p_orgole1i
      	lptr_orgba1_aer( isize,itype,ai_phase) = p_orgba1i
      	lptr_orgba2_aer( isize,itype,ai_phase) = p_orgba2i
      	lptr_orgba3_aer( isize,itype,ai_phase) = p_orgba3i
      	lptr_orgba4_aer( isize,itype,ai_phase) = p_orgba4i
      	lptr_orgpa_aer(  isize,itype,ai_phase) = p_orgpai
      	lptr_ec_aer(     isize,itype,ai_phase) = p_eci
      	lptr_p25_aer(    isize,itype,ai_phase) = p_p25i
! aerosol in cloud water
        if(cw_phase.gt.0)then
	  numptr_aer(      isize,itype,cw_phase) = p_nu0cw
      	  lptr_so4_aer(    isize,itype,cw_phase) = p_so4cwi
      	  lptr_nh4_aer(    isize,itype,cw_phase) = p_nh4cwi
      	  lptr_no3_aer(    isize,itype,cw_phase) = p_no3cwi
          lptr_na_aer(     isize,itype,ai_phase) = p_nacwi
          lptr_cl_aer(     isize,itype,ai_phase) = p_clcwi
      	  lptr_orgaro1_aer(isize,itype,cw_phase) = p_orgaro1cwi
      	  lptr_orgaro2_aer(isize,itype,cw_phase) = p_orgaro2cwi
      	  lptr_orgalk_aer( isize,itype,cw_phase) = p_orgalk1cwi
      	  lptr_orgole_aer( isize,itype,cw_phase) = p_orgole1cwi
      	  lptr_orgba1_aer( isize,itype,cw_phase) = p_orgba1cwi
      	  lptr_orgba2_aer( isize,itype,cw_phase) = p_orgba2cwi
      	  lptr_orgba3_aer( isize,itype,cw_phase) = p_orgba3cwi
      	  lptr_orgba4_aer( isize,itype,cw_phase) = p_orgba4cwi
      	  lptr_orgpa_aer(  isize,itype,cw_phase) = p_orgpacwi
      	  lptr_ec_aer(     isize,itype,cw_phase) = p_eccwi
      	  lptr_p25_aer(    isize,itype,cw_phase) = p_p25cwi
	  do_cloudchem_aer(isize,itype) = .true.
	endif

! Accumulation mode
	itype = 1
	isize = 2
	ncomp_aer(itype) = 16
	numptr_aer(      isize,itype,ai_phase) = p_ac0
      	lptr_so4_aer(    isize,itype,ai_phase) = p_so4aj
      	lptr_nh4_aer(    isize,itype,ai_phase) = p_nh4aj
      	lptr_no3_aer(    isize,itype,ai_phase) = p_no3aj
        lptr_na_aer(     isize,itype,ai_phase) = p_naaj
        lptr_cl_aer(     isize,itype,ai_phase) = p_claj
      	lptr_orgaro1_aer(isize,itype,ai_phase) = p_orgaro1j
      	lptr_orgaro2_aer(isize,itype,ai_phase) = p_orgaro2j
      	lptr_orgalk_aer( isize,itype,ai_phase) = p_orgalk1j
      	lptr_orgole_aer( isize,itype,ai_phase) = p_orgole1j
      	lptr_orgba1_aer( isize,itype,ai_phase) = p_orgba1j
      	lptr_orgba2_aer( isize,itype,ai_phase) = p_orgba2j
      	lptr_orgba3_aer( isize,itype,ai_phase) = p_orgba3j
      	lptr_orgba4_aer( isize,itype,ai_phase) = p_orgba4j
      	lptr_orgpa_aer(  isize,itype,ai_phase) = p_orgpaj
      	lptr_ec_aer(     isize,itype,ai_phase) = p_ecj
      	lptr_p25_aer(    isize,itype,ai_phase) = p_p25j
! aerosol in cloud water
        if(cw_phase.gt.0)then
          numptr_aer(      isize,itype,cw_phase) = p_ac0cw
      	  lptr_so4_aer(    isize,itype,cw_phase) = p_so4cwj
      	  lptr_nh4_aer(    isize,itype,cw_phase) = p_nh4cwj
      	  lptr_no3_aer(    isize,itype,cw_phase) = p_no3cwj
          lptr_na_aer(     isize,itype,ai_phase) = p_nacwj
          lptr_cl_aer(     isize,itype,ai_phase) = p_clcwj
      	  lptr_orgaro1_aer(isize,itype,cw_phase) = p_orgaro1cwj
      	  lptr_orgaro2_aer(isize,itype,cw_phase) = p_orgaro2cwj
      	  lptr_orgalk_aer( isize,itype,cw_phase) = p_orgalk1cwj
      	  lptr_orgole_aer( isize,itype,cw_phase) = p_orgole1cwj
      	  lptr_orgba1_aer( isize,itype,cw_phase) = p_orgba1cwj
      	  lptr_orgba2_aer( isize,itype,cw_phase) = p_orgba2cwj
      	  lptr_orgba3_aer( isize,itype,cw_phase) = p_orgba3cwj
      	  lptr_orgba4_aer( isize,itype,cw_phase) = p_orgba4cwj
      	  lptr_orgpa_aer(  isize,itype,cw_phase) = p_orgpacwj
      	  lptr_ec_aer(     isize,itype,cw_phase) = p_eccwj
      	  lptr_p25_aer(    isize,itype,cw_phase) = p_p25cwj
	  do_cloudchem_aer(isize,itype) = .true.
	endif

! coarse mode
	itype = 2
	isize = 1
	ncomp_aer(itype) = 3
	numptr_aer(      isize,itype,ai_phase) = p_corn
        lptr_anth_aer(   isize,itype,ai_phase) = p_antha
      	lptr_seas_aer(   isize,itype,ai_phase) = p_seas
      	lptr_soil_aer(   isize,itype,ai_phase) = p_soila
! aerosol in cloud water
        if(cw_phase.gt.0)then
	  numptr_aer(      isize,itype,cw_phase) = p_corncw
          lptr_anth_aer(   isize,itype,cw_phase) = p_anthcw
      	  lptr_seas_aer(   isize,itype,cw_phase) = p_seascw
      	  lptr_soil_aer(   isize,itype,cw_phase) = p_soilcw

! no cloudchem for coarse mode because it has no so4/nh4/no3 species
	  do_cloudchem_aer(isize,itype) = .false.
	endif

	massptr_aer(:,:,:,:) = -999888777
	mastercompptr_aer(:,:) = -999888777

	p1st = param_first_scalar

	do iphase=1,nphase_aer
	do itype=1,ntype_aer
	do n = 1, nsize_aer(itype)
	    ll = 0
	    if (lptr_so4_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_so4_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 1
	    end if
	    if (lptr_no3_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_no3_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 2
	    end if
	    if (lptr_nh4_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_nh4_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 3
	    end if
	    if (lptr_orgaro1_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgaro1_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 4
	    end if
	    if (lptr_orgaro2_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgaro2_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 5
	    end if
	    if (lptr_orgalk_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgalk_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 6
	    end if
	    if (lptr_orgole_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgole_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 7
	    end if
	    if (lptr_orgba1_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgba1_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 8
	    end if
	    if (lptr_orgba2_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgba2_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 9
	    end if
	    if (lptr_orgba3_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgba3_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 10
	    end if
	    if (lptr_orgba4_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgba4_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 11
	    end if
	    if (lptr_orgpa_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_orgpa_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 12
	    end if
	    if (lptr_ec_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_ec_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 13
	    end if
	    if (lptr_p25_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_p25_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 14
	    end if
	    if (lptr_anth_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_anth_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 15
	    end if
	    if (lptr_seas_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_seas_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 16
	    end if
	    if (lptr_soil_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_soil_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 17
	    end if
	    if (lptr_na_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_na_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 18
	    end if
	    if (lptr_cl_aer(n,itype,iphase) .ge. p1st) then
		ll = ll + 1
		massptr_aer(ll,n,itype,iphase) = lptr_cl_aer(n,itype,iphase)
		mastercompptr_aer(ll,itype) = 19
            endif
	    ncomp_aer_nontracer(itype) = ll

	    ncomp_aer(itype) = ll

	    mprognum_aer(n,itype,iphase) = 0
	    if (numptr_aer(n,itype,iphase) .ge. p1st) then
		mprognum_aer(n,itype,iphase) = 1
	    end if

	end do ! size
	end do ! type
	end do ! phase

        waterptr_aer(:,:) = 0

	do itype=1,ntype_aer
	do ll=1,ncomp_aer(itype)
	   dens_aer(ll,itype) = dens_mastercomp_aer(mastercompptr_aer(ll,itype))
	   mw_aer(ll,itype) = mw_mastercomp_aer(mastercompptr_aer(ll,itype))
	   hygro_aer(ll,itype) = hygro_mastercomp_aer(mastercompptr_aer(ll,itype))
	   name_aer(ll,itype) = name_mastercomp_aer(mastercompptr_aer(ll,itype))
	end do
	end do

	is_aerosol(:) = .false.
	do iphase=1,nphase_aer
	do itype=1,ntype_aer
	do n = 1, nsize_aer(itype)
	    do ll = 1, ncomp_aer(itype)
	      is_aerosol(massptr_aer(ll,n,itype,iphase))=.true.
	    end do
	    is_aerosol(numptr_aer(n,itype,iphase))=.true.
	end do ! size
	end do ! type
	end do ! phase
        
! for sectional
!    the dhi/dlo_sect are the upper/lower bounds for 
!       mean-volume diameter for a section/bin
! for modal
!    they should be set to reasonable upper/lower
!       bounds for mean-volume diameters of each modes
!    they are primarily used to put reasonable bounds
!       on number (in relation to mass/volume)
!    the dcen_sect are used by initwet for the impaction scavenging
!       lookup tables, and should represent a "base" mean-volume diameter
! dp_meanvol_tmp (below) is the made-sorgam default initial value 
!    for mean-volume diameter (in cm)
! terminology:  (pi/6) * (mean-volume diameter)**3 ==
!	(volume mixing ratio of section/mode)/(number mixing ratio)
!
        dhi_sect(:,:) = 0.0
        dlo_sect(:,:) = 0.0

        itype = 1
        isize = 1
        sigmag_aer(isize,itype) = sginin ! aitken
        dp_meanvol_tmp = 1.0e2*dginin*exp(1.5*l2sginin) ! aitken
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        itype = 1
        isize = 2
        sigmag_aer(isize,itype) = sginia ! accum
        dp_meanvol_tmp = 1.0e2*dginia*exp(1.5*l2sginia) ! accum 
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        itype = 2
        isize = 1
        sigmag_aer(isize,itype) = sginic ! coarse
        dp_meanvol_tmp = 1.0e2*dginic*exp(1.5*l2sginic) ! coarse
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        do itype = 1, ntype_aer
        do isize = 1, nsize_aer(itype)
           volumcen_sect(isize,itype) = (pirs/6.0)*(dcen_sect(isize,itype)**3)
           volumlo_sect(isize,itype)  = (pirs/6.0)*(dlo_sect(isize,itype)**3)
           volumhi_sect(isize,itype)  = (pirs/6.0)*(dhi_sect(isize,itype)**3)
        end do
        end do


! do initialization of the impaction/interception scavenging 
! lookup tables
        call initwet(   &
            ntype_aer, nsize_aer, ncomp_aer,   &
            massptr_aer, dens_aer, numptr_aer,           &
            maxd_acomp, maxd_asize,maxd_atype, maxd_aphase,   &
            dcen_sect, sigmag_aer, &
            waterptr_aer, dens_water_aer, &
            scavimptblvol, scavimptblnum, nimptblgrow_mind,   &
            nimptblgrow_maxd, dlndg_nimptblgrow )

END SUBROUTINE aerosols_sorgam_init_aercld_ptrs

SUBROUTINE aerosols_soa_vbs_init_aercld_ptrs(   &
         num_chem, is_aerosol, config_flags )
!
! initialize pointers used by aerosol-cloud-interaction routines
!
     USE module_data_soa_vbs
!    USE module_configure,only:  grid_config_rec_type
!    USE module_mosaic_wetscav,only:  initwet

    implicit none
    INTEGER, INTENT(IN) :: num_chem
    LOGICAL, INTENT(OUT) :: is_aerosol(num_chem)
    TYPE (grid_config_rec_type) , INTENT (in) :: config_flags


    integer iphase, isize, itype, l, ll, n, p1st
    REAL dp_meanvol_tmp

        nphase_aer = 1
        if(p_so4cwj.ge. param_first_scalar) then
           nphase_aer = 2
        endif
        ai_phase=-999888777
        cw_phase=-999888777
        ci_phase=-999888777
        cr_phase=-999888777
        cs_phase=-999888777
        cg_phase=-999888777
        if(nphase_aer>=1)ai_phase=1
        if(nphase_aer>=2)cw_phase=2
        if(nphase_aer>=3)cr_phase=3
        if(nphase_aer>=4)ci_phase=4
        if(nphase_aer>=5)cw_phase=5
        if(nphase_aer>=6)cg_phase=6

! aitken and accum mode have same set of species 
!    so are treated as isize=1,2 of itype=1
! coarse mode has different set of species
!    so is  treated as isize=1 of itype=2
        ntype_aer = 2
        nsize_aer(1)=2
        nsize_aer(2)=1

        msectional = 0
        maerosolincw = 0
#if defined ( cw_species_are_in_registry )
        maerosolincw = 1
#endif
        name_mastercomp_aer( 1) = 'sulfate'
        dens_mastercomp_aer( 1) = dens_so4_aer
        mw_mastercomp_aer(   1) =   mw_so4_aer
        hygro_mastercomp_aer(1) = hygro_so4_aer

        name_mastercomp_aer( 2) = 'nitrate'
        dens_mastercomp_aer( 2) = dens_no3_aer
        mw_mastercomp_aer(   2) =   mw_no3_aer
        hygro_mastercomp_aer(2) = hygro_no3_aer

        name_mastercomp_aer( 3) = 'ammonium'
        dens_mastercomp_aer( 3) = dens_nh4_aer
        mw_mastercomp_aer(   3) =   mw_nh4_aer
        hygro_mastercomp_aer(3) = hygro_nh4_aer

        name_mastercomp_aer( 4) = 'asoa1'
        dens_mastercomp_aer( 4) = dens_oc_aer
        mw_mastercomp_aer(   4) =   mw_oc_aer
        hygro_mastercomp_aer(4) = hygro_oc_aer

        name_mastercomp_aer( 5) = 'asoa2'
        dens_mastercomp_aer( 5) = dens_oc_aer
        mw_mastercomp_aer(   5) =   mw_oc_aer
        hygro_mastercomp_aer(5) = hygro_oc_aer

        name_mastercomp_aer( 6) = 'asoa3'
        dens_mastercomp_aer( 6) = dens_oc_aer
        mw_mastercomp_aer(   6) =   mw_oc_aer
        hygro_mastercomp_aer(6) = hygro_oc_aer

        name_mastercomp_aer( 7) = 'asoa4'
        dens_mastercomp_aer( 7) = dens_oc_aer
        mw_mastercomp_aer(   7) =   mw_oc_aer
        hygro_mastercomp_aer(7) = hygro_oc_aer

        name_mastercomp_aer( 8) = 'bsoa1'
        dens_mastercomp_aer( 8) = dens_oc_aer
        mw_mastercomp_aer(   8) =   mw_oc_aer
        hygro_mastercomp_aer(8) = hygro_oc_aer

        name_mastercomp_aer( 9) = 'bsoa2'
        dens_mastercomp_aer( 9) = dens_oc_aer
        mw_mastercomp_aer(   9) =   mw_oc_aer
        hygro_mastercomp_aer(9) = hygro_oc_aer

        name_mastercomp_aer( 10) = 'bsoa3'
        dens_mastercomp_aer( 10) = dens_oc_aer
        mw_mastercomp_aer(   10) =   mw_oc_aer
        hygro_mastercomp_aer(10) = hygro_oc_aer

        name_mastercomp_aer( 11) = 'bsoa4'
        dens_mastercomp_aer( 11) = dens_oc_aer
        mw_mastercomp_aer(   11) =   mw_oc_aer
        hygro_mastercomp_aer(11) = hygro_oc_aer

        name_mastercomp_aer( 12) = 'orgpa'
        dens_mastercomp_aer( 12) = dens_oc_aer
        mw_mastercomp_aer(   12) =   mw_oc_aer
        hygro_mastercomp_aer(12) = hygro_oc_aer

        name_mastercomp_aer( 13) = 'ec'
        dens_mastercomp_aer( 13) = dens_ec_aer
        mw_mastercomp_aer(   13) =   mw_ec_aer
        hygro_mastercomp_aer(13) = hygro_ec_aer

        name_mastercomp_aer( 14) = 'p25'
        dens_mastercomp_aer( 14) = dens_oin_aer
        mw_mastercomp_aer(   14) =   mw_oin_aer
        hygro_mastercomp_aer(14) = hygro_oin_aer

        name_mastercomp_aer( 15) = 'anth'
        dens_mastercomp_aer( 15) = dens_oin_aer
        mw_mastercomp_aer(   15) =   mw_oin_aer
        hygro_mastercomp_aer(15) = hygro_oin_aer

        name_mastercomp_aer( 16) = 'seas'
        dens_mastercomp_aer( 16) = dens_seas_aer
        mw_mastercomp_aer(   16) =   mw_seas_aer
        hygro_mastercomp_aer(16) = hygro_seas_aer

        name_mastercomp_aer( 17) = 'soil'
        dens_mastercomp_aer( 17) = dens_dust_aer
        mw_mastercomp_aer(   17) =  mw_dust_aer
        hygro_mastercomp_aer(17) = hygro_dust_aer

        name_mastercomp_aer(18)  = 'sodium'
        dens_mastercomp_aer(18)  = dens_na_aer
        mw_mastercomp_aer(  18)  =   mw_na_aer
        hygro_mastercomp_aer(18) = hygro_na_aer

        name_mastercomp_aer(19)  = 'chloride'
        dens_mastercomp_aer(19)  = dens_cl_aer
        mw_mastercomp_aer(  19)  =   mw_cl_aer
        hygro_mastercomp_aer(19) = hygro_cl_aer

        lptr_so4_aer(    :,:,:) = 1
        lptr_nh4_aer(    :,:,:) = 1
        lptr_no3_aer(    :,:,:) = 1
        lptr_na_aer(     :,:,:) = 1
        lptr_cl_aer(     :,:,:) = 1
        lptr_asoa1_aer(:,:,:) = 1
        lptr_asoa2_aer(:,:,:) = 1
        lptr_asoa3_aer( :,:,:) = 1
        lptr_asoa4_aer( :,:,:) = 1
        lptr_bsoa1_aer( :,:,:) = 1
        lptr_bsoa2_aer( :,:,:) = 1
        lptr_bsoa3_aer( :,:,:) = 1
        lptr_bsoa4_aer( :,:,:) = 1
        lptr_orgpa_aer(  :,:,:) = 1
        lptr_ec_aer(     :,:,:) = 1
        lptr_p25_aer(    :,:,:) = 1
        lptr_anth_aer(   :,:,:) = 1
        lptr_seas_aer(   :,:,:) = 1
        lptr_soil_aer(   :,:,:) = 1
        numptr_aer(      :,:,:) = 1

        do_cloudchem_aer(:,:) = .false.

! Aitken mode
        itype = 1
        isize = 1
        ncomp_aer(itype) = 16
        numptr_aer(      isize,itype,ai_phase) = p_nu0
        lptr_so4_aer(    isize,itype,ai_phase) = p_so4ai
        lptr_nh4_aer(    isize,itype,ai_phase) = p_nh4ai
        lptr_no3_aer(    isize,itype,ai_phase) = p_no3ai
        lptr_na_aer(     isize,itype,ai_phase) = p_naai
        lptr_cl_aer(     isize,itype,ai_phase) = p_clai
        lptr_asoa1_aer(  isize,itype,ai_phase) = p_asoa1i
        lptr_asoa2_aer(  isize,itype,ai_phase) = p_asoa2i
        lptr_asoa3_aer(  isize,itype,ai_phase) = p_asoa3i
        lptr_asoa4_aer(  isize,itype,ai_phase) = p_asoa4i
        lptr_bsoa1_aer(  isize,itype,ai_phase) = p_bsoa1i
        lptr_bsoa2_aer(  isize,itype,ai_phase) = p_bsoa2i
        lptr_bsoa3_aer(  isize,itype,ai_phase) = p_bsoa3i
        lptr_bsoa4_aer(  isize,itype,ai_phase) = p_bsoa4i
        lptr_orgpa_aer(  isize,itype,ai_phase) = p_orgpai
        lptr_ec_aer(     isize,itype,ai_phase) = p_eci
        lptr_p25_aer(    isize,itype,ai_phase) = p_p25i
! aerosol in cloud water
        if(cw_phase.gt.0)then
          numptr_aer(      isize,itype,cw_phase) = p_nu0cw
          lptr_so4_aer(    isize,itype,cw_phase) = p_so4cwi
          lptr_nh4_aer(    isize,itype,cw_phase) = p_nh4cwi
          lptr_no3_aer(    isize,itype,cw_phase) = p_no3cwi
          lptr_na_aer(     isize,itype,ai_phase) = p_nacwi
          lptr_cl_aer(     isize,itype,ai_phase) = p_clcwi
          lptr_asoa1_aer(  isize,itype,cw_phase) = p_asoa1cwi
          lptr_asoa2_aer(  isize,itype,cw_phase) = p_asoa2cwi
          lptr_asoa3_aer(  isize,itype,cw_phase) = p_asoa3cwi
          lptr_asoa4_aer(  isize,itype,cw_phase) = p_asoa4cwi
          lptr_bsoa1_aer(  isize,itype,cw_phase) = p_bsoa1cwi
          lptr_bsoa2_aer(  isize,itype,cw_phase) = p_bsoa2cwi
          lptr_bsoa3_aer(  isize,itype,cw_phase) = p_bsoa3cwi
          lptr_bsoa4_aer(  isize,itype,cw_phase) = p_bsoa4cwi
          lptr_orgpa_aer(  isize,itype,cw_phase) = p_orgpacwi
          lptr_ec_aer(     isize,itype,cw_phase) = p_eccwi
          lptr_p25_aer(    isize,itype,cw_phase) = p_p25cwi
          do_cloudchem_aer(isize,itype) = .true.
        endif

! Accumulation mode
        itype = 1
        isize = 2
        ncomp_aer(itype) = 16
        numptr_aer(      isize,itype,ai_phase) = p_ac0
        lptr_so4_aer(    isize,itype,ai_phase) = p_so4aj
        lptr_nh4_aer(    isize,itype,ai_phase) = p_nh4aj
        lptr_no3_aer(    isize,itype,ai_phase) = p_no3aj
        lptr_na_aer(     isize,itype,ai_phase) = p_naaj
        lptr_cl_aer(     isize,itype,ai_phase) = p_claj
        lptr_asoa1_aer(  isize,itype,ai_phase) = p_asoa1j
        lptr_asoa2_aer(  isize,itype,ai_phase) = p_asoa2j
        lptr_asoa3_aer(  isize,itype,ai_phase) = p_asoa3j
        lptr_asoa4_aer(  isize,itype,ai_phase) = p_asoa4j
        lptr_bsoa1_aer(  isize,itype,ai_phase) = p_bsoa1j
        lptr_bsoa2_aer(  isize,itype,ai_phase) = p_bsoa2j
        lptr_bsoa3_aer(  isize,itype,ai_phase) = p_bsoa3j
        lptr_bsoa4_aer(  isize,itype,ai_phase) = p_bsoa4j
        lptr_orgpa_aer(  isize,itype,ai_phase) = p_orgpaj
        lptr_ec_aer(     isize,itype,ai_phase) = p_ecj
        lptr_p25_aer(    isize,itype,ai_phase) = p_p25j
! aerosol in cloud water
        if(cw_phase.gt.0)then
          numptr_aer(      isize,itype,cw_phase) = p_ac0cw
          lptr_so4_aer(    isize,itype,cw_phase) = p_so4cwj
          lptr_nh4_aer(    isize,itype,cw_phase) = p_nh4cwj
          lptr_no3_aer(    isize,itype,cw_phase) = p_no3cwj
          lptr_na_aer(     isize,itype,ai_phase) = p_nacwj
          lptr_cl_aer(     isize,itype,ai_phase) = p_clcwj
          lptr_asoa1_aer(  isize,itype,cw_phase) = p_asoa1cwj
          lptr_asoa2_aer(  isize,itype,cw_phase) = p_asoa2cwj
          lptr_asoa3_aer(  isize,itype,cw_phase) = p_asoa3cwj
          lptr_asoa4_aer(  isize,itype,cw_phase) = p_asoa4cwj
          lptr_bsoa1_aer(  isize,itype,cw_phase) = p_bsoa1cwj
          lptr_bsoa2_aer(  isize,itype,cw_phase) = p_bsoa2cwj
          lptr_bsoa3_aer(  isize,itype,cw_phase) = p_bsoa3cwj
          lptr_bsoa4_aer(  isize,itype,cw_phase) = p_bsoa4cwj
          lptr_orgpa_aer(  isize,itype,cw_phase) = p_orgpacwj
          lptr_ec_aer(     isize,itype,cw_phase) = p_eccwj
          lptr_p25_aer(    isize,itype,cw_phase) = p_p25cwj
          do_cloudchem_aer(isize,itype) = .true.
        endif

! coarse mode
        itype = 2
        isize = 1
        ncomp_aer(itype) = 3
        numptr_aer(      isize,itype,ai_phase) = p_corn
        lptr_anth_aer(   isize,itype,ai_phase) = p_antha
        lptr_seas_aer(   isize,itype,ai_phase) = p_seas
        lptr_soil_aer(   isize,itype,ai_phase) = p_soila
! aerosol in cloud water
        if(cw_phase.gt.0)then
          numptr_aer(      isize,itype,cw_phase) = p_corncw
          lptr_anth_aer(   isize,itype,cw_phase) = p_anthcw
          lptr_seas_aer(   isize,itype,cw_phase) = p_seascw
          lptr_soil_aer(   isize,itype,cw_phase) = p_soilcw
! no cloudchem for coarse mode because it has no so4/nh4/no3 species
          do_cloudchem_aer(isize,itype) = .false.
        endif

        massptr_aer(:,:,:,:) = -999888777
        mastercompptr_aer(:,:) = -999888777

        p1st = param_first_scalar

        do iphase=1,nphase_aer
        do itype=1,ntype_aer
        do n = 1, nsize_aer(itype)
            ll = 0
            if (lptr_so4_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_so4_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 1
            end if
            if (lptr_no3_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_no3_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 2
            end if
            if (lptr_nh4_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_nh4_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 3
            end if
            if (lptr_asoa1_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_asoa1_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 4
            end if
            if (lptr_asoa2_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_asoa2_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 5
            end if
            if (lptr_asoa3_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_asoa3_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 6
            end if
            if (lptr_asoa4_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_asoa4_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 7
            end if
            if (lptr_bsoa1_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_bsoa1_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 8
            end if
            if (lptr_bsoa2_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_bsoa2_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 9
            end if
            if (lptr_bsoa3_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_bsoa3_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 10
            end if
            if (lptr_bsoa4_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
            massptr_aer(ll,n,itype,iphase) = lptr_bsoa4_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 11
            end if
            if (lptr_orgpa_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_orgpa_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 12
            end if
            if (lptr_ec_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_ec_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 13
            end if
            if (lptr_p25_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_p25_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 14
            end if
            if (lptr_anth_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_anth_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 15
            end if
            if (lptr_seas_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_seas_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 16
            end if
            if (lptr_soil_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_soil_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 17
            end if
            if (lptr_na_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_na_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 18
            end if
            if (lptr_cl_aer(n,itype,iphase) .ge. p1st) then
                ll = ll + 1
                massptr_aer(ll,n,itype,iphase) = lptr_cl_aer(n,itype,iphase)
                mastercompptr_aer(ll,itype) = 19
            endif
            ncomp_aer_nontracer(itype) = ll

            ncomp_aer(itype) = ll

            mprognum_aer(n,itype,iphase) = 0
            if (numptr_aer(n,itype,iphase) .ge. p1st) then
                mprognum_aer(n,itype,iphase) = 1
            end if

        end do ! size
        end do ! type
        end do ! phase

        waterptr_aer(:,:) = 0

        do itype=1,ntype_aer
        do ll=1,ncomp_aer(itype)
           dens_aer(ll,itype) = dens_mastercomp_aer(mastercompptr_aer(ll,itype))
           mw_aer(ll,itype) = mw_mastercomp_aer(mastercompptr_aer(ll,itype))
           hygro_aer(ll,itype) = hygro_mastercomp_aer(mastercompptr_aer(ll,itype))
           name_aer(ll,itype) = name_mastercomp_aer(mastercompptr_aer(ll,itype))
        end do
        end do

        is_aerosol(:) = .false.
        do iphase=1,nphase_aer
        do itype=1,ntype_aer
        do n = 1, nsize_aer(itype)
            do ll = 1, ncomp_aer(itype)
              is_aerosol(massptr_aer(ll,n,itype,iphase))=.true.
            end do
        is_aerosol(numptr_aer(n,itype,iphase))=.true.
        end do ! size
        end do ! type
        end do ! phase

! for sectional
!    the dhi/dlo_sect are the upper/lower bounds for 
!       mean-volume diameter for a section/bin
! for modal
!    they should be set to reasonable upper/lower
!       bounds for mean-volume diameters of each modes
!    they are primarily used to put reasonable bounds
!       on number (in relation to mass/volume)
!    the dcen_sect are used by initwet for the impaction scavenging
!       lookup tables, and should represent a "base" mean-volume diameter
! dp_meanvol_tmp (below) is the made-sorgam default initial value 
!    for mean-volume diameter (in cm)
! terminology:  (pi/6) * (mean-volume diameter)**3 ==
!       (volume mixing ratio of section/mode)/(number mixing ratio)
!
        dhi_sect(:,:) = 0.0
        dlo_sect(:,:) = 0.0

        itype = 1
        isize = 1
        sigmag_aer(isize,itype) = sginin ! aitken
        dp_meanvol_tmp = 1.0e2*dginin*exp(1.5*l2sginin) ! aitken
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        itype = 1
        isize = 2
        sigmag_aer(isize,itype) = sginia ! accum
        dp_meanvol_tmp = 1.0e2*dginia*exp(1.5*l2sginia) ! accum 
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        itype = 2
        isize = 1
        sigmag_aer(isize,itype) = sginic ! coarse
        dp_meanvol_tmp = 1.0e2*dginic*exp(1.5*l2sginic) ! coarse
        dcen_sect(isize,itype) = dp_meanvol_tmp
        dhi_sect(isize,itype)  = dp_meanvol_tmp*4.0
        dlo_sect(isize,itype)  = dp_meanvol_tmp/4.0

        do itype = 1, ntype_aer
        do isize = 1, nsize_aer(itype)
           volumcen_sect(isize,itype) = (pirs/6.0)*(dcen_sect(isize,itype)**3)
           volumlo_sect(isize,itype)  = (pirs/6.0)*(dlo_sect(isize,itype)**3)
           volumhi_sect(isize,itype)  = (pirs/6.0)*(dhi_sect(isize,itype)**3)
        end do
        end do

! do initialization of the impaction/interception scavenging 
! lookup tables
        call initwet(   &
            ntype_aer, nsize_aer, ncomp_aer,   &
            massptr_aer, dens_aer, numptr_aer,           &
            maxd_acomp, maxd_asize,maxd_atype, maxd_aphase,   &
            dcen_sect, sigmag_aer, &
            waterptr_aer, dens_water_aer, &
            scavimptblvol, scavimptblnum, nimptblgrow_mind,   &
            nimptblgrow_maxd, dlndg_nimptblgrow )

    END SUBROUTINE aerosols_soa_vbs_init_aercld_ptrs
 !===========================================================================
   subroutine wetscav_sorgam_driver (id,ktau,dtstep,ktauc,config_flags,      &
               dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,        &
               qlsink,precr,preci,precs,precg,qsrflx,                      &
               gas_aqfrac, numgas_aqfrac,                                  &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )

!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction

!----------------------------------------------------------------------
   !USE module_configure
   !USE module_state_description
   USE module_data_sorgam
   !USE module_mosaic_wetscav,only:  wetscav

!----------------------------------------------------------------------
   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                                &
                                      ids,ide, jds,jde, kds,kde,    &
                                      ims,ime, jms,jme, kms,kme,    &
                                      its,ite, jts,jte, kts,kte,    &
                                      id, ktau, ktauc, numgas_aqfrac
      REAL,      INTENT(IN   ) :: dtstep,dtstepc
!
! all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),          &
         INTENT(INOUT ) ::                                chem

! fraction of gas species in cloud water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, numgas_aqfrac ),     &
         INTENT(IN ) ::                                   gas_aqfrac

!
!
! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(IN   ) ::                                          &
                                                        alt,        &
                                                      t_phy,        &
                                                      p_phy,        &
                                                   t8w,p8w,         &
                                    qlsink,precr,preci,precs,precg, &
                                                    rho_phy,cldfra
   REAL, DIMENSION( ims:ime, jms:jme, num_chem ),          &
         INTENT(OUT ) ::                                qsrflx ! column change due to scavening

   call wetscav (id,ktau,dtstep,ktauc,config_flags,                     &
        dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,            &
        qlsink,precr,preci,precs,precg,qsrflx,                          &
        gas_aqfrac, numgas_aqfrac,                                      &
        ntype_aer, nsize_aer, ncomp_aer,                                &
        massptr_aer, dens_aer, numptr_aer,                              &
        maxd_acomp, maxd_asize,maxd_atype, maxd_aphase, ai_phase, cw_phase, &
        volumcen_sect, volumlo_sect, volumhi_sect,                      &
        waterptr_aer, dens_water_aer,                                   &
        scavimptblvol, scavimptblnum, nimptblgrow_mind, nimptblgrow_maxd,dlndg_nimptblgrow, &
        ids,ide, jds,jde, kds,kde,                                      &
        ims,ime, jms,jme, kms,kme,                                      &
        its,ite, jts,jte, kts,kte                                       )

   end subroutine wetscav_sorgam_driver   
                                              
 subroutine wetscav_soa_vbs_driver (id,ktau,dtstep,ktauc,config_flags,      &
               dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,        &
               qlsink,precr,preci,precs,precg,qsrflx,                      &
               gas_aqfrac, numgas_aqfrac,                                  &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )

!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction

!----------------------------------------------------------------------
   !USE module_configure
   !USE module_state_description
   USE module_data_soa_vbs
   !USE module_mosaic_wetscav,only:  wetscav

!----------------------------------------------------------------------
   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                                &
                                      ids,ide, jds,jde, kds,kde,    &
                                      ims,ime, jms,jme, kms,kme,    &
                                      its,ite, jts,jte, kts,kte,    &
                                      id, ktau, ktauc, numgas_aqfrac
      REAL,      INTENT(IN   ) :: dtstep,dtstepc

! all advected chemical species
REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),          &
         INTENT(INOUT ) ::                                chem

! fraction of gas species in cloud water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, numgas_aqfrac ),     &
         INTENT(IN ) ::                                   gas_aqfrac

!
!
! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
 INTENT(IN   ) ::                                                   &
                                                        alt,        &
                                                      t_phy,        &
                                                      p_phy,        &
                                                   t8w,p8w,         &
                                    qlsink,precr,preci,precs,precg, &
                                                    rho_phy,cldfra
   REAL, DIMENSION( ims:ime, jms:jme, num_chem ),          &
         INTENT(OUT ) ::                                qsrflx ! column change due to scavening

   call wetscav (id,ktau,dtstep,ktauc,config_flags,                     &
        dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,            &
        qlsink,precr,preci,precs,precg,qsrflx,                          &
        gas_aqfrac, numgas_aqfrac,                                      &
        ntype_aer, nsize_aer, ncomp_aer,                                &
        massptr_aer, dens_aer, numptr_aer,                              &
        maxd_acomp, maxd_asize,maxd_atype, maxd_aphase, ai_phase, cw_phase, &
        volumcen_sect, volumlo_sect, volumhi_sect,                      &
        waterptr_aer, dens_water_aer,                                   &
        scavimptblvol, scavimptblnum, nimptblgrow_mind, nimptblgrow_maxd,dlndg_nimptblgrow, &
        ids,ide, jds,jde, kds,kde,                                      &
        ims,ime, jms,jme, kms,kme,                                      &
        its,ite, jts,jte, kts,kte                                       )

   end subroutine wetscav_soa_vbs_driver
 
END MODULE module_prep_wetscav_sorgam 
