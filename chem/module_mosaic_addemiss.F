!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for information and terms of use
!**********************************************************************************  
MODULE module_mosaic_addemiss
!WRF:MODEL_LAYER:CHEMICS

! rce 2005-feb-18 - one fix for indices of volumcen_sect, [now (isize,itype)]
! rce 2005-jan-14 - added subr mosaic_seasalt_emiss (and a call to it)
! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect



   integer, parameter :: mosaic_addemiss_active = 1
                       ! only do emissions when this is positive
                       ! (when it is negative, emissions tendencies are zero)

   integer, parameter :: mosaic_addemiss_masscheck = -1
                       ! only do emissions masscheck calcs when this is positive

   integer, parameter :: mosaic_seasalt_emiss_active = 1
                       ! only do seasalt emissions when this is positive


CONTAINS



!----------------------------------------------------------------------
   subroutine mosaic_addemiss( id, dtstep, u10, v10, alt, dz8w, xland,     &
               config_flags, chem,                                         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,    &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                   &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!
! adds emissions for mosaic aerosol species
! (i.e., emissions tendencies over time dtstep are applied 
! to the aerosol concentrations)
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description, only:  num_chem, param_first_scalar,   &
      emiss_inpt_default, emiss_inpt_pnnl_rs, emiss_inpt_pnnl_cm
   USE module_data_mosaic_asect

   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme )         ,                         &
          INTENT(IN   ) ::   u10, v10, xland

! trace species mixing ratios (aerosol mass = ug/kg-air; number = #/kg-air)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem
!
! aerosol emissions arrays ((ug/m3)*m/s)
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                          &
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme ),            &
         INTENT(IN ) ::                                                    &
          e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,         &
          e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc

! 1/(dry air density) and layer thickness (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   alt, dz8w

! local variables
   integer i, j, k, l, n
   integer iphase, itype
   integer p1st

   real, parameter :: efact1 = 1.0
   real :: aem_so4, aem_no3, aem_cl, aem_msa, aem_co3, aem_nh4,   &
           aem_na, aem_ca, aem_oin, aem_oc, aem_bc, aem_num
   real dum, fact


! fraction of sorgam i/aitken mode emissions that go to each
! of the mosaic 8 "standard" sections
   real, save :: fr8b_aem_sorgam_i(8) =   &
                 (/ 0.965,  0.035,  0.000,  0.000,   &
                    0.000,  0.000,  0.000,  0.000 /)

! fraction of sorgam j/accum mode emissions that go to each
! of the mosaic 8 "standard" sections
   real, save :: fr8b_aem_sorgam_j(8) =   &
                 (/ 0.026,  0.147,  0.350,  0.332,   &
                    0.125,  0.019,  0.001,  0.000/)

! fraction of sorgam coarse mode emissions that go to each
! of the mosaic 8 "standard" sections
   real, save :: fr8b_aem_sorgam_c(8) =   &
                 (/ 0.000,  0.000,  0.000,  0.002,   &
                    0.021,  0.110,  0.275,  0.592 /)

! fraction of mosaic fine (< 2.5 um) emissions that go to each
! of the mosaic 8 "standard" sections
!wig 1-Apr-2005,  Updated fractional breakdown between bins. (See also
!                 bdy_chem_value_mosaic and mosaic_init_wrf_mixrats_opt2
!                 in module_mosaic_initmixrats.F.) Note that the values
!                 here no longer match the other two subroutines.
!rce 10-may-2005, changed fr8b_aem_mosaic_f & fr8b_aem_mosaic_c
!                 to values determined by jdf
   real, save :: fr8b_aem_mosaic_f(8) =   &
       (/ 0.060, 0.045, 0.245, 0.400, 0.100, 0.150, 0., 0./) !10-may-2005
!      (/ 0.100, 0.045, 0.230, 0.375, 0.100, 0.150, 0., 0./) !1-Apr-2005 values
!      (/ 0.0275, 0.0426, 0.2303, 0.3885, 0.1100, 0.2011, 0., 0./) !15-Nov-2004 values
!      (/ 0.01, 0.05, 0.145, 0.60, 0.145, 0.05, 0.00, 0.00 /)
!      (/ 0.04, 0.10, 0.35, 0.29, 0.15, 0.07, 0.0,  0.0  /)

! fraction of mosaic coarse (> 2.5 um) emissions that go to each
! of the mosaic 8 "standard" sections
   real, save :: fr8b_aem_mosaic_c(8) =   &
       (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.300, 0.700 /) ! 10-may-2005
!      (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.933, 0.067 /) ! as of apr-2005
!      (/ 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.16,  0.84  /) !  "old"

! following 5 arrays correspond to the above "fr8b_" arrays 
! but are set at run time base on input (namelist) parameters:
!    only the sorgam or mosaic arrays are non-zero, depending on
!       emiss_inpt_opt
!    when nsize_aer=4 (=number of sections), the values are
!	calculated by adding two of the 8-section values
   real :: fr_aem_sorgam_i(8)
   real :: fr_aem_sorgam_j(8)
   real :: fr_aem_sorgam_c(8)
   real :: fr_aem_mosaic_f(8)
   real :: fr_aem_mosaic_c(8)
   double precision :: chem_sum(num_chem)

   character*80 msg


!   *** currently only works with ntype_aer = 1
	itype = 1
	iphase = ai_phase


!
! compute factors used for apportioning either 
!    the MADE-SORGAM emissions (i=aitken, j=accum, c=coarse modes) OR
!    the MOSAIC emission  (f=fine (< 2.5 um), c=coarse (> 2.5 um))
! to each size section
!
! note:  the fr8b_aer_xxxxxx_y values are specific to the mosaic 8 bin 
!    structure with dlo_sect(1)=0.039 um and dhi_sect(8)=10.0 um,
! also, the fr8b_aem_sorgam_y are specific for the assumed 
!    dgvem_i/j/c used in the sorgam code
! also, the fr8b_aem_mosaic_y values are specific for the assumed (by us)
!    size distribution for fine and coarse primary emissions
!
! when there are 4 bins (nsize_aer=4), each of these "wider" bins 
!    corresponds to 2 of the "narrower" bins of the 8 bin structure
!
! note:  if fr_aem_sorgam_y > 0, then fr_aem_mosaic_y = 0, and vice-versa
!
	if ((nsize_aer(itype) .ne. 4) .and. (nsize_aer(itype) .ne. 8)) then
	    write(msg,'(a,i5)')   &
		'subr mosaic_addemiss - nsize_aer(itype) must be ' //   &
		'4 or 8 but = ',   &
		 nsize_aer(itype)
	    call wrf_error_fatal( msg )
	end if

	fr_aem_sorgam_i(:) = 0.0
	fr_aem_sorgam_j(:) = 0.0
	fr_aem_sorgam_c(:) = 0.0
	fr_aem_mosaic_f(:) = 0.0
	fr_aem_mosaic_c(:) = 0.0

	emiss_inpt_select_1: SELECT CASE( config_flags%emiss_inpt_opt )

	  CASE( emiss_inpt_default, emiss_inpt_pnnl_rs )
	    if (nsize_aer(itype) .eq. 8) then
		fr_aem_sorgam_i(:) = fr8b_aem_sorgam_i(:)
		fr_aem_sorgam_j(:) = fr8b_aem_sorgam_j(:)
		fr_aem_sorgam_c(:) = fr8b_aem_sorgam_c(:)
	    else if (nsize_aer(itype) .eq. 4) then
		do n = 1, nsize_aer(itype)
		    fr_aem_sorgam_i(n) = fr8b_aem_sorgam_i(2*n-1)   &
		                       + fr8b_aem_sorgam_i(2*n)
		    fr_aem_sorgam_j(n) = fr8b_aem_sorgam_j(2*n-1)   &
		                       + fr8b_aem_sorgam_j(2*n)
		    fr_aem_sorgam_c(n) = fr8b_aem_sorgam_c(2*n-1)   &
		                       + fr8b_aem_sorgam_c(2*n)
		end do
	    end if

	  CASE( emiss_inpt_pnnl_cm )
	    if (nsize_aer(itype) .eq. 8) then
		fr_aem_mosaic_f(:) = fr8b_aem_mosaic_f(:)
		fr_aem_mosaic_c(:) = fr8b_aem_mosaic_c(:)
	    else if (nsize_aer(itype) .eq. 4) then
		do n = 1, nsize_aer(itype)
		    fr_aem_mosaic_f(n) = fr8b_aem_mosaic_f(2*n-1)   &
		                       + fr8b_aem_mosaic_f(2*n)
		    fr_aem_mosaic_c(n) = fr8b_aem_mosaic_c(2*n-1)   &
		                       + fr8b_aem_mosaic_c(2*n)
		end do
	    end if

	  CASE DEFAULT
	    return

	END SELECT emiss_inpt_select_1

! when mosaic_addemiss_active <= 0, set fr's to zero,
! which causes the changes to chem(...) to be zero
	if (mosaic_addemiss_active <= 0) then
	    fr_aem_sorgam_i(:) = 0.0
	    fr_aem_sorgam_j(:) = 0.0
	    fr_aem_sorgam_c(:) = 0.0
	    fr_aem_mosaic_f(:) = 0.0
	    fr_aem_mosaic_c(:) = 0.0
	end if


! do mass check initial calc
	if (mosaic_addemiss_masscheck > 0) call addemiss_masscheck(            &
               id, config_flags, 1, 'mosaic_ademiss',                      &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               14,                                                         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,    &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                   &
               e_ecc,e_ecc,e_ecc,e_ecc,e_ecc,e_ecc,e_ecc                   )

	p1st = param_first_scalar

!       
! apply emissions at each section and grid point
!
	do 1900 n = 1, nsize_aer(itype)

	do 1830 j = jts, jte
	do 1820 k = kts, min(config_flags%kemit,kte-1)
	do 1810 i = its, ite

! compute mass emissions [(ug/m3)*m/s] for each species
! using the apportioning fractions
	aem_so4 = fr_aem_mosaic_f(n)*e_so4j(i,k,j)   &
	        + fr_aem_mosaic_c(n)*e_so4c(i,k,j)

	aem_no3 = fr_aem_mosaic_f(n)*e_no3j(i,k,j)   &
	        + fr_aem_mosaic_c(n)*e_no3c(i,k,j)

	aem_oc  = fr_aem_mosaic_f(n)*e_orgj(i,k,j)   &
	        + fr_aem_mosaic_c(n)*e_orgc(i,k,j)   &
	        + fr_aem_sorgam_i(n)*e_orgi(i,k,j)   &
	        + fr_aem_sorgam_j(n)*e_orgj(i,k,j)

	aem_bc  = fr_aem_mosaic_f(n)*e_ecj(i,k,j)    &
	        + fr_aem_mosaic_c(n)*e_ecc(i,k,j)    &
	        + fr_aem_sorgam_i(n)*e_eci(i,k,j)    &
	        + fr_aem_sorgam_j(n)*e_ecj(i,k,j)

	aem_oin = fr_aem_mosaic_f(n)*e_pm25j(i,k,j)  &
	        + fr_aem_mosaic_c(n)*e_pm10(i,k,j)   &
	        + fr_aem_sorgam_i(n)*e_pm25i(i,k,j)  &
	        + fr_aem_sorgam_j(n)*e_pm25j(i,k,j)  &
	        + fr_aem_sorgam_c(n)*e_pm10(i,k,j)

! emissions for these species are currently zero
	aem_nh4 = 0.0
	aem_na  = 0.0
	aem_cl  = 0.0
	aem_ca  = 0.0
	aem_co3 = 0.0
	aem_msa = 0.0

! compute number emissions
! first sum the mass-emissions/density
	aem_num =   &
	(aem_so4/dens_so4_aer) + (aem_no3/dens_no3_aer) +   &
	(aem_cl /dens_cl_aer ) + (aem_msa/dens_msa_aer) +   &
	(aem_co3/dens_co3_aer) + (aem_nh4/dens_nh4_aer) +   &
	(aem_na /dens_na_aer ) + (aem_ca /dens_ca_aer ) +   &
	(aem_oin/dens_oin_aer) + (aem_oc /dens_oc_aer ) +   &
	(aem_bc /dens_bc_aer )

! then multiply by 1.0e-6 to convert ug to g
! and divide by particle volume at center of section (cm3)
	aem_num = aem_num*1.0e-6/volumcen_sect(n,itype)

! apply the emissions and convert from flux to mixing ratio
!	fact = (dtstep/dz8w(i,k,j))*(28.966/1000.)
	fact = (dtstep/dz8w(i,k,j))*alt(i,k,j)

! rce 22-nov-2004 - change to using the "..._aer" species pointers
	l = lptr_so4_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_so4*fact

	l = lptr_no3_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_no3*fact

	l = lptr_cl_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_cl*fact

	l = lptr_msa_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_msa*fact

	l = lptr_co3_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_co3*fact

	l = lptr_nh4_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_nh4*fact

	l = lptr_na_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_na*fact

	l = lptr_ca_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_ca*fact

	l = lptr_oin_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_oin*fact

	l = lptr_oc_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_oc*fact

	l = lptr_bc_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_bc*fact

	l = numptr_aer(n,itype,iphase)
	if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) + aem_num*fact

1810	continue
1820	continue
1830	continue

1900	continue


! do mass check final calc
	if (mosaic_addemiss_masscheck > 0) call addemiss_masscheck(        &
               id, config_flags, 2, 'mosaic_ademiss',                      &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               14,                                                         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,    &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                   &
               e_ecc,e_ecc,e_ecc,e_ecc,e_ecc,e_ecc,e_ecc                   )


! do seasalt emissions
	if (mosaic_seasalt_emiss_active > 0)   &
	    call mosaic_seasalt_emiss(                                     &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )


	return


   END subroutine mosaic_addemiss



!----------------------------------------------------------------------
   subroutine mosaic_seasalt_emiss(                                        &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!
! adds seasalt emissions for mosaic aerosol species
! (i.e., seasalt emissions tendencies over time dtstep are applied 
! to the aerosol mixing ratios)
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description, only:  num_chem, param_first_scalar
   USE module_data_mosaic_asect

   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme ),                                  &
          INTENT(IN   ) ::   u10, v10, xland

! trace species mixing ratios (aerosol mass = ug/kg; number = #/kg)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem

! alt  = 1.0/(dry air density) in (m3/kg)
! dz8w = layer thickness in (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   alt, dz8w

! local variables
	integer i, j, k, l, l_na, l_cl, n
	integer iphase, itype
	integer p1st

	real dum, dumdlo, dumdhi, dumoceanfrac, dumspd10
	real factaa, factbb, fracna, fraccl

	real :: ssemfact_numb( maxd_asize, maxd_atype )
	real :: ssemfact_mass( maxd_asize, maxd_atype )


!   for now just do itype=1
	itype = 1
	iphase = ai_phase

!   compute emissions factors for each size bin
!   (limit emissions to dp > 0.1 micrometer)
	do n = 1, nsize_aer(itype)
	    dumdlo = max( dlo_sect(n,itype), 0.1e-4 )
	    dumdhi = max( dhi_sect(n,itype), 0.1e-4 )
	    call seasalt_emitfactors_1bin( 1, dumdlo, dumdhi,   &
		ssemfact_numb(n,itype), dum, ssemfact_mass(n,itype) )

!   convert mass emissions factor from (g/m2/s) to (ug/m2/s)
	    ssemfact_mass(n,itype) = ssemfact_mass(n,itype)*1.0e6
	end do


!   loop over i,j and apply seasalt emissions
	k = kts
	do 1830 j = jts, jte
	do 1820 i = its, ite

    !Skip this point if over land. xland=1 for land and 2 for water.
    !Also, there is no way to differentiate fresh from salt water.
    !Currently, this assumes all water is salty.
    if( xland(i,j) < 1.5 ) cycle

    !wig: As far as I can tell, only real.exe knows the fractional breakdown
    !     of land use. So, in wrf.exe, dumoceanfrac will always be 1.
	dumoceanfrac = 1. !fraction of grid i,j that is salt water
	dumspd10 = dumoceanfrac* &
         ( (u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j))**(0.5*3.41) )

!   factaa is (s*m2/kg-air)
!   factaa*ssemfact_mass(n) is (s*m2/kg-air)*(ug/m2/s) = ug/kg-air
!   factaa*ssemfact_numb(n) is (s*m2/kg-air)*( #/m2/s) =  #/kg-air
	factaa = (dtstep/dz8w(i,k,j))*alt(i,k,j)

	factbb = factaa * dumspd10

!   apportion seasalt mass emissions assumming that seasalt is pure nacl
	fracna = mw_na_aer / (mw_na_aer + mw_cl_aer)
	fraccl = 1.0 - fracna

	do 1810 n = 1, nsize_aer(itype)

!   only apply emissions if bin has both na and cl species
	    l_na = lptr_na_aer(n,itype,iphase)
	    l_cl = lptr_cl_aer(n,itype,iphase)
	    if ((l_na >= p1st) .and. (l_cl >= p1st)) then

	    chem(i,k,j,l_na) = chem(i,k,j,l_na) +   &
			factbb * ssemfact_mass(n,itype) * fracna

	    chem(i,k,j,l_cl) = chem(i,k,j,l_cl) +   &
			factbb * ssemfact_mass(n,itype) * fraccl

	    l = numptr_aer(n,itype,iphase)
	    if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) +   &
			factbb * ssemfact_numb(n,itype)

	    end if
1810	continue

1820	continue
1830	continue

	return

   END subroutine mosaic_seasalt_emiss



!c----------------------------------------------------------------------
!c   following is from gong06b.f in
!c	/net/cirrus/files1/home/rce/oldfiles1/box/seasaltg
!c----------------------------------------------------------------------
	subroutine seasalt_emitfactors_1bin( ireduce_smallr_emit,	&
      		dpdrylo_cm, dpdryhi_cm,	  &
                emitfact_numb, emitfact_surf, emitfact_mass )
!c
!c   computes seasalt emissions factors for a specifed 
!c   dry particle size range
!c	dpdrylo_cm  = lower dry diameter (cm)
!c	dpdryhi_cm  = upper dry diameter (cm)
!c
!c   number and mass emissions are then computed as
!c	number   emissions (#/m2/s)   == emitfact_numb * (spd10*3.41)
!c	dry-sfc  emissions (cm2/m2/s) == emitfact_surf * (spd10*3.41)
!c	dry-mass emissions (g/m2/s)   == emitfact_mass * (spd10*3.41)
!c
!c   where spd10 = 10 m windspeed in m/s
!c
!c   uses bubble emissions formula (eqn 5a) from 
!c	Gong et al. [JGR, 1997, p 3805-3818]
!c
!c   *** for rdry < rdry_star, this formula overpredicts emissions.
!c	A strictly ad hoc correction is applied to the formula,
!c	based on sea-salt size measurements of
!c	O'Dowd et al. [Atmos Environ, 1997, p 73-80]
!c
!c   *** the correction is only applied when ireduce_smallr_emit > 0
!c
	implicit none

!c   subr arguments
	integer ireduce_smallr_emit
	real dpdrylo_cm, dpdryhi_cm,				&
                emitfact_numb, emitfact_surf, emitfact_mass

!c   local variables
	integer isub_bin, nsub_bin

	real alnrdrylo
	real drydens, drydens_43pi_em12, x_4pi_em8
	real dum, dumadjust, dumb, dumexpb
	real dumsum_na, dumsum_ma, dumsum_sa
	real drwet, dlnrdry
	real df0drwet, df0dlnrdry, df0dlnrdry_star
	real relhum
	real rdry, rdrylo, rdryhi, rdryaa, rdrybb
	real rdrylowermost, rdryuppermost, rdry_star
	real rwet, rwetaa, rwetbb
	real rdry_cm, rwet_cm
	real sigmag_star
	real xmdry, xsdry

	real pi
	parameter (pi = 3.1415936536)

!c   c1-c4 are constants for seasalt hygroscopic growth parameterization
!c   in Eqn 3 and Table 2 of Gong et al. [1997]
	real c1, c2, c3, c4, onethird
	parameter (c1 = 0.7674)
	parameter (c2 = 3.079)
	parameter (c3 = 2.573e-11)
	parameter (c4 = -1.424)
	parameter (onethird = 1.0/3.0)


!c   dry particle density (g/cm3)
	drydens = 2.165
!c   factor for radius (micrometers) to mass (g)
	drydens_43pi_em12 = drydens*(4.0/3.0)*pi*1.0e-12
!c   factor for radius (micrometers) to surface (cm2)
	x_4pi_em8 = 4.0*pi*1.0e-8
!c   bubble emissions formula assume 80% RH
	relhum = 0.80

!c   rdry_star = dry radius (micrometers) below which the
!c   dF0/dr emission formula is adjusted downwards
	rdry_star = 0.1
	if (ireduce_smallr_emit .le. 0) rdry_star = -1.0e20
!c   sigmag_star = geometric standard deviation used for
!c   rdry < rdry_star
	sigmag_star = 1.9

!c   initialize sums
	dumsum_na = 0.0
	dumsum_sa = 0.0
	dumsum_ma = 0.0

!c   rdrylowermost, rdryuppermost = lower and upper 
!c   dry radii (micrometers) for overall integration
        rdrylowermost = dpdrylo_cm*0.5e4
        rdryuppermost = dpdryhi_cm*0.5e4

!c
!c   "section 1"
!c   integrate over rdry > rdry_star, where the dF0/dr emissions 
!c   formula is applicable
!c   (when ireduce_smallr_emit <= 0, rdry_star = -1.0e20,
!c   and the entire integration is done here)
!c
	if (rdryuppermost .le. rdry_star) goto 2000

!c   rdrylo, rdryhi = lower and upper dry radii (micrometers) 
!c   for this part of the integration
        rdrylo = max( rdrylowermost, rdry_star )
        rdryhi = rdryuppermost

	nsub_bin = 1000

	alnrdrylo = log( rdrylo )
	dlnrdry = (log( rdryhi ) - alnrdrylo)/nsub_bin

!c   compute rdry, rwet (micrometers) at lowest size
	rdrybb = exp( alnrdrylo )
	rdry_cm = rdrybb*1.0e-4
	rwet_cm = ( rdry_cm**3 + (c1*(rdry_cm**c2))/		&
      		( (c3*(rdry_cm**c4)) - log10(relhum) ) )**onethird
	rwetbb = rwet_cm*1.0e4

	do 1900 isub_bin = 1, nsub_bin

!c   rdry, rwet at sub_bin lower boundary are those
!c   at upper boundary of previous sub_bin
	rdryaa = rdrybb
	rwetaa = rwetbb

!c   compute rdry, rwet (micrometers) at sub_bin upper boundary
	dum = alnrdrylo + isub_bin*dlnrdry
	rdrybb = exp( dum )

	rdry_cm = rdrybb*1.0e-4
	rwet_cm = ( rdry_cm**3 + (c1*(rdry_cm**c2))/		&
      		( (c3*(rdry_cm**c4)) - log10(relhum) ) )**onethird
	rwetbb = rwet_cm*1.0e4

!c   geometric mean rdry, rwet (micrometers) for sub_bin
	rdry = sqrt(rdryaa * rdrybb)
	rwet = sqrt(rwetaa * rwetbb)
	drwet = rwetbb - rwetaa

!c   xmdry is dry mass in g
	xmdry = drydens_43pi_em12 * (rdry**3.0)

!c   xsdry is dry surface in cm2
	xsdry = x_4pi_em8 * (rdry**2.0)

!c   dumb is "B" in Gong's Eqn 5a
!c   df0drwet is "dF0/dr" in Gong's Eqn 5a
	dumb = ( 0.380 - log10(rwet) ) / 0.650
	dumexpb = exp( -dumb*dumb)
	df0drwet = 1.373 * (rwet**(-3.0)) * 			&
      		(1.0 + 0.057*(rwet**1.05)) * 			&
      		(10.0**(1.19*dumexpb))

	dumsum_na = dumsum_na + drwet*df0drwet
	dumsum_ma = dumsum_ma + drwet*df0drwet*xmdry
	dumsum_sa = dumsum_sa + drwet*df0drwet*xsdry

1900	continue


!c
!c   "section 2"
!c   integrate over rdry < rdry_star, where the dF0/dr emissions 
!c   formula is just an extrapolation and predicts too many emissions
!c
!c   1.  compute dF0/dln(rdry) = (dF0/drwet)*(drwet/dlnrdry) 
!c	at rdry_star
!c   2.  for rdry < rdry_star, assume dF0/dln(rdry) is lognormal,
!c	with the same lognormal parameters observed in 
!c	O'Dowd et al. [1997]
!c
2000	if (rdrylowermost .ge. rdry_star) goto 3000

!c   compute dF0/dln(rdry) at rdry_star
	rdryaa = 0.99*rdry_star
	rdry_cm = rdryaa*1.0e-4
	rwet_cm = ( rdry_cm**3 + (c1*(rdry_cm**c2))/		&
      		( (c3*(rdry_cm**c4)) - log10(relhum) ) )**onethird
	rwetaa = rwet_cm*1.0e4

	rdrybb = 1.01*rdry_star
	rdry_cm = rdrybb*1.0e-4
	rwet_cm = ( rdry_cm**3 + (c1*(rdry_cm**c2))/		&
      		( (c3*(rdry_cm**c4)) - log10(relhum) ) )**onethird
	rwetbb = rwet_cm*1.0e4

	rwet = 0.5*(rwetaa + rwetbb)
	dumb = ( 0.380 - log10(rwet) ) / 0.650
	dumexpb = exp( -dumb*dumb)
	df0drwet = 1.373 * (rwet**(-3.0)) * 			&
      		(1.0 + 0.057*(rwet**1.05)) * 			&
      		(10.0**(1.19*dumexpb))

	drwet = rwetbb - rwetaa
	dlnrdry = log( rdrybb/rdryaa )
	df0dlnrdry_star = df0drwet * (drwet/dlnrdry)


!c   rdrylo, rdryhi = lower and upper dry radii (micrometers) 
!c   for this part of the integration
        rdrylo = rdrylowermost
        rdryhi = min( rdryuppermost, rdry_star )

	nsub_bin = 1000

	alnrdrylo = log( rdrylo )
	dlnrdry = (log( rdryhi ) - alnrdrylo)/nsub_bin

	do 2900 isub_bin = 1, nsub_bin

!c   geometric mean rdry (micrometers) for sub_bin
	dum = alnrdrylo + (isub_bin-0.5)*dlnrdry
	rdry = exp( dum )

!c   xmdry is dry mass in g
	xmdry = drydens_43pi_em12 * (rdry**3.0)

!c   xsdry is dry surface in cm2
	xsdry = x_4pi_em8 * (rdry**2.0)

!c   dumadjust is adjustment factor to reduce dF0/dr
	dum = log( rdry/rdry_star ) / log( sigmag_star )
	dumadjust = exp( -0.5*dum*dum )

	df0dlnrdry = df0dlnrdry_star * dumadjust

	dumsum_na = dumsum_na + dlnrdry*df0dlnrdry
	dumsum_ma = dumsum_ma + dlnrdry*df0dlnrdry*xmdry
	dumsum_sa = dumsum_sa + dlnrdry*df0dlnrdry*xsdry

2900	continue


!c
!c  all done
!c
3000	emitfact_numb = dumsum_na
	emitfact_mass = dumsum_ma
	emitfact_surf = dumsum_sa

	return
	end subroutine seasalt_emitfactors_1bin



END MODULE module_mosaic_addemiss



!----------------------------------------------------------------------
   subroutine addemiss_masscheck( id, config_flags, iflagaa, fromwhere,    &
               dtstep, efact1, dz8w, chem, chem_sum,                       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte,                                  &
               nemit,                                                      &
               e01, e02, e03, e04, e05, e06, e07, e08, e09, e10,           &
               e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21       )
!
! produces test diagnostics for "addemiss" routines
!
! 1. computes {sum over i,j,k ( chem * dz8w )} before and after
!    emissions tendencies are added to chem, 
!    then prints (sum_after - sum_before)/(dtstep*efact1)
! 2. computes {sum over i,j,k ( e_xxx )}, then prints them
! the two should be equal
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description, only:  num_chem

   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id, iflagaa,                             &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte,               &
                                  nemit

   REAL, INTENT(IN   ) ::    dtstep, efact1

! trace species mixing ratios
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(IN    ) ::   chem

! trace species integrals
   DOUBLE PRECISION, DIMENSION( num_chem ),                                &
         INTENT(INOUT ) ::   chem_sum

! layer thickness (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   dz8w

! emissions 
!   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )       ,                &
  REAL,  DIMENSION( ims:ime , kms:config_flags%kemit , jms:jme ),          &
          INTENT(IN   ) ::                                                 &
               e01, e02, e03, e04, e05, e06, e07, e08, e09, e10,           &
               e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21

   character(len=*), intent(in) :: fromwhere

! local variables
   integer, parameter :: nemit_maxd = 21
   integer :: i, j, k, l
   double precision :: chem_sum_prev
   real :: fact
   real :: emit_sum(nemit_maxd)


! compute column integral, summed over i-j grids
! compute {sum over i,j,k ( chem * dz8w ) }
! on second pass (iflagaa==2), subtract the pass-one sum
	do 1900 l = 1, num_chem

	chem_sum_prev = chem_sum(l)
	chem_sum(l) = 0.0

	do j = jts, jte
	do k = kts, kte-1
	do i = its, ite
	    chem_sum(l) = chem_sum(l) + dble( chem(i,k,j,l)*dz8w(i,k,j) )
	end do
	end do
	end do

	if (iflagaa == 2) chem_sum(l) =  (chem_sum(l) - chem_sum_prev)

1900	continue

	if (iflagaa /= 2) return


! compute {sum over i,j,k ( e_xxx ) }
	emit_sum(:) = 0.0

	do 2900 l = 1, min(nemit,nemit_maxd)
	do j = jts, jte
	do k = kts, min(config_flags%kemit,kte-1)
	do i = its, ite
	    if (l== 1) emit_sum(l) = emit_sum(l) + e01(i,k,j)
	    if (l== 2) emit_sum(l) = emit_sum(l) + e02(i,k,j)
	    if (l== 3) emit_sum(l) = emit_sum(l) + e03(i,k,j)
	    if (l== 4) emit_sum(l) = emit_sum(l) + e04(i,k,j)
	    if (l== 5) emit_sum(l) = emit_sum(l) + e05(i,k,j)
	    if (l== 6) emit_sum(l) = emit_sum(l) + e06(i,k,j)
	    if (l== 7) emit_sum(l) = emit_sum(l) + e07(i,k,j)
	    if (l== 8) emit_sum(l) = emit_sum(l) + e08(i,k,j)
	    if (l== 9) emit_sum(l) = emit_sum(l) + e09(i,k,j)
	    if (l==10) emit_sum(l) = emit_sum(l) + e10(i,k,j)

	    if (l==11) emit_sum(l) = emit_sum(l) + e11(i,k,j)
	    if (l==12) emit_sum(l) = emit_sum(l) + e12(i,k,j)
	    if (l==13) emit_sum(l) = emit_sum(l) + e13(i,k,j)
	    if (l==14) emit_sum(l) = emit_sum(l) + e14(i,k,j)
	    if (l==15) emit_sum(l) = emit_sum(l) + e15(i,k,j)
	    if (l==16) emit_sum(l) = emit_sum(l) + e16(i,k,j)
	    if (l==17) emit_sum(l) = emit_sum(l) + e17(i,k,j)
	    if (l==18) emit_sum(l) = emit_sum(l) + e18(i,k,j)
	    if (l==19) emit_sum(l) = emit_sum(l) + e19(i,k,j)
	    if (l==20) emit_sum(l) = emit_sum(l) + e20(i,k,j)

	    if (l==21) emit_sum(l) = emit_sum(l) + e21(i,k,j)
	end do
	end do
	end do
2900	continue

! output the chem_sum and emit_sum
	print 9110, fromwhere, its, ite, jts, jte
	print 9100, 'chem_sum'
	fact = 1.0/(dtstep*efact1)
	print 9120, (l, fact*chem_sum(l), l=1,num_chem)
	print 9100, 'emit_sum'
	print 9120, (l, emit_sum(l), l=1,min(nemit,nemit_maxd))

9100	format( a )
9110	format( / 'addemiss_masscheck output, fromwhere = ', a /   &
	'its, ite, jts, jte =', 4i5  )
9120	format( 5( i5, 1pe11.3 ) )


	return
   END subroutine addemiss_masscheck

