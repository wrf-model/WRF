MODULE module_cam_mam_addemiss
!WRF:MODEL_LAYER:CHEMICS

!----------------------------------------------------------------------
! module_cam_mam_addemiss.F
!
! adapted from module_mosaic_addemiss.F in nov, 2010 by r.c.easter
!
!----------------------------------------------------------------------

#include "MODAL_AERO_CPP_DEFINES.h"

! rce 2005-feb-18 - one fix for indices of volumcen_sect, [now (isize,itype)]
! rce 2005-jan-14 - added subr mosaic_seasalt_emiss (and a call to it)
! rce 2004-dec-03 - many changes associated with the new aerosol "pointer"
!     variables in module_data_mosaic_asect


   private
   public :: cam_mam_addemiss


CONTAINS



!----------------------------------------------------------------------
   subroutine cam_mam_addemiss( id, dtstep, u10, v10, alt, dz8w, xland,    &
               config_flags, chem, slai, ust, smois, ivgtyp, isltyp,       &
               emis_ant,ebio_iso,ebio_olt,ebio_oli,rho_phy,                &
               dust_emiss_active, seasalt_emiss_active,                    &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!
! adds emissions for cam-mam aerosol species
! (i.e., emissions tendencies over time dtstep are applied 
! to the aerosol mass and number mixing ratios)
!
! currently this routine
! - applies emissions held in the emis_ant array
! - does online sea-salt emissions using a modified version of the
!   mosaic sea-salt emissions routine
! - does online dust emissions using a modified version of the
!   mosaic dust emissions routine
!
! still to do
! - for emis_ant emissions, implement emitted sizes similar to those used in cam
! - implement the cam sea-salt and dust emissions routines
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description
!  USE module_state_description, only:  num_chem, param_first_scalar,   &
!     emiss_inpt_default, emiss_inpt_pnnl_rs, emiss_inpt_pnnl_cm,num_emis_ant

   USE module_data_cam_mam_asect, only:  ai_phase,   &
         dens_so4_aer, dens_nh4_aer, dens_pom_aer,   &
         dens_bc_aer, dens_dust_aer, dens_seas_aer,   &
         lptr_so4_aer, lptr_nh4_aer, lptr_pom_aer,   &
         lptr_bc_aer, lptr_dust_aer, lptr_seas_aer,   &
         maxd_asize, maxd_atype, nsize_aer, ntype_aer, numptr_aer

   USE modal_aero_data, only:  &
         modeptr_accum, modeptr_aitken, modeptr_coarse,  &
         modeptr_coardust, modeptr_finedust, modeptr_pcarbon, &
         modeptr_fineseas,  modeptr_coarseas

   USE module_cam_mam_init, only:  &
         pom_emit_1p4_factor, so4_emit_1p2_factor

   USE module_data_sorgam, only:  &
         dgvem_i, dgvem_j, dgvem_c, sgem_i, sgem_j, sgem_c

   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   INTEGER, INTENT(IN) ::    dust_emiss_active, seasalt_emiss_active

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme )         ,                         &
          INTENT(IN   ) ::   u10, v10, xland, slai, ust
   INTEGER,  DIMENSION( ims:ime , jms:jme )      ,                         &
          INTENT(IN   ) ::   ivgtyp, isltyp

! trace species mixing ratios (aerosol mass = ug/kg-air; number = #/kg-air)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem
!
! aerosol emissions arrays ((ug/m3)*m/s)
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                          &
   REAL, DIMENSION( ims:ime,   1:config_flags%kemit, jms:jme,num_emis_ant ),            &
         INTENT(IN ) ::                                                    &
                         emis_ant
!jdf
   REAL,  DIMENSION( ims:ime , jms:jme )      ,                            &
          INTENT(IN   ) ::   ebio_iso, ebio_olt, ebio_oli
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::   rho_phy
!jdf

! 1/(dry air density) and layer thickness (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   alt, dz8w

   REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,      &
          INTENT(IN   ) ::   smois

! local variables

!    anth_emiss_size_opt = 1 --> sizes follow those used in cam5
!    anth_emiss_size_opt = 2 --> sizes follow those used in sorgam
   integer, parameter :: anth_emiss_size_opt = 1   ! user can change this value
!    dust_emiss_size_opt = 1 --> sizes follow those used in cam5
!    dust_emiss_size_opt = 2 --> sizes follow those used in sorgam
   integer, parameter :: dust_emiss_size_opt = 1   ! user can change this value
!    seas_emiss_size_opt = 1 --> sizes follow those used in cam5
!    seas_emiss_size_opt = 2 --> sizes follow those used in sorgam
   integer, parameter :: seas_emiss_size_opt = 1   ! user can change this value

   integer :: i, iphase, isize, itype
   integer :: itype_accum, itype_aitken, itype_pcarbon, &
              itype_finedust, itype_coardust
   integer :: j, k
   integer :: l, lemit, lemit_type, lnumb
   integer :: m, n
   integer :: p1st

   real, parameter :: pi = 3.1415926536
   real :: dp_anth_emiss_aitken, dp_anth_emiss_accum, dp_anth_emiss_coarse
   real :: dlo_seas_emiss( maxd_asize, maxd_atype ), &
           dhi_seas_emiss( maxd_asize, maxd_atype )
   real :: fact_mass, factbb_mass, factbb_numb
   real :: tmp_dens, tmp_dp, tmp_vol1p
   real :: total_soag, fact_soag


   character(len=100) :: msg


	iphase = ai_phase


!
! code applies emissions in the emis_ant array,
! then does "online" sea-salt and dust emissions
!
! currently the code only works with
! config_flags%emiss_inpt_opt = emiss_inpt_default OR emiss_inpt_pnnl_rs
!

	emiss_inpt_select_1: SELECT CASE( config_flags%emiss_inpt_opt )

!jdf
	    CASE( emiss_inpt_default, emiss_inpt_pnnl_rs, emiss_inpt_pnnl_mam )
		write(*,*) 'cam_mam_addemiss DOING emiss - emiss_inpt_opt =', &
			config_flags%emiss_inpt_opt
!jdf
	    CASE DEFAULT
		write(*,*) 'cam_mam_addemiss SKIPPING emiss - emiss_inpt_opt =', &
			config_flags%emiss_inpt_opt
		return

	END SELECT emiss_inpt_select_1


	p1st = param_first_scalar

	itype_accum    = modeptr_accum
	itype_aitken   = modeptr_aitken
#if ( defined MODAL_AERO_3MODE )
	itype_pcarbon  = modeptr_accum
	itype_finedust = modeptr_accum
	itype_coardust = modeptr_coarse
#elif ( defined MODAL_AERO_7MODE )
    if ( (config_flags%chem_opt /= CBMZ_CAM_MAM7_NOAQ) .and. &
         (config_flags%chem_opt /= CBMZ_CAM_MAM7_AQ  ) ) then
       call wrf_error_fatal( 'emission package for MAM7 is not implemented yet' )
    end if
#else
	itype_pcarbon  = modeptr_pcarbon
	itype_finedust = modeptr_finedust
	itype_coardust = modeptr_coardust
#endif

! set emitted sizes
	if (anth_emiss_size_opt == 1) then
	    ! volume-mean diameter for emitted particles (cm)
	    dp_anth_emiss_aitken = 0.0504e-4
	    dp_anth_emiss_accum  = 0.134e-4
	    dp_anth_emiss_coarse = 2.06e-4
	else if (anth_emiss_size_opt == 2) then
	    ! volume-mean diameter for emitted particles (cm)
	    dp_anth_emiss_aitken = 1.0e2 * dgvem_i / exp( 1.5 * (log(sgem_i)**2) )
	    dp_anth_emiss_accum  = 1.0e2 * dgvem_j / exp( 1.5 * (log(sgem_j)**2) )
	    dp_anth_emiss_coarse = 1.0e2 * dgvem_c / exp( 1.5 * (log(sgem_c)**2) )
	else
	    write(msg,'(2a,i7)') 'subr cam_mam_addemiss', &
		' - illegal anth_emiss_size_opt = ', anth_emiss_size_opt
	    call wrf_error_fatal( msg )
	end if


!jdf add case statements here to switch between 2 emission file options
	emiss_inpt_select_2: SELECT CASE( config_flags%emiss_inpt_opt )

	CASE( emiss_inpt_pnnl_rs )
!jdf
	do 1900 lemit_type = 1, 11

	iphase = 1
	isize = 1

! following if/then/else blocks determine
!    which chem species will receive a particular emis_ant(...,p_e_...)
!    additional mass factor for consistency with cam_mam assumptions
!    particle size and density for converting mass to number emissions
!
! oc (org), ec, and pm25 - both the "i" and "j" go to the same
!    cam_mam mode
! pm25 and pm_10 - go to the cam_mam dust species
! no3 - ignore cam_mam does not treat no3
!
	if (lemit_type == 1) then
	    lemit = p_e_so4i
	    itype = itype_aitken
	    l = lptr_so4_aer(isize,itype,iphase)
	    tmp_dens = dens_so4_aer
	    factbb_mass = so4_emit_1p2_factor
	    tmp_dp = dp_anth_emiss_aitken
	else if (lemit_type == 2) then
	    lemit = p_e_so4j
	    itype = itype_accum
	    l = lptr_so4_aer(isize,itype,iphase)
	    tmp_dens = dens_so4_aer
! so4_emit_1p2_factor is 115/96 if so4 (in 3-mode) is being treated 
! as ammonium bisulfate, or 1.0 if treated as pure sulfate
	    factbb_mass = so4_emit_1p2_factor
	    tmp_dp = dp_anth_emiss_accum

	else if (lemit_type == 3) then
	    lemit = p_e_no3i
	    cycle   ! cam_mam does not treat no3
	else if (lemit_type == 4) then
	    lemit = p_e_no3j
	    cycle   ! cam_mam does not treat no3

	else if (lemit_type == 5) then
	    lemit = p_e_orgi
	    itype = itype_pcarbon
	    l = lptr_pom_aer(isize,itype,iphase)
	    tmp_dens = dens_pom_aer
! pom_emit_1p4_factor is 1.4 if pom emissions are carbon-only emissions,
! or 1.0 if they are organic matter emissions
	    factbb_mass = pom_emit_1p4_factor
	    tmp_dp = dp_anth_emiss_accum
	else if (lemit_type == 6) then
	    lemit = p_e_orgj
	    itype = itype_pcarbon
	    l = lptr_pom_aer(isize,itype,iphase)
	    tmp_dens = dens_pom_aer
	    factbb_mass = pom_emit_1p4_factor
	    tmp_dp = dp_anth_emiss_accum

	else if (lemit_type == 7) then
	    lemit = p_e_eci
	    itype = itype_pcarbon
	    l = lptr_bc_aer(isize,itype,iphase)
	    tmp_dens = dens_bc_aer
	    factbb_mass = 1.0
	    tmp_dp = dp_anth_emiss_accum
	else if (lemit_type == 8) then
	    lemit = p_e_ecj
	    itype = itype_pcarbon
	    l = lptr_bc_aer(isize,itype,iphase)
	    tmp_dens = dens_bc_aer
	    factbb_mass = 1.0
	    tmp_dp = dp_anth_emiss_accum

	else if (lemit_type == 9) then
	    lemit = p_e_pm25i
	    itype = itype_finedust
	    l = lptr_dust_aer(isize,itype,iphase)
	    tmp_dens = dens_dust_aer
	    factbb_mass = 1.0
	    tmp_dp = dp_anth_emiss_accum
	else if (lemit_type == 10) then
	    lemit = p_e_pm25j
	    itype = itype_finedust
	    l = lptr_dust_aer(isize,itype,iphase)
	    tmp_dens = dens_dust_aer
	    factbb_mass = 1.0
	    tmp_dp = dp_anth_emiss_accum
	else if (lemit_type == 11) then
	    lemit = p_e_pm_10
	    itype = itype_coardust
	    l = lptr_dust_aer(isize,itype,iphase)
	    tmp_dens = dens_dust_aer
	    factbb_mass = 1.0
	    tmp_dp = dp_anth_emiss_coarse
	else
	    cycle
	end if

	if ((l < p1st) .or. (l > num_chem)) cycle

	lnumb = numptr_aer(isize,itype,iphase)
	if ((lnumb < p1st) .or. (lnumb > num_chem)) lnumb = -999888777

	tmp_vol1p = (pi/6.0)*(tmp_dp**3)   ! mean volume of emitted particles (cm3)
! factbb_numb convert mass emissions (ug/m2/s) to number emissions (#/m2/s)
	factbb_numb = 1.0e-6/(tmp_dens*tmp_vol1p)

	do j = jts, jte
	do k =   1, min(config_flags%kemit,kte)
	do i = its, ite

! emissions fluxes are ug/m2/s
! mult by tmp_fact_mass to get mixing ratio change (ug/kg) over dtstep
	fact_mass = (dtstep/dz8w(i,k,j))*alt(i,k,j)*factbb_mass

	chem(i,k,j,l) = chem(i,k,j,l) + emis_ant(i,k,j,lemit)*fact_mass

	if (lnumb > 0) then
	chem(i,k,j,lnumb) = chem(i,k,j,lnumb) + &
		emis_ant(i,k,j,lemit)*fact_mass*factbb_numb
	end if

	end do ! i
	end do ! k
	end do ! j

1900	continue
!
! now do emissions for e_soag
! add biogenic isoprene for soag_isoprene
! no monoterpenes in CBMZ at present for soag_terpene
! lump anthropogenic ald, hc3, hc5, hc8, ket, oli, olt, and ora2 which is par in CBMZ for soag_bigalk
! lump biogenic olet and olei for soag_bigene
! lump anthropogenic tol and xyl for soag_toluene
!
        do j = jts, jte
        do k =   1, 1
        do i = its, ite
          fact_soag = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
          total_soag = (emis_ant(i,k,j,p_e_tol) * (92.14*0.15/12.0))              + &
                       (emis_ant(i,k,j,p_e_xyl) * (106.16*0.15/12.0))             + &
                       (emis_ant(i,k,j,p_e_hc5) * (77.6*0.05/12.0))               + &
                       (emis_ant(i,k,j,p_e_olt) * (72.34*0.05/12.0))              + &
                       (emis_ant(i,k,j,p_e_oli) * (75.78*0.05/12.0))              + &
                       (ebio_iso(i,j) * (68.11*0.04/12.0))
!                      0.4*emis_ant(i,k,j,p_e_ald) + 2.9*emis_ant(i,k,j,p_e_hc3)  + &
!                      4.8*emis_ant(i,k,j,p_e_hc5) + 7.9*emis_ant(i,k,j,p_e_hc8)  + &
!                      0.9*emis_ant(i,k,j,p_e_ket) + 2.8*emis_ant(i,k,j,p_e_oli)  + &
!                      1.8*emis_ant(i,k,j,p_e_olt) + 1.0*emis_ant(i,k,j,p_e_ora2) + &
!                      (ebio_iso(i,j) * (68.11*0.04/12.0))                        + &
!                      ebio_olt(i,j)                                              + &
!                      ebio_oli(i,j) 
          chem(i,k,j,p_soag) = chem(i,k,j,p_soag) + total_soag*fact_soag
        end do ! i
        end do ! k
        end do ! j
        do j = jts, jte
        do k =   2, min(config_flags%kemit,kte)
        do i = its, ite
          fact_soag = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
          total_soag = (emis_ant(i,k,j,p_e_tol) * (92.14*0.15/12.0))              + &
                       (emis_ant(i,k,j,p_e_xyl) * (106.16*0.15/12.0))             + &
                       (emis_ant(i,k,j,p_e_hc5) * (77.6*0.05/12.0))               + &
                       (emis_ant(i,k,j,p_e_olt) * (72.34*0.05/12.0))              + &
                       (emis_ant(i,k,j,p_e_oli) * (75.78*0.05/12.0))
!                      emis_ant(i,k,j,p_e_xyl)                                    + &
!                      0.4*emis_ant(i,k,j,p_e_ald) + 2.9*emis_ant(i,k,j,p_e_hc3)  + &
!                      4.8*emis_ant(i,k,j,p_e_hc5) + 7.9*emis_ant(i,k,j,p_e_hc8)  + &
!                      0.9*emis_ant(i,k,j,p_e_ket) + 2.8*emis_ant(i,k,j,p_e_oli)  + &
!                      1.8*emis_ant(i,k,j,p_e_olt) + 1.0*emis_ant(i,k,j,p_e_ora2)
          chem(i,k,j,p_soag) = chem(i,k,j,p_soag) + total_soag*fact_soag
        end do ! i
        end do ! k
        end do ! j

	CASE( emiss_inpt_pnnl_mam )

	do 1910 lemit_type = 1, 14

	iphase = 1
        isize = 1
	if (lemit_type == 1) then
	    lemit = p_e_so4i
	    itype = itype_aitken
	    l = lptr_so4_aer(isize,itype,iphase)
	    factbb_mass = so4_emit_1p2_factor
	else if (lemit_type == 2) then
	    lemit = p_e_so4j
	    itype = itype_accum
	    l = lptr_so4_aer(isize,itype,iphase)
! so4_emit_1p2_factor is 115/96 if so4 (in 3-mode) is being treated 
! as ammonium bisulfate, or 1.0 if treated as pure sulfate
	    factbb_mass = so4_emit_1p2_factor
	else if (lemit_type == 3) then
	    lemit = p_e_orgj
	    itype = itype_pcarbon
	    l = lptr_pom_aer(isize,itype,iphase)
	    factbb_mass = pom_emit_1p4_factor
	else if (lemit_type == 4) then
	    lemit = p_e_ecj
	    itype = itype_pcarbon
	    l = lptr_bc_aer(isize,itype,iphase)
	    factbb_mass = 1.0
	else if (lemit_type == 5) then
	    lemit = p_e_dust_a1
	    itype = itype_finedust
	    l = lptr_dust_aer(isize,itype,iphase)
	    factbb_mass = 1.0
	else if (lemit_type == 6) then
	    lemit = p_e_dust_a3
	    itype = itype_coardust
	    l = lptr_dust_aer(isize,itype,iphase)
	    factbb_mass = 1.0
	else if (lemit_type == 7) then
	    lemit = p_e_ncl_a1
	    itype = itype_accum
	    l = lptr_seas_aer(isize,itype,iphase)
	    factbb_mass = 1.0
	else if (lemit_type == 8) then
	    lemit = p_e_ncl_a2
	    itype = itype_aitken
	    l = lptr_seas_aer(isize,itype,iphase)
	    factbb_mass = 1.0
	else if (lemit_type == 9) then
	    lemit = p_e_ncl_a3
	    itype = itype_coardust
	    l = lptr_seas_aer(isize,itype,iphase)
	    factbb_mass = 1.0
!PMA
!Currently my pre-processing script put num_a1 and num_a2 emissions from 
!sea salt and dust into so4j_num and so4i_num 
!num_a3 emissions are solely from dust and sea salt
!

        else if (lemit_type == 10) then
            lemit = p_e_so4i_num
            itype = itype_aitken
            l = numptr_aer(isize,itype,iphase)
        else if (lemit_type == 11) then
            lemit = p_e_so4j_num
            itype = itype_accum
            l = numptr_aer(isize,itype,iphase)
        else if (lemit_type == 12) then
            lemit = p_e_orgj_num
            itype = itype_accum
            l = numptr_aer(isize,itype,iphase)
        else if (lemit_type == 13) then
            lemit = p_e_ecj_num
            itype = itype_accum
            l = numptr_aer(isize,itype,iphase)
        else if (lemit_type == 14) then
            lemit = p_e_num_a3
            itype = itype_coardust
            l = numptr_aer(isize,itype,iphase)
        else
	    cycle
	end if

	if ((l < p1st) .or. (l > num_chem)) cycle

	do j = jts, jte
	do k =   1, min(config_flags%kemit,kte)
	do i = its, ite

! emissions fluxes are ug/m2/s
! mult by tmp_fact_mass to get mixing ratio change (ug/kg) over dtstep
	fact_mass = (dtstep/dz8w(i,k,j))*alt(i,k,j)*factbb_mass
!PMA 
        factbb_numb = (dtstep/dz8w(i,k,j))*alt(i,k,j)
        if (lemit_type < 10) then 
	chem(i,k,j,l) = chem(i,k,j,l) + emis_ant(i,k,j,lemit)*fact_mass
        else
        chem(i,k,j,l) = chem(i,k,j,l) + emis_ant(i,k,j,lemit)*factbb_numb
        end if

	end do ! i
	end do ! k
	end do ! j

1910	continue
!
! units for e_soag* assumed to be ug/m2/s for now
!
	do j = jts, jte
	do k =   1, min(config_flags%kemit,kte)
	do i = its, ite
	  fact_soag = (dtstep/dz8w(i,k,j))*alt(i,k,j)/1000.0
          total_soag = emis_ant(i,k,j,p_e_soag_bigalk)   + &
                       emis_ant(i,k,j,p_e_soag_bigene)   + &
                       emis_ant(i,k,j,p_e_soag_isoprene) + &
                       emis_ant(i,k,j,p_e_soag_terpene)  + &
                       emis_ant(i,k,j,p_e_soag_toluene)
	  chem(i,k,j,p_soag) = chem(i,k,j,p_soag) + total_soag*fact_soag
	end do ! i
	end do ! k
	end do ! j


	END SELECT emiss_inpt_select_2
!jdf end of add case statements here to switch between 2 emission file options

!
! seasalt emissions
!

! set emitted sizes
	dlo_seas_emiss(:,:) = 0.0
	dhi_seas_emiss(:,:) = 0.0

	if (seas_emiss_size_opt == 1) then
#if ( defined MODAL_AERO_3MODE )
! cut sizes are 0.02, 0.08, 1.0, 10.0 um
	    dlo_seas_emiss(1,modeptr_aitken  ) = 0.02e-4
	    dhi_seas_emiss(1,modeptr_aitken  ) = 0.08e-4
	    dlo_seas_emiss(1,modeptr_accum   ) = 0.08e-4
	    dhi_seas_emiss(1,modeptr_accum   ) = 1.00e-4
	    dlo_seas_emiss(1,modeptr_coarse  ) = 1.00e-4
	    dhi_seas_emiss(1,modeptr_coarse  ) = 10.0e-4
#else
! cut sizes are 0.02, 0.08, 0.3, 1.0, 10.0 um
	    dlo_seas_emiss(1,modeptr_aitken  ) = 0.02e-4
	    dhi_seas_emiss(1,modeptr_aitken  ) = 0.08e-4
	    dlo_seas_emiss(1,modeptr_accum   ) = 0.08e-4
	    dhi_seas_emiss(1,modeptr_accum   ) = 0.30e-4
	    dlo_seas_emiss(1,modeptr_fineseas) = 0.30e-4
	    dhi_seas_emiss(1,modeptr_fineseas) = 1.00e-4
	    dlo_seas_emiss(1,modeptr_coarseas) = 1.00e-4
	    dhi_seas_emiss(1,modeptr_coarseas) = 10.0e-4
#endif

	else if (seas_emiss_size_opt == 2) then
#if ( defined MODAL_AERO_3MODE )
! cut sizes are 0.1, 1.25, 10.0 um and no aitken
	    dlo_seas_emiss(1,modeptr_accum   ) = 0.10e-4
	    dhi_seas_emiss(1,modeptr_accum   ) = 1.25e-4
	    dlo_seas_emiss(1,modeptr_coarse  ) = 1.25e-4
	    dhi_seas_emiss(1,modeptr_coarse  ) = 10.0e-4
#else
! cut sizes are 0.1, 0.3, 1.25, 10.0 um and no aitken
	    dlo_seas_emiss(1,modeptr_accum   ) = 0.10e-4
	    dhi_seas_emiss(1,modeptr_accum   ) = 0.30e-4
	    dlo_seas_emiss(1,modeptr_fineseas) = 0.30e-4
	    dhi_seas_emiss(1,modeptr_fineseas) = 1.25e-4
	    dlo_seas_emiss(1,modeptr_coarseas) = 1.25e-4
	    dhi_seas_emiss(1,modeptr_coarseas) = 10.0e-4
#endif

	else
	    write(msg,'(2a,i7)') 'subr cam_mam_addemiss', &
		' - illegal seas_emiss_size_opt = ', seas_emiss_size_opt
	    call wrf_error_fatal( msg )
	end if

! now do the sea-salt emissions
	if (config_flags%seas_opt == 2) then
	    call cam_mam_mosaic_seasalt_emiss(                             &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               dlo_seas_emiss, dhi_seas_emiss,                             &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
	end if


!
! dust emissions
!

! jdf: preliminary version that has not been made generic for situation
	if (config_flags%dust_opt == 2) then
            call cam_mam_mosaic_dust_emiss(                                &
               slai, ust, smois, ivgtyp, isltyp,                           &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               dust_emiss_size_opt,                                        &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
	end if


	return


   END subroutine cam_mam_addemiss



!----------------------------------------------------------------------
   subroutine cam_mam_mosaic_seasalt_emiss(                                &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               dlo_seas_emiss, dhi_seas_emiss,                             &
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
   USE module_data_cam_mam_asect, only:  ai_phase,   &
         lptr_seas_aer,   &
         maxd_asize, maxd_atype, nsize_aer, ntype_aer, numptr_aer
!  USE module_mosaic_addemiss, only:  seasalt_emitfactors_1bin

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

! emissions cut sizes (cm)
   REAL,  DIMENSION( maxd_asize, maxd_atype ), &
          INTENT(IN   ) ::  dlo_seas_emiss, dhi_seas_emiss

! local variables
	integer i, j, k, l, l_seas, n
	integer iphase, itype
	integer p1st

	real dum, dumdlo, dumdhi, dumoceanfrac, dumspd10
	real factaa, factbb

	real :: ssemfact_numb( maxd_asize, maxd_atype )
	real :: ssemfact_mass( maxd_asize, maxd_atype )

    write(*,*) 'in subr cam_mam_mosaic_seasalt_emiss'
    p1st = PARAM_FIRST_SCALAR

!   for now just do itype=1
	iphase = ai_phase

!   compute emissions factors for each size bin
!   (limit emissions to dp > 0.1 micrometer)
	ssemfact_mass(:,:) = 0.0
	ssemfact_numb(:,:) = 0.0
	do itype = 1, ntype_aer
	do n = 1, nsize_aer(itype)
	    dumdlo = max( dlo_seas_emiss(n,itype), 0.1e-4 )
	    dumdhi = max( dhi_seas_emiss(n,itype), 0.1e-4 )
	    if (dumdlo >= dumdhi) cycle
	    call seasalt_emitfactors_1bin( 1, dumdlo, dumdhi,   &
		ssemfact_numb(n,itype), dum, ssemfact_mass(n,itype) )

!   convert mass emissions factor from (g/m2/s) to (ug/m2/s)
	    ssemfact_mass(n,itype) = ssemfact_mass(n,itype)*1.0e6
	end do
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

	do 1815 itype = 1, ntype_aer
	do 1810 n = 1, nsize_aer(itype)
	    if (ssemfact_mass(n,itype) <= 0.0) cycle

!   only apply emissions if bin has both na and cl species
	    l_seas = lptr_seas_aer(n,itype,iphase)
	    if (l_seas < p1st) cycle

	    chem(i,k,j,l_seas) = chem(i,k,j,l_seas) +   &
			factbb * ssemfact_mass(n,itype)

	    l = numptr_aer(n,itype,iphase)
	    if (l >= p1st) chem(i,k,j,l) = chem(i,k,j,l) +   &
			factbb * ssemfact_numb(n,itype)

1810	continue
1815	continue

1820	continue
1830	continue

	return

   END subroutine cam_mam_mosaic_seasalt_emiss



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



!----------------------------------------------------------------------
   subroutine cam_mam_mosaic_dust_emiss(  slai,ust, smois, ivgtyp, isltyp, &
               id, dtstep, u10, v10, alt, dz8w, xland, config_flags, chem, &
               dust_emiss_size_opt,                                        &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!
! adds dust emissions for mosaic aerosol species (i.e. emission tendencies
! over time dtstep are applied to the aerosol mixing ratios)
!
! This is a simple dust scheme based on Shaw et al. (2008) to appear in
! Atmospheric Environment, recoded by Jerome Fast
!
! NOTE: 
! 1) This version only works with the 8-bin version of MOSAIC.
! 2) Dust added to MOSAIC's other inorganic specie, OIN.  If Ca and CO3 are 
!    activated in the Registry, a small fraction also added to Ca and CO3.
! 3) The main departure from Shaw et al., is now alphamask is computed since
!    the land-use categories in that paper and in WRF differ.  WRF currently 
!    does not have that many land-use categories and adhoc assumptions had to
!    be made. This version was tested for Mexico in the dry season.  The main
!    land-use categories in WRF that are likely dust sources are grass, shrub,
!    and savannna (that WRF has in the desert regions of NW Mexico).  Having
!    dust emitted from these types for other locations and other times of the
!    year is not likely to be valid.
! 4) An upper bound on ustar was placed because the surface parameterizations
!    in WRF can produce unrealistically high values that lead to very high
!    dust emission rates.
! 5) Other departures' from Shaw et al. noted below, but are probably not as
!    important as 2) and 3).
!

   USE module_configure, only:  grid_config_rec_type
   USE module_state_description, only:  num_chem, param_first_scalar

   USE module_data_cam_mam_asect, only:  ai_phase,   &
         lptr_dust_aer, ntype_aer, numptr_aer

   USE modal_aero_data, only:  &
         modeptr_accum, modeptr_coarse, modeptr_coardust, modeptr_finedust

   IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  dust_emiss_size_opt,                     &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte

   REAL, INTENT(IN   ) ::    dtstep

! 10-m wind speed components (m/s)
   REAL,  DIMENSION( ims:ime , jms:jme ),                                  &
          INTENT(IN   ) ::   u10, v10, xland, slai, ust
   INTEGER,  DIMENSION( ims:ime , jms:jme ),                               &
          INTENT(IN   ) ::   ivgtyp, isltyp

! trace species mixing ratios (aerosol mass = ug/kg; number = #/kg)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::   chem

! alt  = 1.0/(dry air density) in (m3/kg)
! dz8w = layer thickness in (m)
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::   alt, dz8w

   REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,     &
          INTENT(IN   ) ::   smois

! local variables
        integer, parameter :: max_dust_mode = 2
        integer :: i, ii, j, k, l, l_dust, l_numb, n
        integer :: imode, iphase, isize, itype, izob
        integer :: p1st
        integer :: isize_dust_mode(max_dust_mode)
        integer :: itype_dust_mode(max_dust_mode)
        integer :: n_dust_mode

        real :: dum, dumdlo, dumdhi, dumlandfrac, dumspd10
        real :: factaa, factbb, factcc, fracoin, fracca, fracco3, fractot
        real :: ustart, ustar1, ustart0
        real :: alphamask, f8, f50, f51, f52, wetfactor, sumdelta, ftot
        real :: smois_grav, wp, pclay
        real :: beta(4,7)
        real :: gamma(4), delta(4)
        real :: sz(8)
        real :: dustflux, densdust
        real :: mass1part_mos8bin(8)
        real :: sz_wght_dust_mode(8,max_dust_mode)
        real :: dcen_mos8bin(8)

        character(len=100) :: msg

        write(*,*) 'in subr cam_mam_mosaic_dust_emiss'
        p1st = param_first_scalar


! the mass emission fractions [sz(1:8)] were derived for the 
!    mosaic 8-bin size bins
! the emissions for each mosaic bin are assigned to the cam-mam fine or coarse mode
!    (e.g, the fine mode may get bins 1-5 and the coarse mode bins 6-8)
! for calculating number emissions from mass emissions, use the mosaic bin
!    sizes (and corresponding 1-particle masses) in order to get number emissions
!    consistent with mosaic
!
! note:  the sz() values were recommend by Natalie Mahowold, so probably follow the
!    assumptions used in the "dead" dust module.  
!    applying those assumptions along with the cut sizes might be a better approach
!    than adapting the mosaic 8-bin sz() values

#if (defined MODAL_AERO)
! 3 modes -- dust is in accum and coarse modes
        n_dust_mode = 2
        itype_dust_mode(1) = modeptr_accum
        itype_dust_mode(2) = modeptr_coarse
        isize_dust_mode(:) = 1
        if (dust_emiss_size_opt == 1) then
! cam5 cut sizes for dust emiss are 0.1, 1.0, 10.0 um
! fine mode gets bins 1-4 and 60% of bin 5
           sz_wght_dust_mode(1:8,1) = (/ 1.,  1.,  1.,  1.,  0.6, 0.,  0.,  0.  /)
           sz_wght_dust_mode(1:8,2) = (/ 0.,  0.,  0.,  0.,  0.4, 1.,  1.,  1.  /)
        else if (dust_emiss_size_opt == 2) then
! sorgam cut sizes for dust emiss are 0.1, 2.5, 10.0 um
! fine mode gets bins 1-6
           sz_wght_dust_mode(1:8,1) = (/ 1.,  1.,  1.,  1.,  1.,  1.,  0.,  0.  /)
           sz_wght_dust_mode(1:8,2) = (/ 0.,  0.,  0.,  0.,  0.,  0.,  1.,  1.  /)
        else
            write(msg,'(2a,i7)') 'subr cam_mam_mosaic_dust_emiss', &
                ' - illegal dust_emiss_size_opt = ', dust_emiss_size_opt
            call wrf_error_fatal( msg )
        end if
#else
! 7 modes -- dust is in fine-dust and coarse-dust modes
        n_dust_mode = 2
        itype_dust_mode(1) = modeptr_finedust
        itype_dust_mode(2) = modeptr_coardust
        isize_dust_mode(:) = 1
        if (dust_emiss_size_opt == 1) then
! cam5 cut sizes for dust emiss are 0.1, 2.0, 10.0 um
! fine mode gets bins 1-5 and 60% of bin 6
! (for 7-mode cam, the fine-dust mode is larger than the accumulation mode,
!  so the cut size is larger than with 3-mode cam)
           sz_wght_dust_mode(1:8,1) = (/ 1.,  1.,  1.,  1.,  1.,  0.6, 0.,  0.  /)
           sz_wght_dust_mode(1:8,2) = (/ 0.,  0.,  0.,  0.,  0.,  0.4, 1.,  1.  /)
        else if (dust_emiss_size_opt == 2) then
! sorgam cut sizes for dust emiss are 0.1, 2.5, 10.0 um
! fine mode gets bins 1-6
           sz_wght_dust_mode(1:8,1) = (/ 1.,  1.,  1.,  1.,  1.,  1.,  0.,  0.  /)
           sz_wght_dust_mode(1:8,2) = (/ 0.,  0.,  0.,  0.,  0.,  0.,  1.,  1.  /)
        else
            write(msg,'(2a,i7)') 'subr cam_mam_mosaic_dust_emiss', &
                'illegal dust_emiss_size_opt = ', dust_emiss_size_opt
            call wrf_error_fatal( msg )
        end if
#endif

! bin center diameters for mosaic-8bin (cm)
        dcen_mos8bin(8) = 1.0e-4*(10.0/sqrt(2.0))
        do n = 7, 1, -1
           dcen_mos8bin(n) = dcen_mos8bin(n+1)*0.5
        end do
! mass1part_mos8bin is mass of a single particle in ug, 
! assuming density of dust ~2.5 g cm-3
        densdust=2.5
        do n = 1, 8
           mass1part_mos8bin(n)=0.523598*(dcen_mos8bin(n)**3)*densdust*1.0e06
        end do


!
! from Nickovic et al., JGR, 2001 and Shaw et al. 2007
! beta: fraction of clay, small silt, large silt, and sand correcsponding to Zobler class (7)
! beta (1,*) for 0.5-1 um
! beta (2,*) for 1-10 um
! beta (3,*) for 10-25 um
! beta (4,*) for 25-50 um
!
        beta(1,1)=0.12
        beta(2,1)=0.04
        beta(3,1)=0.04
        beta(4,1)=0.80
        beta(1,2)=0.34
        beta(2,2)=0.28
        beta(3,2)=0.28
        beta(4,2)=0.10
        beta(1,3)=0.45
        beta(2,3)=0.15
        beta(3,3)=0.15
        beta(4,3)=0.25
        beta(1,4)=0.12
        beta(2,4)=0.09
        beta(3,4)=0.09
        beta(4,4)=0.70
        beta(1,5)=0.40
        beta(2,5)=0.05
        beta(3,5)=0.05
        beta(4,5)=0.50
        beta(1,6)=0.34
        beta(2,6)=0.18
        beta(3,6)=0.18
        beta(4,6)=0.30
        beta(1,7)=0.22
        beta(2,7)=0.09
        beta(3,7)=0.09
        beta(4,7)=0.60
        gamma(1)=0.08
        gamma(2)=1.00
        gamma(3)=1.00
        gamma(4)=0.12
!
! * Mass fractions for each size bin. These values were recommended by 
!   Natalie Mahowold, with bins 7 and 8 the same as bins 3 and 4 from CAM.
! * Changed slightly since Natelie's estimates do not add up to 1.0
! * This would need to be made more generic for other bin sizes.
!       sz(1)=0
!       sz(2)=1.78751e-06
!       sz(3)=0.000273786
!       sz(4)=0.00847978
!       sz(5)=0.056055
!       sz(6)=0.0951896
!       sz(7)=0.17
!       sz(8)=0.67
!jdf    sz(1)=0.0
!jdf    sz(2)=0.0
!jdf    sz(3)=0.0005
!jdf    sz(4)=0.0095
!jdf    sz(5)=0.03
!jdf    sz(6)=0.10
!jdf    sz(7)=0.18
!jdf    sz(8)=0.68
        sz(1)=0.0
        sz(2)=0.0
        sz(3)=0.0005
        sz(4)=0.0095
        sz(5)=0.01
        sz(6)=0.06
        sz(7)=0.20
        sz(8)=0.72

        iphase = ai_phase

!   loop over i,j and apply dust emissions
        k = kts
        do 1830 j = jts, jte
        do 1820 i = its, ite

    if( xland(i,j) > 1.5 ) cycle

! compute wind speed anyway, even though ustar is used below

        dumlandfrac = 1.
        dumspd10=(u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j))**(0.5)
        if(dumspd10 >= 5.0) then
           dumspd10 = dumlandfrac* &
         ( dumspd10*dumspd10*(dumspd10-5.0))
         else
            dumspd10=0.
         endif

! part1 - compute vegetation mask
!
! * f8, f50, f51, f52 refer to vegetation classes from the Olsen categories
!   for desert, sand desert, grass semi-desert, and shrub semi-desert
! * in WRF, vegetation types 7, 8 and 10 are grassland, shrubland, and savanna
!   that are dominate types in Mexico and probably have some erodable surface
!   during the dry season
! * currently modified these values so that only a small fraction of cell
!   area is erodable
! * these values are highly tuneable!

         alphamask=0.001
         if (ivgtyp(i,j) .eq. 7) then
           f8=0.005
           f50=0.00
           f51=0.10
           f51=0.066
           f52=0.00
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif
         if (ivgtyp(i,j) .eq. 8) then
           f8=0.010
           f50=0.00
           f51=0.00
           f52=0.15
           f52=0.10
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif
         if (ivgtyp(i,j) .eq. 10) then
           f8=0.00
           f50=0.00
           f51=0.01
           f52=0.00
           alphamask=(f8+f50)*1.0+(f51+f52)*0.5
         endif

! part2 - zobler
! 
! * in Shaw's paper, dust is computed for 4 size ranges:
!   0.5-1 um 
!    1-10 um  
!   10-25 um  
!   25-50 um
! * Shaw's paper also accounts for sub-grid variability in soil
!   texture, but here we just assume the same soil texture for each
!   grid cell
! * since MOSAIC is currently has a maximum size range up to 10 um,
!   neglect upper 2 size ranges and lowest size range (assume small)
! * map WRF soil classes arbitrarily to Zolber soil textural classes
! * skip dust computations for WRF soil classes greater than 13, i.e. 
!   do not compute dust over water, bedrock, and other surfaces
! * should be skipping for water surface at this point anyway
!
         izob=0
         if(isltyp(i,j).eq.1) izob=1
         if(isltyp(i,j).eq.2) izob=1
         if(isltyp(i,j).eq.3) izob=4
         if(isltyp(i,j).eq.4) izob=2
         if(isltyp(i,j).eq.5) izob=2
         if(isltyp(i,j).eq.6) izob=2
         if(isltyp(i,j).eq.7) izob=7
         if(isltyp(i,j).eq.8) izob=2
         if(isltyp(i,j).eq.9) izob=6
         if(isltyp(i,j).eq.10) izob=5
         if(isltyp(i,j).eq.11) izob=2
         if(isltyp(i,j).eq.12) izob=3
         if(isltyp(i,j).ge.13) izob=0
         if(izob.eq.0) goto 1820
!
! part3 - dustprod
!
         do ii=1,4
           delta(ii)=0.0
         enddo
         sumdelta=0.0
         do ii=1,4
           delta(ii)=beta(ii,izob)*gamma(ii)
           if(ii.lt.4) then
             sumdelta=sumdelta+delta(ii)
           endif
         enddo
         do ii=1,4
           delta(ii)=delta(ii)/sumdelta
         enddo

! part4 - wetness
!
! * assume dry for now, have passed in soil moisture to this routine
!   but needs to be included here
! * wetfactor less than 1 would reduce dustflux
! * convert model soil moisture (m3/m3) to gravimetric soil moisture
!   (mass of water / mass of soil in %) assuming a constant density 
!   for soil
         pclay=beta(1,izob)*100.
         wp=0.0014*pclay*pclay+0.17*pclay
         smois_grav=(smois(i,1,j)/2.6)*100.
         if(smois_grav.gt.wp) then
           wetfactor=sqrt(1.0+1.21*(smois_grav-wp)**0.68)
         else
           wetfactor=1.0
         endif
!        wetfactor=1.0

! part5 - dustflux
! lower bound on ustar = 20 cm/s as in Shaw et al, but set upper
! bound to 100 cm/s

         ustar1=ust(i,j)*100.0
         if(ustar1.gt.100.0) ustar1=100.0
         ustart0=20.0
         ustart=ustart0*wetfactor
         if(ustar1.le.ustart) then
           dustflux=0.0
         else
           dustflux=1.0e-14*(ustar1**4)*(1.0-(ustart/ustar1))
         endif
         dustflux=dustflux*10.0
! units kg m-2 s-1
         ftot=0.0
         do ii=1,2
           ftot=ftot+dustflux*alphamask*delta(ii)
         enddo
! convert to ug m-2 s-1
         ftot=ftot*1.0e+09

!   apportion other inorganics only
         factaa = (dtstep/dz8w(i,k,j))*alt(i,k,j)
         factbb = factaa * ftot
         fractot = 1.0

         do imode = 1, n_dust_mode
!   loop over the cam-mam modes that have dust
         itype = itype_dust_mode(imode)
         isize = isize_dust_mode(imode)

         l_dust = lptr_dust_aer(isize,itype,iphase)
         if ((l_dust < p1st) .or. (l_dust > num_chem)) cycle
         l_numb = numptr_aer(isize,itype,iphase)
         if ((l_numb < p1st) .or. (l_numb > num_chem)) l_numb = -1

!   if (ivgtyp(i,j) .eq. 8) print*,'jdf',i,j,ustar1,ustart0,factaa,ftot
!        do 1810 n = 1, nsize_aer(itype)
         do n = 1, 8
!   calculate contribution of each mosaic-8bin bin to the current cam-mam mode
            if (sz_wght_dust_mode(n,imode) <= 0.0) cycle
            factcc = factbb * sz(n) * fractot * sz_wght_dust_mode(n,imode)

            chem(i,k,j,l_dust) = chem(i,k,j,l_dust) + factcc

            if (l_numb >= p1st) &
            chem(i,k,j,l_numb) = chem(i,k,j,l_numb) + factcc/mass1part_mos8bin(n)

        end do ! n

        end do ! imode


1820    continue
1830    continue

        return

   END subroutine cam_mam_mosaic_dust_emiss


!----------------------------------------------------------------------
END MODULE module_cam_mam_addemiss
