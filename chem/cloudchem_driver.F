!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

!  file cloudchem_driver.F


      SUBROUTINE cloudchem_driver(                                   &
               id, ktau, ktauc, dtstep, dtstepc, config_flags,       &
               t_phy, p_phy, rho_phy, alt,                           &
	       moist, cldfra, ph_no2,                                &
	       chem, gas_aqfrac, numgas_aqfrac,                      &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction

!----------------------------------------------------------------------
   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_mosaic_cloudchem, only: mosaic_cloudchem_driver
   USE module_sorgam_cloudchem, only: sorgam_cloudchem_driver

   !  This driver calls subroutines for wet scavenging.
   !
   !  1. MADE-SORGAM
   !  2. MOSAIC

!----------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------
! In WRF, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  In your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         .
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Definitions
!-----------
!-- alt       inverse density
!-- t_phy	  temperature (K)
!-- w             vertical velocity (m/s)
!-- moist	  moisture array (4D - last index is species) (kg/kg)
!-- dz8w	  dz between full levels (m)
!-- p8w		  pressure at full levels (Pa)	
!-- p_phy	  pressure (Pa)
!                 points (dimensionless)
!-- z             3D height with lowest level being the terrain
!-- rho_phy   	  density (kg/m^3)
!-- qlsink        Fractional cloud water sink (/s)
!-- precr         rain precipitation rate at all levels (kg/m2/s)
!-- preci         ice precipitation rate at all levels (kg/m2/s)
!-- precs         snow precipitation rate at all levels (kg/m2/s)
!-- precg         graupel precipitation rate at all levels (kg/m2/s)                             &
!-- R_d		  gas constant for dry air ( 287. J/kg/K)
!-- R_v           gas constant for water vapor (461 J/k/kg)
!-- Cp            specific heat at constant pressure (1004 J/k/kg)
!-- rvovrd	  R_v divided by R_d (dimensionless)
!-- G		  acceleration due to gravity (m/s^2)
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- its           start index for i in tile
!-- ite           end index for i in tile
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- config_flags%kemit  end index for k for emissions arrays
!
!======================================================================

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                                &
                                      ids,ide, jds,jde, kds,kde,    &
                                      ims,ime, jms,jme, kms,kme,    &
                                      its,ite, jts,jte, kts,kte,    &
                                      id, ktau, ktauc,              &
                                      numgas_aqfrac
      REAL,      INTENT(IN   ) :: dtstep, dtstepc
!
! moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),         &
         INTENT(IN ) ::                                   moist

! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(IN   ) ::                                          &
                                t_phy,                              &
                                p_phy,                              &
                                rho_phy,                            &
                                alt,                                &
                                cldfra,                             &
                                ph_no2
!
! all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),          &
         INTENT(INOUT ) ::                                chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, numgas_aqfrac ),     &
         INTENT(INOUT ) ::                                gas_aqfrac



! LOCAL  VAR
     integer :: ii,jj,kk


!-----------------------------------------------------------------

! These are unneeded, since the default behavior is to do nothing.
! If the default changes, then lines need to be added for CBMZ and
! CBMZ_BB.
!   IF (config_flags%chem_opt .eq. 0) return
!   IF (config_flags%chem_opt .eq. 1) return

!
! select which aerosol scheme to take
!
   cps_select: SELECT CASE(config_flags%chem_opt)

   CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )

       call wrf_debug(15, &
       'cloudchem_driver calling mosaic_cloudchem_driver')
       call mosaic_cloudchem_driver(                  &
            id, ktau, ktauc, dtstepc, config_flags,   &
            p_phy, t_phy, rho_phy, alt,               &
            cldfra, ph_no2,                           &
            moist, chem,                              &
            gas_aqfrac, numgas_aqfrac,                &
            ids,ide, jds,jde, kds,kde,                &
            ims,ime, jms,jme, kms,kme,                &
            its,ite, jts,jte, kts,kte )

   CASE ( RADM2SORG_AQ, RACMSORG_AQ )

       call wrf_debug(15, &
       'cloudchem_driver calling sorgam_cloudchem_driver')
       call sorgam_cloudchem_driver(                  &
            id, ktau, ktauc, dtstepc, config_flags,   &
            p_phy, t_phy, rho_phy, alt,               &
            cldfra, ph_no2,                           &
            moist, chem,                              &
            gas_aqfrac, numgas_aqfrac,                &
            ids,ide, jds,jde, kds,kde,                &
            ims,ime, jms,jme, kms,kme,                &
            its,ite, jts,jte, kts,kte )

   CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cloudchem_driver

