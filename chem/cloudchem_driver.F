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
               t_phy, p_phy, rho_phy, alt, dz8w,                           &
               p8w, prain3d,scalar,dvmrdt_sv13d,dvmrcwdt_sv13d,      & !Balwinder.Singh@pnnl.gov: Variables required for CAM-MAM cloud chemistry
               f_ice_phy, f_rain_phy, cldfrai, cldfral,              &
	       moist, cldfra, cldfra_mp_all, ph_no2,                 &
	       chem, gas_aqfrac, numgas_aqfrac,                      &
               is_CAMMGMP_used,                                      &!BSINGH:01/31/2013: Added is_CAMMGMP_used for CAM_MAM_cloudchem
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
   USE module_cam_support, only: gas_pcnst => gas_pcnst_modal_aero, &
        gas_pcnst_pos => gas_pcnst_modal_aero_pos
   USE module_mosaic_cloudchem,  only: mosaic_cloudchem_driver
   USE module_sorgam_cloudchem,  only: sorgam_cloudchem_driver
   USE module_sorgam_vbs_cloudchem, only: sorgam_vbs_cloudchem_driver
   USE module_cam_mam_cloudchem, only: cam_mam_cloudchem_driver
   USE module_sorgam_aqchem, only: sorgam_aqchem_driver
   USE module_sorgam_vbs_aqchem, only: sorgam_vbs_aqchem_driver

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
!-- scalar        scalar array (4D) #/kg
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
   LOGICAL,      INTENT(IN)       :: is_CAMMGMP_used
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
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_scalar ),         &
         INTENT(IN ) ::                                   scalar    !Balwinder.Singh@pnnl.gov: Variables required for CAM-MAM cloud chemistry
!
!tendencies:dvmrdt_sv13d,dvmrcwdt_sv13d are the tendencies which are passsed on from the CAM-MAM cloud chemistry
!           to gasaerexch subroutine in cam_mam_aerchem_driver
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, gas_pcnst_pos ),          &
        INTENT(OUT ) ::                                  dvmrdt_sv13d,dvmrcwdt_sv13d 

! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(IN   ) ::                                          &
                                t_phy,                              &
                                p_phy,                              &
                                rho_phy,                            &
                                alt,                                &
                                dz8w,                               &
                                cldfra,                             &
                                ph_no2,                             &
                                p8w,                                & !Balwinder.Singh@pnnl.gov: Variables required for CAM-MAM cloud chemistry
                                prain3d,                            &
                                F_ICE_PHY,                          &
                                F_RAIN_PHY,                         &
                                cldfrai,                            &
                                cldfral,                            &
                                cldfra_mp_all
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

   CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
        CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ,            &
        CBMZ_MOSAIC_DMS_8BIN_AQ, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, &
        MOZART_MOSAIC_4BIN_AQ_KPP,        &
        SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin

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

   CASE ( RADM2SORG_AQ, RACMSORG_AQ, CBMZSORG_AQ )

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
    CASE (CBMZ_CAM_MAM3_NOAQ,CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_NOAQ,CBMZ_CAM_MAM7_AQ)       
       CALL wrf_debug(15,'cloudchem_driver calling mam_cloudchem_driver')       
       call cam_mam_cloudchem_driver (                &
            !Intent Outs
            dvmrdt_sv13d,dvmrcwdt_sv13d,              & 
            !Intent in-outs
            chem,                                     &
            !Intent ins
            moist, scalar, p8w, prain3d, p_phy,       &
            t_phy, dtstepc, ktau,alt, f_ice_phy,      &
            f_rain_phy, cldfra, cldfra_mp_all,        &
            cldfrai, cldfral, is_CAMMGMP_used,        & 
            ids,ide, jds,jde, kds,kde,                &
            ims,ime, jms,jme, kms,kme,                &
            its,ite, jts,jte, kts,kte                 )
   CASE ( CB05_SORG_VBS_AQ_KPP )

       call wrf_debug(15, &
       'cloudchem_driver calling sorgam_vbs_aqchem_driver')
       call sorgam_vbs_aqchem_driver(                 &
            id, ktau, ktauc, dtstepc, config_flags,   &
            p_phy, t_phy, rho_phy, alt, dz8w,         &
            moist, chem,                              &
            gas_aqfrac, numgas_aqfrac,                &
            ids,ide, jds,jde, kds,kde,                &
            ims,ime, jms,jme, kms,kme,                &
            its,ite, jts,jte, kts,kte )


   CASE ( RADM2SORG_AQCHEM, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, &
          CB05_SORG_AQ_KPP,RACM_SOA_VBS_AQCHEM_KPP )

       call wrf_debug(15, &
       'cloudchem_driver calling sorgam_aqchem_driver')
       call sorgam_aqchem_driver(                  &
            id, ktau, ktauc, dtstepc, config_flags,   &
            p_phy, t_phy, rho_phy, alt, dz8w,         &
            moist, chem,                              &
            gas_aqfrac, numgas_aqfrac,                &
            ids,ide, jds,jde, kds,kde,                &
            ims,ime, jms,jme, kms,kme,                &
            its,ite, jts,jte, kts,kte )

   CASE DEFAULT

   END SELECT cps_select

   END SUBROUTINE cloudchem_driver

