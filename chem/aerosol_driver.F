!WRF:MODEL_LAYER:PHYSICS
!
      SUBROUTINE aerosols_driver (id,ktau,config_flags,dtstep,             &
               alt,t_phy,moist,aerwrf,p8w,t8w,p_phy,chem,rho_phy,dz8w,  &
               z,z_at_w,h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,  &
               cvaro2,cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,&
               e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,           &
               e_so4i,e_so4j,e_no3i,e_no3j,                             &
               vdrog3,aer_dry,aer_water,                                   &
               imicrogram,                                                 &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )

!----------------------------------------------------------------------
   USE module_configure
   USE module_state_description
   USE module_model_constants

! *** add new modules of schemes here

   USE module_aerosols_sorgam
   USE module_data_sorgam

   !  This driver calls subroutines for aerosols parameterizations.
   !
   !  1. MADE-SORGAM
   !
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
!-- t_phy	  temperature (K)
!-- w             vertical velocity (m/s)
!-- moist	  moisture array (4D - last index is species) (kg/kg)
!-- dz8w	  dz between full levels (m)
!-- p8w		  pressure at full levels (Pa)	
!-- p_phy	  pressure (Pa)
!                 points (dimensionless)
!-- z             3D height with lowest level being the terrain
!-- rho_phy   	  density (kg/m^3)
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
!
!======================================================================

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   )    ::                                &
                                      ids,ide, jds,jde, kds,kde,    &
                                      ims,ime, jms,jme, kms,kme,    &
                                      its,ite, jts,jte, kts,kte,    &
			    	      id,ktau
   INTEGER,      INTENT(INOUT   )    ::               imicrogram
      REAL,      INTENT(IN   ) :: dtstep
!
! moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),         &
         INTENT(IN ) ::                                   moist
!
! all advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),          &
         INTENT(INOUT ) ::                                chem
!
! following are aerosol arrays that are not advected
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(INOUT ) ::                                          &
           h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
           cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2
!
! aerosol emissions arrays
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(INOUT ) ::                                          &
               e_so4i,e_so4j,e_no3i,e_no3j,                         &
               e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10
!
! arrays for aerosol/radiation feedback
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(INOUT ) ::                                          &
               aer_dry,aer_water,aerwrf
!
! aerosol/radm2 interaction
!
   REAL,  DIMENSION(ims:ime,kms:kme-0,jms:jme,ldrog),               &
               INTENT(IN   ) ::                                     &
                                                     VDROG3
!
! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(IN   ) ::                                          &
                                                      t_phy,        &
                                                      p_phy,        &
                                                      dz8w,         &
                                            alt,      z    ,        &
                                            t8w,p8w,z_at_w ,        &
                                                    rho_phy
!
! sulf concentration before modification by chemical mechanism
!
     REAL, dimension (ims:ime,kms:kme-0,jms:jme),                   &
               INTENT(INOUT) ::                                     &
                               vcsulf_old

! LOCAL  VAR
     integer :: ii,jj,kk


!-----------------------------------------------------------------

   IF (config_flags%chem_opt .eq. 0) return
   IF (config_flags%chem_opt .eq. 1) return

!
! select which aerosol scheme to take 
!
   cps_select: SELECT CASE(config_flags%chem_opt)

       CASE (RADM2SORG)
       CALL wrf_debug(15,'in sorgam')
       do ii=its,ite
          do kk=kts,kte
             do jj=jts,jte
                if(chem(ii,kk,jj,p_nu0).lt.1.e07)then
                   chem(ii,kk,jj,p_nu0)=1.e7
                endif
             enddo
          enddo
       enddo
       call sorgam_driver (id,ktau,dtstep,t_phy,moist,aerwrf,p8w,t8w,   &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                 &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,    &
               e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,        &
               e_so4i,e_so4j,e_no3i,e_no3j,                             &
               vdrog3,aer_dry,aer_water,                                &
               imicrogram,                                              &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )

       CASE (RACMSORG)
       CALL wrf_debug(15,'in sorgam')
       do ii=its,ite
          do kk=kts,kte
             do jj=jts,jte
                if(chem(ii,kk,jj,p_nu0).lt.1.e07)then
                   chem(ii,kk,jj,p_nu0)=1.e7
                endif
             enddo
          enddo
       enddo
       call sorgam_driver (id,ktau,dtstep,t_phy,moist,aerwrf,p8w,t8w,   &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                 &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,    &
               e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,        &
               e_so4i,e_so4j,e_no3i,e_no3j,                             &
               vdrog3,aer_dry,aer_water,                                &
               imicrogram,                                              &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )

     CASE DEFAULT 

   END SELECT cps_select

   END SUBROUTINE aerosols_driver


