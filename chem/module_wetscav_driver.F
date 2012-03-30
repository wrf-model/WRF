!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  

MODULE module_wetscav_driver

  REAL, PARAMETER :: mwso4 = 96.00   ! Molecular mass of SO4-- (g/mol)
  REAL, PARAMETER :: mwno3 = 62.0    ! Molecular mass of NO3- (g/mol)

CONTAINS


!===========================================================================
!===========================================================================
      subroutine wetscav_driver( id, ktau, dtstep, ktauc, config_flags,    &
               dtstepc, alt, t_phy, moist, p8w,                            &
               t8w, dx, dy, p_phy, chem,                                   &
               rho_phy, cldfra, cldfra2, rainprod, evapprod,               &
               hno3_col_mdel, qlsink, precr, preci, precs, precg,          &
	       gas_aqfrac, numgas_aqfrac, dz8w,                            &
	       h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,           &
               cvaro2,cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,           &
               wd_no3,wd_so4,                                              &
               qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp,                         &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )

!----------------------------------------------------------------------
!
!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction
!
!  This driver calls subroutines for wet scavenging.
!
!  1. MADE-SORGAM
!  2. MOSAIC
!
!----------------------------------------------------------------------

   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_mozcart_wetscav, only:  wetscav_mozcart
   USE module_mosaic_wetscav, only:  wetscav_cbmz_mosaic
   USE module_aerosols_sorgam, only:  wetscav_sorgam_driver

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
                                      id, ktau, ktauc, numgas_aqfrac

      REAL,      INTENT(IN   ) :: dtstep,dtstepc
      REAL,      INTENT(IN   ) :: dx, dy
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

! fraction of gas species in cloud water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, numgas_aqfrac ),     &
         INTENT(IN ) ::                                   gas_aqfrac

!
! following are aerosol arrays that are not advected
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(INOUT ) ::                                          &
           h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
           cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2
!
! Accumulated wet deposition
!
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: wd_no3,wd_so4
!
! input from meteorology
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(IN   ) ::                                          &
                                                      alt,          &
                                                      t_phy,        &
                                                      p_phy,        &
                                                      t8w,          &
                                                      p8w,          &
                                                      dz8w,         &
	                            qlsink,precr,preci,precs,precg, &
                                                    rho_phy,cldfra
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
         INTENT(INOUT ) ::                             cldfra2,     &
                                                       rainprod,    &
                                                       evapprod
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
         INTENT(IN) ::                                 qc_b4mp,     &
                                                       qv_b4mp,     &
                                                       qi_b4mp,     &
                                                       qs_b4mp
   REAL,  DIMENSION( ims:ime , jms:jme )         ,                  &
         INTENT(INOUT ) ::                             hno3_col_mdel
!

! LOCAL  VAR
   integer :: ii,jj,kk
   REAL, DIMENSION( ims:ime, jms:jme, num_chem ) :: qsrflx ! column change due to scavening
   REAL ::  tmp_minval = 1.0e7

!
! Wet deposition over the current time step
!
  REAL, DIMENSION( ims:ime , jms:jme ) :: wdi_no3,wdi_so4

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

   CASE ( RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM )
       CALL wrf_debug(15,'wetscav_driver calling sorgam_wetscav_driver' )
       call wetscav_sorgam_driver (id,ktau,dtstep,ktauc,config_flags,      &
               dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,        &
               qlsink,precr,preci,precs,precg, qsrflx,                     &
               gas_aqfrac, numgas_aqfrac,                                  &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
! not clear if the following is necessary or appropriate
       tmp_minval = 1.0e7
       do jj=jts,jte
       do kk=kts,kte
       do ii=its,ite
          if (chem(ii,kk,jj,p_nu0) .lt. tmp_minval) then
             chem(ii,kk,jj,p_nu0) = tmp_minval
          endif
       enddo
       enddo
       enddo

   CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN)
       CALL wrf_error_fatal('Wet scavenging is currently not possible with MOSAIC unless aqueous aerosols are turned on.')

   CASE (CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ)
       CALL wrf_debug(15,'wetscav_driver calling mosaic_wetscav_driver')
       call wetscav_cbmz_mosaic (id,ktau,dtstep,ktauc,config_flags,        &
               dtstepc,alt,t_phy,p8w,t8w,p_phy,chem,rho_phy,cldfra,        &
               qlsink,precr,preci,precs,precg, qsrflx,                     &
               gas_aqfrac, numgas_aqfrac,                                  &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
   CASE (MOZART_KPP,MOZCART_KPP,MOZART_MOSAIC_4BIN_VBS0_KPP)
       CALL wrf_debug(15,'wetscav_driver calling wetscav_mozcart')
       call wetscav_mozcart( id, ktau, dtstep, ktauc, config_flags,             &
                             dtstepc, t_phy, p8w, t8w, p_phy,                   &
                             chem, rho_phy, cldfra2, rainprod, evapprod,        &
                             qv_b4mp, qc_b4mp, qi_b4mp, qs_b4mp,                &
                             gas_aqfrac, numgas_aqfrac, dz8w, dx, dy,           &
                             moist(ims,kms,jms,p_qv), moist(ims,kms,jms,p_qc),  &
                             moist(ims,kms,jms,p_qi), moist(ims,kms,jms,p_qs),  &
                             hno3_col_mdel,                                     &
                             ids,ide, jds,jde, kds,kde,                         &
                             ims,ime, jms,jme, kms,kme,                         &
                             its,ite, jts,jte, kts,kte                          )

   CASE DEFAULT

   END SELECT cps_select

   ! Calculate wet deposition of the species by accumulating their (negative)
   ! column change. It is assumed that all precipitation reaches the ground.
   
    SELECT CASE(config_flags%chem_opt)
      
      CASE ( RADM2SORG_AQCHEM,RACMSORG_AQCHEM )
      
      do jj=jts,jte
        do ii=its,ite
          
          ! Nitrate wet deposition over the current time step:
          wdi_no3(ii,jj) = - 0.001*qsrflx(ii,jj,p_no3cwj)/mwno3 &
                           - 0.001*qsrflx(ii,jj,p_no3cwi)/mwno3  ! mmol/m2
          
          ! Accumulated nitrate wet deposition:
          wd_no3(ii,jj) = wd_no3(ii,jj) + wdi_no3(ii,jj)         ! mmol/m2
          
          ! Sulfate wet deposition over the current time step:
          wdi_so4(ii,jj) = - 0.001*qsrflx(ii,jj,p_so4cwj)/mwso4 &
                           - 0.001*qsrflx(ii,jj,p_so4cwi)/mwso4 &
                           - qsrflx(ii,jj,p_sulf) &
                           - qsrflx(ii,jj,p_so2)                 ! mmol/m2
          
          ! Accumulated sulfate wet deposition:
          wd_so4(ii,jj) = wd_so4(ii,jj) + wdi_so4(ii,jj)         ! mmol/m2
          
        enddo
      enddo
    
    CASE DEFAULT
      
    END SELECT
   end subroutine wetscav_driver


END MODULE module_wetscav_driver


