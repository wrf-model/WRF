! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Aerosol Option:  MOSAIC (Model for Simulating Aerosol Interactions &
! Chemistry)
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
! of the code, or Jerome Fast, the WRF-chem implementation team leader
! for PNNL.
!
! Terms of Use:
!  1) Users are requested to consult the primary author prior to
!     modifying the MOSAIC code or incorporating it or its submodules in
!     another code. This is meant to ensure that the any linkages and/or
!     assumptions will not adversely affect the operation of MOSAIC.
!  2) The MOSAIC source code is intended for research and educational
!     purposes. Users are requested to contact the primary author
!     regarding the use of MOSAIC code for any commercial application.
!  3) Users preparing publications resulting from the usage of MOSAIC are
!     requested to cite one or more of the references below (depending on
!     the application) for proper acknowledgement.
!
! References for MOSAIC (The first one is the primary MOSAIC reference):
! * Zaveri R.A., R.C. Easter, J.D. Fast, and L.K. Peters (2008), Model
!   for Simulating Aerosol Interactions and Chemistry (MOSAIC), J.
!   Geophys. Res., 113, D13204, doi:10.1029/2007JD008782.
! * Zaveri R.A., R.C. Easter, and A.S. Wexler (2005), A new method for
!   multi-component activity coefficients of electrolytes in aqueous
!   atmospheric aerosols, J. Geophys. Res., 110, D02201,
!   doi:10.1029/2004JD004681.
! * Zaveri R.A., R.C. Easter, and L.K. Peters (2005), A computationally
!   efficient multicomponent equilibrium solver for aerosols (MESA), J.
!   Geophys. Res., 110, D24203, doi:24203, doi:10.1029/2004JD005618.
!
! Reference for volatility basis set SOA code:
! * Shrivastava, M., J. Fast, R. Easter, W. I. Gustafson Jr., R. A.
!   Zaveri, J. L. Jimenez, P. Saide, and A. Hodzic, 2010: Modeling
!   organic aerosols in a megacity: comparison of simple and complex
!   representations of the volatility basis set approach. Atmos. Chem.
!   Phys., in review.
!
! Reference for the alternative SOA treatment that is less expensive to run 
! and is used in the package  mozart_mosaic_4bin_vbs0
! SOA is treated using a simplified approach for anthropogenic and biomass burning species based on Hodzic and Jimenez, GMD, 2011
! Hodzic, A. and Jimenez, J. L.: Modeling anthropogenically controlled secondary organic aerosols in a megacity: 
! a simplified framework for global and climate models, Geosci. Model Dev., 4, 901-917, doi:10.5194/gmd-4-901-2011, 2011.
!
! References for implementation of the aerosol optical properties and
! direct effect in WRF-Chem:
! * Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!   Barnard, E.G. Chapman, G.A. Grell, and S.E. Peckham (2005), Evolution
!   of ozone, particulates, and aerosol direct radiative forcing in the
!   vicinity of Houston using a fully-coupled meteorology-chemistry-
!   aerosol model, J. Geophys. Res., 111, D21305,
!   doi:10.1029/2005JD006721.
! * Barnard, J.C., J.D. Fast, G. Paredes-Miranda, W.P. Arnott, and
!   A. Laskin (2010), Technical note: evaluation of the WRF-Chem
!   "aerosol chemical to aerosol optical properties" module using data
!   from the MILAGRO campaign, Atmos. Chem. Phys., 10, 7325-7340,
!   doi:10.5194/acp-10-7325-2010.
!
! References for implementation of the aerosol-cloud interactions and
! indirect effects in WRF-Chem:
! * Gustafson, W. I., E. G. Chapman, S. J. Ghan, R. C. Easter, and J. D.
!   Fast (2007), Impact on modeled cloud characteristics due to
!   simplified treatment of uniform cloud condensation nuclei during
!   NEAQS 2004, Geophys. Res. Lett., 34, L19809, L19809,
!   doi:10.1029/2007GL0300321.
! * Chapman, E.G., W. I. Gustafson Jr., R. C. Easter, J. C. Barnard,
!   S. J. Ghan, M. S. Pekour, and J. D. Fast (2009): Coupling aerosol-
!   cloud-radiative processes in the WRF-Chem model: Investigating the
!   radiative impact of elevated point sources, Atmos. Chem. Phys., 9,
!   945-964, www.atmos-chem-phys.net/9/945/2009/.
!
! Contact Jerome Fast for updates on the status of manuscripts under
! review.
!
! Additional information:
! * www.pnl.gov/atmospheric/research/wrf-chem
!
! Support: 
! Funding for developing and evaluating MOSAIC was provided by the U.S.
! Department of Energy under the auspices of Atmospheric Science Program
! of the Office of Biological and Environmental Research, the NASA Earth
! Sciences Enterprise under grant NAGW 3367, and PNNL Laboratory Directed
! Research and Development program.
!************************************************************************
	module module_mosaic_sumpm


!
!   *** NOTE - when the cw species are NOT in the registry, then
!   then the p_xxx_cwnn variables are not in module_state_description,
!   and the following cpp directive should be commented out
!
#define cw_species_are_in_registry


	contains


!-----------------------------------------------------------------------
   subroutine sum_pm_mosaic (                                          &
         alt, chem,                                                    &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   REAL :: mass

   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte
!
! Sum over bins with center diameter < 2.5e-4 cm for pm2_5_dry,
! pm2_5_dry_ec, and pm2_5_water. All bins go into pm10
!
   pm2_5_dry(its:imax,kts:kmax,jts:jmax)    = 0.
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = 0.
   pm2_5_water(its:imax,kts:kmax,jts:jmax)  = 0.
   pm10(its:imax,kts:kmax,jts:jmax)         = 0.

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype)
      if (dcen_sect(n,itype) .le. 2.5e-4) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  mass = chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(n,itype,iphase))    




 

 
                  pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + mass

                  pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)            &
                                      + chem(i,k,j,lptr_bc_aer(n,itype,iphase))

                  pm2_5_water(i,k,j) = pm2_5_water(i,k,j)              &
                                     + chem(i,k,j,waterptr_aer(n,itype))

                  pm10(i,k,j) = pm10(i,k,j) + mass
               enddo
            enddo
         enddo
      else
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  pm10(i,k,j) = pm10(i,k,j)                              &
                              + chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                              + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                              + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                              + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                              + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                              + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                              + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                              + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(n,itype,iphase))



               enddo
            enddo
         enddo
      endif
   enddo ! size
   enddo ! type
   enddo ! phase

   !Convert the units from mixing ratio to concentration (ug m^-3)
   pm2_5_dry(its:imax,kts:kmax,jts:jmax) = pm2_5_dry(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) &
                                              / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_water(its:imax,kts:kmax,jts:jmax) = pm2_5_water(its:imax,kts:kmax,jts:jmax) &
                                             / alt(its:imax,kts:kmax,jts:jmax)
   pm10(its:imax,kts:kmax,jts:jmax) = pm10(its:imax,kts:kmax,jts:jmax) &
                                      / alt(its:imax,kts:kmax,jts:jmax)

   end subroutine sum_pm_mosaic


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   subroutine sum_pm_mosaic_vbs2 (                                      &
         alt, chem,                                                    &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   REAL :: mass

   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte
!
! Sum over bins with center diameter < 2.5e-4 cm for pm2_5_dry,
! pm2_5_dry_ec, and pm2_5_water. All bins go into pm10
!
   pm2_5_dry(its:imax,kts:kmax,jts:jmax)    = 0.
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = 0.
   pm2_5_water(its:imax,kts:kmax,jts:jmax)  = 0.
   pm10(its:imax,kts:kmax,jts:jmax)         = 0.
   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype)
      if (dcen_sect(n,itype) .le. 2.5e-4) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  mass = chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))


                  pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + mass

                  pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)            &
                                      + chem(i,k,j,lptr_bc_aer(n,itype,iphase))

                  pm2_5_water(i,k,j) = pm2_5_water(i,k,j)              &
                                     + chem(i,k,j,waterptr_aer(n,itype))

                  pm10(i,k,j) = pm10(i,k,j) + mass
               enddo
            enddo
         enddo
      else
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  pm10(i,k,j) = pm10(i,k,j)                              &
                              + chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))



               enddo
            enddo
         enddo
      endif
   enddo ! size
   enddo ! type
   enddo ! phase

   !Convert the units from mixing ratio to concentration (ug m^-3)
   pm2_5_dry(its:imax,kts:kmax,jts:jmax) = pm2_5_dry(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) &
                                              / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_water(its:imax,kts:kmax,jts:jmax) = pm2_5_water(its:imax,kts:kmax,jts:jmax) &
                                             / alt(its:imax,kts:kmax,jts:jmax)
   pm10(its:imax,kts:kmax,jts:jmax) = pm10(its:imax,kts:kmax,jts:jmax) &
                                      / alt(its:imax,kts:kmax,jts:jmax)

   end subroutine sum_pm_mosaic_vbs2


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   subroutine sum_pm_mosaic_vbs0 (                                      &
         alt, chem,                                                    &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   REAL :: mass

   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte
!
!
! Sum over bins with center diameter < 2.5e-4 cm for pm2_5_dry,
! pm2_5_dry_ec, and pm2_5_water. All bins go into pm10
!
   pm2_5_dry(its:imax,kts:kmax,jts:jmax)    = 0.
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = 0.
   pm2_5_water(its:imax,kts:kmax,jts:jmax)  = 0.
   pm10(its:imax,kts:kmax,jts:jmax)         = 0.
   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype)
      if (dcen_sect(n,itype) .le. 2.5e-4) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  mass = chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_smpa_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))

! SMPA and SMPBB do not participate to pm2.5 mass

                  pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + mass

                  pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)            &
                                      + chem(i,k,j,lptr_bc_aer(n,itype,iphase))

                  pm2_5_water(i,k,j) = pm2_5_water(i,k,j)              &
                                     + chem(i,k,j,waterptr_aer(n,itype))

                  pm10(i,k,j) = pm10(i,k,j) + mass
               enddo
            enddo
         enddo
      else
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  pm10(i,k,j) = pm10(i,k,j)                              &
                              + chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_smpa_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))

! SMPA and SMPBB do not participate to pm10 mass

               enddo
            enddo
         enddo
      endif
   enddo ! size
   enddo ! type
   enddo ! phase

   !Convert the units from mixing ratio to concentration (ug m^-3)
   pm2_5_dry(its:imax,kts:kmax,jts:jmax) = pm2_5_dry(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) &
                                              / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_water(its:imax,kts:kmax,jts:jmax) = pm2_5_water(its:imax,kts:kmax,jts:jmax) &
                                             / alt(its:imax,kts:kmax,jts:jmax)
   pm10(its:imax,kts:kmax,jts:jmax) = pm10(its:imax,kts:kmax,jts:jmax) &
                                      / alt(its:imax,kts:kmax,jts:jmax)

   end subroutine sum_pm_mosaic_vbs0



!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
   subroutine sum_pm_mosaic_vbs4 (                                      &
         alt, chem,                                                    &
         pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                   &
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: pm2_5_dry,pm2_5_water,pm2_5_dry_ec,pm10

   REAL :: mass

   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte
!
!
! Sum over bins with center diameter < 2.5e-4 cm for pm2_5_dry,
! pm2_5_dry_ec, and pm2_5_water. All bins go into pm10
!
   pm2_5_dry(its:imax,kts:kmax,jts:jmax)    = 0.
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = 0.
   pm2_5_water(its:imax,kts:kmax,jts:jmax)  = 0.
   pm10(its:imax,kts:kmax,jts:jmax)         = 0.
   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype)
      if (dcen_sect(n,itype) .le. 2.5e-4) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  mass = chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_glysoa_r1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoaX_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(n,itype,iphase))

! SMPA and SMPBB do not participate to pm2.5 mass

                  pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + mass

                  pm2_5_dry_ec(i,k,j) = pm2_5_dry_ec(i,k,j)            &
                                      + chem(i,k,j,lptr_bc_aer(n,itype,iphase))

                  pm2_5_water(i,k,j) = pm2_5_water(i,k,j)              &
                                     + chem(i,k,j,waterptr_aer(n,itype))

                  pm10(i,k,j) = pm10(i,k,j) + mass
               enddo
            enddo
         enddo
      else
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                  pm10(i,k,j) = pm10(i,k,j)                              &
                              + chem(i,k,j,lptr_so4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_no3_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_cl_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_nh4_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_na_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_oin_aer(n,itype,iphase)) &
                                                          + chem(i,k,j,lptr_oc_aer(n,itype,iphase))  &
                                                          + chem(i,k,j,lptr_bc_aer(n,itype,iphase))  &
                       + chem(i,k,j,lptr_glysoa_r1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoaX_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(n,itype,iphase))

! SMPA and SMPBB do not participate to pm10 mass

               enddo
            enddo
         enddo
      endif
   enddo ! size
   enddo ! type
   enddo ! phase

   !Convert the units from mixing ratio to concentration (ug m^-3)
   pm2_5_dry(its:imax,kts:kmax,jts:jmax) = pm2_5_dry(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) = pm2_5_dry_ec(its:imax,kts:kmax,jts:jmax) &
                                              / alt(its:imax,kts:kmax,jts:jmax)
   pm2_5_water(its:imax,kts:kmax,jts:jmax) = pm2_5_water(its:imax,kts:kmax,jts:jmax) &
                                             / alt(its:imax,kts:kmax,jts:jmax)
   pm10(its:imax,kts:kmax,jts:jmax) = pm10(its:imax,kts:kmax,jts:jmax) &
                                      / alt(its:imax,kts:kmax,jts:jmax)

   end subroutine sum_pm_mosaic_vbs4


!-----------------------------------------------------------------------


       subroutine  sum_vbs0 ( aero_diag_opt,                           &
             alt, chem,                                                &
             hoa_a01,hoa_a02,hoa_a03,hoa_a04,                          &
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             asmpsoa_a01,asmpsoa_a02,asmpsoa_a03,asmpsoa_a04,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,              &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4,                              &
             smpa_v1,smpbb_v1,                              &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte                                  )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::  aero_diag_opt
   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: hoa_a01,hoa_a02,hoa_a03,hoa_a04,               &
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             asmpsoa_a01,asmpsoa_a02,asmpsoa_a03,asmpsoa_a04,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,                      &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4,smpa_v1,smpbb_v1



   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte

    totoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.

   if( aero_diag_opt > 0 ) then
    hoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    asmpsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    asmpsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a02(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    asmpsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a03(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    asmpsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a04(its:imax,kts:kmax,jts:jmax)    = 0.

! Species to calculate O:C ratios
     biog_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v4(its:imax,kts:kmax,jts:jmax)    = 0.
     smpa_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     smpbb_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v4(its:imax,kts:kmax,jts:jmax)    = 0.
   endif



   do iphase=1,nphase_aer
      do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                 totoa_a01(i,k,j)= totoa_a01(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase))
                 totoa_a02(i,k,j)= totoa_a02(i,k,j) &
                        + chem(i,k,j,lptr_oc_aer(2,itype,iphase)) &
                        + chem(i,k,j,lptr_glysoa_sfc_aer(2,itype,iphase))    &
                        + chem(i,k,j,lptr_smpa_aer(2,itype,iphase))    &
                        + chem(i,k,j,lptr_smpbb_aer(2,itype,iphase))    &
                        + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                        + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))
                 totoa_a03(i,k,j)= totoa_a03(i,k,j)  &
                       + chem(i,k,j,lptr_smpa_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_smpbb_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase))
                 totoa_a04(i,k,j)= totoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_smpbb_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase))
               enddo
            enddo
         enddo
      enddo ! type
   enddo ! phase

   if( aero_diag_opt > 0 ) then
   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype) !The 4th bin is 2.5-10um and outside the AMS measurements
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax

        biog_v1(i,k,j)= biog_v1(i,k,j) &
                         + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))

        ant_v1(i,k,j)=  ant_v1(i,k,j) &
                         + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))

        smpa_v1(i,k,j)=  smpa_v1(i,k,j) &
                         + chem(i,k,j,lptr_smpa_aer(n,itype,iphase))
        smpbb_v1(i,k,j)=  smpbb_v1(i,k,j) &
                         + chem(i,k,j,lptr_smpbb_aer(n,itype,iphase))

                 enddo
               enddo
             enddo
                    enddo
                  enddo
               enddo

      biog_v1(its:imax,kts:kmax,jts:jmax) = biog_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      ant_v1(its:imax,kts:kmax,jts:jmax) = ant_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      smpa_v1(its:imax,kts:kmax,jts:jmax) = smpa_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      smpbb_v1(its:imax,kts:kmax,jts:jmax) = smpbb_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)


   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
! missing summation! (hoa = hoa + x ...)
!        hoa_a01(i,k,j)= chem(i,k,j,lptr_oc_aer(1,itype,iphase))
!
!
!        soa_a01(i,k,j)= (chem(i,k,j,lptr_smpa_aer(1,itype,iphase)) &
!                       + chem(i,k,j,lptr_smpbb_aer(1,itype,iphase)) &
!                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase)))
!
!
!        bbsoa_a01(i,k,j)= chem(i,k,j,lptr_smpbb_aer(1,itype,iphase))
!
!
!        biog_a01(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase)))
!
!        asmpsoa_a01(i,k,j)= chem(i,k,j,lptr_smpa_aer(1,itype,iphase))
!
!        totoa_a01(i,k,j)= (  &
!                       + chem(i,k,j,lptr_smpa_aer(1,itype,iphase)) &
!                       + chem(i,k,j,lptr_smpbb_aer(1,itype,iphase)) &
!                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
!                      + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
!                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase)))

        hoa_a01(i,k,j)= hoa_a01(i,k,j) &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase))


        soa_a01(i,k,j)= soa_a01(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))


        bbsoa_a01(i,k,j)= bbsoa_a01(i,k,j) &
                       + chem(i,k,j,lptr_smpbb_aer(1,itype,iphase))


        biog_a01(i,k,j)= biog_a01(i,k,j) &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))

        asmpsoa_a01(i,k,j)= asmpsoa_a01(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(1,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
! missing summation! (hoa = hoa + x ...)
!          hoa_a02(i,k,j)= (chem(i,k,j,lptr_oc_aer(2,itype,iphase)))
!
!
!          soa_a02(i,k,j)= (chem(i,k,j,lptr_smpa_aer(2,itype,iphase)) &
!                       + chem(i,k,j,lptr_smpbb_aer(2,itype,iphase)) &
!                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase)))
!
!
!          bbsoa_a02(i,k,j)= (chem(i,k,j,lptr_smpbb_aer(2,itype,iphase)))
!
!
!          biog_a02(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
!                         + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase)))
!
!
!          asmpsoa_a02(i,k,j)= chem(i,k,j,lptr_smpa_aer(2,itype,iphase))
!
!          totoa_a02(i,k,j)= ( chem(i,k,j,lptr_oc_aer(2,itype,iphase)) &
!                        + chem(i,k,j,lptr_smpa_aer(2,itype,iphase))    &
!                        + chem(i,k,j,lptr_smpbb_aer(2,itype,iphase))    &
!                        + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
!                        + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase)))
          hoa_a02(i,k,j)= hoa_a02(i,k,j) &
                       + chem(i,k,j,lptr_oc_aer(2,itype,iphase))


          soa_a02(i,k,j)= soa_a02(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))


          bbsoa_a02(i,k,j)= bbsoa_a02(i,k,j) &
                       + chem(i,k,j,lptr_smpbb_aer(2,itype,iphase))


          biog_a02(i,k,j)= biog_a02(i,k,j) &
                        + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase)) &
                        + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))


          asmpsoa_a02(i,k,j)= asmpsoa_a02(i,k,j) &
                        + chem(i,k,j,lptr_smpa_aer(2,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
! missing summation! (hoa = hoa + x ...)
!          hoa_a03(i,k,j)= (chem(i,k,j,lptr_oc_aer(3,itype,iphase)))
!
!          soa_a03(i,k,j)= (chem(i,k,j,lptr_smpa_aer(3,itype,iphase)) &
!                       + chem(i,k,j,lptr_smpbb_aer(3,itype,iphase)) &
!                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase)))
!
!
!          bbsoa_a03(i,k,j)= (chem(i,k,j,lptr_smpbb_aer(3,itype,iphase)))
!
!          biog_a03(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
!                         + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase)))
!
!          asmpsoa_a03(i,k,j)= chem(i,k,j,lptr_smpa_aer(3,itype,iphase))
!
!
!          totoa_a03(i,k,j)= ( &
!                       + chem(i,k,j,lptr_smpa_aer(3,itype,iphase))    &
!                       + chem(i,k,j,lptr_smpbb_aer(3,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
!                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase)))
          hoa_a03(i,k,j)= hoa_a03(i,k,j) &
                       + (chem(i,k,j,lptr_oc_aer(3,itype,iphase)))

          soa_a03(i,k,j)= soa_a03(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))


          bbsoa_a03(i,k,j)= bbsoa_a03(i,k,j) &
                       + chem(i,k,j,lptr_smpbb_aer(3,itype,iphase))

          biog_a03(i,k,j)= biog_a03(i,k,j) &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))

          asmpsoa_a03(i,k,j)= asmpsoa_a03(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(3,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
! missing summation! (hoa = hoa + x ...)
!          hoa_a04(i,k,j)= (chem(i,k,j,lptr_oc_aer(4,itype,iphase)))
!
!
!          soa_a04(i,k,j)= (chem(i,k,j,lptr_smpa_aer(4,itype,iphase)) &
!                       + chem(i,k,j,lptr_smpbb_aer(4,itype,iphase)) &
!                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase)))
!
!
!         bbsoa_a04(i,k,j)= (chem(i,k,j,lptr_smpbb_aer(4,itype,iphase)))
!
!
!         biog_a04(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase)))
!
!         asmpsoa_a04(i,k,j)= chem(i,k,j,lptr_smpa_aer(4,itype,iphase))
!
!
!
!         totoa_a04(i,k,j)= (  &
!                       + chem(i,k,j,lptr_smpa_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_smpbb_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
!                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase)))
          hoa_a04(i,k,j)= hoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase))


          soa_a04(i,k,j)= soa_a04(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_smpbb_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))


         bbsoa_a04(i,k,j)= bbsoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_smpbb_aer(4,itype,iphase))


         biog_a04(i,k,j)= biog_a04(i,k,j) &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))

         asmpsoa_a04(i,k,j)= asmpsoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_smpa_aer(4,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase
   endif

!Factor of 1.4 used below to convert OC to OA
        totoa_a01(its:imax,kts:kmax,jts:jmax) =totoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a02(its:imax,kts:kmax,jts:jmax) =totoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a03(its:imax,kts:kmax,jts:jmax) =totoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a04(its:imax,kts:kmax,jts:jmax) =totoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        if( aero_diag_opt > 0 ) then
        hoa_a01(its:imax,kts:kmax,jts:jmax) =hoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a01(its:imax,kts:kmax,jts:jmax) =soa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bboa_a01(its:imax,kts:kmax,jts:jmax) =bboa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bbsoa_a01(its:imax,kts:kmax,jts:jmax) =bbsoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)


        biog_a01(its:imax,kts:kmax,jts:jmax) =biog_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        asmpsoa_a01(its:imax,kts:kmax,jts:jmax) =asmpsoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)



        hoa_a02(its:imax,kts:kmax,jts:jmax) =hoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a02(its:imax,kts:kmax,jts:jmax) =soa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bboa_a02(its:imax,kts:kmax,jts:jmax) =bboa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bbsoa_a02(its:imax,kts:kmax,jts:jmax) =bbsoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        biog_a02(its:imax,kts:kmax,jts:jmax) =biog_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        asmpsoa_a02(its:imax,kts:kmax,jts:jmax) =asmpsoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)


        hoa_a03(its:imax,kts:kmax,jts:jmax) =hoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a03(its:imax,kts:kmax,jts:jmax) =soa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bboa_a03(its:imax,kts:kmax,jts:jmax) =bboa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bbsoa_a03(its:imax,kts:kmax,jts:jmax) =bbsoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        biog_a03(its:imax,kts:kmax,jts:jmax) =biog_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        asmpsoa_a03(its:imax,kts:kmax,jts:jmax) =asmpsoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)


        hoa_a04(its:imax,kts:kmax,jts:jmax) =hoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a04(its:imax,kts:kmax,jts:jmax) =soa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bboa_a04(its:imax,kts:kmax,jts:jmax) =bboa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        bbsoa_a04(its:imax,kts:kmax,jts:jmax) =bbsoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        biog_a04(its:imax,kts:kmax,jts:jmax) =biog_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        asmpsoa_a04(its:imax,kts:kmax,jts:jmax) =asmpsoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        endif


   end subroutine sum_vbs0


!-----------------------------------------------------------------------

       subroutine  sum_vbs2 ( aero_diag_opt,                           &
             alt, chem,                                                &
             hoa_a01,hoa_a02,hoa_a03,hoa_a04,                          &
             hoa_a05,hoa_a06,hoa_a07,hoa_a08,                          & !BSINGH(12/04/2013): Added 4 more bins(5 to 8) for all apecies
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             bboa_a05,bboa_a06,bboa_a07,bboa_a08,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             soa_a05,soa_a06,soa_a07,soa_a08,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             bbsoa_a05,bbsoa_a06,bbsoa_a07,bbsoa_a08,                  &
             hsoa_a01,hsoa_a02,hsoa_a03,hsoa_a04,                      &
             hsoa_a05,hsoa_a06,hsoa_a07,hsoa_a08,                      &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             biog_a05,biog_a06,biog_a07,biog_a08,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,              &
             arosoa_a05,arosoa_a06,arosoa_a07,arosoa_a08,              &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             totoa_a05,totoa_a06,totoa_a07,totoa_a08,                  &
             hsoa_c,hsoa_o,bbsoa_c,bbsoa_o,                            &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4,                              &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte                                  )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::  aero_diag_opt
   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: hoa_a01,hoa_a02,hoa_a03,hoa_a04,               &
         hoa_a05,hoa_a06,hoa_a07,hoa_a08,                              &
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             bboa_a05,bboa_a06,bboa_a07,bboa_a08,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             soa_a05,soa_a06,soa_a07,soa_a08,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             bbsoa_a05,bbsoa_a06,bbsoa_a07,bbsoa_a08,                  &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             biog_a05,biog_a06,biog_a07,biog_a08,                      &
             hsoa_a01,hsoa_a02,hsoa_a03,hsoa_a04,                      &
             hsoa_a05,hsoa_a06,hsoa_a07,hsoa_a08,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,              &
             arosoa_a05,arosoa_a06,arosoa_a07,arosoa_a08,              &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             totoa_a05,totoa_a06,totoa_a07,totoa_a08,                  &
             hsoa_c,hsoa_o,bbsoa_c,bbsoa_o,                            &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4



   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte

    totoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a08(its:imax,kts:kmax,jts:jmax)    = 0.

   if( aero_diag_opt > 0 ) then
    hoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a02(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a03(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a04(its:imax,kts:kmax,jts:jmax)    = 0.

    
    hoa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a05(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a05(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a06(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a06(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a07(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a07(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a08(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a08(its:imax,kts:kmax,jts:jmax)    = 0.



! Species to calculate O:C ratios
     hsoa_c(its:imax,kts:kmax,jts:jmax)    = 0.
     hsoa_o(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_c(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_o(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v4(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v4(its:imax,kts:kmax,jts:jmax)    = 0.
   endif

! NOTE - summation seems to be wrong for code below, but not my code so did not fix...
   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                 totoa_a01(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase)))
                 totoa_a02(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(2,itype,iphase)))
                 totoa_a03(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase)))
                 totoa_a04(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase)))
                 totoa_a05(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(5,itype,iphase)))
                 totoa_a06(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(6,itype,iphase)))
                 totoa_a07(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(7,itype,iphase)))
                 totoa_a08(i,k,j) = (chem(i,k,j,lptr_pcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(8,itype,iphase)))
               enddo
            enddo
         enddo
   enddo ! type

   if( aero_diag_opt > 0 ) then
   !do iphase=1,nphase_aer!BSINGH - Commented out as we need to add only phase 1 (interstitial) values species
   iphase = 1
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype) !The 4th bin is 2.5-10um and outside the AMS measurements
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax

          hsoa_c(i,k,j)=hsoa_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)))*180/211

         hsoa_o(i,k,j)= hsoa_o(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)))

          bbsoa_c(i,k,j)= bbsoa_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)))*180/211

         bbsoa_o(i,k,j)=bbsoa_o(i,k,j) &
                       +(chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)))

        biog_v1(i,k,j)= biog_v1(i,k,j) &
                         + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))

        ant_v1(i,k,j)=  ant_v1(i,k,j) &
                         + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))

                 enddo
               enddo
             enddo
                    enddo
                  enddo


! NOTE - summation also wrong for code below, but not my code so did not fix...

   !do iphase=1,nphase_aer !BSINGH - Commented out as we need to add only phase 1 (interstitial) values species

   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a01(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(1,itype,iphase)))

          bboa_a01(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(1,itype,iphase)))

          soa_a01(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase)))

        arosoa_a01(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))


        bbsoa_a01(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)))

        hsoa_a01(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)))

        biog_a01(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase)))




               enddo
            enddo
         enddo
   enddo ! type


   !do iphase=1,nphase_aer!BSINGH - Commented out as we need to add only phase 1 (interstitial) values species
   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a02(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(2,itype,iphase)))

          bboa_a02(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(2,itype,iphase)))

          soa_a02(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase)))

        arosoa_a02(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))


        bbsoa_a02(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)))

        hsoa_a02(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)))

        biog_a02(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type


   !do iphase=1,nphase_aer!BSINGH - Commented out as we need to add only phase 1 (interstitial) values species
   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a03(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(3,itype,iphase)))

          bboa_a03(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(3,itype,iphase)))

          soa_a03(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase)))

        arosoa_a03(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))


        bbsoa_a03(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)))

        hsoa_a03(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)))

        biog_a03(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type


   !do iphase=1,nphase_aer!BSINGH - Commented out as we need to add only phase 1 (interstitial) values species
   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a04(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(4,itype,iphase)))

          bboa_a04(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(4,itype,iphase)))

          soa_a04(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase)))

        arosoa_a04(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))


        bbsoa_a04(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)))

        hsoa_a04(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)))

        biog_a04(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type
   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a05(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(5,itype,iphase)))

          bboa_a05(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(5,itype,iphase)))

          soa_a05(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(5,itype,iphase)))

        arosoa_a05(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))


        bbsoa_a05(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)))

        hsoa_a05(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)))

        biog_a05(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(5,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type


   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a06(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(6,itype,iphase)))

          bboa_a06(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(6,itype,iphase)))

          soa_a06(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(6,itype,iphase)))

        arosoa_a06(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))


        bbsoa_a06(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)))

        hsoa_a06(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)))

        biog_a06(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(6,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type


   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a07(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(7,itype,iphase)))

          bboa_a07(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(7,itype,iphase)))

          soa_a07(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(7,itype,iphase)))

        arosoa_a07(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))


        bbsoa_a07(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)))

        hsoa_a07(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)))

        biog_a07(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(7,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type


   iphase = 1
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a08(i,k,j)= (chem(i,k,j,lptr_pcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(8,itype,iphase)))

          bboa_a08(i,k,j)= (chem(i,k,j,lptr_pcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(8,itype,iphase)))

          soa_a08(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(8,itype,iphase)))

        arosoa_a08(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))


        bbsoa_a08(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)))

        hsoa_a08(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)))

        biog_a08(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(8,itype,iphase)))

               enddo
            enddo
         enddo
   enddo ! type
   endif


   end subroutine sum_vbs2


!BSINGH(12/12/2013): Added following function for SAPRC 8 bin
   !----------------------------------------------------------------------


       subroutine  sum_aq_vbs2 (                                          &
             alt, chem,                                                &
             hoa_cw01,hoa_cw02,hoa_cw03,hoa_cw04,hoa_cw05,hoa_cw06,hoa_cw07,hoa_cw08,                          &
             bboa_cw01,bboa_cw02,bboa_cw03,bboa_cw04,bboa_cw05,bboa_cw06,bboa_cw07,bboa_cw08,                  &
             soa_cw01,soa_cw02,soa_cw03,soa_cw04,soa_cw05,soa_cw06,soa_cw07,soa_cw08,                          &
             bbsoa_cw01,bbsoa_cw02,bbsoa_cw03,bbsoa_cw04,bbsoa_cw05,bbsoa_cw06,bbsoa_cw07,bbsoa_cw08,          &
             hsoa_cw01,hsoa_cw02,hsoa_cw03,hsoa_cw04,hsoa_cw05,hsoa_cw06,hsoa_cw07,hsoa_cw08,                  &
             biog_cw01,biog_cw02,biog_cw03,biog_cw04,biog_cw05,biog_cw06,biog_cw07,biog_cw08,                  &
             arosoa_cw01,arosoa_cw02,arosoa_cw03,arosoa_cw04,arosoa_cw05,arosoa_cw06,arosoa_cw07,arosoa_cw08,  &
             totoa_cw01,totoa_cw02,totoa_cw03,totoa_cw04,totoa_cw05,totoa_cw06,totoa_cw07,totoa_cw08,          &
             hsoa_cw_c,hsoa_cw_o,bbsoa_cw_c,bbsoa_cw_o,                            &
             biog_cw_v1,                                                  &
             ant_cw_v1,                                                   &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte                                  )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: hoa_cw01,hoa_cw02,hoa_cw03,hoa_cw04,               &
                        hoa_cw05,hoa_cw06,hoa_cw07,hoa_cw08,               & 
             bboa_cw01,bboa_cw02,bboa_cw03,bboa_cw04,                      &
             bboa_cw05,bboa_cw06,bboa_cw07,bboa_cw08,                      &
             soa_cw01,soa_cw02,soa_cw03,soa_cw04,                          &
             soa_cw05,soa_cw06,soa_cw07,soa_cw08,                          &
             bbsoa_cw01,bbsoa_cw02,bbsoa_cw03,bbsoa_cw04,                  &
             bbsoa_cw05,bbsoa_cw06,bbsoa_cw07,bbsoa_cw08,                  &
             biog_cw01,biog_cw02,biog_cw03,biog_cw04,                      &
             biog_cw05,biog_cw06,biog_cw07,biog_cw08,                      &
             hsoa_cw01,hsoa_cw02,hsoa_cw03,hsoa_cw04,                      &
             hsoa_cw05,hsoa_cw06,hsoa_cw07,hsoa_cw08,                      &
             arosoa_cw01,arosoa_cw02,arosoa_cw03,arosoa_cw04,              &
             arosoa_cw05,arosoa_cw06,arosoa_cw07,arosoa_cw08,              &
             totoa_cw01,totoa_cw02,totoa_cw03,totoa_cw04,                  &
             totoa_cw05,totoa_cw06,totoa_cw07,totoa_cw08,                  &
             hsoa_cw_c,hsoa_cw_o,bbsoa_cw_c,bbsoa_cw_o,                    &
             biog_cw_v1,                                                  &
             ant_cw_v1



   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte

    hoa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw01(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw02(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw02(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw03(its:imax,kts:kmax,jts:jmax)    = 0.
    hoa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw04(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw04(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw05(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw05(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw06(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw06(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw07(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw07(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_cw08(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_cw08(its:imax,kts:kmax,jts:jmax)    = 0.



! Species to calculate O:C ratios
     hsoa_cw_c(its:imax,kts:kmax,jts:jmax)    = 0.
     hsoa_cw_o(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_cw_c(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_cw_o(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_cw_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_cw_v1(its:imax,kts:kmax,jts:jmax)    = 0.


   !do iphase=2,2 !set nphase_aer=2 for cloud-borne aerosols !BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype) !The 4th bin is 2.5-10um and outside the AMS measurements
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax

          hsoa_cw_c(i,k,j)=hsoa_cw_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)))*180/211

         hsoa_cw_o(i,k,j)= hsoa_cw_o(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)))

          bbsoa_cw_c(i,k,j)= bbsoa_cw_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)))*180/211

         bbsoa_cw_o(i,k,j)=bbsoa_cw_o(i,k,j) &
                       +(chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)))

        biog_cw_v1(i,k,j)= biog_cw_v1(i,k,j) &
                         + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))

        ant_cw_v1(i,k,j)=  ant_cw_v1(i,k,j) &
                         + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))

                 enddo
               enddo
             enddo
                    enddo
                  enddo





   !do iphase=2,2 !set nphase_aer=2 for cloud-borne aerosols !BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw01(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase))

          bboa_cw01(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase))

          soa_cw01(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))

        arosoa_cw01(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))


        bbsoa_cw01(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)))

        hsoa_cw01(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)))

        biog_cw01(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))



         totoa_cw01(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase)))


               enddo
            enddo
         enddo
   enddo ! type



   !do iphase=2,2 !set nphase_aer=2 for cloud-borne aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw02(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase))

          bboa_cw02(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase))

          soa_cw02(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))

        arosoa_cw02(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))


        bbsoa_cw02(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)))

        hsoa_cw02(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)))

        biog_cw02(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))



         totoa_cw02(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(2,itype,iphase)))


               enddo
            enddo
         enddo
   enddo ! type



   !do iphase=2,2 !set nphase_aer=2 for cloud-borne aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw03(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase))

          bboa_cw03(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase))

          soa_cw03(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))

        arosoa_cw03(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))


        bbsoa_cw03(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)))

        hsoa_cw03(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)))

        biog_cw03(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))



         totoa_cw03(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase)))


               enddo
            enddo
         enddo
   enddo ! type



   !do iphase=2,2 !set nphase_aer=2 for cloud-borne aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw04(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase))

          bboa_cw04(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase))

          soa_cw04(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))

        arosoa_cw04(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))


        bbsoa_cw04(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)))

        hsoa_cw04(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)))

        biog_cw04(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))



         totoa_cw04(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase)))


               enddo
            enddo
         enddo
   enddo ! type
   

   !do iphase=2,2 !set nphase_aer=1 for interstitial aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         if(nsize_aer(itype).ge.5) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw05(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(5,itype,iphase))

          bboa_cw05(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(5,itype,iphase))

          soa_cw05(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))

        arosoa_cw05(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))


        bbsoa_cw05(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)))

        hsoa_cw05(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)))

        biog_cw05(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))



         totoa_cw05(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(5,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(5,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(5,itype,iphase)))


               enddo
            enddo
         enddo
        endif  ! check for number of size bins
   enddo ! type
   


   !do iphase=2,2 !set nphase_aer=1 for interstitial aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         if(nsize_aer(itype).ge.6) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw06(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(6,itype,iphase))

          bboa_cw06(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(6,itype,iphase))

          soa_cw06(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))

        arosoa_cw06(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))


        bbsoa_cw06(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)))

        hsoa_cw06(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)))

        biog_cw06(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))



         totoa_cw06(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(6,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(6,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(6,itype,iphase)))


               enddo
            enddo
         enddo
         endif ! size bins
   enddo ! type
   

   !do iphase=2,2 !set nphase_aer=1 for interstitial aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
        if(nsize_aer(itype).ge.7) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw07(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(7,itype,iphase))

          bboa_cw07(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(7,itype,iphase))

          soa_cw07(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))

        arosoa_cw07(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))


        bbsoa_cw07(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)))

        hsoa_cw07(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)))

        biog_cw07(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))



         totoa_cw07(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(7,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(7,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(7,itype,iphase)))


               enddo
            enddo
         enddo
          endif ! size bins
   enddo ! type
   

   !do iphase=2,2 !set nphase_aer=1 for interstitial aerosols!BSINGH - Commented out 
   iphase = 2
   do itype=1,ntype_aer
         if(nsize_aer(itype).ge.8) then
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_cw08(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(8,itype,iphase))

          bboa_cw08(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(8,itype,iphase))

          soa_cw08(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))

        arosoa_cw08(i,k,j)= chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))


        bbsoa_cw08(i,k,j)= (chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)))

        hsoa_cw08(i,k,j)= ( chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)))

        biog_cw08(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))



         totoa_cw08(i,k,j)= ( chem(i,k,j,lptr_pcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(8,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(8,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(8,itype,iphase)))


               enddo
            enddo
         enddo
           endif ! size bins
   enddo ! type
   





   end subroutine sum_aq_vbs2

!BSINGH -ENDS

!----------------------------------------------------------------------

!-----------------------------------------------------------------------


       subroutine  sum_vbs4 ( aero_diag_opt,                           &
             alt, chem,                                                &
             hoa_a01,hoa_a02,hoa_a03,hoa_a04,                          &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4,                              &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte                                  )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::  aero_diag_opt
   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: hoa_a01,hoa_a02,hoa_a03,hoa_a04,               &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4



   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte

    totoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.

   if( aero_diag_opt > 0 ) then
    hoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a01(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a02(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a03(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a04(its:imax,kts:kmax,jts:jmax)    = 0.

! Species to calculate O:C ratios
     biog_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v4(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v4(its:imax,kts:kmax,jts:jmax)    = 0.
   endif

   do iphase=1,nphase_aer
      do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
                 totoa_a01(i,k,j)= totoa_a01(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase))
                 totoa_a02(i,k,j)= totoa_a02(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(2,itype,iphase))
                 totoa_a03(i,k,j)= totoa_a03(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase))
                 totoa_a04(i,k,j)= totoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase))
               enddo
            enddo
         enddo
      enddo ! type
   enddo ! phase

   if( aero_diag_opt > 0 ) then
   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype) !The 4th bin is 2.5-10um and outside the AMS measurements
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax

        biog_v1(i,k,j)= biog_v1(i,k,j) &
                         + chem(i,k,j,lptr_bsoaX_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_bsoa1_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_bsoa2_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_bsoa3_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_bsoa4_aer(n,itype,iphase))

        ant_v1(i,k,j)=  ant_v1(i,k,j) &
                         + chem(i,k,j,lptr_asoaX_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_asoa1_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_asoa2_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_asoa3_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_asoa4_aer(n,itype,iphase))

                 enddo
               enddo
             enddo
                    enddo
                  enddo
               enddo

      biog_v1(its:imax,kts:kmax,jts:jmax) = biog_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      ant_v1(its:imax,kts:kmax,jts:jmax) = ant_v1(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
        hoa_a01(i,k,j)= hoa_a01(i,k,j) &
                       +chem(i,k,j,lptr_oc_aer(1,itype,iphase))

        soa_a01(i,k,j)= soa_a01(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(1,itype,iphase))

        biog_a01(i,k,j)= biog_a01(i,k,j) &
                       + chem(i,k,j,lptr_bsoa1_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(1,itype,iphase))
               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
        hoa_a02(i,k,j)= hoa_a02(i,k,j) &
                       +chem(i,k,j,lptr_oc_aer(2,itype,iphase))

        soa_a02(i,k,j)= soa_a02(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(2,itype,iphase))

        biog_a02(i,k,j)= biog_a02(i,k,j) &
                       + chem(i,k,j,lptr_bsoa1_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(2,itype,iphase))
               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
        hoa_a03(i,k,j)= hoa_a03(i,k,j) &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase))

        soa_a03(i,k,j)= soa_a03(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(3,itype,iphase))

        biog_a03(i,k,j)= biog_a03(i,k,j) &
                       + chem(i,k,j,lptr_bsoa1_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(3,itype,iphase))
               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
        hoa_a04(i,k,j)= hoa_a04(i,k,j) &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase))

        soa_a04(i,k,j)= soa_a04(i,k,j) &
                       + chem(i,k,j,lptr_asoaX_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa3_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_asoa4_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoaX_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_r2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_sfc_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_oh_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_glysoa_nh4_aer(4,itype,iphase))

        biog_a04(i,k,j)= biog_a04(i,k,j) &
                       + chem(i,k,j,lptr_bsoa1_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa2_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa3_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_bsoa4_aer(4,itype,iphase))
               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase
   endif

!Factor of 1.4 used below to convert OC to OA
        totoa_a01(its:imax,kts:kmax,jts:jmax) =totoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a02(its:imax,kts:kmax,jts:jmax) =totoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a03(its:imax,kts:kmax,jts:jmax) =totoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        totoa_a04(its:imax,kts:kmax,jts:jmax) =totoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      if( aero_diag_opt > 0 ) then
        hoa_a01(its:imax,kts:kmax,jts:jmax) =hoa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a01(its:imax,kts:kmax,jts:jmax) =soa_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        biog_a01(its:imax,kts:kmax,jts:jmax) =biog_a01(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        hoa_a02(its:imax,kts:kmax,jts:jmax) =hoa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a02(its:imax,kts:kmax,jts:jmax) =soa_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        biog_a02(its:imax,kts:kmax,jts:jmax) =biog_a02(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)


        hoa_a03(its:imax,kts:kmax,jts:jmax) =hoa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a03(its:imax,kts:kmax,jts:jmax) =soa_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        biog_a03(its:imax,kts:kmax,jts:jmax) =biog_a03(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)

        hoa_a04(its:imax,kts:kmax,jts:jmax) =hoa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        soa_a04(its:imax,kts:kmax,jts:jmax) =soa_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
        biog_a04(its:imax,kts:kmax,jts:jmax) =biog_a04(its:imax,kts:kmax,jts:jmax) &
                                           / alt(its:imax,kts:kmax,jts:jmax)
      endif

   end subroutine sum_vbs4





!----------------------------------------------------------------------
      subroutine  sum_vbs9 (                                           &
             alt, chem,                                                &
             hoa_a01,hoa_a02,hoa_a03,hoa_a04,                          &
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             hsoa_a01,hsoa_a02,hsoa_a03,hsoa_a04,                      &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,              &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             hsoa_c,hsoa_o,bbsoa_c,bbsoa_o,                            &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4,                              &
             ids,ide, jds,jde, kds,kde,                                &
             ims,ime, jms,jme, kms,kme,                                &
             its,ite, jts,jte, kts,kte                                  )

   USE module_state_description, only: num_chem
   USE module_data_mosaic_asect
   IMPLICIT NONE

   INTEGER,      INTENT(IN   )    ::                                   &
                                      ids,ide, jds,jde, kds,kde,       &
                                      ims,ime, jms,jme, kms,kme,       &
                                      its,ite, jts,jte, kts,kte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(IN) :: alt

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(IN ) :: chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(OUT) :: hoa_a01,hoa_a02,hoa_a03,hoa_a04,               &
             bboa_a01,bboa_a02,bboa_a03,bboa_a04,                      &
             soa_a01,soa_a02,soa_a03,soa_a04,                          &
             bbsoa_a01,bbsoa_a02,bbsoa_a03,bbsoa_a04,                  &
             biog_a01,biog_a02,biog_a03,biog_a04,                      &
             hsoa_a01,hsoa_a02,hsoa_a03,hsoa_a04,                      &
             arosoa_a01,arosoa_a02,arosoa_a03,arosoa_a04,                      &
             totoa_a01,totoa_a02,totoa_a03,totoa_a04,                  &
             hsoa_c,hsoa_o,bbsoa_c,bbsoa_o,                            &
             biog_v1,biog_v2,biog_v3,biog_v4,                          &
             ant_v1,ant_v2,ant_v3,ant_v4



   INTEGER :: i,imax,j,jmax,k,kmax,n,itype,iphase

   imax = min(ite,ide-1)
   jmax = min(jte,jde-1)
   kmax = kte

    hoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a01(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a02(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a02(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a03(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a03(its:imax,kts:kmax,jts:jmax)    = 0.

    hoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    soa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bboa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    bbsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    hsoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    arosoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    biog_a04(its:imax,kts:kmax,jts:jmax)    = 0.
    totoa_a04(its:imax,kts:kmax,jts:jmax)    = 0.

! Species to calculate O:C ratios
     hsoa_c(its:imax,kts:kmax,jts:jmax)    = 0.
     hsoa_o(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_c(its:imax,kts:kmax,jts:jmax)    = 0.
     bbsoa_o(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     biog_v4(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v1(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v2(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v3(its:imax,kts:kmax,jts:jmax)    = 0.
     ant_v4(its:imax,kts:kmax,jts:jmax)    = 0.


   do iphase=1,nphase_aer
   do itype=1,ntype_aer
   do n = 1, nsize_aer(itype) !The 4th bin is 2.5-10um and outside the AMS measurements
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax

          hsoa_c(i,k,j)=hsoa_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(n,itype,iphase)))*180/211 

         hsoa_o(i,k,j)= hsoa_o(i,k,j) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(n,itype,iphase)) 

          bbsoa_c(i,k,j)= bbsoa_c(i,k,j) &
                       + (chem(i,k,j,lptr_pcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(n,itype,iphase)))*180/211 

         bbsoa_o(i,k,j)=bbsoa_o(i,k,j) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(n,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(n,itype,iphase)) 

        biog_v1(i,k,j)= biog_v1(i,k,j) &
                         + chem(i,k,j,lptr_biog1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog1_o_aer(n,itype,iphase))
        biog_v2(i,k,j)=  biog_v2(i,k,j) &
                         + chem(i,k,j,lptr_biog2_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog2_o_aer(n,itype,iphase))
        biog_v3(i,k,j)=  biog_v3(i,k,j) &
                         + chem(i,k,j,lptr_biog3_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog3_o_aer(n,itype,iphase))
        biog_v4(i,k,j)=  biog_v4(i,k,j) &
                         +chem(i,k,j,lptr_biog4_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_biog4_o_aer(n,itype,iphase))

        ant_v1(i,k,j)=  ant_v1(i,k,j) &
                         + chem(i,k,j,lptr_ant1_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant1_o_aer(n,itype,iphase))
        ant_v2(i,k,j)=  ant_v2(i,k,j) &
                         + chem(i,k,j,lptr_ant2_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant2_o_aer(n,itype,iphase))
        ant_v3(i,k,j)=   ant_v3(i,k,j) &
                         + chem(i,k,j,lptr_ant3_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant3_o_aer(n,itype,iphase))
        ant_v4(i,k,j)=  ant_v4(i,k,j) &
                         + chem(i,k,j,lptr_ant4_c_aer(n,itype,iphase))    &
                         + chem(i,k,j,lptr_ant4_o_aer(n,itype,iphase))

                 enddo
               enddo
             enddo
                    enddo
                  enddo
               enddo

! NOTE - summation also wrong for code below, but not my code so did not fix...

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a01(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(1,itype,iphase)) 

          bboa_a01(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(1,itype,iphase)) 

          soa_a01(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(1,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(1,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(1,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(1,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(1,itype,iphase))

        arosoa_a01(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(1,itype,iphase))



        bbsoa_a01(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(1,itype,iphase)) 

        hsoa_a01(i,k,j)= chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(1,itype,iphase)) 

        biog_a01(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(1,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(1,itype,iphase))



         totoa_a01(i,k,j)=  chem(i,k,j,lptr_pcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(1,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(1,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(1,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(1,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(1,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(1,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(1,itype,iphase))


               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a02(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(2,itype,iphase)) 

          bboa_a02(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(2,itype,iphase)) 

          soa_a02(i,k,j)=     chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(2,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(2,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(2,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(2,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(2,itype,iphase))

        arosoa_a02(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(2,itype,iphase))


        bbsoa_a02(i,k,j)= chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(2,itype,iphase)) 

        hsoa_a02(i,k,j)=  chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(2,itype,iphase)) 


       biog_a02(i,k,j)= chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(2,itype,iphase))


         totoa_a02(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(2,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(2,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(2,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a03(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(3,itype,iphase)) 

          bboa_a03(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(3,itype,iphase)) 

          soa_a03(i,k,j)=  chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(3,itype,iphase))

       arosoa_a03(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(3,itype,iphase))


        bbsoa_a03(i,k,j)=  chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(3,itype,iphase)) 

        hsoa_a03(i,k,j)=   chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(3,itype,iphase)) 

       biog_a03(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(3,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(3,itype,iphase))


         totoa_a03(i,k,j)=  chem(i,k,j,lptr_pcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(3,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(3,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(3,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(3,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(3,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(3,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(3,itype,iphase))

               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase

   do iphase=1,nphase_aer
   do itype=1,ntype_aer
         do j=jts,jmax
            do k=kts,kmax
               do i=its,imax
         hoa_a04(i,k,j)= chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(4,itype,iphase)) 

          bboa_a04(i,k,j)= chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(4,itype,iphase)) 

          soa_a04(i,k,j)=  chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(4,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(4,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(4,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(4,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(4,itype,iphase))

       arosoa_a04(i,k,j)= chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))   &
                       + chem(i,k,j,lptr_ant2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(4,itype,iphase))


        bbsoa_a04(i,k,j)=  chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(4,itype,iphase)) 

        hsoa_a04(i,k,j)=  chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(4,itype,iphase)) 

        biog_a04(i,k,j)= (chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(4,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(4,itype,iphase))



         totoa_a04(i,k,j)=  chem(i,k,j,lptr_pcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_b_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg3_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg4_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg5_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg6_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_pcg7_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_c_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg1_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg2_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg3_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg4_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg5_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg6_f_o_aer(4,itype,iphase)) &
                       + chem(i,k,j,lptr_opcg7_f_o_aer(4,itype,iphase)) &
                       + (chem(i,k,j,lptr_ant1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_c_aer(4,itype,iphase)))    &
                       + chem(i,k,j,lptr_ant1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_ant4_o_aer(4,itype,iphase))    &
                       + (chem(i,k,j,lptr_biog1_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_c_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_c_aer(4,itype,iphase)))    &
                       + chem(i,k,j,lptr_biog1_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog2_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog3_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_biog4_o_aer(4,itype,iphase))    &
                       + chem(i,k,j,lptr_oc_aer(4,itype,iphase))


               enddo
            enddo
         enddo
   enddo ! type
   enddo ! phase


   end subroutine sum_vbs9


!-----------------------------------------------------------------------
	end module module_mosaic_sumpm

