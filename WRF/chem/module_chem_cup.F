! *** changes needed involving soa
!
! include soa in calculation of oa_a_1to4_ic_cup and oa_cw_1to4_ic_cup
!
! account for soa hygroscopicity for droplet activation (??)


!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use

! *** NOTE ***
! This module should in theory work with any chem_opt package that uses MOSAIC 
!and has cloud-borne aerosols. However, it was only tested with chem_opt=203 
!(saprc99_mosaic_8bin_vbs2_aq_kpp) during implementation into WRF-Chem in 
!April 2017 at PNNL.


!**********************************************************************************  

!  file module_chem_cup.F
      module module_chem_cup


      implicit none


      private

      public :: chem_cup_driver

      logical, parameter :: do_deep = .true.

      integer, parameter :: r4=4
      integer, parameter :: r8=8

      integer, parameter :: mode_chemcup_timeinteg = 2
      ! when mode_chemcup_timeinteg = 1, simulate the entire cumulus cloud fraction 
      !    with multiple time substeps of dt = dtstepc
      ! when mode_chemcup_timeinteg = 2, simulate the entire cumulus cloud fraction 
      !    with a single timestep of dt = tcloud_cup, then set tcloud_cup negative 
      !    to signify that chem_cup calcs. are done for this cloud
      ! 
      ! *** note 1 - mode_chemcup_timeinteg = 2 is recommended
      ! *** note 2 - mode_chemcup_timeinteg = 1 may need lower values for the various ..._smallaa parameters

      real(r8), parameter :: cldfra_ls_testvalue = -0.5
      ! *** used for testing *** -- when cldfra_ls_testvalue >= 0.0, it overrides cldfra 
      real(r8), parameter :: wact_cup_testvalue = -0.1
      ! *** used for testing *** -- when wact_cup_testvalue > 0.0, it overrides wact_cup

      real(r8), parameter :: air_outflow_limit = 0.90_r8
      ! maximum faction of layer mass that can be transported out (to other layers) in a transport sub-time-step

      real(r8), parameter :: af_cucld_smallaa = 0.003_r8  ! "cutoff" value for cumulus cloud fractional area
      real(r8), parameter :: af_cucld_maxaa = 0.8_r8      !  maximum value for cumulus cloud fractional area

! subarea-average vertical mass fluxes (kg/m2/s) smaller than
!    aw_up_smallaa*rhoair are treated as zero
! note that with a*w = 3e-5 m/s, dz over 1 hour = 0.11 m which is small
      real(r8), parameter :: aw_up_smallaa = 3.0e-5_r8   ! m/s
! maximum expected updraft
      real(r8), parameter :: w_up_maxaa = 50.0_r8   ! m/s
! updraft fractional areas below af_up_smallaa are ignored
      real(r8), parameter :: af_up_smallaa = aw_up_smallaa/w_up_maxaa

      real(r8), parameter :: af_up_maxaa = 0.2_r8        !  maximum value for cumulus updraft fractional area

      real(r8), parameter :: qci_incu_smallaa = 1.0e-6_r8  ! "cutoff" value for in-cloud   cloud-ice   (kg/kg)
      real(r8), parameter :: qci_inup_smallaa = 1.0e-6_r8  ! "cutoff" value for in-updraft cloud-ice   (kg/kg)
      real(r8), parameter :: qcw_incu_smallaa = 1.0e-6_r8  ! "cutoff" value for in-cloud   cloud-water (kg/kg)
      real(r8), parameter :: qcw_inup_smallaa = 1.0e-5_r8  ! "cutoff" value for in-updraft cloud-water (kg/kg)

      real(r8), parameter :: tau_active_smallaa   = 30.0_r8  ! "cutoff" value for active cloud calculations (s) 
      real(r8), parameter :: tau_inactive_smallaa = 30.0_r8  ! "cutoff" value for inactive cloud calculations (s) 


      contains


!----------------------------------------------------------------------
      subroutine chem_cup_driver(                                     &
            grid_id, ktau, ktauc, dtstep, dtstepc, config_flags,      &
            t_phy, p_phy, rho_phy, alt, dz8w, zmid, z_at_w,           &
            moist, cldfra, ph_no2,                                    &
            chem,                                                     &
            chem_cupflag, cupflag, shall, tcloud_cup, nca, wact_cup,  &
            cldfra_cup, updfra_cup, qc_ic_cup, qc_iu_cup,             &
            mfup_cup, mfup_ent_cup, mfdn_cup, mfdn_ent_cup,           &
            fcvt_qc_to_pr_cup, fcvt_qc_to_qi_cup, fcvt_qi_to_pr_cup,  &
            co_a_ic_cup, hno3_a_ic_cup,                               &
            so4_a_1to4_ic_cup, so4_cw_1to4_ic_cup,                      &
            nh4_a_1to4_ic_cup, nh4_cw_1to4_ic_cup,                      &
            no3_a_1to4_ic_cup, no3_cw_1to4_ic_cup,                      &
            oa_a_1to4_ic_cup,  oa_cw_1to4_ic_cup,                       &
            oin_a_1to4_ic_cup,  oin_cw_1to4_ic_cup,                     &
            bc_a_1to4_ic_cup,  bc_cw_1to4_ic_cup,                       &
            na_a_1to4_ic_cup,  na_cw_1to4_ic_cup,                       &
            cl_a_1to4_ic_cup,  cl_cw_1to4_ic_cup,                       &
            water_1to4_ic_cup,                                          &
            so4_a_5to6_ic_cup, so4_cw_5to6_ic_cup,                      &
            nh4_a_5to6_ic_cup, nh4_cw_5to6_ic_cup,                      &
            no3_a_5to6_ic_cup, no3_cw_5to6_ic_cup,                      &
            oa_a_5to6_ic_cup,  oa_cw_5to6_ic_cup,                       &
            oin_a_5to6_ic_cup,  oin_cw_5to6_ic_cup,                     &
            bc_a_5to6_ic_cup,  bc_cw_5to6_ic_cup,                       &
            na_a_5to6_ic_cup,  na_cw_5to6_ic_cup,                       &
            cl_a_5to6_ic_cup,  cl_cw_5to6_ic_cup,                       &
            water_5to6_ic_cup,                                          &
            ids,ide, jds,jde, kds,kde,                                &
            ims,ime, jms,jme, kms,kme,                                &
            its,ite, jts,jte, kts,kte                                 )
!
!  convective cloud processing (vertical transport, 
!     activation/resuspension, cloud chemistry, wet removal eventually)
!     by **cup** convective clouds
!
!  currently only works with following chem_opt packages
!     CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )
!  currently does not do wet removal
!

      use module_configure
      use module_state_description
      use module_model_constants
      use module_scalar_tables, only:  chem_dname_table

!----------------------------------------------------------------------
! arguments

      type(grid_config_rec_type), intent(in) :: config_flags

      integer, intent(in) ::              &
            ids,ide, jds,jde, kds,kde,    &
            ims,ime, jms,jme, kms,kme,    &
            its,ite, jts,jte, kts,kte

      integer, intent(in) ::              &
            grid_id, ktau, ktauc

      integer, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: &
            chem_cupflag               !  1 if cupflag=.true. and chem_cup calcs were successful
                                       ! -1 if cupflag=.true. and chem_cup calcs failed
                                       !  0 if cupflag=.false.

      real, intent(in) :: dtstep, dtstepc  ! model and chemistry time-steps (s)

      real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
            t_phy,                   & ! temp (K)
            p_phy,                   & ! pressure (Pa)
            rho_phy,                 & ! moist air density (kg/m3)
            alt,                     & ! dry air 1/density (m3/kg)
            dz8w,                    & ! layer thickness (m)
            zmid,                    & ! height at layer center (m)
            z_at_w,                  & ! height at layer boundary (m)
            cldfra,                  & ! grid-resolved cloud fraction
            ph_no2                     ! no2 photolysis rate (1/s)

! advected moisture variables
      real, dimension( ims:ime, kms:kme, jms:jme, num_moist ), intent(in) :: &
            moist

! all advected chemical species
      real, dimension( ims:ime, kms:kme, jms:jme, num_chem ), intent(inout) :: &
            chem

      logical, dimension( ims:ime, jms:jme ), intent(in) :: &
            cupflag                    ! .true. if cup convection is present in column

      real, dimension( ims:ime, jms:jme ), intent(in) :: &
            nca,                     & ! time remaining for this cloud (s)
            shall,                   & ! cumulus type:  0=deep,  1=shallow,  2=none
            wact_cup                   ! vertical velocity for cloud-base aerosol activation (m/s)

      real, dimension( ims:ime, jms:jme ), intent(inout) :: &
            tcloud_cup                 ! cumulus cloud duration (s)

      real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
            cldfra_cup,              & ! cumulus cloud fractional area (-)
            updfra_cup,              & ! cumulus updraft fractional area (-)
            qc_ic_cup,               & ! cloud-water in cumulus cloud (kg/kg)
            qc_iu_cup,               & ! cloud-water in updraft (kg/kg)
            mfup_cup,                & ! updraft mass-flux (kg/m2/s)
            mfup_ent_cup,            & ! updraft mass-flux change from entrainment (kg/m2/s)
            mfdn_cup,                & ! downdraft mass-flux (kg/m2/s)
            mfdn_ent_cup,            & ! downdraft mass-flux change from entrainment (kg/m2/s)
            fcvt_qc_to_pr_cup,       & ! fraction of cloud-water converted to precip    as air move thru updraft layer (-)
            fcvt_qc_to_qi_cup,       & ! fraction of cloud-water converted to cloud-ice as air move thru updraft layer (-)
            fcvt_qi_to_pr_cup          ! fraction of cloud-ice   converted to precip    as air move thru updraft layer (-)

! interstitial and cloudborne mixing ratios within the convective cloud,
! summed over size bins that the ams can see (Dp < 625 nm)
      real, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: &
            co_a_ic_cup, hno3_a_ic_cup,               &
            so4_a_1to4_ic_cup, so4_cw_1to4_ic_cup,  &
            nh4_a_1to4_ic_cup, nh4_cw_1to4_ic_cup,  &
            no3_a_1to4_ic_cup, no3_cw_1to4_ic_cup,  &
            oa_a_1to4_ic_cup,  oa_cw_1to4_ic_cup,   &
            oin_a_1to4_ic_cup,  oin_cw_1to4_ic_cup,   &
            bc_a_1to4_ic_cup,  bc_cw_1to4_ic_cup,   &
            na_a_1to4_ic_cup,  na_cw_1to4_ic_cup,   &
            cl_a_1to4_ic_cup,  cl_cw_1to4_ic_cup,   &
            water_1to4_ic_cup,                       &
            so4_a_5to6_ic_cup, so4_cw_5to6_ic_cup,  &
            nh4_a_5to6_ic_cup, nh4_cw_5to6_ic_cup,  &
            no3_a_5to6_ic_cup, no3_cw_5to6_ic_cup,  &
            oa_a_5to6_ic_cup,  oa_cw_5to6_ic_cup,   &
            oin_a_5to6_ic_cup,  oin_cw_5to6_ic_cup,   &
            bc_a_5to6_ic_cup,  bc_cw_5to6_ic_cup,   &
            na_a_5to6_ic_cup,  na_cw_5to6_ic_cup,   &
            cl_a_5to6_ic_cup,  cl_cw_5to6_ic_cup,   &
            water_5to6_ic_cup  



!-----------------------------------------------------------------
! local variables
      integer :: iok
      integer :: ii, jj, kk

      character(len=12) :: chem_name(num_chem)

!-----------------------------------------------------------------

! check for correct options
      if ( config_flags%chem_conv_tr <= 0 .or. &
           config_flags%cu_physics /= kfcupscheme ) then
         call wrf_debug( 15, 'chem_cup_driver skipped because - ' // &
                             'chem_conv_tr or cu_physics' )
         return
      end if

      chem_name(1:num_chem) = chem_dname_table(grid_id,1:num_chem)


      chem_opt_select: select case(config_flags%chem_opt)

         case ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
                SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP ) !BSINGH(12/11/13): Got rid of SAPRC99_MOSAIC_4BIN_VBS2_AQ_KPP pkg statement
!-------------------------------------------------------------------------------
! Below lines commented by Manish Shrivastava to skip CUP chemistry
!------------------------------------------------------------------------------
            call wrf_debug(15, &
               'chem_cup_driver calling mosaic_chem_cup_driver')
            call mosaic_chem_cup_driver(                                    &
                  grid_id, ktau, ktauc, dtstep, dtstepc, config_flags,      &
                  t_phy, p_phy, rho_phy, alt, dz8w, zmid, z_at_w,           &
                  moist, cldfra, ph_no2,                                    &
                  chem, chem_name,                                          &
                  chem_cupflag, cupflag, shall, tcloud_cup, nca, wact_cup,  &
                  cldfra_cup, updfra_cup, qc_ic_cup, qc_iu_cup,             &
                  mfup_cup, mfup_ent_cup, mfdn_cup, mfdn_ent_cup,           &
                  fcvt_qc_to_pr_cup, fcvt_qc_to_qi_cup, fcvt_qi_to_pr_cup,  &
                  co_a_ic_cup, hno3_a_ic_cup,                               &
                  so4_a_1to4_ic_cup, so4_cw_1to4_ic_cup,                      &
                  nh4_a_1to4_ic_cup, nh4_cw_1to4_ic_cup,                      &
                  no3_a_1to4_ic_cup, no3_cw_1to4_ic_cup,                      &
                  oa_a_1to4_ic_cup,  oa_cw_1to4_ic_cup,                       &
                  oin_a_1to4_ic_cup,  oin_cw_1to4_ic_cup,                     &
                  bc_a_1to4_ic_cup,  bc_cw_1to4_ic_cup,                       &
                  na_a_1to4_ic_cup,  na_cw_1to4_ic_cup,                       &
                  cl_a_1to4_ic_cup,  cl_cw_1to4_ic_cup,                       &
                  water_1to4_ic_cup,                                          &
                  so4_a_5to6_ic_cup, so4_cw_5to6_ic_cup,                      &
                  nh4_a_5to6_ic_cup, nh4_cw_5to6_ic_cup,                      &
                  no3_a_5to6_ic_cup, no3_cw_5to6_ic_cup,                      &
                  oa_a_5to6_ic_cup,  oa_cw_5to6_ic_cup,                       &
                  oin_a_5to6_ic_cup,  oin_cw_5to6_ic_cup,                     &
                  bc_a_5to6_ic_cup,  bc_cw_5to6_ic_cup,                       &
                  na_a_5to6_ic_cup,  na_cw_5to6_ic_cup,                       &
                  cl_a_5to6_ic_cup,  cl_cw_5to6_ic_cup,                       &
                  water_5to6_ic_cup,                                          &
                  ids,ide, jds,jde, kds,kde,                                &
                  ims,ime, jms,jme, kms,kme,                                &
                  its,ite, jts,jte, kts,kte, kte+1                          )
!-----------------------------------------------------------------------------------------
! Above lines commented by Manish Shrivastava to skip CUP chemistry
!-------------------------------------------------------------------------------------------
         case default
            chem_cupflag = 0
            call wrf_debug( 15, 'chem_cup_driver skipped because - ' // &
                                'chem_opt' )

      end select chem_opt_select

      return
      end subroutine chem_cup_driver


!----------------------------------------------------------------------
      subroutine mosaic_chem_cup_driver(                              &
            grid_id, ktau, ktauc, dtstep, dtstepc, config_flags,      &
            t_phy, p_phy, rho_phy, alt, dz8w, zmid, z_at_w,           &
            moist, cldfra, ph_no2,                                    &
            chem, chem_name,                                          &
            chem_cupflag, cupflag, shall, tcloud_cup, nca, wact_cup,  &
            cldfra_cup, updfra_cup, qc_ic_cup, qc_iu_cup,             &
            mfup_cup, mfup_ent_cup, mfdn_cup, mfdn_ent_cup,           &
            fcvt_qc_to_pr_cup, fcvt_qc_to_qi_cup, fcvt_qi_to_pr_cup,  &
            co_a_ic_cup, hno3_a_ic_cup,                               &
            so4_a_1to4_ic_cup, so4_cw_1to4_ic_cup,                      &
            nh4_a_1to4_ic_cup, nh4_cw_1to4_ic_cup,                      &
            no3_a_1to4_ic_cup, no3_cw_1to4_ic_cup,                      &
            oa_a_1to4_ic_cup,  oa_cw_1to4_ic_cup,                       &
            oin_a_1to4_ic_cup,  oin_cw_1to4_ic_cup,                     &
            bc_a_1to4_ic_cup,  bc_cw_1to4_ic_cup,                       &
            na_a_1to4_ic_cup,  na_cw_1to4_ic_cup,                       &
            cl_a_1to4_ic_cup,  cl_cw_1to4_ic_cup,                       &
            water_1to4_ic_cup,                                          &
            so4_a_5to6_ic_cup, so4_cw_5to6_ic_cup,                      &
            nh4_a_5to6_ic_cup, nh4_cw_5to6_ic_cup,                      &
            no3_a_5to6_ic_cup, no3_cw_5to6_ic_cup,                      &
            oa_a_5to6_ic_cup,  oa_cw_5to6_ic_cup,                       &
            oin_a_5to6_ic_cup,  oin_cw_5to6_ic_cup,                     &
            bc_a_5to6_ic_cup,  bc_cw_5to6_ic_cup,                       &
            na_a_5to6_ic_cup,  na_cw_5to6_ic_cup,                       &
            cl_a_5to6_ic_cup,  cl_cw_5to6_ic_cup,                       &
            water_5to6_ic_cup,                                          &
            ids,ide, jds,jde, kds,kde,                                &
            ims,ime, jms,jme, kms,kme,                                &
            its,ite, jts,jte, kts,kte, ktep1                          )

!  wet removal by grid-resolved precipitation
!  scavenging of cloud-phase aerosols and gases by collection, freezing, ...
!  scavenging of interstitial-phase aerosols by impaction
!  scavenging of gas-phase gases by mass transfer and reaction

      use module_configure
      use module_state_description
      use module_model_constants

      use module_data_mosaic_asect, only:  &
         dcen_sect, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, msectional, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_na_aer, lptr_nh4_aer, lptr_no3_aer, &
         lptr_oc_aer, lptr_oin_aer, lptr_so4_aer, lptr_bc_aer, &
         lptr_pcg1_b_c_aer,lptr_pcg1_b_o_aer,lptr_opcg1_b_c_aer, &
         lptr_opcg1_b_o_aer,lptr_pcg1_f_c_aer,lptr_pcg1_f_o_aer, &
         lptr_opcg1_f_c_aer, lptr_opcg1_f_o_aer, lptr_ant1_c_aer, &
         lptr_biog1_c_aer, waterptr_aer, &
         dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer

!----------------------------------------------------------------------
! arguments

      type(grid_config_rec_type), intent(in) :: config_flags

      integer, intent(in) ::              &
            ids,ide, jds,jde, kds,kde,    &
            ims,ime, jms,jme, kms,kme,    &
            its,ite, jts,jte, kts,kte, ktep1

      integer, intent(in) ::              &
            grid_id, ktau, ktauc

      integer, dimension( ims:ime, kms:kme, jms:jme ), intent(inout) :: &
            chem_cupflag

      real, intent(in) :: dtstep, dtstepc  ! model and chemistry time-steps (s)

      real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
            t_phy,                   & ! temp (K)
            p_phy,                   & ! pressure (Pa)
            rho_phy,                 & ! moist air density (kg/m3)
            alt,                     & ! dry air 1/density (m3/kg)
            dz8w,                    & ! layer thickness (m)
            zmid,                    & ! height at layer center (m)
            z_at_w,                  & ! height at layer boundary (m)
            cldfra,                  & ! grid-resolved cloud fraction
            ph_no2                     ! no2 photolysis rate (1/s)

! advected moisture variables
      real, dimension( ims:ime, kms:kme, jms:jme, num_moist ), intent(in) :: &
            moist

! all advected chemical species
      real, dimension( ims:ime, kms:kme, jms:jme, num_chem ), intent(inout) :: &
            chem

! interstitial and cloudborne mixing ratios within the convective cloud,
! summed over size bins that the ams can see (Dp < 625 nm)
      real, dimension( ims:ime, kms:kme, jms:jme ), intent(out) :: &
            co_a_ic_cup, hno3_a_ic_cup,                                 &
            so4_a_1to4_ic_cup, so4_cw_1to4_ic_cup,                      &
            nh4_a_1to4_ic_cup, nh4_cw_1to4_ic_cup,                      &
            no3_a_1to4_ic_cup, no3_cw_1to4_ic_cup,                      &
            oa_a_1to4_ic_cup,  oa_cw_1to4_ic_cup,                       &
            oin_a_1to4_ic_cup,  oin_cw_1to4_ic_cup,                     &
            bc_a_1to4_ic_cup,  bc_cw_1to4_ic_cup,                       &
            na_a_1to4_ic_cup,  na_cw_1to4_ic_cup,                       &
            cl_a_1to4_ic_cup,  cl_cw_1to4_ic_cup,                       &
            water_1to4_ic_cup,                                          &
            so4_a_5to6_ic_cup, so4_cw_5to6_ic_cup,                      &
            nh4_a_5to6_ic_cup, nh4_cw_5to6_ic_cup,                      &
            no3_a_5to6_ic_cup, no3_cw_5to6_ic_cup,                      &
            oa_a_5to6_ic_cup,  oa_cw_5to6_ic_cup,                       &
            oin_a_5to6_ic_cup,  oin_cw_5to6_ic_cup,                     &
            bc_a_5to6_ic_cup,  bc_cw_5to6_ic_cup,                       &
            na_a_5to6_ic_cup,  na_cw_5to6_ic_cup,                       &
            cl_a_5to6_ic_cup,  cl_cw_5to6_ic_cup,                       &
            water_5to6_ic_cup


      character(len=12), intent(in) :: chem_name(num_chem)


      logical, dimension( ims:ime, jms:jme ), intent(in) :: &
            cupflag                    ! .true. if cup convection is present in column

      real, dimension( ims:ime, jms:jme ), intent(in) :: &
            nca,                     & ! time remaining for this cloud (s)
            shall,                   & ! cumulus type:  0=deep,  1=shallow,  2=none
            wact_cup                   ! vertical velocity for cloud-base aerosol activation (m/s)

      real, dimension( ims:ime, jms:jme ), intent(inout) :: &
            tcloud_cup                 ! cumulus cloud duration (s)

      real, dimension( ims:ime, kms:kme, jms:jme ), intent(in) :: &
            cldfra_cup,              & ! cumulus cloud fractional area (-)
            updfra_cup,              & ! cumulus updraft fractional area (-)
            qc_ic_cup,               & ! cloud-water in cumulus cloud (kg/kg)
            qc_iu_cup,               & ! cloud-water in updraft (kg/kg)
            mfup_cup,                & ! updraft mass-flux (kg/m2/s)
            mfup_ent_cup,            & ! updraft mass-flux change from entrainment (kg/m2/s)
            mfdn_cup,                & ! downdraft mass-flux (kg/m2/s)
            mfdn_ent_cup,            & ! downdraft mass-flux change from entrainment (kg/m2/s)
            fcvt_qc_to_pr_cup,       & ! fraction of cloud-water converted to precip    as air move thru updraft layer (-)
            fcvt_qc_to_qi_cup,       & ! fraction of cloud-water converted to cloud-ice as air move thru updraft layer (-)
            fcvt_qi_to_pr_cup          ! fraction of cloud-ice   converted to precip    as air move thru updraft layer (-)

!-----------------------------------------------------------------
! local variables
      integer :: aer_mech_id
      integer :: chem_cupflag_1d(kts:kte)
      integer :: i, icalcflagaa, idiagee, idiagff, ishall, isize, itype
      integer :: j
      integer :: k, kcldbot_1d, kcldtop_1d
      integer :: lundiag, lunerr

      real(r8) :: chem_1d(kts:kte,num_chem)
      real(r8) :: chem_incu(kts:kte,num_chem)
      real(r8) :: cldfra_ls_1d(kts:kte)
      real(r8) :: cldfra_cup_1d(kts:kte)
      real(r8) :: dz_1d(kts:kte)
      real(r8) :: fcvt_qc_to_pr_cup_1d(kts:kte)
      real(r8) :: fcvt_qc_to_qi_cup_1d(kts:kte)
      real(r8) :: fcvt_qi_to_pr_cup_1d(kts:kte)
      real(r8) :: mfup_cup_1d(kts:ktep1)
      real(r8) :: mfup_ent_cup_1d(kts:kte)
      real(r8) :: mfdn_cup_1d(kts:ktep1)
      real(r8) :: mfdn_ent_cup_1d(kts:kte)
      real(r8) :: pcen_1d(kts:kte)
      real(r8) :: ph_no2_1d(kts:kte)
      real(r8) :: qc_ic_cup_1d(kts:kte)
      real(r8) :: qc_iu_cup_1d(kts:kte)
      real(r8) :: qi_ic_cup_1d(kts:kte)
      real(r8) :: qi_iu_cup_1d(kts:kte)
      real(r8) :: rhocen_1d(kts:kte)
      real(r8) :: tcloud_cup_1d
      real(r8) :: tcen_1d(kts:kte)
      real(r8) :: tmpa
      real(r8) :: updfra_cup_1d(kts:kte)
      real(r8) :: wact_cup_1d
      real(r8) :: zbnd_1d(kts:ktep1)
      real(r8) :: zcen_1d(kts:kte)


!-----------------------------------------------------------------

      lunerr  = 6
      lundiag = 6
      lundiag = 121

      idiagff = 0 ; idiagee = 0
      if ((ide-ids <= 3) .and. (jde-jds <= 3)) then
         idiagff = 1  ! turn on diagnostics for single column runs
!        idiagff = 0  ! (do this to turn off extra diagnostics)
      end if

      aer_mech_id = 3

      if (ktau <= 1) then
         write(*,'(a)')
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'cldfra_ls_testvalue, wact_cup_testvalue   ', &
             cldfra_ls_testvalue, wact_cup_testvalue
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'air_outflow_limit                         ', &
             air_outflow_limit
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'af_cucld_smallaa, af_cucld_maxaa          ', &
             af_cucld_smallaa, af_cucld_maxaa
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'aw_up_smallaa, w_up_maxaa                 ', &
             aw_up_smallaa, w_up_maxaa
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'af_up_smallaa, af_up_maxaa                ', &
             af_up_smallaa, af_up_maxaa
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'qci_incu_smallaa, qci_inup_smallaa        ', &
             qci_incu_smallaa, qci_inup_smallaa
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'qcw_incu_smallaa, qcw_inup_smallaa        ', &
             qcw_incu_smallaa, qcw_inup_smallaa
         write(*,'(2a,1p,4e12.4)') 'chemcup_control -- ', &
            'tau_active_smallaa, tau_inactive_smallaa  ', &
             tau_active_smallaa, tau_inactive_smallaa
         write(*,'(a,2i5/(a,3(i9,i5)))') &
            'chemcup_control -- grid_id, ktau', grid_id, ktau, &
            'chemcup_control -- d indices', ids,ide, jds,jde, kds,kde, &
            'chemcup_control -- m indices', ims,ime, jms,jme, kms,kme, &
            'chemcup_control -- e indices', its,ite, jts,jte, kts,kte
         write(*,'(a)')
      end if


! loop over grid columns and do chem_cup where shall = 1 (shallow cu) or 2 (deep)
main_j_loop: &
      do j = jts, jte
main_i_loop: &
      do i = its, ite

      idiagee = 0
      if (idiagff > 0) then
         ! turn on diagnostics at i=j=1 for single column runs
         if (i==its .and. j==jts) idiagee = 1
      end if

      if (idiagee > 0) write(*,'(a,i7,l5,i5,20x,4f10.1)') &
         'chcup_a20 ktau, cupflag, ishall,      tcloud, nca dtc', &
         ktau, cupflag(i,j), nint(shall(i,j)), tcloud_cup(i,j), nca(i,j), dtstepc


      ! icalcflagaa > 0 if chem_cup calculations will be done for current i,j and timestep
      icalcflagaa = 0
      if (abs(shall(i,j)-1.0) < 0.1) then
         ishall = 1
         icalcflagaa = 1
      else if (abs(shall(i,j)) < 0.1) then
         ishall = 0
         if ( do_deep ) icalcflagaa = 1
      end if
      ! no calculations if cloud has "expired"
      if (nca(i,j) < 0.01) icalcflagaa = 0
      ! no calculations if cloud lifetime is too small
      if (abs(tcloud_cup(i,j)) < tau_active_smallaa) icalcflagaa = 0

      ! also no calculations when mode_chemcup_timeinteg=2 and the "one time only"
      !    calculations have already been done (indicated by tcloud_cup < 0)
      if (icalcflagaa > 0) then
         if (mode_chemcup_timeinteg == 2 .and. tcloud_cup(i,j) <= 0.0) then
            icalcflagaa = -1
         end if
      end if

      if (icalcflagaa >= 0) then
         ! zero out these history variables at current i,j except when icalcflagaa = -1
         chem_cupflag(i,:,j) = 0
         co_a_ic_cup(i,:,j) = 0.0  ;  hno3_a_ic_cup(i,:,j) = 0.0
         so4_a_1to4_ic_cup(i,:,j) = 0.0 ; so4_cw_1to4_ic_cup(i,:,j) = 0.0
         nh4_a_1to4_ic_cup(i,:,j) = 0.0 ; nh4_cw_1to4_ic_cup(i,:,j) = 0.0
         no3_a_1to4_ic_cup(i,:,j) = 0.0 ; no3_cw_1to4_ic_cup(i,:,j) = 0.0
         oa_a_1to4_ic_cup (i,:,j) = 0.0 ;  oa_cw_1to4_ic_cup(i,:,j) = 0.0
         oin_a_1to4_ic_cup (i,:,j) = 0.0 ;  oin_cw_1to4_ic_cup(i,:,j) = 0.0
         bc_a_1to4_ic_cup (i,:,j) = 0.0 ;  bc_cw_1to4_ic_cup(i,:,j) = 0.0
         na_a_1to4_ic_cup (i,:,j) = 0.0 ;  na_cw_1to4_ic_cup(i,:,j) = 0.0
         cl_a_1to4_ic_cup (i,:,j) = 0.0 ;  cl_cw_1to4_ic_cup(i,:,j) = 0.0
         water_1to4_ic_cup (i,:,j) = 0.0 
         so4_a_5to6_ic_cup(i,:,j) = 0.0 ; so4_cw_5to6_ic_cup(i,:,j) = 0.0
         nh4_a_5to6_ic_cup(i,:,j) = 0.0 ; nh4_cw_5to6_ic_cup(i,:,j) = 0.0
         no3_a_5to6_ic_cup(i,:,j) = 0.0 ; no3_cw_5to6_ic_cup(i,:,j) = 0.0
         oa_a_5to6_ic_cup (i,:,j) = 0.0 ;  oa_cw_5to6_ic_cup(i,:,j) = 0.0
         oin_a_5to6_ic_cup (i,:,j) = 0.0 ;  oin_cw_5to6_ic_cup(i,:,j) = 0.0
         bc_a_5to6_ic_cup (i,:,j) = 0.0 ;  bc_cw_5to6_ic_cup(i,:,j) = 0.0
         na_a_5to6_ic_cup (i,:,j) = 0.0 ;  na_cw_5to6_ic_cup(i,:,j) = 0.0
         cl_a_5to6_ic_cup (i,:,j) = 0.0 ;  cl_cw_5to6_ic_cup(i,:,j) = 0.0
         water_5to6_ic_cup (i,:,j) = 0.0 

      end if

      if (icalcflagaa <= 0) cycle main_i_loop


      write(*,'(/a,i10,4i5)') &
         'mosaic_chem_cup_driver doing ktau, id, i, j, ishall =', ktau, grid_id, i, j, ishall

      chem_cupflag_1d(kts:kte) = chem_cupflag(i,kts:kte,j)

      chem_1d(kts:kte,1:num_chem) = chem(i,kts:kte,j,1:num_chem)

      cldfra_ls_1d(kts:kte)    = cldfra(i,kts:kte,j)
      dz_1d(kts:kte)           = dz8w(i,kts:kte,j)
      pcen_1d(kts:kte)         = p_phy(i,kts:kte,j)
      ph_no2_1d(kts:kte)       = ph_no2(i,kts:kte,j)
      rhocen_1d(kts:kte)       = 1.0_r8/alt(i,kts:kte,j)
      tcen_1d(kts:kte)         = t_phy(i,kts:kte,j)
      zbnd_1d(kts:ktep1)          = z_at_w(i,kts:ktep1,j)
      zcen_1d(kts:kte)         = zmid(i,kts:kte,j)

      qc_ic_cup_1d(kts:kte)    = qc_ic_cup(i,kts:kte,j)
      qc_iu_cup_1d(kts:kte)    = qc_iu_cup(i,kts:kte,j)
      qi_ic_cup_1d(kts:kte)    = 0.0_r8
      qi_iu_cup_1d(kts:kte)    = 0.0_r8
      wact_cup_1d                = wact_cup(i,j)

      fcvt_qc_to_pr_cup_1d(kts:kte) = fcvt_qc_to_pr_cup(i,kts:kte,j)
      fcvt_qc_to_qi_cup_1d(kts:kte) = fcvt_qc_to_qi_cup(i,kts:kte,j)
      fcvt_qi_to_pr_cup_1d(kts:kte) = fcvt_qi_to_pr_cup(i,kts:kte,j)

      if ( mode_chemcup_timeinteg == 1 ) then
         ! simulate the entire cumulus cloud fraction using substeps of dt = dtstepc
         !    also force dtsub <= nca+dtstep because tcloud_cup may not be an
         !       integer multiple of dtstepc
         !    use nca+dtstep because nca is decremented in phys before chem sees it
         tcloud_cup_1d = min( nca(i,j)+dtstep, dtstepc )
      else if ( mode_chemcup_timeinteg == 2 ) then
         ! simulate the entire cumulus cloud fraction in a "one time only" calculation
         !    with dt = tcloud_cup
         tcloud_cup_1d = tcloud_cup(i,j)
      else
         call wrf_error_fatal( &
            '*** mosaic_chem_cup_driver -- bad value for mode_chemcup_timeinteg' )
      end if
      cldfra_cup_1d(kts:kte)   = cldfra_cup(i,kts:kte,j)
      updfra_cup_1d(kts:kte)   = updfra_cup(i,kts:kte,j)
      mfup_cup_1d(kts:ktep1)      = mfup_cup(i,kts:ktep1,j)
      mfup_ent_cup_1d(kts:kte) = mfup_ent_cup(i,kts:kte,j)
      mfdn_cup_1d(kts:ktep1)     = mfdn_cup(i,kts:ktep1,j)
      mfdn_ent_cup_1d(kts:kte) = mfdn_ent_cup(i,kts:kte,j)

      if (idiagee > 0) write(*,'(a,i6,l5,i5,20x,2f10.1)') &
         'chcup_a20 ktau, cupflag, ishall,      tcloud, tcloud1d', &
         ktau, cupflag(i,j), nint(shall(i,j)), tcloud_cup(i,j), tcloud_cup_1d

! *** following are for testing
      if (cldfra_ls_testvalue >= 0.0) &
         cldfra_ls_1d(kts:kte) = max( 0.0, min( 1.0, cldfra_ls_testvalue ) )
      if (wact_cup_testvalue >= 0.0) &
         wact_cup_1d = wact_cup_testvalue


      kcldbot_1d = kts-1
      kcldtop_1d = kts-1
      do k = kts, kte
         if (cldfra_cup_1d(k) > 0.0_r8) then
            kcldtop_1d = k
            if (kcldbot_1d < kts) kcldbot_1d = k
         end if
      end do


!     subr. chem_cup_1d( &
!        config_flags, aer_mech_id, &
!        lundiag, lunerr, &
!        kts, kte, ktep1, p1st, num_chem, num_moist, &
!        ktau, grid_id, i, j, &
!        ishall, kcldbot_inp, kcldtop_inp, &
!        tau_active, tau_inactive, &
!        dz, zcen, zbnd, pcen, tcen, rhocen, ph_no2, &
!        af_lscld, af_cucld_inp, af_up_inp, &
!        qcw_incu_inp, qci_incu_inp, &
!        qcw_inup_inp, qci_inup_inp, &
!        mf_up_inp, mf_up_ent_inp, &
!        mf_dn_inp, mf_dn_ent_inp, &
!        fcvt_qc_to_pr_inp, fcvt_qc_to_qi_inp, fcvt_qi_to_pr_inp, &
!        wact_inp, &
!        chem, chem_incu, chem_name, chem_cupflag, &
!        maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
!        ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
!        ai_phase, cw_phase, msectional, &
!        massptr_aer, numptr_aer, &
!        lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
!        dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer

      call chem_cup_1d( &
         config_flags, aer_mech_id, &
         lunerr, lundiag, idiagee, &
         kts, kte, ktep1, param_first_scalar, num_chem, num_moist, &
         ktau, grid_id, i, j, &
         ishall, kcldbot_1d, kcldtop_1d, &
         tcloud_cup_1d, tcloud_cup_1d, &
         dz_1d, zcen_1d, zbnd_1d, pcen_1d, tcen_1d, rhocen_1d, ph_no2_1d, &
         cldfra_ls_1d, cldfra_cup_1d, updfra_cup_1d, &
         qc_ic_cup_1d, qi_ic_cup_1d, &
         qc_iu_cup_1d, qi_iu_cup_1d, &
         mfup_cup_1d, mfup_ent_cup_1d, &
         mfdn_cup_1d, mfdn_ent_cup_1d, &
         fcvt_qc_to_pr_cup_1d, fcvt_qc_to_qi_cup_1d, fcvt_qi_to_pr_cup_1d, &
         wact_cup_1d, &
         chem_1d, chem_incu, chem_name, chem_cupflag_1d, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, msectional, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer )

      write(*,'(/a,4i10)') &
         'mosaic_chem_cup_driver back  ktau, id, i, j =', ktau, grid_id, i, j

      chem_cupflag(i,kts:kte,j) = chem_cupflag_1d(kts:kte) 

      chem(i,kts:kte,j,1:num_chem) = chem_1d(kts:kte,1:num_chem)

!     call wrf_error_fatal( 'aborting after first call to chem_cup_1d' )
!     if (ktau >= 465) call wrf_error_fatal( &
!        'aborting after call to chem_cup_1d and ktau>=465' )

      if (mode_chemcup_timeinteg == 2) then
         ! calculations for this cloud are totally finished
         ! set tcloud_cup to a negative value to indicate this on next timestep
         tcloud_cup(i,j) = -tcloud_cup(i,j)  
      end if

!  Do the gas variables co and hno3 here
         do k = kcldbot_1d, kcldtop_1d
            if (chem_cupflag(i,k,j) <= 0) cycle
            co_a_ic_cup(i,k,j)  = co_a_ic_cup(i,k,j) &
                                     + chem_incu(k,p_co)
            hno3_a_ic_cup(i,k,j)  = hno3_a_ic_cup(i,k,j) &
                                     + chem_incu(k,p_hno3)
         enddo

      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         if (dcen_sect(isize,itype) .le. 0.625e-4) then
         do k = kcldbot_1d, kcldtop_1d
            if (chem_cupflag(i,k,j) <= 0) cycle
            so4_a_1to4_ic_cup(i,k,j)  = so4_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_so4_aer(isize,itype,ai_phase))
            so4_cw_1to4_ic_cup(i,k,j) = so4_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_so4_aer(isize,itype,cw_phase))
            nh4_a_1to4_ic_cup(i,k,j)  = nh4_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_nh4_aer(isize,itype,ai_phase))
            nh4_cw_1to4_ic_cup(i,k,j) = nh4_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_nh4_aer(isize,itype,cw_phase))
            no3_a_1to4_ic_cup(i,k,j)  = no3_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_no3_aer(isize,itype,ai_phase))
            no3_cw_1to4_ic_cup(i,k,j) = no3_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_no3_aer(isize,itype,cw_phase))
            oa_a_1to4_ic_cup(i,k,j)   = oa_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oc_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_ant1_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_biog1_c_aer(isize,itype,ai_phase)) 


            oa_cw_1to4_ic_cup(i,k,j)  = oa_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oc_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_ant1_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_biog1_c_aer(isize,itype,cw_phase))

            oin_a_1to4_ic_cup(i,k,j)  = oin_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oin_aer(isize,itype,ai_phase))
            oin_cw_1to4_ic_cup(i,k,j) = oin_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oin_aer(isize,itype,cw_phase))

            bc_a_1to4_ic_cup(i,k,j)  = bc_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_bc_aer(isize,itype,ai_phase))
            bc_cw_1to4_ic_cup(i,k,j) = bc_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_bc_aer(isize,itype,cw_phase))

            na_a_1to4_ic_cup(i,k,j)  = na_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_na_aer(isize,itype,ai_phase))
            na_cw_1to4_ic_cup(i,k,j) = na_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_na_aer(isize,itype,cw_phase))

            cl_a_1to4_ic_cup(i,k,j)  = cl_a_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_cl_aer(isize,itype,ai_phase))
            cl_cw_1to4_ic_cup(i,k,j) = cl_cw_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_cl_aer(isize,itype,cw_phase))

            water_1to4_ic_cup(i,k,j)  = water_1to4_ic_cup(i,k,j) &
                                     + chem_incu(k,waterptr_aer(isize,itype))
                                     
         end do ! k

           elseif (dcen_sect(isize,itype) .gt. 0.625e-4 .and. &
                     dcen_sect(isize,itype) .le. 2.5e-4) then
         do k = kcldbot_1d, kcldtop_1d
            if (chem_cupflag(i,k,j) <= 0) cycle
            so4_a_5to6_ic_cup(i,k,j)  = so4_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_so4_aer(isize,itype,ai_phase))
            so4_cw_5to6_ic_cup(i,k,j) = so4_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_so4_aer(isize,itype,cw_phase))
            nh4_a_5to6_ic_cup(i,k,j)  = nh4_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_nh4_aer(isize,itype,ai_phase))
            nh4_cw_5to6_ic_cup(i,k,j) = nh4_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_nh4_aer(isize,itype,cw_phase))
            no3_a_5to6_ic_cup(i,k,j)  = no3_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_no3_aer(isize,itype,ai_phase))
            no3_cw_5to6_ic_cup(i,k,j) = no3_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_no3_aer(isize,itype,cw_phase))
            oa_a_5to6_ic_cup(i,k,j)   = oa_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oc_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_o_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_ant1_c_aer(isize,itype,ai_phase)) &
                                     + chem_incu(k,lptr_biog1_c_aer(isize,itype,ai_phase))


            oa_cw_5to6_ic_cup(i,k,j)  = oa_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oc_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_b_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_b_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_pcg1_f_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_opcg1_f_o_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_ant1_c_aer(isize,itype,cw_phase)) &
                                     + chem_incu(k,lptr_biog1_c_aer(isize,itype,cw_phase))

            oin_a_5to6_ic_cup(i,k,j)  = oin_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oin_aer(isize,itype,ai_phase))
            oin_cw_5to6_ic_cup(i,k,j) = oin_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_oin_aer(isize,itype,cw_phase))

            bc_a_5to6_ic_cup(i,k,j)  = bc_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_bc_aer(isize,itype,ai_phase))
            bc_cw_5to6_ic_cup(i,k,j) = bc_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_bc_aer(isize,itype,cw_phase))

            na_a_5to6_ic_cup(i,k,j)  = na_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_na_aer(isize,itype,ai_phase))
            na_cw_5to6_ic_cup(i,k,j) = na_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_na_aer(isize,itype,cw_phase))

            cl_a_5to6_ic_cup(i,k,j)  = cl_a_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_cl_aer(isize,itype,ai_phase))
            cl_cw_5to6_ic_cup(i,k,j) = cl_cw_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,lptr_cl_aer(isize,itype,cw_phase))

            water_5to6_ic_cup(i,k,j)  = water_5to6_ic_cup(i,k,j) &
                                     + chem_incu(k,waterptr_aer(isize,itype))


         end do ! k
           end if ! dcen size 

      end do ! isize
      end do ! itype

         
      end do main_i_loop
      end do main_j_loop


      return
      end subroutine mosaic_chem_cup_driver


!-------------------------------------------------------------------------------
      subroutine chem_cup_1d( &
         config_flags, aer_mech_id, &
         lunerr, lundiag, idiagaa_inp, &
         kts, kte, ktep1, p1st, num_chem, num_moist, &
         ktau, grid_id, i, j, &
         ishall, kcldbot_inp, kcldtop_inp, &
         tau_active, tau_inactive, &
         dz, zcen, zbnd, pcen, tcen, rhocen, ph_no2, &
         af_lscld, af_cucld_inp, af_up_inp, &
         qcw_incu_inp, qci_incu_inp, &
         qcw_inup_inp, qci_inup_inp, &
         mf_up_inp, mf_up_ent_inp, &
         mf_dn_inp, mf_dn_ent_inp, &
         fcvt_qc_to_pr, fcvt_qc_to_qi, fcvt_qi_to_pr, &
         wact_inp, &
         chem, chem_incu, chem_name, chem_cupflag, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, msectional, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer )

      use module_configure, only:  grid_config_rec_type
      use module_configure, only:  &
         p_qc, &
         p_h2o2, p_hcl, p_hno3, p_nh3, p_so2, p_sulf

! arguments
!
! note: arguments ending in "_inp" may be adjusted for consistency
      type(grid_config_rec_type), intent(in) :: config_flags

      integer, intent(in) :: aer_mech_id
      integer, intent(in) :: grid_id
      integer, intent(in) :: i, idiagaa_inp, ishall
      integer, intent(in) :: j
      integer, intent(in) :: ktau, kts, kte, ktep1
      integer, intent(in) :: kcldbot_inp, kcldtop_inp
      integer, intent(in) :: lunerr, lundiag
      integer, intent(in) :: num_chem, num_moist
      integer, intent(in) :: p1st

      integer, intent(inout) :: chem_cupflag(kts:kte)  ! the "in" value is 0

      integer, intent(in) :: maxd_acomp, maxd_aphase, maxd_atype, maxd_asize
      integer, intent(in) :: &
         nphase_aer, ntype_aer, &
         ai_phase, cw_phase, msectional, &
         nsize_aer( maxd_atype ), &
         ncomp_aer( maxd_atype ), &
         massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), &
         numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
         lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_oin_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_cl_aer(maxd_asize, maxd_atype, maxd_aphase)
      real(r4), intent(in) :: &
         dlo_sect( maxd_asize, maxd_atype ), &
         dhi_sect( maxd_asize, maxd_atype ), &
         dens_aer( maxd_acomp, maxd_atype ), &
         hygro_aer( maxd_acomp, maxd_atype ), &
         sigmag_aer(maxd_asize, maxd_atype)


      real(r8), intent(in) :: tau_active, tau_inactive
      real(r8), intent(in) :: wact_inp  ! vertical velocity for activation at cloud-base (m/s)

      real(r8), intent(in), dimension(kts:kte) :: &
         dz, &            ! layer thickness (m)
         zcen, &          ! height at layer center (m)
         pcen, &          ! pressure at layer center (pa)
         tcen, &          ! temperature at layer center (K)
         rhocen, &        ! dry air density at layer center (kg/m3)
         ph_no2, &        ! no2 photolysis rate (1/s)
         af_lscld, &      ! grid-resolved cloud fractional area
         af_cucld_inp, &  ! cumulus cloud fractional area
         af_up_inp, &     ! updraft fractional area
         qcw_incu_inp, &  ! cloud-water mixing ratio in cumulus cloud (kg/kg)
         qci_incu_inp, &  ! cloud-ice   mixing ratio in updraft       (kg/kg)
         qcw_inup_inp, &  ! cloud-water mixing ratio in cumulus cloud (kg/kg)
         qci_inup_inp, &  ! cloud-ice   mixing ratio in updraft       (kg/kg)
         mf_up_ent_inp, & ! change to updraft mass flux in layer from entrainment (kg/m2/s)
         mf_dn_ent_inp, & ! change to updraft mass flux in layer from entrainment (kg/m2/s)
         fcvt_qc_to_pr, & ! fraction of cloud-water converted to precip    as air move thru updraft layer (-)
         fcvt_qc_to_qi, & ! fraction of cloud-water converted to cloud-ice as air move thru updraft layer (-)
         fcvt_qi_to_pr    ! fraction of cloud-ice   converted to precip    as air move thru updraft layer (-)

      real(r8), intent(in), dimension(kts:ktep1) :: &
         zbnd, &          ! height at layer boundary (m)
         mf_up_inp, &     ! updraft mass flux at layer boundary (kg/m2/s)
         mf_dn_inp        ! updraft mass flux at layer boundary (kg/m2/s)

      real(r8), intent(inout), dimension(kts:kte,num_chem) :: &
         chem, &          ! grid-average aerosol and trace-gas mixing ratios (ug/kg, #/kg, or ppmv)
         chem_incu        ! in-convective-cloud aerosol and trace-gas mixing ratios (ug/kg, #/kg, or ppmv)

      character(len=12), intent(in) :: chem_name(num_chem)

! local variables
      integer :: aip, cwp, typ
      integer :: idiagaa, idiagbb, iflagaa, iok, icomp, isize, itype, itmpa, itmpb
      integer :: ido_inact(kts:kte)
      integer :: jtsub
      integer :: k, kaa, kzz
      integer :: kcldbot, kcldtop, kcldbotliq
      integer :: kdiagbot, kdiagtop
      integer :: kupdrbot, kupdrtop, kdndrbot, kdndrtop
      integer :: l, la, lc, l2, l3, lundiagbb
      integer :: m
      integer :: n
      integer :: ntsub  ! number of time sub-steps for active cloud integration

      logical, parameter :: do_activa  = .true.

!------To turn off cloud chemistry set do_aqchem-false : Manish Shrivastava -------------------
      logical, parameter :: do_aqchem  = .true.
!---------------------------------------------------------------------------
      logical, parameter :: do_inact   = .true.
      logical, parameter :: do_resusp  = .true.
      logical, parameter :: do_updraft = .true.

      logical, parameter :: do_2ndact_deep  = .true.
      logical, parameter :: do_2ndact_shal  = .false.
      logical, parameter :: do_dndraft_deep = .true.
      logical, parameter :: do_dndraft_shal = .false.
      logical, parameter :: do_wetrem_deep  = .true.
      logical, parameter :: do_wetrem_shal  = .false.

      logical :: do_2ndact, do_dndraft, do_wetrem

      real(r8), parameter :: rerrtol1_mbal = 3.0e-6

      real(r8) :: af_cucld(kts:kte)  ! cumulus cloud fractional area
      real(r8) :: af_dn(kts:kte)     ! downdraft fractional area
      real(r8) :: af_ev(kts:kte)     ! environment fractional area
      real(r8) :: af_up(kts:kte)     ! updraft fractional area
      real(r8) :: af_inact(kts:kte)  ! inactive cumulus cloud fractional area
      real(r8) :: chem_av_new(kts:kte,num_chem)      ! grid-average chem values at end   of sub time-step
      real(r8) :: chem_av_old(kts:kte,num_chem)      ! grid-average chem values at start of sub time-step
      real(r8) :: chem_inact_dsp(kts:kte,num_chem)   ! inactive cloud chem values at end   of time-step 
                                                     ! but after  resuspension/dissipation calculations
      real(r8) :: chem_inact_new(kts:kte,num_chem)   ! inactive cloud chem values at end   of time-step
                                                     ! but before resuspension/dissipation calculations
      real(r8) :: chem_inact_old(kts:kte,num_chem)   ! inactive cloud chem values at start of time-step
      real(r8) :: chem_dn(kts:ktep1,num_chem)        ! steady-state dndraft chem values (at layer boundaries)
      real(r8) :: chem_up(kts:ktep1,num_chem)        ! steady-state updraft chem values (at layer boundaries)
      ! the following dchemdt_xx_yyyyyy arrays are d(chem)/dt in environment or updraft
      ! due to resuspension, activation, aqueous-chemistry, or wet-removal [(ug/kg or ...)/s]
      real(r8) :: dchemdt_ev_resusp(kts:kte,num_chem)
      real(r8) :: dchemdt_up_activa(kts:kte,num_chem)
      real(r8) :: dchemdt_up_aqchem(kts:kte,num_chem)
      real(r8) :: dchemdt_up_wetrem(kts:kte,num_chem)
      ! the following del_chem_yyyyyy arrays are change (delta) 
      ! of grid-average chem due to various processes (ug/kg or ...)
      real(r8) :: del_chem_activa(kts:kte,num_chem)  ! activation
      real(r8) :: del_chem_aqchem(kts:kte,num_chem)  ! aqueous-chemistry
      real(r8) :: del_chem_residu(kts:kte,num_chem)  ! residual
      real(r8) :: del_chem_resusp(kts:kte,num_chem)  ! resuspension
      real(r8) :: del_chem_totall(kts:kte,num_chem)  ! total change
      real(r8) :: del_chem_wetrem(kts:kte,num_chem)  ! wet-removal
      real(r8) :: del_chem_ztrans(kts:kte,num_chem)  ! vertical-transport
      real(r8) :: del_chem_actvbb(kts:kte,num_chem)  ! activation for inactive cloud
      real(r8) :: del_chem_aqchbb(kts:kte,num_chem)  ! aqueous-chemistry for inactive cloud
      real(r8) :: del_chem_resdbb(kts:kte,num_chem)  ! residual for inactive cloud
      real(r8) :: del_chem_respbb(kts:kte,num_chem)  ! resuspension for inactive cloud
      real(r8) :: del_chem_totlbb(kts:kte,num_chem)  ! total change for inactive cloud
      real(r8) :: dtsub                 ! time sub-step for active cloud integration (s)
      real(r8) :: mf_dn_det(kts:kte)    ! change to downdraft mass flux in layer from detrainment (kg/m2/s)
      real(r8) :: mf_dn_ent(kts:kte)    ! change to downdraft mass flux in layer from entrainment (kg/m2/s)
      real(r8) :: mf_dn(kts:ktep1)      ! downdraft mass flux at layer boundary (kg/m2/s)
      real(r8) :: mf_ev(kts:ktep1)      ! environment mass flux at layer boundary (kg/m2/s)
      real(r8) :: mf_up_det(kts:kte)    ! change to updraft mass flux in layer from detrainment (kg/m2/s)
      real(r8) :: mf_up_ent(kts:kte)    ! adjusted change to updraft mass flux in layer from entrainment (kg/m2/s)
      real(r8) :: mf_up(kts:ktep1)      ! adjusted updraft mass flux at layer boundary (kg/m2/s)
      real(r8) :: qci_incu(kts:kte), qci_inup(kts:kte)
      real(r8) :: qcw_incu(kts:kte), qcw_inup(kts:kte)
      real(r8), parameter :: qcw_cldchem_cutoff = 1.0e-6_r8
      real(r8) :: rhodz(kts:kte)        ! rho*dz (kg/m2)
      real(r8) :: rhodzsum              ! sum( rho*dz )
      real(r8) :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg
      real(r8) :: tmp_chem_dn(num_chem)     ! temporary/working value of chem_dn
      real(r8) :: tmp_chem_up(num_chem)     ! temporary/working value of chem_up
      real(r8) :: tmp_dt                    ! temporary/working value of dt for cloud chemistry
      real(r8) :: tmp_fmact(maxd_asize,maxd_atype), tmp_fnact(maxd_asize,maxd_atype)
      real(r8) :: tmp_gas_aqfrac_up(num_chem)  ! temporary/working value of gas_aqfrac in updraft
      real(r8) :: tmp_mfxchem_dn(num_chem)  ! temporary/working value of mf_up*chem_dn
      real(r8) :: tmp_mfxchem_up(num_chem)  ! temporary/working value of mf_up*chem_up
      real(r8) :: tmp_mf_dn ! temporary/working value of mf_dn
      real(r8) :: tmp_mf_up ! temporary/working value of mf_up
      real(r8) :: tmp_w_up  ! temporary/working value of w in updraft
      real(r8) :: tmpveca(201), tmpvecb(201)
      ! tmp_zflux_... - temporary/working values of trace-species vertical fluxes [(ug/kg or ...)/m2/s]
      real(r8) :: tmp_zflux_bot, tmp_zflux_botev, tmp_zflux_botup, tmp_zflux_botdn
      real(r8) :: tmp_zflux_top, tmp_zflux_topev, tmp_zflux_topup, tmp_zflux_topdn
      real(r8) :: wact  ! vertical velocity for activation at cloud-base (m/s)
      real(r8), parameter :: wact_min = 0.2  ! (m/s)
      ! the following zav_yyyyyy arrays are vertical averages of 
      ! the corresponding yyyyyy arrays, weighted by rhodz
      real(r8) :: zav_chem_av_new(num_chem)
      real(r8) :: zav_chem_av_old(num_chem)
      real(r8) :: zav_chem_dn(num_chem)
      real(r8) :: zav_chem_up(num_chem)
      real(r8) :: zav_del_chem_activa(num_chem)
      real(r8) :: zav_del_chem_aqchem(num_chem)
      real(r8) :: zav_del_chem_residu(num_chem)
      real(r8) :: zav_del_chem_resusp(num_chem)
      real(r8) :: zav_del_chem_totall(num_chem)
      real(r8) :: zav_del_chem_wetrem(num_chem)
      real(r8) :: zav_del_chem_ztrans(num_chem)
      real(r8) :: zav_del_chem_actvbb(num_chem)
      real(r8) :: zav_del_chem_aqchbb(num_chem)
      real(r8) :: zav_del_chem_resdbb(num_chem)
      real(r8) :: zav_del_chem_respbb(num_chem)
      real(r8) :: zav_del_chem_totlbb(num_chem)

      character(len=160) :: msg


! sanity checks
      if ( (ai_phase < 1) .or. (ai_phase > nphase_aer) .or. &
           (cw_phase < 1) .or. (cw_phase > nphase_aer) ) then
         write(msg,'(a,3(1x,i10))') &
            'chem_cup_1d - bad ai_phase, cw_phase, nphase_aer =', &
            ai_phase, cw_phase, nphase_aer
         call wrf_message( msg )
         call wrf_error_fatal( msg )
      end if
      if (aer_mech_id /= 3) then
         write(msg,'(a,3(1x,i10))') &
            'chem_cup_1d - bad aer_mech_id = ', aer_mech_id
         call wrf_message( msg )
         call wrf_error_fatal( msg )
      end if

! checks on inputs
      idiagaa = 0 
      if (lundiag > 0) idiagaa = idiagaa_inp
      if (idiagaa > 0) write(lundiag,'(//a,i10,4i5)') &
         'chem_cup_1d doing ktau, id, i, j, ishall =', ktau, grid_id, i, j, ishall

      idiagbb = 0 
      if (idiagaa_inp >      0) idiagbb = idiagaa_inp
      if (idiagaa_inp <= -1000) idiagbb = -idiagaa_inp/1000
      lundiagbb = 6 
      if (lundiag > 0) lundiagbb = lundiag

      if (ishall == 1) then
         do_2ndact  = do_2ndact_shal
         do_dndraft = do_dndraft_shal
         do_wetrem  = do_wetrem_shal
      else
         do_2ndact  = do_2ndact_deep
         do_dndraft = do_dndraft_deep
         do_wetrem  = do_wetrem_deep
      end if

      kcldbot = kcldbot_inp
      kcldtop = kcldtop_inp

      af_cucld(:)  = max( af_cucld_inp(:), 0.0_r8 )
      af_up(:)     = max( af_up_inp(:), 0.0_r8 )
      qcw_incu(:)  = max( qcw_incu_inp(:), 0.0_r8 )
      qci_incu(:)  = max( qci_incu_inp(:), 0.0_r8 )
      qcw_inup(:)  = max( qcw_inup_inp(:), 0.0_r8 )
      qci_inup(:)  = max( qci_inup_inp(:), 0.0_r8 )
      mf_up(:)     = max( mf_up_inp(:), 0.0_r8 )
      mf_up_ent(:) = max( mf_up_ent_inp(:), 0.0_r8 )
      mf_up_det(:) = 0.0_r8
      wact = max( wact_inp, wact_min )

      if ( do_dndraft ) then
! the cloud parameterization does not provide (?) downdraft area
! the chem_cup only uses af_dn for af_ev = 1 - af_up - af_dn,
!    and it only uses af_ev for determining resuspension fraction of
!    detrained cloud-borne aerosol
! for now, just leave af_dn = 0
         af_dn(:)     = 0.0_r8
         mf_dn(:)     = min( mf_dn_inp(:), 0.0_r8 )
         mf_dn_ent(:) = max( mf_dn_ent_inp(:), 0.0_r8 )
         mf_dn_det(:) = 0.0_r8
      else
         af_dn(:)     = 0.0_r8
         mf_dn(:)     = 0.0_r8
         mf_dn_ent(:) = 0.0_r8
         mf_dn_det(:) = 0.0_r8
      end if

      if ( 1 .eq. 1 ) then
      if (idiagaa > 0) then
      write(lundiag,'(a,2i10)')      'kcldbot/top', kcldbot, kcldtop
      write(lundiag,'(a,1p,2e10.2)') 'tau_...    ', tau_active, tau_inactive
      write(lundiag,'(a,1p,2e10.2)') 'wact_inp   ', wact_inp
      write(lundiag,'(a)') 'zbnd'
      write(lundiag,'(1p,15e10.2)') zbnd
      write(lundiag,'(a)') 'zcen'
      write(lundiag,'(1p,15e10.2)') zcen
      write(lundiag,'(a)') 'dz'
      write(lundiag,'(1p,15e10.2)') dz
      write(lundiag,'(a)') 'pcen'
      write(lundiag,'(1p,15e10.2)') pcen
      write(lundiag,'(a)') 'tcen'
      write(lundiag,'(1p,15e10.2)') tcen
      write(lundiag,'(a)') 'rhocen'
      write(lundiag,'(1p,15e10.2)') rhocen
      write(lundiag,'(a)') 'af_lscld'
      write(lundiag,'(1p,15e10.2)') af_lscld
      write(lundiag,'(a)') 'af_cucld'
      write(lundiag,'(1p,15e10.2)') af_cucld
      write(lundiag,'(a)') 'af_up'
      write(lundiag,'(1p,15e10.2)') af_up
      write(lundiag,'(a)') 'qcw_incu'
      write(lundiag,'(1p,15e10.2)') qcw_incu
      write(lundiag,'(a)') 'qcw_inup'
      write(lundiag,'(1p,15e10.2)') qcw_inup
      write(lundiag,'(a)') 'mf_up'
      write(lundiag,'(1p,15e10.2)') mf_up
      write(lundiag,'(a)') 'mf_up_ent'
      write(lundiag,'(1p,15e10.2)') mf_up_ent
      if ( do_dndraft ) then
      write(lundiag,'(a)') 'mf_dn'
      write(lundiag,'(1p,15e10.2)') mf_dn
      write(lundiag,'(a)') 'mf_dn_ent'
      write(lundiag,'(1p,15e10.2)') mf_dn_ent
      end if
      end if
      end if

!
! apply "reality checks" to inputs
!
      call chem_cup_check_adjust_inputs( &
         lunerr, lundiag, idiagaa, &
         kts, kte, ktep1, &
         ktau, grid_id, i, j, &
         ishall, do_dndraft, &
         kcldbot, kcldtop, kcldbotliq, &
         kupdrbot, kupdrtop, kdndrbot, kdndrtop, &
         iok, &
         tau_active, tau_inactive, &
         dz, zcen, zbnd, pcen, tcen, rhocen, &
         af_lscld, af_cucld, af_up, af_dn, &
         qcw_incu, qci_incu, &
         qcw_inup, qci_inup, &
         mf_up, mf_up_ent, mf_up_det, &
         mf_dn, mf_dn_ent, mf_dn_det )

      if (idiagaa > 0) write(lundiag,'(//a,i10)') &
         'chem_cup_check_adjust_inputs iok =', iok

      if ( do_dndraft ) then
         kdiagbot = max( min(kupdrbot,kdndrbot)-2, kts )
         kdiagtop = min( max(kupdrtop,kdndrtop)+2, kte )
      else
         kdiagbot = max( kupdrbot-2, kts )
         kdiagtop = min( kupdrtop+2, kte )
      end if
!     kdiagbot = kts
!     kdiagtop = kte

      if ( .not. do_updraft ) then
         mf_up(:)     = 0.0_r8
         mf_up_ent(:) = 0.0_r8
         mf_up_det(:) = 0.0_r8
      end if

! diagnostic output
      if (idiagaa > 0) then
       
      write(lundiag,'(/4a)') 'k,   ', &
            'qcw_incu*1e3 a/b,   qci_incu*1e3 a/b,   ', &
            'qcw_inup*1e3 a/b,   qci_inup*1e3 a/b'
      do k = kdiagtop, kdiagbot, -1
         if (mod(kdiagtop-k,1) == 0) write(lundiag,'(a)')
         write(lundiag,'(i2,6(3x,1p,2e10.2))') k, &
            qcw_incu_inp(k)*1.0e3, qcw_incu(k)*1.0e3, &
            qci_incu_inp(k)*1.0e3, qci_incu(k)*1.0e3, &
            qcw_inup_inp(k)*1.0e3, qcw_inup(k)*1.0e3, &
            qci_inup_inp(k)*1.0e3, qci_inup(k)*1.0e3
      end do

      if ( do_dndraft ) then
         write(lundiag,'(/a2,2a23,2(2a23,a13))') &
            'k', 'af_cucld a/b', 'af_up a/b', &
            'mf_up a/b', 'mf_up_ent a/b', 'mf_up_det b', &
            'mf_dn a/b', 'mf_dn_ent a/b', 'mf_dn_det b'
      else
         write(lundiag,'(/a2,2a23,2(2a23,a13))') &
            'k', 'af_cucld a/b', 'af_up a/b', &
            'mf_up a/b', 'mf_up_ent a/b', 'mf_up_det b'
      end if
      do k = kdiagtop, kdiagbot, -1
         if (mod(kdiagtop-k,1) == 0) write(lundiag,'(a)')
         if ( do_dndraft ) then
            write(lundiag, &
               '(i2,1p, 2(3x,2e10.2), 2(2(3x,2e10.2), 3x,e10.2))') k, &
               af_cucld_inp(k), af_cucld(k), &
               af_up_inp(k), af_up(k), &
               mf_up_inp(k), mf_up(k), &
               mf_up_ent_inp(k), mf_up_ent(k), &
               mf_up_det(k), &
               mf_dn_inp(k), mf_dn(k), &
               mf_dn_ent_inp(k), mf_dn_ent(k), &
               mf_dn_det(k)
         else
            write(lundiag, &
               '(i2,1p, 2(3x,2e10.2), 2(2(3x,2e10.2), 3x,e10.2))') k, &
               af_cucld_inp(k), af_cucld(k), &
               af_up_inp(k), af_up(k), &
               mf_up_inp(k), mf_up(k), &
               mf_up_ent_inp(k), mf_up_ent(k), &
               mf_up_det(k)
         end if
      end do

      write(lundiag,'(/a,2i5)') 'kcldbot,  top inp', kcldbot_inp, kcldtop_inp
      write(lundiag,'( a,2i5)') 'kcldbot,  top    ', kcldbot, kcldtop
      write(lundiag,'( a,2i5)') 'kupdrbot, top    ', kupdrbot, kupdrtop
      if ( do_dndraft ) &
      write(lundiag,'( a,2i5)') 'kdndrbot, top    ', kdndrbot, kdndrtop
      write(lundiag,'(a)')

      end if ! (idiagaa > 0)

      chem_incu = 0.0
      if (iok < 0) then
         chem_cupflag = -1
         goto 89000
      end if


!
! determine time step for active cloud calculations
! the mass of air moving out of a layer 
!    into updraft and downdraft (through their entrainment)
!    and into adjacent layers (through environment subsidence)
!    cannot exceed the mass of air in the layer
!
      rhodz(kts:kte) = rhocen(kts:kte)*dz(kts:kte)
      rhodzsum = sum( rhodz(kts:kte) )

      mf_ev(kts:ktep1) = -( mf_up(kts:ktep1) + mf_dn(kts:ktep1) )
      af_ev(kts:kte) = 1.0_r8 - ( af_up(kts:kte) + af_dn(kts:kte) )

      tmpa = 1.0e10   ! tmpa is the smallest of the
                      ! individual layer maximum time steps
      do k = kupdrbot, kupdrtop
         tmpb = mf_up_ent(k) + mf_dn_ent(k) &
              + max( mf_ev(k+1), 0.0_r8 ) &
              + max( -mf_ev(k),  0.0_r8 )           ! tmpb is air-mass flux out of this layer
         tmpc = rhodz(k) / max( tmpb, 1.0e-10_r8 )  ! tmpc is max. time step for this layer
         tmpa = min( tmpa, tmpc )                   ! tmpa is the smallest of all the tmpc
         if (idiagaa > 0) write(lundiag,'(a,1x,i10,1p,e11.3)') &
            'k, dtmax', k, tmpc
      end do
      tmpd = tmpa
      tmpa = tmpa * air_outflow_limit
      ntsub = floor( tau_active/tmpa ) + 1
      dtsub = tau_active/ntsub
      if (idiagaa > 0) then
         write(lundiag,'(a,1x,i10,1p,2e11.3)') 'k, dtmax', -1, tmpd, tmpa
         write(lundiag,'(a,1x,i10,1p,2e11.3)') &
            'ntsub, dtsub, tau_active', ntsub, dtsub, tau_active
         write(lundiag,'(2a,1x,10l5)') &
            'do_activa, _2ndact, _resusp, _aqchem, ', &
            '_wetrem, _updraft, _dndraft', &
            do_activa, do_2ndact, do_resusp, do_aqchem, &
            do_wetrem, do_updraft, do_dndraft
      end if

!
!
! do multiple substeps of active cloud
!
!
      chem_av_new(:,:) = chem(:,:)
      zav_chem_av_new(:) = 0.0_r8
      do l = p1st, num_chem
         zav_chem_av_new(l) = sum( rhodz(kts:kte)*chem_av_new(kts:kte,l) ) / rhodzsum
      end do
      del_chem_activa(:,:) = 0.0_r8
      del_chem_aqchem(:,:) = 0.0_r8
      del_chem_residu(:,:) = 0.0_r8
      del_chem_resusp(:,:) = 0.0_r8
      del_chem_totall(:,:) = 0.0_r8
      del_chem_wetrem(:,:) = 0.0_r8
      del_chem_ztrans(:,:) = 0.0_r8
      del_chem_actvbb(:,:) = 0.0_r8
      del_chem_aqchbb(:,:) = 0.0_r8
      del_chem_resdbb(:,:) = 0.0_r8
      del_chem_respbb(:,:) = 0.0_r8
      del_chem_totlbb(:,:) = 0.0_r8

      ido_inact = 0

active_cloud_jtsub_loop: &
      do jtsub = 1, ntsub


!
! calculate aerosol and gas profiles in the updraft
!
      chem_av_old(:,:) = chem_av_new(:,:)
      zav_chem_av_old(:) = zav_chem_av_new(:) 

      chem_up(:,:) = 0.0_r8
      dchemdt_ev_resusp(:,:) = 0.0_r8
      dchemdt_up_activa(:,:) = 0.0_r8
      dchemdt_up_aqchem(:,:) = 0.0_r8
      dchemdt_up_wetrem(:,:) = 0.0_r8

      zav_chem_up(:) = 0.0_r8
      tmp_mfxchem_up = 0.0_r8

do_updraft_mixratio_calc: &
      if ( do_updraft ) then

updraft_mixratio_k_loop: &
      do k = kupdrbot, kupdrtop

         tmp_mf_up = mf_up(k)

! do activation at first cloudy layer
! when first cloudy layer > first updraft layer
         if ( do_activa ) then
         if ((k == kcldbotliq) .and. (k > kupdrbot)) then
            iflagaa = 1
            call chem_cup_activate_up( &
               lunerr, lundiag, idiagaa, &
               kts, kte, p1st, num_chem, &
               ktau, grid_id, i, j, k, iflagaa, &
               pcen(k), tcen(k), rhocen(k), qcw_inup(k), &
               rhodz(k), af_up(k), wact, &
               tmp_mf_up, tmp_mfxchem_up, dchemdt_up_activa, &
               maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
               ncomp_aer, nsize_aer, ntype_aer, &
               ai_phase, cw_phase, msectional, &
               massptr_aer, numptr_aer, &
               dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer )
         end if
         end if ! ( do_activa ) then

! do entrainment
         if (mf_up_ent(k) > 0.0_r8) then
            do l = p1st, num_chem
               tmp_mfxchem_up(l) = tmp_mfxchem_up(l) + chem_av_old(k,l)*mf_up_ent(k)
            end do
            tmp_mf_up = tmp_mf_up + mf_up_ent(k)
         end if

! do activation at first cloudy layer
! when first cloudy layer = first updraft layer
         if ( do_activa ) then
         if ((k == kcldbotliq) .and. (k == kupdrbot)) then
            iflagaa = 2
         else if (( do_2ndact ) .and. (k > kcldbotliq)) then
            iflagaa = 10
         else
            iflagaa = 0
         end if
         if (iflagaa > 0) then
            call chem_cup_activate_up( &
               lunerr, lundiag, idiagaa, &
               kts, kte, p1st, num_chem, &
               ktau, grid_id, i, j, k, iflagaa, &
               pcen(k), tcen(k), rhocen(k), qcw_inup(k), &
               rhodz(k), af_up(k), wact, &
               tmp_mf_up, tmp_mfxchem_up, dchemdt_up_activa, &
               maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
               ncomp_aer, nsize_aer, ntype_aer, &
               ai_phase, cw_phase, msectional, &
               massptr_aer, numptr_aer, &
               dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer )
         end if
         end if ! ( do_activa ) then

! do cloud chemistry
         if ( do_aqchem ) then
         if (qcw_inup(k) > qcw_cldchem_cutoff) then
            tmp_w_up = tmp_mf_up / (af_up(k) * rhocen(k))
            tmp_w_up = max( tmp_w_up, 0.1_r8 )
            tmp_dt = dz(k)/tmp_w_up

!           subr. chem_cup_aqchem( &
!              config_flags, aer_mech_id, &
!              lunerr, lundiag, idiagaa, &
!              kts, kte, p1st, num_chem, &
!              p_qc, num_moist, &
!              ktau, grid_id, i, j, &
!              iflagaa, ido_aqchem, &
!              dt_aqchem, &
!              pcen, tcen, rhocen, rhodz, qcw, ph_no2, &
!              af_up, tmp_gas_aqfrac_up, tmp_mf_up, tmp_mfxchem_up, &
!              dchemdt_up_aqchem, chem_inact )

            call chem_cup_aqchem( &
               config_flags, aer_mech_id, &
               lunerr, lundiag, idiagaa, &
               kts, kte, p1st, num_chem, &
               p_qc, num_moist, &
               ktau, grid_id, i, j, &
               k, ido_inact, &
               tmp_dt, &
               pcen, tcen, rhocen, rhodz, qcw_inup, ph_no2, &
               af_up(k), tmp_gas_aqfrac_up, tmp_mf_up, tmp_mfxchem_up, &
               dchemdt_up_aqchem, chem_inact_new )
         end if
         end if ! ( do_aqchem ) then

! do wet removal
         if ( do_wetrem ) then
! for now just do
!    fcvt_qc_to_pr transfers cloud-borne to precip, which is assumed to fall out
! eventually should track ice-borne aerosol and do
!    fcvt_qc_to_qi transfers cloud-borne to ice-borne
!    fcvt_qi_to_pr transfers   ice-borne to precip, which is assumed to fall out
!    detrained ice-borne goes to interstitial (?)
            tmpf = min( 1.0_r8, max( 0.0_r8, fcvt_qc_to_pr(k) ) )
            if (tmpf > 1.0e-10_r8) then
               if ( do_aqchem .and. (qcw_inup(k) > qcw_cldchem_cutoff) ) then
                  do l = p1st, num_chem
                     if (tmp_gas_aqfrac_up(l) <= 1.0e-10_r8) cycle
                     tmpg = min( 1.0_r8, max( 0.0_r8, tmp_gas_aqfrac_up(l)*tmpf ) )
                     ! tmpd = wet-removal change to (mf_up * chem_up) as air moves thru the layer
                     tmpd = -tmp_mfxchem_up(l)*tmpg
                     tmp_mfxchem_up(l) = tmp_mfxchem_up(l) + tmpd
                     dchemdt_up_wetrem(k,l) = tmpd/(rhodz(k)*af_up(k))
!                    if (idiagaa > 0) &
!                       write(lundiag,'(a,2i5,1p,2e10.2,2x,a)') &
!                       'gas_aqfrac', k, l, tmp_gas_aqfrac_up(l), qcw_inup(k), chem_name(l)
                  end do
               end if

               do itype = 1, ntype_aer
               do isize = 1, nsize_aer(itype)
               do icomp = 0, ncomp_aer(itype)
                  if (icomp == 0) then
                     l = numptr_aer(isize,itype,cw_phase)
                  else
                     l = massptr_aer(icomp,isize,itype,cw_phase)
                  end if
                  if ((l < p1st) .or. (l > num_chem)) cycle
                  ! tmpd = wet-removal change to (mf_up * chem_up) as air moves thru the layer
                  tmpd = -tmp_mfxchem_up(l)*tmpf  
                  tmp_mfxchem_up(l) = tmp_mfxchem_up(l) + tmpd
                  dchemdt_up_wetrem(k,l) = tmpd/(rhodz(k)*af_up(k))
               end do
               end do
               end do
            end if ! (tmpf > 1.0e-10_r8)
         end if ! ( do_wetrem )


! calculate updraft mixing ratios after above processes
         tmp_chem_up(p1st:num_chem) = tmp_mfxchem_up(p1st:num_chem)/tmp_mf_up

! do detrainment (which does not change the updraft mixing ratios
         tmp_mf_up = max( 0.0_r8, tmp_mf_up - mf_up_det(k) )
!        tmp_mf_up = mf_up(k+1)
         do l = p1st, num_chem
            tmp_mfxchem_up(l) = tmp_mf_up*tmp_chem_up(l)
         end do

! tmp_chem_up at this point is the chem_up at top of layer k
         chem_up(k+1,p1st:num_chem) = tmp_chem_up(p1st:num_chem)

! calculate resuspension of detrained aerosol 
! this occurs in occurs in the environment, so does not affect chem_up
         if ( do_resusp ) then
            tmpa = 1.0_r8 - af_lscld(k)   ! fraction resuspended = 1 - ls_cloud_fraction
            tmpa = max( 0.0_r8, min( 1.0_r8, tmpa ) )

            tmpb = tmpa * mf_up_det(k)/(rhodz(k)*af_ev(k))

!           do l1 = kts, kte
!              l2 = l1 + lgrp*3
!              l3 = l1 + lgrp*4

!              tmpc = tmp_chem_up(l2)*tmpb
!              dchemdt_ev_resusp(k,l2) = dchemdt_ev_resusp(k,l2) - tmpc
!              dchemdt_ev_resusp(k,l3) = dchemdt_ev_resusp(k,l3) + tmpc
!           end do ! l1

            do itype = 1, ntype_aer
            do isize = 1, nsize_aer(itype)
               do l2 = 0, ncomp_aer(itype)
                  if (l2 == 0) then
                     la = numptr_aer(isize,itype,ai_phase)
                     lc = numptr_aer(isize,itype,cw_phase)
                  else
                     la = massptr_aer(l2,isize,itype,ai_phase)
                     lc = massptr_aer(l2,isize,itype,cw_phase)
                  end if
                  if ((la < p1st) .or. (la > num_chem)) cycle
                  if ((lc < p1st) .or. (lc > num_chem)) cycle

                  tmpc = tmp_chem_up(lc)*tmpb
                  dchemdt_ev_resusp(k,lc) = dchemdt_ev_resusp(k,lc) - tmpc
                  dchemdt_ev_resusp(k,la) = dchemdt_ev_resusp(k,la) + tmpc
               end do ! l2
            end do ! isize
            end do ! itype

         end if ! ( do_resusp ) then

      end do updraft_mixratio_k_loop

      do l = 1, num_chem
         zav_chem_up(l) = sum( rhodz(kts:kte)*chem_up(kts:kte,l) ) / rhodzsum
      end do

      end if do_updraft_mixratio_calc


      chem_dn(:,:) = 0.0_r8   !BSINGH(03/11/2015): Moved out of the if(do_dndraft) condition to avoid uninitilized chem_dn
      zav_chem_dn(:) = 0.0_r8 !BSINGH(03/11/2015): Moved out of the if(do_dndraft) condition to avoid uninitilized zav_chem_dn
!
! calculate aerosol and gas profiles in the dndraft
!
do_dndraft_mixratio_calc: &
      if ( do_dndraft ) then

      tmp_mfxchem_dn = 0.0_r8

dndraft_mixratio_k_loop: &
      do k = kdndrtop, kdndrbot, -1

         tmp_mf_dn = mf_dn(k+1)
! at this point, tmp_mf_dn and tmp_mvxchem_dn are at the layer top

! do entrainment
         if (mf_dn_ent(k) > 0.0_r8) then
            do l = p1st, num_chem
               tmp_mfxchem_dn(l) = tmp_mfxchem_dn(l) - chem_av_old(k,l)*mf_dn_ent(k)
            end do
            tmp_mf_dn = tmp_mf_dn - mf_dn_ent(k)
         end if
! at this point, tmp_mf_dn and tmp_mvxchem_dn are at the layer bottom
! (but before detrainment)

! calculate dndraft mixing ratios after above processes
         tmp_chem_dn(p1st:num_chem) = tmp_mfxchem_dn(p1st:num_chem)/tmp_mf_dn

! do detrainment (which does not change the dndraft mixing ratios
         tmp_mf_dn = min( 0.0_r8, tmp_mf_dn + mf_dn_det(k) )
         do l = p1st, num_chem
            tmp_mfxchem_dn(l) = tmp_mf_dn*tmp_chem_dn(l)
         end do

! tmp_chem_dn at this point is the chem_dn at bottom of layer k
         chem_dn(k,p1st:num_chem) = tmp_chem_dn(p1st:num_chem)

! calculate resuspension of detrained aerosol 
! this occurs in occurs in the environment, so does not affect chem_dn
         if ( do_resusp ) then
!           tmpa = 1.0_r8 - af_lscld(k)   ! fraction resuspended = 1 - ls_cloud_fraction
!           tmpa = max( 0.0_r8, min( 1.0_r8, tmpa ) )
            tmpa = 0.0_r8  ! assume that downdraft is subsaturated at detrainment level
                           ! so any cloud-borne aerosol is resuspended

            tmpb = tmpa * mf_dn_det(k)/(rhodz(k)*af_ev(k))

            do itype = 1, ntype_aer
            do isize = 1, nsize_aer(itype)
               do l2 = 0, ncomp_aer(itype)
                  if (l2 == 0) then
                     la = numptr_aer(isize,itype,ai_phase)
                     lc = numptr_aer(isize,itype,cw_phase)
                  else
                     la = massptr_aer(l2,isize,itype,ai_phase)
                     lc = massptr_aer(l2,isize,itype,cw_phase)
                  end if
                  if ((la < p1st) .or. (la > num_chem)) cycle
                  if ((lc < p1st) .or. (lc > num_chem)) cycle

                  tmpc = tmp_chem_dn(lc)*tmpb
                  dchemdt_ev_resusp(k,lc) = dchemdt_ev_resusp(k,lc) - tmpc
                  dchemdt_ev_resusp(k,la) = dchemdt_ev_resusp(k,la) + tmpc
               end do ! l2
            end do ! isize
            end do ! itype

         end if ! ( do_resusp ) then

      end do dndraft_mixratio_k_loop

      do l = 1, num_chem
         zav_chem_dn(l) = sum( rhodz(kts:kte)*chem_dn(kts:kte,l) ) / rhodzsum
      end do

      end if do_dndraft_mixratio_calc


!
!
! now solve mass conservation for timestep dtsub
!
!
      if ( do_dndraft ) then
         kaa = min( kupdrbot, kdndrbot ) ; kzz = max( kupdrtop, kdndrtop )
      else
         kaa = kupdrbot ; kzz = kupdrtop
      end if

      do l = p1st, num_chem

      tmp_zflux_top = 0.0_r8

      do k = kaa, kzz

      tmp_zflux_bot   = tmp_zflux_top

      !BSINGH(12/19/2013) - Commented out the following (tmp_zflux_botev, tmp_zflux_botup and tmp_zflux_botdn)
      !variables as "tmp_zflux_topev" is undefined at this point and other variables
      !(tmp_zflux_botev, tmp_zflux_botup and tmp_zflux_botdn) are acting as dummy variables in the code

      !tmp_zflux_botev = tmp_zflux_topev
      !tmp_zflux_botup = tmp_zflux_topup
      !tmp_zflux_botdn = tmp_zflux_topdn

      if (mf_ev(k+1) >= 0.0_r8) then
         tmp_zflux_topev = mf_ev(k+1)*chem_av_old(k,l)
      else
         tmp_zflux_topev = mf_ev(k+1)*chem_av_old(k+1,l)
      end if

      tmp_zflux_topup = mf_up(k+1)*chem_up(k+1,l)
      tmp_zflux_topdn = mf_dn(k+1)*chem_dn(k+1,l)  ! this will be zero when do_dndraft=.false.

      tmp_zflux_top = tmp_zflux_topev + tmp_zflux_topup + tmp_zflux_topdn

      tmpa = (tmp_zflux_bot - tmp_zflux_top)*dtsub/rhodz(k)
      tmpb = dchemdt_up_activa(k,l)*af_up(k)*dtsub
      tmpc = dchemdt_up_aqchem(k,l)*af_up(k)*dtsub
      tmpd = dchemdt_up_wetrem(k,l)*af_up(k)*dtsub
      tmpe = dchemdt_ev_resusp(k,l)*af_ev(k)*dtsub

      del_chem_ztrans(k,l) = del_chem_ztrans(k,l) + tmpa
      del_chem_activa(k,l) = del_chem_activa(k,l) + tmpb
      del_chem_aqchem(k,l) = del_chem_aqchem(k,l) + tmpc
      del_chem_wetrem(k,l) = del_chem_wetrem(k,l) + tmpd
      del_chem_resusp(k,l) = del_chem_resusp(k,l) + tmpe

      chem_av_new(k,l) = chem_av_old(k,l) + (tmpa + tmpb + tmpc + tmpd + tmpe)

      if (chem_av_new(k,l) < 0.0_r8) then
         del_chem_residu(k,l) = del_chem_residu(k,l) - chem_av_new(k,l)
         chem_av_new(k,l) = 0.0_r8
      end if
      del_chem_totall(k,l) = del_chem_totall(k,l) + (chem_av_new(k,l) - chem_av_old(k,l))

!     if (idiagaa > 0) then
!     if ((l == numptr_aer(2,1,ai_phase)) .or. (l == numptr_aer(2,1,cw_phase))) then 
!        tmpveca(1) = (tmp_zflux_botev - tmp_zflux_topev)*dtsub/rhodz(k)
!        tmpveca(2) = (tmp_zflux_botup - tmp_zflux_topup)*dtsub/rhodz(k)
!        tmpveca(3) = (tmp_zflux_botdn - tmp_zflux_topdn)*dtsub/rhodz(k)
!        tmpveca(4) = tmpb
!        tmpveca(5) = tmpc
!        tmpveca(6) = tmpd
!        tmpveca(7) = tmpe
!        write(lundiag,'(i4,1p,7e12.4,3x,2a)') k, tmpveca(1:7), 'tends ', chem_name(l)
!     end if
!     end if

      end do ! k

      zav_chem_av_new(l)     = sum( rhodz(kts:kte)*chem_av_new(    kts:kte,l) ) / rhodzsum
      zav_del_chem_activa(l) = sum( rhodz(kts:kte)*del_chem_activa(kts:kte,l) ) / rhodzsum
      zav_del_chem_aqchem(l) = sum( rhodz(kts:kte)*del_chem_aqchem(kts:kte,l) ) / rhodzsum
      zav_del_chem_residu(l) = sum( rhodz(kts:kte)*del_chem_residu(kts:kte,l) ) / rhodzsum
      zav_del_chem_resusp(l) = sum( rhodz(kts:kte)*del_chem_resusp(kts:kte,l) ) / rhodzsum
      zav_del_chem_totall(l) = sum( rhodz(kts:kte)*del_chem_totall(kts:kte,l) ) / rhodzsum
      zav_del_chem_wetrem(l) = sum( rhodz(kts:kte)*del_chem_wetrem(kts:kte,l) ) / rhodzsum
      zav_del_chem_ztrans(l) = sum( rhodz(kts:kte)*del_chem_ztrans(kts:kte,l) ) / rhodzsum

      end do ! l


! diagnostic outputs
      if (idiagaa > 0) then
         call chem_cup_1d_diags_pt21( &
         lundiag, kdiagbot, kdiagtop, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, jtsub, ntsub, &
         ishall, do_dndraft, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         rhodz, rhodzsum, &
         chem_av_new, chem_av_old, chem_up, chem_dn, &
         zav_chem_av_new, zav_chem_av_old, zav_chem_up, zav_chem_dn, &
         zav_del_chem_activa, zav_del_chem_aqchem, &
         zav_del_chem_residu, zav_del_chem_resusp, &
         zav_del_chem_wetrem, zav_del_chem_ztrans, &
         zav_del_chem_totall, &
         chem_name ) 
      end if ! (idiagaa > 0)


      end do active_cloud_jtsub_loop


! mass balance check after active cloud calcs
      itmpa = 0
      tmpveca(1:20) = 0.0_r8
      do l = p1st, num_chem
         tmpa = 0.0_r8 ; tmpb = 0.0_r8 ; tmpd = 0.0_r8
         do k = kts, kte
            tmpa = tmpa +  rhodz(k)*chem(k,l)
            tmpb = tmpb +  rhodz(k)*chem_av_new(k,l)
            tmpd = tmpd +  rhodz(k)*(chem_av_new(k,l) - chem(k,l))
         end do ! k
         tmpa = tmpa/rhodzsum      ! initial column-avg mix-ratio
         tmpb = tmpb/rhodzsum      ! ending column-avg mix-ratio
         tmpd = tmpd/rhodzsum      ! column-avg mix-ratio change

         tmpvecb(11) = zav_del_chem_activa(l)
         tmpvecb(12) = zav_del_chem_resusp(l)
         tmpvecb(13) = zav_del_chem_aqchem(l)
         tmpvecb(14) = zav_del_chem_wetrem(l)
         tmpvecb(15) = zav_del_chem_ztrans(l)
         tmpvecb(16) = zav_del_chem_residu(l)

         tmpe = sum( tmpvecb(11:16) )  ! column-avg m.r. changes by processes
         tmpf = sum( max(  tmpvecb(11:16), 0.0 ) )
         tmpg = sum( max( -tmpvecb(11:16), 0.0 ) )
         tmpf = max( tmpf, tmpg )
         tmpg = (tmpd-tmpe)/max( tmpa, tmpb, tmpf, 1.0e-30_r8 )  ! normalized residual

         if (abs(tmpg) > abs(tmpveca(1))) then
            itmpa = l
            tmpveca(1) = tmpg
            tmpveca(2) = tmpd-tmpe
            tmpveca(3) = tmpd
            tmpveca(4) = tmpe
            tmpveca(5) = tmpa
            tmpveca(6) = tmpb
            tmpveca(11:16) = tmpvecb(11:16)
         end if
         if ((idiagbb > 0) .and. (abs(tmpg) > rerrtol1_mbal)) then
            write(lundiagbb,'(/a,i10,3i5,1p,6e11.3,2x,a)') &
               'chem_cup_1d massbal       active -', ktau, grid_id, i, j, &
               tmpg, (tmpd-tmpe), tmpd, tmpe, tmpa, tmpb, chem_name(l)
            write(lundiagbb,'(2a,1p,8e11.3)') &
                'zav_del_chem_activa, resusp, ', &
                'aqchem, wetrem, ztrans, residu', tmpvecb(11:16)
         end if
      end do ! l

      if (idiagaa > 0) then
         msg = 'perfect' ; if (itmpa > 0) msg = chem_name(itmpa)
         write(lundiag,'(/a,i10,3i5,1p,6e11.3,2x,a)') &
            'chem_cup_1d massbal worst active -', &
            ktau, grid_id, i, j, tmpveca(1:6), msg(1:12)
         write(lundiag,'(2a,1p,8e11.3)') &
             'zav_del_chem_activa, resusp, ', &
             'aqchem, wetrem, ztrans, residu', tmpveca(11:16)
      end if


!
!
! now do calculations for inactive cloud
!
! > start with chem_av_new in the entire grid-area
! > divide the grid area into 
!    > inactive cloud portion with area fraction = af_cucld(k) - af_up(k)
!    > "other" portion = the rest
! > in inactive cloud portion
!    > start with chem_inact_old = chem_av_new
!      (cannot use chem_up here because in the steady-state updraft approach, 
!       the updrafts process aerosol/gas mass, 
!       but they do not really contain any aerosol/gas mass)
!    > set interstitial/activated fractions based on 
!         the fractions in the updraft
!    > do cloud chemistry
!    > do partial aerosol resuspension before "dissipating" 
!         the inactive cloud back to the grid-average
! > "dissipate" the inactive cloud back to the grid-average
!
      af_inact = 0.0
      ido_inact = 0
      chem_inact_new = -1.0e10
      chem_inact_dsp = -1.0e10
      if ( .not. do_inact ) goto 79000

      itmpa = 0
      do k = kcldbot, kcldtop
         ! count levels that have qcw>0 both in cumulus and in updraft 
         ! and also have cumulus area fraction > updraft area fraction
         tmpa = af_cucld(k)-af_up(k)
         if ( (qcw_incu(k) > 0.0) .and. &
              (qcw_inup(k) > 0.0) .and. &
              (tmpa >= af_cucld_smallaa*0.5) ) then
            af_inact(k) = max( 0.0_r8, min( 1.0_r8, tmpa ) )
            ido_inact(k) = 1
            itmpa = itmpa + 1
         end if
      end do

      if (itmpa <= 0)then
         if (idiagaa > 0) write(lundiag,'(/a,4i10)') &
            'chem_cup_1d - no inactive cloud calcs - ktau, id, i, j =', &
            ktau, grid_id, i, j

         chem(:,:) = chem_av_new(:,:)  ! put updated mix-ratios into chem
         goto 79000
      end if


! set initial mixing ratios for inactive cloud
      chem_av_old(:,:) = chem_av_new(:,:)
      chem_inact_old(:,:) = chem_av_new(:,:)

      if (idiagaa > 0) write(lundiag,'(//a)') 'inactive k, fmact, fnact'
      do k = kcldtop, kcldbot, -1
         if (ido_inact(k) <= 0) cycle

         tmp_fmact = 0.0_r8 ; tmp_fnact = 0.0_r8
         do itype = 1, ntype_aer
         do isize = 1, nsize_aer(itype)
            tmpg = 0.0_r8
            do l2 = 0, ncomp_aer(itype)
               if (l2 == 0) then
                  la = numptr_aer(isize,itype,ai_phase)
                  lc = numptr_aer(isize,itype,cw_phase)
               else
                  la = massptr_aer(l2,isize,itype,ai_phase)
                  lc = massptr_aer(l2,isize,itype,cw_phase)
               end if
               if ((la < p1st) .or. (la > num_chem)) cycle
               if ((lc < p1st) .or. (lc > num_chem)) cycle

               tmpa = max( chem_up(k+1,la), 1.0e-35_r8 )
               tmpc = max( chem_up(k+1,lc), 1.0e-35_r8 )
               tmpd = tmpa + tmpc
               tmpe = max( chem_inact_old(k,la) + chem_inact_old(k,lc), 0.0_r8 )
               ! apply interstitial/activated fractions from the updraft
               chem_inact_old(k,la) = tmpe*(tmpa/tmpd)
               chem_inact_old(k,lc) = tmpe*(tmpc/tmpd)

               del_chem_actvbb(k,la) = af_inact(k)*(chem_inact_old(k,la) - chem_av_new(k,la))
               del_chem_actvbb(k,lc) = af_inact(k)*(chem_inact_old(k,lc) - chem_av_new(k,lc))

               if (l2 == 0) then
                  tmp_fnact(isize,itype) = tmpc/tmpd
               else
                  tmpe = max( tmpe, 1.0e-35_r8 )
                  tmp_fmact(isize,itype) = tmp_fmact(isize,itype) + tmpe*(tmpc/tmpd)
                  tmpg = tmpg + tmpe
               end if
            end do ! l2
            tmp_fmact(isize,itype) = tmp_fmact(isize,itype)/tmpg
         end do ! isize
         if (idiagaa > 0) &
            write(lundiag,'( i3,2(2x,8f8.4) / (3x,2(2x,8f8.4)) )') k, &
               tmp_fmact(1:nsize_aer(itype),itype), tmp_fnact(1:nsize_aer(itype),itype)
! Added ',itype' in tmp_fmact and tmp_fnact arrays by Manish Shrivastava 
         end do ! itype

         chem_inact_new(k,:) = chem_inact_old(k,:)
      end do ! k


      if ( do_aqchem ) then

! do cloud chemistry
!     subr. chem_cup_aqchem( &
!        config_flags, aer_mech_id, &
!        lunerr, lundiag, idiagaa, &
!        kts, kte, p1st, num_chem, &
!        p_qc, num_moist, &
!        ktau, grid_id, i, j, &
!        iflagaa, ido_aqchem, &
!        dt_aqchem, &
!        pcen, tcen, rhocen, rhodz, qcw, ph_no2, &
!        af_up, tmp_gas_aqfrac_up, tmp_mf_up, tmp_mfxchem_up, &
!        dchemdt_up_aqchem, chem_inact )

      itmpb = kts-1
      call chem_cup_aqchem( &
         config_flags, aer_mech_id, &
         lunerr, lundiag, idiagaa, &
         kts, kte, p1st, num_chem, &
         p_qc, num_moist, &
         ktau, grid_id, i, j, &
         itmpb, ido_inact, &
         tau_inactive, &
         pcen, tcen, rhocen, rhodz, qcw_incu, ph_no2, &
         af_up(kts), tmp_gas_aqfrac_up, tmp_mf_up, tmp_mfxchem_up, &
         dchemdt_up_aqchem, chem_inact_new )

      do k = kcldbot, kcldtop
         if (ido_inact(k) <= 0) cycle
         do l = p1st, num_chem
            del_chem_aqchbb(k,l) = af_inact(k)*(chem_inact_new(k,l) - chem_inact_old(k,l))
         end do
      end do

      end if ! ( do_aqchem )


! do "dissipation" of the cumulus cloud,
!    because there is no "memory" for the aerosols/gases within the cumulus cloud
!
! as part of this, do partial resuspension of activated aerosols in the inactive cloud
!    part of the cumulus cloud goes to grid-resolved cloud, and part goes to clear air
!    if the grid-resolved cloud fraction is 0.0, 
!       then all  of the activated aerosols in the cumulus cloud are resuspended
!    if the grid-resolved cloud fraction is 1.0, 
!       then none of the activated aerosols in the cumulus cloud are resuspended

      chem_inact_dsp = chem_inact_new
      do k = kcldbot, kcldtop
         if (ido_inact(k) <= 0) cycle

! first do the resuspension
         if ( do_resusp ) then
            tmpa = 1.0_r8 - af_lscld(k)   ! fraction resuspended = 1 - ls_cloud_fraction
            tmpa = max( 0.0_r8, min( 1.0_r8, tmpa ) )

            if (tmpa > 0.0) then
            do itype = 1, ntype_aer
            do isize = 1, nsize_aer(itype)
               do l2 = 0, ncomp_aer(itype)
                  if (l2 == 0) then
                     la = numptr_aer(isize,itype,ai_phase)
                     lc = numptr_aer(isize,itype,cw_phase)
                  else
                     la = massptr_aer(l2,isize,itype,ai_phase)
                     lc = massptr_aer(l2,isize,itype,cw_phase)
                  end if
                  if ((la < p1st) .or. (la > num_chem)) cycle
                  if ((lc < p1st) .or. (lc > num_chem)) cycle

                  tmpc = chem_inact_new(k,lc)*tmpa
                  chem_inact_dsp(k,lc) = chem_inact_new(k,lc) - tmpc
                  chem_inact_dsp(k,la) = chem_inact_new(k,la) + tmpc
                  del_chem_respbb(k,lc) = -tmpc*af_inact(k)
                  del_chem_respbb(k,la) = tmpc*af_inact(k)
               end do ! l2
            end do ! isize
            end do ! itype
            end if ! (tmpa > 0.0)
         end if ! ( do_resusp )

! now the dissipation
         tmpb = 1.0 - af_inact(k)
         do l = p1st, num_chem
            chem_av_new(k,l) = tmpb*chem_av_old(k,l) + af_inact(k)*chem_inact_dsp(k,l)
            del_chem_totlbb(k,l) = chem_av_new(k,l) - chem_av_old(k,l)
            del_chem_resdbb(k,l) = del_chem_totlbb(k,l) &
               - ( del_chem_actvbb(k,l) + del_chem_aqchbb(k,l) + del_chem_respbb(k,l) ) 
         end do ! l

      end do ! k

      do l = p1st, num_chem
      zav_del_chem_actvbb(l) = sum( rhodz(kts:kte)*del_chem_actvbb(kts:kte,l) ) / rhodzsum
      zav_del_chem_aqchbb(l) = sum( rhodz(kts:kte)*del_chem_aqchbb(kts:kte,l) ) / rhodzsum
      zav_del_chem_resdbb(l) = sum( rhodz(kts:kte)*del_chem_resdbb(kts:kte,l) ) / rhodzsum
      zav_del_chem_respbb(l) = sum( rhodz(kts:kte)*del_chem_respbb(kts:kte,l) ) / rhodzsum
      zav_del_chem_totlbb(l) = sum( rhodz(kts:kte)*del_chem_totlbb(kts:kte,l) ) / rhodzsum
      end do


! diagnostics
      if (idiagaa > 0) then
         call chem_cup_1d_diags_pt41( &
         lundiag, 1, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         rhodz, rhodzsum, &
         chem_av_old, chem_av_new, &
         zav_del_chem_actvbb, zav_del_chem_aqchbb, &
         zav_del_chem_resdbb, zav_del_chem_respbb, &
         zav_del_chem_totlbb, zav_del_chem_wetrem, &
         chem_name ) 
      end if ! (idiagaa > 0)


! mass balance check after inactive cloud calcs
      itmpa = 0
      tmpveca(1:20) = 0.0
      do l = p1st, num_chem
         tmpa = 0.0_r8 ; tmpb = 0.0_r8 ; tmpd = 0.0_r8
         do k = kts, kte
            tmpa = tmpa +  rhodz(k)*chem(k,l)
            tmpb = tmpb +  rhodz(k)*chem_av_new(k,l)
            tmpd = tmpd +  rhodz(k)*(chem_av_new(k,l) - chem(k,l))
         end do ! k
         tmpa = tmpa/rhodzsum      ! initial column-avg mix-ratio
         tmpb = tmpb/rhodzsum      ! ending column-avg mix-ratio
         tmpd = tmpd/rhodzsum      ! column-avg mix-ratio change

         tmpvecb(11) = zav_del_chem_activa(l)
         tmpvecb(12) = zav_del_chem_resusp(l)
         tmpvecb(13) = zav_del_chem_aqchem(l)
         tmpvecb(14) = zav_del_chem_wetrem(l)
         tmpvecb(15) = zav_del_chem_ztrans(l)
         tmpvecb(16) = zav_del_chem_residu(l)
         tmpvecb(17) = zav_del_chem_actvbb(l)
         tmpvecb(18) = zav_del_chem_respbb(l)
         tmpvecb(19) = zav_del_chem_aqchbb(l)

         tmpe = sum( tmpvecb(11:14) )  + sum( tmpvecb(17:19) )  ! column-avg m.r. changes by processes
         tmpf = sum( max(  tmpvecb(11:14), 0.0 ) ) + sum( max(  tmpvecb(17:19), 0.0 ) )
         tmpg = sum( max( -tmpvecb(11:14), 0.0 ) ) + sum( max( -tmpvecb(17:19), 0.0 ) )
         tmpf = max( tmpf, tmpg )
         tmpg = (tmpd-tmpe)/max( tmpa, tmpb, tmpf, 1.0e-30_r8 )  ! normalized residual

         if (abs(tmpg) > abs(tmpveca(1))) then
            itmpa = l
            tmpveca(1) = tmpg
            tmpveca(2) = tmpd-tmpe
            tmpveca(3) = tmpd
            tmpveca(4) = tmpe
            tmpveca(5) = tmpa
            tmpveca(6) = tmpb
            tmpveca(11:19) = tmpvecb(11:19)
         end if
         if ((idiagbb > 0) .and. (abs(tmpg) > rerrtol1_mbal)) then
            write(lundiagbb,'(/a,i10,3i5,1p,6e11.3,2x,a)') &
               'chem_cup_1d massbal       final  -', ktau, grid_id, i, j, &
               tmpg, (tmpd-tmpe), tmpd, tmpe, tmpa, tmpb, chem_name(l)
            write(lundiagbb,'(2a,1p,8e11.3)') &
                'zav_del_chem_activa, resusp, ', &
                'aqchem, wetrem, ztrans, residu', tmpvecb(11:16)
            write(lundiagbb,'(2a,1p,8e11.3)') &
                'zav_del_chem_actvbb, respbb, ', &
                'aqchbb                        ', tmpvecb(17:19)
         end if
      end do ! l

      if (idiagaa > 0) then
         msg = 'perfect' ; if (itmpa > 0) msg = chem_name(itmpa)
         write(lundiag,'(/a,i10,3i5,1p,6e11.3,2x,a)') &
            'chem_cup_1d massbal worst final  -', &
            ktau, grid_id, i, j, tmpveca(1:6), msg(1:12)
         write(lundiag,'(2a,1p,8e11.3)') &
             'zav_del_chem_activa, resusp, ', &
             'aqchem, wetrem, ztrans, residu', tmpveca(11:16)
         write(lundiag,'(2a,1p,8e11.3)') &
             'zav_del_chem_actvbb, respbb, ', &
             'aqchbb                        ', tmpveca(17:19)
      end if


! diagnostics
      if (idiagaa > 0) then
         call chem_cup_1d_diags_pt41( &
         lundiag, 2, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         rhodz, rhodzsum, &
         chem, chem_av_new, &
         zav_del_chem_actvbb, zav_del_chem_aqchbb, &
         zav_del_chem_resdbb, zav_del_chem_respbb, &
         zav_del_chem_totlbb, zav_del_chem_wetrem, &
         chem_name ) 
      end if ! (idiagaa > 0)


! put final mixing ratios into chem
      chem(:,:) = chem_av_new(:,:)


79000 continue ! "goto 79000" means no inactive cloud calcs


! calculate average in-convective-cloud mixing ratios
! The code below calculates average in-convective-cloud mixing ratios
! Commented out by Manish Shrivastava on 05/14/2013 because we wanted to look at just the active part values of CO_IC_CUP and other  variables
!--------------------------------------------------------------------------------
!      do k = kcldbot, kcldtop
!         if (af_up(k) < 0.5*af_up_smallaa) then

!            if (ido_inact(k) <= 0) then
!               cycle  ! note that chem_incu and chem_cupflag are initialized
!                      ! to zero, so those will be the values at this level
!            else
!               tmpa = 0.0_r8 ; tmpb = 1.0_r8
!            end if
!         else
!            if (ido_inact(k) <= 0) then
!               tmpa = 1.0_r8 ; tmpb = 0.0_r8
!            else
!               tmpa = af_up(k)/(af_up(k) + af_inact(k))
!               tmpa = max( 0.0_r8, min( 1.0_r8, tmpa ) )
!               tmpb = 1.0_r8 - tmpa
!            end if
!         end if
!---------------------------------------------------------------------------------

!The code below was changed by Easter Richard to calculate just active part cloud values
!It has changes so that the chem_incu will be equal to the updraft mixing ratios,
!Rather than area-weighted average of updraft and inactive cloud.

!------------------------------------------------------------------------------------
      do k = kcldbot, kcldtop
         if (af_up(k) < 0.5*af_up_smallaa) then

            cycle   ! change to make chem_incu = chem_up (but no updraft at this k)
         else
            tmpa = 1.0_r8 ; tmpb = 0.0_r8   ! change to make chem_incu = chem_up
         end if
!-----------------------------------------------------------------------------------------------
         ! chem_up(k,:) is at top of layer k
         ! when k>kcldbot, use an average of the k and k+1 values
         ! when k=kcldbot, use the k+1 values because the k values are essentially 
         !                 clear air (at the top of the last sub-cloud-base layer)
         if (k == kcldbot) then
            chem_incu(k,:) = tmpa*chem_up(k+1,:) + tmpb*chem_inact_new(k,:)
         else
            chem_incu(k,:) = tmpa*0.5*(chem_up(k,:)+chem_up(k+1,:)) + tmpb*chem_inact_new(k,:)
         end if
         chem_cupflag(k) = 1
      end do

      if (idiagaa > 0) then
         call chem_cup_1d_diags_pt71( &
         lundiag, kcldbot, kcldtop, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         af_up, af_inact, &
         chem_av_new, chem_up, chem_inact_new, chem_incu, &
         chem_name ) 
      end if


89000 continue ! "goto 89000" means check_adjust_inputs failed
      if (idiagaa > 0) write(lundiag,'(/a,4i10)') &
         'chem_cup_1d done  ktau, id, i, j =', ktau, grid_id, i, j

      return
      end subroutine chem_cup_1d


!-------------------------------------------------------------------------------
      subroutine chem_cup_1d_diags_pt21( &
         lundiag_inp, kdiagbot, kdiagtop, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, jtsub, ntsub, &
         ishall, do_dndraft, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         rhodz, rhodzsum, &
         chem_av_new, chem_av_old, chem_up, chem_dn, &
         zav_chem_av_new, zav_chem_av_old, zav_chem_up, zav_chem_dn, &
         zav_del_chem_activa, zav_del_chem_aqchem, &
         zav_del_chem_residu, zav_del_chem_resusp, &
         zav_del_chem_wetrem, zav_del_chem_ztrans, &
         zav_del_chem_totall, &
         chem_name ) 

      use module_configure, only:  &
         p_qc, &
         p_h2o2, p_hcl, p_hno3, p_nh3, p_so2, p_sulf

! arguments
      integer, intent(in) :: lundiag_inp
      integer, intent(in) :: kts, kte, ktep1, p1st, num_chem
      integer, intent(in) :: ktau, grid_id, i, j, jtsub, ntsub
      integer, intent(in) :: kdiagbot, kdiagtop
      integer, intent(in) :: ishall

      integer, intent(in) :: maxd_acomp, maxd_aphase, maxd_atype, maxd_asize
      integer, intent(in) :: &
         nphase_aer, ntype_aer, &
         ai_phase, cw_phase, &
         nsize_aer( maxd_atype ), &
         ncomp_aer( maxd_atype ), &
         massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), &
         numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
         lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_oin_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_cl_aer(maxd_asize, maxd_atype, maxd_aphase)

      logical, intent(in) :: do_dndraft

      real(r8), intent(in) :: rhodzsum

      real(r8), intent(in), dimension(kts:kte) :: rhodz

      real(r8), intent(in), dimension(kts:kte,1:num_chem) :: &
         chem_av_new, chem_av_old

      real(r8), intent(in), dimension(kts:ktep1,1:num_chem) :: chem_up, chem_dn

      real(r8), intent(in), dimension(1:num_chem) :: &
         zav_chem_av_new, zav_chem_av_old, zav_chem_up, zav_chem_dn, &
         zav_del_chem_activa, zav_del_chem_aqchem, &
         zav_del_chem_residu, zav_del_chem_resusp, &
         zav_del_chem_wetrem, zav_del_chem_ztrans, &
         zav_del_chem_totall

      character(len=12), intent(in) :: chem_name(num_chem)

! local variables
      integer, parameter :: mxg_max=100, nxg_max=8
      integer :: aip, cwp, typ
      integer :: isize, itype, itmpa
      integer :: l, l2, l3, lundiag
      integer :: lxg(nxg_max,mxg_max)
      integer :: k
      integer :: m, mxg
      integer :: n, nxg(mxg_max)

      real(r8) :: fxg(nxg_max,mxg_max)
      real(r8) :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg
      real(r8) :: tmpveca(num_chem)

      character(len=80)  :: fmtaa, fmtbb, fmtcc, fmtdd, fmtee


      lundiag = lundiag_inp
!     lundiag = 122

      m = 1
      nxg(m) = 3
      lxg(1:nxg(m),m) = (/ p_h2o2, p_so2, p_sulf /)
      fxg(1:nxg(m),m) = 1.0e3
      m = m + 1
      nxg(m) = 2
      lxg(1:nxg(m),m) = (/ p_nh3, p_hno3 /)
      fxg(1:nxg(m),m) = 1.0e3

      aip = ai_phase ; cwp = cw_phase
      typ = 1 ! aerosol type
      if (cw_phase > 0) then
         n = 2 ! size bin
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ lptr_so4_aer(n,typ,aip), lptr_so4_aer(n,typ,cwp) /)
         fxg(1:nxg(m),m) = 28.966/96.0
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ lptr_nh4_aer(n,typ,aip), lptr_nh4_aer(n,typ,cwp) /)
         fxg(1:nxg(m),m) = 28.966/18.0
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ lptr_no3_aer(n,typ,aip), lptr_no3_aer(n,typ,cwp) /)
         fxg(1:nxg(m),m) = 28.966/62.0
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ numptr_aer(n,typ,aip), numptr_aer(n,typ,cwp) /)
         fxg(1:nxg(m),m) = 1.0e-6
      else
         n = 2 ! size bin
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ lptr_so4_aer(n,typ,aip), lptr_nh4_aer(n,typ,aip) /)
         fxg(1:nxg(m),m) = (/ 28.966/96.0, 28.966/18.0 /)
         m = m + 1
         nxg(m) = 2
         lxg(1:nxg(m),m) = (/ lptr_no3_aer(n,typ,aip), numptr_aer(n,typ,aip) /)
         fxg(1:nxg(m),m) = (/ 28.966/62.0, 1.0e-6 /)
      end if
      mxg = m


      write(lundiag,'(/a,i10,5i5)') &
         'chem_cup_1d_diags_pt21 - ktau, id, i, j =', ktau, grid_id, i, j
      do m = 1, mxg
         n = nxg(m)
         write(lundiag,'(a9,2i3,3x,5(i5, 8x  ))') 'm, n, lxg', m, n, (lxg(l2,m), l2=1,n)
         write(lundiag,'(15x,   3x,5(a12,1x  ))') (chem_name(lxg(l2,m)), l2=1,n)
         write(lundiag,'(15x,   5(1p,e13.3))') (fxg(l2,m), l2=1,n)
      end do
      write(lundiag,'(/2a)') '*** units for following:  ', &
         'trace gas and aerosol mass = ppb,  aerosol number = #/mg'
      

      write(lundiag,'(/a,i10,5i5)') &
         'ktau, jtsub, ntsub, id, i, j =', ktau, jtsub, ntsub, grid_id, i, j

      do m = 1, mxg
!        cycle
         n = nxg(m)
         if ( do_dndraft ) then
            if (n == 3) then
               fmtaa = '(/4x,    4(3x,a33  ))'
               fmtbb = '( 4x,    4(3x,3a11 ))'
               fmtcc = '( i2, 1p,4(3x,3e11.3))'
               fmtdd = '(90x,a20,1p,3x,3e11.3)'
            else
               fmtaa = '(/4x,    4(3x,a22  ))'
               fmtbb = '( 4x,    4(3x,2a11 ))'
               fmtcc = '( i2, 1p,4(3x,2e11.3))'
               fmtdd = '(57x,a20,1p,3x,2e11.3)'
            end if
         else
            if (n == 3) then
               fmtaa = '(/4x,    3(3x,a33  ))'
               fmtbb = '( 4x,    3(3x,3a11 ))'
               fmtcc = '( i2, 1p,3(3x,3e11.3))'
               fmtdd = '(54x,a20,1p,3x,3e11.3)'
            else
               fmtaa = '(/4x,    3(3x,a22  ))'
               fmtbb = '( 4x,    3(3x,2a11 ))'
               fmtcc = '( i2, 1p,3(3x,2e11.3))'
               fmtdd = '(32x,a20,1p,3x,2e11.3)'
            end if
         end if
 
         if ( do_dndraft ) then
            write(lundiag,fmtaa) &
               'chem_up                                 ', &
               'chem_dn                                 ', &
               'chem_av_old                             ', &
               'chem_av_new                             '
            write(lundiag,fmtbb) &
               ( ( chem_name(lxg(l2,m)), l2=1,n ), l3=1,4 )
         else
            write(lundiag,fmtaa) &
               'chem_up                                 ', &
               'chem_av_old                             ', &
               'chem_av_new                             '
            write(lundiag,fmtbb) &
               ( ( chem_name(lxg(l2,m)), l2=1,n ), l3=1,3 )
         end if

         itmpa = 0
         do l2 = 1, n
            l = lxg(l2,n)
            if (zav_chem_up(l) /= 0.0_r8 ) then
               itmpa = 1
               cycle
            end if
            tmpa = 0.0
            do k = kts, kte
               tmpa = max( tmpa, abs( chem_av_new(k,l)-chem_av_old(k,l) ) )
            end do
            if (tmpa /= 0.0_r8 ) itmpa = 1
         end do

         do k = kdiagtop, kdiagbot, -1
            if (itmpa == 0) cycle
            if ( do_dndraft ) then
               write(lundiag,fmtcc) k, &
                 ( chem_up(    k,lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
                 ( chem_dn(    k,lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
                 ( chem_av_old(k,lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
                 ( chem_av_new(k,lxg(l2,m))*fxg(l2,m) , l2=1,n )
            else
               write(lundiag,fmtcc) k, &
                 ( chem_up(    k,lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
                 ( chem_av_old(k,lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
                 ( chem_av_new(k,lxg(l2,m))*fxg(l2,m) , l2=1,n )
            end if
         end do ! k
         if (itmpa > 0) write(lundiag,'(a)')
         if ( do_dndraft ) then
            write(lundiag,fmtcc) -1, &
               ( zav_chem_up(    lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
               ( zav_chem_dn(    lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
               ( zav_chem_av_old(lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
               ( zav_chem_av_new(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         else
            write(lundiag,fmtcc) -1, &
               ( zav_chem_up(    lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
               ( zav_chem_av_old(lxg(l2,m))*fxg(l2,m) , l2=1,n ), &
               ( zav_chem_av_new(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         end if
         write(lundiag,fmtdd) &
             'zav_del_chem_totall ', &
            ( zav_del_chem_totall(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         if (itmpa == 0) cycle
         write(lundiag,fmtdd) &
             'zav_del_chem_aqchem ', &
            ( zav_del_chem_aqchem(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         write(lundiag,fmtdd) &
             'zav_del_chem_wetrem ', &
            ( zav_del_chem_wetrem(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         write(lundiag,fmtdd) &
             'zav_del_chem_activa ', &
            ( zav_del_chem_activa(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         write(lundiag,fmtdd) &
             'zav_del_chem_resusp ', &
            ( zav_del_chem_resusp(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         write(lundiag,fmtdd) &
             'zav_del_chem_ztrans ', &
            ( zav_del_chem_ztrans(lxg(l2,m))*fxg(l2,m) , l2=1,n )
         write(lundiag,fmtdd) &
             'zav_del_chem_residu ', &
            ( zav_del_chem_residu(lxg(l2,m))*fxg(l2,m) , l2=1,n )
      end do ! m = 1, mxg


      tmpveca(p1st:num_chem) = zav_del_chem_aqchem(p1st:num_chem)
      write(lundiag,'(/a)') 'zav_del_chem_aqchem summary'

      write(lundiag,'(a,1p,10e12.4)') 'h2o2                 ', &
         tmpveca(p_h2o2)*1.0e3

      tmpb = tmpveca(p_so2) + tmpveca(p_sulf)
      write(lundiag,'(a,1p,10e12.4)') 'so2+h2so4, individ   ', &
         tmpb*1.0e3, &
         tmpveca(p_so2)*1.0e3, &
         tmpveca(p_sulf)*1.0e3
      itype = 1
      tmpa = 28.966/96.0 ; tmpb = 0.0
      do isize = 1, nsize_aer(itype)
         tmpb = tmpb + tmpveca(lptr_so4_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'so4_cw total, individ', &
         tmpb*tmpa, &
         ( tmpveca(lptr_so4_aer(isize,itype,cw_phase))*tmpa, isize=1,nsize_aer(itype) )

      write(lundiag,'(a,1p,10e12.4)') 'nh3                  ', &
         tmpveca(p_nh3)*1.0e3
      tmpa = 28.966/18.0 ; tmpb = 0.0
      do isize = 1, nsize_aer(itype)
         tmpb = tmpb + tmpveca(lptr_nh4_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'nh4_cw total, individ', &
         tmpb*tmpa, &
         ( tmpveca(lptr_nh4_aer(isize,itype,cw_phase))*tmpa, isize=1,nsize_aer(itype) )

      write(lundiag,'(a,1p,10e12.4)') 'hno3                 ', &
         tmpveca(p_hno3)*1.0e3
      tmpa = 28.966/62.0 ; tmpb = 0.0
      do isize = 1, nsize_aer(itype)
         tmpb = tmpb + tmpveca(lptr_no3_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'no3_cw total, individ', &
         tmpb*tmpa, &
         ( tmpveca(lptr_no3_aer(isize,itype,cw_phase))*tmpa, isize=1,nsize_aer(itype) )

      write(lundiag,'(a,1p,10e12.4)') 'hcl                  ', &
         tmpveca(p_hcl)*1.0e3
      tmpa = 28.966/35.5 ; tmpb = 0.0
      do isize = 1, nsize_aer(itype)
         tmpb = tmpb + tmpveca(lptr_cl_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'cl_cw  total, individ', &
         tmpb*tmpa, &
         ( tmpveca(lptr_cl_aer(isize,itype,cw_phase))*tmpa, isize=1,nsize_aer(itype) )

      tmpa = 1.0e-6 ; tmpb = 0.0
      do isize = 1, nsize_aer(itype)
         tmpb = tmpb + tmpveca(numptr_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'num_cw total, individ', &
         tmpb*tmpa, &
         ( tmpveca(numptr_aer(isize,itype,cw_phase))*tmpa, isize=1,nsize_aer(itype) )




      return
      end subroutine chem_cup_1d_diags_pt21


!-------------------------------------------------------------------------------
      subroutine chem_cup_1d_diags_pt41( &
         lundiag_inp, iflagaa, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         rhodz, rhodzsum, &
         chem_av_old, chem_av_new, &
         zav_del_chem_actvbb, zav_del_chem_aqchbb, &
         zav_del_chem_resdbb, zav_del_chem_respbb, &
         zav_del_chem_totlbb, zav_del_chem_wetrem, &
         chem_name ) 

      use module_configure, only:  &
         p_qc, &
         p_h2o2, p_hcl, p_hno3, p_nh3, p_so2, p_sulf

! arguments
      integer, intent(in) :: lundiag_inp, iflagaa
      integer, intent(in) :: kts, kte, ktep1, p1st, num_chem
      integer, intent(in) :: ktau, grid_id, i, j

      integer, intent(in) :: maxd_acomp, maxd_aphase, maxd_atype, maxd_asize
      integer, intent(in) :: &
         nphase_aer, ntype_aer, &
         ai_phase, cw_phase, &
         nsize_aer( maxd_atype ), &
         ncomp_aer( maxd_atype ), &
         massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), &
         numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
         lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_oin_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_cl_aer(maxd_asize, maxd_atype, maxd_aphase)

      real(r8), intent(in) :: rhodzsum

      real(r8), intent(in), dimension(kts:kte) :: rhodz

      real(r8), intent(in), dimension(kts:kte,1:num_chem) :: chem_av_old, chem_av_new

      real(r8), intent(in), dimension(1:num_chem) :: &
         zav_del_chem_actvbb, zav_del_chem_aqchbb, &
         zav_del_chem_resdbb, zav_del_chem_respbb, &
         zav_del_chem_totlbb, zav_del_chem_wetrem

      character(len=12), intent(in) :: chem_name(num_chem)

! local variables
      integer :: isize, itype
      integer :: l, lundiag
      integer :: m
      real(r8) :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg, tmph, tmpi, tmpj, tmpk
      real(r8) :: tmpveca(num_chem)


      lundiag = lundiag_inp
!     lundiag = 122

      if (iflagaa <= 1) then
         write(lundiag,'(/a,i10,5i5)') &
            'chem_cup_1d_diags_pt41 - ktau, id, i, j =', ktau, grid_id, i, j
      else
         write(lundiag,'(/a,i10,5i5)') &
            'chem_cup_1d_diags_pt42 - ktau, id, i, j =', ktau, grid_id, i, j
      end if

      do m = 1, 9

      if (iflagaa <= 1) then
         if (m > 6) cycle
      else
         if (m < 7) cycle
      end if

      if      (m == 1) then
         tmpveca(p1st:num_chem) = zav_del_chem_actvbb(p1st:num_chem)
         write(lundiag,'(/a)') 'inactive zav_del_chem_actvbb summary'
      else if (m == 2) then
         tmpveca(p1st:num_chem) = zav_del_chem_aqchbb(p1st:num_chem)
         write(lundiag,'(/a)') 'inactive zav_del_chem_aqchbb summary'
      else if (m == 3) then
         tmpveca(p1st:num_chem) = zav_del_chem_respbb(p1st:num_chem)
         write(lundiag,'(/a)') 'inactive zav_del_chem_respbb summary'
      else if (m == 4) then
         tmpveca(p1st:num_chem) = zav_del_chem_totlbb(p1st:num_chem)
         write(lundiag,'(/a)') 'inactive zav_del_chem_totlbb summary'
      else if (m == 5) then
         tmpveca(p1st:num_chem) = zav_del_chem_resdbb(p1st:num_chem)
         write(lundiag,'(/a)') 'inactive zav_del_chem_resdbb summary'
      else if (m == 6) then
         do l = p1st, num_chem
         tmpveca(l) = sum( rhodz(kts:kte)*chem_av_old(kts:kte,l) ) / rhodzsum
         end do
         write(lundiag,'(/a)') 'inactive zav_chem_av_old summary'
      else if (m == 7) then
         do l = p1st, num_chem
         tmpveca(l) = sum( rhodz(kts:kte)*chem_av_old(kts:kte,l) ) / rhodzsum
         end do
         write(lundiag,'(/a)') '***final zav_chem_av_old summary'
      else if (m == 8) then
         do l = p1st, num_chem
         tmpveca(l) = sum( rhodz(kts:kte)*chem_av_new(kts:kte,l) ) / rhodzsum
         end do
         write(lundiag,'(/a)') '***final zav_chem_av_new summary'
      else if (m == 9) then
         do l = p1st, num_chem
         tmpveca(l) = sum( rhodz(kts:kte)* &
            (chem_av_new(kts:kte,l)-chem_av_old(kts:kte,l)) ) / rhodzsum
         end do
         write(lundiag,'(/a)') '***final zav_chem_av_del summary'
      else
         cycle
      end if

      write(lundiag,'(a,1p,10e12.4)') 'h2o2                 ', &
         tmpveca(p_h2o2)*1.0e3

      tmpg = tmpveca(p_so2) + tmpveca(p_sulf)
      write(lundiag,'(a,1p,10e12.4)') 'so2+h2so4, individ   ', &
         tmpg*1.0e3, &
         tmpveca(p_so2)*1.0e3, &
         tmpveca(p_sulf)*1.0e3
      itype = 1
      tmpe = 28.966/96.0 ; tmpa = 0.0_r8 ; tmpc = 0.0_r8
      tmpi = 0.0_r8 ; tmpj = 0.0_r8
      do isize = 1, nsize_aer(itype)
         tmpa = tmpa + tmpveca(lptr_so4_aer(isize,itype,ai_phase))
         tmpc = tmpc + tmpveca(lptr_so4_aer(isize,itype,cw_phase))
         tmpi = tmpi + zav_del_chem_wetrem(lptr_so4_aer(isize,itype,ai_phase))
         tmpj = tmpj + zav_del_chem_wetrem(lptr_so4_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'so4_a  total, individ', &
         tmpa*tmpe, &
         ( tmpveca(lptr_so4_aer(isize,itype,ai_phase))*tmpe, isize=1,nsize_aer(itype) )
      write(lundiag,'(a,1p,10e12.4)') 'so4_cw total, individ', &
         tmpc*tmpe, &
         ( tmpveca(lptr_so4_aer(isize,itype,cw_phase))*tmpe, isize=1,nsize_aer(itype) )
      if (m >= 7) then
         tmph = tmpg*1.0e3 + (tmpa+tmpc)*tmpe
         write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         if (m <= 8) then
            write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         else
            tmpk = (tmpi+tmpj)*tmpe &
                 + (zav_del_chem_wetrem(p_so2)+zav_del_chem_wetrem(p_sulf))*1.0e3
            write(lundiag,'(a,1p,10e12.4)') '   all othr, wet, tot', (tmph-tmpk), tmpk, tmph
         end if
      end if

      write(lundiag,'(a,1p,10e12.4)') 'nh3                  ', &
         tmpveca(p_nh3)*1.0e3
      tmpe = 28.966/18.0 ; tmpa = 0.0_r8 ; tmpc = 0.0_r8
      tmpi = 0.0_r8 ; tmpj = 0.0_r8
      do isize = 1, nsize_aer(itype)
         tmpa = tmpa + tmpveca(lptr_nh4_aer(isize,itype,ai_phase))
         tmpc = tmpc + tmpveca(lptr_nh4_aer(isize,itype,cw_phase))
         tmpi = tmpi + zav_del_chem_wetrem(lptr_nh4_aer(isize,itype,ai_phase))
         tmpj = tmpj + zav_del_chem_wetrem(lptr_nh4_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'nh4_a  total, individ', &
         tmpa*tmpe, &
         ( tmpveca(lptr_nh4_aer(isize,itype,ai_phase))*tmpe, isize=1,nsize_aer(itype) )
      write(lundiag,'(a,1p,10e12.4)') 'nh4_cw total, individ', &
         tmpc*tmpe, &
         ( tmpveca(lptr_nh4_aer(isize,itype,cw_phase))*tmpe, isize=1,nsize_aer(itype) )
      if (m >= 7) then
         tmph = tmpveca(p_nh3)*1.0e3 + (tmpa+tmpc)*tmpe
         write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         if (m <= 8) then
            write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         else
            tmpk = (tmpi+tmpj)*tmpe + zav_del_chem_wetrem(p_nh3)*1.0e3
            write(lundiag,'(a,1p,10e12.4)') '   all othr, wet, tot', (tmph-tmpk), tmpk, tmph
         end if
      end if

      write(lundiag,'(a,1p,10e12.4)') 'hno3                 ', &
         tmpveca(p_hno3)*1.0e3
      tmpe = 28.966/62.0 ; tmpa = 0.0_r8 ; tmpc = 0.0_r8
      tmpi = 0.0_r8 ; tmpj = 0.0_r8
      do isize = 1, nsize_aer(itype)
         tmpa = tmpa + tmpveca(lptr_no3_aer(isize,itype,ai_phase))
         tmpc = tmpc + tmpveca(lptr_no3_aer(isize,itype,cw_phase))
         tmpi = tmpi + zav_del_chem_wetrem(lptr_no3_aer(isize,itype,ai_phase))
         tmpj = tmpj + zav_del_chem_wetrem(lptr_no3_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'no3_a  total, individ', &
         tmpa*tmpe, &
         ( tmpveca(lptr_no3_aer(isize,itype,ai_phase))*tmpe, isize=1,nsize_aer(itype) )
      write(lundiag,'(a,1p,10e12.4)') 'no3_cw total, individ', &
         tmpc*tmpe, &
         ( tmpveca(lptr_no3_aer(isize,itype,cw_phase))*tmpe, isize=1,nsize_aer(itype) )
      if (m >= 7) then
         tmph = tmpveca(p_hno3)*1.0e3 + (tmpa+tmpc)*tmpe
         write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         if (m <= 8) then
            write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         else
            tmpk = (tmpi+tmpj)*tmpe + zav_del_chem_wetrem(p_hno3)*1.0e3
            write(lundiag,'(a,1p,10e12.4)') '   all othr, wet, tot', (tmph-tmpk), tmpk, tmph
         end if
      end if

      write(lundiag,'(a,1p,10e12.4)') 'hcl                  ', &
         tmpveca(p_hcl)*1.0e3
      tmpe = 28.966/35.5 ; tmpa = 0.0_r8 ; tmpc = 0.0_r8
      tmpi = 0.0_r8 ; tmpj = 0.0_r8
      do isize = 1, nsize_aer(itype)
         tmpa = tmpa + tmpveca(lptr_cl_aer(isize,itype,ai_phase))
         tmpc = tmpc + tmpveca(lptr_cl_aer(isize,itype,cw_phase))
         tmpi = tmpi + zav_del_chem_wetrem(lptr_cl_aer(isize,itype,ai_phase))
         tmpj = tmpj + zav_del_chem_wetrem(lptr_cl_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'cl_a   total, individ', &
         tmpa*tmpe, &
         ( tmpveca(lptr_cl_aer(isize,itype,ai_phase))*tmpe, isize=1,nsize_aer(itype) )
      write(lundiag,'(a,1p,10e12.4)') 'cl_cw  total, individ', &
         tmpc*tmpe, &
         ( tmpveca(lptr_cl_aer(isize,itype,cw_phase))*tmpe, isize=1,nsize_aer(itype) )
      if (m >= 7) then
         tmph = tmpveca(p_hcl)*1.0e3 + (tmpa+tmpc)*tmpe
         if (m <= 8) then
            write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         else
            tmpk = (tmpi+tmpj)*tmpe + zav_del_chem_wetrem(p_hcl)*1.0e3
            write(lundiag,'(a,1p,10e12.4)') '   all othr, wet, tot', (tmph-tmpk), tmpk, tmph
         end if
      end if

      tmpe = 1.0e-6 ; tmpa = 0.0_r8 ; tmpc = 0.0_r8 
       tmpi = 0.0_r8 ; tmpj = 0.0_r8
      do isize = 1, nsize_aer(itype)
         tmpa = tmpa + tmpveca(numptr_aer(isize,itype,ai_phase))
         tmpc = tmpc + tmpveca(numptr_aer(isize,itype,cw_phase))
         tmpi = tmpi + zav_del_chem_wetrem(numptr_aer(isize,itype,ai_phase))
         tmpj = tmpj + zav_del_chem_wetrem(numptr_aer(isize,itype,cw_phase))
      end do
      write(lundiag,'(a,1p,10e12.4)') 'num_a  total, individ', &
         tmpa*tmpe, &
         ( tmpveca(numptr_aer(isize,itype,ai_phase))*tmpe, isize=1,nsize_aer(itype) )
      write(lundiag,'(a,1p,10e12.4)') 'num_cw total, individ', &
         tmpc*tmpe, &
         ( tmpveca(numptr_aer(isize,itype,cw_phase))*tmpe, isize=1,nsize_aer(itype) )
      if (m >= 7) then
         tmph = (tmpa+tmpc)*tmpe
         if (m <= 8) then
            write(lundiag,'(a,1p,10e12.4)') '   all total         ', tmph
         else
            tmpk = (tmpi+tmpj)*tmpe
            write(lundiag,'(a,1p,10e12.4)') '   all othr, wet, tot', (tmph-tmpk), tmpk, tmph
         end if
      end if

      end do ! m = 1, 6

      return
      end subroutine chem_cup_1d_diags_pt41


!-------------------------------------------------------------------------------
      subroutine chem_cup_1d_diags_pt71( &
         lundiag_inp, kcldbot, kcldtop, &
         kts, kte, ktep1, p1st, num_chem, &
         ktau, grid_id, i, j, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nphase_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, &
         massptr_aer, numptr_aer, &
         lptr_cl_aer, lptr_nh4_aer, lptr_no3_aer, lptr_oin_aer, lptr_so4_aer, &
         af_up, af_inact, &
         chem_av_new, chem_up, chem_inact_new, chem_incu, &
         chem_name ) 

      use module_configure, only:  &
         p_qc, &
         p_h2o2, p_hcl, p_hno3, p_nh3, p_so2, p_sulf

! arguments
      integer, intent(in) :: lundiag_inp, kcldbot, kcldtop
      integer, intent(in) :: kts, kte, ktep1, p1st, num_chem
      integer, intent(in) :: ktau, grid_id, i, j

      integer, intent(in) :: maxd_acomp, maxd_aphase, maxd_atype, maxd_asize
      integer, intent(in) :: &
         nphase_aer, ntype_aer, &
         ai_phase, cw_phase, &
         nsize_aer( maxd_atype ), &
         ncomp_aer( maxd_atype ), &
         massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), &
         numptr_aer( maxd_asize, maxd_atype, maxd_aphase ), &
         lptr_so4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_oin_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_no3_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_nh4_aer(maxd_asize, maxd_atype, maxd_aphase), &
         lptr_cl_aer(maxd_asize, maxd_atype, maxd_aphase)

      real(r8), intent(in), dimension(kts:kte) :: af_up, af_inact

      real(r8), intent(in), dimension(kts:kte,1:num_chem) :: &
         chem_av_new, chem_inact_new, chem_incu

      real(r8), intent(in), dimension(kts:ktep1,1:num_chem) :: chem_up

      character(len=12), intent(in) :: chem_name(num_chem)

! local variables
      integer :: k, n, n2
      integer :: lundiag
      real(r8) :: tmpa, tmpb, tmpc, tmpd, tmpe, tmpf, tmpg, tmph, tmpi, tmpj, tmpk
      real(r8) :: tmpveca(num_chem)


      lundiag = lundiag_inp
!     lundiag = 122

      write(lundiag,'(/a,i10,5i5)') &
         'chem_cup_1d_diags_pt71 - ktau, id, i, j =', ktau, grid_id, i, j

      n2 = (nsize_aer(1)+1)/2

      write(lundiag,'(/a,i2.2,a,i2.2,a)') &
         'af_up, af_inact;   so4_a01:', n2, &
         ' gridav/up/inact/incu;   so4_cw01:', n2, ' ...'
      tmpa = (28.966/96.0)
      do k = kcldtop+1, kts, -1
         tmpveca = 0.0
         do n = 1, (nsize_aer(1)+1)/2
            tmpveca(1) = tmpveca(1) + chem_av_new(   k,lptr_so4_aer(n,1,1))
            tmpveca(2) = tmpveca(2) + chem_up(       k,lptr_so4_aer(n,1,1))
            tmpveca(3) = tmpveca(3) + chem_inact_new(k,lptr_so4_aer(n,1,1))
            tmpveca(4) = tmpveca(4) + chem_incu(     k,lptr_so4_aer(n,1,1))
            tmpveca(5) = tmpveca(5) + chem_av_new(   k,lptr_so4_aer(n,1,2))
            tmpveca(6) = tmpveca(6) + chem_up(       k,lptr_so4_aer(n,1,2))
            tmpveca(7) = tmpveca(7) + chem_inact_new(k,lptr_so4_aer(n,1,2))
            tmpveca(8) = tmpveca(8) + chem_incu(     k,lptr_so4_aer(n,1,2))
         end do
         write(lundiag,'(i3,2x,2f8.5,1p,2(2x,4e10.2))') &
            k, af_up(k), af_inact(k), tmpveca(1:8)*tmpa
      end do

      write(lundiag,'(/a,i2.2,a,i2.2,a)') &
         'af_up, af_inact;   no3_a01:', n2, &
         ' gridav/up/inact/incu;   no3_cw01:', n2, ' ...'
      tmpa = (28.966/62.0)
      do k = kcldtop+1, kts, -1
         tmpveca = 0.0
         do n = 1, (nsize_aer(1)+1)/2
            tmpveca(1) = tmpveca(1) + chem_av_new(   k,lptr_no3_aer(n,1,1))
            tmpveca(2) = tmpveca(2) + chem_up(       k,lptr_no3_aer(n,1,1))
            tmpveca(3) = tmpveca(3) + chem_inact_new(k,lptr_no3_aer(n,1,1))
            tmpveca(4) = tmpveca(4) + chem_incu(     k,lptr_no3_aer(n,1,1))
            tmpveca(5) = tmpveca(5) + chem_av_new(   k,lptr_no3_aer(n,1,2))
            tmpveca(6) = tmpveca(6) + chem_up(       k,lptr_no3_aer(n,1,2))
            tmpveca(7) = tmpveca(7) + chem_inact_new(k,lptr_no3_aer(n,1,2))
            tmpveca(8) = tmpveca(8) + chem_incu(     k,lptr_no3_aer(n,1,2))
         end do
         write(lundiag,'(i3,2x,2f8.5,1p,2(2x,4e10.2))') &
            k, af_up(k), af_inact(k), tmpveca(1:8)*tmpa
      end do

      write(lundiag,'(/2a)') &
         'af_up, af_inact;   so2', &
         ' gridav/up/inact/incu;   hno3 ...'
      tmpa = 1.0e3
      do k = kcldtop+1, kts, -1
         tmpveca = 0.0
         do n = 1, (nsize_aer(1)+1)/2
            tmpveca(1) = tmpveca(1) + chem_av_new(   k,p_so2)
            tmpveca(2) = tmpveca(2) + chem_up(       k,p_so2)
            tmpveca(3) = tmpveca(3) + chem_inact_new(k,p_so2)
            tmpveca(4) = tmpveca(4) + chem_incu(     k,p_so2)
            tmpveca(5) = tmpveca(5) + chem_av_new(   k,p_hno3)
            tmpveca(6) = tmpveca(6) + chem_up(       k,p_hno3)
            tmpveca(7) = tmpveca(7) + chem_inact_new(k,p_hno3)
            tmpveca(8) = tmpveca(8) + chem_incu(     k,p_hno3)
         end do
         write(lundiag,'(i3,2x,2f8.5,1p,2(2x,4e10.2))') &
            k, af_up(k), af_inact(k), tmpveca(1:8)*tmpa
      end do

      return
      end subroutine chem_cup_1d_diags_pt71


!-------------------------------------------------------------------------------
      subroutine chem_cup_activate_up( &
         lunerr, lundiag, idiagaa, &
         kts, kte, p1st, num_chem, &
         ktau, grid_id, i, j, k, iflagaa, &
         pcen, tcen, rhocen, qcw, &
         rhodz, af_up, wact, &
         tmp_mf_up, tmp_mfxchem_up, dchemdt_up_activa, &
         maxd_acomp, maxd_aphase, maxd_atype, maxd_asize, &
         ncomp_aer, nsize_aer, ntype_aer, &
         ai_phase, cw_phase, msectional, &
         massptr_aer, numptr_aer, &
         dlo_sect, dhi_sect, dens_aer, hygro_aer, sigmag_aer )
!
! compute changes to updraft mixing ratios from aerosol activation
!    as updraft air moves through layer k
!
! when iflagaa = 1 or 2, do traditional cloud-base activation
! when iflagaa = 10,     do secondary activation
!
      use module_mixactivate, only:  activate

! arguments
      integer :: lunerr, lundiag, idiagaa
      integer :: kts, kte, p1st, num_chem
      integer :: ktau, grid_id, i, j, k, iflagaa

      integer :: maxd_acomp, maxd_aphase, maxd_atype, maxd_asize
      integer :: &
         ntype_aer, &
         ai_phase, cw_phase, msectional, &
         nsize_aer( maxd_atype ), &
         ncomp_aer( maxd_atype ), &
         massptr_aer( maxd_acomp, maxd_asize, maxd_atype, maxd_aphase ), &
         numptr_aer( maxd_asize, maxd_atype, maxd_aphase )
      real(r4) :: &
         dlo_sect( maxd_asize, maxd_atype ), &
         dhi_sect( maxd_asize, maxd_atype ), &
         dens_aer( maxd_acomp, maxd_atype ), &
         hygro_aer( maxd_acomp, maxd_atype ), &
         sigmag_aer(maxd_asize, maxd_atype)

      real(r8) :: pcen, tcen, rhocen, qcw
      real(r8) :: rhodz, af_up, wact
      real(r8) :: tmp_mf_up
      real(r8) :: tmp_mfxchem_up(1:num_chem)
      real(r8) :: dchemdt_up_activa(kts:kte,1:num_chem)

! local variables
      integer :: isize, itype
      integer :: la, lc, l2
      real(r8) :: tmpb, tmpc
      real(r8) :: tmpvol
      real(r8) :: tmp_chem_up(1:num_chem)
      real(r8), dimension( 1:maxd_asize, 1:maxd_atype ) :: &
                     fn, fm, hygro, numbr, volum

! single precision variable used for calls to other wrf-chem routines
      real(r4) :: flux_fullact_sp
      real(r4) :: rhocen_sp, tcen_sp, wact_sp
      real(r4) :: smax_prescribed_sp
      real(r4), dimension( 1:maxd_asize, 1:maxd_atype ) :: &
                     fn_sp, fs_sp, fm_sp, fluxn_sp, fluxs_sp, fluxm_sp, &
                     hygro_sp, numbr_sp, volum_sp

!     if ( do_activa )
! get current mixing ratios in updraft
      tmp_chem_up(p1st:num_chem) = tmp_mfxchem_up(p1st:num_chem)/tmp_mf_up

! calculate activation fractions using the abdul-razak and ghan 
!    parameterization (subr. activate)
! for this need aerosol number, volume, and weighted hygroscopicity for
!    each aerosol type and size
      hygro(:,:) = 0.0_r8
      numbr(:,:) = 0.0_r8
      volum(:,:) = 0.0_r8

      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         la = numptr_aer(isize,itype,ai_phase)
         numbr(isize,itype) = numbr(isize,itype) + max( 0.0_r8, tmp_chem_up(la) )
         do l2 = 1, ncomp_aer(itype)
            la = massptr_aer(l2,isize,itype,ai_phase)
            tmpvol = max( 0.0_r8, tmp_chem_up(la) ) / dens_aer(l2,itype)
            volum(isize,itype) = volum(isize,itype) + tmpvol
            hygro(isize,itype) = hygro(isize,itype) + tmpvol*hygro_aer(l2,itype)
         end do
      end do ! isize
      end do ! itype

      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         hygro(isize,itype) = hygro(isize,itype) / max( 1.0e-35_r8, volum(isize,itype) )
         ! convert numbr from (#/kg) to (#/m3)
         numbr(isize,itype) = numbr(isize,itype)*rhocen
         ! convert volum to (m3/m3) -- need 1e-12 factor because 
         !    (rhocen*chem)/dens_aer = [(ugaero/m3air)/(gaero/cm3aero)]
         volum(isize,itype) = volum(isize,itype)*rhocen*1.0e-12_r8

         ! avoid zero numbr or volum
         tmpb = 1.0e-32_r8  ! 1e-32 m3/m3 volume ~= 1e-20 ug/m3 mass
         tmpc = (dlo_sect(isize,itype) + dhi_sect(isize,itype))*0.5e-2_r8  ! dcen in m
         tmpc = 1.91_r8 * tmpb / (tmpc**3)  ! number in #/m3 when volume = tmpb
         if ((volum(isize,itype) < tmpb) .or. (numbr(isize,itype) < tmpc)) then
            volum(isize,itype) = tmpb
            numbr(isize,itype) = tmpc
            hygro(isize,itype) = 0.3_r8
         end if
      end do ! isize
      end do ! itype

!     subr. activate( wbar, sigw, wdiab, wminf, wmaxf, tair, rhoair,  &
!                     msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
!                     na, volc, dlo_sect, dhi_sect, sigman, hygro, &
!                     fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
!                     grid_id, ktau, ii, jj, kk )

!     call activate( wact, 0.0, 0.0, 0.0, 1.0, tcen, rhocen,  &
!                     msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
!                     numbr, volum, dlo_sect, dhi_sect, sigmag_aer, hygro, &
!                     fn, fs, fm, fluxn, fluxs, fluxm, flux_fullact, &
!                     grid_id, ktau, i, j, k )

      wact_sp = wact
      tcen_sp = tcen
      rhocen_sp = rhocen
      numbr_sp = numbr
      volum_sp = volum
      hygro_sp = hygro

      if (iflagaa < 10) then
         call activate( &
                      wact_sp, 0.0, 0.0, 0.0, 1.0, tcen_sp, rhocen_sp,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      numbr_sp, volum_sp, dlo_sect, dhi_sect, sigmag_aer, hygro_sp, &
                      fn_sp, fs_sp, fm_sp, fluxn_sp, fluxs_sp, fluxm_sp, flux_fullact_sp, &
                      grid_id, ktau, i, j, k )
      else
         if (qcw < qcw_inup_smallaa) return
         smax_prescribed_sp = 0.001  ! 0.1% supersaturation
         call activate( &
                      wact_sp, 0.0, 0.0, 0.0, 1.0, tcen_sp, rhocen_sp,  &
                      msectional, maxd_atype, ntype_aer, maxd_asize, nsize_aer,    &
                      numbr_sp, volum_sp, dlo_sect, dhi_sect, sigmag_aer, hygro_sp, &
                      fn_sp, fs_sp, fm_sp, fluxn_sp, fluxs_sp, fluxm_sp, flux_fullact_sp, &
                      grid_id, ktau, i, j, k, smax_prescribed_sp )
      end if

      fn = fn_sp
      fm = fm_sp

      if (idiagaa > 0) then
         write(lundiag,'(/a,i10,5i5,1p,e10.2)/a') &
            'chem_cup_activate_up - ktau, id, i, j, k, iflagaa, wact', &
            ktau, grid_id, i, j, k, iflagaa, wact, &
            'chem_cup_activate_up - fn then fm'
         write(lundiag,'(8f11.7)') &
            ((fn(isize,itype), isize=1,nsize_aer(itype)), itype=1,ntype_aer)
         write(lundiag,'(8f11.7)') &
            ((fm(isize,itype), isize=1,nsize_aer(itype)), itype=1,ntype_aer)
!        do itype = 1, ntype_aer
!        do isize = 1, nsize_aer(itype)
!           tmpb = volum(isize,itype) / max(numbr(isize,itype),1.0e-10)
!           tmpb = ( tmpb*6.0/3.14159 )**(1.0/3.0)
!           write(lundiag,'(a,2i5,1p,4e10.2)') 'itype, isize, diams', itype, isize, &
!              dlo_sect(isize,itype), tmpb, dhi_sect(isize,itype)
!        end do ! isize
!        end do ! itype
      end if

! apply activation fractions
      do itype = 1, ntype_aer
      do isize = 1, nsize_aer(itype)
         do l2 = 0, ncomp_aer(itype)
            if (l2 == 0) then
               la = numptr_aer(isize,itype,ai_phase)
               lc = numptr_aer(isize,itype,cw_phase)
            else
               la = massptr_aer(l2,isize,itype,ai_phase)
               lc = massptr_aer(l2,isize,itype,cw_phase)
            end if
            if ((la < p1st) .or. (la > num_chem)) cycle
            if ((lc < p1st) .or. (lc > num_chem)) cycle

            if (l2 == 0) then
               tmpb = tmp_chem_up(la)*fn(isize,itype)
            else
               tmpb = tmp_chem_up(la)*fm(isize,itype)
            end if
            ! update chem_up
            tmp_chem_up(la) = tmp_chem_up(la) - tmpb
            tmp_chem_up(lc) = tmp_chem_up(lc) + tmpb

            ! update mf_up*chem_up
            tmp_mfxchem_up(la) = tmp_chem_up(la)*tmp_mf_up
            tmp_mfxchem_up(lc) = tmp_chem_up(lc)*tmp_mf_up

            ! increment dchemdt_up_activa
            tmpc = tmpb*tmp_mf_up/(rhodz*af_up)
            dchemdt_up_activa(k,la) = dchemdt_up_activa(k,la) - tmpc
            dchemdt_up_activa(k,lc) = dchemdt_up_activa(k,lc) + tmpc

         end do ! l2
      end do ! isize
      end do ! itype

      return
      end subroutine chem_cup_activate_up


!-------------------------------------------------------------------------------
      subroutine chem_cup_aqchem( &
         config_flags, aer_mech_id, &
         lunerr, lundiag, idiagaa, &
         kts, kte, p1st, num_chem, &
         p_qc, num_moist, &
         ktau, grid_id, i, j, &
         iflagaa, ido_aqchem, &
         dt_aqchem, &
         pcen, tcen, rhocen, rhodz, qcw, ph_no2, &
         af_up, tmp_gas_aqfrac_up, tmp_mf_up, tmp_mfxchem_up, &
         dchemdt_up_aqchem, chem_inact )
!
! compute changes to updraft mixing ratios from cloud chemistry
!    as updraft air moves through layer k
!
! when iflagaa >= kts, do cloud chemistry 
!    in updraft for the layer given by k=iflagaa
! when iflagaa < kts, do cloud chemistry 
!    in inactive cloud for the layers having ido_aqchem(k)>0
!
      use module_configure, only:  grid_config_rec_type
      use module_mosaic_cloudchem, only:  mosaic_cloudchem_driver

! arguments
      type(grid_config_rec_type), intent(in) :: config_flags

      integer :: aer_mech_id
      integer :: lunerr, lundiag, idiagaa
      integer :: kts, kte, p1st, num_chem
      integer :: p_qc, num_moist
      integer :: ktau, grid_id, i, j
      integer :: iflagaa, ido_aqchem(kts:kte)

      real(r8) :: dt_aqchem
      real(r8), dimension( kts:kte ) :: pcen, tcen, rhocen, rhodz, qcw, ph_no2
      real(r8) :: af_up
      real(r8) :: tmp_gas_aqfrac_up(1:num_chem)
      real(r8) :: tmp_mf_up
      real(r8) :: tmp_mfxchem_up(1:num_chem)
      real(r8) :: dchemdt_up_aqchem(kts:kte,1:num_chem)
      real(r8) :: chem_inact(kts:kte,1:num_chem)

! local variables
      integer :: k, k2
      integer :: l
      real(r8) :: tmpb
      real(r8) :: tmp_chem_up(1:num_chem), tmp_chem_up_old(1:num_chem)

! single precision variable used for calls to other wrf-chem routines
      real(r4) :: dt_aqchem_sp
      real(r4), dimension( 1:1, kts:kte, 1:1 ) :: &
         tmp_alt_sp, tmp_cldfra_sp, tmp_p_sp, tmp_ph_no2_sp, tmp_rho_sp, tmp_t_sp
      real(r4), dimension( 1:1, kts:kte, 1:1, 1:num_chem ) :: &
         tmp_chem_sp, tmp_chem_old_sp, tmp_gas_aqfrac_sp
      real(r4), dimension( 1:1, kts:kte, 1:1, 1:num_moist ) :: &
         tmp_moist_sp


      if (aer_mech_id /= 3) return

      tmp_gas_aqfrac_up = 0.0

      tmp_chem_old_sp = 0.0
      tmp_chem_sp = 0.0
      tmp_cldfra_sp = 0.0
      tmp_moist_sp = 0.0
      tmp_ph_no2_sp = 0.0
      tmp_gas_aqfrac_sp = 0.0
      dt_aqchem_sp = dt_aqchem

      if (iflagaa >= kts) then
         ! doing aqchem for 1 layer of updraft
         k = iflagaa
         ! first get current mixing ratios in updraft
         tmp_chem_up_old(p1st:num_chem) = tmp_mfxchem_up(p1st:num_chem)/tmp_mf_up

         tmp_chem_old_sp(1,k,1,p1st:num_chem) = tmp_chem_up_old(p1st:num_chem)
         tmp_chem_sp(1,k,1,p1st:num_chem) = tmp_chem_old_sp(1,k,1,p1st:num_chem)

         tmp_cldfra_sp(1,k,1) = 1.0
         tmp_moist_sp(1,k,1,p_qc) = qcw(k)
         tmp_ph_no2_sp(1,k,1) = ph_no2(k)

      else
         ! doing aqchem for multiple levels of inactive cloud
         do k = kts, kte
            if (ido_aqchem(k) <= 0) cycle
            tmp_chem_old_sp(1,k,1,p1st:num_chem) = chem_inact(k,p1st:num_chem)
            tmp_chem_sp(1,k,1,p1st:num_chem) = tmp_chem_old_sp(1,k,1,p1st:num_chem)

            tmp_cldfra_sp(1,k,1) = 1.0
            tmp_moist_sp(1,k,1,p_qc) = qcw(k)
            tmp_ph_no2_sp(1,k,1) = ph_no2(k)
         end do
      end if

      do k = kts, kte
         tmp_p_sp(1,k,1) = pcen(k)
         tmp_t_sp(1,k,1) = tcen(k)
         tmp_rho_sp(1,k,1) = rhocen(k)
         tmp_alt_sp(1,k,1) = 1.0/rhocen(k)
      end do

      if (aer_mech_id == 3) then
!        subr. mosaic_cloudchem_driver(   &
!           id, ktau, ktauc, dtstepc, config_flags,   &
!           p_phy, t_phy, rho_phy, alt,   &
!           cldfra, ph_no2,   &
!           moist, chem,   &
!           gas_aqfrac, numgas_aqfrac,   &
!           ids,ide, jds,jde, kds,kde,   &
!           ims,ime, jms,jme, kms,kme,   &
!           its,ite, jts,jte, kts,kte )

         call mosaic_cloudchem_driver(   &
            grid_id, ktau, ktau, dt_aqchem_sp, config_flags,   &
            tmp_p_sp, tmp_t_sp, tmp_rho_sp, tmp_alt_sp,   &
            tmp_cldfra_sp, tmp_ph_no2_sp,   &
            tmp_moist_sp, tmp_chem_sp,   &
            tmp_gas_aqfrac_sp, num_chem,   &
            1,1,  1,1,  kts,kte,  &
            1,1,  1,1,  kts,kte,  &
            1,1,  1,1,  kts,kte )

!     else
!        call cloudchem routine for other aerosol treatments
      end if


      if (iflagaa >= kts) then
         k = iflagaa
         do l = p1st, num_chem
            tmp_gas_aqfrac_up(l) = tmp_gas_aqfrac_sp(1,k,1,l)

            if (tmp_chem_sp(1,k,1,l) == tmp_chem_old_sp(1,k,1,l)) cycle

            tmp_chem_up(l) = tmp_chem_sp(1,k,1,l) 
            tmpb = tmp_chem_up(l) - tmp_chem_up_old(l)

            ! update mf_up*chem_up
            tmp_mfxchem_up(l) = tmp_chem_up(l)*tmp_mf_up
            ! increment dchemdt_up_aqchem
            dchemdt_up_aqchem(k,l) = dchemdt_up_aqchem(k,l) &
                                   + tmpb*tmp_mf_up/(rhodz(k)*af_up)
         end do

      else
         do k = kts, kte
            if (ido_aqchem(k) <= 0) cycle
            do l = p1st, num_chem
               if (tmp_chem_sp(1,k,1,l) == tmp_chem_old_sp(1,k,1,l)) cycle
               chem_inact(k,l) = tmp_chem_sp(1,k,1,l)
            end do
         end do
      end if

      return
      end subroutine chem_cup_aqchem


!-------------------------------------------------------------------------------
      subroutine chem_cup_check_adjust_inputs( &
         lunerr, lundiag, idiagaa, &
         kts, kte, ktep1, &
         ktau, grid_id, i, j, &
         ishall, do_dndraft, &
         kcldbot, kcldtop, kcldbotliq, &
         kupdrbot, kupdrtop, kdndrbot, kdndrtop, &
         iok, &
         tau_active, tau_inactive, &
         dz, zcen, zbnd, pcen, tcen, rhocen, &
         af_lscld, af_cucld, af_up, af_dn, &
         qcw_incu, qci_incu, &
         qcw_inup, qci_inup, &
         mf_up, mf_up_ent, mf_up_det, &
         mf_dn, mf_dn_ent, mf_dn_det )
!
! apply "reality checks" to inputs
! to avoid problems in the chem_cup_1d algorithm
!

! arguments
      integer, intent(in) :: lunerr, lundiag, idiagaa
      integer, intent(in) :: kts, kte, ktep1
      integer, intent(in) :: ktau, grid_id, i, j
      integer, intent(in) :: ishall
      integer, intent(inout) :: kcldbot, kcldtop, kcldbotliq
      integer, intent(inout) :: kupdrbot, kupdrtop, kdndrbot, kdndrtop
      integer, intent(inout) :: iok

      logical, intent(in) :: do_dndraft

      real(r8), intent(in) :: tau_active, tau_inactive

      real(r8), intent(in), dimension(kts:kte) :: &
         dz, zcen, pcen, tcen, rhocen, af_lscld

      real(r8), intent(inout), dimension(kts:kte) :: &
         af_cucld, af_up, af_dn, &
         qcw_incu, qci_incu, qcw_inup, qci_inup, &
         mf_up_ent, mf_up_det, mf_dn_ent, mf_dn_det

      real(r8), intent(in), dimension(kts:ktep1) :: &
         zbnd

      real(r8), intent(inout), dimension(kts:ktep1) :: &
         mf_up, mf_dn 

! local variables
      integer :: k, ktmpa, ktmpb, ktmpc, ktmpd

      real(r8) :: tmpa


      iok = -1

! check for tau_active too small
      if (tau_active < tau_active_smallaa) then
         write(lunerr,'(2a,i10,3i5)') &
            'chem_cup_check_adjust_inputs - ', &
            'tau_active < tau_active_smallaa', ktau, grid_id, i, j
         return
      end if


! identify bottom and top of the updraft
      mf_up(kts) = 0.0_r8
      mf_up(ktep1) = 0.0_r8
      kupdrbot = kts-1
      kupdrtop = kts-1
      tmpa = 1.0e20
      do k = kts, kte
         if (mf_up(k) >= aw_up_smallaa*rhocen(k)) then
            if (kupdrbot < kts) kupdrbot = k-1
            kupdrtop = k
            tmpa = min( tmpa, mf_up(k) )
         else
            mf_up(k) = 0.0_r8
         end if
      end do
      if (kupdrbot < kts) then
         write(lunerr,'(2a,i10,3i5)') &
            'chem_cup_check_adjust_inputs - ', &
            'no mf_up > aw_up_smallaa*rho', ktau, grid_id, i, j
         return
      end if

! force mf_up to have "reasonable" positive values throughout the updraft layers
      do k = kupdrbot+1, kupdrtop
         mf_up(k) = max( mf_up(k), tmpa )
      end do


      if ( do_dndraft ) then
! identify bottom and top of the dndraft
      mf_dn(kts) = 0.0_r8
      mf_dn(ktep1) = 0.0_r8
      kdndrbot = kts-1
      kdndrtop = kts-1
      tmpa = -1.0e20
      do k = kts, kte
         if (k > kupdrtop) then
            mf_dn(k) = 0.0_r8 ! do not allow downdraft to extend higher than updraft
         else if (mf_dn(k) <= -aw_up_smallaa*rhocen(k)) then
            if (kdndrbot < kts) kdndrbot = k-1
            kdndrtop = k
            tmpa = max( tmpa, mf_dn(k) )
         else
            mf_dn(k) = 0.0_r8
         end if
      end do
      if (kdndrbot < kts) then
         write(lunerr,'(2a,i10,3i5)') &
            'chem_cup_check_adjust_inputs - ', &
            'no mf_dn > aw_dn_smallaa*rho', ktau, grid_id, i, j
         return
      end if

! force mf_dn to have "reasonable" negative values throughout the dndraft layers
      do k = kdndrbot+1, kdndrtop
         mf_dn(k) = min( mf_dn(k), tmpa )
      end do
      end if ! ( do_dndraft )


! identify layers with cloudwater and cloud-ice
      ktmpa = kts-1 ; ktmpb = kts-1 ; ktmpc = kts-1 ; ktmpd = kts-1
      do k = kts, kte
         if ((k < kupdrbot) .or. (k > kupdrtop)) then
            qcw_inup(k) = 0.0_r8
            qci_inup(k) = 0.0_r8
            qcw_incu(k) = 0.0_r8
            qci_incu(k) = 0.0_r8
         else
            if (qcw_inup(k) < qcw_inup_smallaa) then
               qcw_inup(k) = 0.0_r8
            else
               if (ktmpa < kts) ktmpa = k
               ktmpb = k
            end if
            if (qci_inup(k) < qci_inup_smallaa) then
               qci_inup(k) = 0.0_r8
            else
               if (ktmpc < kts) ktmpc = k
               ktmpd = k
            end if

            if (qcw_incu(k) < qcw_incu_smallaa) then
               qcw_incu(k) = 0.0_r8
            end if
            if (qci_incu(k) < qci_incu_smallaa) then
               qci_incu(k) = 0.0_r8
            end if
         end if
      end do
      if ((ktmpa < kts) .and. (ktmpc < kts)) then
         write(lunerr,'(2a,i10,3i5)') &
            'chem_cup_check_adjust_inputs - ', &
            'no qcw/qci_inup > qcw/i_inup_smallaa', ktau, grid_id, i, j
         return
      end if
! at this point,  kupdrbot <= ktmpa <= ktmpb <= kupdrtop
!    and/or       kupdrbot <= ktmpc <= ktmpd <= kupdrtop

      kcldbotliq = ktmpa
      if (ktmpa < kts ) then
         ! in this case, ktmpc >= kts
         kcldbot = ktmpc
         kcldtop = ktmpd
         kcldbotliq = kts-1
      else if (ktmpc < kts ) then
         ! in this case, ktmpa >= kts
         kcldbot = ktmpa
         kcldtop = ktmpb
      else
         kcldbot = min( ktmpa, ktmpc )
         kcldtop = max( ktmpb, ktmpd )
      end if

! check/adjust af_cucld and af_up
!
! *** this should be done better
!     check the ecpp code, which does something like
!        af_up = max( af_up, mf_up/rho*wup_maxaa 
      do k = kts, kte
         if ((k < kupdrbot) .or. (k > kupdrtop)) then
            af_up(k) = 0.0_r8
         else
            af_up(k) = max( af_up(k), af_up_smallaa )
            af_up(k) = min( af_up(k), af_up_maxaa )
         end if
      end do

      do k = kts, kte
         if ((k < kcldbot) .or. (k > kcldtop)) then
            af_cucld(k) = 0.0_r8
         else
            af_cucld(k) = max( af_cucld(k), af_up(k), af_cucld_smallaa )
            af_cucld(k) = min( af_cucld(k), af_cucld_maxaa )
         end if
      end do

! calculate mf_up_det and adjust mf_up_ent if needed
      do k = kts, kte
         if ((k < kupdrbot) .or. (k > kupdrtop)) then
            mf_up_ent(k) = 0.0_r8
            mf_up_det(k) = 0.0_r8
         else
            tmpa = mf_up(k+1) - mf_up(k)
            mf_up_det(k) = mf_up_ent(k) - tmpa
            if (mf_up_det(k) < 0.0_r8) then
               mf_up_ent(k) = tmpa
               mf_up_det(k) = 0.0_r8
            end if
        end if
      end do

! calculate mf_dn_det and adjust mf_dn_ent if needed
      do k = kts, kte
         if ((k < kdndrbot) .or. (k > kdndrtop)) then
            mf_dn_ent(k) = 0.0_r8
            mf_dn_det(k) = 0.0_r8
         else
            tmpa = mf_dn(k+1) - mf_dn(k)
            mf_dn_det(k) = mf_dn_ent(k) - tmpa
            if (mf_dn_det(k) < 0.0_r8) then
               mf_dn_ent(k) = tmpa
               mf_dn_det(k) = 0.0_r8
            end if
        end if
      end do

      iok = 0
      return
      end subroutine chem_cup_check_adjust_inputs


!-----------------------------------------------------------------
      end module module_chem_cup
