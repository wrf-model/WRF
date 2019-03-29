   SUBROUTINE dfi_accumulate( grid )

      USE module_domain, ONLY : domain
!      USE module_configure
      USE module_driver_constants
      USE module_machine
!      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      REAL hn
      CHARACTER*80 mess

      !  Input data.

      TYPE(domain) , POINTER          :: grid

      IF ( grid%dfi_opt .EQ. DFI_NODFI .OR. grid%dfi_stage .EQ. DFI_FST ) RETURN

#if (EM_CORE == 1)


      hn = grid%hcoeff(grid%itimestep+1)

      ! accumulate dynamic variables
       grid%dfi_mu(:,:)    = grid%dfi_mu(:,:)    + grid%MU_2(:,:)    * hn
       grid%dfi_u(:,:,:)   = grid%dfi_u(:,:,:)   + grid%u_2(:,:,:)   * hn
       grid%dfi_v(:,:,:)   = grid%dfi_v(:,:,:)   + grid%v_2(:,:,:)   * hn
       grid%dfi_w(:,:,:)   = grid%dfi_w(:,:,:)   + grid%w_2(:,:,:)   * hn
       grid%dfi_ww(:,:,:)  = grid%dfi_ww(:,:,:)  + grid%ww(:,:,:)    * hn
       grid%dfi_t(:,:,:)   = grid%dfi_t(:,:,:)   + grid%t_2(:,:,:)   * hn
       grid%dfi_phb(:,:,:) = grid%dfi_phb(:,:,:) + grid%phb(:,:,:)   * hn
       grid%dfi_ph0(:,:,:) = grid%dfi_ph0(:,:,:) + grid%ph0(:,:,:)   * hn
       grid%dfi_php(:,:,:) = grid%dfi_php(:,:,:) + grid%php(:,:,:)   * hn
       grid%dfi_p(:,:,:)   = grid%dfi_p(:,:,:)   + grid%p(:,:,:)     * hn
       grid%dfi_ph(:,:,:)  = grid%dfi_ph(:,:,:)  + grid%ph_2(:,:,:)  * hn
       grid%dfi_tke(:,:,:) = grid%dfi_tke(:,:,:) + grid%tke_2(:,:,:) * hn
       grid%dfi_al(:,:,:)  = grid%dfi_al(:,:,:)  + grid%al(:,:,:)    * hn
       grid%dfi_alt(:,:,:) = grid%dfi_alt(:,:,:) + grid%alt(:,:,:)   * hn
       grid%dfi_pb(:,:,:)  = grid%dfi_pb(:,:,:)  + grid%pb(:,:,:)    * hn
      ! dfi_savehydmeteors is a namelist parameter, default =0  which means hydrometeor
      ! and scalar fields will be spinning up in DFI; if dfi_savehydmeteors=1 then
      ! hydrometeor fields will stay unchanged in DFI, but water vapor mixing ratio
      ! QV will be modified.
     IF ( grid%dfi_savehydmeteors .EQ. 0 ) then  
       grid%dfi_moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  + grid%moist(:,:,:,:)  * hn
       grid%dfi_scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) + grid%scalar(:,:,:,:) * hn
       IF ( grid%mp_physics_dfi .NE. NUWRF4ICESCHEME_DFI ) THEN
          grid%dfi_re_cloud(:,:,:)        = grid%dfi_re_cloud(:,:,:)        + grid%re_cloud(:,:,:)        * hn
          grid%dfi_re_ice(:,:,:)          = grid%dfi_re_ice(:,:,:)          + grid%re_ice(:,:,:)          * hn
          grid%dfi_re_snow(:,:,:)         = grid%dfi_re_snow(:,:,:)         + grid%re_snow(:,:,:)         * hn
#if (EM_CORE == 1)
       ELSE IF ( grid%mp_physics_dfi .EQ. NUWRF4ICESCHEME_DFI ) THEN
          grid%dfi_re_cloud_gsfc(:,:,:)   = grid%dfi_re_cloud_gsfc(:,:,:)   + grid%re_cloud_gsfc(:,:,:)   * hn
          grid%dfi_re_rain_gsfc(:,:,:)    = grid%dfi_re_rain_gsfc(:,:,:)    + grid%re_rain_gsfc(:,:,:)    * hn
          grid%dfi_re_ice_gsfc(:,:,:)     = grid%dfi_re_ice_gsfc(:,:,:)     + grid%re_ice_gsfc(:,:,:)     * hn
          grid%dfi_re_snow_gsfc(:,:,:)    = grid%dfi_re_snow_gsfc(:,:,:)    + grid%re_snow_gsfc(:,:,:)    * hn
          grid%dfi_re_graupel_gsfc(:,:,:) = grid%dfi_re_graupel_gsfc(:,:,:) + grid%re_graupel_gsfc(:,:,:) * hn
          grid%dfi_re_hail_gsfc(:,:,:)    = grid%dfi_re_hail_gsfc(:,:,:)    + grid%re_hail_gsfc(:,:,:)    * hn
#endif
       ENDIF
     ELSE
       grid%dfi_moist(:,:,:,P_QV) = grid%dfi_moist(:,:,:,P_QV) + grid%moist(:,:,:,P_QV) * hn
     ENDIF
!      IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
!       grid%dfi_QVG(:,:)   = grid%dfi_QVG(:,:)    + grid%QVG(:,:)    * hn
!      ENDIF

      ! accumulate DFI coefficient
      grid%hcoeff_tot = grid%hcoeff_tot + hn
#endif

#if (NMM_CORE == 1)
      hn = grid%hcoeff(grid%ntsd+1)
      grid%dfi_pd(:,:)    = grid%dfi_pd(:,:)    + grid%pd(:,:)    * hn
      grid%dfi_pint(:,:,:)   = grid%dfi_pint(:,:,:)   + grid%pint(:,:,:)   * hn
      grid%dfi_dwdt(:,:,:)   = grid%dfi_dwdt(:,:,:)   + grid%dwdt(:,:,:)   * hn
      grid%dfi_t(:,:,:)   = grid%dfi_t(:,:,:)   + grid%t(:,:,:)   * hn
      grid%dfi_q(:,:,:)   = grid%dfi_q(:,:,:)   + grid%q(:,:,:)   * hn
      grid%dfi_q2(:,:,:)  = grid%dfi_q2(:,:,:)  + grid%q2(:,:,:)  * hn
      grid%dfi_cwm(:,:,:) = grid%dfi_cwm(:,:,:) + grid%cwm(:,:,:) * hn
      grid%dfi_u(:,:,:)   = grid%dfi_u(:,:,:)   + grid%u(:,:,:)   * hn
      grid%dfi_v(:,:,:)   = grid%dfi_v(:,:,:)   + grid%v(:,:,:)   * hn
      grid%dfi_moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  + grid%moist(:,:,:,:)  * hn
      grid%dfi_scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) + grid%scalar(:,:,:,:) * hn
      ! accumulate DFI coefficient
      grid%hcoeff_tot = grid%hcoeff_tot + hn
      write(mess,*) 'grid%hcoeff_tot: ', grid%hcoeff_tot
      call wrf_message(mess)
#endif

   END SUBROUTINE dfi_accumulate

   SUBROUTINE dfi_bck_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc
      CHARACTER*80 mess

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain

#if (EM_CORE == 1)
         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi
#endif

      END INTERFACE

      grid%dfi_stage = DFI_BCK

#if (EM_CORE == 1)
      if(grid%cycling) then
!         print *,'   Rebalancing is on '
         CALL rebalance_driver_dfi ( grid )
      endif
#endif

      ! Negate time step
      IF ( grid%time_step_dfi .gt. 0 ) THEN
        CALL nl_set_time_step ( 1, -grid%time_step_dfi )
      ELSE
        CALL nl_set_time_step ( 1, -grid%time_step )
        CALL nl_set_time_step_fract_num ( 1, -grid%time_step_fract_num )
      ENDIF

      CALL Setup_Timekeeping (grid)

      !tgs 7apr11 - need to call start_domain here to reset bc initialization for negative dt
      CALL start_domain ( grid , .TRUE. )
      !tgs 7apr11 - save arrays should be done after start_domain to get correct grid%p field
      !             used in computation of initial RH.
      CALL dfi_save_arrays ( grid )

      ! set physics options to zero
      CALL nl_set_mp_physics( grid%id, 0 )
      CALL nl_set_ra_lw_physics( grid%id, 0 )
      CALL nl_set_ra_sw_physics( grid%id, 0 )
      CALL nl_set_sf_surface_physics( grid%id, 0 )
      CALL nl_set_sf_sfclay_physics( grid%id, 0 )
      CALL nl_set_sf_urban_physics( grid%id, 0 )
      CALL nl_set_bl_pbl_physics( grid%id, 0 )
      CALL nl_set_cu_physics( grid%id, 0 )
      CALL nl_set_cu_diag( grid%id, 0 )
      CALL nl_set_damp_opt( grid%id, 0 )
      CALL nl_set_sst_update( grid%id, 0 )
#if (EM_CORE == 1)
      CALL nl_set_sf_ocean_physics( grid%id, 0 )
#endif
      CALL nl_set_fractional_seaice( grid%id, 0 )
      CALL nl_set_gwd_opt( grid%id, 0 )
      CALL nl_set_feedback( grid%id, 0 )
      ! set bc
#if (EM_CORE == 1)
      CALL nl_set_diff_6th_opt( grid%id, 0 )
      CALL nl_set_constant_bc(1, grid%constant_bc)
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
#endif

#if ( WRF_CHEM == 1 )
      ! set chemistry option to zero
      CALL nl_set_chem_opt (grid%id, 0)
      CALL nl_set_aer_ra_feedback (grid%id, 0)
      CALL nl_set_io_form_auxinput5 (grid%id, 0)
      CALL nl_set_io_form_auxinput6 (grid%id, 0)
      CALL nl_set_io_form_auxinput7 (grid%id, 0)
      CALL nl_set_io_form_auxinput8 (grid%id, 0)
      CALL nl_set_io_form_auxinput13 (grid%id, 0)
      CALL nl_set_io_form_auxinput14 (grid%id, 0)
      CALL nl_set_io_form_auxinput15 (grid%id, 0)
#endif

      ! set diffusion to zero for backward integration

#if (EM_CORE == 1)
      grid%km_opt_dfi = grid%km_opt
      grid%diff_opt_dfi = grid%diff_opt
      CALL nl_set_km_opt( grid%id, 1)
      CALL nl_set_diff_opt( grid%id, 0)
      CALL nl_set_moist_adv_dfi_opt( grid%id, grid%moist_adv_dfi_opt)
      CALL nl_set_moist_adv_opt( grid%id,grid%moist_adv_dfi_opt)
#endif

      ! If a request to do pressure level diags, then shut it off
      ! until we get to the real forecast part of this integration.

#if (EM_CORE == 1)
      CALL nl_set_p_lev_diags( grid%id, grid%p_lev_diags_dfi)
#endif

#if (NMM_CORE == 1)

!
! CHANGE (SIGN ONLY OF) IMPORTANT TIME CONSTANTS
!
      CALL nl_get_time_step( grid%id, grid%time_step )
      if (grid%time_step .lt. 0) then
!       DT   =-DT

        write(mess,*) 'changing signs for backward integration'
        call wrf_message(mess)
        grid%CPGFV = -grid%CPGFV
        grid%EN    = -grid%EN
        grid%ENT   = -grid%ENT
        grid%F4D   = -grid%F4D
        grid%F4Q   = -grid%F4Q
        grid%EF4T  = -grid%EF4T
  
        grid%EM(:) = -grid%EM(:)
        grid%EMT(:) = -grid%EMT(:)
        grid%F4Q2(:) = -grid%F4Q2(:)
  
        grid%WPDAR(:,:)= -grid%WPDAR(:,:)
        grid%CPGFU(:,:)= -grid%CPGFU(:,:)
        grid%CURV(:,:)= -grid%CURV(:,:)
        grid%FCP(:,:)= -grid%FCP(:,:)
        grid%FAD(:,:)= -grid%FAD(:,:)
        grid%F(:,:)= -grid%F(:,:)
      endif
#endif

      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

!tgs 7apr11 - this call has been moved up
!     CALL dfi_save_arrays ( grid )
      CALL dfi_clear_accumulation( grid )
      CALL optfil_driver(grid)

      !tgs need to call start_domain here to reset bc initialization for negative dt
      CALL start_domain ( grid , .TRUE. )

   END SUBROUTINE dfi_bck_init


   SUBROUTINE dfi_fwd_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility, ONLY : WRFU_ClockSet
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc
      CHARACTER*80 mess
      INTEGER n_moist,nm,n_scalar,ns

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

#if (EM_CORE == 1)
         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi
#endif

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      grid%dfi_stage = DFI_FWD

      ! for Setup_Timekeeping to use when setting the clock

      IF ( grid%time_step_dfi .gt. 0 ) THEN
        CALL nl_set_time_step ( grid%id, grid%time_step_dfi )
      ELSE

      CALL nl_get_time_step( grid%id, grid%time_step )
        CALL nl_get_time_step_fract_num( grid%id, grid%time_step_fract_num )

      grid%time_step = abs(grid%time_step)
      CALL nl_set_time_step( grid%id, grid%time_step )

        grid%time_step_fract_num = abs(grid%time_step_fract_num)
        CALL nl_set_time_step_fract_num( grid%id, grid%time_step_fract_num )

      ENDIF

      grid%itimestep=0
      grid%xtime=0.

      ! reset physics options to normal
      CALL nl_set_mp_physics( grid%id, grid%mp_physics)
      CALL nl_set_ra_lw_physics( grid%id, grid%ra_lw_physics)
      CALL nl_set_ra_sw_physics( grid%id, grid%ra_sw_physics)
      CALL nl_set_sf_surface_physics( grid%id, grid%sf_surface_physics)
      CALL nl_set_sf_sfclay_physics( grid%id, grid%sf_sfclay_physics)
      CALL nl_set_sf_urban_physics( grid%id, grid%sf_urban_physics)
      CALL nl_set_bl_pbl_physics( grid%id, grid%bl_pbl_physics)
      CALL nl_set_cu_physics( grid%id, grid%cu_physics)
      CALL nl_set_cu_diag( grid%id, grid%cu_diag )
      CALL nl_set_damp_opt( grid%id, grid%damp_opt)
      CALL nl_set_sst_update( grid%id, 0)
#if (EM_CORE == 1)
      CALL nl_set_sf_ocean_physics( grid%id, 0)
#endif
      CALL nl_set_fractional_seaice( grid%id, grid%fractional_seaice)
      CALL nl_set_gwd_opt( grid%id, grid%gwd_opt)
      CALL nl_set_feedback( grid%id, 0 )
#if (EM_CORE == 1)
      CALL nl_set_diff_6th_opt( grid%id, grid%diff_6th_opt)
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
      ! set bc
      CALL nl_set_constant_bc(grid%id, grid%constant_bc)
#endif

#if (NMM_CORE == 1)
!
! CHANGE (SIGN ONLY OF) IMPORTANT TIME CONSTANTS
!
!mptest      CALL nl_get_time_step( grid%id, grid%time_step )

!!! here need to key off something else being the "wrong" sign
      if (grid%cpgfv .gt. 0) then

        write(mess,*) 'changing signs for forward integration'
        call wrf_message(mess)
        grid%CPGFV = -grid%CPGFV
        grid%EN    = -grid%EN
        grid%ENT   = -grid%ENT
        grid%F4D   = -grid%F4D
        grid%F4Q   = -grid%F4Q
        grid%EF4T  = -grid%EF4T
  
        grid%EM(:) = -grid%EM(:)
        grid%EMT(:) = -grid%EMT(:)
        grid%F4Q2(:) = -grid%F4Q2(:)
  
        grid%WPDAR(:,:)= -grid%WPDAR(:,:)
        grid%CPGFU(:,:)= -grid%CPGFU(:,:)
        grid%CURV(:,:)= -grid%CURV(:,:)
        grid%FCP(:,:)= -grid%FCP(:,:)
        grid%FAD(:,:)= -grid%FAD(:,:)
        grid%F(:,:)= -grid%F(:,:)
      endif
#endif


!#if ( WRF_CHEM == 1 )
!      ! reset chem option to normal
!      CALL nl_set_chem_opt( grid%id, grid%chem_opt)
!      CALL nl_set_aer_ra_feedback (grid%id, grid%aer_ra_feedback)
!#endif

#if (EM_CORE == 1)
      ! reset km_opt to normal
      grid%km_opt = grid%km_opt_dfi
      grid%diff_opt = grid%diff_opt_dfi
      CALL nl_set_km_opt( grid%id, grid%km_opt_dfi)
      CALL nl_set_diff_opt( grid%id, grid%diff_opt_dfi)
      CALL nl_set_moist_adv_opt( grid%id, grid%moist_adv_opt)
#endif


      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( head_grid )
      grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

      IF ( grid%dfi_opt .EQ. DFI_DFL ) THEN
         CALL dfi_save_arrays ( grid )
      END IF
      CALL dfi_clear_accumulation( grid )
      CALL optfil_driver(grid)

      !tgs need to call it here to reset bc initialization for positive time_step
      CALL start_domain ( grid , .TRUE. )

      !tgs  After start_domain moist and scalar arrays are fully dimentioned,
      !and initial values should be restored here if grid%dfi_savehydmeteors .EQ. 1:
    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then
!       print *,'FWD n_moist,PARAM_FIRST_SCALAR,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG', &
!                n_moist,PARAM_FIRST_SCALAR,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
!       print *,'FWD num_scalar,P_QNC,P_QNI,P_QNR,P_QNWFA,P_QNIFA',P_QNC,P_QNI,P_QNR,P_QNWFA,P_QNIFA
        n_moist = num_moist
         DO nm=PARAM_FIRST_SCALAR+1,n_moist
             grid%moist(:,:,:,nm)=grid%dfi_moist(:,:,:,nm)
         ENDDO
        grid%scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:)
    ENDIF

   END SUBROUTINE dfi_fwd_init

   SUBROUTINE dfi_fst_init ( grid )

      USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      CHARACTER (LEN=80)      :: wrf_error_message
      INTEGER n_moist,nm

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

         SUBROUTINE dfi_save_arrays(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_save_arrays

         SUBROUTINE dfi_clear_accumulation(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_clear_accumulation

         SUBROUTINE optfil_driver(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE optfil_driver

#if (EM_CORE == 1)
         SUBROUTINE rebalance_driver_dfi (grid)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
         END SUBROUTINE rebalance_driver_dfi
#endif

         SUBROUTINE start_domain(grid, allowed_to_read)
            USE module_domain, ONLY : domain
            TYPE (domain)          :: grid
            LOGICAL, INTENT(IN)    :: allowed_to_read
         END SUBROUTINE start_domain
      END INTERFACE

      grid%dfi_stage = DFI_FST

     ! reset time_step to normal and adaptive_time_step
      CALL nl_set_time_step( grid%id, grid%time_step )

      grid%itimestep=0
      grid%xtime=0.         ! BUG: This will probably not work for all DFI options
! only use adaptive time stepping for forecast
#if (EM_CORE == 1)
      if (grid%id == 1) then
         CALL nl_set_use_adaptive_time_step( 1, grid%use_adaptive_time_step )
      endif
      CALL nl_set_sst_update( grid%id, grid%sst_update)
      CALL nl_set_sf_ocean_physics( grid%id, grid%sf_ocean_physics)
      ! reset to normal bc
      CALL nl_set_constant_bc(grid%id, .false.)
#endif
      CALL nl_set_feedback( grid%id, grid%feedback )

#if ( WRF_CHEM == 1 )
      ! reset chem option to normal
      CALL nl_set_chem_opt( grid%id, grid%chem_opt)
      CALL nl_set_aer_ra_feedback (grid%id, grid%aer_ra_feedback)
      CALL nl_set_io_form_auxinput5 (grid%id, grid%io_form_auxinput5)
      CALL nl_set_io_form_auxinput6 (grid%id, grid%io_form_auxinput6)
      CALL nl_set_io_form_auxinput7 (grid%id, grid%io_form_auxinput7)
      CALL nl_set_io_form_auxinput8 (grid%id, grid%io_form_auxinput8)
      CALL nl_set_io_form_auxinput13(grid%id, grid%io_form_auxinput13)
      CALL nl_set_io_form_auxinput14(grid%id, grid%io_form_auxinput14)
      CALL nl_set_io_form_auxinput15(grid%id, grid%io_form_auxinput15)

      ! This resets the open file handles associated with the chemistry
      ! auxinputs.  It may be a good idea to do this with other auxinputs as well.
      ! A better fix than the below may be to never assign the file handles to begin
      ! with when running DFI.

      grid%auxinput5_oid = 0
      grid%auxinput6_oid = 0
      grid%auxinput7_oid = 0
      grid%auxinput8_oid = 0
      grid%auxinput12_oid = 0
      grid%auxinput13_oid = 0
      grid%auxinput14_oid = 0
      grid%auxinput15_oid = 0
#endif
    
#if (EM_CORE == 1)
      ! reset pressure level diags to normal
      CALL nl_set_p_lev_diags ( grid%id, grid%p_lev_diags)
#endif


      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL start_domain ( grid , .TRUE. )

   END SUBROUTINE dfi_fst_init


   SUBROUTINE dfi_write_initialized_state( grid )

      ! Driver layer
      USE module_domain, ONLY : domain, head_grid
      USE module_io_domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      INTEGER             :: fid, ierr
      CHARACTER (LEN=80)  :: wrf_error_message
      CHARACTER (LEN=256) :: rstname
      CHARACTER (LEN=132) :: message

      TYPE (grid_config_rec_type) :: config_flags

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      WRITE (wrf_err_message,'(A,I4)') 'Writing out initialized model state'
      CALL wrf_message(TRIM(wrf_err_message))

      WRITE (rstname,'(A,I2.2)')'wrfinput_initialized_d',grid%id
      CALL open_w_dataset ( fid, TRIM(rstname), grid, config_flags, output_input, "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         WRITE( message , '("program wrf: error opening ",A," for writing")') TRIM(rstname)
         CALL WRF_ERROR_FATAL ( message )
      END IF
      CALL output_input ( fid,   grid, config_flags, ierr )
      CALL close_dataset ( fid, config_flags, "DATASET=INPUT" )

   END SUBROUTINE dfi_write_initialized_state

   SUBROUTINE tdfi_write_analyzed_state ( grid )

      ! Driver layer
      USE module_domain, ONLY : domain, head_grid
      USE module_io_domain
      USE module_configure, ONLY : grid_config_rec_type, model_config_rec

      IMPLICIT NONE

      TYPE (domain) , POINTER :: grid
      INTEGER             :: fid, ierr
      CHARACTER (LEN=80)  :: wrf_error_message
      CHARACTER (LEN=256) :: rstname
      CHARACTER (LEN=132) :: message

      TYPE (grid_config_rec_type) :: config_flags

      CALL model_to_grid_config_rec ( grid%id , model_config_rec , config_flags )

      rstname = 'wrfout_analyzed_d01'
      CALL open_w_dataset ( fid, TRIM(rstname),grid, config_flags, output_input, "DATASET=INPUT", ierr )
      IF ( ierr .NE. 0 ) THEN
         WRITE( message , '("program wrf: error opening ",A," for writing")') TRIM(rstname)
         CALL WRF_ERROR_FATAL ( message )
      END IF
      CALL output_input ( fid,   grid, config_flags, ierr )
      CALL close_dataset ( fid, config_flags, "DATASET=INPUT" )

   END SUBROUTINE tdfi_write_analyzed_state


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI array reset group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_array_reset ( )

      USE module_domain, ONLY : domain, head_grid, set_current_grid_ptr

      IMPLICIT NONE

      INTERFACE
         RECURSIVE SUBROUTINE dfi_array_reset_recurse(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_array_reset_recurse
      END INTERFACE

      ! Copy filtered arrays back into state arrays in grid structure, and
      !   restore original land surface fields

      CALL dfi_array_reset_recurse(head_grid)

      CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_array_reset

   SUBROUTINE dfi_array_reset( grid )

      USE module_domain, ONLY : domain
!      USE module_configure
!      USE module_driver_constants
!      USE module_machine
!      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      INTEGER :: its, ite, jts, jte, kts, kte, &
                 i, j, k
      INTEGER :: n_moist, nm, n_scalar, ns

      !  Input data.
      TYPE(domain) , POINTER          :: grid

      ! local
!      real p1000mb,eps,svp1,svp2,svp3,svpt0
      real eps
!      parameter (p1000mb = 1.e+05, eps=0.622,svp1=0.6112,svp3=29.65,svpt0=273.15)
      parameter (eps=0.622)
      integer nsoil , ncount
      REAL es,qs,pol,tx,temp,pres,rslf,rsif             
      CHARACTER*80 mess

      ncount = 0
      nsoil = grid%num_soil_layers

      IF ( grid%dfi_opt .EQ. DFI_NODFI ) RETURN


      ! Set dynamic variables
      ! divide by total DFI coefficient

#if (EM_CORE == 1)
      grid%MU_2(:,:)    = grid%dfi_mu(:,:)      / grid%hcoeff_tot
      grid%u_2(:,:,:)   = grid%dfi_u(:,:,:)     / grid%hcoeff_tot
      grid%v_2(:,:,:)   = grid%dfi_v(:,:,:)     / grid%hcoeff_tot
      grid%w_2(:,:,:)   = grid%dfi_w(:,:,:)     / grid%hcoeff_tot
      grid%ww(:,:,:)    = grid%dfi_ww(:,:,:)    / grid%hcoeff_tot
      grid%t_2(:,:,:)   = grid%dfi_t(:,:,:)     / grid%hcoeff_tot
      grid%phb(:,:,:)   = grid%dfi_phb(:,:,:)   / grid%hcoeff_tot
      grid%ph0(:,:,:)   = grid%dfi_ph0(:,:,:)   / grid%hcoeff_tot
      grid%php(:,:,:)   = grid%dfi_php(:,:,:)   / grid%hcoeff_tot
      grid%p(:,:,:)     = grid%dfi_p(:,:,:)     / grid%hcoeff_tot
      grid%ph_2(:,:,:)  = grid%dfi_ph(:,:,:)    / grid%hcoeff_tot
      grid%tke_2(:,:,:) = grid%dfi_tke(:,:,:)   / grid%hcoeff_tot
      grid%al(:,:,:)    = grid%dfi_al(:,:,:)    / grid%hcoeff_tot
      grid%alt(:,:,:)   = grid%dfi_alt(:,:,:)   / grid%hcoeff_tot
      grid%pb(:,:,:)    = grid%dfi_pb(:,:,:)    / grid%hcoeff_tot
    IF ( grid%dfi_savehydmeteors .EQ. 0 ) then
!    print *,'Normal DFI'
      grid%moist(:,:,:,:)  = max(0.,grid%dfi_moist(:,:,:,:)  / grid%hcoeff_tot)
      grid%scalar(:,:,:,:) = max(0.,grid%dfi_scalar(:,:,:,:) / grid%hcoeff_tot)
      IF ( grid%mp_physics_dfi .NE. NUWRF4ICESCHEME_DFI ) THEN
         grid%re_cloud(:,:,:)        = max(0.,grid%dfi_re_cloud(:,:,:)        / grid%hcoeff_tot)
         grid%re_ice(:,:,:)          = max(0.,grid%dfi_re_ice(:,:,:)          / grid%hcoeff_tot)
         grid%re_snow(:,:,:)         = max(0.,grid%dfi_re_snow(:,:,:)         / grid%hcoeff_tot)
#if (EM_CORE == 1)
      ELSE IF ( grid%mp_physics_dfi .EQ. NUWRF4ICESCHEME_DFI ) THEN
         grid%re_cloud_gsfc(:,:,:)   = max(0.,grid%dfi_re_cloud_gsfc(:,:,:)   / grid%hcoeff_tot)
         grid%re_rain_gsfc(:,:,:)    = max(0.,grid%dfi_re_rain_gsfc(:,:,:)    / grid%hcoeff_tot)
         grid%re_ice_gsfc(:,:,:)     = max(0.,grid%dfi_re_ice_gsfc(:,:,:)     / grid%hcoeff_tot)
         grid%re_snow_gsfc(:,:,:)    = max(0.,grid%dfi_re_snow_gsfc(:,:,:)    / grid%hcoeff_tot)
         grid%re_graupel_gsfc(:,:,:) = max(0.,grid%dfi_re_graupel_gsfc(:,:,:) / grid%hcoeff_tot)
         grid%re_hail_gsfc(:,:,:)    = max(0.,grid%dfi_re_hail_gsfc(:,:,:)    / grid%hcoeff_tot)
#endif
      ENDIF
    ELSE
!    print *,'In dfi_array_reset, QV comp, dfi_save_hydrometeors=1'
      grid%moist(:,:,:,P_QV)  = max(0.,grid%dfi_moist(:,:,:,P_QV)  / grid%hcoeff_tot)
    ENDIF
     if ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
!      grid%qvg(:,:)    = grid%dfi_qvg(:,:)      / grid%hcoeff_tot
     endif

    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then
!    print *,'restore initial surface fields'
      ! restore initial fields
      grid%SNOW  (:,:) = grid%dfi_SNOW  (:,:)
      grid%SNOWH (:,:) = grid%dfi_SNOWH (:,:)
      grid%SNOWC (:,:) = grid%dfi_SNOWC (:,:)
      grid%CANWAT(:,:) = grid%dfi_CANWAT(:,:)
      grid%TSK   (:,:) = grid%dfi_TSK   (:,:)

      grid%TSLB  (:,:,:)       = grid%dfi_TSLB   (:,:,:)
      grid%SMOIS (:,:,:)       = grid%dfi_SMOIS  (:,:,:)
      IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
         grid%QVG   (:,:) = grid%dfi_QVG   (:,:)
         grid%TSNAV (:,:) = grid%dfi_TSNAV (:,:)  
         grid%SOILT1(:,:) = grid%dfi_SOILT1(:,:)   
         grid%SMFR3D(:,:,:)       = grid%dfi_SMFR3D (:,:,:)
         grid%KEEPFR3DFLAG(:,:,:) = grid%dfi_KEEPFR3DFLAG(:,:,:)
      ENDIF

    ELSE
!      print *, 'take into account DFI snowfall'
      ! restore initial fields
            its = grid%sp31 ; ite = grid%ep31 ;
            jts = grid%sp33 ; jte = grid%ep33 ;
    DO j=jts,jte
       DO i=its,ite

        grid%SNOW  (i,j) = max(grid%SNOW  (i,j),grid%dfi_SNOW  (i,j))
        grid%SNOWH (i,j) = max(grid%SNOWH (i,j),grid%dfi_SNOWH (i,j))
        grid%SNOWC (i,j) = max(grid%SNOWC (i,j),grid%dfi_SNOWC (i,j))
        grid%CANWAT(i,j) = max(grid%CANWAT(i,j),grid%dfi_CANWAT(i,j))
      IF(grid%SNOWC (i,j) .eq.0.) then
        grid%TSK   (i,j) = grid%dfi_TSK   (i,j)
        do k=1,nsoil
          grid%TSLB  (i,k,j)       = grid%dfi_TSLB   (i,k,j)
          grid%SMOIS (i,k,j)       = grid%dfi_SMOIS  (i,k,j)
         IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
          grid%KEEPFR3DFLAG(i,k,j) = grid%dfi_KEEPFR3DFLAG(i,k,j)
          grid%SMFR3D(i,k,j)       = grid%dfi_SMFR3D (i,k,j)
         ENDIF
        enddo
         IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
          grid%QVG   (i,j)         = grid%dfi_QVG   (i,j)
          grid%TSNAV (i,j)         = grid%dfi_TSNAV (i,j)
          grid%SOILT1(i,j)         = grid%dfi_SOILT1(i,j)
         ENDIF
      ENDIF

      ENDDO
    ENDDO

    ENDIF

      ! restore analized hydrometeor and scalar fileds
    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then
!    print *,'In dfi_array_reset - restore initial hydrometeors'
!      grid%moist(:,:,:,:)      = grid%dfi_moist(:,:,:,:)    !tgs
        n_moist  = num_moist
      if (grid%dfi_stage .EQ. DFI_BCK) then
!tgs - backward integration changed only QV
        n_moist        = P_QV
        n_scalar       = PARAM_FIRST_SCALAR - 1
      endif
         DO nm=PARAM_FIRST_SCALAR+1,n_moist
             grid%moist(:,:,:,nm)=grid%dfi_moist(:,:,:,nm)
         ENDDO
        grid%scalar(:,:,:,:)     = grid%dfi_scalar(:,:,:,:)

       if(grid%dfi_stage .EQ. DFI_FWD) then
!tgs change QV to restore initial RH field after the diabatic DFI
            its = grid%sp31 ; ite = grid%ep31 ;  
            kts = grid%sp32 ; kte = grid%ep32 ;
            jts = grid%sp33 ; jte = grid%ep33 ;
   DO j=jts,jte
      DO i=its,ite
         do k = kts , kte
          if ( grid%use_theta_m .EQ. 1 ) then
            temp = (grid%t_2(i,k,j)+t0) / (1. + R_v/R_d*grid%moist(i,k,j,P_Qv)) * &
                       ( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb ) ** (r_d / Cp)
          else
            temp = (grid%t_2(i,k,j)+t0) * &
                       ( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb ) ** (r_d / Cp)
          endif
          pres = grid%p(i,k,j)+grid%pb(i,k,j)
!tgs rslf - function to compute qs from Thompson microphysics
          qs = rslf(pres, temp)

!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-thomp',temp,pres,qs
!      endif
!!         es=svp1*10.*EXP(svp2*(temp-svpt0)/(temp-svp3))
!!          qs=0.622*es/(pres/100.-es)

!     if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-wrf',temp,pres,qs
!      endif
        Tx = temp-273.15
        POL = 0.99999683       + TX*(-0.90826951E-02 +          &
           TX*(0.78736169E-04   + TX*(-0.61117958E-06 +         &
           TX*(0.43884187E-08   + TX*(-0.29883885E-10 +         &
           TX*(0.21874425E-12   + TX*(-0.17892321E-14 +         &
           TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
        es = 6.1078/POL**8
!!        qs = 0.62197*es / (pres/100. - es*0.37803)
!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-ruc',temp,pres,qs
!      endif

!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'before grid%moist (i,k,j,P_QV),grid%dfi_rh(i,k,j)',   &
!               grid%moist(i,k,j,P_QV),grid%dfi_rh(i,k,j)
!      endif

! Ensure that saturated points going into DFI remain saturated after DFI
      IF(grid%dfi_rh(i,k,j) == 1. ) then
         grid%moist (i,k,j,P_QV) = MAX(grid%moist (i,k,j,P_QV),grid%dfi_rh(i,k,j)*QS)
          ncount=ncount+1
      ENDIF
!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'after grid%moist (i,k,j,P_QV)',  &
!                grid%moist(i,k,j,P_QV)
!      endif
         enddo
      ENDDO
   ENDDO
!       print *,'QV reset to saturation at ncount= ',ncount
       endif
      !tgs need to call rebalance here again after hydrometeor and QV reset
!         print *,'   Rebalancing is on '
         CALL rebalance_driver_dfi ( grid )
   ENDIF

#endif

#if (NMM_CORE == 1)
        write(mess,*) ' divide by grid%hcoeff_tot: ', grid%hcoeff_tot
        call wrf_message(mess)
 if (grid%hcoeff_tot .lt. 0.0001) then
 call wrf_error_fatal("bad grid%hcoeff_tot")
 endif
       grid%pd(:,:)       = grid%dfi_pd(:,:)  / grid%hcoeff_tot
       grid%pint(:,:,:)      = grid%dfi_pint(:,:,:) / grid%hcoeff_tot
!       grid%dwdt(:,:,:)      = grid%dfi_dwdt(:,:,:) / grid%hcoeff_tot
       grid%t(:,:,:)      = grid%dfi_t(:,:,:) / grid%hcoeff_tot
       grid%q(:,:,:)      = grid%dfi_q(:,:,:) / grid%hcoeff_tot
       grid%q2(:,:,:)     = grid%dfi_q2(:,:,:) / grid%hcoeff_tot
       grid%cwm(:,:,:)    = grid%dfi_cwm(:,:,:) / grid%hcoeff_tot
       grid%u(:,:,:)      = grid%dfi_u(:,:,:) / grid%hcoeff_tot
       grid%v(:,:,:)      = grid%dfi_v(:,:,:) / grid%hcoeff_tot
      grid%moist(:,:,:,:)  = grid%dfi_moist(:,:,:,:)  / grid%hcoeff_tot
      grid%scalar(:,:,:,:) = grid%dfi_scalar(:,:,:,:) / grid%hcoeff_tot

      ! restore initial fields
      grid%SNOW(:,:)   = grid%dfi_SNOW(:,:)
      grid%SNOWH(:,:)  = grid%dfi_SNOWH(:,:)
!      grid%SNOWC(:,:)  = grid%dfi_SNOWC(:,:)
      grid%CANWAT(:,:) = grid%dfi_CANWAT(:,:)
      grid%NMM_TSK(:,:)    = grid%dfi_NMM_TSK(:,:)
      ! save soil fields
      grid%STC(:,:,:)         = grid%dfi_STC(:,:,:)
      grid%SMC(:,:,:)        = grid%dfi_SMC(:,:,:)
      grid%SH2O(:,:,:)       = grid%dfi_SH2O(:,:,:)
#endif


   END SUBROUTINE dfi_array_reset

   SUBROUTINE optfil_driver( grid )

      USE module_domain, ONLY : domain
      USE module_utility
!      USE module_wrf_error
!      USE module_timing
!      USE module_date_time
!      USE module_configure
      USE module_state_description
      USE module_model_constants

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid

      ! Local variables
      integer :: nstep2, nstepmax, rundfi, i, rc,hr,min,sec
      integer :: yr,jday
      real :: timestep, tauc
      TYPE(WRFU_TimeInterval) :: run_interval
      CHARACTER*80 mess

      timestep=abs(grid%dt)
      run_interval = grid%stop_subtime - grid%start_subtime
      CALL WRFU_TimeGet(grid%start_subtime, YY=yr, dayofYear=jday, H=hr, M=min, S=sec, rc=rc)
      CALL WRFU_TimeGet(grid%stop_subtime, YY=yr, dayofYear=jday, H=hr, M=min, S=sec, rc=rc)

      CALL WRFU_TimeIntervalGet( run_interval, S=rundfi, rc=rc )
      rundfi = abs(rundfi)

      nstep2= ceiling((1.0 + real(rundfi)/timestep) / 2.0)

      ! nstep2 is equal to a half of timesteps per initialization period,
      ! should not exceed nstepmax

      tauc = real(grid%dfi_cutoff_seconds)

      ! Get DFI coefficient
      grid%hcoeff(:) = 0.0
      IF ( grid%dfi_nfilter < 0 .OR. grid%dfi_nfilter > 8 ) THEN
write(mess,*) 'Invalid filter specified in namelist.'
call wrf_message(mess)
      ELSE
         call dfcoef(nstep2-1, grid%dt, tauc, grid%dfi_nfilter, grid%hcoeff)
      END IF

      IF ( MOD(int(1.0 + real(rundfi)/timestep),2) /= 0 ) THEN
         DO i=1,nstep2-1
            grid%hcoeff(2*nstep2-i) = grid%hcoeff(i)
         END DO
      ELSE
         DO i=1,nstep2
            grid%hcoeff(2*nstep2-i+1) = grid%hcoeff(i)
         END DO
      END IF

   END SUBROUTINE optfil_driver


   SUBROUTINE dfi_clear_accumulation( grid )

      USE module_domain, ONLY : domain
!      USE module_configure
!      USE module_driver_constants
!      USE module_machine
!      USE module_dm
!      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      !  Input data.
      TYPE(domain) , POINTER          :: grid

#if (EM_CORE == 1)
      grid%dfi_mu(:,:)       = 0.
      grid%dfi_u(:,:,:)      = 0.
      grid%dfi_v(:,:,:)      = 0.
      grid%dfi_w(:,:,:)      = 0.
      grid%dfi_ww(:,:,:)     = 0.
      grid%dfi_t(:,:,:)      = 0.
      grid%dfi_phb(:,:,:)    = 0.
      grid%dfi_ph0(:,:,:)    = 0.
      grid%dfi_php(:,:,:)    = 0.
      grid%dfi_p(:,:,:)      = 0.
      grid%dfi_ph(:,:,:)     = 0.
      grid%dfi_tke(:,:,:)    = 0.
      grid%dfi_al(:,:,:)     = 0.
      grid%dfi_alt(:,:,:)    = 0.
      grid%dfi_pb(:,:,:)     = 0.
    IF ( grid%dfi_savehydmeteors .EQ. 0 ) then  
!    print *,'normal DFI'
      grid%dfi_moist(:,:,:,:)  = 0.
      grid%dfi_scalar(:,:,:,:) = 0.
      IF ( grid%mp_physics_dfi .NE. NUWRF4ICESCHEME_DFI ) THEN
         grid%re_cloud(:,:,:)        = 0.
         grid%re_ice(:,:,:)          = 0.
         grid%re_snow(:,:,:)         = 0.
#if (EM_CORE == 1)
      ELSE IF ( grid%mp_physics_dfi .EQ. NUWRF4ICESCHEME_DFI ) THEN
         grid%re_cloud_gsfc(:,:,:)   = 0.
         grid%re_rain_gsfc(:,:,:)    = 0.
         grid%re_ice_gsfc(:,:,:)     = 0.
         grid%re_snow_gsfc(:,:,:)    = 0.
         grid%re_graupel_gsfc(:,:,:) = 0.
         grid%re_hail_gsfc(:,:,:)    = 0.
#endif
      ENDIF
    ELSE
!    print *,'In dfi_clear_accumulation, clear dfi_QV - dfi_savehydmeteors=1'
      grid%dfi_moist(:,:,:,P_QV) = 0.
    ENDIF
     if ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
!      grid%dfi_qvg(:,:)      = 0.
     endif
#endif

#if (NMM_CORE == 1)
       grid%dfi_pd(:,:)    = 0.
       grid%dfi_pint(:,:,:)   = 0.
       grid%dfi_dwdt(:,:,:)   = 0.
       grid%dfi_t(:,:,:)   = 0.
       grid%dfi_q(:,:,:)   = 0.
       grid%dfi_q2(:,:,:)  = 0.
       grid%dfi_cwm(:,:,:) = 0.
       grid%dfi_u(:,:,:)   = 0.
       grid%dfi_v(:,:,:)   = 0.
      grid%dfi_moist(:,:,:,:)  = 0.
      grid%dfi_scalar(:,:,:,:) = 0.
#endif

      grid%hcoeff_tot  = 0.0

   END SUBROUTINE dfi_clear_accumulation


   SUBROUTINE dfi_save_arrays( grid )

      USE module_domain, ONLY : domain
!      USE module_configure
!      USE module_driver_constants
!      USE module_machine
!      USE module_dm
      USE module_model_constants
      USE module_state_description

      IMPLICIT NONE

      INTEGER :: its, ite, jts, jte, kts, kte, &
                 i, j, k, n_moist, nm

      !  Input data.
      TYPE(domain) , POINTER          :: grid
      ! local

      REAL es,qs,pol,tx,temp,pres,rslf

#if (EM_CORE == 1)
      ! save surface 2-D fields
      grid%dfi_SNOW(:,:)   = grid%SNOW(:,:)
      grid%dfi_SNOWH(:,:)  = grid%SNOWH(:,:)
      grid%dfi_SNOWC(:,:)  = grid%SNOWC(:,:)
      grid%dfi_CANWAT(:,:) = grid%CANWAT(:,:)
      grid%dfi_TSK(:,:)    = grid%TSK(:,:)

      ! save soil fields
      grid%dfi_TSLB(:,:,:)         = grid%TSLB(:,:,:)
      grid%dfi_SMOIS(:,:,:)        = grid%SMOIS(:,:,:)
      ! RUC LSM only, need conditional
      IF ( grid%sf_surface_physics .EQ. RUCLSMSCHEME ) then
      grid%dfi_QVG(:,:)            = grid%QVG(:,:)
      grid%dfi_SOILT1(:,:)         = grid%SOILT1(:,:)
! use this array to save initial mu_2
!      grid%dfi_TSNAV(:,:)          = (grid%c1h(k)*grid%mu_2(:,:))
      grid%dfi_TSNAV(:,:)          = grid%TSNAV(:,:)
      grid%dfi_SMFR3D(:,:,:)       = grid%SMFR3D(:,:,:)
      grid%dfi_KEEPFR3DFLAG(:,:,:) = grid%KEEPFR3DFLAG(:,:,:)
      ENDIF
#endif

#if (NMM_CORE == 1)
      ! save surface 2-D fields
      grid%dfi_SNOW(:,:)   = grid%SNOW(:,:)
      grid%dfi_SNOWH(:,:)  = grid%SNOWH(:,:)
!      grid%dfi_SNOWC(:,:)  = grid%SNOWC(:,:)
      grid%dfi_CANWAT(:,:) = grid%CANWAT(:,:)
      grid%dfi_NMM_TSK(:,:)    = grid%NMM_TSK(:,:)
      ! save soil fields
      grid%dfi_STC(:,:,:)         = grid%STC(:,:,:)
      grid%dfi_SMC(:,:,:)        = grid%SMC(:,:,:)
      grid%dfi_SH2O(:,:,:)       = grid%SH2O(:,:,:)
#endif

#if (EM_CORE == 1)
      ! save hydrometeor and scalar fields
    IF ( grid%dfi_savehydmeteors .EQ. 1 ) then    !tgs
!    print *,'In dfi_save_arrays - save initial hydrometeors'
!       print *,'SAVE n_moist,PARAM_FIRST_SCALAR,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG', &
!                n_moist,PARAM_FIRST_SCALAR,P_QV,P_QC,P_QR,P_QI,P_QS,P_QG
        n_moist = num_moist
         DO nm=PARAM_FIRST_SCALAR+1,n_moist
             grid%dfi_moist(:,:,:,nm)=max(0.,grid%moist(:,:,:,nm))
         ENDDO
        grid%dfi_scalar(:,:,:,:) = max(0.,grid%scalar(:,:,:,:))
    ENDIF

       if(grid%dfi_stage .EQ. DFI_BCK) then
! compute initial RH field to be reintroduced after the diabatic DFI
            its = grid%sp31 ; ite = grid%ep31 ;  
            kts = grid%sp32 ; kte = grid%ep32 ;
            jts = grid%sp33 ; jte = grid%ep33 ;
   DO j=jts,jte
      DO i=its,ite
         do k = kts , kte
          if ( grid%use_theta_m .EQ. 1 ) then
            temp = (grid%t_2(i,k,j)+t0) / (1. + R_v/R_d*grid%moist(i,k,j,P_Qv)) * &
                       ( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb ) ** (r_d / Cp)
          else
            temp = (grid%t_2(i,k,j)+t0) * &
                       ( (grid%p(i,k,j)+grid%pb(i,k,j))/p1000mb ) ** (r_d / Cp)
          endif
          pres = grid%p(i,k,j)+grid%pb(i,k,j)
!tgs rslf - function to compute qs from Thompson microphysics
          qs = rslf(pres, temp)
!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-thomp',temp,pres,qs
!      endif

!!          es=svp1*10.*EXP(svp2*(temp-svpt0)/(temp-svp3))
!!          qs=0.622*es/(pres/100.-es)

!     if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-wrf',temp,pres,qs
!      endif
        Tx = temp-svpt0
        POL = 0.99999683       + TX*(-0.90826951E-02 +     &
           TX*(0.78736169E-04   + TX*(-0.61117958E-06 +    &
           TX*(0.43884187E-08   + TX*(-0.29883885E-10 +    &
           TX*(0.21874425E-12   + TX*(-0.17892321E-14 +    &
           TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
        es = 6.1078/POL**8
!!        qs = 0.62197*es / (pres/100. - es*0.37803)
!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs-ruc',temp,pres,qs
!      endif

          grid%dfi_rh (i,k,j) = MIN(1.,MAX(0.,grid%moist(i,k,j,P_QV)/qs))
!      if(i.eq. 464 .and. j.eq. 212 .and. k.eq.6) then
!       print *,'temp,pres,qs,grid%moist (i,k,j,P_QV),grid%dfi_rh (i,k,j)',temp,pres,qs,  &
!              grid%moist(i,k,j,P_QV),grid%dfi_rh (i,k,j)
!      endif

!tgs saturation check for points with water or ice clouds
      IF ((grid%moist (i,k,j,P_QC) .GT. 1.e-6  .or.        &
          grid%moist (i,k,j,P_QI) .GT. 1.e-6)  .and.       &
          grid%dfi_rh (i,k,j) .lt. 1.) THEN
               grid%dfi_rh (i,k,j)=1.
      ENDIF
         
        end do
      END DO
    ENDDO
       endif

#endif

   END SUBROUTINE dfi_save_arrays


   SUBROUTINE dfcoef (NSTEPS,DT,TAUC,NFILT,H)
!
!     calculate filter weights with selected window.
!
!       peter lynch and xiang-yu huang
!
!       ref: see hamming, r.w., 1989: digital filters,
!                prentice-hall international. 3rd edition.
!
!       input:      nsteps  -  number of timesteps
!                              forward or backward.
!                   dt      -  time step in seconds.
!                   tauc    -  cut-off period in seconds.
!                   nfilt   -  indicator for selected filter.
!
!       output:     h       -  array(0:nsteps) with the
!                              required filter weights
!
!------------------------------------------------------------

      implicit none

      integer, intent(in)   :: nsteps, nfilt
      real   , intent(in)   :: dt, tauc
      real, intent(out)  :: h(1:nsteps+1)
      
      ! Local data

      integer               :: n
      real                  :: pi, omegac, x, window, deltat
      real                  :: hh(0:nsteps)

      pi=4*ATAN(1.)
      deltat=ABS(dt)

      ! windows are defined by a call and held in hh.

      if ( nfilt .eq. -1) call debug    (nsteps,h)

      IF ( NFILT .EQ. 0 ) CALL UNIFORM  (NSTEPS,HH)
#if ( WRFPLUS == 1 )
      IF ( NFILT .EQ. 1 ) CALL LANCZOS_ (NSTEPS,HH)
#else
      IF ( NFILT .EQ. 1 ) CALL LANCZOS  (NSTEPS,HH)
#endif
      IF ( NFILT .EQ. 2 ) CALL HAMMING  (NSTEPS,HH)
      IF ( NFILT .EQ. 3 ) CALL BLACKMAN (NSTEPS,HH)
      IF ( NFILT .EQ. 4 ) CALL KAISER   (NSTEPS,HH)
      IF ( NFILT .EQ. 5 ) CALL POTTER2  (NSTEPS,HH)
      IF ( NFILT .EQ. 6 ) CALL DOLPHWIN (NSTEPS,HH)

      IF ( NFILT .LE. 6 ) THEN     ! sinc-windowed filters

         ! calculate the cutoff frequency
         OMEGAC = 2.*PI/TAUC

         DO N=0,NSTEPS
            WINDOW = HH(N)
            IF ( N .EQ. 0 ) THEN
              X = (OMEGAC*DELTAT/PI)
            ELSE
              X = SIN(N*OMEGAC*DELTAT)/(N*PI)
            END IF
            HH(N) = X*WINDOW
         END DO

         ! normalize the sums to be unity
         CALL NORMLZ(HH,NSTEPS)

         DO N=0,NSTEPS
            H(N+1) = HH(NSTEPS-N)
         END DO

      ELSE IF ( NFILT .EQ. 7 ) THEN     ! dolph filter

         CALL DOLPH(DT,TAUC,NSTEPS,H)

      ELSE IF ( NFILT .EQ. 8 ) THEN     ! 2nd order, 2nd type quick start filter (RHO)

         CALL RHOFIL(DT,TAUC,2,NSTEPS*2,2,H,NSTEPS)

      END IF

      RETURN

   END SUBROUTINE dfcoef


   SUBROUTINE NORMLZ(HH,NMAX)

   ! normalize the sum of hh to be unity

      implicit none
  
      integer, intent(in)                       :: nmax
      real   , dimension(0:nmax), intent(out)   :: hh

      ! local data
      real     :: sumhh
      integer  :: n

      SUMHH = HH(0)
      DO N=1,NMAX
        SUMHH = SUMHH + 2*HH(N)
      ENDDO
      DO N=0,NMAX
        HH(N)  = HH(N)/SUMHH
      ENDDO

      RETURN

   END subroutine normlz


   subroutine debug(nsteps, ww)

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
      integer :: n

      do n=0,nsteps
         ww(n)=0
      end do

      ww(int(nsteps/2))=1

      return

   end subroutine debug


   SUBROUTINE UNIFORM(NSTEPS,WW)

   !  define uniform or rectangular window function.

      implicit none

      integer, intent(in)                        :: nsteps
      real   , dimension(0:nsteps), intent(out)  :: ww
       
      integer          :: n

      DO N=0,NSTEPS
        WW(N) = 1.
      ENDDO

      RETURN

   END subroutine uniform


#if ( WRFPLUS == 1 )
   SUBROUTINE LANCZOS_(NSTEPS,WW)
#else
   SUBROUTINE LANCZOS(NSTEPS,WW)
#endif

   ! define (genaralised) lanczos window function.

      implicit none

      integer,  parameter                      :: nmax = 1000
      integer,  intent(in)                     :: nsteps
      real   ,  dimension(0:nmax), intent(out) :: ww
      integer  ::  n
      real     :: power, pi, w

      ! (for the usual lanczos window, power = 1 )
      POWER = 1

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = SIN(N*PI/(NSTEPS+1)) / ( N*PI/(NSTEPS+1))
         ENDIF
         WW(N) = W**POWER
      ENDDO

      RETURN

#if ( WRFPLUS == 1 )
   END SUBROUTINE lanczos_
#else
   END SUBROUTINE lanczos
#endif


   SUBROUTINE HAMMING(NSTEPS,WW)

   ! define (genaralised) hamming window function.

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   ::   n
      real      :: alpha, pi, w

      ! (for the usual hamming window, alpha=0.54,
      !      for the hann window, alpha=0.50).
      ALPHA=0.54

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = ALPHA + (1-ALPHA)*COS(N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE hamming


   SUBROUTINE BLACKMAN(NSTEPS,WW)

   ! define blackman window function.

      implicit none

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer  :: n

      real :: pi, w

      PI=4*ATAN(1.)
      DO N=0,NSTEPS
         IF ( N .EQ. 0 ) THEN
            W = 1.0
         ELSE
            W = 0.42 + 0.50*COS(  N*PI/(NSTEPS))   &
                     + 0.08*COS(2*N*PI/(NSTEPS))
         ENDIF
         WW(N) = W
      ENDDO

      RETURN

   END SUBROUTINE blackman


   SUBROUTINE KAISER(NSTEPS,WW)

   ! define kaiser window function.

      implicit none

      real, external :: bessi0

      integer, intent(in)           :: nsteps
      real, dimension(0:nsteps)    :: ww
      integer   :: n
      real      :: alpha, xi0a, xn, as

      ALPHA = 1

      XI0A =  BESSI0(ALPHA)
      DO N=0,NSTEPS
         XN = N
         AS = ALPHA*SQRT(1.-(XN/NSTEPS)**2)
         WW(N) = BESSI0(AS) / XI0A
      ENDDO

      RETURN

   END SUBROUTINE kaiser


   REAL FUNCTION BESSI0(X)

   ! from numerical recipes (press, et al.)

      implicit none

      real(8)   :: Y
      real(8)   :: P1 = 1.0d0
      real(8)   :: P2 = 3.5156230D0
      real(8)   :: P3 = 3.0899424D0
      real(8)   :: P4 = 1.2067492D0
      real(8)   :: P5 = 0.2659732D0
      real(8)   :: P6 = 0.360768D-1
      real(8)   :: P7 = 0.45813D-2

      real*8    :: Q1 = 0.39894228D0
      real*8    :: Q2 = 0.1328592D-1
      real*8    :: Q3 = 0.225319D-2
      real*8    :: Q4 = -0.157565D-2
      real*8    :: Q5 = 0.916281D-2
      real*8    :: Q6 = -0.2057706D-1
      real*8    :: Q7 = 0.2635537D-1
      real*8    :: Q8 = -0.1647633D-1
      real*8    :: Q9 = 0.392377D-2

      real            :: x, ax


      IF (ABS(X).LT.3.75) THEN
        Y=(X/3.75)**2
        BESSI0=P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7)))))
      ELSE
        AX=ABS(X)
        Y=3.75/AX
        BESSI0=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4    &
           +Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
      ENDIF
      RETURN

   END FUNCTION bessi0


   SUBROUTINE POTTER2(NSTEPS,WW)

      ! define potter window function.
      ! modified to fall off over twice the range.

      implicit none

      integer, intent(in)                       :: nsteps
      real, dimension(0:nsteps),intent(out)     ::  ww
      integer  :: n
      real     :: ck, sum, arg

      ! local data
      real, dimension(0:3)   :: d
      real                   :: pi
      integer                :: ip

      d(0) = 0.35577019
      d(1) = 0.2436983
      d(2) = 0.07211497
      d(3) = 0.00630165

      PI=4*ATAN(1.)

      CK = 1.0
      DO N=0,NSTEPS
         IF (N.EQ.NSTEPS) CK = 0.5
         ARG = PI*FLOAT(N)/FLOAT(NSTEPS)
!min---        modification in next statement
         ARG = ARG/2.
!min---        end of modification
         SUM = D(0)
         DO IP=1,3
            SUM = SUM + 2.*D(IP)*COS(ARG*FLOAT(IP))
         END DO
         WW(N) = CK*SUM
      END DO

      RETURN

   END SUBROUTINE potter2


   SUBROUTINE dolphwin(m, window)

!     calculation of dolph-chebyshev window or, for short,
!     dolph window, using the expression in the reference:
!
!     antoniou, andreas, 1993: digital filters: analysis,
!     design and applications. mcgraw-hill, inc., 689pp.
!
!     the dolph window is optimal in the following sense:
!     for a given main-lobe width, the stop-band attenuation
!     is minimal; for a given stop-band level, the main-lobe
!     width is minimal.
!
!     it is possible to specify either the ripple-ratio r
!     or the stop-band edge thetas.

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window

      ! local data
      REAL, DIMENSION(0:2*M)               :: t
      REAL, DIMENSION(0:M)                 :: w, time
      REAL    :: pi, thetas, x0, term1, term2, rr, r, db, sum, arg
      INTEGER :: n, nm1, nt, i

      PI = 4*ATAN(1.D0)
      THETAS = 2*PI/M

      N = 2*M+1
      NM1 = N-1
      X0 = 1/COS(THETAS/2)

      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)
      WRITE(*,'(1X,''DOLPH: M,N='',2I8)')M,N
      WRITE(*,'(1X,''DOLPH: THETAS (STOP-BAND EDGE)='',F10.3)')THETAS
      WRITE(*,'(1X,''DOLPH: R,DB='',2F10.3)')R, DB

      DO NT=0,M
        SUM = RR
        DO I=1,M
          ARG = X0*COS(I*PI/N)
          CALL CHEBY(T,NM1,ARG)
          TERM1 = T(NM1)
          TERM2 = COS(2*NT*PI*I/N)
          SUM = SUM + 2*TERM1*TERM2
        ENDDO
        W(NT) = SUM/N
        TIME(NT) = NT
      ENDDO

!     fill up the array for return
      DO NT=0,M
        WINDOW(NT) = W(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolphwin


   SUBROUTINE dolph(deltat, taus, m, window)

!     calculation of dolph-chebyshev window or, for short,
!     dolph window, using the expression in the reference:
!
!     antoniou, andreas, 1993: digital filters: analysis,
!     design and applications. mcgraw-hill, inc., 689pp.
!
!     the dolph window is optimal in the following sense:
!     for a given main-lobe width, the stop-band attenuation
!     is minimal; for a given stop-band level, the main-lobe
!     width is minimal.

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)                  ::  m
      REAL, DIMENSION(0:M), INTENT(OUT)    ::  window
      REAL, INTENT(IN)                     :: deltat, taus

      ! local data
      integer, PARAMETER        :: NMAX = 5000
      REAL, dimension(0:NMAX)   :: t, w, time
      real, dimension(0:2*nmax) :: w2
      INTEGER                   :: NPRPE=0        ! no of pe
      CHARACTER*80              :: MES

      real    :: pi, thetas, x0, term1, term2, rr, r,db, sum, arg, sumw
      integer :: n, nm1, i, nt
      
      PI = 4*ATAN(1.D0)

      WRITE (mes,'(A,F8.2,A,F10.2)') 'In dolph, deltat = ',deltat,' taus = ',taus
      CALL wrf_message(TRIM(mes))

      N = 2*M+1
      NM1 = N-1

      THETAS = 2*PI*ABS(DELTAT/TAUS)
      X0 = 1/COS(THETAS/2)
      TERM1 = (X0 + SQRT(X0**2-1))**(FLOAT(N-1))
      TERM2 = (X0 - SQRT(X0**2-1))**(FLOAT(N-1))
      RR = 0.5*(TERM1+TERM2)
      R = 1/RR
      DB = 20*LOG10(R)

      WRITE (mes,'(A,2I8)') 'In dolph: M,N = ', M,N
      CALL wrf_message(TRIM(mes))
      WRITE (mes,'(A,F10.3)') 'In dolph: THETAS (STOP-BAND EDGE) = ', thetas
      CALL wrf_message(TRIM(mes))
      WRITE (mes,'(A,2F10.3)') 'In dolph: R,DB = ', R,DB
      CALL wrf_message(TRIM(mes))

      DO NT=0,M
         SUM = 1
         DO I=1,M
            ARG = X0*COS(I*PI/N)
            CALL CHEBY(T,NM1,ARG)
            TERM1 = T(NM1)
            TERM2 = COS(2*NT*PI*I/N)
            SUM = SUM + R*2*TERM1*TERM2
         ENDDO
         W(NT) = SUM/N
         TIME(NT) = NT
         WRITE (mes,'(A,F10.6,2x,E17.7)') 'In dolph: TIME, W = ', TIME(NT), W(NT)
         CALL wrf_message(TRIM(mes))
      ENDDO
!     fill in the negative-time values by symmetry.
      DO NT=0,M
         W2(M+NT) = W(NT)
         W2(M-NT) = W(NT)
      ENDDO

!     fill up the array for return
      SUMW = 0.
      DO NT=0,2*M
         SUMW = SUMW + W2(NT)
      ENDDO
      WRITE (mes,'(A,F10.4)') 'In dolph: SUM OF WEIGHTS W2 = ', sumw
      CALL wrf_message(TRIM(mes))

      DO NT=0,M
         WINDOW(NT) = W2(NT)
      ENDDO

      RETURN

   END SUBROUTINE dolph


   SUBROUTINE cheby(t, n, x)

!     calculate all chebyshev polynomials up to order n
!     for the argument value x.

!     reference: numerical recipes, page 184, recurrence
!         t_n(x) = 2xt_{n-1}(x) - t_{n-2}(x) ,  n>=2.

      IMPLICIT NONE

      ! Arguments
      INTEGER, INTENT(IN)  :: n
      REAL, INTENT(IN)     :: x
      REAL, DIMENSION(0:N) :: t

      integer  :: nn

      T(0) = 1
      T(1) = X
      IF(N.LT.2) RETURN
      DO NN=2,N
         T(NN) = 2*X*T(NN-1) - T(NN-2)
      ENDDO

      RETURN

   END SUBROUTINE cheby


   SUBROUTINE rhofil(dt, tauc, norder, nstep, ictype, frow, nosize)

!       RHO = recurssive high order.
!
!       This routine calculates and returns the
!       Last Row, FROW, of the FILTER matrix.
!
!       Input Parameters:
!              DT  :  Time Step in seconds
!            TAUC  :  Cut-off period (hours)
!          NORDER  :  Order of QS Filter
!           NSTEP  :  Number of step/Size of row.
!          ICTYPE  :  Initial Conditions
!          NOSIZE  :  Max. side of FROW.
!
!       Working Fields:
!           ACOEF  :  X-coefficients of filter
!           BCOEF  :  Y-coefficients of filter
!          FILTER  :  Filter Matrix.
!
!       Output Parameters:
!            FROW  : Last Row of Filter Matrix.
!
!       Note: Two types of initial conditions are permitted.
!          ICTYPE = 1 : Order increasing each row to NORDER.
!          ICTYPE = 2 : Order fixed at NORDER throughout.
!
!       DOUBLE PRECISION USED THROUGHOUT.

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      DOUBLE PRECISION MUC

!       N.B. Single Precision for List Parameters.
      REAL, intent(in)    ::  DT,TAUC

!       Space for the last row of FILTER.
      integer, intent(in) ::  norder, nstep, ictype, nosize
      REAL   , dimension(0:nosize), intent(out)::  FROW

!       Arrays for rho filter.
      integer, PARAMETER  :: NOMAX=100
      real   , dimension(0:NOMAX) :: acoef, bcoef
      real   , dimension(0:NOMAX,0:NOMAX) :: filter
!       Working space.
      real   , dimension(0:NOMAX) :: alpha, beta

      real   :: DTT

      DTT = ABS(DT)
      PI = 2*DASIN(1.D0)
      IOTA = CMPLX(0.,1.)

!       Filtering Parameters (derived).
      THETAC = 2*PI*DTT/(TAUC)
      MUC = tan(THETAC/2)
      FC = THETAC/(2*PI)

!     Clear the arrays.
    DO NC=0,NOMAX
       ACOEF(NC) = 0.
       BCOEF(NC) = 0.
       ALPHA(NC) = 0.
       BETA (NC) = 0.
       FROW (NC) = 0.
       DO NR=0,NOMAX
          FILTER(NR,NC) = 0.
       ENDDO
    ENDDO

!     Fill up the Filter Matrix.
    FILTER(0,0) = 1.

!     Get the coefficients of the Filter.
    IF ( ICTYPE.eq.2 ) THEN
       CALL RHOCOF(NORDER,NOMAX,MUC, ACOEF,BCOEF)
    ENDIF

    DO 100 NROW=1,NSTEP

       IF ( ICTYPE.eq.1 ) THEN
          NORD = MIN(NROW,NORDER)
          IF ( NORD.le.NORDER) THEN
             CALL RHOCOF(NORD,NOMAX,MUC, ACOEF,BCOEF)
          ENDIF
       ENDIF

       DO K=0,NROW
          ALPHA(K) = ACOEF(NROW-K)
          IF(K.lt.NROW) BETA(K) = BCOEF(NROW-K)
       ENDDO

!        Correction for terms of negative index.
       IF ( ICTYPE.eq.2 ) THEN
          IF ( NROW.lt.NORDER ) THEN
             CN = 0.
             DO NN=NROW+1,NORDER
                CN = CN + (ACOEF(NN)+BCOEF(NN))
             ENDDO
             ALPHA(0) = ALPHA(0) + CN
          ENDIF
       ENDIF

!       Check sum of ALPHAs and BETAs = 1
      SUMAB = 0.
      DO NN=0,NROW
        SUMAB = SUMAB + ALPHA(NN)
          IF(NN.lt.NROW) SUMAB = SUMAB + BETA(NN)
      ENDDO

      DO KK=0,NROW-1
         SUMBF = 0.
         DO LL=0,NROW-1
            SUMBF = SUMBF + BETA(LL)*FILTER(LL,KK)
         ENDDO
         FILTER(NROW,KK) = ALPHA(KK)+SUMBF
      ENDDO
      FILTER(NROW,NROW) = ALPHA(NROW)

!       Check sum of row elements = 1
      SUMROW = 0.
      DO NN=0,NROW
        SUMROW = SUMROW + FILTER(NROW,NN)
      ENDDO

100 CONTINUE

      DO NC=0,NSTEP
        FROW(NC) = FILTER(NSTEP,NC)
      ENDDO

      RETURN

   END SUBROUTINE rhofil


   SUBROUTINE rhocof(nord, nomax, muc, ca, cb)

!     Get the coefficients of the RHO Filter

!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT NONE

      ! Arguments
      integer, intent(in)      :: nord, nomax
      real, dimension(0:nomax) :: ca, cb

      ! Functions
      double precision, external :: cnr

      ! Local variables
      INTEGER                  :: nn
      COMPLEX                  :: IOTA
      DOUBLE PRECISION         :: MUC, ZN
      DOUBLE PRECISION         :: pi, root2, rn, sigma, gain, sumcof

      PI = 2*ASIN(1.)
      ROOT2 = SQRT(2.)
      IOTA = (0.,1.)

      RN = 1./FLOAT(NORD)
      SIGMA = 1./( SQRT(2.**RN-1.) )

      GAIN  = (MUC*SIGMA/(1+MUC*SIGMA))**NORD
      ZN = (1-MUC*SIGMA)/(1+MUC*SIGMA)

      DO NN=0,NORD
        CA(NN) = CNR(NORD,NN)*GAIN
        IF(NN.gt.0) CB(NN) = -CNR(NORD,NN)*(-ZN)**NN
      ENDDO

!     Check sum of coefficients = 1
      SUMCOF = 0.
      DO NN=0,NORD
        SUMCOF = SUMCOF + CA(NN)
        IF(NN.gt.0) SUMCOF = SUMCOF + CB(NN)
      ENDDO

      RETURN

   END SUBROUTINE RHOCOF


   DOUBLE PRECISION FUNCTION cnr(n,r)

   ! Binomial Coefficient (n,r).

!      IMPLICIT DOUBLE PRECISION(C,X)
      IMPLICIT NONE

      ! Arguments
      INTEGER , intent(in)    :: n, R

      ! Local variables
      INTEGER          :: k
      DOUBLE PRECISION :: coeff, xn, xr, xk

      IF ( R.eq.0 ) THEN
         CNR = 1.0
         RETURN
      ENDIF
      Coeff = 1.0
      XN = DFLOAT(N)
      XR = DFLOAT(R)
      DO K=1,R
        XK = DFLOAT(K)
        COEFF = COEFF * ( (XN-XR+XK)/XK )
      ENDDO
      CNR = COEFF

      RETURN

   END FUNCTION cnr


    SUBROUTINE optfil (grid,NH,DELTAT,NHMAX)
!----------------------------------------------------------------------

!    SUBROUTINE optfil (NH,DELTAT,TAUP,TAUS,LPRINT,         &
!                         H,NHMAX)
!
! - Huang and Lynch optimal filter
!   Monthly Weather Review, Feb 1993
!----------------------------------------------------------
!       Input Parameters in List:
!             NH     :  Half-length of the Filter
!             DELTAT :  Time-step (in seconds).      
!             TAUP   :  Period of pass-band edge (hours).
!             TAUS   :  Period of stop-band edge (hours).
!             LPRINT :  Logical switch for messages.
!             NHMAX  :  Maximum permitted Half-length.
!
!       Output Parameters in List:
!             H      :  Impulse Response.
!             DP     :  Deviation in pass-band (db)
!             DS     :  Deviation in stop-band (db)
!----------------------------------------------------------
!
     USE module_domain, ONLY : domain
    
     TYPE(domain) , POINTER :: grid

     REAL,DIMENSION( 20) :: EDGE
     REAL,DIMENSION( 10) :: FX, WTX, DEVIAT
     REAL,DIMENSION(2*NHMAX+1) :: H
     logical LPRINT
     REAL, INTENT (IN) :: DELTAT
     INTEGER, INTENT (IN) :: NH, NHMAX
!
         TAUP = 3.
         TAUS = 1.5
         LPRINT = .true.
!initialize H array

         NL=2*NHMAX+1
        do 101 n=1,NL
          H(n)=0.
 101    continue

        NFILT = 2*NH+1
        print *,' start optfil, NFILT=', nfilt

!
! 930325 PL & XYH : the upper limit is changed from 64 to 128.
        IF(NFILT.LE.0 .OR. NFILT.GT.128 ) THEN
    WRITE(6,*) 'NH=',NH
       CALL wrf_error_fatal (' Sorry, error 1 in call to OPTFIL ')
 ENDIF
!
!       The following four should always be the same.
        JTYPE = 1
        NBANDS = 2
!CC     JPRINT = 0
        LGRID = 16
!
!       calculate transition frequencies.
   DT = ABS(DELTAT)
          FS = DT/(TAUS*3600.)
          FP = DT/(TAUP*3600.)
          IF(FS.GT.0.5) then
!            print *,' FS too large in OPTFIL '
       CALL wrf_error_fatal (' FS too large in OPTFIL ')
!            return
          end if
          IF(FP.LT.0.0) then
!            print *, ' FP too small in OPTFIL '
       CALL wrf_error_fatal (' FP too small in OPTFIL ')
!            return
          end if
!
!       Relative Weights in pass- and stop-bands.
          WTP = 1.0
          WTS = 1.0
!
!CC     NOTE: (FP,FC,FS) is an arithmetic progression, so
!CC           (1/FS,1/FC,1/FP) is a harmonic one.
!CC           TAUP = 1/( (1/TAUC)-(1/DTAU) )
!CC           TAUS = 1/( (1/TAUC)+(1/DTAU) )
!CC           TAUC   :  Cut-off Period (hours).
!CC           DTAU   :  Transition half-width (hours).
!CC           FC = 1/TAUC  ;  DF = 1/DTAU
!CC           FP = FC - DF :  FS = FC + DF
!
          IF ( LPRINT ) THEN
               TAUC = 2./((1/TAUS)+(1/TAUP))
               DTAU = 2./((1/TAUS)-(1/TAUP))
               FC = DT/(TAUC*3600.)
               DF = DT/(DTAU*3600.)
               WRITE(6,*) ' DT ' , dt
               WRITE(6,*) ' TAUS, TAUP ' , TAUS,TAUP
               WRITE(6,*) ' TAUC, DTAU ' , TAUC,DTAU
               WRITE(6,*) '   FP,   FS ' ,   FP,  FS   
               WRITE(6,*) '   FC,   DF ' ,   FC,  DF
               WRITE(6,*) '  WTS,  WTP ' ,  WTS, WTP
          ENDIF
!
!       Fill the control vectors for MCCPAR
        EDGE(1) = 0.0
        EDGE(2) = FP
        EDGE(3) = FS
        EDGE(4) = 0.5
        FX(1) = 1.0
        FX(2) = 0.0
        WTX(1) = WTP
        WTX(2) = WTS

        CALL MCCPAR(NFILT,JTYPE,NBANDS,LPRINT,LGRID,       &
                   EDGE,FX,WTX,DEVIAT, h )
!
!       Save the deviations in the pass- and stop-bands.
        DP = DEVIAT(1)
        DS = DEVIAT(2)
!
!     Fill out the array H (only first half filled in MCCPAR).
      IF(MOD(NFILT,2).EQ.0) THEN
         NHALF = ( NFILT )/2
      ELSE
         NHALF = (NFILT+1)/2
      ENDIF
      DO 100 nn=1,NHALF
         H(NFILT+1-nn) = h(nn)
  100 CONTINUE

!       normalize the sums to be unity
        sumh = 0
        do 150 n=1,NFILT
          sumh = sumh + H(n)
  150   continue
  print *,'SUMH =', sumh

        do 200 n=1,NFILT
          H(n)  = H(n)/sumh
  200   continue
        do 201 n=1,NFILT
        grid%hcoeff(n)=H(n)
  201   continue
!       print *,'HCOEFF(n) ', grid%hcoeff
!
   END SUBROUTINE optfil


      SUBROUTINE MCCPAR (NFILT,JTYPE,NBANDS,LPRINT,LGRID,    &
                   EDGE,FX,WTX,DEVIAT,h )

!      PROGRAM FOR THE DESIGN OF LINEAR PHASE FINITE IMPULSE
!      REPONSE (FIR) FILTERS USING THE REMEZ EXCHANGE ALGORITHM
!
!************************************************************
!*  Reference: McClellan, J.H., T.W. Parks and L.R.Rabiner, *
!*             1973:  A computer program for designing      *
!*             optimum FIR linear phase digital filters.    *
!*             IEEE Trans. on Audio and Electroacoustics,   *
!*             Vol AU-21, No. 6, 506-526.                   *
!************************************************************
!
!      THREE TYPES OF FILTERS ARE INCLUDED -- BANDPASS FILTERS
!      DIFFERENTIATORS, AND HILBERT TRANSFORM FILTERS
!
!---------------------------------------------------------------
!
!      COMMON /x3x/ PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION H(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION EDGE(20),FX(10),WTX(10),DEVIAT(10)
      DOUBLE PRECISION PI2,PI
      DOUBLE PRECISION AD,DEV,X,Y
      LOGICAL LPRINT
      
      PI  = 3.141592653589793
      PI2 = 6.283185307179586

!      ......

      NFMAX = 128
100      CONTINUE

!      PROGRAM INPUT SECTION

!CC     READ(5,*) NFILT,JTYPE,NBANDS,JPRINT,LGRID

      IF(NFILT.GT.NFMAX.OR.NFILT.LT.3) THEN
         CALL wrf_error_fatal (' **** ERROR IN INPUT DATA ****' )
      END IF
      IF(NBANDS.LE.0) NBANDS = 1

!      ....

      IF(LGRID.LE.0) LGRID = 16
      JB = 2*NBANDS
!cc       READ(5,*) (EDGE(J),J=1,JB)
!cc       READ(5,*) (FX(J),J=1,NBANDS)
!cc       READ(5,*) (WTX(J),J=1,NBANDS)
      IF(JTYPE.EQ.0) THEN
         CALL wrf_error_fatal (' **** ERROR IN INPUT DATA ****' )
      END IF
      NEG = 1
      IF(JTYPE.EQ.1) NEG = 0
      NODD = NFILT/2
      NODD = NFILT-2*NODD
      NFCNS = NFILT/2
      IF(NODD.EQ.1.AND.NEG.EQ.0) NFCNS = NFCNS+1

!      ...

      GRID(1) = EDGE(1)
      DELF = LGRID*NFCNS
      DELF = 0.5/DELF
      IF(NEG.EQ.0) GOTO 135
      IF(EDGE(1).LT.DELF) GRID(1) = DELF
135      CONTINUE
      J = 1
      L = 1
      LBAND = 1
140      FUP = EDGE(L+1)
145      TEMP = GRID(J)

!      ....

      DES(J) = EFF(TEMP,FX,WTX,LBAND,JTYPE)
      WT(J) = WATE(TEMP,FX,WTX,LBAND,JTYPE)
      J = J+1
      GRID(J) = TEMP+DELF
      IF(GRID(J).GT.FUP) GOTO 150
      GOTO 145
150      GRID(J-1) = FUP
      DES(J-1) = EFF(FUP,FX,WTX,LBAND,JTYPE)
      WT(J-1) = WATE(FUP,FX,WTX,LBAND,JTYPE)
      LBAND = LBAND+1
      L = L+2
      IF(LBAND.GT.NBANDS) GOTO 160
      GRID(J) = EDGE(L)
      GOTO 140
160      NGRID = J-1
      IF(NEG.NE.NODD) GOTO 165
      IF(GRID(NGRID).GT.(0.5-DELF)) NGRID = NGRID-1
165      CONTINUE

!      ......

      IF(NEG) 170,170,180
170      IF(NODD.EQ.1) GOTO 200
      DO 175 J=1,NGRID
            CHANGE = DCOS(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J) = WT(J)*CHANGE
175      CONTINUE
      GOTO 200
180      IF(NODD.EQ.1) GOTO 190
      DO 185 J = 1,NGRID
            CHANGE = DSIN(PI*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
185      CONTINUE
      GOTO 200
190      DO 195 J =1,NGRID
            CHANGE = DSIN(PI2*GRID(J))
            DES(J) = DES(J)/CHANGE
            WT(J)  = WT(J)*CHANGE
195      CONTINUE

!      ......

200      TEMP = FLOAT(NGRID-1)/FLOAT(NFCNS)
      DO 210 J = 1,NFCNS
            IEXT(J) = (J-1)*TEMP+1
210      CONTINUE
      IEXT(NFCNS+1) = NGRID
      NM1 = NFCNS-1
      NZ  = NFCNS+1

! CALL THE REMEZ EXCHANGE ALGORITHM TO DO THE APPROXIMATION PROBLEM

      CALL REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)

! CALCULATE THE IMPULSE RESPONSE

      IF(NEG) 300,300,320
300      IF(NODD.EQ.0) GOTO 310
      DO 305 J=1,NM1
            H(J) = 0.5*ALPHA(NZ-J)
305      CONTINUE
      H(NFCNS)=ALPHA(1)
      GOTO 350
310      H(1) = 0.25*ALPHA(NFCNS)
      DO 315 J = 2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)+ALPHA(NFCNS+2-J))
315      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)+0.25*ALPHA(2)
      GOTO 350
320      IF(NODD.EQ.0) GOTO 330
      H(1) = 0.25*ALPHA(NFCNS)
      H(2) = 0.25*ALPHA(NM1)
      DO 325 J = 3,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+3-J))
325      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(3)
      H(NZ) = 0.0
      GOTO 350
330      H(1) = 0.25*ALPHA(NFCNS)
      DO 335 J =2,NM1
            H(J) = 0.25*(ALPHA(NZ-J)-ALPHA(NFCNS+2-J))
335      CONTINUE
      H(NFCNS) = 0.5*ALPHA(1)-0.25*ALPHA(2)

!   PROGRAM OUTPUT SECTION

350   CONTINUE
!
      IF(LPRINT) THEN

         print *, '****************************************************'
         print *, 'FINITE IMPULSE RESPONSE (FIR)'
         print *, 'LINEAR PHASE DIGITAL FILTER DESIGN'
         print *, 'REMEZ EXCHANGE ALGORITHM'
      
      IF(JTYPE.EQ.1) WRITE(6,365)
365      FORMAT(25X,'BANDPASS FILTER'/)

      IF(JTYPE.EQ.2) WRITE(6,370)
370      FORMAT(25X,'DIFFERENTIATOR '/)

      IF(JTYPE.EQ.3) WRITE(6,375)
375      FORMAT(25X,'HILBERT TRANSFORMER '/)

      WRITE(6,378) NFILT
378      FORMAT(15X,'FILTER LENGTH =',I3/)

      WRITE(6,380)
380      FORMAT(15X,'***** IMPULSE RESPONSE *****')

      DO 381 J = 1,NFCNS
            K = NFILT+1-J
            IF(NEG.EQ.0) WRITE(6,382) J,H(J),K
            IF(NEG.EQ.1) WRITE(6,383) J,H(J),K
381      CONTINUE
382      FORMAT(20X,'H(',I3,') = ',E15.8,' = H(',I4,')')
383      FORMAT(20X,'H(',I3,') = ',E15.8,' = -H(',I4,')')

      IF(NEG.EQ.1.AND.NODD.EQ.1) WRITE(6,384) NZ
384      FORMAT(20X,'H(',I3,') = 0.0')

      DO 450 K=1,NBANDS,4
            KUP = K+3
            IF(KUP.GT.NBANDS) KUP = NBANDS
     print *
            WRITE(6,385) (J,J=K,KUP)
385            FORMAT(24X,4('BAND',I3,8X))
            WRITE(6,390) (EDGE(2*J-1),J=K,KUP)
390            FORMAT(2X,'LOWER BAND EDGE',5F15.8)
            WRITE(6,395) (EDGE(2*J),J=K,KUP)
395            FORMAT(2X,'UPPER BAND EDGE',5F15.8)
            IF(JTYPE.NE.2) WRITE(6,400) (FX(J),J=K,KUP)
400            FORMAT(2X,'DESIRED VALUE',2X,5F15.8)
            IF(JTYPE.EQ.2) WRITE(6,405) (FX(J),J=K,KUP)
405            FORMAT(2X,'DESIRED SLOPE',2X,5F15.8)
            WRITE(6,410) (WTX(J),J=K,KUP)
410            FORMAT(2X,'WEIGHTING',6X,5F15.8)
            DO 420 J = K,KUP
                  DEVIAT(J) = DEV/WTX(J)
420            CONTINUE
            WRITE(6,425) (DEVIAT(J),J=K,KUP)
425            FORMAT(2X,'DEVIATION',6X,5F15.8)
            IF(JTYPE.NE.1) GOTO 450
            DO 430 J = K,KUP
                  DEVIAT(J) = 20.0*ALOG10(DEVIAT(J))
430            CONTINUE
            WRITE(6,435) (DEVIAT(J),J=K,KUP)
435            FORMAT(2X,'DEVIATION IN DB',5F15.8)
450      CONTINUE
      print *, 'EXTREMAL FREQUENCIES'
      WRITE(6,455) (GRID(IEXT(J)),J=1,NZ)
455      FORMAT((2X,5F15.7))
      WRITE(6,460)
460      FORMAT(1X,70(1H*))
!
      ENDIF
!
!CC     IF(NFILT.NE.0) GOTO 100  ! removal of re-run loop.
!
   END SUBROUTINE mccpar


      FUNCTION EFF(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         EFF = FX(LBAND)
         RETURN
1        EFF = FX(LBAND)*TEMP
      END FUNCTION eff


      FUNCTION WATE(TEMP,FX,WTX,LBAND,JTYPE)
         DIMENSION FX(5),WTX(5)
         IF(JTYPE.EQ.2) GOTO 1
         WATE = WTX(LBAND)
         RETURN
1        IF(FX(LBAND).LT.0.0001) GOTO 2
         WATE = WTX(LBAND)/TEMP
         RETURN
2        WATE = WTX(LBAND)
      END FUNCTION wate


!      SUBROUTINE ERROR
!!         WRITE(6,*)' **** ERROR IN INPUT DATA ****'
!          CALL wrf_error_fatal (' **** ERROR IN INPUT DATA ****' )
!      END SUBROUTINE error

      
      SUBROUTINE REMEZ(EDGE,NBANDS,PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID)
!  THIS SUBROUTINE IMPLEMENTS THE REMEZ EXCHANGE ALGORITHM
!  FOR THE WEIGHTED CHEBCHEV APPROXIMATION OF A CONTINUOUS
!  FUNCTION WITH A SUM OF COSINES. INPUTS TO THE SUBROUTINE
!  ARE A DENSE GRID WHICH REPLACES THE FREQUENCY AXIS, THE
!  DESIRED FUNCTION ON THIS GRID, THE WEIGHT FUNCTION ON THE
!  GRID, THE NUMBER OF COSINES, AND THE INITIAL GUESS OF THE
!  EXTREMAL FREQUENCIES. THE PROGRAM MINIMIZES THE CHEBYSHEV
!  ERROR BY DETERMINING THE BEST LOCATION OF THE EXTREMAL
!  FREQUENCIES (POINTS OF MAXIMUM ERROR) AND THEN CALCULATES
!  THE COEFFICIENTS OF THE BEST APPROXIMATION.
!
!      COMMON /x3x/ PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID
      DIMENSION EDGE(20)
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DIMENSION A(66),P(65),Q(65)
      DOUBLE PRECISION PI2,DNUM,DDEN,DTEMP,A,P,Q
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION, EXTERNAL :: D, GEE
!
!  THE PROGRAM ALLOWS A MAXIMUM NUMBER OF ITERATIONS OF 25
!
      ITRMAX=25
      DEVL=-1.0
      NZ=NFCNS+1
      NZZ=NFCNS+2
      NITER=0
  100 CONTINUE
      IEXT(NZZ)=NGRID+1
      NITER=NITER+1
      IF(NITER.GT.ITRMAX) GO TO 400
      DO 110 J=1,NZ
      DTEMP=GRID(IEXT(J))
      DTEMP=DCOS(DTEMP*PI2)
  110 X(J)=DTEMP
      JET=(NFCNS-1)/15+1
      DO 120 J=1,NZ
  120 AD(J)=D(J,NZ,JET,X)
      DNUM=0.0
      DDEN=0.0
      K=1
      DO 130 J=1,NZ
      L=IEXT(J)
      DTEMP=AD(J)*DES(L)
      DNUM=DNUM+DTEMP
      DTEMP=K*AD(J)/WT(L)
      DDEN=DDEN+DTEMP
  130 K=-K
      DEV=DNUM/DDEN
      NU=1
      IF(DEV.GT.0.0) NU=-1
      DEV=-NU*DEV
      K=NU
      DO 140 J=1,NZ
      L=IEXT(J)
      DTEMP=K*DEV/WT(L)
      Y(J)=DES(L)+DTEMP
  140 K=-K
      IF(DEV.GE.DEVL) GO TO 150
      WRITE(6,*) ' ******** FAILURE TO CONVERGE *********** '
      WRITE(6,*) ' PROBABLE CAUSE IS MACHINE ROUNDING ERROR '
      WRITE(6,*) ' THE IMPULSE RESPONSE MAY BE CORRECT '
      WRITE(6,*) ' CHECK WITH A FREQUENCY RESPONSE '
      WRITE(6,*) ' **************************************** '
      GO TO 400
  150 DEVL=DEV
      JCHNGE=0
      K1=IEXT(1)
      KNZ=IEXT(NZ)
      KLOW=0
      NUT=-NU
      J=1
!
!  SEARCH FOR THE EXTERMAL FREQUENCIES OF THE BEST
!  APPROXIMATION.

  200 IF(J.EQ.NZZ) YNZ=COMP
      IF(J.GE.NZZ) GO TO 300
      KUP=IEXT(J+1)
      L=IEXT(J)+1
      NUT=-NUT
      IF(J.EQ.2) Y1=COMP
      COMP=DEV
      IF(L.GE.KUP) GO TO 220
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 220
      COMP=NUT*ERR
  210 L=L+1
      IF(L.GE.KUP) GO TO 215
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 215
      COMP=NUT*ERR
      GO TO 210
  215 IEXT(J)=L-1
      J=J+1
      KLOW=L-1
      JCHNGE=JCHNGE+1
      GO TO 200
  220 L=L-1
  225 L=L-1
      IF(L.LE.KLOW) GO TO 250
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.GT.0.0) GO TO 230
      IF(JCHNGE.LE.0) GO TO 225
      GO TO 260
  230 COMP=NUT*ERR
  235 L=L-1
      IF(L.LE.KLOW) GO TO 240
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 240
      COMP=NUT*ERR
      GO TO 235
  240 KLOW=IEXT(J)
      IEXT(J)=L+1
      J=J+1
      JCHNGE=JCHNGE+1
      GO TO 200
  250 L=IEXT(J)+1
      IF(JCHNGE.GT.0) GO TO 215
  255 L=L+1
      IF(L.GE.KUP) GO TO 260
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 255
      COMP=NUT*ERR
      GO TO 210
  260 KLOW=IEXT(J)
      J=J+1
      GO TO 200
  300 IF(J.GT.NZZ) GO TO 320
      IF(K1.GT.IEXT(1)) K1=IEXT(1)
      IF(KNZ.LT.IEXT(NZ)) KNZ=IEXT(NZ)
      NUT1=NUT
      NUT=-NU
      L=0
      KUP=K1
      COMP=YNZ*(1.00001)
      LUCK=1
  310 L=L+1
      IF(L.GE.KUP) GO TO 315
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 310
      COMP=NUT*ERR
      J=NZZ
      GO TO 210
  315 LUCK=6
      GO TO 325
  320 IF(LUCK.GT.9) GO TO 350
      IF(COMP.GT.Y1) Y1=COMP
      K1=IEXT(NZZ)
  325 L=NGRID+1
      KLOW=KNZ
      NUT=-NUT1
      COMP=Y1*(1.00001)
  330 L=L-1
      IF(L.LE.KLOW) GO TO 340
      ERR=GEE(L,NZ,GRID,PI2,X,Y,AD)
      ERR=(ERR-DES(L))*WT(L)
      DTEMP=NUT*ERR-COMP
      IF(DTEMP.LE.0.0) GO TO 330
      J=NZZ
      COMP=NUT*ERR
      LUCK=LUCK+10
      GO TO 235
  340 IF(LUCK.EQ.6) GO TO 370
      DO 345 J=1,NFCNS
  345 IEXT(NZZ-J)=IEXT(NZ-J)
      IEXT(1)=K1
      GO TO 100
  350 KN=IEXT(NZZ)
      DO 360 J=1,NFCNS
  360 IEXT(J)=IEXT(J+1)
      IEXT(NZ)=KN
      GO TO 100
  370 IF(JCHNGE.GT.0) GO TO 100
!
!  CALCULATION OF THE COEFFICIENTS OF THE BEST APPROXIMATION
!  USING THE INVERSE DISCRETE FOURIER TRANSFORM.
!      
  400 CONTINUE
      NM1=NFCNS-1
      FSH=1.0E-06
      GTEMP=GRID(1)
      X(NZZ)=-2.0
      CN=2*NFCNS-1
      DELF=1.0/CN
      L=1
      KKK=0
      IF(EDGE(1).EQ.0.0.AND.EDGE(2*NBANDS).EQ.0.5) KKK=1
      IF(NFCNS.LE.3) KKK=1
      IF(KKK.EQ.1) GO TO 405
      DTEMP=DCOS(PI2*GRID(1))
      DNUM=DCOS(PI2*GRID(NGRID))
      AA=2.0/(DTEMP-DNUM)
      BB=-(DTEMP+DNUM)/(DTEMP-DNUM)
  405 CONTINUE
      DO 430 J=1,NFCNS
      FT=(J-1)*DELF
      XT=DCOS(PI2*FT)
      IF(KKK.EQ.1) GO TO 410
      XT=(XT-BB)/AA
! original :      FT=ARCOS(XT)/PI2
      FT=ACOS(XT)/PI2
  410 XE=X(L)
      IF(XT.GT.XE) GO TO 420
      IF((XE-XT).LT.FSH) GO TO 415
      L=L+1
      GO TO 410
  415 A(J)=Y(L)
      GO TO 425
  420 IF((XT-XE).LT.FSH) GO TO 415
      GRID(1)=FT
      A(J)=GEE(1,NZ,GRID,PI2,X,Y,AD)
  425 CONTINUE
      IF(L.GT.1) L=L-1
  430 CONTINUE
      GRID(1)=GTEMP
      DDEN=PI2/CN
      DO 510 J=1,NFCNS
      DTEMP=0.0
      DNUM=(J-1)*DDEN
      IF(NM1.LT.1) GO TO 505
      DO 500 K=1,NM1
  500 DTEMP=DTEMP+A(K+1)*DCOS(DNUM*K)
  505 DTEMP=2.0*DTEMP+A(1)
  510 ALPHA(J)=DTEMP
      DO 550 J=2,NFCNS
  550 ALPHA(J)=2*ALPHA(J)/CN
      ALPHA(1)=ALPHA(1)/CN
      IF(KKK.EQ.1) GO TO 545
      P(1)=2.0*ALPHA(NFCNS)*BB+ALPHA(NM1)
      P(2)=2.0*AA*ALPHA(NFCNS)
      Q(1)=ALPHA(NFCNS-2)-ALPHA(NFCNS)
      DO 540 J=2,NM1
      IF(J.LT.NM1) GO TO 515
      AA=0.5*AA
      BB=0.5*BB
  515 CONTINUE
      P(J+1)=0.0
      DO 520 K=1,J
      A(K)=P(K)
  520 P(K)=2.0*BB*A(K)
      P(2)=P(2)+A(1)*2.0*AA
      JM1=J-1
      DO 525 K=1,JM1
  525 P(K)=P(K)+Q(K)+AA*A(K+1)
      JP1=J+1
      DO 530 K=3,JP1
  530 P(K)=P(K)+AA*A(K-1)
      IF(J.EQ.NM1) GO TO 540
      DO 535 K=1,J
  535 Q(K)=-A(K)
      Q(1)=Q(1)+ALPHA(NFCNS-1-J)
  540 CONTINUE
      DO 543 J=1,NFCNS
  543 ALPHA(J)=P(J)
  545 CONTINUE
      IF(NFCNS.GT.3) RETURN
      ALPHA(NFCNS+1)=0.0
      ALPHA(NFCNS+2)=0.0
   END SUBROUTINE remez

   DOUBLE PRECISION FUNCTION D(K,N,M,X)
!      COMMON /x3x/ PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION Q
      DOUBLE PRECISION PI2
      D = 1.0
      Q = X(K)
      DO 3 L = 1,M
      DO 2 J = L,N,M
            IF(J-K) 1,2,1
1                  D = 2.0*D*(Q-X(J))
2      CONTINUE
3      CONTINUE
      D = 1.0/D
   END FUNCTION D


   DOUBLE PRECISION FUNCTION GEE(K,N,GRID,PI2,X,Y,AD)
!      COMMON /x3x/ PI2,AD,DEV,X,Y,GRID,DES,WT,ALPHA,IEXT,NFCNS,NGRID
      DIMENSION IEXT(66),AD(66),ALPHA(66),X(66),Y(66)
      DIMENSION DES(1045),GRID(1045),WT(1045)
      DOUBLE PRECISION AD,DEV,X,Y
      DOUBLE PRECISION P,C,D,XF
      DOUBLE PRECISION PI2
      P = 0.0
      XF = GRID(K)
      XF = DCOS(PI2*XF)
      D = 0.0
      DO 1 J =1,N
            C = XF-X(J)
            C = AD(J)/C
            D = D+C
            P = P+C*Y(J)
1      CONTINUE
      GEE = P/D
   END FUNCTION GEE

!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE LIQUID SATURATION VAPOR MIXING RATIO AS
! A FUNCTION OF TEMPERATURE AND PRESSURE in Thompson microphysics
!

      REAL FUNCTION RSLF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESL,X
      REAL, PARAMETER:: C0= .611583699E03
      REAL, PARAMETER:: C1= .444606896E02
      REAL, PARAMETER:: C2= .143177157E01
      REAL, PARAMETER:: C3= .264224321E-1
      REAL, PARAMETER:: C4= .299291081E-3
      REAL, PARAMETER:: C5= .203154182E-5
      REAL, PARAMETER:: C6= .702620698E-8
      REAL, PARAMETER:: C7= .379534310E-11
      REAL, PARAMETER:: C8=-.321582393E-13

      X=MAX(-80.,T-273.16)

!      ESL=612.2*EXP(17.67*X/(T-29.65))
      ESL=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSLF=.622*ESL/(P-ESL)

      END FUNCTION RSLF
!+---+-----------------------------------------------------------------+
! THIS FUNCTION CALCULATES THE ICE SATURATION VAPOR MIXING RATIO AS A
! FUNCTION OF TEMPERATURE AND PRESSURE
!
      REAL FUNCTION RSIF(P,T)

      IMPLICIT NONE
      REAL, INTENT(IN):: P, T
      REAL:: ESI,X
      REAL, PARAMETER:: C0= .609868993E03
      REAL, PARAMETER:: C1= .499320233E02
      REAL, PARAMETER:: C2= .184672631E01
      REAL, PARAMETER:: C3= .402737184E-1
      REAL, PARAMETER:: C4= .565392987E-3
      REAL, PARAMETER:: C5= .521693933E-5
      REAL, PARAMETER:: C6= .307839583E-7
      REAL, PARAMETER:: C7= .105785160E-9
      REAL, PARAMETER:: C8= .161444444E-12

      X=MAX(-80.,T-273.16)
      ESI=C0+X*(C1+X*(C2+X*(C3+X*(C4+X*(C5+X*(C6+X*(C7+X*C8)))))))
      RSIF=.622*ESI/(P-ESI)

!    ALTERNATIVE
!  ; Source: Murphy and Koop, Review of the vapour pressure of ice and
!             supercooled water for atmospheric applications, Q. J. R.
!             Meteorol. Soc (2005), 131, pp. 1539-1565.
!     ESI = EXP(9.550426 - 5723.265/T + 3.53068*ALOG(T) - 0.00728332*T)

      END FUNCTION RSIF
!+---+-----------------------------------------------------------------+


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI startfwd group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_startfwd_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_startfwd_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_startfwd_init_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL dfi_startfwd_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_startfwd_init


   RECURSIVE SUBROUTINE dfi_startfwd_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_startfwd_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_startfwd_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_startfwd_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_startfwd_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_startfwd_init_recurse


   SUBROUTINE dfi_startfwd_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

      END INTERFACE

      grid%dfi_stage = DFI_STARTFWD

#if (EM_CORE == 1)
      ! No need for adaptive time-step
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
#endif

      CALL Setup_Timekeeping (grid)
      grid%start_subtime = domain_get_start_time ( head_grid )
      grid%stop_subtime = domain_get_stop_time ( head_grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

   END SUBROUTINE dfi_startfwd_init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI startbck group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_startbck_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_startbck_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_startbck_init_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL dfi_startbck_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_startbck_init


   RECURSIVE SUBROUTINE dfi_startbck_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_startbck_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_startbck_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_startbck_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_startbck_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_startbck_init_recurse


   SUBROUTINE dfi_startbck_init ( grid )

      USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
      USE module_utility
      USE module_state_description

      IMPLICIT NONE

      TYPE (domain) , POINTER                 :: grid
      INTEGER rc

      INTERFACE
         SUBROUTINE Setup_Timekeeping(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE Setup_Timekeeping

      END INTERFACE

      grid%dfi_stage = DFI_STARTBCK

      ! set physics options to zero
      CALL nl_set_mp_physics( grid%id, 0 )
      CALL nl_set_ra_lw_physics( grid%id, 0 )
      CALL nl_set_ra_sw_physics( grid%id, 0 )
      CALL nl_set_sf_surface_physics( grid%id, 0 )
      CALL nl_set_sf_sfclay_physics( grid%id, 0 )
      CALL nl_set_sf_urban_physics( grid%id, 0 )
      CALL nl_set_bl_pbl_physics( grid%id, 0 )
      CALL nl_set_cu_physics( grid%id, 0 )
      CALL nl_set_cu_diag( grid%id, 0 )
      CALL nl_set_damp_opt( grid%id, 0 )
      CALL nl_set_sst_update( grid%id, 0 )
#if (EM_CORE == 1)
      CALL nl_set_sf_ocean_physics( grid%id, 0 )
#endif
      CALL nl_set_gwd_opt( grid%id, 0 )
#if (EM_CORE == 1)
      CALL nl_set_diff_6th_opt( grid%id, 0 )
      CALL nl_set_use_adaptive_time_step( grid%id, .false. )
#endif
      CALL nl_set_feedback( grid%id, 0 )
#if (EM_CORE == 1)
      ! set bc
      CALL nl_set_constant_bc( grid%id, head_grid%constant_bc)
#endif

#if ( WRF_CHEM == 1 )
      ! set chemistry option to zero
      CALL nl_set_chem_opt (grid%id, 0)
      CALL nl_set_aer_ra_feedback (grid%id, 0)
      CALL nl_set_io_form_auxinput5 (grid%id, 0)
      CALL nl_set_io_form_auxinput6 (grid%id, 0)
      CALL nl_set_io_form_auxinput7 (grid%id, 0)
      CALL nl_set_io_form_auxinput8 (grid%id, 0)
      CALL nl_set_io_form_auxinput13 (grid%id, 0)
      CALL nl_set_io_form_auxinput14 (grid%id, 0)
      CALL nl_set_io_form_auxinput15 (grid%id, 0)
#endif

#if (EM_CORE == 1)
      ! set diffusion to zero for backward integration
      grid%km_opt_dfi = grid%km_opt
      grid%diff_opt_dfi = grid%diff_opt
      CALL nl_set_km_opt( grid%id, grid%km_opt_dfi)
      CALL nl_set_diff_opt( grid%id, grid%diff_opt_dfi)
      CALL nl_set_moist_adv_dfi_opt( grid%id, grid%moist_adv_dfi_opt)
      CALL nl_set_moist_adv_opt( grid%id,grid%moist_adv_dfi_opt)
#endif

      ! If a request to do pressure level diags, then shut it off
      ! until we get to the real forecast part of this integration.

#if (EM_CORE == 1)
      CALL nl_set_p_lev_diags( grid%id, grid%p_lev_diags_dfi)
#endif

      !tgs need to call start_domain here to reset bc initialization for
      !      negative dt, but only for outer domain.
      if (grid%id == 1) then
        CALL start_domain ( grid , .TRUE. )
      endif

      ! Call wrf_run to advance forward 1 step

      ! Negate time step
      CALL nl_set_time_step ( grid%id, -grid%time_step )

      CALL Setup_Timekeeping (grid)

      grid%start_subtime = domain_get_start_time ( grid )
      grid%stop_subtime = domain_get_stop_time ( grid )

      CALL WRFU_ClockSet(grid%domain_clock, currTime=grid%start_subtime, rc=rc)

   END SUBROUTINE dfi_startbck_init


   SUBROUTINE wrf_dfi_bck_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time
     USE module_utility
     USE module_state_description
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_bck_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_bck_init_recurse
     END INTERFACE

     ! We can only call dfi_bck_init for the head_grid
     !    since nests have not been instantiated at this point,
     !    so, dfi_bck_init will need to be called for each
     !    nest from integrate.
     CALL dfi_bck_init_recurse(head_grid)

   END SUBROUTINE wrf_dfi_bck_init

   RECURSIVE SUBROUTINE dfi_bck_init_recurse(grid)

     USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_bck_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_bck_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_bck_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_bck_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_bck_init_recurse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI forward initialization group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_fwd_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_fwd_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_fwd_init_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL dfi_fwd_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_fwd_init

   RECURSIVE SUBROUTINE dfi_fwd_init_recurse(grid)

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_fwd_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_fwd_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_fwd_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_fwd_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_fwd_init_recurse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI forecast initialization group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_fst_init ( )

     USE module_domain, ONLY : domain, head_grid, domain_get_stop_time, domain_get_start_time, set_current_grid_ptr
     USE module_utility
     
     IMPLICIT NONE

     INTERFACE
        SUBROUTINE dfi_fst_init_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_fst_init_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL dfi_fst_init_recurse(head_grid)
     
     CALL set_current_grid_ptr( head_grid )

   END SUBROUTINE wrf_dfi_fst_init

   RECURSIVE SUBROUTINE dfi_fst_init_recurse ( grid )

     USE module_domain, ONLY : domain, domain_get_stop_time, domain_get_start_time, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_fst_init(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_fst_init
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        grid_ptr%dt = abs(grid_ptr%dt)
        grid_ptr%time_step = abs(grid_ptr%time_step)
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_fst_init( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_fst_init_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_fst_init_recurse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DFI write initialization group of functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE wrf_dfi_write_initialized_state( )

     USE module_domain, ONLY : domain, head_grid

     INTERFACE
        SUBROUTINE dfi_write_initialized_state_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_write_initialized_state_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL dfi_write_initialized_state_recurse(head_grid)
     
   END SUBROUTINE wrf_dfi_write_initialized_state

   RECURSIVE SUBROUTINE dfi_write_initialized_state_recurse( grid )

     USE module_domain, ONLY : domain, max_nests
     
     IMPLICIT NONE
     
     INTERFACE
        SUBROUTINE dfi_write_initialized_state( grid )
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE dfi_write_initialized_state
     END INTERFACE
     
     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
     
     grid_ptr => grid
     
     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        CALL dfi_write_initialized_state( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_write_initialized_state_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_write_initialized_state_recurse

   SUBROUTINE wrf_tdfi_write_analyzed_state( )

     USE module_domain, ONLY : domain, head_grid

     INTERFACE
        SUBROUTINE tdfi_write_analyzed_state_recurse(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE tdfi_write_analyzed_state_recurse
     END INTERFACE

     ! Now, setup all nests

     CALL tdfi_write_analyzed_state_recurse(head_grid)

   END SUBROUTINE wrf_tdfi_write_analyzed_state

   RECURSIVE SUBROUTINE tdfi_write_analyzed_state_recurse( grid )

     USE module_domain, ONLY : domain, max_nests

     IMPLICIT NONE

     INTERFACE
        SUBROUTINE tdfi_write_analyzed_state( grid )
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
        END SUBROUTINE tdfi_write_analyzed_state
     END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr

     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        !
        ! Assure that time-step is set back to positive
        !   for this forward step.
        !
        CALL tdfi_write_analyzed_state( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL tdfi_write_analyzed_state_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO

   END SUBROUTINE tdfi_write_analyzed_state_recurse

   RECURSIVE SUBROUTINE dfi_array_reset_recurse(grid)

     USE module_domain, ONLY : domain, max_nests, set_current_grid_ptr

     IMPLICIT NONE

      INTERFACE
         SUBROUTINE dfi_array_reset(grid)
            USE module_domain, ONLY : domain
            TYPE (domain), POINTER :: grid
         END SUBROUTINE dfi_array_reset
      END INTERFACE

     INTEGER :: kid
     TYPE (domain), POINTER :: grid
     TYPE (domain), POINTER :: grid_ptr
    
     grid_ptr => grid

     DO WHILE ( ASSOCIATED( grid_ptr ) )
        CALL set_current_grid_ptr( grid_ptr )
        CALL dfi_array_reset( grid_ptr )
        DO kid = 1, max_nests
           IF ( ASSOCIATED( grid_ptr%nests(kid)%ptr ) ) THEN
              CALL dfi_array_reset_recurse(grid_ptr%nests(kid)%ptr)
           ENDIF
        END DO
        grid_ptr => grid_ptr%sibling
     END DO
     
   END SUBROUTINE dfi_array_reset_recurse

!-------------------------------------------------------------------

#if (EM_CORE == 1)
   SUBROUTINE rebalance_driver_dfi ( grid )

      USE module_domain, ONLY : domain

      IMPLICIT NONE

      TYPE (domain)          :: grid

      CALL rebalance_dfi( grid &
!
#include "actual_new_args.inc"
!
      )

   END SUBROUTINE rebalance_driver_dfi

!---------------------------------------------------------------------

   SUBROUTINE rebalance_dfi ( grid  &
!
#include "dummy_new_args.inc"
!
                        )
      USE module_domain, ONLY : domain
      USE module_utility
      USE module_state_description
      USE module_model_constants
      USE module_driver_constants
#ifdef DM_PARALLEL
   USE module_dm
   USE module_comm_dm, ONLY : &
                           HALO_EM_INIT_1_sub   &
                          ,HALO_EM_INIT_2_sub   &
                          ,HALO_EM_INIT_3_sub   &
                          ,HALO_EM_INIT_4_sub   &
                          ,HALO_EM_INIT_5_sub   
#endif


      IMPLICIT NONE

      TYPE (domain)        :: grid

#include "dummy_new_decl.inc"

      REAL :: p_surf ,  pd_surf, p_surf_int , pb_int , ht_hold
      REAL :: qvf , qvf1 , qvf2, qtot
      REAL :: pfu, pfd, phm
      REAL :: z0, z1, z2, w1, w2

      !  Local domain indices and counters.

      INTEGER :: n_moist

      INTEGER                             ::                       &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte, &
                                     ips, ipe, jps, jpe, kps, kpe, &
                                     i, j, k, kk, ispe, ktf

      SELECT CASE ( model_data_order )
         CASE ( DATA_ORDER_ZXY )
            kds = grid%sd31 ; kde = grid%ed31 ;
            ids = grid%sd32 ; ide = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            kms = grid%sm31 ; kme = grid%em31 ;
            ims = grid%sm32 ; ime = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            kts = grid%sp31 ; kte = grid%ep31 ;   ! note that tile is entire patch
            its = grid%sp32 ; ite = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XYZ )
            ids = grid%sd31 ; ide = grid%ed31 ;
            jds = grid%sd32 ; jde = grid%ed32 ;
            kds = grid%sd33 ; kde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            jms = grid%sm32 ; jme = grid%em32 ;
            kms = grid%sm33 ; kme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            jts = grid%sp32 ; jte = grid%ep32 ;   ! note that tile is entire patch
            kts = grid%sp33 ; kte = grid%ep33 ;   ! note that tile is entire patch

         CASE ( DATA_ORDER_XZY )
            ids = grid%sd31 ; ide = grid%ed31 ;
            kds = grid%sd32 ; kde = grid%ed32 ;
            jds = grid%sd33 ; jde = grid%ed33 ;

            ims = grid%sm31 ; ime = grid%em31 ;
            kms = grid%sm32 ; kme = grid%em32 ;
            jms = grid%sm33 ; jme = grid%em33 ;

            its = grid%sp31 ; ite = grid%ep31 ;   ! note that tile is entire patch
            kts = grid%sp32 ; kte = grid%ep32 ;   ! note that tile is entire patch
            jts = grid%sp33 ; jte = grid%ep33 ;   ! note that tile is entire patch

      END SELECT

            ktf=MIN(kte,kde-1)

      DO j=jts,jte
         DO i=its,ite
            grid%ph_2(i,1,j) = 0.
         END DO
      END DO

      !  Base state potential temperature and inverse density (alpha = 1/rho) from
      !  the half eta levels and the base-profile surface pressure.  Compute 1/rho
      !  from equation of state.  The potential temperature is a perturbation from t0.

         n_moist = num_moist
!         n_moist = num_moist-1

!       print *,'n_moist,PARAM_FIRST_SCALAR',n_moist,PARAM_FIRST_SCALAR

       DO j = jts, MIN(jte,jde-1)
         DO i = its, MIN(ite,ide-1)

               !  Integrate the hydrostatic equation (from the RHS of the bigstep vertical momentum
               !  equation) down from the top to get the pressure perturbation.  First get the pressure
               !  perturbation, moisture, and inverse density (total and perturbation) at the top-most level.
               kk = kte-1
               k=kk+1

               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
                 qtot = qtot + 0.5*(moist(i,kk,j,ispe)+moist(i,kk,j,ispe))
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2

               grid%p(i,kk,j) = - 0.5*((grid%c1f(k)*grid%Mu_2(i,j))+qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/grid%rdnw(kk)/qvf2
               if ( grid%use_theta_m .EQ. 1 ) then
                  qvf = 1.
               else
                  qvf = 1.+rvovrd*moist(i,kk,j,P_QV)
               endif
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf*         &
                      (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)

               !  Now, integrate down the column to compute the pressure perturbation, and diagnose the two
               !  inverse density fields (total and perturbation).

           DO kk=kte-2,1,-1
               k = kk + 1
               qtot = 0.
               DO ispe=PARAM_FIRST_SCALAR,n_moist
               qtot = qtot + 0.5*(  moist(i,kk  ,j,ispe) + moist(i,kk+1,j,ispe) )
               ENDDO
               qvf2 = 1./(1.+qtot)
               qvf1 = qtot*qvf2
               grid%p(i,kk,j) = grid%p(i,kk+1,j) - ((grid%c1f(k)*grid%Mu_2(i,j)) +       &
                               qvf1*(grid%c1f(k)*grid%Mub(i,j)+grid%c2f(k)))/qvf2/grid%rdn(kk+1)
               if ( grid%use_theta_m .EQ. 1 ) then
                  qvf = 1.
               else
                  qvf = 1.+rvovrd*moist(i,kk,j,P_QV)
               endif
               grid%alt(i,kk,j) = (r_d/p1000mb)*(grid%t_2(i,kk,j)+t0)*qvf* &
                           (((grid%p(i,kk,j)+grid%pb(i,kk,j))/p1000mb)**cvpm)
               grid%al(i,kk,j) = grid%alt(i,kk,j) - grid%alb(i,kk,j)
               grid%p_hyd(i,kk,j) = grid%p(i,kk,j) + grid%pb(i,kk,j)
           ENDDO

               !  This is the hydrostatic equation used in the model after the small timesteps.  In
               !  the model, grid%al (inverse density) is computed from the geopotential.

               IF (grid%hypsometric_opt == 1) THEN
                  DO kk  = 2,kte
                     k = kk - 1
                     grid%ph_2(i,kk,j) = grid%ph_2(i,kk-1,j) - &
                                   grid%dnw(kk-1) * ( ((grid%c1h(k)*grid%mub(i,j)+grid%c2h(k))+(grid%c1h(k)*grid%mu_2(i,j)))*grid%al(i,kk-1,j) &
                                 + (grid%c1h(k)*grid%mu_2(i,j))*grid%alb(i,kk-1,j) )
                     grid%ph0(i,kk,j) = grid%ph_2(i,kk,j) + grid%phb(i,kk,j)
                  END DO
               ELSE IF (grid%hypsometric_opt == 2) THEN
                ! Alternative hydrostatic eq.: dZ = -al*p*dLOG(p), where p is
                ! dry pressure.
                ! Note that al*p approximates Rd*T and dLOG(p) does z.
                ! Here T varies mostly linear with z, the first-order
                ! integration produces better result.

                  grid%ph_2(i,1,j) = grid%phb(i,1,j)
                  DO k = 2,kte
                     pfu = grid%c3f(k  )*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k  ) + grid%p_top
                     pfd = grid%c3f(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4f(k-1) + grid%p_top
                     phm = grid%c3h(k-1)*(grid%MUB(i,j)+grid%MU_2(i,j)) + grid%c4h(k-1) + grid%p_top
                     grid%ph_2(i,k,j) = grid%ph_2(i,k-1,j) + grid%alt(i,k-1,j)*phm*LOG(pfd/pfu)
                  END DO

                  DO k = 1,kte
                     grid%ph_2(i,k,j) = grid%ph_2(i,k,j) - grid%phb(i,k,j)
                  END DO

                  DO k = 1,kte
                     grid%ph0(i,k,j) = grid%ph_2(i,k,j) + grid%phb(i,k,j)
                  END DO

               END IF
! update surface pressure PSFC:
                  z0 = grid%ph0(i,1,j)/g
                  z1 = 0.5*(grid%ph0(i,1,j)+grid%ph0(i,2,j))/g
                  z2 = 0.5*(grid%ph0(i,2,j)+grid%ph0(i,3,j))/g
                  w1 = (z0 - z2)/(z1 - z2)
                  w2 = 1. - w1
                  grid%psfc(i,j) = w1*(grid%p(i,1,j)+grid%pb(i,1,j))+w2*(grid%p(i,2,j)+grid%pb(i,2,j))

         ENDDO  !i
        ENDDO  !j

      ips = its ; ipe = ite ; jps = jts ; jpe = jte ; kps = kts ; kpe = kte
#ifdef DM_PARALLEL
# include "HALO_EM_INIT_1.inc"
# include "HALO_EM_INIT_2.inc"
# include "HALO_EM_INIT_3.inc"
# include "HALO_EM_INIT_4.inc"
# include "HALO_EM_INIT_5.inc"
#endif
   END SUBROUTINE rebalance_dfi

#endif
!---------------------------------------------------------------------
