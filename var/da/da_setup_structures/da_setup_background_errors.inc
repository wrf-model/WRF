subroutine da_setup_background_errors(grid, be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors.
   !          Wrapper subroutine.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)   :: grid
!  type (xb_type), intent(in)  :: xb       ! First guess structure.
   type (be_type), intent(out) :: be       ! Back. errors structure.
#if (WRF_CHEM == 1)
   integer :: ic
#endif

   integer :: num_cv_3d_basic, num_cv_3d_extra, num_cv_2d

   if (trace_use) call da_trace_entry("da_setup_background_errors")

!  Hybrid parameters:
   be % ne = ensdim_alpha                          ! Size of ensemble.

   if (be % ne > 0) then     ! Calculation to preserve total variance.
      if ( je_factor > alpha_std_dev**2 ) then
         jb_factor   = je_factor / ( je_factor - alpha_std_dev**2 )
      else
         jb_factor   = -999.
         write(6,*) 'Full ensemble mode: deactivating Jb control variable'
            max_vert_var1 = 0
            max_vert_var2 = 0
            max_vert_var3 = 0
            max_vert_var4 = 0
            max_vert_var5 = 0
!#ifdef CLOUD_CV
            max_vert_var6 = 0
            max_vert_var7 = 0
            max_vert_var8 = 0
            max_vert_var9 = 0
            max_vert_var10 = 0
            max_vert_var11 = 0
!#endif
!#ifdef CHEM_CV
            max_vert_var12 = 0
!#endif
      end if
      sigma_alpha = alpha_std_dev
      write(6,'(a,4f15.5)')' jb_factor, je_factor, alpha_std_dev, sigma_alpha = ', &
                    jb_factor, je_factor, alpha_std_dev, sigma_alpha
   else
      jb_factor = 1.0
   end if

   num_cv_3d_basic = 4
   num_cv_3d_extra = 0
   num_cv_2d       = 1
   if ( cv_options /= 3 ) then
      if ( cloud_cv_options >= 2 ) then
         num_cv_3d_extra = 5
      end if
      if ( use_cv_w ) then
         num_cv_3d_extra = num_cv_3d_extra + 1
      end if
   end if

   ! + 2 is for alpha_cv and num_ens
   be % ncv_mz  = num_cv_3d_basic + num_cv_3d_extra + num_cv_2d + 2
   allocate ( be%cv_mz(1:be%ncv_mz) )
   be%cv_mz(:) = 0 ! initialize

   ! initialize
   be % v1 % mz = 0
   be % v2 % mz = 0
   be % v3 % mz = 0
   be % v4 % mz = 0
   be % v5 % mz = 0
   be % v6 % mz = 0
   be % v7 % mz = 0
   be % v8 % mz = 0
   be % v9 % mz = 0
   be % v10% mz = 0
   be % v11% mz = 0

#if (WRF_CHEM == 1)
   num_cv_3d_basic = 0
   num_cv_3d_extra = 0
   num_cv_2d = 0

!   if (num_scalechem >= PARAM_FIRST_SCALAR) then
!      num_cv_3d_basic = num_cv_3d_basic + 1
!      allocate (be%alpha_chem (num_scalechem) )
!      do ic = 1, num_scalechem
!         be % alpha_chem(ic) % mz = 0
!      end do
!      allocate (be%cv%sizechem (num_scalechem) )
!   end if

   if (num_chem >= PARAM_FIRST_SCALAR) then
!      allocate (be%v12 (num_chem) )
!      do ic = 1, num_chem
!         be % v12(ic) % mz = 0
!      end do
      allocate (be%cv%sizechemic (num_chem) )
   end if

   if (num_scaleant >= PARAM_FIRST_SCALAR) then
      num_cv_3d_extra = num_cv_3d_extra + 1
      allocate (be%alpha_ant_chem (num_scaleant) )
      do ic = 1, num_scaleant
         be % alpha_ant_chem(ic) % mz = 0
         num_cv_3d_extra = num_cv_3d_extra + 1
      end do
      allocate (be%cv%sizeant (num_scaleant) )
   end if

   if (num_scalebb >= PARAM_FIRST_SCALAR) then
      allocate (be%alpha_bb_chem (num_scalebb) )
      do ic = 1, num_scalebb
         be % alpha_bb_chem(ic) % mz = 0
      end do
      allocate (be%cv%sizebb (num_scalebb) )
   end if

!   if (num_scalebio >= PARAM_FIRST_SCALAR) then
!      num_cv_3d_extra = num_cv_3d_extra + 1
!      allocate (be%alpha_bio_chem (num_scalebio) )
!      do ic = 1, num_scalebio
!         be % alpha_bio_chem(ic) % mz = 0
!      end do
!      allocate (be%cv%sizebio (num_scalebb) )
!   end if

   be % ncv_mz_chem  = num_cv_3d_basic + num_cv_3d_extra + num_cv_2d
   allocate ( be%cv_mz_chem(1:be%ncv_mz_chem) )
   be%cv_mz_chem(:) = 0 ! initialize

#endif

   if (global) then
      call da_setup_be_global(be)
   else if(fg_format == fg_format_wrf_arw_regional) then    
      if ( (cv_options == 5) .or. (cv_options == 6) .or. (cv_options == 7) ) then
         call da_setup_be_regional (grid%xb, be, grid)
      else if(cv_options == 3 ) then
         call da_setup_be_ncep_gfs (grid, be)
#if (WRF_CHEM == 1)
      else if(cv_options == 10 ) then
            call da_setup_be_chem (grid, be)
!      else if(cv_options == 11 ) then
!            call da_setup_be_chem_conc (grid%xb, be, grid)
#endif
      else 
         write(unit=message(1),fmt='(A,I4)') &
             'Invalid CV option chosen:  cv_options = ', cv_options
         call da_error(__FILE__,__LINE__,message(1:1))
      endif
   else if(fg_format == fg_format_wrf_nmm_regional ) then
!rizvi TBD            call da_setup_be_regional (grid%xb, be)
            call da_setup_be_nmm_regional (grid%xb, be)
   end if

   ! initialize
   be % cv % size1   = 0
   be % cv % size2   = 0
   be % cv % size3   = 0
   be % cv % size4   = 0
   be % cv % size5   = 0
   be % cv % size6   = 0
   be % cv % size7   = 0
   be % cv % size8   = 0
   be % cv % size9   = 0
   be % cv % size10  = 0
   be % cv % size11i = 0

   call da_setup_cv (be)

   if (trace_use) call da_trace_exit("da_setup_background_errors")

end subroutine da_setup_background_errors


