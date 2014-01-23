subroutine da_setup_background_errors(grid, be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors.
   !          Wrapper subroutine.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)   :: grid
!  type (xb_type), intent(in)  :: xb       ! First guess structure.
   type (be_type), intent(out) :: be       ! Back. errors structure.

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
#ifdef CLOUD_CV
            max_vert_var6 = 0
            max_vert_var7 = 0
            max_vert_var8 = 0
            max_vert_var9 = 0
#endif
      end if
      sigma_alpha = alpha_std_dev
      write(6,'(a,4f15.5)')' jb_factor, je_factor, alpha_std_dev, sigma_alpha = ', &
                    jb_factor, je_factor, alpha_std_dev, sigma_alpha
   else
      jb_factor = 1.0
   end if

   be % v1 % mz = 0
   be % v2 % mz = 0
   be % v3 % mz = 0
   be % v4 % mz = 0
   be % v5 % mz = 0
#ifdef CLOUD_CV
   be % v6 % mz = 0
   be % v7 % mz = 0
   be % v8 % mz = 0
   be % v9 % mz = 0
#endif
   if (global) then
      call da_setup_be_global(be)
   else if(fg_format == fg_format_wrf_arw_regional) then    
      if (cv_options == 5 .or. cv_options == 6) then
            call da_setup_be_regional (grid%xb, be, grid)
      else if(cv_options == 3 ) then
            call da_setup_be_ncep_gfs (grid, be)
      endif
   else if(fg_format == fg_format_wrf_nmm_regional ) then
!rizvi TBD            call da_setup_be_regional (grid%xb, be)
            call da_setup_be_nmm_regional (grid%xb, be)
   end if

   call da_setup_cv (be)

   if (trace_use) call da_trace_exit("da_setup_background_errors")

end subroutine da_setup_background_errors


