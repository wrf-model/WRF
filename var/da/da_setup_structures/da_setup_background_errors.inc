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
   jb_factor = 1.0
   sigma_alpha = 0.0

   if ( ensdim_alpha > 0 ) then ! Calculation to preserve total variance.
      if ( je_factor > alpha_std_dev*alpha_std_dev ) then
         jb_factor = je_factor / ( je_factor - alpha_std_dev*alpha_std_dev )
         sigma_alpha = alpha_std_dev
      end if
      write(6,'(a,4f15.5)')' jb_factor, je_factor, alpha_std_dev, sigma_alpha = ', &
                    jb_factor, je_factor, alpha_std_dev, sigma_alpha
   end if

   be % v1 % mz = 0
   be % v2 % mz = 0
   be % v3 % mz = 0
   be % v4 % mz = 0
   be % v5 % mz = 0

   if (global) then
      call da_setup_be_global(be)
   else if(fg_format == fg_format_wrf_arw_regional) then    
      if (cv_options == 5) then
            call da_setup_be_regional (grid%xb, be)
      else if(cv_options == 3 ) then
            call da_setup_be_ncep_gfs (grid, be)
      endif
   else if(fg_format == fg_format_wrf_nmm_regional ) then
            call da_setup_be_nmm_regional (grid%xb, be)
   end if

   call da_setup_cv (be)

   if (trace_use) call da_trace_exit("da_setup_background_errors")

end subroutine da_setup_background_errors


