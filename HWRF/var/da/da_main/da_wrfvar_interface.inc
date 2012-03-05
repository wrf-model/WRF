subroutine da_wrfvar_interface (grid, config_flags) 

   !------------------------------------------------------------------------
   ! Purpose: TBD
   !------------------------------------------------------------------------

   implicit none

   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   ! integer :: idum1, idum2

   call da_trace_entry("da_wrfvar_interface")

   ! call mediation_setup_step (grid , config_flags , 1 , 1 , 1)

   call set_scalar_indices_from_config (grid%id , idum1 , idum2)

   call model_to_grid_config_rec (grid%id , model_config_rec , config_flags)

   grid%itimestep = 1

   call da_solve (grid , config_flags)

   call da_trace_exit("da_wrfvar_interface")

end subroutine da_wrfvar_interface


