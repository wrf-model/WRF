subroutine da_esmf_run( gcomp, importState, exportState, clock, rc )

   !-----------------------------------------------------------------------
   ! Purpose: WRFVAR run routine.
   !
   !     The arguments are:
   !       gcomp           Component
   !       importState     Importstate
   !       exportState     Exportstate
   !       clock           External clock
   !       rc              Return code; equals ESMF_SUCCESS if there are no
   !                       errors, otherwise ESMF_FAILURE.
   !-----------------------------------------------------------------------

   implicit none

   type(ESMF_GridComp), intent(inout) :: gcomp
   type(ESMF_State),    intent(inout) :: importState, exportState
   type(ESMF_Clock),    intent(inout) :: clock
   integer,             intent(out)   :: rc

   call da_wrfvar_interface ( head_grid, config_flags )

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_esmf_run


