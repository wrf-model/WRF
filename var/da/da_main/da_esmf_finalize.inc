subroutine da_wrfvar_finalize( gcomp, importState, exportState, clock, rc )

   !-----------------------------------------------------------------------
   ! Purpose: WRFVAR finalize routine.
   !
   !     The arguments are:
   !       gcomp           Component
   !       importState     Importstate
   !       exportState     Exportstate
   !       clock           External clock
   !       rc              Return code; equals ESMF_SUCCESS if there are no
   !                       errors, otherwise ESMF_FAILURE
   !-----------------------------------------------------------------------

   implicit none

   type(ESMF_GridComp), intent(inout) :: gcomp
   type(ESMF_State),    intent(inout) :: importState, exportState
   type(ESMF_Clock),    intent(inout) :: clock
   integer,             intent(out)   :: rc

   ! output wrfvar analysis

   if ((config_flags%real_data_init_type == 1) .OR. &
       (config_flags%real_data_init_type == 3)) then
      call da_med_initialdata_output( head_grid , config_flags )

      call med_shutdown_io ( head_grid , config_flags )
   end if

   call da_message((/"wrfvar: SUCCESS COMPLETE WRFVAR"/))
   call wrf_shutdown

   ! return success status
   rc = ESMF_SUCCESS

end subroutine da_wrfvar_finalize


