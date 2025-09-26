program da_wrfvar_esmf

!   use da_wrfvar_esmf_super

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !----------------------------------------------------------------------

   implicit none

   type(esmf_gridcomp) :: gcomp
   type(esmf_state)    :: importstate, exportstate
   type(esmf_clock)    :: clock
   type(esmf_vm)       :: vm   
   integer :: rc

   ! this call includes everything that must be done before esmf_initialize() 
   ! is called.  
   call init_modules(1)   ! phase 1 returns after mpi_init() (if it is called)

   call esmf_initialize( vm=vm, defaultcalendar=esmf_cal_gregorian, rc=rc )

   call da_wrfvar_init( gcomp, importstate, exportstate, clock, rc )

   call da_wrfvar_run( gcomp, importstate, exportstate, clock, rc )

   call da_wrfvar_finalize( gcomp, importstate, exportstate, clock, rc )

   call esmf_finalize( rc=rc )

end program da_wrfvar_esmf

