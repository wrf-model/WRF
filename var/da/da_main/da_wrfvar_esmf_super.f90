module da_wrfvar_esmf_super

   !-----------------------------------------------------------------------
   ! Purpose: defines wrfvar_init(), wrfvar_run(), and wrfvar_finalize() 
   ! routines for use by ESMF superstructure.  
   ! WRFVAR can be built with either ESMF_Mod (from an installed ESMF library) 
   ! or with built-in wrf_esmf_mod.  The choice is made at configure time 
   ! via cpp token WRF_ESMF_MOD.  
   ! Note that WRF_ESMF_MOD is used by module_domain.  
   !------------------------------------------------------------------------

!   use module_machine
!   use module_domain
!   use module_integrate
!   use module_driver_constants
!   use module_configure

!   use module_timing

#ifdef DM_PARALLEL
!   use module_dm
#endif

!   use da_wrfvar_io

!   use da_control
!   use da_define_structures
!   use da_setup_structures
!   use da_test
!   use da_minimisation
!   use da_wrf_interfaces

   implicit none

   real    :: time

   integer :: loop, levels_to_process

   type (domain) , pointer :: keep_grid, grid_ptr, null_domain
   type (grid_config_rec_type), save :: config_flags
   integer                 :: number_at_same_level
   integer                 :: time_step_begin_restart

   integer :: domain_id , fid , oid , idum1 , idum2

#ifdef DM_PARALLEL
   integer                 :: nbytes
   integer, parameter      :: configbuflen = 4* CONFIG_BUF_LEN
   integer                 :: configbuf( configbuflen )
#endif

   character (LEN=80)      :: rstname
   character (LEN=80)      :: message

contains

#include "da_esmf_init.inc"
#include "da_esmf_run.inc"
#include "da_esmf_finalize.inc"
#include "da_wrfvar_interface.inc"

end module da_wrfvar_esmf_super

