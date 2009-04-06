!
!*** Jan Mandel August-October 2007
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu
!
! This module contains general purpose utilities and WRF wrappers because I want the
! model to be able to run standalone. No physics here.
! Some are dependent on WRF indexing scheme. Some violate WRF conventions but these
! are not called from the WRF dependent code. Some are not called at all.
! 

module module_fr_sfire_util

! various method selection parameters
! 1. add the parameter and its static default here
! 2. add copy from config_flags in module_fr_sfire_driver%%set_flags
! 3. (optional) add a line in Registry.EM to define the variable and set default value


integer ::              &
 fire_print_msg=1,      &  ! print SFIRE progress 
 fire_print_file=1,     &  ! write many files by write_array_m; compile with DEBUG_OUT, do not run in parallel
 fuel_left_method=1,    &  ! 1=simple, 2=exact in linear case
 fuel_left_irl=2,       &  ! refinement for fuel calculation, must be even
 fuel_left_jrl=2,       &
 boundary_guard=-1,     &  ! crash if fire gets this many cells to domain boundary, -1=off
 fire_grows_only=1,     &  ! fire can spread out only (level set functions may not increase)
 fire_upwinding=3,      &  ! upwind normal spread: 1=standard, 2=godunov, 3=eno, 4=sethian 
 fire_upwind_split=0,   &  ! 1=upwind advection separately from normal direction spread
 fire_test_steps=0,     &  ! 0=no fire, 1=normal, >1 = do specified number of steps and terminate (testing only)
 fire_topo_from_atm=1      ! 0 = expect ZSF set correctly on entry, 1 = populate by interploating from atmosphere
 
 


real ::                 &
 fire_atm_feedback=1. , &  ! 1 = normal, 0. = one way coupling atmosphere -> fire only
 fire_back_weight=0.5,  &  ! RK parameter, 0 = Euler method, 0.5 = Heun, 1 = fake backward Euler
 fire_viscosity=0.4,    &  ! artificial viscosity
 fire_lfn_ext_up=1.        ! 0.=extend level set function at boundary by reflection, 1.=always up

contains

!
!****************
!
subroutine crash(s)
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='SFIRE:'//s
call wrf_error_fatal(msg)
end subroutine crash


end module module_fr_sfire_util
