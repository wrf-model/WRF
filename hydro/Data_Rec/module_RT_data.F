!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
!
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
!
!  User controllable options: <if applicable>

Module module_RT_data
   use overland_data
   use module_subsurface_data
   use module_subsurface_static_data
   use module_subsurface_input
   use module_subsurface_output
   use module_reservoir, only: reservoir_container
   use iso_fortran_env, only: int64
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

! define Routing data
#include "rt_include.inc"
   TYPE ( RT_FIELD ), DIMENSION (max_domain) :: RT_DOMAIN
   save RT_DOMAIN
   integer :: cur_did
end module module_RT_data
