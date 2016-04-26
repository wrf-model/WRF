!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
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

Module module_gw_gw2d_data
   IMPLICIT NONE
   INTEGER, PARAMETER :: max_domain=5

#include "gw_field_include.inc"
      type (gw_field) :: gw2d(max_domain)
      save gw2d

end module module_gw_gw2d_data
