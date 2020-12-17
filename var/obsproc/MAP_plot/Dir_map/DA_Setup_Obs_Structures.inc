SUBROUTINE DA_Setup_Obs_Structures

!------------------------------------------------------------------------------
! PURPOSE: Define, allocate and read of observation structure.
!
! METHOD:  Define, allocate and read of observation structure.
!
! HISTORY: 01/20/2000 - Creation of F90 version.           Dale Barker
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   INTEGER, parameter           :: obs_unit = 99

   TRUELAT1=91.0   
   TRUELAT2=91.0

!-----------------------------------------------------------------------------
!   LOAD AND PROCESS OBSERVATIONS
!-----------------------------------------------------------------------------

   call DA_Read_Obs_Info (obs_unit)

!-----------------------------------------------------------------------------
!     [4.0] Allocate and initialise residual structure:
!------------------------------------------------------------------------------

   call DA_Read_Obs (obs_unit)

END SUBROUTINE DA_Setup_Obs_Structures
