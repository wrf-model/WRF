!
! CRTM_Module
!
! Main container module for the CRTM.
!

MODULE CRTM_Module

  ! Module information
  ! ------------------
  ! Support modules
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  ! Structure definition modules
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_Geometry_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_RTSolution_Define
  USE CRTM_Options_Define
  USE CRTM_AncillaryInput_Define
  USE CRTM_IRlandCoeff          , ONLY: CRTM_IRlandCoeff_Classification

  ! Parameter definition module
  USE CRTM_Parameters
  USE SensorInfo_Parameters

  ! The main function modules
  USE CRTM_LifeCycle
  USE CRTM_Forward_Module
  USE CRTM_Tangent_Linear_Module
  USE CRTM_Adjoint_Module
  USE CRTM_K_Matrix_Module
  ! ...The aerosol optical depth tool
  USE CRTM_AOD_Module


  ! Visibility
  ! ----------
  PUBLIC


  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: CRTM_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PRIVATE, PARAMETER :: CRTM_VERSION_ID = &
  'REL-2.1.3'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Version
!
! PURPOSE:
!       Subroutine to the CRTM version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Version( version )
!
! OUTPUTS:
!       version:       Character string identifying the CRTM release version.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Version( version )
    CHARACTER(*), INTENT(OUT) :: version
    version = CRTM_VERSION_ID
  END SUBROUTINE CRTM_Version

END MODULE CRTM_Module
