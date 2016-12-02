!
! NLTE_Parameters
!
! Module defining parameters used in the various NLTE applications.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, 08-05-2010
!                       yong.han@noaa.gov
!
!       Refactored by:  Paul van Delst, 18-Jan-2011
!                       paul.vandelst@noaa.gov
!

MODULE NLTE_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Public parameters
  PUBLIC :: N_NLTE_LAYERS
  PUBLIC :: N_NLTE_PREDICTORS


  PUBLIC :: N_PLay, Pt
  PUBLIC :: N_ENSEMBLE_TEMPERATURES
  PUBLIC :: MIN_TM_INDEX  
  PUBLIC :: MAX_TM_INDEX  
  PUBLIC :: MEAN_TM_INDEX 
  PUBLIC :: NLTE_WMO_SENSOR_ID
  PUBLIC :: N_NLTE_SENSOR_ID

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NLTE_Parameters.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'

  ! Pressure levels for computing mean temperatures in the two layers
  INTEGER,  PARAMETER :: N_NLTE_LAYERS = 2
  INTEGER,  PARAMETER :: N_PLay = N_NLTE_LAYERS
  REAL(fp), PARAMETER :: Pt(2, N_PLay) = &
                                 RESHAPE( (/ 0.005_fp, 0.2_fp, &
                                             0.2_fp,  52.0_fp /), &
                                           (/2, N_PLay/)) 

  ! Number of predictors including the constant term
  INTEGER, PARAMETER  :: N_NLTE_PREDICTORS = N_NLTE_LAYERS + 1

  ! number of mean temperatures (Min Tm, Max Tm and Mean Tm)
  INTEGER, PARAMETER  :: N_ENSEMBLE_TEMPERATURES = 3
  INTEGER, PARAMETER  :: MIN_TM_INDEX  = 1
  INTEGER, PARAMETER  :: MAX_TM_INDEX  = 2
  INTEGER, PARAMETER  :: MEAN_TM_INDEX = 3

  ! Sensor id's for NLTE "sensitive" sensors
  INTEGER, PARAMETER  :: N_NLTE_SENSOR_ID   = 3
  INTEGER, PARAMETER  :: NLTE_WMO_SENSOR_ID(N_NLTE_SENSOR_ID) = (/&
                                             420, &  ! AIRS
                                             221, &  ! IASI
                                             620 /)  ! CrIS
END MODULE NLTE_Parameters
