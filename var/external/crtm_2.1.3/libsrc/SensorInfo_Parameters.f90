MODULE SensorInfo_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  ! ...none
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! Allowable polarisation type values and names
  PUBLIC :: N_POLARIZATION_TYPES   
  PUBLIC :: INVALID_POLARIZATION   
  PUBLIC :: UNPOLARIZED            
  PUBLIC :: INTENSITY              
  PUBLIC :: FIRST_STOKES_COMPONENT 
  PUBLIC :: SECOND_STOKES_COMPONENT
  PUBLIC :: THIRD_STOKES_COMPONENT 
  PUBLIC :: FOURTH_STOKES_COMPONENT
  PUBLIC :: VL_POLARIZATION        
  PUBLIC :: HL_POLARIZATION        
  PUBLIC :: plus45L_POLARIZATION   
  PUBLIC :: minus45L_POLARIZATION  
  PUBLIC :: VL_MIXED_POLARIZATION  
  PUBLIC :: HL_MIXED_POLARIZATION  
  PUBLIC :: RC_POLARIZATION        
  PUBLIC :: LC_POLARIZATION        
  PUBLIC :: POLARIZATION_TYPE_NAME


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: $'
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)

  ! The polarisation flags
  INTEGER, PARAMETER :: N_POLARIZATION_TYPES    = 12
  INTEGER, PARAMETER :: INVALID_POLARIZATION    = 0
  INTEGER, PARAMETER :: UNPOLARIZED             = 1
  INTEGER, PARAMETER :: INTENSITY               = UNPOLARIZED
  INTEGER, PARAMETER :: FIRST_STOKES_COMPONENT  = UNPOLARIZED
  INTEGER, PARAMETER :: SECOND_STOKES_COMPONENT = 2
  INTEGER, PARAMETER :: THIRD_STOKES_COMPONENT  = 3
  INTEGER, PARAMETER :: FOURTH_STOKES_COMPONENT = 4
  INTEGER, PARAMETER :: VL_POLARIZATION         = 5
  INTEGER, PARAMETER :: HL_POLARIZATION         = 6
  INTEGER, PARAMETER :: plus45L_POLARIZATION    = 7
  INTEGER, PARAMETER :: minus45L_POLARIZATION   = 8
  INTEGER, PARAMETER :: VL_MIXED_POLARIZATION   = 9
  INTEGER, PARAMETER :: HL_MIXED_POLARIZATION   = 10
  INTEGER, PARAMETER :: RC_POLARIZATION         = 11
  INTEGER, PARAMETER :: LC_POLARIZATION         = 12
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_POLARIZATION_TYPES ) :: &
    POLARIZATION_TYPE_NAME = (/ 'Invalid                                          ', &
                                'Unpolarized/Intensity/First Stokes component (I) ', &
                                'Second Stokes component (Q)                      ', &
                                'Third Stokes component (U)                       ', &
                                'Fourth Stokes component (V)                      ', &
                                'Vertical linear polarization                     ', &
                                'Horizontal linear polarization                   ', &
                                '+45deg. linear polarization                      ', &
                                '-45deg. linear polarization                      ', &
                                'Vertical polarization at nadir; mixed off nadir  ', &
                                'Horizontal polarization at nadir; mixed off nadir', &
                                'Right circular polarization                      ', &
                                'Left circular polarization                       ' /)


END MODULE SensorInfo_Parameters
