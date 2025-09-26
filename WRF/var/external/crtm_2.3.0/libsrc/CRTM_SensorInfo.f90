!
! CRTM_SensorInfo
!
! Module of sensor information parameters definitions for the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jun-2005
!                       paul.vandelst@ssec.wisc.edu

MODULE CRTM_SensorInfo

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Get_SensorAttributes


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SensorInfo.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'
  ! WMO SENSOR codes from COMMON CODE TABLE C-8
  INTEGER, PUBLIC, PARAMETER :: WMO_HIRS2    = 605
  INTEGER, PUBLIC, PARAMETER :: WMO_MSU      = 623
  INTEGER, PUBLIC, PARAMETER :: WMO_AVHRR2   = 590
  INTEGER, PUBLIC, PARAMETER :: WMO_HIRS3    = 606
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSUA    = 570
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSUB    = 574
  INTEGER, PUBLIC, PARAMETER :: WMO_AVHRR3   = 591
  INTEGER, PUBLIC, PARAMETER :: WMO_MHS      = 203
  INTEGER, PUBLIC, PARAMETER :: WMO_VAS      = 630
  INTEGER, PUBLIC, PARAMETER :: WMO_SOUNDER  = 626
  INTEGER, PUBLIC, PARAMETER :: WMO_IMAGER   = 615
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMI     = 905
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMT1    = 906
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMT2    = 907
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMIS    = 908
  INTEGER, PUBLIC, PARAMETER :: WMO_MODIS    = 389
  INTEGER, PUBLIC, PARAMETER :: WMO_HSB      = 246
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSRE    = 345
  INTEGER, PUBLIC, PARAMETER :: WMO_AIRS     = 420
  INTEGER, PUBLIC, PARAMETER :: WMO_VISSR    = 489
  INTEGER, PUBLIC, PARAMETER :: WMO_MVIRI    = 205
  INTEGER, PUBLIC, PARAMETER :: WMO_SEVIRI   = 207
  INTEGER, PUBLIC, PARAMETER :: WMO_ABI      = INVALID_WMO_SENSOR_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_WINDSAT  = INVALID_WMO_SENSOR_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_ATMS     = 621
  INTEGER, PUBLIC, PARAMETER :: WMO_IASI     = 221

  ! WMO SATELLITE codes from COMMON CODE TABLE C-5
  INTEGER, PUBLIC, PARAMETER :: WMO_TIROSN      = 708
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA05      = WMO_TIROSN
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA06      = 706
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA07      = 707
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA08      = 200
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA09      = 201
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA10      = 202
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA11      = 203
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA12      = 204
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA14      = 205
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA15      = 206
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA16      = 207
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA17      = 208
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA18      = 209
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA19      = 223
  INTEGER, PUBLIC, PARAMETER :: WMO_METOPA      =   4
  INTEGER, PUBLIC, PARAMETER :: WMO_METOPB      =   3
  INTEGER, PUBLIC, PARAMETER :: WMO_METOPC      =   5
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES04      = 734
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES05      = 735
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES06      = 250
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES07      = 251
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES08      = 252
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES09      = 253
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES10      = 254
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES11      = 255
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES12      = 256
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES13      = 257
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP13      = 246
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP14      = 247
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP15      = 248
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP16      = 249
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP17      = 285
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP18      = 246
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP19      = INVALID_WMO_SATELLITE_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP20      = INVALID_WMO_SATELLITE_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_TERRA       = 783
  INTEGER, PUBLIC, PARAMETER :: WMO_AQUA        = 784
  INTEGER, PUBLIC, PARAMETER :: WMO_GMS5        = 152
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT03  = 50
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT04  = 51
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT05  = 52
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT06  = 53
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT07  = 54
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT08  = 55
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT09  = 56
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT10  = 57
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT11  = 70
  INTEGER, PUBLIC, PARAMETER :: WMO_GOESR       = INVALID_WMO_SATELLITE_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_CORIOLIS    = INVALID_WMO_SATELLITE_ID
  INTEGER, PUBLIC, PARAMETER :: WMO_NPOESSC1    = 224

CONTAINS

  FUNCTION CRTM_Get_SensorAttributes( Sensor_Id       , &  ! Input
                                      nChannels       , &  ! Optional output
                                      nFOVs           , &  ! Optional output       
                                      Detector        , &  ! Optional output    
                                      WMO_Sensor_Id   , &  ! Optional output
                                      WMO_Satellite_Id, &  ! Optional output
                                      SensorName      , &  ! Optional output
                                      SatelliteName   , &  ! Optional output
                                      RCS_Id          , &  ! Version control
                                      Message_Log     ) &  ! Error messaging 
                                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Sensor_Id
    INTEGER,      OPTIONAL, INTENT(OUT) :: nChannels
    INTEGER,      OPTIONAL, INTENT(OUT) :: nFOVs
    INTEGER,      OPTIONAL, INTENT(OUT) :: Detector
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: SensorName
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: SatelliteName
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='CRTM_Get_SensorAttributes'
    INTEGER,      PARAMETER :: SL=20
    ! Local variables
    INTEGER       :: local_nChannels
    INTEGER       :: local_nFOVs
    INTEGER       :: local_Detector
    INTEGER       :: local_WMO_Sensor_Id   
    INTEGER       :: local_WMO_Satellite_Id
    CHARACTER(SL) :: local_SensorName
    CHARACTER(SL) :: local_SatelliteName
    
    ! Set up
    Error_Status  = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_Id
    ! Defaults
    local_nChannels        =  0
    local_nFOVs            =  0
    local_Detector         = -1
    local_WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    local_WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    local_SensorName       = 'NONE'
    local_SatelliteName    = 'NONE'
    
    ! Search for sensor attributes
    ! This humungous CASE construct will have to be replaced, but for now it'll do.
    ! The code below was created from the Create_SensorInfo_Code.f90 program
    SELECT CASE (TRIM(ADJUSTL(Sensor_Id)))
      CASE('hirs2_n05')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 708
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'TIROS-N'
      CASE('msu_n05')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 708
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'TIROS-N'
      CASE('ssu_n05')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 708
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'TIROS-N'
      CASE('avhrr2_n05')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 708
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'TIROS-N'
      CASE('hirs2_n06')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 706
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-06'
      CASE('msu_n06')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 706
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-06'
      CASE('ssu_n06')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 706
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-06'
      CASE('avhrr2_n06')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 706
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-06'
      CASE('hirs2_n07')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 707
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-07'
      CASE('msu_n07')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 707
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-07'
      CASE('ssu_n07')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 707
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-07'
      CASE('avhrr2_n07')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 707
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-07'
      CASE('hirs2_n08')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 200
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-08'
      CASE('msu_n08')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 200
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-08'
      CASE('ssu_n08')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 200
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-08'
      CASE('avhrr2_n08')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 200
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-08'
      CASE('hirs2_n09')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 201
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-09'
      CASE('msu_n09')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 201
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-09'
      CASE('ssu_n09')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 201
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-09'
      CASE('avhrr2_n09')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 201
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-09'
      CASE('hirs2_n10')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 202
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-10'
      CASE('msu_n10')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 202
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-10'
      CASE('avhrr2_n10')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 202
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-10'
      CASE('hirs2_n11')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 203
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-11'
      CASE('msu_n11')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 203
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-11'
      CASE('ssu_n11')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 203
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-11'
      CASE('avhrr2_n11')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 203
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-11'
      CASE('hirs2_n12')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 204
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-12'
      CASE('msu_n12')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 204
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-12'
      CASE('avhrr2_n12')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 204
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-12'
      CASE('hirs2_n14')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 605
        local_WMO_Satellite_Id = 205
        local_SensorName       = 'HIRS/2'
        local_SatelliteName    = 'NOAA-14'
      CASE('msu_n14')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 623
        local_WMO_Satellite_Id = 205
        local_SensorName       = 'MSU'
        local_SatelliteName    = 'NOAA-14'
      CASE('ssu_n14')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 627
        local_WMO_Satellite_Id = 205
        local_SensorName       = 'SSU'
        local_SatelliteName    = 'NOAA-14'
      CASE('avhrr2_n14')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 590
        local_WMO_Satellite_Id = 205
        local_SensorName       = 'AVHRR/2'
        local_SatelliteName    = 'NOAA-14'
      CASE('hirs3_n15')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 606
        local_WMO_Satellite_Id = 206
        local_SensorName       = 'HIRS/3'
        local_SatelliteName    = 'NOAA-15'
      CASE('amsua_n15')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 206
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'NOAA-15'
      CASE('amsub_n15')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 574
        local_WMO_Satellite_Id = 206
        local_SensorName       = 'AMSU-B'
        local_SatelliteName    = 'NOAA-15'
      CASE('avhrr3_n15')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 591
        local_WMO_Satellite_Id = 206
        local_SensorName       = 'AVHRR/3'
        local_SatelliteName    = 'NOAA-15'
      CASE('hirs3_n16')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 606
        local_WMO_Satellite_Id = 207
        local_SensorName       = 'HIRS/3'
        local_SatelliteName    = 'NOAA-16'
      CASE('amsua_n16')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 207
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'NOAA-16'
      CASE('amsub_n16')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 574
        local_WMO_Satellite_Id = 207
        local_SensorName       = 'AMSU-B'
        local_SatelliteName    = 'NOAA-16'
      CASE('avhrr3_n16')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 591
        local_WMO_Satellite_Id = 207
        local_SensorName       = 'AVHRR/3'
        local_SatelliteName    = 'NOAA-16'
      CASE('hirs3_n17')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 606
        local_WMO_Satellite_Id = 208
        local_SensorName       = 'HIRS/3'
        local_SatelliteName    = 'NOAA-17'
      CASE('amsua_n17')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 208
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'NOAA-17'
      CASE('amsub_n17')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 574
        local_WMO_Satellite_Id = 208
        local_SensorName       = 'AMSU-B'
        local_SatelliteName    = 'NOAA-17'
      CASE('avhrr3_n17')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 591
        local_WMO_Satellite_Id = 208
        local_SensorName       = 'AVHRR/3'
        local_SatelliteName    = 'NOAA-17'
      CASE('hirs4_n18')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 607
        local_WMO_Satellite_Id = 209
        local_SensorName       = 'HIRS/4'
        local_SatelliteName    = 'NOAA-18'
      CASE('amsua_n18')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 209
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'NOAA-18'
      CASE('mhs_n18')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 203
        local_WMO_Satellite_Id = 209
        local_SensorName       = 'MHS'
        local_SatelliteName    = 'NOAA-18'
      CASE('avhrr3_n18')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 591
        local_WMO_Satellite_Id = 209
        local_SensorName       = 'AVHRR/3'
        local_SatelliteName    = 'NOAA-18'
      CASE('amsua_n19')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 210
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'NOAA-19'
      CASE('mhs_n19')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 203
        local_WMO_Satellite_Id = 210
        local_SensorName       = 'MHS'
        local_SatelliteName    = 'NOAA-19'
      CASE('hirs4_metop-a')
        local_nChannels        = 19
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 607
        local_WMO_Satellite_Id = 4
        local_SensorName       = 'HIRS/4'
        local_SatelliteName    = 'MetOp-A'
      CASE('avhrr3_metop-a')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 591
        local_WMO_Satellite_Id = 4
        local_SensorName       = 'AVHRR/3'
        local_SatelliteName    = 'MetOp-A'
      CASE('amsua_metop-a')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 4
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'MetOp-A'
      CASE('mhs_metop-a')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 203
        local_WMO_Satellite_Id = 4
        local_SensorName       = 'MHS'
        local_SatelliteName    = 'MetOp-A'
      CASE('amsua_metop-b')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 3
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'MetOp-B'
      CASE('mhs_metop-b')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 203
        local_WMO_Satellite_Id = 3
        local_SensorName       = 'MHS'
        local_SatelliteName    = 'MetOp-B'
      CASE('amsua_metop-c')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 5
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'MetOp-C'
      CASE('mhs_metop-c')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 203
        local_WMO_Satellite_Id = 5
        local_SensorName       = 'MHS'
        local_SatelliteName    = 'MetOp-C'
      CASE('vas_g04')
        local_nChannels        = 12
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 630
        local_WMO_Satellite_Id = 734
        local_SensorName       = 'VAS'
        local_SatelliteName    = 'GOES-04'
      CASE('vas_g05')
        local_nChannels        = 12
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 630
        local_WMO_Satellite_Id = 735
        local_SensorName       = 'VAS'
        local_SatelliteName    = 'GOES-05'
      CASE('vas_g06')
        local_nChannels        = 12
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 630
        local_WMO_Satellite_Id = 250
        local_SensorName       = 'VAS'
        local_SatelliteName    = 'GOES-06'
      CASE('vas_g07')
        local_nChannels        = 12
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 630
        local_WMO_Satellite_Id = 251
        local_SensorName       = 'VAS'
        local_SatelliteName    = 'GOES-07'
      CASE('sndr_g08')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 252
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-08'
      CASE('imgr_g08')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 252
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-08'
      CASE('sndr_g09')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 253
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-09'
      CASE('imgr_g09')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 253
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-09'
      CASE('sndr_g10')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-10'
      CASE('sndrD1_g10')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-10'
      CASE('sndrD2_g10')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-10'
      CASE('sndrD3_g10')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-10'
      CASE('sndrD4_g10')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-10'
      CASE('imgr_g10')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 254
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-10'
      CASE('sndr_g11')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-11'
      CASE('sndrD1_g11')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-11'
      CASE('sndrD2_g11')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-11'
      CASE('sndrD3_g11')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-11'
      CASE('sndrD4_g11')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-11'
      CASE('imgr_g11')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 255
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-11'
      CASE('sndr_g12')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-12'
      CASE('sndrD1_g12')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-12'
      CASE('sndrD2_g12')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-12'
      CASE('sndrD3_g12')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-12'
      CASE('sndrD4_g12')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-12'
      CASE('imgr_g12')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 256
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-12'
      CASE('sndr_g13')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-13'
      CASE('sndrD1_g13')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-13'
      CASE('sndrD2_g13')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-13'
      CASE('sndrD3_g13')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-13'
      CASE('sndrD4_g13')
        local_nChannels        = 18
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 626
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'SOUNDER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgr_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrS1_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrS2_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrD1S1_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrD1S2_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrD2S1_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('imgrD2S2_g13')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 615
        local_WMO_Satellite_Id = 257
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'GOES-13'
      CASE('abi_gr')
        local_nChannels        = 10
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = -1
        local_WMO_Satellite_Id = -1
        local_SensorName       = 'ABI'
        local_SatelliteName    = 'GOES-R'
      CASE('imgr_mt1r')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 294
        local_WMO_Satellite_Id = 171
        local_SensorName       = 'IMAGER'
        local_SatelliteName    = 'MTSAT-1R'
      CASE('ssmi_f13')
        local_nChannels        = 7
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 905
        local_WMO_Satellite_Id = 246
        local_SensorName       = 'SSM/I'
        local_SatelliteName    = 'DMSP-13'
      CASE('ssmi_f14')
        local_nChannels        = 7
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 905
        local_WMO_Satellite_Id = 247
        local_SensorName       = 'SSM/I'
        local_SatelliteName    = 'DMSP-14'
      CASE('ssmi_f15')
        local_nChannels        = 7
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 905
        local_WMO_Satellite_Id = 248
        local_SensorName       = 'SSM/I'
        local_SatelliteName    = 'DMSP-15'
      CASE('ssmt1_f13')
        local_nChannels        = 7
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 906
        local_WMO_Satellite_Id = 246
        local_SensorName       = 'SSM/T-1'
        local_SatelliteName    = 'DMSP-13'
      CASE('ssmt1_f15')
        local_nChannels        = 7
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 906
        local_WMO_Satellite_Id = 248
        local_SensorName       = 'SSM/T-1'
        local_SatelliteName    = 'DMSP-15'
      CASE('ssmt2_f14')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 907
        local_WMO_Satellite_Id = 247
        local_SensorName       = 'SSM/T-2'
        local_SatelliteName    = 'DMSP-14'
      CASE('ssmt2_f15')
        local_nChannels        = 5
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 907
        local_WMO_Satellite_Id = 248
        local_SensorName       = 'SSM/T-2'
        local_SatelliteName    = 'DMSP-15'
      CASE('ssmis_f16')
        local_nChannels        = 24
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 908
        local_WMO_Satellite_Id = 249
        local_SensorName       = 'SSMIS'
        local_SatelliteName    = 'DMSP-16'
      CASE('ssmis_f17')
        local_nChannels        = 24
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SSMIS
        local_WMO_Satellite_Id = WMO_DMSP17
        local_SensorName       = 'SSMIS'
        local_SatelliteName    = 'DMSP-17'
      CASE('ssmis_f18')
        local_nChannels        = 24
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SSMIS
        local_WMO_Satellite_Id = WMO_DMSP18
        local_SensorName       = 'SSMIS'
        local_SatelliteName    = 'DMSP-18'
      CASE('ssmis_f19')
        local_nChannels        = 24
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SSMIS
        local_WMO_Satellite_Id = WMO_DMSP19
        local_SensorName       = 'SSMIS'
        local_SatelliteName    = 'DMSP-19'
      CASE('ssmis_f20')
        local_nChannels        = 24
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SSMIS
        local_WMO_Satellite_Id = WMO_DMSP20
        local_SensorName       = 'SSMIS'
        local_SatelliteName    = 'DMSP-20'
      CASE('amsua_aqua')
        local_nChannels        = 15
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 570
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AMSU-A'
        local_SatelliteName    = 'AQUA'
      CASE('hsb_aqua')
        local_nChannels        = 4
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 246
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'HSB'
        local_SatelliteName    = 'AQUA'
      CASE('amsre_aqua')
        local_nChannels        = 12
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 345
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AMSR-E'
        local_SatelliteName    = 'AQUA'
      CASE('airs281SUBSET_aqua')
        local_nChannels        = 281
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airs324SUBSET_aqua')
        local_nChannels        = 324
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM12_aqua')
        local_nChannels        = 130
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM11_aqua')
        local_nChannels        = 144
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM10_aqua')
        local_nChannels        = 167
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM9_aqua')
        local_nChannels        = 167
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM8_aqua')
        local_nChannels        = 161
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM7_aqua')
        local_nChannels        = 167
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM6_aqua')
        local_nChannels        = 167
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM5_aqua')
        local_nChannels        = 159
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM4d_aqua')
        local_nChannels        = 106
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM4c_aqua')
        local_nChannels        = 94
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM3_aqua')
        local_nChannels        = 192
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM4b_aqua')
        local_nChannels        = 106
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM4a_aqua')
        local_nChannels        = 104
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM2b_aqua')
        local_nChannels        = 150
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM1b_aqua')
        local_nChannels        = 130
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM2a_aqua')
        local_nChannels        = 116
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('airsM1a_aqua')
        local_nChannels        = 118
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE('modis_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD01_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD02_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD03_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD04_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD05_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD06_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD07_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD08_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD09_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modisD10_terra')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 783
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'TERRA'
      CASE('modis_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD01_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD02_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD03_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD04_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD05_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD06_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD07_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD08_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD09_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('modisD10_aqua')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 389
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'MODIS'
        local_SatelliteName    = 'AQUA'
      CASE('vissrDetA_gms5')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 489
        local_WMO_Satellite_Id = 152
        local_SensorName       = 'VISSR'
        local_SatelliteName    = 'GMS-5'
      CASE('vissrDetB_gms5')
        local_nChannels        = 3
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 489
        local_WMO_Satellite_Id = 152
        local_SensorName       = 'VISSR'
        local_SatelliteName    = 'GMS-5'
      CASE('mviriNOM_m03')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 50
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-03'
      CASE('mviriBKUP_m03')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 50
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-03'
      CASE('mviriNOM_m04')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 51
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-04'
      CASE('mviriBKUP_m04')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 51
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-04'
      CASE('mviriNOM_m05')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 52
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-05'
      CASE('mviriBKUP_m05')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 52
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-05'
      CASE('mviriNOM_m06')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 53
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-06'
      CASE('mviriBKUP_m06')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 53
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-06'
      CASE('mviriNOM_m07')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 54
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-07'
      CASE('mviriBKUP_m07')
        local_nChannels        = 2
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 205
        local_WMO_Satellite_Id = 54
        local_SensorName       = 'MVIRI'
        local_SatelliteName    = 'METEOSAT-07'
      CASE('seviri_m08')
        local_nChannels        = 8
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SEVIRI
        local_WMO_Satellite_Id = WMO_METEOSAT08
        local_SensorName       = 'SEVIRI'
        local_SatelliteName    = 'METEOSAT-08'
      CASE('seviri_m09')
        local_nChannels        = 8
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SEVIRI
        local_WMO_Satellite_Id = WMO_METEOSAT09
        local_SensorName       = 'SEVIRI'
        local_SatelliteName    = 'METEOSAT-09'
      CASE('seviri_m10')
        local_nChannels        = 8
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SEVIRI
        local_WMO_Satellite_Id = WMO_METEOSAT10
        local_SensorName       = 'SEVIRI'
        local_SatelliteName    = 'METEOSAT-10'
      CASE('seviri_m11')
        local_nChannels        = 8
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_SEVIRI
        local_WMO_Satellite_Id = WMO_METEOSAT11
        local_SensorName       = 'SEVIRI'
        local_SatelliteName    = 'METEOSAT-11'
      CASE('windsat_coriolis')
        local_nChannels        = 16
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 2047
        local_WMO_Satellite_Id = 283
        local_SensorName       = 'WindSat'
        local_SatelliteName    = 'Coriolis'
      CASE('atms_c1')
        local_nChannels        = 22
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = WMO_ATMS
        local_WMO_Satellite_Id = WMO_NPOESSC1
        local_SensorName       = 'ATMS'
        local_SatelliteName    = 'NPOESS-C1'
      CASE('airs_aqua')
        local_nChannels        = 2378
        local_nFOVs            = -1
        local_Detector         = -1
        local_WMO_Sensor_Id    = 420
        local_WMO_Satellite_Id = 784
        local_SensorName       = 'AIRS'
        local_SatelliteName    = 'Aqua'
      CASE DEFAULT
        Error_Status=FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'No entry for Sensor_Id'//TRIM(ADJUSTL(Sensor_Id)), &
                              Error_Status, &
                              Message_Log=Message_Log) 
    END SELECT
    
    ! Return requested attributes
    IF ( PRESENT(nChannels       ) ) nChannels        = local_nChannels
    IF ( PRESENT(nFOVs           ) ) nFOVs            = local_nFOVs          
    IF ( PRESENT(Detector        ) ) Detector         = local_Detector       
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = local_WMO_Sensor_Id        
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = local_WMO_Satellite_Id     
    IF ( PRESENT(SensorName      ) ) SensorName       = local_SensorName    
    IF ( PRESENT(SatelliteName   ) ) SatelliteName    = local_SatelliteName
    
  END FUNCTION CRTM_Get_SensorAttributes
  
END MODULE CRTM_SensorInfo
