subroutine da_fm_decoder (fm, platform, synop, ship,  metar, pilot, sound,   &
                                      satem, satob, airep, gpspw, gpsref, gpseph, &
                                      ssmi_retrieval, ssmi_tb, ssmt1, ssmt2, &
                                      qscat,profiler, buoy, other)

   !---------------------------------------------------------------------------  
   ! Purpose: Given the WMO code fm, return the observation platform type and 
   ! increment the corresponding counter if present.
   !
   ! Returned platforms are reduced to 10 output classes:
   !
   !   Name    WMO Codes     WMO Code names
   !   synop    12,14       'SYNOP','SYNOP MOBIL'
   !   ship     13, 17      'SHIP'
   !   metar    15,16       'METAR','SPECI'
   !   pilot    32,33,34    'PILOT','PILOT SHIP','PILOT MOBIL'
   !   sound    35,36,37,38 'TEMP','TEMP SHIP, 'TEMP DROP','TEMP MOBIL'
   !   satem    86          'SATEM'
   !   satob    88          'SATOB'
   !   airep    42, 96,97   'AIREP', 'AMDAR'
   !   gpspw    111         'GPSPW'
   !   gpsztd   114         'GPSZD'
   !   gpsref   116         'GPSRF'
   !   gpseph   118         'GPSEP'
   !   ssmi_retrieval 125         'SSMI_RETRIEVAL'
   !   ssmi_tb  126         'SSMI_TB'
   !   SSM/T1   121         'SSMT1'
   !   SSM/T2   122         'SSMT2'
   !   QSCAT    281         'QSCAT'
   !   Profiler 132         'PROFI'
   !   Buoy     18,19       'BUOY '
   !   other      Any other code
! not WMO
   !   MTGIRS  161          'MTGIRS'
   !   TAMDAR   91          'TAMDAR'
   !----------------------------------------------------------------------------

    implicit none

    integer,              intent (in)    :: fm
    character (LEN = 40), intent (out)   :: platform
    integer, optional,    intent (inout) :: synop, ship,  metar, &
                                            pilot, sound, satem, &
                                            satob, airep, gpspw, &
                                            ssmi_retrieval, ssmi_tb,&
                                            ssmt1, ssmt2, qscat, &
                                            profiler, buoy, gpsref, gpseph, mtgirs,&
                                            tamdar, other

   if (trace_use_frequent) call da_trace_entry("da_fm_decoder")

   select case(fm)

      !  Report of surface observations from a fixed land station

      case (12) ; platform = 'SYNOP'
         if (present (synop)) synop = synop + 1

      !  Report of surface observations from a sea station

      case (13) ; platform = 'SHIP'
         if (present (ship)) ship  = ship + 1

      !  Report of surface observations from a mobile land station

      case (14) ; platform = 'SYNOP MOBIL'
         if (present (synop)) synop  = synop + 1

      !  Aviation routine weather report (with/without trend forecast)

      case (15) ; platform = 'METAR'
         if (present (metar)) metar  = metar + 1

      !  Aviation selected special weather report 
      !  (with/without trend forecast)

      case (16) ; platform = 'SPECI'
         if (present (metar)) metar  = metar + 1

      !  Report of buoy observation

      case (18,19) ; platform = 'BUOY'
         if (present (buoy)) buoy  = buoy + 1

      !  Report of ground radar weather observation

      case (20) ; platform = 'RAdoB'
         if (present (other)) other  = other + 1

      !  Radiological data report (monitored on a routine basis and/or 
      !  in case of accident

      case (22) ; platform = 'RADREP'
         if (present (other)) other  = other + 1

      !  Upper-wind report from a fixed land station

      case (32) ; platform = 'PILOT'
         if (present (pilot)) pilot = pilot + 1

      !  Upper-wind report from a sea station

      case (33) ; platform = 'PILOT SHIP'
         if (present (pilot)) pilot = pilot + 1

      !  Upper-wind report from a mobile land station

      case (34) ; platform = 'PILOT MOBIL'
         if (present (pilot)) pilot = pilot + 1

      !  Upper-level pressure, temperature, humidity and wind report 
      !  from a fixed land station

      case (35) ; platform = 'TEMP'
         if (present (sound)) sound = sound + 1

      !  Upper-level pressure, temperature, humidity and wind report 
      !  from a sea station

      case (36) ; platform = 'TEMP SHIP'
         if (present (sound)) sound = sound + 1

      !  Upper-level pressure, temperature, humidity and wind report 
      !  from a sonde released by carrier balloons and aircraft

      case (37) ; platform = 'TEMP DROP'
         if (present (sound)) sound = sound + 1

      !  Upper-level pressure, temperature, humidity and wind report 
      !  from a mobile land station

      case (38) ; platform = 'TEMP MOBIL'
         if (present (sound)) sound = sound + 1

      !  Upper-level temperature, wind and air density from a land 
      !  rocketsonde station

      case (39) ; platform = 'ROCOB'
         if (present (other)) other = other + 1

      !  Upper-level temperature, wind and air density from a 
      !  rocketsonde station on a ship

      case (40) ; platform = 'ROCOB SHIP'
         if (present (other)) other = other + 1

      !  Upper-air report from an aircraft 
      !  (other than weather reconnaissance aircraft)

      case (41) ; platform = 'CODAR'
         if (present (other)) other = other + 1

      !  Aircraft report (aircraft meteorological data relay)

      case (42) ; platform = 'AMDAR'
         if (present (airep)) airep = airep + 1

      !  Ice analysis

      case (43) ; platform = 'ICEAN'
         if (present (other)) other = other + 1

      !  Analysis in full form

      case (45) ; platform = 'IAC'
         if (present (other)) other = other + 1

      !  Analysis in abbreviated form

      case (46) ; platform = 'IAC FLEET'
         if (present (other)) other = other + 1

      !  Processed data in form of grid-point values

      case (47) ; platform = 'GRID'
         if (present (other)) other = other + 1

      !  Processed data in form of grid-point values (abbreviated code form)

      case (49) ; platform = 'GRAF'
         if (present (other)) other = other + 1

      !  Forecast upper wind and temperature for aviation

      case (50) ; platform = 'WinTEM'
         if (present (other)) other = other + 1

      !  Aerodrome forecast

      case (51) ; platform = 'TAF'
         if (present (other)) other = other + 1

      !  Area forecast for aviation

      case (53) ; platform = 'ARFOR'
         if (present (other)) other = other + 1

      !  Route forecast for aviation

      case (54) ; platform = 'ROFOR'
         if (present (other)) other = other + 1

      !  Radiological trajectory dose forecast 
      !  (defined time of arrival and location)

      case (57) ; platform = 'RAdoF'
         if (present (other)) other = other + 1

      !  Forecast for shipping

      case (61) ; platform = 'MAFOR'
         if (present (other)) other = other + 1

      !  Report of marine surface observations along a ship's track

      case (62) ; platform = 'TRACKOB'
         if (present (other)) other = other + 1

      !  Report of bathymetrical observation

      case (63) ; platform = 'BATHY'
         if (present (other)) other = other + 1

      !  Temperature salinity and current report from a sea station

      case (64) ; platform = 'TRESAC'
         if (present (other)) other = other + 1

      !  Report of spectral wave information from sea station or from
      !  a remote platform (airecraft or satellite)

      case (65) ; platform = 'WAVEOB'
         if (present (other)) other = other + 1

      !  Report of hydrological observations from a hydrological station

      case (66) ; platform = 'HYDRA'
         if (present (other)) other = other + 1

      !  Hydrological forecast

      case (67) ; platform = 'HYFOR'
         if (present (other)) other = other + 1

      !  Report of monthly values from a land station

      case (71) ; platform = 'CLIMAT'
         if (present (other)) other = other + 1

      !  Report of monthly means and total from an ocean weather station

      case (72) ; platform = 'CLIMAT SHIP'
         if (present (other)) other = other + 1

      !  Report of monthly means for an oceanic area

      case (73) ; platform = 'NACLI CLinP SPLCI CLISA inCLI'
         if (present (other)) other = other + 1

      !  Report of monthly aerological means from a land station

      case (75) ; platform = 'CLIMAT TEMP'
         if (present (other)) other = other + 1

      !  Report of monthly aerological means from an ocean weather station

      case (76) ; platform = 'CLIMAT TEMP SHIP'
         if (present (other)) other = other + 1

      !  Synoptic report of bearings of sources of atmospherics

      case (81) ; platform = 'SFAZI'
         if (present (other)) other = other + 1

      !  Synoptic report of the geographical location of sources of 
      !  atmopsherics

      case (82) ; platform = 'SFLOC'
         if (present (other)) other = other + 1

      !  Detailed report of the distribution of sources of atmospherics
      !  by bearings for any period up to including 24 hours

      case (83) ; platform = 'SFAZU'
         if (present (other)) other = other + 1

      !  Report of synoptic interpretation of cloud data obtained by a
      !  meteorlogical satellite

      case (85) ; platform = 'SAREP'
         if (present (other)) other = other + 1

      !  Report of satellite remote upper-air soundings of 
      !  pressure, temperature and humidity

      case (86) ; platform = 'SATEM'
         if (present (satem)) satem = satem + 1

      !  Report of satellite clear radiance observations

      case (87) ; platform = 'SARAD'
         if (present (other)) other = other + 1

      !  Report of satellite remote upper-air soundings of 
      !  pressure, temperature and humidity

      case (88) ; platform = 'SATOB'
         if (present (satob)) satob = satob + 1

      !  Airep reports (not a WMO report)

      case (96:97) ; platform = 'AIREP'
         if (present (airep)) airep = airep + 1

      !  GPS Precipitable Water Vapor or GPS Zineth Total Delay
      !  (not a WMO report)

      case (111) ; platform = 'GPSPW'
         if (present (gpspw)) gpspw = gpspw + 1
      case (114) ; platform = 'GPSZD'
         if (present (gpspw)) gpspw = gpspw + 1

      !  GPS Refractivity (not a WMO report)

      case (116) ; platform = 'GPSRF'
         if (present (gpsref)) gpsref = gpsref + 1

      case (118) ; platform = 'GPSEP'
         if (present (gpseph)) gpseph = gpseph + 1

      case (121) ; platform = 'SSMT1'
         if (present (ssmt1)) ssmt1 = ssmt1 + 1

      case (122) ; platform = 'SSMT2'
         if (present (ssmt2)) ssmt2 = ssmt2 + 1

      case (125) ; platform = 'SSMI_RETRIEVAL'
         if (present (ssmi_retrieval)) ssmi_retrieval = ssmi_retrieval + 1

      case (126) ; platform = 'SSMI_TB'
         if (present (ssmi_tb)) ssmi_tb = ssmi_tb + 1

      case (281) ; platform = 'QSCAT'
         if (present (qscat)) qscat = qscat + 1

      case (132) ; platform = 'PROFI'
         if (present (profiler)) profiler = profiler + 1
      !  Others

      case (101) ; platform = 'TAMDAR'
         if (present (tamdar)) tamdar = tamdar + 1

      case (161) ; platform = 'MTGIRS'
         if (present (mtgirs)) mtgirs = mtgirs + 1

      case default;
                   platform = 'UNKNOWN'
          if (present (other)) other = other + 1

   end select

   !  Reduce the platform name to one of the 10 classes

   select case (trim (platform))

      case ('SYNOP','SYNOP MOBIL');                       platform = "SYNOP";
      case ('SHIP');                                      platform = "SHIP";
      case ('METAR','SPECI');                             platform = "METAR";
      case ('PILOT','PILOT SHIP','PILOT MOBIL');          platform = "PILOT";
      case ('TEMP','TEMP SHIP','TEMP DROP','TEMP MOBIL'); platform = "SOUND";
      case ('SATEM');                                     platform = "SATEM";
      case ('SATOB');                                     platform = "SATOB";
      case ('AIREP');                                     platform = "AIREP";
      case ('OTHER');                                     platform = "OTHER";
      case ('BUOY');                                      platform = "BUOY";
      case ('PROFI');                                     platform = "PROFI";
      case ('MTGIRS');                                    platform = "MTGIRS";
      case default;                                       platform = "OTHER";

   end select

   if (trace_use_frequent) call da_trace_exit("da_fm_decoder")
     
end subroutine da_fm_decoder 


