MODULE da_verif_obs_control
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!   Main module for 
!   defining and initializing various data type, 
!   defining unit numbers and cosnstants
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/25/2006
! Updates:
!            Syed RH Rizvi     NCAR/MMM         05/08/2007
!            Significance test & error bars are added
!----------------------------------------------------------------------------   
   implicit none

  integer, parameter       :: maxnum = 10, nstd = 16, nstdh = 150
  integer, parameter       :: num_verif_var =5        
  real,    dimension(nstd) :: stdp
  real,    dimension(nstdh):: stdh
  real                     :: rmiss = -99.99
  integer                  :: num_miss = -99
  real, parameter          :: missing_r = -888888.

  data stdp/1000.0, 925.0, 850.0, 700.0, 500.0, 400.0, 300.0, &
             250.0, 200.0, 150.0, 100.0,  70.0,  50.0 ,30.0, 20.0 ,10.0/ 
  data stdh/0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, &
            2.2, 2.4, 2.6, 2.8, 3.0, 3.2, 3.4, 3.6, 3.8, 4.0, &
            4.2, 4.4, 4.6, 4.8, 5.0, 5.2, 5.4, 5.6, 5.8, 6.0, &
            6.2, 6.4, 6.6, 6.8, 7.0, 7.2, 7.4, 7.6, 7.8, 8.0, &
            8.2, 8.4, 8.6, 8.8, 9.0, 9.2, 9.4, 9.6, 9.8,10.0, &
           10.2,10.4,10.6,10.8,11.0,11.2,11.4,11.6,11.8,12.0, &
           12.2,12.4,12.6,12.8,13.0,13.2,13.4,13.6,13.8,14.0, &
           14.2,14.4,14.6,14.8,15.0,15.2,15.4,15.6,15.8,16.0, &
           16.2,16.4,16.6,16.8,17.0,17.2,17.4,17.6,17.8,18.0, &
           18.2,18.4,18.6,18.8,19.0,19.2,19.4,19.6,19.8,20.0, &
           20.2,20.4,20.6,20.8,21.0,21.2,21.4,21.6,21.8,22.0, &
           22.2,22.4,22.6,22.8,23.0,23.2,23.4,23.6,23.8,24.0, &
           24.2,24.4,24.6,24.8,25.0,25.2,25.4,25.6,25.8,26.0, &
           26.2,26.4,26.6,26.8,27.0,27.2,27.4,27.6,27.8,28.0, &
           28.2,28.4,28.6,28.8,29.0,29.2,29.4,29.6,29.8,30.0 /


  character (len=1)   :: verif_var(num_verif_var)
  character (len= 2)  :: verif_type(2)
  data verif_var/'U','V','T','Q','P'/
  data verif_type/'OI','AO'/

  type stats_value
    integer     :: num
    real        :: abias
    real        :: bias
    real        :: rmse
  end type stats_value

  type surface_type
    type (stats_value) :: uomb, uoma
    type (stats_value) :: vomb, voma
    type (stats_value) :: tomb, toma
    type (stats_value) :: pomb, poma
    type (stats_value) :: qomb, qoma
  end type surface_type

  type upr_type
    type (stats_value) :: uomb(nstd), uoma(nstd)
    type (stats_value) :: vomb(nstd), voma(nstd)
    type (stats_value) :: tomb(nstd), toma(nstd)
    type (stats_value) :: qomb(nstd), qoma(nstd)
  end type upr_type

  type gpspw_type
    type (stats_value)          :: tpwomb, tpwoma         
  end type gpspw_type

  type gpsref_type
    type (stats_value)          :: refomb(nstdh), refoma(nstdh)         
  end type gpsref_type

! namelist.varstats variables

! record1
  INTEGER                               :: exp_num   ! number of experiments
  CHARACTER(LEN=512),DIMENSION(maxnum)  :: exp_dirs, out_dirs   
! record2
  CHARACTER(LEN=10)      :: start_date
  CHARACTER(LEN=10)      :: end_date
  INTEGER                :: interval       ! interval(h) between initial times
!                                          ! Typically 6 or 12 hours

! record3

  LOGICAL  :: if_plot_rmse
  LOGICAL  :: if_plot_bias
  LOGICAL  :: if_plot_abias

! record4
  LOGICAL  :: if_plot_surface

  LOGICAL  :: if_plot_synop  
  LOGICAL  :: if_plot_metar  
  LOGICAL  :: if_plot_ships  
  LOGICAL  :: if_plot_buoy   
  LOGICAL  :: if_plot_sonde_sfc
  LOGICAL  :: if_plot_qscat

  LOGICAL  :: if_plot_upr   
!
  LOGICAL  :: if_plot_sound
  LOGICAL  :: if_plot_airep
  LOGICAL  :: if_plot_pilot 
  LOGICAL  :: if_plot_profiler
  LOGICAL  :: if_plot_polaramv 
  LOGICAL  :: if_plot_geoamv   
  LOGICAL  :: if_plot_tamdar

  LOGICAL  :: if_plot_gpspw 
  LOGICAL  :: if_plot_gpsref
  LOGICAL  :: if_plot_airsret 
! record5
  character (len=50)    :: file_path_string
  character (len=512)   :: wrf_file
  integer  :: istart, iend, jstart, jend

  NAMELIST /Record1/ exp_num, exp_dirs, out_dirs
  NAMELIST /Record2/ start_date, end_date, interval
  NAMELIST /Record3/ if_plot_rmse, if_plot_bias, if_plot_abias
  NAMELIST /Record4/ if_plot_synop, if_plot_sonde_sfc,if_plot_metar, &
                     if_plot_ships, if_plot_buoy , if_plot_qscat, &
                     if_plot_sound, if_plot_airep, if_plot_pilot, if_plot_profiler, &
                     if_plot_geoamv, if_plot_polaramv, if_plot_gpspw, if_plot_gpsref, &
                     if_plot_airsret, if_plot_tamdar
  NAMELIST /Record5/ file_path_string
  NAMELIST /Record6/ wrf_file, istart, iend, jstart, jend
!
! Namelist declaration over
  integer      :: nml_unit, diag_unit_in, diag_unit_out, info_unit, plot_stats_unit
  real, dimension(34,2) :: alpha
end MODULE da_verif_obs_control
