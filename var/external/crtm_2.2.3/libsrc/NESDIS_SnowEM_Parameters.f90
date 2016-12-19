!
! NESDIS_SnowEM_Parameters
!
! Module containing the parameters related to microwave snow emissivity model
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 03-Jun-2005 
!                       banghua.yan@noaa.gov
!                       Fuzhong Weng
!                       fuzhong.weng@noaa.gov
!

MODULE NESDIS_SnowEM_Parameters


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
  PRIVATE


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_SnowEM_Parameters.f90 60152 2015-08-13 19:19:13Z paul.vandelst@noaa.gov $'

  ! Snow types
  INTEGER, PUBLIC, PARAMETER :: INVALID_SNOW_TYPE   = -999
  INTEGER, PUBLIC, PARAMETER :: WET_SNOW            =  1
  INTEGER, PUBLIC, PARAMETER :: GRASS_AFTER_SNOW    =  2
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_A           =  3
  INTEGER, PUBLIC, PARAMETER :: POWDER_SNOW         =  4
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_B           =  5
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_C           =  6
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_D           =  7
  INTEGER, PUBLIC, PARAMETER :: THIN_CRUST_SNOW     =  8
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_E           =  9
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_A = 10
  INTEGER, PUBLIC, PARAMETER :: SHALLOW_SNOW        = 11
  INTEGER, PUBLIC, PARAMETER :: DEEP_SNOW           = 12
  INTEGER, PUBLIC, PARAMETER :: CRUST_SNOW          = 13
  INTEGER, PUBLIC, PARAMETER :: MEDIUM_SNOW         = 14
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_B = 15
  INTEGER, PUBLIC, PARAMETER :: THICK_CRUST_SNOW    = 16

  INTEGER, PUBLIC, PARAMETER                          :: N_FREQUENCY  = 10
  INTEGER, PUBLIC, PARAMETER                          :: N_FREQ_AMSRE = 7




  REAL(fp), PUBLIC, PARAMETER, DIMENSION (N_FREQUENCY)    ::                                &
  FREQUENCY_DEFAULT = (/ 4.9_fp,  6.93_fp, 10.65_fp, 18.7_fp,23.8_fp,   &
                        31.4_fp, 50.3_fp,  52.5_fp, 89.0_fp,150._fp/)



  ! Define sixteen MW weighted emissivity spectra for AMSU ALGORITHMS

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  WET_SNOW_EMISS = (/0.87_fp,0.89_fp,0.91_fp,0.93_fp,0.94_fp,           &
                          0.94_fp,0.94_fp,0.93_fp,0.92_fp,0.90_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  GRASS_AFTER_SNOW_EMISS = (/0.91_fp,0.91_fp,0.92_fp,0.91_fp,                &
                                  0.90_fp,0.90_fp,0.91_fp,0.91_fp,           &
                                  0.91_fp,0.86_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_A_EMISS = (/0.90_fp,0.89_fp,0.88_fp,0.87_fp, 0.86_fp,         &
                           0.86_fp,0.85_fp,0.85_fp,0.82_fp,0.82_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  POWDER_SNOW_EMISS = (/0.91_fp,0.91_fp,0.93_fp,0.93_fp,0.93_fp,        &
                             0.93_fp,0.89_fp,0.88_fp,0.79_fp,0.79_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_B_EMISS = (/0.90_fp,0.89_fp,0.88_fp,0.85_fp,0.84_fp,          &
                           0.83_fp,0.83_fp,0.82_fp,0.79_fp,0.73_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_C_EMISS = (/0.90_fp,0.89_fp,0.86_fp,0.82_fp,0.80_fp,          &
                           0.79_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_D_EMISS = (/0.88_fp,0.86_fp,0.85_fp,0.80_fp,0.78_fp,          &
                           0.77_fp,0.77_fp,0.76_fp,0.72_fp,0.72_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  THIN_CRUST_SNOW_EMISS = (/0.93_fp,0.94_fp,0.96_fp,0.96_fp,0.95_fp,    &
                                 0.93_fp,0.87_fp,0.86_fp,0.74_fp,0.65_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_E_EMISS = (/0.87_fp,0.86_fp,0.84_fp,0.80_fp,0.76_fp,          &
                           0.76_fp,0.75_fp,0.75_fp,0.70_fp,0.69_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EMISS = (/0.87_fp,0.86_fp,0.83_fp,0.77_fp,             &
                                     0.73_fp,0.68_fp,0.66_fp,0.66_fp,        &
                                     0.68_fp,0.67_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  SHALLOW_SNOW_EMISS = (/0.89_fp,0.89_fp,0.88_fp,0.87_fp, 0.86_fp,      &
                              0.82_fp,0.77_fp,0.76_fp,0.69_fp,0.64_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  DEEP_SNOW_EMISS = (/0.88_fp,0.87_fp,0.86_fp,0.83_fp,0.81_fp,          &
                           0.77_fp,0.74_fp,0.73_fp,0.69_fp,0.64_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  CRUST_SNOW_EMISS = (/0.86_fp,0.86_fp,0.86_fp,0.85_fp,0.82_fp,         &
                            0.78_fp,0.69_fp,0.68_fp,0.51_fp,0.47_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  MEDIUM_SNOW_EMISS = (/0.89_fp,0.88_fp,0.87_fp,0.83_fp,0.80_fp,        &
                             0.75_fp,0.70_fp,0.70_fp,0.64_fp,0.60_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EMISS = (/0.91_fp,0.92_fp,0.93_fp,0.88_fp,             &
                                     0.84_fp,0.76_fp,0.66_fp,0.64_fp,        &
                                     0.48_fp,0.44_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  THICK_CRUST_SNOW_EMISS = (/0.94_fp,0.95_fp,0.97_fp,0.91_fp,                &
                                  0.86_fp,0.74_fp,0.63_fp,0.63_fp,           &
                                  0.50_fp,0.45_fp/)


!
  REAL(fp), PUBLIC, PARAMETER, DIMENSION (N_FREQ_AMSRE)    ::                                &
  FREQUENCY_AMSRE = (/ 6.925_fp, 10.65_fp, 18.7_fp,23.8_fp,                   &
                        36.5_fp, 89.0_fp,150._fp/)


 ! Define sixteen MW weighted emissivity spectra for AMSRE ALGORITHMS

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EM_AMSRE = (/0.91_fp, 0.93_fp, 0.94_fp, 0.95_fp, 0.95_fp,     &
                        0.93_fp, 0.93_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EM_AMSRE = (/0.91_fp, 0.92_fp, 0.91_fp, 0.90_fp,           &
                                0.91_fp, 0.91_fp, 0.91_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EM_AMSRE = (/0.90_fp, 0.89_fp, 0.88_fp, 0.87_fp, 0.86_fp,    &
                         0.82_fp, 0.82_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EM_AMSRE = (/0.92_fp, 0.93_fp, 0.94_fp, 0.94_fp, 0.92_fp,  &
                           0.80_fp, 0.80_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EM_AMSRE = (/0.87_fp, 0.86_fp, 0.83_fp, 0.80_fp, 0.79_fp,    &
                         0.77_fp, 0.77_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EM_AMSRE = (/0.89_fp, 0.88_fp, 0.85_fp, 0.84_fp, 0.83_fp,    &
                         0.79_fp, 0.79_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EM_AMSRE = (/0.84_fp, 0.83_fp, 0.82_fp, 0.80_fp,            &
                               0.78_fp, 0.72_fp, 0.72_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EM_AMSRE = (/0.95_fp, 0.96_fp, 0.96_fp, 0.95_fp,            &
                               0.91_fp, 0.75_fp, 0.75_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EM_AMSRE = (/0.80_fp, 0.80_fp, 0.80_fp, 0.79_fp,                  &
                         0.75_fp, 0.70_fp, 0.70_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EM_AMSRE = (/0.91_fp, 0.90_fp, 0.89_fp, 0.87_fp,        &
                                   0.82_fp, 0.69_fp, 0.69_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EM_AMSRE = (/0.90_fp, 0.89_fp, 0.85_fp,0.82_fp, 0.76_fp, &
                         0.65_fp, 0.65_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EM_AMSRE = (/0.89_fp, 0.88_fp, 0.86_fp, 0.83_fp, 0.78_fp,    &
                          0.70_fp, 0.70_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EM_AMSRE = (/0.88_fp, 0.86_fp, 0.80_fp, 0.75_fp, 0.69_fp,   &
                           0.67_fp, 0.67_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EM_AMSRE = (/0.96_fp, 0.97_fp, 0.92_fp, 0.87_fp, 0.72_fp,  &
                           0.50_fp, 0.50_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EM_AMSRE = (/0.93_fp, 0.94_fp, 0.89_fp, 0.85_fp,        &
                                   0.74_fp, 0.48_fp, 0.48_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EM_AMSRE = (/0.88_fp, 0.88_fp, 0.87_fp, 0.85_fp,           &
                                0.77_fp, 0.52_fp, 0.52_fp/)



 ! Define sixteen MW H-POL emissivity spectra for AMSRE ALGORITHMS


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EH_AMSRE = (/0.93_fp, 0.92_fp, 0.93_fp, 0.94_fp, 0.93_fp,     &
                        0.93_fp, 0.90_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EH_AMSRE = (/0.91_fp, 0.90_fp, 0.90_fp, 0.90_fp,           &
                                0.91_fp, 0.90_fp, 0.85_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EH_AMSRE = (/0.85_fp, 0.85_fp, 0.84_fp, 0.84_fp, 0.82_fp,    &
                         0.80_fp, 0.80_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EH_AMSRE = (/0.90_fp, 0.90_fp, 0.92_fp, 0.92_fp, 0.90_fp,  &
                           0.80_fp, 0.79_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EH_AMSRE = (/0.82_fp, 0.81_fp, 0.77_fp, 0.76_fp, 0.74_fp,    &
                         0.74_fp, 0.74_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EH_AMSRE = (/0.84_fp, 0.83_fp, 0.80_fp, 0.78_fp, 0.77_fp,   &
                         0.75_fp, 0.69_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EH_AMSRE = (/0.77_fp, 0.77_fp, 0.76_fp, 0.75_fp, 0.73_fp,    &
                         0.71_fp, 0.71_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EH_AMSRE = (/0.95_fp, 0.94_fp, 0.95_fp, 0.94_fp,            &
                               0.89_fp, 0.75_fp, 0.65_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EH_AMSRE = (/0.73_fp, 0.73_fp, 0.74_fp, 0.72_fp, 0.71_fp,    &
                         0.68_fp, 0.67_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EH_AMSRE = (/0.88_fp, 0.87_fp, 0.86_fp, 0.85_fp,        &
                                   0.80_fp, 0.68_fp, 0.63_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EH_AMSRE = (/0.86_fp, 0.84_fp, 0.80_fp, 0.78_fp,               &
                            0.72_fp, 0.62_fp, 0.57_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EH_AMSRE = (/0.87_fp, 0.85_fp, 0.83_fp, 0.80_fp, 0.77_fp,    &
                         0.68_fp, 0.62_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EH_AMSRE = (/0.82_fp, 0.78_fp, 0.74_fp, 0.71_fp, 0.67_fp,   &
                          0.64_fp, 0.64_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EH_AMSRE = (/0.90_fp, 0.90_fp, 0.89_fp, 0.88_fp, 0.83_fp,  &
                           0.53_fp, 0.48_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EH_AMSRE = (/0.87_fp, 0.85_fp, 0.84_fp, 0.82_fp,        &
                                   0.74_fp, 0.53_fp, 0.49_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EH_AMSRE = (/0.85_fp, 0.84_fp, 0.83_fp, 0.81_fp,           &
                                0.79_fp, 0.51_fp, 0.46_fp/)



 ! Define sixteen MW V-POL emissivity spectra for AMSRE ALGORITHMS

 REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EV_AMSRE = (/0.96_fp, 0.94_fp, 0.96_fp, 0.95_fp, 0.94_fp,     &
                        0.94_fp, 0.91_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EV_AMSRE = (/0.96_fp, 0.94_fp, 0.95_fp, 0.96_fp,           &
                                0.96_fp, 0.92_fp, 0.87_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EV_AMSRE = (/0.99_fp, 0.97_fp, 0.96_fp, 0.96_fp, 0.93_fp,    &
                         0.87_fp, 0.87_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EV_AMSRE = (/0.98_fp, 0.97_fp, 0.99_fp, 0.98_fp, 0.96_fp,  &
                           0.84_fp, 0.83_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EV_AMSRE = (/0.97_fp, 0.95_fp, 0.93_fp, 0.92_fp, 0.89_fp,    &
                         0.84_fp,0.84_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EV_AMSRE = (/1.00_fp, 0.97_fp, 0.96_fp, 0.94_fp, 0.91_fp,    &
                         0.84_fp, 0.78_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EV_AMSRE = (/0.99_fp, 0.96_fp, 0.93_fp, 0.90_fp, 0.86_fp,    &
                         0.80_fp, 0.80_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EV_AMSRE = (/0.98_fp, 0.97_fp, 0.98_fp, 0.97_fp,            &
                               0.92_fp, 0.77_fp, 0.67_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EV_AMSRE = (/0.98_fp, 0.95_fp, 0.90_fp, 0.86_fp, 0.82_fp,    &
                         0.74_fp, 0.73_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EV_AMSRE = (/0.96_fp, 0.95_fp, 0.95_fp, 0.93_fp,        &
                                   0.87_fp, 0.71_fp, 0.66_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EV_AMSRE = (/0.97_fp, 0.95_fp, 0.94_fp, 0.90_fp, 0.84_fp, &
                            0.68_fp, 0.63_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EV_AMSRE = (/0.96_fp, 0.94_fp, 0.92_fp, 0.90_fp, 0.85_fp,    &
                         0.77_fp, 0.71_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EV_AMSRE = (/0.98_fp, 0.96_fp, 0.93_fp, 0.90_fp, 0.81_fp,   &
                          0.71_fp, 0.71_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EV_AMSRE = (/0.99_fp, 0.97_fp, 0.98_fp, 0.96_fp, 0.92_fp,  &
                           0.57_fp, 0.52_fp/)


  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EV_AMSRE = (/1.00_fp, 0.97_fp, 0.97_fp, 0.95_fp,        &
                                   0.86_fp, 0.58_fp, 0.54_fp/)

  REAL(fp), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EV_AMSRE = (/0.98_fp, 0.96_fp, 0.96_fp, 0.94_fp,           &
                                0.89_fp, 0.56_fp, 0.51_fp/)


END MODULE NESDIS_SnowEM_Parameters
