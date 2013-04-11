!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_SnowEM_Parameters Module
!
! PURPOSE:
!       Module containing the parameters related to microwave snow emissivity model
!
! CATEGORY:
!       Surface : MW Surface Snow Emissivity Model Parameters
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SnowEM_Parameters Module
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds of variable types.
!
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (06-03-2005)
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE NESDIS_SnowEM_Parameters


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  INTEGER, PUBLIC, PARAMETER :: INVALID_SNOW_TYPE   = -999


  ! ----------------
  ! Valid snow types
  ! ----------------

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

  INTEGER(ip_kind), PUBLIC, PARAMETER                          :: N_FREQUENCY  = 10
  INTEGER(ip_kind), PUBLIC, PARAMETER                          :: N_FREQ_AMSRE = 7

!

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION (N_FREQUENCY)    ::                                &
  FREQUENCY_DEFAULT = (/ 4.9_fp_kind,  6.93_fp_kind, 10.65_fp_kind, 18.7_fp_kind,23.8_fp_kind,   &
                        31.4_fp_kind, 50.3_fp_kind,  52.5_fp_kind, 89.0_fp_kind,150._fp_kind/)



  ! Define sixteen MW weighted emissivity spectra for AMSU ALGORITHMS

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  WET_SNOW_EMISS = (/0.87_fp_kind,0.89_fp_kind,0.91_fp_kind,0.93_fp_kind,0.94_fp_kind,           &
                          0.94_fp_kind,0.94_fp_kind,0.93_fp_kind,0.92_fp_kind,0.90_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  GRASS_AFTER_SNOW_EMISS = (/0.91_fp_kind,0.91_fp_kind,0.92_fp_kind,0.91_fp_kind,                &
                                  0.90_fp_kind,0.90_fp_kind,0.91_fp_kind,0.91_fp_kind,           &
                                  0.91_fp_kind,0.86_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_A_EMISS = (/0.90_fp_kind,0.89_fp_kind,0.88_fp_kind,0.87_fp_kind, 0.86_fp_kind,         &
                           0.86_fp_kind,0.85_fp_kind,0.85_fp_kind,0.82_fp_kind,0.82_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  POWDER_SNOW_EMISS = (/0.91_fp_kind,0.91_fp_kind,0.93_fp_kind,0.93_fp_kind,0.93_fp_kind,        &
                             0.93_fp_kind,0.89_fp_kind,0.88_fp_kind,0.79_fp_kind,0.79_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_B_EMISS = (/0.90_fp_kind,0.89_fp_kind,0.88_fp_kind,0.85_fp_kind,0.84_fp_kind,          &
                           0.83_fp_kind,0.83_fp_kind,0.82_fp_kind,0.79_fp_kind,0.73_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_C_EMISS = (/0.90_fp_kind,0.89_fp_kind,0.86_fp_kind,0.82_fp_kind,0.80_fp_kind,          &
                           0.79_fp_kind,0.78_fp_kind,0.78_fp_kind,0.77_fp_kind,0.77_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_D_EMISS = (/0.88_fp_kind,0.86_fp_kind,0.85_fp_kind,0.80_fp_kind,0.78_fp_kind,          &
                           0.77_fp_kind,0.77_fp_kind,0.76_fp_kind,0.72_fp_kind,0.72_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  THIN_CRUST_SNOW_EMISS = (/0.93_fp_kind,0.94_fp_kind,0.96_fp_kind,0.96_fp_kind,0.95_fp_kind,    &
                                 0.93_fp_kind,0.87_fp_kind,0.86_fp_kind,0.74_fp_kind,0.65_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  RS_SNOW_E_EMISS = (/0.87_fp_kind,0.86_fp_kind,0.84_fp_kind,0.80_fp_kind,0.76_fp_kind,          &
                           0.76_fp_kind,0.75_fp_kind,0.75_fp_kind,0.70_fp_kind,0.69_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EMISS = (/0.87_fp_kind,0.86_fp_kind,0.83_fp_kind,0.77_fp_kind,             &
                                     0.73_fp_kind,0.68_fp_kind,0.66_fp_kind,0.66_fp_kind,        &
                                     0.68_fp_kind,0.67_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  SHALLOW_SNOW_EMISS = (/0.89_fp_kind,0.89_fp_kind,0.88_fp_kind,0.87_fp_kind, 0.86_fp_kind,      &
                              0.82_fp_kind,0.77_fp_kind,0.76_fp_kind,0.69_fp_kind,0.64_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  DEEP_SNOW_EMISS = (/0.88_fp_kind,0.87_fp_kind,0.86_fp_kind,0.83_fp_kind,0.81_fp_kind,          &
                           0.77_fp_kind,0.74_fp_kind,0.73_fp_kind,0.69_fp_kind,0.64_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  CRUST_SNOW_EMISS = (/0.86_fp_kind,0.86_fp_kind,0.86_fp_kind,0.85_fp_kind,0.82_fp_kind,         &
                            0.78_fp_kind,0.69_fp_kind,0.68_fp_kind,0.51_fp_kind,0.47_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  MEDIUM_SNOW_EMISS = (/0.89_fp_kind,0.88_fp_kind,0.87_fp_kind,0.83_fp_kind,0.80_fp_kind,        &
                             0.75_fp_kind,0.70_fp_kind,0.70_fp_kind,0.64_fp_kind,0.60_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EMISS = (/0.91_fp_kind,0.92_fp_kind,0.93_fp_kind,0.88_fp_kind,             &
                                     0.84_fp_kind,0.76_fp_kind,0.66_fp_kind,0.64_fp_kind,        &
                                     0.48_fp_kind,0.44_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQUENCY)  ::                                   &
  THICK_CRUST_SNOW_EMISS = (/0.94_fp_kind,0.95_fp_kind,0.97_fp_kind,0.91_fp_kind,                &
                                  0.86_fp_kind,0.74_fp_kind,0.63_fp_kind,0.63_fp_kind,           &
                                  0.50_fp_kind,0.45_fp_kind/)


!
  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION (N_FREQ_AMSRE)    ::                                &
  FREQUENCY_AMSRE = (/ 6.925_fp_kind, 10.65_fp_kind, 18.7_fp_kind,23.8_fp_kind,                   &
                        36.5_fp_kind, 89.0_fp_kind,150._fp_kind/)


 ! Define sixteen MW weighted emissivity spectra for AMSRE ALGORITHMS

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EM_AMSRE = (/0.91_fp_kind, 0.93_fp_kind, 0.94_fp_kind, 0.95_fp_kind, 0.95_fp_kind,     &
                        0.93_fp_kind, 0.93_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EM_AMSRE = (/0.91_fp_kind, 0.92_fp_kind, 0.91_fp_kind, 0.90_fp_kind,           &
                                0.91_fp_kind, 0.91_fp_kind, 0.91_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EM_AMSRE = (/0.90_fp_kind, 0.89_fp_kind, 0.88_fp_kind, 0.87_fp_kind, 0.86_fp_kind,    &
                         0.82_fp_kind, 0.82_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EM_AMSRE = (/0.92_fp_kind, 0.93_fp_kind, 0.94_fp_kind, 0.94_fp_kind, 0.92_fp_kind,  &
                           0.80_fp_kind, 0.80_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EM_AMSRE = (/0.87_fp_kind, 0.86_fp_kind, 0.83_fp_kind, 0.80_fp_kind, 0.79_fp_kind,    &
                         0.77_fp_kind, 0.77_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EM_AMSRE = (/0.89_fp_kind, 0.88_fp_kind, 0.85_fp_kind, 0.84_fp_kind, 0.83_fp_kind,    &
                         0.79_fp_kind, 0.79_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EM_AMSRE = (/0.84_fp_kind, 0.83_fp_kind, 0.82_fp_kind, 0.80_fp_kind,            &
                               0.78_fp_kind, 0.72_fp_kind, 0.72_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EM_AMSRE = (/0.95_fp_kind, 0.96_fp_kind, 0.96_fp_kind, 0.95_fp_kind,            &
                               0.91_fp_kind, 0.75_fp_kind, 0.75_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EM_AMSRE = (/0.80_fp_kind, 0.80_fp_kind, 0.80_fp_kind, 0.79_fp_kind,                  &
                         0.75_fp_kind, 0.70_fp_kind, 0.70_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EM_AMSRE = (/0.91_fp_kind, 0.90_fp_kind, 0.89_fp_kind, 0.87_fp_kind,        &
                                   0.82_fp_kind, 0.69_fp_kind, 0.69_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EM_AMSRE = (/0.90_fp_kind, 0.89_fp_kind, 0.85_fp_kind,0.82_fp_kind, 0.76_fp_kind, &
                         0.65_fp_kind, 0.65_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EM_AMSRE = (/0.89_fp_kind, 0.88_fp_kind, 0.86_fp_kind, 0.83_fp_kind, 0.78_fp_kind,    &
                          0.70_fp_kind, 0.70_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EM_AMSRE = (/0.88_fp_kind, 0.86_fp_kind, 0.80_fp_kind, 0.75_fp_kind, 0.69_fp_kind,   &
                           0.67_fp_kind, 0.67_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EM_AMSRE = (/0.96_fp_kind, 0.97_fp_kind, 0.92_fp_kind, 0.87_fp_kind, 0.72_fp_kind,  &
                           0.50_fp_kind, 0.50_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EM_AMSRE = (/0.93_fp_kind, 0.94_fp_kind, 0.89_fp_kind, 0.85_fp_kind,        &
                                   0.74_fp_kind, 0.48_fp_kind, 0.48_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EM_AMSRE = (/0.88_fp_kind, 0.88_fp_kind, 0.87_fp_kind, 0.85_fp_kind,           &
                                0.77_fp_kind, 0.52_fp_kind, 0.52_fp_kind/)



 ! Define sixteen MW H-POL emissivity spectra for AMSRE ALGORITHMS


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EH_AMSRE = (/0.93_fp_kind, 0.92_fp_kind, 0.93_fp_kind, 0.94_fp_kind, 0.93_fp_kind,     &
                        0.93_fp_kind, 0.90_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EH_AMSRE = (/0.91_fp_kind, 0.90_fp_kind, 0.90_fp_kind, 0.90_fp_kind,           &
                                0.91_fp_kind, 0.90_fp_kind, 0.85_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EH_AMSRE = (/0.85_fp_kind, 0.85_fp_kind, 0.84_fp_kind, 0.84_fp_kind, 0.82_fp_kind,    &
                         0.80_fp_kind, 0.80_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EH_AMSRE = (/0.90_fp_kind, 0.90_fp_kind, 0.92_fp_kind, 0.92_fp_kind, 0.90_fp_kind,  &
                           0.80_fp_kind, 0.79_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EH_AMSRE = (/0.82_fp_kind, 0.81_fp_kind, 0.77_fp_kind, 0.76_fp_kind, 0.74_fp_kind,    &
                         0.74_fp_kind, 0.74_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EH_AMSRE = (/0.84_fp_kind, 0.83_fp_kind, 0.80_fp_kind, 0.78_fp_kind, 0.77_fp_kind,   &
                         0.75_fp_kind, 0.69_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EH_AMSRE = (/0.77_fp_kind, 0.77_fp_kind, 0.76_fp_kind, 0.75_fp_kind, 0.73_fp_kind,    &
                         0.71_fp_kind, 0.71_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EH_AMSRE = (/0.95_fp_kind, 0.94_fp_kind, 0.95_fp_kind, 0.94_fp_kind,            &
                               0.89_fp_kind, 0.75_fp_kind, 0.65_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EH_AMSRE = (/0.73_fp_kind, 0.73_fp_kind, 0.74_fp_kind, 0.72_fp_kind, 0.71_fp_kind,    &
                         0.68_fp_kind, 0.67_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EH_AMSRE = (/0.88_fp_kind, 0.87_fp_kind, 0.86_fp_kind, 0.85_fp_kind,        &
                                   0.80_fp_kind, 0.68_fp_kind, 0.63_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EH_AMSRE = (/0.86_fp_kind, 0.84_fp_kind, 0.80_fp_kind, 0.78_fp_kind,               &
                            0.72_fp_kind, 0.62_fp_kind, 0.57_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EH_AMSRE = (/0.87_fp_kind, 0.85_fp_kind, 0.83_fp_kind, 0.80_fp_kind, 0.77_fp_kind,    &
                         0.68_fp_kind, 0.62_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EH_AMSRE = (/0.82_fp_kind, 0.78_fp_kind, 0.74_fp_kind, 0.71_fp_kind, 0.67_fp_kind,   &
                          0.64_fp_kind, 0.64_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EH_AMSRE = (/0.90_fp_kind, 0.90_fp_kind, 0.89_fp_kind, 0.88_fp_kind, 0.83_fp_kind,  &
                           0.53_fp_kind, 0.48_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EH_AMSRE = (/0.87_fp_kind, 0.85_fp_kind, 0.84_fp_kind, 0.82_fp_kind,        &
                                   0.74_fp_kind, 0.53_fp_kind, 0.49_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EH_AMSRE = (/0.85_fp_kind, 0.84_fp_kind, 0.83_fp_kind, 0.81_fp_kind,           &
                                0.79_fp_kind, 0.51_fp_kind, 0.46_fp_kind/)



 ! Define sixteen MW V-POL emissivity spectra for AMSRE ALGORITHMS

 REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  WET_SNOW_EV_AMSRE = (/0.96_fp_kind, 0.94_fp_kind, 0.96_fp_kind, 0.95_fp_kind, 0.94_fp_kind,     &
                        0.94_fp_kind, 0.91_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  GRASS_AFTER_SNOW_EV_AMSRE = (/0.96_fp_kind, 0.94_fp_kind, 0.95_fp_kind, 0.96_fp_kind,           &
                                0.96_fp_kind, 0.92_fp_kind, 0.87_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_A_EV_AMSRE = (/0.99_fp_kind, 0.97_fp_kind, 0.96_fp_kind, 0.96_fp_kind, 0.93_fp_kind,    &
                         0.87_fp_kind, 0.87_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  POWDER_SNOW_EV_AMSRE = (/0.98_fp_kind, 0.97_fp_kind, 0.99_fp_kind, 0.98_fp_kind, 0.96_fp_kind,  &
                           0.84_fp_kind, 0.83_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_B_EV_AMSRE = (/0.97_fp_kind, 0.95_fp_kind, 0.93_fp_kind, 0.92_fp_kind, 0.89_fp_kind,    &
                         0.84_fp_kind,0.84_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_C_EV_AMSRE = (/1.00_fp_kind, 0.97_fp_kind, 0.96_fp_kind, 0.94_fp_kind, 0.91_fp_kind,    &
                         0.84_fp_kind, 0.78_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_D_EV_AMSRE = (/0.99_fp_kind, 0.96_fp_kind, 0.93_fp_kind, 0.90_fp_kind, 0.86_fp_kind,    &
                         0.80_fp_kind, 0.80_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THIN_CRUST_SNOW_EV_AMSRE = (/0.98_fp_kind, 0.97_fp_kind, 0.98_fp_kind, 0.97_fp_kind,            &
                               0.92_fp_kind, 0.77_fp_kind, 0.67_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  RS_SNOW_E_EV_AMSRE = (/0.98_fp_kind, 0.95_fp_kind, 0.90_fp_kind, 0.86_fp_kind, 0.82_fp_kind,    &
                         0.74_fp_kind, 0.73_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_A_EV_AMSRE = (/0.96_fp_kind, 0.95_fp_kind, 0.95_fp_kind, 0.93_fp_kind,        &
                                   0.87_fp_kind, 0.71_fp_kind, 0.66_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  SHALLOW_SNOW_EV_AMSRE = (/0.97_fp_kind, 0.95_fp_kind, 0.94_fp_kind, 0.90_fp_kind, 0.84_fp_kind, &
                            0.68_fp_kind, 0.63_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  DEEP_SNOW_EV_AMSRE = (/0.96_fp_kind, 0.94_fp_kind, 0.92_fp_kind, 0.90_fp_kind, 0.85_fp_kind,    &
                         0.77_fp_kind, 0.71_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  CRUST_SNOW_EV_AMSRE = (/0.98_fp_kind, 0.96_fp_kind, 0.93_fp_kind, 0.90_fp_kind, 0.81_fp_kind,   &
                          0.71_fp_kind, 0.71_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  MEDIUM_SNOW_EV_AMSRE = (/0.99_fp_kind, 0.97_fp_kind, 0.98_fp_kind, 0.96_fp_kind, 0.92_fp_kind,  &
                           0.57_fp_kind, 0.52_fp_kind/)


  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  BOTTOM_CRUST_SNOW_B_EV_AMSRE = (/1.00_fp_kind, 0.97_fp_kind, 0.97_fp_kind, 0.95_fp_kind,        &
                                   0.86_fp_kind, 0.58_fp_kind, 0.54_fp_kind/)

  REAL(fp_kind), PUBLIC, PARAMETER, DIMENSION(N_FREQ_AMSRE)  ::                                   &
  THICK_CRUST_SNOW_EV_AMSRE = (/0.98_fp_kind, 0.96_fp_kind, 0.96_fp_kind, 0.94_fp_kind,           &
                                0.89_fp_kind, 0.56_fp_kind, 0.51_fp_kind/)


END MODULE NESDIS_SnowEM_Parameters
