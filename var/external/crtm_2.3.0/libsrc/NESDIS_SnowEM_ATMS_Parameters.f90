!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_SnowEM_ATMS_Parameters Module
!
! PURPOSE:
!       Module containing the snow emissivity library ATMS channels. The library contain 16
!       pre-defined snow spectrum which characterize 16 basic snow types. This library is used
!       together with a snow-typing algorithm to implement the library-based snow emissivity modeling.
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
!       Type_Kinds:     Module containing definitions for kinds of variable types.
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
!       Written by:     Ming Chen,IMSG Inc., ming.chen@noaa.gov (04-28-2012)
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!
!  Copyright (C) 2012 Fuzhong Weng and Ming Chen
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

MODULE NESDIS_SnowEM_ATMS_Parameters

   USE NESDIS_SnowEM_Parameters
   USE Type_Kinds, ONLY: fp
   IMPLICIT NONE


   INTEGER, PUBLIC, PARAMETER :: N_FREQ_ATMS  = 13
   INTEGER, PUBLIC, PARAMETER :: N_SNOW_TYPES = 16

   CHARACTER(LEN=20),DIMENSION(N_SNOW_TYPES), &
   PUBLIC, PARAMETER :: SNOW_TYPE_NAMES=(/ &
        'WET_SNOW            ',&                    !1
        'GRASS_AFTER_SNOW    ',&                    !2
        'RS_SNOW_A           ',&                    !3
        'POWDER_SNOW         ',&                    !4
        'RS_SNOW_B           ',&                    !5
        'RS_SNOW_C           ',&                    !6
        'RS_SNOW_D           ',&                    !7
        'THIN_CRUST_SNOW     ',&                    !8
        'RS_SNOW_E           ',&                    !9
        'BOTTOM_CRUST_SNOW_A ',&                    !10
        'SHALLOW_SNOW        ',&                    !11
        'DEEP_SNOW           ',&                    !12
        'CRUST_SNOW          ',&                    !13
        'MEDIUM_SNOW         ',&                    !14
        'BOTTOM_CRUST_SNOW_B ',&                    !15
        'THICK_CRUST_SNOW    '/)                    !16

   INTEGER,PUBLIC,PARAMETER, DIMENSION(N_SNOW_TYPES) :: Code2Excel_Idx =  &
         (/2, 1,  4, 11, 14, 12,  8, 16, 10, 15, 13,  5,  6,  3,  7,  9/)
   INTEGER,PUBLIC,PARAMETER, DIMENSION(N_SNOW_TYPES) :: Excel2Code_Idx =  &
         (/2, 1, 14,  3, 12, 13, 15,  7, 16,  9,  4,  6, 11,  5,  10, 8/)

   REAL(fp),PUBLIC,PARAMETER, DIMENSION(N_FREQ_ATMS) :: FREQUENCY_ATMS = &
       (/23.80_fp,31.40_fp,50.30_fp,51.76_fp,52.80_fp,53.60_fp,54.40_fp, &
         54.90_fp,55.50_fp,57.30_fp,88.20_fp,165.50_fp,183.30_fp/)


  ! Define sixteen MW H-Pol emissivity spectra for ATMS ALGORITHMS
    REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SNOW_TYPES) :: SNOW_EMISS_ATMS_H  = RESHAPE((/   &     !Excel
      0.94_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.89_fp,0.88_fp,& !1
      0.90_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.90_fp,0.84_fp,0.82_fp,& !2
      0.84_fp,0.83_fp,0.81_fp,0.81_fp,0.81_fp,0.81_fp,0.81_fp,0.81_fp,0.81_fp,0.81_fp,0.80_fp,0.80_fp,0.80_fp,& !3
      0.92_fp,0.91_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.86_fp,0.86_fp,0.86_fp,0.80_fp,0.79_fp,0.78_fp,& !4
      0.76_fp,0.75_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,& !5
      0.78_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.67_fp,0.66_fp,& !6
      0.75_fp,0.74_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.71_fp,0.71_fp,0.71_fp,& !7
      0.94_fp,0.91_fp,0.85_fp,0.85_fp,0.85_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.83_fp,0.75_fp,0.62_fp,0.60_fp,& !8
      0.72_fp,0.71_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.68_fp,0.67_fp,0.66_fp,& !9
      0.85_fp,0.82_fp,0.77_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.68_fp,0.62_fp,0.60_fp,& !10
      0.78_fp,0.74_fp,0.69_fp,0.69_fp,0.69_fp,0.69_fp,0.69_fp,0.68_fp,0.68_fp,0.68_fp,0.62_fp,0.56_fp,0.54_fp,& !11
      0.80_fp,0.78_fp,0.75_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.73_fp,0.68_fp,0.60_fp,0.59_fp,& !12
      0.71_fp,0.69_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.64_fp,0.64_fp,0.64_fp,& !13
      0.88_fp,0.85_fp,0.75_fp,0.74_fp,0.74_fp,0.73_fp,0.73_fp,0.72_fp,0.72_fp,0.71_fp,0.53_fp,0.47_fp,0.45_fp,& !14
      0.82_fp,0.77_fp,0.68_fp,0.68_fp,0.67_fp,0.67_fp,0.67_fp,0.67_fp,0.66_fp,0.66_fp,0.53_fp,0.48_fp,0.47_fp,& !15
      0.81_fp,0.80_fp,0.72_fp,0.71_fp,0.70_fp,0.70_fp,0.69_fp,0.69_fp,0.69_fp,0.68_fp,0.51_fp,0.45_fp,0.43_fp & !16
       /),(/N_FREQ_ATMS,N_SNOW_TYPES/))


   ! Define sixteen MW V-Pol emissivity spectra for ATMS ALGORITHMS
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SNOW_TYPES)  :: SNOW_EMISS_ATMS_V  = RESHAPE((/   &    !Excel
      0.95_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.90_fp,0.89_fp,& !1
      0.96_fp,0.96_fp,0.95_fp,0.95_fp,0.95_fp,0.95_fp,0.95_fp,0.95_fp,0.95_fp,0.94_fp,0.92_fp,0.86_fp,0.84_fp,& !2
      0.96_fp,0.94_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.87_fp,0.87_fp,0.87_fp,& !3
      0.98_fp,0.97_fp,0.93_fp,0.93_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.91_fp,0.84_fp,0.83_fp,0.82_fp,& !4
      0.92_fp,0.90_fp,0.88_fp,0.88_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.84_fp,0.84_fp,0.84_fp,& !5
      0.94_fp,0.92_fp,0.89_fp,0.89_fp,0.89_fp,0.89_fp,0.89_fp,0.89_fp,0.88_fp,0.88_fp,0.84_fp,0.76_fp,0.75_fp,& !6
      0.90_fp,0.88_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.80_fp,0.80_fp,0.80_fp,& !7
      0.97_fp,0.94_fp,0.88_fp,0.88_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.86_fp,0.77_fp,0.64_fp,0.62_fp,& !8
      0.86_fp,0.84_fp,0.80_fp,0.80_fp,0.80_fp,0.79_fp,0.79_fp,0.79_fp,0.79_fp,0.79_fp,0.74_fp,0.73_fp,0.72_fp,& !9
      0.93_fp,0.89_fp,0.83_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.81_fp,0.81_fp,0.81_fp,0.71_fp,0.65_fp,0.63_fp,& !10
      0.90_fp,0.86_fp,0.80_fp,0.79_fp,0.79_fp,0.79_fp,0.79_fp,0.78_fp,0.78_fp,0.78_fp,0.68_fp,0.62_fp,0.60_fp,& !11
      0.90_fp,0.87_fp,0.83_fp,0.83_fp,0.83_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.77_fp,0.69_fp,0.68_fp,& !12
      0.90_fp,0.85_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp,0.77_fp,0.71_fp,0.71_fp,0.71_fp,& !13
      0.96_fp,0.94_fp,0.83_fp,0.82_fp,0.81_fp,0.81_fp,0.80_fp,0.80_fp,0.79_fp,0.78_fp,0.58_fp,0.51_fp,0.49_fp,& !14
      0.95_fp,0.90_fp,0.79_fp,0.78_fp,0.77_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.58_fp,0.53_fp,0.52_fp,& !15
      0.94_fp,0.91_fp,0.80_fp,0.79_fp,0.79_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp,0.76_fp,0.57_fp,0.50_fp,0.48_fp & !16
      /),(/N_FREQ_ATMS,N_SNOW_TYPES/))


   ! Define sixteen Mixed MW emissivity spectra for ATMS ALGORITHMS
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SNOW_TYPES)  :: SNOW_EMISS_ATMS_LIB  = RESHAPE((/   &
      0.94_fp,0.94_fp,0.94_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.92_fp,0.89_fp,0.89_fp,& !1
      0.90_fp,0.90_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.85_fp,0.83_fp,& !2
      0.86_fp,0.86_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.82_fp,0.82_fp,0.82_fp,& !3
      0.93_fp,0.93_fp,0.89_fp,0.88_fp,0.88_fp,0.88_fp,0.88_fp,0.87_fp,0.87_fp,0.87_fp,0.79_fp,0.79_fp,0.79_fp,& !4
      0.84_fp,0.83_fp,0.83_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.82_fp,0.79_fp,0.71_fp,0.70_fp,& !5
      0.80_fp,0.79_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp,0.77_fp,& !6
      0.78_fp,0.77_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.72_fp,0.72_fp,0.72_fp,& !7
      0.95_fp,0.93_fp,0.87_fp,0.86_fp,0.86_fp,0.86_fp,0.85_fp,0.85_fp,0.85_fp,0.84_fp,0.74_fp,0.63_fp,0.60_fp,& !8
      0.76_fp,0.76_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.74_fp,0.70_fp,0.69_fp,0.68_fp,& !9
      0.73_fp,0.68_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.68_fp,0.67_fp,0.66_fp,& !10
      0.86_fp,0.82_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.75_fp,0.69_fp,0.63_fp,0.61_fp,& !11
      0.81_fp,0.77_fp,0.74_fp,0.73_fp,0.73_fp,0.73_fp,0.73_fp,0.73_fp,0.73_fp,0.72_fp,0.69_fp,0.63_fp,0.61_fp,& !12
      0.82_fp,0.78_fp,0.69_fp,0.68_fp,0.68_fp,0.67_fp,0.67_fp,0.67_fp,0.67_fp,0.66_fp,0.51_fp,0.46_fp,0.45_fp,& !13
      0.80_fp,0.75_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.70_fp,0.69_fp,0.64_fp,0.59_fp,0.58_fp,& !14
      0.84_fp,0.76_fp,0.66_fp,0.65_fp,0.64_fp,0.64_fp,0.63_fp,0.63_fp,0.63_fp,0.62_fp,0.48_fp,0.43_fp,0.42_fp,& !15
      0.86_fp,0.74_fp,0.63_fp,0.63_fp,0.63_fp,0.63_fp,0.62_fp,0.62_fp,0.62_fp,0.61_fp,0.50_fp,0.44_fp,0.42_fp & !16
     /),(/N_FREQ_ATMS,N_SNOW_TYPES/))


   ! Define sixteen MW weighted emissivity spectra for AMSU ALGORITHMS *NESDIS_SnowEM_Parameters Module
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQUENCY,N_SNOW_TYPES)  :: SNOW_EMISS_DEFAULT_LIB  = &      !Col_inx in Excel
     RESHAPE((/ &
      0.87_fp,0.89_fp,0.91_fp,0.93_fp,0.94_fp,0.94_fp,0.94_fp,0.93_fp,0.92_fp,0.90_fp, &                         !2
      0.91_fp,0.91_fp,0.92_fp,0.91_fp,0.90_fp,0.90_fp,0.91_fp,0.91_fp,0.91_fp,0.86_fp, &                         !1
      0.90_fp,0.89_fp,0.88_fp,0.87_fp,0.86_fp,0.86_fp,0.85_fp,0.85_fp,0.82_fp,0.82_fp, &                         !4
      0.91_fp,0.91_fp,0.93_fp,0.93_fp,0.93_fp,0.93_fp,0.89_fp,0.88_fp,0.79_fp,0.79_fp, &                         !11
      0.90_fp,0.89_fp,0.88_fp,0.85_fp,0.84_fp,0.83_fp,0.83_fp,0.82_fp,0.79_fp,0.73_fp, &                         !14
      0.90_fp,0.89_fp,0.86_fp,0.82_fp,0.80_fp,0.79_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp, &                         !12
      0.88_fp,0.86_fp,0.85_fp,0.80_fp,0.78_fp,0.77_fp,0.77_fp,0.76_fp,0.72_fp,0.72_fp, &                         !8
      0.93_fp,0.94_fp,0.96_fp,0.96_fp,0.95_fp,0.93_fp,0.87_fp,0.86_fp,0.74_fp,0.65_fp, &                         !16
      0.87_fp,0.86_fp,0.84_fp,0.80_fp,0.76_fp,0.76_fp,0.75_fp,0.75_fp,0.70_fp,0.69_fp, &                         !10
      0.87_fp,0.86_fp,0.83_fp,0.77_fp,0.73_fp,0.68_fp,0.66_fp,0.66_fp,0.68_fp,0.67_fp, &                         !15
      0.89_fp,0.89_fp,0.88_fp,0.87_fp,0.86_fp,0.82_fp,0.77_fp,0.76_fp,0.69_fp,0.64_fp, &                         !13
      0.88_fp,0.87_fp,0.86_fp,0.83_fp,0.81_fp,0.77_fp,0.74_fp,0.73_fp,0.69_fp,0.64_fp, &                         !5
      0.86_fp,0.86_fp,0.86_fp,0.85_fp,0.82_fp,0.78_fp,0.69_fp,0.68_fp,0.51_fp,0.47_fp, &                         !6
      0.89_fp,0.88_fp,0.87_fp,0.83_fp,0.80_fp,0.75_fp,0.70_fp,0.70_fp,0.64_fp,0.60_fp, &                         !3
      0.91_fp,0.92_fp,0.93_fp,0.88_fp,0.84_fp,0.76_fp,0.66_fp,0.64_fp,0.48_fp,0.44_fp, &                         !7
      0.94_fp,0.95_fp,0.97_fp,0.91_fp,0.86_fp,0.74_fp,0.63_fp,0.63_fp,0.50_fp,0.45_fp  &                         !9
      /),(/N_FREQUENCY,N_SNOW_TYPES/))

   ! Define sixteen Mixed MW emissivity spectra for ATMS ALGORITHMS (from the pol-spectra)
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SNOW_TYPES)  :: SNOW_EMISS_ATMS_LIB_2  = RESHAPE((/   &
     0.945_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.935_fp,0.895_fp,0.885_fp,&
     0.930_fp,0.935_fp,0.930_fp,0.930_fp,0.930_fp,0.930_fp,0.930_fp,0.930_fp,0.930_fp,0.925_fp,0.910_fp,0.850_fp,0.830_fp,&
     0.900_fp,0.885_fp,0.860_fp,0.860_fp,0.860_fp,0.860_fp,0.860_fp,0.860_fp,0.860_fp,0.860_fp,0.835_fp,0.835_fp,0.835_fp,&
     0.950_fp,0.940_fp,0.900_fp,0.900_fp,0.895_fp,0.895_fp,0.895_fp,0.890_fp,0.890_fp,0.885_fp,0.820_fp,0.810_fp,0.800_fp,&
     0.840_fp,0.825_fp,0.810_fp,0.810_fp,0.805_fp,0.805_fp,0.805_fp,0.805_fp,0.805_fp,0.805_fp,0.790_fp,0.790_fp,0.790_fp,&
     0.860_fp,0.845_fp,0.825_fp,0.825_fp,0.825_fp,0.825_fp,0.825_fp,0.825_fp,0.820_fp,0.820_fp,0.795_fp,0.715_fp,0.705_fp,&
     0.825_fp,0.810_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.755_fp,0.755_fp,0.755_fp,&
     0.955_fp,0.925_fp,0.865_fp,0.865_fp,0.860_fp,0.855_fp,0.855_fp,0.855_fp,0.855_fp,0.845_fp,0.760_fp,0.630_fp,0.610_fp,&
     0.790_fp,0.775_fp,0.750_fp,0.750_fp,0.750_fp,0.745_fp,0.745_fp,0.745_fp,0.745_fp,0.745_fp,0.710_fp,0.700_fp,0.690_fp,&
     0.890_fp,0.855_fp,0.800_fp,0.795_fp,0.790_fp,0.790_fp,0.790_fp,0.785_fp,0.785_fp,0.780_fp,0.695_fp,0.635_fp,0.615_fp,&
     0.840_fp,0.800_fp,0.745_fp,0.740_fp,0.740_fp,0.740_fp,0.740_fp,0.730_fp,0.730_fp,0.730_fp,0.650_fp,0.590_fp,0.570_fp,&
     0.850_fp,0.825_fp,0.790_fp,0.785_fp,0.785_fp,0.780_fp,0.780_fp,0.780_fp,0.780_fp,0.775_fp,0.725_fp,0.645_fp,0.635_fp,&
     0.805_fp,0.770_fp,0.720_fp,0.720_fp,0.720_fp,0.720_fp,0.720_fp,0.715_fp,0.715_fp,0.715_fp,0.675_fp,0.675_fp,0.675_fp,&
     0.920_fp,0.895_fp,0.790_fp,0.780_fp,0.775_fp,0.770_fp,0.765_fp,0.760_fp,0.755_fp,0.745_fp,0.555_fp,0.490_fp,0.470_fp,&
     0.885_fp,0.835_fp,0.735_fp,0.730_fp,0.720_fp,0.720_fp,0.715_fp,0.715_fp,0.710_fp,0.705_fp,0.555_fp,0.505_fp,0.495_fp,&
     0.875_fp,0.855_fp,0.760_fp,0.750_fp,0.745_fp,0.740_fp,0.735_fp,0.730_fp,0.730_fp,0.720_fp,0.540_fp,0.475_fp,0.455_fp &
      /),(/N_FREQ_ATMS,N_SNOW_TYPES/))


CONTAINS


   FUNCTION SnowType_Name2Index(sname) RESULT(sindex)
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: sname
      ! Function result
      INTEGER :: sindex
      INTEGER :: Idx


      sindex=INVALID_SNOW_TYPE
      DO Idx=1, N_SNOW_TYPES
         IF(TRIM(sname) .EQ. TRIM(SNOW_TYPE_NAMES(Idx))) THEN
           sindex=Idx
         EXIT
         ENDIF
      ENDDO
      RETURN
    END FUNCTION SnowType_Name2Index


    FUNCTION SnowType_Index2Name(sindex) RESULT(sname)
      ! Arguments
      INTEGER, INTENT(IN)  :: sindex
      ! Function result
      CHARACTER(LEN=100)   :: sname

      sname=TRIM(SNOW_TYPE_NAMES(sindex))
      RETURN
   END FUNCTION SnowType_Index2Name

END MODULE NESDIS_SnowEM_ATMS_Parameters
