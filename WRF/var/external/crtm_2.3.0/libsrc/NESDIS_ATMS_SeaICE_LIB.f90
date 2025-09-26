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

MODULE NESDIS_ATMS_SeaIce_LIB
   
   USE Type_Kinds, ONLY: fp, ip
   IMPLICIT NONE


   INTEGER(ip), PUBLIC, PARAMETER :: N_FREQ_ATMS  = 13
   INTEGER(ip), PUBLIC, PARAMETER :: N_SEAICE_TYPES = 13
   INTEGER(ip), PUBLIC, PARAMETER :: N_FREQ_AMSRE = 7
   INTEGER(ip), PUBLIC, PARAMETER :: INVALID_SEAICE_TYPE = -1
   
   CHARACTER(LEN=20),DIMENSION(N_SEAICE_TYPES), &
   PUBLIC, PARAMETER :: SEAICE_TYPE_NAMES=(/ &
        'RS_ICE_A_EMISS      ',&                    !1
        'RS_ICE_B_EMISS      ',&                    !2
        'MIXED_NEWICE_SNOW_EM',&                    !3
        'NARE_NEWICE_EMISS   ',&                    !4
        'BROKEN_ICE_EMISS    ',&                    !5
        'FIRST_YEAR_ICE_EMISS',&                    !6
        'COMPOSITE_PACK_ICE  ',&                    !7
        'RS_ICE_C_EMISS      ',&                    !8
        'FAST_ICE_EMISS      ',&                    !9	
        'RS_ICE_D_EMISS      ',&                    !10
        'RS_ICE_E_EMISS      ',&                    !11
        'RS_ICE_F_EMISS      ',&                    !12
        'GREASE_ICE_EMISS    '/)                    !13

   REAL(fp),PUBLIC,PARAMETER, DIMENSION(N_FREQ_ATMS) :: FREQUENCY_ATMS = &
       (/23.80_fp,31.40_fp,50.30_fp,51.76_fp,52.80_fp,53.60_fp,54.40_fp, &
         54.90_fp,55.50_fp,57.30_fp,88.20_fp,165.50_fp,183.30_fp/)
   

  ! Define sixteen MW H-Pol emissivity spectra for ATMS ALGORITHMS   
    REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SEAICE_TYPES) :: SEAICE_EMISS_ATMS_H  = RESHAPE((/   &     
     0.94_fp,0.95_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.94_fp,0.92_fp,0.91_fp,0.91_fp,& !1
     0.86_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.88_fp,0.87_fp,0.87_fp,& !2
     0.85_fp,0.84_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.82_fp,0.80_fp,0.79_fp,& !3
     0.76_fp,0.77_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.73_fp,0.73_fp,0.73_fp,& !4
     0.77_fp,0.79_fp,0.78_fp,0.78_fp,0.78_fp,0.77_fp,0.77_fp,0.77_fp,0.77_fp,0.77_fp,0.72_fp,0.69_fp,0.68_fp,& !5
     0.88_fp,0.87_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.82_fp,0.82_fp,0.82_fp,0.76_fp,0.66_fp,0.64_fp,& !6
     0.82_fp,0.80_fp,0.76_fp,0.76_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.74_fp,0.67_fp,0.56_fp,0.53_fp,& !7
     0.78_fp,0.73_fp,0.67_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.65_fp,0.60_fp,0.56_fp,0.54_fp,& !8
     0.76_fp,0.74_fp,0.69_fp,0.69_fp,0.68_fp,0.68_fp,0.68_fp,0.68_fp,0.68_fp,0.67_fp,0.60_fp,0.52_fp,0.50_fp,& !9
     0.70_fp,0.70_fp,0.67_fp,0.67_fp,0.67_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.66_fp,0.59_fp,0.53_fp,0.52_fp,& !10
     0.61_fp,0.62_fp,0.63_fp,0.63_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.67_fp,0.69_fp,0.70_fp,& !11
     0.57_fp,0.59_fp,0.61_fp,0.61_fp,0.61_fp,0.61_fp,0.61_fp,0.61_fp,0.61_fp,0.62_fp,0.64_fp,0.65_fp,0.65_fp,& !12
     0.45_fp,0.47_fp,0.50_fp,0.50_fp,0.51_fp,0.51_fp,0.51_fp,0.51_fp,0.51_fp,0.51_fp,0.54_fp,0.56_fp,0.57_fp & !13
       /),(/N_FREQ_ATMS,N_SEAICE_TYPES/)) 
      

   ! Define sixteen MW V-Pol emissivity spectra for ATMS ALGORITHMS     
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SEAICE_TYPES)  :: SEAICE_EMISS_ATMS_V  = RESHAPE((/   &    
      0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.99_fp,0.98_fp,0.97_fp,0.97_fp,& !1
      0.98_fp,0.97_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.94_fp,0.93_fp,0.93_fp,& !2
      0.94_fp,0.93_fp,0.92_fp,0.92_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.88_fp,0.86_fp,0.85_fp,& !3
      0.91_fp,0.91_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.88_fp,0.88_fp,0.88_fp,& !4
      0.91_fp,0.91_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.90_fp,0.89_fp,0.87_fp,0.84_fp,0.83_fp,& !5
      0.97_fp,0.96_fp,0.92_fp,0.92_fp,0.92_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.91_fp,0.84_fp,0.74_fp,0.72_fp,& !6
      0.93_fp,0.91_fp,0.85_fp,0.84_fp,0.84_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.82_fp,0.72_fp,0.61_fp,0.58_fp,& !7
      0.86_fp,0.79_fp,0.73_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.71_fp,0.66_fp,0.62_fp,0.60_fp,& !8
      0.91_fp,0.87_fp,0.81_fp,0.80_fp,0.80_fp,0.80_fp,0.80_fp,0.79_fp,0.79_fp,0.79_fp,0.69_fp,0.61_fp,0.59_fp,& !9
      0.88_fp,0.88_fp,0.85_fp,0.85_fp,0.85_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.77_fp,0.71_fp,0.70_fp,& !10
      0.82_fp,0.83_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.85_fp,0.86_fp,0.88_fp,0.89_fp,& !11
      0.78_fp,0.80_fp,0.82_fp,0.82_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.86_fp,0.87_fp,0.87_fp,& !12
      0.72_fp,0.74_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.78_fp,0.82_fp,0.84_fp,0.85_fp & !13
      /),(/N_FREQ_ATMS,N_SEAICE_TYPES/))
      

   ! Define sixteen Mixed MW emissivity spectra for ATMS ALGORITHMS     
     REAL(fp),PUBLIC,PARAMETER,DIMENSION(N_FREQ_ATMS,N_SEAICE_TYPES)  :: SEAICE_EMISS_ATMS_LIB  = RESHAPE((/   &
    0.97_fp,0.97_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.96_fp,0.95_fp,0.94_fp,0.94_fp,& !1
    0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.92_fp,0.91_fp,0.90_fp,0.90_fp,& !2
    0.90_fp,0.89_fp,0.88_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.85_fp,0.83_fp,0.82_fp,& !3
    0.84_fp,0.84_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.81_fp,0.80_fp,0.80_fp,& !4
    0.84_fp,0.85_fp,0.84_fp,0.84_fp,0.84_fp,0.84_fp,0.83_fp,0.83_fp,0.83_fp,0.83_fp,0.80_fp,0.76_fp,0.75_fp,& !5
    0.93_fp,0.91_fp,0.88_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.87_fp,0.86_fp,0.80_fp,0.70_fp,0.68_fp,& !6
    0.88_fp,0.85_fp,0.80_fp,0.80_fp,0.79_fp,0.79_fp,0.79_fp,0.79_fp,0.79_fp,0.78_fp,0.70_fp,0.58_fp,0.56_fp,& !7
    0.82_fp,0.76_fp,0.70_fp,0.69_fp,0.69_fp,0.69_fp,0.69_fp,0.69_fp,0.69_fp,0.68_fp,0.63_fp,0.59_fp,0.57_fp,& !8
    0.84_fp,0.81_fp,0.75_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.73_fp,0.73_fp,0.65_fp,0.57_fp,0.55_fp,& !9
    0.79_fp,0.79_fp,0.76_fp,0.76_fp,0.76_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.75_fp,0.68_fp,0.62_fp,0.61_fp,& !10
    0.72_fp,0.72_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.74_fp,0.76_fp,0.79_fp,0.79_fp,& !11
    0.68_fp,0.69_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.72_fp,0.75_fp,0.76_fp,0.76_fp,& !12
    0.59_fp,0.61_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.64_fp,0.65_fp,0.68_fp,0.70_fp,0.71_fp & !13
     /),(/N_FREQ_ATMS,N_SEAICE_TYPES/)) 
 

CONTAINS
    
   FUNCTION SeaIceType_Name2Index(sname) RESULT(sindex)
      ! Arguments
      CHARACTER(LEN=*), INTENT(IN) :: sname
      ! Function result
      INTEGER :: sindex
      INTEGER :: Idx
         
      sindex=INVALID_SEAICE_TYPE
      DO Idx=1, N_SEAICE_TYPES
         IF(TRIM(sname) .EQ. TRIM(SeaIce_TYPE_NAMES(Idx))) THEN
           sindex=Idx
           EXIT
         ENDIF
      ENDDO
      RETURN
    END FUNCTION SeaIceType_Name2Index
    
    
    FUNCTION SeaIceType_Index2Name(sindex) RESULT(sname)
      ! Arguments
      INTEGER, INTENT(IN)  :: sindex
      ! Function result
      CHARACTER(LEN=100)   :: sname
      
      sname=TRIM(SeaIce_TYPE_NAMES(sindex))
      RETURN
   END FUNCTION SeaIceType_Index2Name

END MODULE NESDIS_ATMS_SeaIce_LIB
