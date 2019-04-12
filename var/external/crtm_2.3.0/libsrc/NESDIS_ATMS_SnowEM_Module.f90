!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_ATMS_SnowEM_Module
!
! PURPOSE:
!       Module containing the snow-typing algorithms. A general interface is used to call one of the
!       snow-typing algorithms in !terms of the input arguments. This Module is used together with
!       NESDIS_SnowEM_ATMS_Parameters Module to implement the library-based snow emissivity  model.
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       Surface : MW Surface Snow Emissivity of ATMS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
!       USE NESDIS_ATMS_SnowEM_Module
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds of variable types
!
!       NESDIS_LandEM_Module:     Module containing the microwave land emissivity model
!
!       NESDIS_SnowEM_Parameters: Module containing the predefined microwave snow emissivity spectra
!
! CONTAINS:
!
! PUBLIC SUNPROGRAMS:
!
!       NESDIS_ATMS_SNOWEM:       Subroutine to calculate the microwave snow emissivity from ATMS
!
!
! PRIVATE SUBPROGRAMS:
!       These subroutines are used to determine the snow types from the brightness temperatures(TB)
!       of five ATMS window channels( 23.8 GHz, 31.4 GHz, 50.3 GHz, 88.2 GHz, 165.5 GHz) and/or
!       surface temperature plus snow depth. The five channels are further divided into two
!       groups: Group-1 ( 23.8 GHz, 31.4 GHz, 50.3 GHz, 88.2 GHz) and Group-2 (88.2 GHz, 165.5GHz),
!       corresponding to the window channels of AMSU-A and AMSU-B, respectively.
!       Different combinations of available ATMS window-channel and surface observations result
!       in differenet snow-typing algotrithms:
!
!       ATMS_SNOW_ByTB_A      : by ATMS TBs of Group-1 channels
!       ATMS_SNOW_ByTB_B      : by ATMS TBs of Group-2 channels
!       ATMS_SNOW_ByTBs       : by the TBs  of all the five ATMS channels
!       ATMS_SNOW_ByTBTs_A    : by ATMS TBs of Group-1 channels and surface temperature
!       ATMS_SNOW_ByTBTs_B    : by ATMS TBs of Group-2 channels and surface temperature
!       ATMS_SNOW_ByTBTs      : by the TBs  of all the five ATMS channels and surface temperature (regression-based)
!       ATMS_SNOW_ByTBTs_D    : by the TBs  of all the five ATMS channels and surface temperature (diagnosis-based)
!       ATMS_SNOW_ByTypes     : bydefault surface type (4)
!       ATMS_ALandEM_Snow     : Subroutine to initilize the vaiables to default values
!       em_initialization     : Subroutine to initialization snow emissivity
!       em_interpolate        : Subroutine to perform frequency interpolation of snow emissivity
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
!       Written by:     Ming Chen, IMSG Inc., Banghua.Yan@noaa.gov (04-28-2012)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
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

MODULE NESDIS_ATMS_SnowEM_Module

  USE Type_Kinds
  USE NESDIS_LandEM_Module
  USE NESDIS_SnowEM_ATMS_Parameters
  IMPLICIT NONE

! Visibilities
  PRIVATE
  PUBLIC  :: NESDIS_ATMS_SNOWEM


CONTAINS


   !################################################################################
   !################################################################################
   !##                                                                            ##
   !##                         ## PUBLIC MODULE ROUTINES ##                       ##
   !##                                                                            ##
   !################################################################################
   !################################################################################

   !-------------------------------------------------------------------------------------------------------------
   !
   ! NAME:
   !       NESDIS_ATMS_SNOWEM
   !
   ! PURPOSE:
   !       Subroutine to simulate microwave emissivity over snow conditions from ATMS measurements at window
   !       channels.
   !
   !
   ! CATEGORY:
   !       CRTM : Surface : MW SNOWEM
   !
   ! LANGUAGE:
   !       Fortran-95
   !
   ! CALLING SEQUENCE:
   !       CALL NESDIS_ATMS_SNOWEM
   !
   ! INPUT ARGUMENTS:
   !
   !         Frequency                Frequency User defines
   !                                  This is the "I" dimension
   !                                  UNITS:      GHz
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Scalar
   !
   !
   !         Satellite_Angle          The local zenith angle in degree for ATMS measurements.
   !                                  ** NOTE: THIS IS A MANDATORY MEMBER **
   !                                  **       OF THIS STRUCTURE          **
   !                                  UNITS:      Degrees
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Rank-1, (I)
   !
   !         User_Angle               The local angle value in degree user defines.
   !                                  ** NOTE: THIS IS A MANDATORY MEMBER **
   !                                  **       OF THIS STRUCTURE          **
   !                                  UNITS:      Degrees
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Rank-1, (I)
   !
   !
   !         Tbs                      BRIGHTNESS TEMPERATURES AT FIVE ATMS WINDOW CHANNELS
   !                                  UNITS:      Kelvin, K
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION   4*1 SCALAR
   !
   !                        WHICH ARE
   !                                  tbs[1] = TB at 23.8 GHz
   !                                  tbs[2] = TB at 31.4 GHz
   !                                  tbs[3] = TB at 50.3 GHz
   !                                  tbs[4] = TB at 88.2 GHz
   !                                  tbs[5] = TB at 165.5 GHz
   !
   !
   !         Tss = Land_Temperature:  The land surface temperature.
   !                                  UNITS:      Kelvin, K
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Scalar
   !
   !
   !         Snow_Depth:              The snow depth.
   !                                  UNITS:      mm
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Scalar
   !
   ! **** IMPORTANT NOTES ****
   !
   !        When one variable among  Tbs[],  Ts are not available, set -999.0
   !
   !
   !
   !
   ! OUTPUT ARGUMENTS:
   !
   !         Emissivity_H:            The surface emissivity at a horizontal polarization.
   !                                  ** NOTE: THIS IS A MANDATORY MEMBER **
   !                                  **       OF THIS STRUCTURE          **
   !                                  UNITS:      N/A
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Scalar
   !
   !         Emissivity_V:            The surface emissivity at a vertical polarization.
   !                                  ** NOTE: THIS IS A MANDATORY MEMBER **
   !                                  **       OF THIS STRUCTURE          **
   !                                  UNITS:      N/A
   !                                  TYPE:       REAL( fp )
   !                                  DIMENSION:  Scalar
   !
   !
   !
   !
   ! OPTIONAL OUTPUT ARGUMENTS:
   !
   !       snow_type  -  snow type (not output here)
   !                     1 : Wet Snow
   !                     2 : Grass_after_Snow
   !                     3 : RS_Snow (A)
   !                     4 : Powder Snow
   !                     5 : RS_Snow (B)
   !                     6 : RS_Snow (C)
   !                     7 : RS_Snow (D)
   !                     8 : Thin Crust Snow
   !                     9 : RS_Snow (E)
   !                     10: Bottom Crust Snow (A)
   !                     11: Shallow Snow
   !                     12: Deep Snow
   !                     13: Crust Snow
   !                     14: Medium Snow
   !                     15: Bottom Crust Snow (B)
   !                     16: Thick Crust Snow
   !                   -999: ATMS measurements are not available or over non-snow conditions
   !
   ! CALLS:
   !       ATMS_SNOW_ByTB_A      : by ATMS TBs of Group-1 channels
   !       ATMS_SNOW_ByTB_B      : by ATMS TBs of Group-2 channels
   !       ATMS_SNOW_ByTBs       : by the TBs  of all the five ATMS channels
   !       ATMS_SNOW_ByTBTs_A    : by ATMS TBs of Group-1 channels and surface temperature
   !       ATMS_SNOW_ByTBTs_B    : by ATMS TBs of Group-2 channels and surface temperature
   !       ATMS_SNOW_ByTBTs      : by the TBs  of all the five ATMS channels and surface temperature
   !       ATMS_SNOW_ByTypes     : bydefault surface type (4)
   !       ATMS_ALandEM_Snow     : Subroutine to initilize the vaiables to default values
   !       em_initialization     : Subroutine to initialization snow emissivity
   !       NESDIS_LandEM         : EM physical model for angular interpolations
   !
   ! SIDE EFFECTS:
   !       None.
   !
   ! RESTRICTIONS:
   !       None.
   !
   ! COMMENTS:
   !       Note the INTENT on the output SensorData argument is IN OUT rather than
   !       just OUT. This is necessary because the argument may be defined upon
   !       input. To prevent memory leaks, the IN OUT INTENT is a must.
   !
   ! CREATION HISTORY:
   !       Written by:
   !                       Ming Chen, IMSG Inc., ming.chen@noaa.gov (04-28-2012)
   !                       Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
   !
   !
   !  Copyright (C) 2005 Fuzhong Weng and Ming Chen
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
   !
   !------------------------------------------------------------------------------------------------------------


   SUBROUTINE  NESDIS_ATMS_SNOWEM(Satellite_Angle,                             &  ! INPUT
                                  User_Angle,                                  &  ! INPUT
                                  Frequency,                                   &  ! INPUT
                                  Tbs,                                         &  ! INPUT
                                  Tss,                                         &  ! INPUT
                                  Snow_Depth,                                  &  ! INPUT
                                  Emissivity_H,                                &  ! OUPUT
                                  Emissivity_V)                                   ! OUTPUT



     IMPLICIT NONE

     INTEGER, PARAMETER :: nch = N_FREQ_ATMS, nwch = 5
     REAL(fp),INTENT(IN) :: Satellite_Angle,User_Angle,Frequency
     REAL(fp),INTENT(IN),  OPTIONAL  :: Tbs(:), Tss, Snow_Depth
     REAL(fp),INTENT(OUT)            :: Emissivity_H,Emissivity_V
     REAL(fp) :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem
     REAL(fp) :: Ts = 273.15 ! default skin-surface temperature
     INTEGER :: Snow_Type = 4 ! default snow type
     INTEGER :: i

     LOGICAL  :: VALID_SNOW_DEPTH = .FALSE.
     INTEGER  :: input_type = 0

   ! Analyze the input types and determine which algorithms to be used

     IF (PRESENT(Snow_Depth) .AND. PRESENT(Tss) ) THEN
       VALID_SNOW_DEPTH = .TRUE.
       IF ((Snow_Depth < 0.0_fp) .OR. (Snow_Depth >= 3000.0_fp)) &
       VALID_SNOW_DEPTH = .FALSE.
     ENDIF

     IF (PRESENT(TBs)) THEN
        IF (SIZE(Tbs) >= 4) input_type=IBSET(input_type, 0)
        IF (SIZE(Tbs) >= 5) input_type=IBSET(input_type, 1)
        DO i=1,SIZE(Tbs)
          IF ((Tbs(i) <= 100.0_fp) .OR. (Tbs(i) >= 290.0_fp) ) THEN
            IF (i <= 4) input_type=IBCLR(input_type, 0)
            IF (i >= 4) input_type=IBCLR(input_type, 1)
          ENDIF
        END DO
     ENDIF

     IF (PRESENT(Tss) ) THEN
        input_type=IBSET(input_type, 2)
        Ts=Tss
        IF ((Ts <= 150.0_fp) .OR. (Ts >= 290.0_fp) ) THEN
           input_type=IBCLR(input_type, 2)
           VALID_SNOW_DEPTH = .FALSE.
        ENDIF
     ENDIF


   ! Initialization
     CALL em_initialization(frequency,em_vector)
     SELECT CASE (input_type)
        CASE (1)
           CALL ATMS_SNOW_ByTB_A(Frequency,Tbs(1:4),Snow_Type,em_vector)
        CASE (2)
           CALL ATMS_SNOW_ByTB_B(Frequency,Tbs(4:5),Snow_Type,em_vector)
        CASE (3)
           CALL ATMS_SNOW_ByTBs(Frequency,Tbs,Snow_Type,em_vector)
        CASE (5)
           CALL ATMS_SNOW_ByTBTs_A(Frequency,Tbs(1:4),Ts,Snow_Type,em_vector)
        CASE (6)
           CALL ATMS_SNOW_ByTBTs_B(Frequency,Tbs(4:5),Ts,Snow_Type,em_vector)
        CASE (7)
           CALL ATMS_SNOW_ByTBTs(Frequency,Tbs,Ts,Snow_Type,em_vector)
        CASE DEFAULT
           IF (VALID_SNOW_DEPTH) THEN
             CALL ATMS_ALandEM_Snow(Satellite_Angle,Frequency,Snow_Depth,Ts,&
                   Snow_Type,em_vector)
           ELSE
             !Use Default Snow type (4) for ATMS
             CALL ATMS_SNOW_ByTypes(Frequency,Snow_Type,em_vector)
           ENDIF
     END SELECT
   ! the above regression-based snow-typing algs are superseded by the diagnosis-based snow-typing
     CALL ATMS_SNOW_ByTBTs_D(Frequency,Tbs,Ts,Snow_Type,em_vector)

   ! Get the emissivity angle dependence
     CALL NESDIS_LandEM(Satellite_Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp_kind,9,13,2.0_fp,esh1,esv1)
     CALL NESDIS_LandEM(User_Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp_kind,9,13,2.0_fp,esh2,esv2)

     desh = esh1 - esh2
     desv = esv1 - esv2
     dem = ( desh + desv ) * 0.5_fp

   ! Emissivity at User's Angle
     Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

     IF (Emissivity_H > one) Emissivity_H = one
     IF (Emissivity_V > one) Emissivity_V = one

     IF (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp
     IF (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp

     RETURN

   END SUBROUTINE NESDIS_ATMS_SNOWEM




   !##################################################################################
   !##################################################################################
   !##                                                                              ##
   !##                          ## PRIVATE MODULE ROUTINES ##                       ##
   !##                                                                              ##
   !##################################################################################
   !##################################################################################



   SUBROUTINE ATMS_SNOW_ByTypes(frequency,snow_type,em_vector)

   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:  calculate emissivity by snow_type
   !
   !
   ! abstract: 1. look up the snow emissivity spectrum of the specified snow type
   !           2. Interpolate/extrapolate emissivity at a required frequency
   !
   !
   ! input argument list:
   !
   !      frequency        - frequency in GHz
   !      snow_type        - snow type
   ! output argument list:
   !
   !     emissivity  -  weighted emissivity from both V- and H- Pols.
   !
   ! important internal variables:
   !
   !     freq[1 ~ 10]  -  ten frequencies for sixteen snow types of emissivity
   !     em[1~16,*]    -  sixteen snow emissivity spectra
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER:: ncand = N_SNOW_TYPES,nch = N_FREQ_ATMS
     INTEGER:: i,snow_type
     REAL(fp)   :: em(ncand,nch)
     REAL(fp)   :: em_vector(2)
     REAL(fp)   :: frequency,freq(nch),emissivity,emis(nch)
     REAL(fp)   :: kratio, bconst

   ! Sixteen candidate snow emissivity spectra
     IF (snow_type == INVALID_SNOW_TYPE)snow_type = 4

     em = TRANSPOSE(SNOW_EMISS_ATMS_LIB)
     freq = FREQUENCY_ATMS
     emis=em(snow_type,:)

   ! Estimate snow emissivity at a required frequency
     DO i = 2, nch
        IF (frequency <  freq(1))   EXIT
        IF (frequency >= freq(nch)) EXIT
        IF (frequency <  freq(i)) THEN
           emissivity = emis(i-1) + (emis(i) - emis(i-1))*(frequency - freq(i-1))  &
                /(freq(i) - freq(i-1))
           EXIT
        ENDIF
     END DO

   ! Extrapolate to lower frequencies than 4.9GHz
     IF (frequency <= freq(1)) THEN
        kratio = (emis(2) - emis(1))/(freq(2) - freq(1))
        bconst = emis(1) - kratio*freq(1)
        emissivity =  kratio*frequency + bconst
        IF (emissivity > one)          emissivity = one
        IF (emissivity <= 0.8_fp) emissivity = 0.8_fp
     ENDIF

   ! Assume emissivity = constant at frequencies >= 150 GHz
     IF (frequency >= freq(nch)) emissivity = emis(nch)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

   END SUBROUTINE ATMS_SNOW_ByTypes


   SUBROUTINE ATMS_SNOW_ByTBTs_D(frequency,tb,ts,snow_type,em_vector)
   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram: Calculate emissivity by diagnosis-based algorithm
   !
   !
   ! abstract:
   !   Diagnose the snow type, and use the emissivity spectrum of the snow type as the first-guess to diagnose
   !   the magnitude of necessary adjustment with respect to window-channel TBs and surface temperature Ts. Perfrom
   !   necessary interpolation/extrapolation a required frequency and user angle.
   !
   !
   ! input argument list:
   !
   !     frequency        -  frequency in GHz
   !     theta            -  local zenith angle (currently, not used here)
   !     tb[1] ~ tb[5]    -  brightness temperature at five ATMS window channels:
   !                              tb[1] : 23.8 GHz
   !                              tb[2] : 31.4 GHz
   !                              tb[3] : 50.3 GHz
   !                              tb[4] : 88.2 GHz
   !                              tb[5] : 165.5 GHz
   !
   ! output argument list:
   !
   !      em_vector[1] and [2]  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !      snow_type        -  snow type
   !
   !
   ! remarks:
   !
   ! program history log:
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER , PARAMETER  :: ntype = N_SNOW_TYPES, nch = N_FREQ_ATMS, nwch = 5
     REAL(fp), PARAMETER  :: earthrad = 6374._fp, satheight = 833.4_fp
     INTEGER  :: freq_idx,snow_type
     REAL(fp) :: frequency
     REAL(fp) :: em(nch,ntype), em_vector(:)
     REAL(fp) :: tb(:),freq(nch)
     REAL(fp) :: ts, emissivity
     REAL(fp) :: ediff(ntype), X(nwch),Y(nwch),emw(nwch)
     REAL(fp) :: XX,XY,del,dem,dem2,delta,deltb
     INTEGER  :: minlc(1)
     REAL(fp) :: theta,deg2rad,sinthetas,costhetas
     INTEGER  :: windex(nwch)=(/1,2,3,11,12/)             ! window channel index of the library spectrum
     ! Coefficients of quadratic equations used to estimate the emissivity difference
     ! between Ch-31.4GHz and 88.2GHZ
     REAL(fp) :: coe1(6)=(/ -0.837001E+00_fp, 0.954882E-02_fp, -0.271471E-04_fp, &
                             -0.536112E-02_fp, 0.144279E-04_fp, 0.218317E-02_fp/)
     ! between Ch-31.4GHz and 165.5GHZ
     REAL(fp) :: coe2(6)=(/ -0.854226E+00_fp, 1.203220E-02_fp, -0.216043E-04_fp, &
                             -0.887216E-02_fp, 0.118303E-04_fp, 0.263699E-02_fp/)
     ! Quadratic EQ terms (1.0,tb(4),tb(4)^2,tb(5),tb(5)^2,Ts)
     REAL(fp) :: coe3(6)

   ! Sixteen candidate snow emissivity spectra
     em = SNOW_EMISS_ATMS_LIB
     freq = FREQUENCY_ATMS

     deg2rad = pi/180.0_fp
     sinthetas = sin(theta*deg2rad)* earthrad/(earthrad + satheight)
     sinthetas = sinthetas*sinthetas
     costhetas = 1.0_fp - sinthetas

     minlc =minloc(ABS(freq-frequency)); freq_idx=minlc(1)

   !*** IDENTIFY SNOW TYPE
     snow_type = 4 !default
     ediff=abs(Tb(1)/em(1,:)-Tb(2)/em(2,:))+abs(Tb(2)/em(2,:)-Tb(4)/em(11,:))
     minlc = minloc(ediff) ; snow_type=minlc(1)

   !*** adjustment from the library values
     emw=em(windex,snow_type)
     X=1.0_fp/emw ; Y=LOG(Tb/(Ts*emw))
     IF(frequency > 100.0_fp) THEN
       XX=DOT_PRODUCT(X((/1,2,4,5/)),X((/1,2,4,5/)))
       XY=DOT_PRODUCT(X((/1,2,4,5/)),Y((/1,2,4,5/)))
       del=XY/XX
       deltb=Tb(3)-Tb(5)
     ELSE
       XX=DOT_PRODUCT(X((/1,2,4/)),X((/1,2,4/)))
       XY=DOT_PRODUCT(X((/1,2,4/)),Y((/1,2,4/)))
       del=XY/XX
       deltb=Tb(3)-Tb(4)
     ENDIF

     IF(frequency <= 30.0_fp ) dem = 1.1_fp*del
     IF(frequency > 30._fp .AND. frequency <= 40.0_fp ) dem = 1.05_fp*del
     IF(frequency > 40._fp .AND. frequency <= 50.0_fp ) dem = 1.0_fp*del
     IF(frequency > 50_fp) THEN
        IF(del .LE. 0.0_fp .AND. ABS(deltb) .LT. 30.0_fp) delta=0.5_fp+deltb/50.0_fp
        IF(del .LE. 0.0_fp .AND. ABS(deltb) .GE. 30.0_fp) delta=1.0_fp+deltb/50.0_fp
        IF(del .GT. 0.0_fp .AND. ABS(deltb) .LT. 35.0_fp) delta=1.05_fp-deltb/70.0_fp
        IF(del .GT. 0.0_fp .AND. ABS(deltb) .GE. 35.0_fp) delta=.85_fp-deltb/70.0_fp
        IF(frequency <= 100.0_fp) dem  = del+(delta*del-del)*(frequency-50.0_fp)/(100.0_fp-50.0_fp)
        IF(frequency >  100.0_fp) dem  = 0.65_fp*delta*del
     ENDIF
     dem2=dem
     IF (frequency > 80.0_fp  )THEN
       coe3=(/1.0_fp,tb(4),tb(4)*tb(4),tb(5),tb(5)*tb(5),Ts/)
       IF(del<-0.13_fp)del=-0.13_fp
       IF(frequency <= 100.0_fp )THEN
         dem2=del-SUM(coe1*coe3)
       ELSE
         dem2=del-SUM(coe2*coe3)
       ENDIF
     ENDIF

     emissivity = em(freq_idx,snow_type)+(dem+dem2)/2.0_fp
     IF (emissivity >  1.0_fp )emissivity = 1.0_fp
     IF (emissivity <= 0.3_fp )emissivity = 0.3_fp

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTBTs_D


   SUBROUTINE ATMS_SNOW_ByTBs(frequency,tb,snow_type,em_vector)

   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at required frequency with respect to option ATMS window channels
   !
   !
   ! input argument list:
   !
   !      frequency    -  frequency in GHz
   !      theta        -  local zenith angle (not used here)
   !      tb[1]~tb[5]  -  brightness temperature at five ATMS window channels:
   !                              tb[1] : 23.8 GHz
   !                              tb[2] : 31.4 GHz
   !                              tb[3] : 50.3 GHz
   !                              tb[4] : 88.2 GHz
   !                              tb[5] : 165.5GHz
   !
   ! output argument list:
   !
   !     em_vector[1] and [2] - emissivity at two polarizations.
   !                            set esv = esh here and will be updated
   !     snow_type       - snow type (reference [2])
   !
   ! important internal variables:
   !
   !     coe    - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER:: nch =  N_FREQ_ATMS, nwch = 5,ncoe = 10
     REAL(fp) :: tb(:),frequency
     REAL(fp) :: em_vector(:),emissivity,discriminator(nwch)
     INTEGER  :: i,snow_type,ich,nvalid_ch
     REAL(fp),SAVE :: coe(nwch*(ncoe+1))


   ! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
     coe(1:7)=(/ &
          -1.326040e+000_fp,  2.475904e-002_fp, &
          -5.741361e-005_fp, -1.889650e-002_fp, &
           6.177911e-005_fp,  1.451121e-002_fp, &
          -4.925512e-005_fp/)

   ! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
     coe(12:18)=(/ &
          -1.250541e+000_fp,  1.911161e-002_fp, &
          -5.460238e-005_fp, -1.266388e-002_fp, &
           5.745064e-005_fp,  1.313985e-002_fp, &
          -4.574811e-005_fp/)

   ! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
     coe(23:29)=(/  &
          -1.246754e+000_fp,  2.368658e-002_fp, &
          -8.061774e-005_fp, -3.206323e-002_fp, &
           1.148107e-004_fp,  2.688353e-002_fp, &
          -7.358356e-005_fp/)

   ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
    coe(34:42)=(/ &
          -1.278780e+000_fp,  1.625141e-002_fp, &
          -4.764536e-005_fp, -1.475181e-002_fp, &
           5.107766e-005_fp,  1.083021e-002_fp, &
          -4.154825e-005_fp,  7.703879e-003_fp, &
          -6.351148e-006_fp/)

   ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb5
     coe(45:55)=(/ &
          -1.691077e+000_fp,  3.352403e-002_fp, &
          -7.310338e-005_fp, -4.396138e-002_fp, &
           1.028994e-004_fp,  2.301014e-002_fp, &
          -7.070810e-005_fp,  1.270231e-002_fp, &
          -2.139023e-005_fp, -2.257991e-003_fp, &
           1.269419e-005_fp/)

     !SAVE coe

   ! Calculate emissivity discriminators at five ATMS window channels
     DO ich = 1, nwch
        discriminator(ich) = coe(1+(ich-1)*11)
        IF (ich .LE. 3) nvalid_ch = 3
        IF (ich .EQ. 4) nvalid_ch = 4
        IF (ich .EQ. 5) nvalid_ch = 5
        DO i=1,nvalid_ch
           discriminator(ich) = discriminator(ich) + coe((ich-1)*11 + 2*i)*tb(i) +  &
                coe((ich-1)*11 + 2*i+1)*tb(i)*tb(i)
        END DO
     END DO
   ! Identify one snow emissivity spectrum and interpolate/extrapolate emissivity
   ! at a required frequency
     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

   END SUBROUTINE ATMS_SNOW_ByTBs


   SUBROUTINE ATMS_SNOW_ByTB_A(frequency,tba,snow_type,em_vector)

   !------------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at required frequency with respect to secenery ATMS
   !
   !
   ! input argument list:
   !
   !      frequency      -  frequency in GHz
   !      theta          -  local zenith angle (not used here)
   !      tba[1]~tba[4]  -  brightness temperature at fiveGroup-1 ATMS window channels:
   !                            tba[1] : 23.8 GHz
   !                            tba[2] : 31.4 GHz
   !                            tba[3] : 50.3 GHz
   !                            tba[4] : 88.2 GHz
   !
   ! output argument list:
   !
   !     em_vector(1) and (2)  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !     snow_type        -  snow type
   !
   ! important internal variables:
   !
   !     coe      - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !--------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER:: nch = N_FREQ_ATMS,nwch = 5,ncoe = 8
     REAL(fp)    :: tba(:)
     REAL(fp)    :: em_vector(:),emissivity,frequency,discriminator(nwch)
     INTEGER :: snow_type,i,ich,nvalid_ch
     REAL(fp),SAVE  :: coe(50)


   ! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
     coe(1:7)=(/ &
          -1.326040e+000_fp,  2.475904e-002_fp, -5.741361e-005_fp, &
          -1.889650e-002_fp,  6.177911e-005_fp,  1.451121e-002_fp, &
          -4.925512e-005_fp/)

   ! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
    coe(11:17)=(/ &
          -1.250541e+000_fp,  1.911161e-002_fp, -5.460238e-005_fp, &
          -1.266388e-002_fp,  5.745064e-005_fp,  1.313985e-002_fp, &
          -4.574811e-005_fp/)

   ! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
     coe(21:27)=(/ &
          -1.246754e+000_fp,  2.368658e-002_fp, -8.061774e-005_fp, &
          -3.206323e-002_fp,  1.148107e-004_fp,  2.688353e-002_fp, &
          -7.358356e-005_fp/)

   ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
     coe(31:39)=(/ &
          -1.278780e+000_fp,  1.625141e-002_fp, -4.764536e-005_fp, &
          -1.475181e-002_fp,  5.107766e-005_fp,  1.083021e-002_fp, &
          -4.154825e-005_fp,  7.703879e-003_fp, -6.351148e-006_fp/)

   ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4
     coe(41:49)=(/ &
          -1.624857e+000_fp,  3.138243e-002_fp, -6.757028e-005_fp, &
          -4.178496e-002_fp,  9.691893e-005_fp,  2.165964e-002_fp, &
          -6.702349e-005_fp,  1.111658e-002_fp, -1.050708e-005_fp/)

   !  SAVE coe


   ! Calculate emissivity discriminators at five ATMS window channels
     DO ich = 1, nwch
        discriminator(ich) = coe(1+(ich-1)*10)
        IF (ich .LE. 2) nvalid_ch = 3
        IF (ich .GE. 3) nvalid_ch = 4
        DO i=1,nvalid_ch
           discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
                coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
        END DO
     END DO

   ! Quality Control
     IF (discriminator(4) .GT. discriminator(2)) THEN
        discriminator(4) = discriminator(2) + (150.0_fp - 89.0_fp)*  &
          (discriminator(5) - discriminator(2))/ (150.0_fp - 31.4_fp)
     ENDIF

   ! Quality control at 50.3 GHz
     IF ((discriminator(3) .GT. discriminator(2)) .OR.  &
        (discriminator(3) .LT. discriminator(4)))  THEN
        discriminator(3) = discriminator(2) + (89.0_fp - 50.3_fp)*   &
          (discriminator(4) - discriminator(2))/(89.0_fp - 31.4_fp)
     ENDIF

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTB_A


   SUBROUTINE ATMS_SNOW_ByTB_B(frequency,tbb,snow_type,em_vector)

   !-------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at required frequency with respect to secenery Group-2 ATMS Channels
   !
   !
   ! input argument list:
   !
   !      frequency        -  frequency in GHz
   !      theta            -  local zenith angle (not used here)
   !      tbb[1] ~ tbb[2]  -  brightness temperature at five Group-2 ATMS window channels:
   !                              tbb[1] : 88.2  GHz
   !                              tbb[2] : 165.5 GHz
   !
   ! output argument list:
   !     em_vector(1) and (2)  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !     snow_type        -  snow type (reference [2])
   !
   ! important internal variables:
   !
   !     coe    - fitting coefficients to estimate discriminator at 31.4 ~ 150  GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !-------------------------------------------------------------------------------------------------------!
     IMPLICIT NONE

     INTEGER,PARAMETER:: nch = N_FREQ_ATMS,nwch = 3,ncoe = 4
     REAL(fp)    :: tbb(:)
     REAL(fp)    :: em_vector(:),emissivity,frequency,ed0(nwch),discriminator(5)
     INTEGER :: snow_type,i,k,ich,nvalid_ch
     REAL(fp)  :: coe(50)
     SAVE coe

   ! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5
     coe(1:5) = (/-4.015636e-001_fp,9.297894e-003_fp, -1.305068e-005_fp, &
          3.717131e-004_fp, -4.364877e-006_fp/)
   ! Fitting Coefficients at 89 GHz: Using Tb4, Tb5
     coe(11:15) = (/-2.229547e-001_fp, -1.828402e-003_fp,1.754807e-005_fp, &
          9.793681e-003_fp, -3.137189e-005_fp/)
   ! Fitting Coefficients at 150 GHz: Using Tb4, Tb5
     coe(21:25) = (/-3.395416e-001_fp,-4.632656e-003_fp,1.270735e-005_fp, &
          1.413038e-002_fp,-3.133239e-005_fp/)
!    SAVE coe

   ! Calculate emissivity discriminators at five ATMS window channels
     DO ich = 1, nwch
        ed0(ich) = coe(1+(ich-1)*10)
        nvalid_ch = 2
        DO i=1,nvalid_ch
           ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +  &
                coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
        END DO
     END DO

   ! Quality Control
     IF (ed0(2) .GT. ed0(1))     &
          ed0(2) = ed0(1) + (150.0_fp - 89.0_fp) * &
         (ed0(3) - ed0(1))/(150.0_fp - 31.4_fp)

   ! Match the format of the input variable
   ! Missing value at 23.8 GHz
     discriminator(1) = -999.9_fp; discriminator(2) = ed0(1)
   ! Missing value at 50.3 GHz
     discriminator(3) = -999.9_fp; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTB_B


   SUBROUTINE ATMS_SNOW_ByTBTs(frequency,tb,ts,snow_type,em_vector)
   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at a required frequency with respect to secenery ABTs
   !
   !
   ! input argument list:
   !
   !     frequency        -  frequency in GHz
   !     theta            -  local zenith angle (currently, not used here)
   !     tb[1] ~ tb[5]    -  brightness temperature at five ATMS window channels:
   !                              tb[1] : 23.8 GHz
   !                              tb[2] : 31.4 GHz
   !                              tb[3] : 50.3 GHz
   !                              tb[4] : 88.2 GHz
   !                              tb[5] : 65.5 GHz
   !
   ! output argument list:
   !
   !      em_vector[1] and [2]  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !      snow_type        -  snow type
   !
   ! important internal variables:
   !
   !     nind           -  number of threshold in decision trees
   !                          to identify each snow type  ( = 6)
   !     em(1~16,*)     -  sixteen snow emissivity spectra
   !     DI_coe         -  coefficients to generate six discriminators to describe
   !                       the overall emissivity variability within a wider frequency range
   !     threshold      -  thresholds in decision trees to identify snow types
   !     index_in       -  six indices to discriminate snow type
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER:: ncand = N_SNOW_TYPES,nch = N_FREQ_ATMS,nthresh=38
     INTEGER,PARAMETER:: nind=6,ncoe=8,nLIcoe=6,nHIcoe=12
     INTEGER:: i,j,k,num,npass,snow_type,md0,md1,nmodel(ncand-1)
     REAL(fp)   :: frequency,tb150,LI,HI,DS1,DS2,DS3
     REAL(fp)   :: em(ncand,nch), em_vector(:)
     REAL(fp)   :: tb(:),freq(nch),DTB(nind-1),DI(nind-1),       &
          DI_coe(nind-1,0:ncoe-1),threshold(nthresh,nind),       &
          index_in(nind),threshold0(nind)
     REAL(fp)   :: LI_coe(0:nLIcoe-1),HI_coe(0:nHIcoe-1)
     REAL(fp)   :: ts,emissivity
     REAL(fp)   :: discriminator(5)
     LOGICAL:: pick_status,tindex(nind)
     SAVE      threshold,DI_coe,LI_coe, HI_coe,nmodel

     nmodel = (/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/)

   ! Fitting coefficients for emissivity index at 31.4 GHz
     LI_coe = (/ &
          7.963632e-001_fp,  7.215580e-003_fp,  &
         -2.015921e-005_fp, -1.508286e-003_fp,  &
          1.731405e-005_fp, -4.105358e-003_fp/)

   ! Fitting coefficients for emissivity index at 150 GHz
     HI_coe = (/ &
          1.012160e+000_fp,  6.100397e-003_fp, &
         -1.774347e-005_fp, -4.028211e-003_fp, &
          1.224470e-005_fp,  2.345612e-003_fp, &
         -5.376814e-006_fp, -2.795332e-003_fp, &
          8.072756e-006_fp,  3.529615e-003_fp, &
          1.955293e-006_fp, -4.942230e-003_fp/)

   ! Fitting coefficients for five discriminators
     DI_coe(1,0:ncoe-1)=(/  3.285557e-002_fp, 2.677179e-005_fp,  &
          4.553101e-003_fp, 5.639352e-005_fp,-1.825188e-004_fp,  &
          1.636145e-004_fp, 1.680881e-005_fp,-1.708405e-004_fp/)
     DI_coe(2,0:ncoe-1)=(/ -4.275539e-002_fp,-2.541453e-005_fp,  &
          4.154796e-004_fp, 1.703443e-004_fp, 4.350142e-003_fp,  &
          2.452873e-004_fp,-4.748506e-003_fp, 2.293836e-004_fp/)
     DI_coe(3,0:ncoe-1)=(/ -1.870173e-001_fp,-1.061678e-004_fp,  &
          2.364055e-004_fp,-2.834876e-005_fp, 4.899651e-003_fp,  &
         -3.418847e-004_fp,-2.312224e-004_fp, 9.498600e-004_fp/)
     DI_coe(4,0:ncoe-1)=(/ -2.076519e-001_fp, 8.475901e-004_fp,  &
         -2.072679e-003_fp,-2.064717e-003_fp, 2.600452e-003_fp,  &
          2.503923e-003_fp, 5.179711e-004_fp, 4.667157e-005_fp/)
     DI_coe(5,0:ncoe-1)=(/ -1.442609e-001_fp,-8.075003e-005_fp,  &
         -1.790933e-004_fp,-1.986887e-004_fp, 5.495115e-004_fp,  &
         -5.871732e-004_fp, 4.517280e-003_fp, 7.204695e-004_fp/)


   ! Six thresholds for sixteen candidate snow types
   ! Note: some snow type contains several possible
   !      selections for six thresholds

   !1 Wet Snow
     threshold(1,1:6)=(/0.88_fp,0.86_fp, -999.9_fp, 0.01_fp,   0.01_fp,  200._fp/)
     threshold(2,1:6)=(/0.88_fp,0.85_fp, -999.9_fp, 0.06_fp,   0.10_fp,  200._fp/)
     threshold(3,1:6)=(/0.88_fp,0.83_fp,  -0.02_fp, 0.12_fp,   0.16_fp,  204._fp/)
     threshold(4,1:6)=(/0.90_fp,0.89_fp, -999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)
     threshold(5,1:6)=(/0.92_fp,0.85_fp, -999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !2 Grass_after_Snow
     threshold(6,1:6)=(/0.84_fp,0.83_fp,-999.9_fp,   0.08_fp,  0.10_fp,195._fp/)
     threshold(7,1:6)=(/0.85_fp,0.85_fp,-999.9_fp,   0.10_fp,-999.9_fp,190._fp/)
     threshold(8,1:6)=(/0.86_fp,0.81_fp,-999.9_fp,   0.12_fp,-999.9_fp,200._fp/)
     threshold(9,1:6)=(/0.86_fp,0.81_fp,    0.0_fp,  0.12_fp,-999.9_fp,189._fp/)
     threshold(10,1:6)=(/0.90_fp,0.81_fp,-999.9_fp,-999.9_fp,-999.9_fp,195._fp/)

   !3 RS_Snow (A)
     threshold(11,1:6)=(/0.80_fp,0.76_fp,-999.9_fp,  0.05_fp,-999.9_fp,185._fp/)
     threshold(12,1:6)=(/0.82_fp,0.78_fp,-999.9_fp,-999.9_fp,  0.25_fp,180._fp/)
     threshold(13,1:6)=(/0.90_fp,0.76_fp,-999.9_fp,-999.9_fp,-999.9_fp,180._fp/)

   !4 Powder  Snow
     threshold(14,1:6)=(/0.89_fp,0.73_fp,-999.9_fp,  0.20_fp,-999.9_fp,-999.9_fp/)
     threshold(15,1:6)=(/0.89_fp,0.75_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)
     threshold(16,1:6)=(/0.93_fp,0.72_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !5 RS_Snow (B)
     threshold(17,1:6)=(/0.82_fp,0.70_fp,-999.9_fp,  0.20_fp,-999.9_fp,160._fp/)
     threshold(18,1:6)=(/0.83_fp,0.70_fp,-999.9_fp,-999.9_fp,-999.9_fp,160._fp/)

   !6 RS_Snow (C)
     threshold(19,1:6)=(/0.75_fp,0.76_fp,  -999.9_fp,  0.08_fp,-999.9_fp,172._fp/)
     threshold(20,1:6)=(/0.77_fp,0.72_fp,  -999.9_fp,  0.12_fp,  0.15_fp,175._fp/)
     threshold(21,1:6)=(/0.78_fp,0.74_fp,  -999.9_fp,-999.9_fp,  0.20_fp,172._fp/)
     threshold(22,1:6)=(/0.80_fp,0.77_fp,  -999.9_fp,-999.9_fp,-999.9_fp,170._fp/)
     threshold(23,1:6)=(/0.82_fp,-999.9_fp,-999.9_fp,  0.15_fp,  0.22_fp,170._fp/)
     threshold(24,1:6)=(/0.82_fp,  0.73_fp,-999.9_fp,-999.9_fp,-999.9_fp,170._fp/)

   !7 RS_Snow (D)
     threshold(25,1:6)=(/0.75_fp,0.70_fp,-999.9_fp,  0.15_fp,  0.25_fp,  167._fp/)
     threshold(26,1:6)=(/0.77_fp,0.76_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)
     threshold(27,1:6)=(/0.80_fp,0.72_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)
     threshold(28,1:6)=(/0.77_fp,0.73_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

     threshold(29,1:6)=(/0.81_fp,0.71_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)
     threshold(30,1:6)=(/0.82_fp,0.69_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !8 Thin Crust Snow
     threshold(31,1:6)=(/0.88_fp,0.58_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !9 RS_Snow (E)
     threshold(32,1:6)=(/0.73_fp,0.67_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !10 Bottom Crust Snow (A)
     threshold(33,1:6)=(/0.83_fp,0.66_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !11 Shallow Snow
     threshold(34,1:6)=(/0.82_fp,0.60_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !12 Deep Snow
     threshold(35,1:6)=(/0.77_fp,0.60_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !13 Crust Snow
     threshold(36,1:6)=(/0.77_fp,0.7_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !14 Medium Snow
     threshold(37,1:6)=(/-999.9_fp,0.55_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !15 Bottom Crust Snow(B)
     threshold(38,1:6)=(/0.74_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp,-999.9_fp/)

   !16 Thick Crust Snow
   ! lowest priority: No constraints

   ! Sixteen candidate snow emissivity spectra
     em = TRANSPOSE(SNOW_EMISS_ATMS_LIB)
     freq = FREQUENCY_ATMS

   !***  DEFINE SIX DISCRIMINATORS

     dtb(1) = tb(1) - tb(2)
     dtb(2) = tb(2) - tb(4)
     dtb(3) = tb(2) - tb(5)
     dtb(4) = tb(3) - tb(5)
     dtb(5) = tb(4) - tb(5)
     tb150  = tb(5)

     LI = LI_coe(0)
     DO i=0,1
        LI = LI + LI_coe(2*i+1)*tb(i+1) + LI_coe(2*i+2)*tb(i+1)*tb(i+1)
     END DO
     LI = LI + LI_coe(nLIcoe-1)*ts

     HI = HI_coe(0)
     DO i=0,4
        HI = HI + HI_coe(2*i+1)*tb(i+1) + HI_coe(2*i+2)*tb(i+1)*tb(i+1)
     END DO
     HI = HI + HI_coe(nHIcoe-1)*ts

     DO num=1,nind-1
        DI(num) = DI_coe(num,0) + DI_coe(num,1)*tb(2)
        DO i=1,5
           DI(num) = DI(num) + DI_coe(num,1+i)*DTB(i)
        END DO
        DI(num) = DI(num) +  DI_coe(num,ncoe-1)*ts
     END DO

   !*** DEFINE FIVE INDIES
     !HI = DI(0) - DI(3)
     DS1 = DI(1) + DI(2)
     DS2 = DI(4) + DI(5)
     DS3 = DS1 + DS2 + DI(3)

     index_in(1) = LI
     index_in(2) = HI
     index_in(3) = DS1
     index_in(4) = DS2
     index_in(5) = DS3
     index_in(6) = tb150

   !*** IDENTIFY SNOW TYPE


   ! Initialization
     md0 = 1
     snow_type = ncand
     pick_status = .FALSE.

   ! Pick one snow type
   ! Check all possible selections for six thresholds for each snow type
     DO i = 1, ncand - 1
        md1 = nmodel(i)
        DO j = md0, md1
           npass = 0
           DO k = 1 , nind
              threshold0(k) = threshold(j,k)
           END DO
           CALL six_indices(nind,index_in,threshold0,tindex)

   ! Corrections
           IF ((i == 5)  .AND. (index_in(2) >  0.75_fp)) tindex(2) = .FALSE.
           IF ((i == 5)  .AND. (index_in(4) >  0.20_fp)                        &
                .AND. (index_in(1) >  0.88_fp)) tindex(1) = .FALSE.
           IF ((i == 10) .AND. (index_in(1) <= 0.83_fp)) tindex(1) = .TRUE.
           IF ((i == 13) .AND. (index_in(2) <  0.52_fp)) tindex(2) = .TRUE.
           DO k = 1, nind
              IF (.NOT. tindex(k)) EXIT
              npass = npass + 1
           END DO
           IF (npass == nind) EXIT
        END DO

        IF (npass == nind) THEN
           pick_status = .TRUE.
           snow_type  = i
        ENDIF
        IF (pick_status) EXIT
        md0 = md1 + 1
     END DO

     discriminator(1) = LI + DI(1)
     discriminator(2) = LI
     discriminator(3) = DI(4) + HI
     discriminator(4) = LI - DI(2)
     discriminator(5) = HI

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTBTs



   SUBROUTINE ATMS_SNOW_ByTBTs_A(frequency,tba,ts,snow_type,em_vector)
   !-----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at required frequency with respect to secenery GROUP-1 ATMS channels
   !
   !
   ! input argument list:
   !
   !      frequency        -  frequency in GHz
   !      theta            -  local zenith angle (not used here)
   !      ts               -  surface temperature
   !      tba[1] ~ tba[4]  -  brightness temperature at four GROUP-1 ATMS window channels:
   !                              tba[1] : 23.8 GHz
   !                              tba[2] : 31.4 GHz
   !                              tba[3] : 50.3 GHz
   !                              tba[4] : 88.2 GHz
   ! output argument list:
   !
   !     em_vector[1] and [2]  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !     snow_type        -  snow type (reference [2])
   !
   ! important internal variables:
   !
   !     coe      - fitting coefficients to estimate discriminator at 23.8 ~ 150  GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !-----------------------------------------------------------------------------------------------------------!
     IMPLICIT NONE

     INTEGER,PARAMETER:: nch = 10, nwch = 5,ncoe = 9
     REAL(fp)  :: tba(:)
     REAL(fp)  :: em_vector(:),emissivity,ts,frequency,discriminator(nwch)
     INTEGER   :: snow_type,i,ich,nvalid_ch
     REAL(fp),SAVE  :: coe(nch*(ncoe+1))


   ! Fitting Coefficients at 23.8 GHz: Using Tb1, Tb2 and Ts
     coe(1:6)=(/ &
           8.210105e-001_fp,  1.216432e-002_fp, &
          -2.113875e-005_fp, -6.416648e-003_fp, &
           1.809047e-005_fp, -4.206605e-003_fp/)

   ! Fitting Coefficients at 31.4 GHz: Using Tb1, Tb2 and Ts
     coe(11:16)=(/ &
           7.963632e-001_fp,  7.215580e-003_fp, &
          -2.015921e-005_fp, -1.508286e-003_fp, &
           1.731405e-005_fp, -4.105358e-003_fp/)

   ! Fitting Coefficients at 50.3 GHz: Using Tb1, Tb2, Tb3 and Ts
     coe(21:28)=(/ &
           1.724160e+000_fp,  5.556665e-003_fp, &
          -2.915872e-005_fp, -1.146713e-002_fp, &
           4.724243e-005_fp,  3.851791e-003_fp, &
          -5.581535e-008_fp, -5.413451e-003_fp/)

   ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4 and Ts
     coe(31:40)=(/ &
           9.962065e-001_fp,  1.584161e-004_fp, &
          -3.988934e-006_fp,  3.427638e-003_fp, &
          -5.084836e-006_fp, -6.178904e-004_fp, &
           1.115315e-006_fp,  9.440962e-004_fp, &
           9.711384e-006_fp, -4.259102e-003_fp/)

   ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4 and Ts
     coe(41:50)=(/ &
          -5.244422e-002_fp,  2.025879e-002_fp, &
          -3.739231e-005_fp, -2.922355e-002_fp, &
           5.810726e-005_fp,  1.376275e-002_fp, &
          -3.757061e-005_fp,  6.434187e-003_fp, &
           6.190403e-007_fp, -2.944785e-003_fp/)



   ! Calculate emissivity discriminators at five ATMS window channels
     DO ich = 1, nwch
        discriminator(ich) = coe(1+(ich-1)*10)
        IF (ich .LE. 2) nvalid_ch = 2
        IF (ich .EQ. 3) nvalid_ch = 3
        IF (ich .GE. 4) nvalid_ch = 4
        DO i=1,nvalid_ch
           discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
                coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
        END DO
        discriminator(ich) = discriminator(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
     END DO

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTBTs_A


   SUBROUTINE ATMS_SNOW_ByTBTs_B(frequency,tbb,ts,snow_type,em_vector)
   !-------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !  emissivity at required frequency with respect to secenery BTs
   !
   !
   ! input argument list:
   !
   !      frequency        -  frequency in GHz
   !      theta            -  local zenith angle (not used here)
   !      ts               -  surface temperature in degree
   !      tbb[1] ~ tbb[2]  -  brightness temperature at five ATMS window channels:
   !                              tbb[1] : 88.2  GHz
   !                              tbb[2] : 165.5 GHz
   !
   ! output argument list:
   !
   !     em_vector(1) and (2)  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !     snow_type        -  snow type
   !
   ! important internal variables:
   !
   !     coe      - fitting coefficients to estimate discriminator at 31.4 ~ 150  GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !-------------------------------------------------------------------------------------------------------!
     INTEGER,PARAMETER:: nch =10,nwch = 3,ncoe = 5
     REAL(fp)    :: tbb(:)
     REAL(fp)    :: em_vector(:),emissivity,ts,frequency,ed0(nwch),discriminator(5)
     INTEGER :: snow_type,i,ich,nvalid_ch
     REAL(fp),SAVE  :: coe(nch*(ncoe+1))


   ! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5 and Ts
     coe(1:6) = (/ 3.110967e-001_fp,  1.100175e-002_fp, -1.677626e-005_fp, &
                  -4.020427e-003_fp,  9.242240e-006_fp, -2.363207e-003_fp/)
   ! Fitting Coefficients at 89 GHz: Using Tb4, Tb5 and Ts
     coe(11:16)=(/ 1.148098e+000_fp,  1.452926e-003_fp,  1.037081e-005_fp, &
                   1.340696e-003_fp,  -5.185640e-006_fp, -4.546382e-003_fp /)
   ! Fitting Coefficients at 150 GHz: Using Tb4, Tb5 and Ts
     coe(21:26)=(/ 1.165323e+000_fp, -1.030435e-003_fp,  4.828009e-006_fp, &
                   4.851731e-003_fp, -2.588049e-006_fp, -4.990193e-003_fp/)
   !  SAVE coe

   ! Calculate emissivity discriminators at five ATMS window channels
     DO ich = 1, nwch
        ed0(ich) = coe(1+(ich-1)*10)
        nvalid_ch = 2
        DO i=1,nvalid_ch
           ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +   &
                coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
        END DO
        ed0(ich) = ed0(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
     END DO

   ! Quality control
     IF (ed0(2) .GT. ed0(1))  THEN
        ed0(2) = ed0(1) + (150.0_fp - 89.0_fp)*(ed0(3) - ed0(1)) / &
                (150.0_fp - 31.4_fp)
     ENDIF

   ! Match the format of the input variable
   ! Missing value at 23.8 GHz
     discriminator(1) = -999.9_fp;  discriminator(2) = ed0(1)
   ! Missing value at 50.3 GHz
     discriminator(3) = -999.9_fp; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE ATMS_SNOW_ByTBTs_B


   SUBROUTINE ATMS_ALandEM_Snow(theta,frequency,snow_depth,ts,snow_type,em_vector)
   !------------------------------------------------------------------------------------------------------------
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity at required frequency with respect to option MODL
   !   using the NESDIS_LandEM and a bias correction algorithm, where the original NESDIS_LandEM with a
   !   bias correction algorithm is referred to as value-added NESDIS_LandEM or AlandEM.
   !
   !
   ! input argument list:
   !
   !      frequency        -  frequency in GHz
   !      theta            -  local zenith angle in degree
   !      snow_depth       -  snow depth in mm
   !      ts           -  surface temperature
   !
   ! output argument list:
   !
   !     em_vector(1) and (2)  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !       snow_type        -  snow type
   !
   ! important internal variables:
   !
   !    esv_3w and esh_3w   -  initial emissivity discriminator at two polarizations
   !                           at three ATMS window channels computed using NESDIS_LandEM
   !    esv_3w[1] and esh_3w[1] : 31.4 GHz
   !    esv_3w[2] and esh_3w[2] : 88.2 GHz
   !    esv_3w[3] and esh_3w[3] : 165.5GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !------------------------------------------------------------------------------------------------------------

     USE NESDIS_LandEM_Module, ONLY: NESDIS_LandEM
     IMPLICIT NONE

     INTEGER :: nw_ind
     PARAMETER(nw_ind=3)
     REAL(fp) theta, frequency, freq,snow_depth, ts, em_vector(2)
     REAL(fp) esv,esh,esh0,esv0,theta0
     INTEGER snow_type,ich
     REAL(fp)   freq_3w(nw_ind),esh_3w(nw_ind),esv_3w(nw_ind)
     COMPLEX(fp)  eair
     freq_3w = (/31.4_fp,89.0_fp,150.0_fp/)

     eair = cmplx(one,-zero,fp)

     snow_type = -999

     CALL NESDIS_LandEM(theta, frequency,0.0_fp,0.0_fp,ts,ts,0.0_fp_kind,9,13,snow_depth,esh0,esv0)

     theta0 = theta
     DO ich = 1, nw_ind
        freq =freq_3w(ich)
        theta = theta0
        CALL NESDIS_LandEM(theta, freq,0.0_fp,0.0_fp,ts,ts,0.0_fp_kind,9,13,snow_depth,esh,esv)
        esv_3w(ich) = esv
        esh_3w(ich) = esh
     END DO

     CALL ems_adjust(theta,frequency,snow_depth,ts,esv_3w,esh_3w,em_vector,snow_type)

     RETURN

   END SUBROUTINE ATMS_ALandEM_Snow


   SUBROUTINE em_initialization(frequency,em_vector)

   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram: ATMS snow emissivity initialization
   !
   !
   ! abstract:   ATMS snow emissivity initialization
   !
   !
   ! input argument list:
   !
   !      frequency   - frequency in GHz
   !
   ! output argument list:
   !
   !     em_vector[1] and [2]  -  initial emissivity at two polarizations.
   !
   ! important internal variables:
   !
   !      freq[1~10]  - ten frequencies for sixteen snow types of emissivity
   !      em[1~16,*]  - sixteen snow emissivity spectra
   !      snow_type   - snow type
   !                    where it is initialized to as the type 4,i.e, Powder Snow
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER :: nch = N_FREQ_ATMS,ncand=N_SNOW_TYPES
     REAL(fp) :: frequency,em_vector(:),freq(nch)
     REAL(fp) :: em(ncand,nch)
     REAL(fp) :: kratio, bconst,emissivity
     INTEGER :: ich

   ! Sixteen candidate snow emissivity spectra

     em = TRANSPOSE(SNOW_EMISS_ATMS_LIB)
     freq = FREQUENCY_ATMS

   ! Initialization for emissivity at certain frequency
   !    In case of no any inputs available for various options
   !    A constant snow type & snow emissivity spectrum is assumed
   !                    (e.g., powder) snow_type = 4

   ! Specify snow emissivity at required frequency
     DO ich = 2, nch
        IF (frequency <  freq(1))   EXIT
        IF (frequency >= freq(nch)) EXIT
        IF (frequency <  freq(ich)) THEN
           emissivity = em(4,ich-1) + (em(4,ich) - em(4,ich-1))     &
                *(frequency - freq(ich-1))/(freq(ich) - freq(ich-1))
           EXIT
        ENDIF
     END DO

   ! Extrapolate to lower frequencies than 4.9GHz
     IF (frequency <= freq(1)) THEN
        kratio = (em(4,2) - em(4,1))/(freq(2) - freq(1))
        bconst = em(4,1) - kratio*freq(1)
        emissivity =  kratio*frequency + bconst
        IF (emissivity >  one)         emissivity = one
        IF (emissivity <= 0.8_fp) emissivity = 0.8_fp
     ENDIF


   ! Assume emissivity = constant at frequencies >= 150 GHz
     IF (frequency >= freq(nch)) emissivity = em(4,nch)
     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN
   END SUBROUTINE em_initialization



   SUBROUTINE em_interpolate(frequency,discriminator,emissivity,snow_type)

   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:  determine snow_type and calculate emissivity
   !
   !
   ! abstract: 1. Find one snow emissivity spectrum to mimic the emission
   !              property of the realistic snow condition using a set of
   !              discrminators
   !           2. Interpolate/extrapolate emissivity at a required frequency
   !
   !
   ! input argument list:
   !
   !      frequency        - frequency in GHz
   !      discriminators   - emissivity discriminators at five ATMS window
   !                         channels
   !            discriminator[1]   :  emissivity discriminator at 23.8 GHz
   !            discriminator[2]   :  emissivity discriminator at 31.4 GHz
   !            discriminator[3]   :  emissivity discriminator at 50.3 GHz
   !            discriminator[4]   :  emissivity discriminator at 88.2 GHz
   !            discriminator[5]   :  emissivity discriminator at 165.5GHz
   !
   !       Note: discriminator(1) and discriminator(3) are missing value in
   !            'Group-2 & Ts','Group-2' and 'MODL' options., which are defined to as -999.9,
   ! output argument list:
   !
   !     emissivity  -  weighted emissivity from both V- and H- Pols.
   !     snow_type             - snow type
   !
   ! important internal variables:
   !
   !     freq[1 ~ 10]  -  ten frequencies for sixteen snow types of emissivity
   !     em[1~16,*]    -  sixteen snow emissivity spectra
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER,PARAMETER:: ncand = N_SNOW_TYPES,nch =N_FREQ_ATMS
     INTEGER:: ich,ichmin,ichmax,i,k,snow_type
     REAL(fp)   :: dem,demmin0
     REAL(fp)   :: em(ncand,nch)
     REAL(fp)   :: frequency,freq(nch),emissivity,discriminator(:),emis(nch)
     REAL(fp)   :: cor_factor,adjust_check,kratio, bconst

   ! Sixteen candidate snow emissivity spectra

     em = TRANSPOSE(SNOW_EMISS_ATMS_LIB)
     freq = FREQUENCY_ATMS

   ! Adjust unreasonable discriminator
     IF (discriminator(4) > discriminator(2))  THEN
         discriminator(4) = discriminator(2) +(discriminator(5) - discriminator(2))*  &
             (150.0_fp - 89.0_fp)/(150.0_fp - 31.4_fp)
     ENDIF
     IF ( (discriminator(3) /= -999.9_fp) .AND.       &
        ( ((discriminator(3)-0.01_fp) > discriminator(2)) .OR.     &
        ((discriminator(3)-0.01_fp) < discriminator(4)))    ) THEN
        discriminator(3) = discriminator(2) +  (discriminator(4) - discriminator(2))* &
      (89.0_fp - 50.3_fp) / (89.0_fp - 31.4_fp)
     ENDIF

   ! Find a snow emissivity spectrum
     IF (snow_type .EQ. -999) THEN
        demmin0 = 10.0_fp
        DO k = 1, ncand
           dem = zero
           ichmin = 1
           ichmax = 3
           IF (discriminator(1) == -999.9_fp) THEN
              ichmin = 2
              ichmax = 2
           ENDIF
           DO ich = ichmin,ichmax
              dem = dem + abs(discriminator(ich) - em(k,ich+4))
           END DO
           DO ich = 4,5
              dem = dem + abs(discriminator(ich) - em(k,ich+5))
           END DO
           IF (dem < demmin0) THEN
              demmin0 = dem
              snow_type = k
           ENDIF
        END DO
     ENDIF

   ! Shift snow emissivity according to discriminator at 31.4 GHz
     cor_factor = discriminator(2) - em(snow_type,6)
     DO ich = 1, nch
        emis(ich) = em(snow_type,ich) + cor_factor
        IF (emis(ich) .GT. one)    emis(ich) = one
        IF (emis(ich) .LT. 0.3_fp) emis(ich) = 0.3_fp
     END DO

   ! Emisivity data quality control
     adjust_check = zero
     DO ich = 5, 9
        IF (ich .LE. 7) THEN
           IF (discriminator(ich - 4) .NE. -999.9_fp) &
              adjust_check = adjust_check + abs(emis(ich) - discriminator(ich - 4))
        ELSE
           IF (discriminator(ich - 4) .NE. -999.9_fp)  &
              adjust_check = adjust_check + abs(emis(ich+1) - discriminator(ich - 4))
        ENDIF
     END DO

     IF (adjust_check >= 0.04_fp) THEN
        IF (discriminator(1) /= -999.9_fp) THEN
           IF (discriminator(1) < emis(4)) THEN
              emis(5) = emis(4) + (31.4_fp - 23.8_fp) * &
                 (discriminator(2) - emis(4))/(31.4_fp - 18.7_fp)
           ELSE
              emis(5) = discriminator(1)
           ENDIF
        ENDIF
        emis(6) = discriminator(2)
        IF (discriminator(3) /= -999.9_fp) THEN
           emis(7) = discriminator(3)
        ELSE
   !       In case of missing the emissivity discriminator at 50.3 GHz
           emis(7) = emis(6) + (89.0_fp - 50.3_fp) * &
                    (discriminator(4) - emis(6))/(89.0_fp - 31.4_fp)
        ENDIF
        emis(8) = emis(7)
        emis(9) = discriminator(4)
        emis(10) = discriminator(5)
     ENDIF

   ! Estimate snow emissivity at a required frequency
     DO i = 2, nch
        IF (frequency <  freq(1))   EXIT
        IF (frequency >= freq(nch)) EXIT
        IF (frequency <  freq(i)) THEN
           emissivity = emis(i-1) + (emis(i) - emis(i-1))*(frequency - freq(i-1))  &
                /(freq(i) - freq(i-1))
           EXIT
        ENDIF
     END DO

   ! Extrapolate to lower frequencies than 4.9GHz
     IF (frequency <= freq(1)) THEN
        kratio = (emis(2) - emis(1))/(freq(2) - freq(1))
        bconst = emis(1) - kratio*freq(1)
        emissivity =  kratio*frequency + bconst
        IF (emissivity > one)          emissivity = one
        IF (emissivity <= 0.8_fp) emissivity = 0.8_fp
     ENDIF

   ! Assume emissivity = constant at frequencies >= 150 GHz
     IF (frequency >= freq(nch)) emissivity = emis(nch)


   END SUBROUTINE em_interpolate



   SUBROUTINE ems_adjust(theta,frequency,depth,ts,esv_3w,esh_3w,em_vector,snow_type)


   !------------------------------------------------------------------------------------------------------------
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   !
   ! abstract:
   !         Calculate the emissivity discriminators and interpolate/extrapolate
   !
   !  emissivity at required frequency with respect to secenery MODL
   !
   !
   ! input argument list:
   !
   !      frequency   -  frequency in GHz
   !      theta       -  local zenith angle in degree
   !      depth       -  snow depth in mm
   !      ts          -  surface temperature
   !
   ! output argument list:
   !
   !     em_vector(1) and (2)  -  emissivity at two polarizations.
   !                              set esv = esh here and will be updated
   !     snow_type        -  snow type
   !
   ! important internal variables:
   !
   !     dem_coe  -  fitting coefficients to compute discriminator correction value
   !              dem_coe[1,*]   : 31.4 GHz
   !              dem_coe[2,*]   : 88.2 GHz
   !              dem_coe[3,*]   : 165.5GHz
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !------------------------------------------------------------------------------------------------------------

     IMPLICIT NONE
     INTEGER,PARAMETER:: nch=10,nw_3=3
     INTEGER,PARAMETER:: ncoe=6
     REAL(fp),PARAMETER  :: earthrad = 6374._fp, satheight = 833.4_fp
     INTEGER  :: snow_type,ich
     REAL(fp) :: theta,frequency,depth,ts,esv_3w(:),esh_3w(:)
     REAL(fp) :: discriminator(5),emmod(nw_3),dem(nw_3)
     REAL(fp) :: emissivity,em_vector(2)
     REAL(Double) :: dem_coe(nw_3,0:ncoe-1),sinthetas,costhetas,deg2rad

     SAVE  dem_coe

     dem_coe(1,0:ncoe-1)=(/ 2.306844e+000_Double, -7.287718e-003_Double, &
          -6.433248e-004_Double,  1.664216e-005_Double, &
           4.766508e-007_Double, -1.754184e+000_Double/)

     dem_coe(2,0:ncoe-1)=(/ 3.152527e+000_Double, -1.823670e-002_Double, &
          -9.535361e-004_Double,  3.675516e-005_Double, &
           9.609477e-007_Double, -1.113725e+000_Double/)

     dem_coe(3,0:ncoe-1)=(/3.492495e+000_Double, -2.184545e-002_Double,  &
           6.536696e-005_Double,  4.464352e-005_Double, &
          -6.305717e-008_Double, -1.221087e+000_Double/)


     deg2rad = 3.14159_fp*pi/180.0_fp
     sinthetas = sin(theta*deg2rad)* earthrad/(earthrad + satheight)
     sinthetas = sinthetas*sinthetas
     costhetas = one - sinthetas

     DO ich = 1, nw_3
        emmod(ich) = costhetas*esv_3w(ich) + sinthetas*esh_3w(ich)
     END DO

     DO ich=1,nw_3
        dem(ich) = dem_coe(ich,0) + dem_coe(ich,1)*ts + dem_coe(ich,2)*depth +   &
             dem_coe(ich,3)*ts*ts + dem_coe(ich,4)*depth*depth         +   &
             dem_coe(ich,5)*emmod(ich)
     END DO

     emmod(1) = emmod(1) + dem(1)
     emmod(2) = emmod(2) + dem(2)
     emmod(3) = emmod(3) + dem(3)

   ! Match the format of the input variable

   ! Missing value at 23.8 GHz
     discriminator(1) = -999.9_fp
     discriminator(2) = emmod(1)

   ! Missing value at 50.3 GHz
     discriminator(3) = -999.9_fp
     discriminator(4) = emmod(2)
     discriminator(5) = emmod(3)

     CALL em_interpolate(frequency,discriminator,emissivity,snow_type)

     em_vector(1) = emissivity
     em_vector(2) = emissivity

     RETURN

   END SUBROUTINE ems_adjust


   SUBROUTINE six_indices(nind,index_in,threshold,tindex)

   !----------------------------------------------------------------------------------------------------------!
   !$$$  subprogram documentation block
   !
   ! subprogram:
   !
   ! abstract:
   !
   !
   ! input argument list:
   !
   !      nind        -  Number of threshold in decision trees
   !                     to identify each snow type  ( = 6)
   !      index_in    -  six indices to discriminate snow type
   !      threshold   -  Thresholds in decision trees to identify snow types
   !
   ! output argument list:
   !
   !      tindex      - state vaiable to show surface snow emissivity feature
   !              tindex[ ] = .T.: snow emissivity feature matches the
   !                                corresponding threshold for certain snow type
   !              tindex[ ] = .F.: snow emissivity feature doesn't match the
   !                                corresponding threshold for certain snow type
   !
   ! remarks:
   !
   ! program history log:
   !            Banghua Yan, nesdis                                 date: 2003-08-18
   !            Ming Chen, IMSG at NOAA/NESDIS/STAR                 date: 2012-04-28
   !
   ! attributes:
   !   language: f90
   !   machine:  ibm rs/6000 sp
   !
   !----------------------------------------------------------------------------------------------------------!

     IMPLICIT NONE

     INTEGER ::  i,nind
     REAL(fp)::  index_in(:),threshold(:)
     LOGICAL ::  tindex(:)

     DO i=1,nind
        tindex(i) = .FALSE.
        IF (threshold(i) .EQ. -999.9_fp) THEN
           tindex(i) = .TRUE.
        ELSE
           IF ( (i .LE. 2) .OR. (i .GT. (nind-1)) ) THEN
              IF (index_in(i) .GE. threshold(i)) tindex(i) = .TRUE.
           ELSE
              IF (index_in(i) .LE. threshold(i)) tindex(i) = .TRUE.
           ENDIF
        ENDIF
     END DO
     RETURN

   END SUBROUTINE six_indices


END MODULE NESDIS_ATMS_SnowEM_Module
