!
! NESDIS_MHS_SICEEM_Module
!
! Module containing the MHS microwave sea ice emissivity model
!
! References:
!       Yan,B., F.Weng and K.Okamoto,2004: "A microwave snow emissivity model",
!         8th Specialist Meeting on Microwave Radiometry and Remote Sensing Applications,
!         24-27 February, 2004, Rome, Italy.
!
!       Yan,B., F.Weng, H.Meng, and N.Grody, 2007: "Retrieval of Snow Surface Microwave
!         Emissivity from Advanced Microwave Sounding Unit (AMSU),
!         submitted to J.G.R.
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 05-Oct-2007, banghua.yan@noaa.gov
!

MODULE NESDIS_MHS_SICEEM_Module


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp, Double
  USE NESDIS_LandEM_Module
  ! Disable implicit typing
  IMPLICIT NONE

  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NESDIS_ICEEM_MHS


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_MHS_SICEEM_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


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
!       NESDIS_ICEEM_MHS
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over sea ice conditions from MHS measurements at
!       window channels.
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       CRTM : Surface : MW ICE EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL ICEEM_MHS
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
!         Satellite_Angle          The local zenith angle in degree for AMSU measurements.
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
!         Tbb                      BRIGHTNESS TEMPERATURES AT TWO MHS WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   2*1 SCALAR
!
!                         WHICH ARE
!
!                                  tbb[1] = TB at 89 GHz
!                                  tbb[2] = TB at 157 GHz
!
!
!         Ts = Sea Ice Temperature:        The sea ice surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
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
! CALLS:
!
!     MHS_Ts_EM           : Subroutine to calculate sea ice emissivity from MHS and Ts
!
!     MHS_EM              : Subroutine to calculate sea ice emissivity from MHS
!
!
!
! PROGRAM HISTORY LOG:
!   2004-01-01  yan,b   - implement the algorithm for the ice emissivity
!   2007-10-02  yan,b - modify the code for MHS

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
!       Written by:     Banghua Yan, Banghua.Yan@noaa.gov (02-OCTOBER-2007)
!
!------------------------------------------------------------------------------------------------------------



subroutine   NESDIS_ICEEM_MHS(Satellite_Angle,                                               & ! INPUT
                              User_Angle,                                                    & ! INPUT
                              frequency,                                                     & ! INPUT
                              Ts,                                                            & ! INPUT
                              tbb,                                                           & ! INPUT
                              Emissivity_H,                                                  & ! OUTPUT
                              Emissivity_V)                                                    ! OUTPUT


  integer,PARAMETER ::  MHS_Ts_ALG  = 1, MHS_ALG  = 2, PHY_ALG = 3
  integer, parameter::  nwchb = 2
  integer :: input_type,i
  real(fp)    :: Satellite_Angle,User_Angle,frequency,Ts
  real(fp)    :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem
  real(fp)    :: tbb(nwchb)
  real(fp), intent(out) :: Emissivity_H,Emissivity_V


!  Initialization

  em_vector(1) = 0.82_fp

  em_vector(2) = 0.85_fp

  input_type = MHS_Ts_ALG

  do i=1,nwchb
     if((tbb(i) <= 100.0_fp) .or. (tbb(i) >= 320.0_fp) ) then
         input_type =  PHY_ALG
         exit
     end if
     if ((Ts <= 150.0_fp) .or. (Ts >= 330.0_fp) ) input_type = MHS_ALG
  end do


! Emissivity at the local zenith angle of satellite measurements

  GET_option: SELECT CASE (input_type)

  CASE (MHS_Ts_ALG)

     call MHS_Ts_EM(Satellite_Angle,frequency,tbb,Ts,em_vector)

  CASE (MHS_ALG)

    call MHS_EM(Satellite_Angle,frequency,tbb,em_vector)

  CASE (PHY_ALG)

     em_vector(1) = 0.82_fp

     em_vector(2) = 0.85_fp

  END SELECT GET_option


! Get the emissivity angle dependence

  call NESDIS_LandEM(Satellite_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh1,esv1)

  call NESDIS_LandEM(User_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp

! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp

  if (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


end subroutine NESDIS_ICEEM_MHS

subroutine MHS_Ts_EM(angle,frequency,tb,ts,em_vector)

!----------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2007-10-02
!
! abstract:
!         Calculate the emissivity spectrum and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery MHS and TS
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      angle            -  local zenith angle (degree)
!      tb[1] ~ tb[2]  -  brightness temperature at two MHS window channels:
!                              tb[1] : 89 GHz
!                              tb[2] : 157 GHz
! output argument list:
!
!     em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
! important internal variables:
!
!
!     coe      - fitting coefficients to estimate emissivity at 89 and 157  GHz
!
! remarks:
!
! attributes:
!
!   language: f90
!
!-----------------------------------------------------------------------------------------------

   integer, parameter :: nchanw = 2, ncoe= 5
   integer :: ich,jch,k
   real(fp)    :: ts,angle,frequency,emissivity
   real(fp)    :: freq(nchanw),tb(nchanw),em_vector(2)
   real(Double)     :: coe(nchanw,ncoe),emiss(nchanw)

   data (coe(1,k),k=1,ncoe)/7.980265e-001,  5.158272e-003, -4.294968e-004,  &
                           -2.504401e-004, -3.908565e-003/
   data (coe(2,k),k=1,ncoe)/1.204627e+000,  7.588716e-004,  4.476923e-003,  &
                           -3.921044e-004, -6.019630e-003/
   save coe

   !Initialization
    emiss(1:nchanw) = 0.0_fp
    freq(1) = 89.0_fp
    freq(2) =  157.0_fp

   ! estimate emissivity spectrum
   do ich = 1, nchanw
      emiss(ich) = coe(ich,1)
      do jch = 1, nchanw
         emiss(ich) = emiss(ich) + coe(ich,jch+1)*tb(jch)
      enddo
      emiss(ich) = emiss(ich) + coe(ich,4)*angle + coe(ich,5)*ts
   enddo
   !Estimate snow emissivity at a required frequency
   do ich = 2, nchanw
     if(frequency <  freq(1)) then
        emissivity = emiss(1)
        exit
     endif
     if(frequency >= freq(nchanw)) then
        emissivity = emiss(nchanw)
        exit
     endif
     if(frequency <  freq(ich)) then
        emissivity = emiss(ich-1) + (emiss(ich) - emiss(ich-1))*(frequency - freq(ich-1))  &
             /(freq(ich) - freq(ich-1))
        exit
     end if
   enddo
   ! No polarization (Weighted emissivity)
   em_vector(1) = emissivity
   em_vector(2) = emissivity
   return

end subroutine MHS_Ts_EM

subroutine MHS_EM(angle,frequency,tb,em_vector)

!----------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2007-10-02
!
! abstract:
!         Calculate the emissivity spectrum and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery MHS
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      angle            -  local zenith angle (degree)
!      tb[1] ~ tb[2]  -  brightness temperature at two MHS window channels:
!                              tb[1] : 89 GHz
!                              tb[2] : 157 GHz
! output argument list:
!
!     em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
! important internal variables:
!
!
!     coe      - fitting coefficients to estimate emissivity at 89 and 157 GHz
!
!
! remarks:
!
! attributes:
!
!   language: f90
!
!-----------------------------------------------------------------------------------------------

   integer, parameter :: nchanw = 2, ncoe= 4
   integer :: ich,jch,k
   real(fp)    :: angle,frequency,emissivity
   real(fp)    :: freq(nchanw),tb(nchanw),em_vector(2)
   real(Double)     :: coe(nchanw,ncoe),emiss(nchanw)

   !n18
   data (coe(1,k),k=1,ncoe)/1.095665e-001,  6.551590e-003, -3.253155e-003,  1.998972e-004/
   data (coe(2,k),k=1,ncoe)/1.443208e-001,  2.904739e-003,  1.281714e-004,  3.014659e-004/

   save coe

   !Initialization
    emiss(1:nchanw) = 0.0_fp
    freq(1) = 89.0_fp
    freq(2) =  157.0_fp
   ! estimate emissivity spectrum
   do ich = 1, nchanw
      emiss(ich) = coe(ich,1)
      do jch = 1, nchanw
         emiss(ich) = emiss(ich) + coe(ich,jch+1)*tb(jch)
      enddo
      emiss(ich) = emiss(ich) + coe(ich,4)*angle
   enddo
   !Estimate snow emissivity at a required frequency
   do ich = 2, nchanw
     if(frequency <  freq(1)) then
        emissivity = emiss(1)
        exit
     endif
     if(frequency >= freq(nchanw)) then
        emissivity = emiss(nchanw)
        exit
     endif
     if(frequency <  freq(ich)) then
        emissivity = emiss(ich-1) + (emiss(ich) - emiss(ich-1))*(frequency - freq(ich-1))  &
             /(freq(ich) - freq(ich-1))
        exit
     end if
   enddo
   ! No polarization (Weighted emissivity)
   em_vector(1) = emissivity
   em_vector(2) = emissivity
   return
end subroutine MHS_EM

END MODULE NESDIS_MHS_SICEEM_Module
