!
! NESDIS_SSMIS_SnowEM_Module
!
! Module containing the SSMIS microwave snow emissivity model
!
! References:
!       Yan,B., F.Weng, and K.Okamoto, 2004, A microwave snow emissivity model,
!         8th Specialist Meeting on Microwave Radiometry and Remote Sensing Applications,
!         24-27 February, 2004, Rome, Italy.
!
!       Yan,B., F.Weng, and H.Meng, 2008, Retrieval of Snow Surface Microwave
!         Emissivity from Advanced Microwave Sounding Unit (AMSU),
!         J. Geophys. Res., 113, D19206, doi:10.1029/2007JD009559
!
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, 22-Apr-2008, banghua.yan@noaa.gov
!                       Fuzhong Weng, fuzhong.weng@noaa.gov
!
!

MODULE NESDIS_SSMIS_SnowEM_Module


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE NESDIS_LandEM_Module
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: NESDIS_SSMIS_SnowEM


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_SSMIS_SnowEM_Module.f90 99117 2017-11-27 18:37:14Z tong.zhu@noaa.gov $'


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!-------------------------------------------------------------------------------------------------------
!
! NAME:
!       NESDIS_SSMIS_SnowEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over snow conditions from SSMIS measurements at
!       window channels.
!
! REFERENCES:
!       (1) Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!       (2) Banghua Yan, Fuzhong Weng, Huan Meng, and Norman Grody,2008:"Retrieval of Snow Surface Microwave
!       Emissivity from Advanced Microwave Sounding Unit (AMSU),JGR (revised)
!
! CATEGORY:
!       CRTM : Surface : MW SNOW EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_SSMIS_SnowEM
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
!         Angle                    The angle values in degree.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!
!         Tb                      BRIGHTNESS TEMPERATURES AT EIGHT SSMIS WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   7*1 SCALAR
!
!                 tb[1] :  at 19.35 GHz  v-polarization
!                 tb[2] :  at 19.35 GHz  h-polarization
!                 tb[3] :  at 22.235GHz  v-polarization
!                 tb[4] :  at 37    GHz  v-polarization
!                 tb[5] :  at 37    GHz  h-polarization
!                 tb[6] :  at 91.655GHz  v-polarization
!                 tb[7] :  at 91.655GHz  h-polarization
!                 tb[8] :  at 150   GHz  h-polarization
!
!         Ts                       Snow surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
!         Depth:                   The snow  depth
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar

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
! INTERNAL ARGUMENTS:
!
!        SSMIS_Angle         : local zenith angle in degree
!
!
! CALLS:
!
!   SSMIS_SnowEM_TBTS : Subroutine to calculate the microwave emissivity over snow conditions using Tb & TS
!
!   SSMIS_SnowEM_TB  : Subroutine to calculate the microwave emissivity over snow conditions using Tb
!
!
! PROGRAM HISTORY LOG:
!   2008-04-22  yan,b -  implement the algorithm for snow emissivity
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, Perot Inc., Banghua.Yan@noaa.gov (22-April-2008)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!  Copyright (C) 2008 Fuzhong Weng and Banghua Yan
!
!------------------------------------------------------------------------------------------------------

  subroutine NESDIS_SSMIS_SnowEM(frequency,                                          & ! INPUT
                               Angle,                                             & ! INPUT
                               Ts,                                                & ! INPUT
                               tb,                                                & ! INPUT
                               Depth,                                             & ! INPUT
                               Emissivity_H,                                      & ! OUTPUT
                               Emissivity_V)                                        ! OUTPUT

  integer, parameter:: nch = 8, NALGONE = 1, NALGTWO = 2

  real(fp), parameter :: SSMIS_Angle= 53.0_fp

  REAL(fp), PARAMETER ::  ev_default = 0.9_fp

  REAL(fp), PARAMETER ::  eh_default = 0.88_fp

  integer  :: NALG_TYPE, ich

  real(fp), intent(in)     :: Depth,Angle,frequency,Ts,tb(nch)

  real(fp)     :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem

  real(fp), intent(out) :: Emissivity_H, Emissivity_V

  real(fp) :: local_depth
  
  local_depth = depth
  Emissivity_H  =  eh_default  ;  Emissivity_V  =  ev_default

  !Emissivity at SSMIS_Angle

  NALG_TYPE = NALGONE

  if ( (Ts <= 140.0_fp) .or. (Ts >= 330.0_fp) ) NALG_TYPE = NALGTWO

  do ich = 1, nch

     if ( (tb(ich) .le. 50.0_fp) .or. (tb(ich) .ge. 330.0_fp) )  RETURN

  enddo

  if (NALG_TYPE == NALGONE ) then

      CALL SSMIS_SnowEM_TBTS(frequency,Ts,tb,em_vector)

  else

      if (NALG_TYPE == NALGTWO) then

          CALL SSMIS_SnowEM_TB(frequency,tb,em_vector)

      else

          RETURN

      endif

  endif

  ! Get the emissivity angle dependence

  if (local_depth .lt. 0.1_fp) local_depth = 0.1_fp

  if (local_depth .gt. 10.0_fp) local_depth = 10.0_fp

  call NESDIS_LandEM(SSMIS_Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,local_depth,esh1,esv1)

  call NESDIS_LandEM(Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,local_depth,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp

! Emissivity at User's Angle

  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

! Quality Control

  if(Emissivity_H .gt. 1.0_fp) Emissivity_H = 1.0_fp

  if(Emissivity_H .lt. 0.3_fp) Emissivity_H = 0.3_fp

  if(Emissivity_V .gt. 1.0_fp) Emissivity_V = 1.0_fp

  if(Emissivity_V .lt. 0.3_fp) Emissivity_V = 0.3_fp

  if(Emissivity_V .lt. Emissivity_H) Emissivity_V = Emissivity_H

  return


  END subroutine NESDIS_SSMIS_SnowEM


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  subroutine SSMIS_SnowEM_TBTS(frequency,Ts,tb,em_vector)
!------------------------------------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: SSMIS_SnowEM_TBTS noaa/nesdis SSM/IS emissivity model over snow
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2008-04-22
!
! abstract: Simulate microwave emissivity over  snow conditions
!           using SSMIS  measurements and surface temperature
!
! program history log:
!
!      04/2008   : Implement the algorithm for snow emissivity to F90 code by Banghua Yan
!
! input argument list:
!
!      frequency: frequency in GHz
!      Ts       : scattering layer temperature (K)
!
!      tb       : BRIGHTNESS TEMPERATURES AT EIGHT SSMIS WINDOW CHANNELS (K)
!
!                 tb[1] :  at 19.35 GHz  v-polarization
!                 tb[2] :  at 19.35 GHz  h-polarization
!                 tb[3] :  at 22.235GHz  v-polarization
!                 tb[4] :  at 37    GHz  v-polarization
!                 tb[5] :  at 37    GHz  h-polarization
!                 tb[6] :  at 91.655GHz  v-polarization
!                 tb[7] :  at 91.655GHz  h-polarization
!                 tb[8] :  at 150   GHz  h-polarization

! output argument list:
!
!      em_vector     :  emissivity at two polarizations
!           em_vector[1] = eh
!           em_vector[2] = ev
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  integer,parameter :: nch = 8, nchl = 5, ncoe = 10, ncoel = 7

  integer,parameter :: nchv = 4, nchh = 4

  integer :: ich, jch, k, nchx

  real(fp), parameter, dimension(nch) ::   &

      freq=(/19.35_fp,19.35_fp,22.235_fp,37.0_fp,37.0_fp,  &

             91.655_fp, 91.655_fp, 150._fp/)

  real(fp), parameter, dimension(nchv) ::  &

       freqv=(/19.35_fp,22.235_fp,37.0_fp,91.655_fp/)

  real(fp), parameter, dimension(nchh) :: &

       freqh=(/19.35_fp,37.0_fp,91.655_fp,150._fp/)

  real(fp) ev(nchv),eh(nchh)

  real(fp) frequency,Ts,tb(*),em_vector(*),em(nch)

  real(fp) coel(nchl,ncoel) , coe(nch,ncoe)

  !snow

  ! 19V

   data (coel(1,k),k=1,ncoel)/8.981891e-001,  3.625613e-003,  6.417758e-005,  7.202909e-004, &
                              1.096995e-004, -1.891836e-004, -3.924795e-003/
  ! 19H
   data (coel(2,k),k=1,ncoel)/8.046941e-001, -1.027311e-003,  4.531593e-003,  8.102580e-004, &
                              3.999433e-004, -4.280942e-004, -3.512124e-003/
  ! 22V
   data (coel(3,k),k=1,ncoel)/9.288673e-001,  1.027458e-004,  1.681447e-004,  4.128222e-003, &
                              3.315663e-004, -2.441378e-004, -4.197293e-003/
  ! 37 V
   data (coel(4,k),k=1,ncoel)/8.905462e-001, -9.035784e-004,  4.710625e-004,  4.889428e-004, &
                              5.158289e-003, -5.767560e-004, -4.185101e-003/
  ! 37 H
   data (coel(5,k),k=1,ncoel)/8.412739e-001, -1.485943e-003,  7.072478e-004,  8.233924e-004, &
                              7.863072e-004,  3.776767e-003, -3.959416e-003/
  ! 91V
   data (coe(6,k),k=1,ncoe)/9.599149e-001, -5.281095e-004,  5.402629e-004, -2.068626e-004,  &

        5.283779e-004, -4.540359e-004,  5.131390e-003, 6.851193e-005, -2.794534e-004, -4.609839e-003/

  ! 91H
    data (coe(7,k),k=1,ncoe)/9.543127e-001, -4.628303e-004,  6.871569e-004, -5.296606e-004,  &

        7.954335e-004, -6.303838e-004,  3.386022e-004, 4.961890e-003, -3.591371e-004, -4.582450e-003/

  ! 150 H
   data (coe(8,k),k=1,ncoe)/1.190583e+000,  9.914897e-004,  4.892147e-004, -2.502870e-003,  &

        9.855123e-004, -1.407097e-004,  6.999056e-004, 3.696870e-004,  4.173831e-003, -5.769176e-003/


   save coel, coe

   ! Initialization

  em_vector(1) = 0.7_fp

  em_vector(2) = 0.8_fp

   !*** Get intial emissivity for each frequency

   ! frequencies from 19.35 to 37 GHz

     do ich = 1, nchl

        em(ich) = coel(ich,1)

        do jch = 1, nchl

            em(ich) = em(ich) + coel(ich,1+jch)*tb(jch)

        enddo

        em(ich) = em(ich) + coel(ich,ncoel)*Ts

     enddo

   ! frequencies from 91.655 to 160 GHz

     do ich = nchl+1, nch

        em(ich) = coe(ich,1)

        do jch = 1, nch

            em(ich) = em(ich) + coe(ich,1+jch)*tb(jch)

        enddo

        em(ich) = em(ich) + coe(ich,ncoe)*Ts

     enddo

   !*** Interpolate emissivity at a certain frequency

   ev(1) = em(1)   ! 19V
   ev(2) = em(3)
   ev(3) = em(4)
   ev(4) = em(6)
   eh(1) = em(2)
   eh(2) = em(5)
   eh(3) = em(7)
   eh(4) = em(8)

   ! v-component
  nchx = 4
  do ich=1,nchv
     if(frequency <= freqv(ich)) then
        nchx = ich
        exit
     end if
  end do


  if (nchx == 1) then
     em_vector(2) = ev(1)
  else
     if (frequency .ge. freqv(nchv)) then
        em_vector(2) = ev(nchv)
     else
        em_vector(2) = ev(nchx-1) + (ev(nchx) - ev(nchx-1))* &
             (frequency - freqv(nchx-1))/(freqv(nchx) - freqv(nchx-1))
     end if
  end if


! h-component
  nchx = 4
  do ich=1,nchh
     if(frequency <= freqh(ich)) then
        nchx = ich
        exit
     end if
  end do

  if (nchx == 1) then
     em_vector(1) = eh(1)
  else
     if (frequency .ge. freqh(nchh)) then
        em_vector(1) = eh(nchh)
     else
        em_vector(1) = eh(nchx-1) + (eh(nchx) - eh(nchx-1))* &
             (frequency - freqh(nchx-1))/(freqh(nchx) - freqh(nchx-1))
     end if

  end if

  end subroutine  SSMIS_SnowEM_TBTS

  subroutine SSMIS_SnowEM_TB(frequency,tb,em_vector)
!------------------------------------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: SSMIS_SnowEM_TB  noaa/nesdis SSM/IS emissivity model over snow
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2008-04-22
!
! abstract: Simulate microwave emissivity over  snow conditions
!           using SSMIS  measurements
!
! program history log:
!
!      04/2008   : Implement the algorithm for snow/ice emissivity to F90 code by Banghua Yan
!
! input argument list:
!
!      frequency: frequency in GHz
!
!      tb       : BRIGHTNESS TEMPERATURES AT SEVEN SSMI WINDOW CHANNELS (K)
!
!                 tb[1] :  at 19.35 GHz  v-polarization
!                 tb[2] :  at 19.35 GHz  h-polarization
!                 tb[3] :  at 22.235GHz  v-polarization
!                 tb[4] :  at 37    GHz  v-polarization
!                 tb[5] :  at 37    GHz  h-polarization
!                 tb[6] :  at 91.655GHz  v-polarization
!                 tb[7] :  at 91.655GHz  h-polarization
!                 tb[8] :  at 150   GHz  h-polarization

! output argument list:
!
!      em_vector     :  emissivity at two polarizations
!           em_vector[1] = eh
!           em_vector[2] = ev
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  integer,parameter :: nch = 8, nchl = 5, ncoe = 9, ncoel = 6

  integer,parameter :: nchv = 4, nchh = 4

  integer :: ich, jch, k, nchx

  real(fp), parameter, dimension(nch) ::   &

      freq=(/19.35_fp,19.35_fp,22.235_fp,37.0_fp,37.0_fp,  &

             91.655_fp, 91.655_fp, 150._fp/)

  real(fp), parameter, dimension(nchv) ::  &

       freqv=(/19.35_fp,22.235_fp,37.0_fp,91.655_fp/)

  real(fp), parameter, dimension(nchh) :: &
       freqh=(/19.35_fp,37.0_fp,91.655_fp,150._fp/)

  real(fp) ev(nchv),eh(nchh)

  real(fp) frequency,tb(*),em_vector(*),em(nch)

  real(fp) coel(nchl,ncoel) , coe(nch,ncoe)

  !snow

  ! 19V
  data (coel(1,k),k=1,ncoel)/3.310760e-001,  1.113338e-002,  1.055088e-003, -8.363702e-003, &
                             7.934856e-004, -2.215757e-003/

  ! 19H
   data (coel(2,k),k=1,ncoel)/2.972085e-001,  5.691049e-003,  5.418323e-003, -7.318597e-003, &
        1.011842e-003, -2.241598e-003/
  ! 22V
   data (coel(3,k),k=1,ncoel)/3.223795e-001,  8.131774e-003,  1.227853e-003, -5.586470e-003,  &
         1.062826e-003, -2.411414e-003/

  ! 37 V
   data (coel(4,k),k=1,ncoel)/2.858207e-001,  7.102124e-003,  1.527692e-003, -9.197528e-003,  &
         5.887423e-003, -2.737736e-003/
  ! 37 H
   data (coel(5,k),k=1,ncoel)/2.691576e-001,  6.088046e-003,  1.706904e-003, -8.340728e-003,  &
         1.476130e-003,  1.732309e-003/
  ! 91V
   data (coe(6,k),k=1,ncoe)/3.121725e-001,  2.465109e-003,  6.876130e-004, -3.946431e-003,  &
         1.778840e-003,-1.474594e-003,  3.657647e-003,1.179306e-003, -1.877871e-003/


  ! 91H
    data (coe(7,k),k=1,ncoe)/3.104197e-001,  2.512587e-003,  8.336290e-004, -4.247001e-003,  &
          2.038457e-003, -1.644870e-003, -1.126363e-003,6.066067e-003, -1.948056e-003/


  ! 150 H
   data (coe(8,k),k=1,ncoe)/3.799388e-001,  4.737473e-003,  6.736217e-004, -7.182906e-003,  &
        2.550452e-003, -1.417927e-003, -1.144465e-003,1.759830e-003,  2.173425e-003/

   save coel, coe

   ! Initialization

  em_vector(1) = 0.7_fp

  em_vector(2) = 0.8_fp

   !*** Get intial emissivity for each frequency

   ! frequencies from 19.35 to 37 GHz

     do ich = 1, nchl
        em(ich) = coel(ich,1)

        do jch = 1, nchl

            em(ich) = em(ich) + coel(ich,1+jch)*tb(jch)

        enddo

     enddo

   ! frequencies from 91.655 to 160 GHz

     do ich = nchl+1, nch

        em(ich) = coe(ich,1)

        do jch = 1, nch

            em(ich) = em(ich) + coe(ich,1+jch)*tb(jch)

        enddo

     enddo

   !*** Interpolate emissivity at a certain frequency

   ev(1) = em(1)   ! 19V
   ev(2) = em(3)
   ev(3) = em(4)
   ev(4) = em(6)
   eh(1) = em(2)
   eh(2) = em(5)
   eh(3) = em(7)
   eh(4) = em(8)

   ! v-component
  nchx = 4
  do ich=1,nchv
     if(frequency <= freqv(ich)) then
        nchx = ich
        exit
     end if
  end do


  if (nchx == 1) then
     em_vector(2) = ev(1)
  else
     if (frequency .ge. freqv(nchv)) then
        em_vector(2) = ev(nchv)
     else
        em_vector(2) = ev(nchx-1) + (ev(nchx) - ev(nchx-1))* &
             (frequency - freqv(nchx-1))/(freqv(nchx) - freqv(nchx-1))
     end if
  end if

! h-component
  nchx = 4
  do ich=1,nchh
     if(frequency <= freqh(ich)) then
        nchx = ich
        exit
     end if
  end do

  if (nchx == 1) then
     em_vector(1) = eh(1)
  else
     if (frequency .ge. freqh(nchh)) then
        em_vector(1) = eh(nchh)
     else
        em_vector(1) = eh(nchx-1) + (eh(nchx) - eh(nchx-1))* &
             (frequency - freqh(nchx-1))/(freqh(nchx) - freqh(nchx-1))
     end if

  end if


  end subroutine  SSMIS_SnowEM_TB


END MODULE NESDIS_SSMIS_SnowEM_Module
