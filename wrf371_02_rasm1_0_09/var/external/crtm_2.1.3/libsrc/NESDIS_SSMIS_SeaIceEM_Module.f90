!
! NESDIS_SSMIS_SeaIceEM_Module
!
! Module containing the SSMIS microwave sea ice emissivity model
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

MODULE NESDIS_SSMIS_SeaIceEM_Module

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
  PUBLIC :: NESDIS_SSMIS_IceEM


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Version Id for the module
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: NESDIS_SSMIS_SeaIceEM_Module.f90 29405 2013-06-20 20:19:52Z paul.vandelst@noaa.gov $'


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
!       NESDIS_SSMIS_IceEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over sea ice conditions from SSMIS measurements at
!       window channels.
!
! REFERENCES:
!       (1) Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!       (2) Banghua Yan, Fuzhong Weng, Huan Meng, and Norman Grody,2008:"Retrieval of Snow Surface Microwave
!       Emissivity from Advanced Microwave Sounding Unit (AMSU),JGR (revised)
!
! CATEGORY:
!       CRTM : Surface : MW SEA ICE EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_SSMIS_IceEM
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
!         Tb                      BRIGHTNESS TEMPERATURES AT SEVEN SSMI WINDOW CHANNELS
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
!         Ts                       Sea ice  surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
!         Depth:                   The sea ice  depth
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
!   SSMIS_IceEM_TBTS : Subroutine to calculate the microwave emissivity over sea ice conditions using Tb & TS
!
!   SSMIS_IceEM_TB  : Subroutine to calculate the microwave emissivity over sea ice conditions using Tb
!
!
! PROGRAM HISTORY LOG:
!   2008-04-22  yan,b -  implement the algorithm for sea ice emissivity
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

  subroutine NESDIS_SSMIS_IceEM(frequency,                                          & ! INPUT
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

      CALL SSMIS_IceEM_TBTS(frequency,Ts,tb,em_vector)

  else

      if (NALG_TYPE == NALGTWO) then

          CALL SSMIS_IceEM_TB(frequency,tb,em_vector)

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


  END subroutine NESDIS_SSMIS_IceEM


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  subroutine SSMIS_IceEM_TBTS(frequency,Ts,tb,em_vector)
!------------------------------------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: SSMIS_IceEM_TBTS  noaa/nesdis SSM/IS emissivity model over sea ice
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2008-04-22
!
! abstract: Simulate microwave emissivity over  sea ice conditions
!           using SSMIS  measurements and surface temperature
!
! program history log:
!
!      04/2008   : Implement the algorithm for sea ice emissivity to F90 code by Banghua Yan
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

  !sea ice

  ! 19V

   data (coel(1,k),k=1,ncoel)/8.879032e-001,  4.134221e-003,  1.156689e-004, -4.970509e-005, &
        2.223032e-004, -2.124130e-004, -3.771933e-003/

  ! 19H
   data (coel(2,k),k=1,ncoel)/7.697834e-001, -2.562848e-004,  4.465238e-003, -5.527435e-005, &
        4.077187e-004, -3.881797e-004, -3.275239e-003/

  ! 22V
   data (coel(3,k),k=1,ncoel)/8.934178e-001,  2.676737e-004,  1.989840e-004 , 3.819892e-003,  &
        4.860662e-004, -2.959553e-004, -4.052348e-003/

 ! 37 V
   data (coel(4,k),k=1,ncoel)/8.244729e-001, -1.289065e-004,  1.053260e-004, -7.350717e-005, &
        4.954225e-003, -1.689613e-004, -3.988151e-003/

 ! 37 H
   data (coel(5,k),k=1,ncoel)/7.196874e-001, -3.379492e-004,  2.069358e-004, -1.598259e-006, &
         4.319646e-004,  4.347880e-003, -3.541164e-003/

 ! 91V
   data (coe(6,k),k=1,ncoe)/ 8.653359e-001,  5.288367e-004, -5.125727e-005, -8.794597e-004,  &
   3.514040e-004,  9.305991e-005,  5.174877e-003,1.822821e-005, -3.308302e-004, -4.377484e-003/


  ! 91H
    data (coe(7,k),k=1,ncoe)/8.274148e-001,  6.633592e-004, -5.711323e-005, -1.124871e-003, &
    4.037884e-004,  1.087483e-004,  3.410273e-004,4.979564e-003, -4.947016e-004, -4.156180e-003/


  ! 150 H
   data (coe(8,k),k=1,ncoe)/1.111926e+000,  2.059818e-003, -1.997786e-004, -3.249339e-003, &
   1.353929e-003,  1.849169e-004,  3.662670e-004,1.930959e-004,  4.435763e-003, -5.650276e-003/

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

  end subroutine  SSMIS_IceEM_TBTS

  subroutine SSMIS_IceEM_TB(frequency,tb,em_vector)
!------------------------------------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: SSMIS_IceEM_TB  noaa/nesdis SSM/IS emissivity model over sea ice
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2008-04-22
!
! abstract: Simulate microwave emissivity over  sea ice conditions
!           using SSMIS  measurements
!
! program history log:
!
!      04/2008   : Implement the algorithm for ice emissivity to F90 code by Banghua Yan
!
! input argument list:
!
!      frequency: frequency in GHz
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

  !Ice

  ! 19V
  data (coel(1,k),k=1,ncoel)/1.605829e-001,  8.265494e-003, -1.627299e-004, -5.892222e-003, &
        1.479194e-004,  9.405677e-004/

  ! 19H
   data (coel(2,k),k=1,ncoel)/1.382458e-001,  3.330978e-003,  4.223496e-003, -5.128467e-003, &
         3.431191e-004,  6.129800e-004/

  ! 22V
   data (coel(3,k),k=1,ncoel)/1.120268e-001,  4.706078e-003, -1.001120e-004, -2.456973e-003, &
         4.061533e-004,  9.427397e-004/

  ! 37 V
   data (coel(4,k),k=1,ncoel)/5.546296e-002,  4.239192e-003, -1.890347e-004, -6.250949e-003, &
         4.875579e-003,  1.050108e-003/

  ! 37 H
   data (coe(5,k),k=1,ncoel)/3.687292e-002,  3.540579e-003, -5.443526e-005, -5.486699e-003, &
         3.621257e-004,  5.430321e-003/

  ! 91V
   data (coe(6,k),k=1,ncoe)/8.934081e-002,  4.936928e-003, -4.314208e-004, -7.566763e-003,  &
   1.153184e-003,  1.345888e-003,  4.329734e-003,-5.217262e-005, -1.858809e-004/

  ! 91H
    data (coe(7,k),k=1,ncoe)/9.065092e-002,  4.848599e-003, -4.180975e-004, -7.474024e-003 , &
    1.164892e-003,  1.298313e-003, -4.612869e-004,4.912736e-003, -3.571615e-004/


  ! 150 H
   data (coe(8,k),k=1,ncoe)/1.103030e-001,  7.749599e-003, -6.904418e-004, -1.188111e-002,  &
   2.388950e-003,  1.801997e-003, -7.246896e-004,1.021470e-004,  4.622970e-003/

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

  end subroutine  SSMIS_IceEM_TB


END MODULE NESDIS_SSMIS_SeaIceEM_Module
