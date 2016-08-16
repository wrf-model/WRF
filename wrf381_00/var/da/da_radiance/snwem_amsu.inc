subroutine  snwem_amsu(theta,frequency,snow_depth,ts,tba,tbb,esh,esv)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  noaa/nesdis emissivity model over snow/ice for AMSU-A/B
!
!   prgmmr: Banghua Yan      org: nesdis              date: 2003-08-18
!
! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
!    snow for AMSU-A/B. The processing varies according to input parameters
!         Option 1 :  AMSU-A & B window channels of brightness temperatures (Tb)
!                      and surface temperature (Ts) are available
!         Option 2 :  AMSU-A window channels of Tb and Ts are available
!         Option 3 :  AMSU-A & B window channels of Tb are available
!         Option 4 :  AMSU-A window channels of Tb are available
!         Option 5 :  AMSU-B window channels of Tb and Ts are available
!         Option 6 :  AMSU-B window channels of Tb are available
!         Option 7 :  snow depth and Ts are available
!
! references:
!    Yan, B., F. Weng and K.Okamoto,2004:
!       "A microwave snow emissivity model, submitted to TGRS
!
!   version: 3
!
! program history log:
!     beta       : November 28, 2000
!
!     version 2.0: June 18, 2003.
!
!                  Version 2.0 enhances the capability/performance of beta version of
!               land emissivity model (LandEM) over snow conditions. Two new subroutines
!               (i.e., snowem_tb and six_indices) are added as replacements of the
!               previous snow emissivity. If AMSU measurements are not available, the
!               results are the same as these in beta version. The new snow emissivity
!               model is empirically derived from satellite retrievals and ground-based
!               measurements.
!
!     version 3.0: August 18, 2003.
!
!                  Version 3.0 is an extended version of LandEM 2.0 over snow conditions.
!               It covers seven different options (see below for details) for LandEM
!               inputs over snow conditions. When All or limited AMSU measurements are
!               available, one of the subroutines sem_ABTs, sem_ATs, sem_AB, sem_amsua,
!               sem_BTs and sem_amsub, which are empirically derived from satellite
!               retrievals and ground-based measurements, are called to simuate snow
!               emissivity; when no AMSU measurements are avalaiable, the subroutine
!               ALandEM_Snow is called where the results over snow conditions in beta
!               version are adjusted with a bias correction that is obtained using a
!               statistical algorithm. Thus, LandEM 3.0 significantly enhances the
!               flexibility/performance of LandEM 2.0 in smulating emissivity over snow
!               conditions.
!
!
!   2004-07-26 okamoto - modified the version 3.0 for use in gsi
!   2004-12-13 kokron  - the 'depth' variable was declared then used before being given
!                        a value. changed to use snow_depth in all calculations
!
!
! input argument list:
!     theta            -  local zenith angle in radian
!     frequency        -  frequency in GHz
!     t_skin           -  scattering layer temperature (K)   (gdas)
!     snow_depth       -  scatter medium depth (mm)          (gdas)
!     tba[1] ~ tba[4]  -  Tb at four AMSU-A window channels
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89 GHz
!     tbb[1] ~ tbb[2]  -  Tb at two AMSU-B window channels:
!                              tbb[1] : 89 GHz
!                              tbb[2] : 150 GHz
!       When tba[ ] or tbb[ ] = -999.9: a missing value (no available data)
!
! output argument list:
!       esv        -  emissivity at vertical polarization
!       esh        -  emissivity at horizontal polarization
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
!                    999: AMSU measurements are not available or over non-snow conditions
! important internal variables/parameters:
!
!       INDATA[1] ~ INDATA[7]  -  seven options calling different subroutines
!       INDATA(1) = ABTs   :  call sem_ABTs
!       INDATA(2) = ATs    :  call sem_ATs
!       INDATA(3) = AMSUAB :  call sem_AB
!       INDATA(4) = AMSUA  :  call sem_amsua
!       INDATA(5) = BTs    :  call sem_BTs
!       INDATA(6) = AMSUB  :  call sem_amsub
!       INDATA(7) = MODL   :  call ALandEM_Snow
!       input_type     -  specific option index
!       tb[1] ~ tb[5]  -  Tb at five AMSU-A & B window channels:
!                              tb[1] = tba[1]
!                              tb[2] = tba[2]
!                              tb[3] = tba[3]
!                              tb[4] = tba[4]
!                              tb[5] = tbb[2]
!
! remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov or Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind)      :: nch,nwcha,nwchb,nwch,nalg
  Parameter(nch = 10, nwcha = 4, nwchb = 2, nwch = 5,nalg = 7)
  real(r_kind)    :: theta,frequency,ts,snow_depth
  real(r_kind)    :: em_vector(2),esv,esh
  real(r_kind)    :: tb(nwch),tba(nwcha),tbb(nwchb)
  logical :: INDATA(nalg),AMSUAB,AMSUA,AMSUB,ABTs,ATs,BTs,MODL
  integer(i_kind) :: snow_type,input_type,i,ich,np,k
  
  Equivalence(INDATA(1), ABTs)
  Equivalence(INDATA(2), ATs)
  Equivalence(INDATA(3), AMSUAB)
  Equivalence(INDATA(4), AMSUA)
  Equivalence(INDATA(5), BTs)
  Equivalence(INDATA(6), AMSUB)
  Equivalence(INDATA(7), MODL)

! Initialization
  call em_initialization(frequency,em_vector)
  snow_type  = -999
  input_type = -999
  do k = 1, nalg
     INDATA(k) = .TRUE.
  end do

! Read AMSU & Ts data and set available option
! Get five AMSU-A/B window measurements
  tb(1) = tba(1); tb(2) = tba(2); tb(3) = tba(3); tb(4) = tba(4); tb(5) = tbb(2)

! Check available data
  if((ts <= 100.0_r_kind) .or. (ts >= 320.0_r_kind) ) then
     ABTs = .false.;  ATs  = .false.;  BTs  = .false.;  MODL = .false.
  end if
  do i=1,nwcha
     if((tba(i) <= 100.0_r_kind) .or. (tba(i) >= 320.0_r_kind) ) then
        ABTs   = .false.;  ATs    = .false.;  AMSUAB = .false.;  AMSUA  = .false.
        exit
     end if
  end do
  do i=1,nwchb
     if((tbb(i) <= 100.0_r_kind) .or. (tbb(i) >= 320.0_r_kind) ) then
        ABTs  = .false.;  AMSUAB = .false.;  BTs  = .false.;  AMSUB  = .false.
        exit
     end if
  end do
  if((snow_depth < 0.0_r_kind) .or. (snow_depth >= 3000.0_r_kind)) MODL = .false.
  if((frequency >= 80._r_kind) .and. (BTs)) then
     ATs = .false.;  AMSUAB = .false.
  end if

! Check input type and call a specific Option/subroutine
  do np = 1, nalg
     if (INDATA(np)) then
        input_type = np
        exit
     end if
  end do
!    write(UNIT=stdout,FMT='(a,2f6.1,i5,a,4f7.1,a,2f7.1)') 'freq,theta,input_tyep=',frequency,theta,input_type, ' tba=',tba,' tbb=',tbb

  GET_option: SELECT CASE (input_type)
  CASE (1)
     call sem_ABTs(theta,frequency,tb,ts,snow_type,em_vector)
  CASE (2)
     call sem_ATs(theta,frequency,tba,ts,snow_type,em_vector)
  CASE (3)
     call sem_AB(theta,frequency,tb,snow_type,em_vector)
  CASE (4)
     call sem_amsua(theta,frequency,tba,snow_type,em_vector)
  CASE(5)
     call sem_BTs(theta,frequency,tbb,ts,snow_type,em_vector)
  CASE(6)
     call sem_amsub(theta,frequency,tbb,snow_type,em_vector)
  CASE(7)
     call ALandEM_Snow(theta,frequency,snow_depth,ts,snow_type,em_vector)
  END SELECT GET_option
  
  esv = em_vector(1)
  esh = em_vector(2)
  
  return
end subroutine snwem_amsu


!---------------------------------------------------------------------!
subroutine em_initialization(frequency,em_vector)

!$$$  subprogram documentation block
!
! subprogram:   AMSU-A/B snow emissivity initialization
!
!   prgmmr:  Banghua Yan                org: nesdis              date: 2003-08-18
!
! abstract: AMSU-A/B snow emissivity initialization
!
! program history log:
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
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
!  use constants, only: one
  implicit none
  
  integer(i_kind) ::  nch,ncand
  Parameter(nch = 10,ncand=16)
  real(r_kind)    :: frequency,em_vector(*),freq(nch)
  real(r_kind)    :: em(ncand,nch)
  real(r_kind)    :: kratio, bconst,emissivity
  integer(i_kind) :: snow_type,ich,k
  save      em

! Sixteen candidate snow emissivity spectra
  data  (em(1,k),k=1,nch)/0.87_r_kind,0.89_r_kind,0.91_r_kind,0.93_r_kind, &
       0.94_r_kind,0.94_r_kind,0.94_r_kind,0.93_r_kind,0.92_r_kind,0.90_r_kind/
  data  (em(2,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.92_r_kind,0.91_r_kind, &
       0.90_r_kind,0.90_r_kind,0.91_r_kind,0.91_r_kind,0.91_r_kind,0.86_r_kind/
  data  (em(3,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind, &
       0.86_r_kind,0.86_r_kind,0.85_r_kind,0.85_r_kind,0.82_r_kind,0.82_r_kind/
  data  (em(4,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.93_r_kind,0.93_r_kind, &
       0.93_r_kind,0.93_r_kind,0.89_r_kind,0.88_r_kind,0.79_r_kind,0.79_r_kind/
  data  (em(5,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.85_r_kind, &
       0.84_r_kind,0.83_r_kind,0.83_r_kind,0.82_r_kind,0.79_r_kind,0.73_r_kind/
  data  (em(6,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.86_r_kind,0.82_r_kind, &
       0.80_r_kind,0.79_r_kind,0.78_r_kind,0.78_r_kind,0.77_r_kind,0.77_r_kind/
  data  (em(7,k),k=1,nch)/0.88_r_kind,0.86_r_kind,0.85_r_kind,0.80_r_kind, &
       0.78_r_kind,0.77_r_kind,0.77_r_kind,0.76_r_kind,0.72_r_kind,0.72_r_kind/
  data  (em(8,k),k=1,nch)/0.93_r_kind,0.94_r_kind,0.96_r_kind,0.96_r_kind, &
       0.95_r_kind,0.93_r_kind,0.87_r_kind,0.86_r_kind,0.74_r_kind,0.65_r_kind/
  data  (em(9,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.84_r_kind,0.80_r_kind, &
       0.76_r_kind,0.76_r_kind,0.75_r_kind,0.75_r_kind,0.70_r_kind,0.69_r_kind/
  data  (em(10,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.83_r_kind,0.77_r_kind, &
       0.73_r_kind,0.68_r_kind,0.66_r_kind,0.66_r_kind,0.68_r_kind,0.67_r_kind/
  data  (em(11,k),k=1,nch)/0.89_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind, &
       0.86_r_kind,0.82_r_kind,0.77_r_kind,0.76_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(12,k),k=1,nch)/0.88_r_kind,0.87_r_kind,0.86_r_kind,0.83_r_kind, &
       0.81_r_kind,0.77_r_kind,0.74_r_kind,0.73_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(13,k),k=1,nch)/0.86_r_kind,0.86_r_kind,0.86_r_kind,0.85_r_kind, &
       0.82_r_kind,0.78_r_kind,0.69_r_kind,0.68_r_kind,0.51_r_kind,0.47_r_kind/
  data  (em(14,k),k=1,nch)/0.89_r_kind,0.88_r_kind,0.87_r_kind,0.83_r_kind, &
       0.80_r_kind,0.75_r_kind,0.70_r_kind,0.70_r_kind,0.64_r_kind,0.60_r_kind/
  data  (em(15,k),k=1,nch)/0.91_r_kind,0.92_r_kind,0.93_r_kind,0.88_r_kind, &
       0.84_r_kind,0.76_r_kind,0.66_r_kind,0.64_r_kind,0.48_r_kind,0.44_r_kind/
  data  (em(16,k),k=1,nch)/0.94_r_kind,0.95_r_kind,0.97_r_kind,0.91_r_kind, &
       0.86_r_kind,0.74_r_kind,0.63_r_kind,0.63_r_kind,0.50_r_kind,0.45_r_kind/
  data  freq/4.9_r_kind,6.93_r_kind,10.65_r_kind,18.7_r_kind,23.8_r_kind, &
       31.4_r_kind, 50.3_r_kind,52.5_r_kind, 89.0_r_kind, 150._r_kind/

! Initialization for emissivity at certain frequency
!    In case of no any inputs available for various options
!    A constant snow type & snow emissivity spectrum is assumed
!                    (e.g., powder) snow_type = 4

! Specify snow emissivity at required frequency
  do ich = 2, nch
     if(frequency <  freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency <  freq(ich)) then
        emissivity = em(4,ich-1) + (em(4,ich) - em(4,ich-1))     &
             *(frequency - freq(ich-1))/(freq(ich) - freq(ich-1))
        exit
     end if
  end do
  
! Extrapolate to lower frequencies than 4.9GHz
  if (frequency <= freq(1)) then
     kratio = (em(4,2) - em(4,1))/(freq(2) - freq(1))
     bconst = em(4,1) - kratio*freq(1)
     emissivity =  kratio*frequency + bconst
     if(emissivity >  one)         emissivity = one
     if(emissivity <= 0.8_r_kind) emissivity = 0.8_r_kind
  end if
  

! Assume emissivity = constant at frequencies >= 150 GHz
  if (frequency >= freq(nch)) emissivity = em(4,nch)
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine em_initialization



!---------------------------------------------------------------------!
subroutine  em_interpolate(frequency,discriminator,emissivity,snow_type)

!$$$  subprogram documentation block
!
! subprogram:  determine snow_type and calculate emissivity
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract: 1. Find one snow emissivity spectrum to mimic the emission
!              property of the realistic snow condition using a set of
!              discrminators
!           2. Interpolate/extrapolate emissivity at a required frequency
!
! program history log:
!
! input argument list:
!
!      frequency        - frequency in GHz
!      discriminators   - emissivity discriminators at five AMSU-A & B window
!                         channels
!            discriminator[1]   :  emissivity discriminator at 23.8 GHz
!            discriminator[2]   :  emissivity discriminator at 31.4 GHz
!            discriminator[3]   :  emissivity discriminator at 50.3 GHz
!            discriminator[4]   :  emissivity discriminator at 89   GHz
!            discriminator[5]   :  emissivity discriminator at 150  GHz
!
!       Note: discriminator(1) and discriminator(3) are missing value in
!            'AMSU-B & Ts','AMUS-B' and 'MODL' options., which are defined to as -999.9,
! output argument list:
!
!     em_vector[1] and [2]  -  emissivity at two polarizations.
!     snow_type             - snow type
!
! important internal variables:
!
!     freq[1 ~ 10]  -  ten frequencies for sixteen snow types of emissivity
!     em[1~16,*]    -  sixteen snow emissivity spectra
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
!  use constants, only: zero, one
  implicit none
  
  integer(i_kind),parameter:: ncand = 16,nch =10
  integer(i_kind):: ich,ichmin,ichmax,i,j,k,s,snow_type
  real(r_kind)   :: dem,demmin0
  real(r_kind)   :: em(ncand,nch)
  real(r_kind)   :: frequency,freq(nch),emissivity,discriminator(*),emis(nch)
  real(r_kind)   :: cor_factor,adjust_check,kratio, bconst
  data  freq/4.9_r_kind, 6.93_r_kind, 10.65_r_kind, 18.7_r_kind,&
       23.8_r_kind, 31.4_r_kind, 50.3_r_kind, 52.5_r_kind, &
       89.0_r_kind, 150._r_kind/

! Sixteen candidate snow emissivity spectra
  data  (em(1,k),k=1,nch)/0.87_r_kind,0.89_r_kind,0.91_r_kind,0.93_r_kind,0.94_r_kind,&
       0.94_r_kind,0.94_r_kind,0.93_r_kind,0.92_r_kind,0.90_r_kind/
  data  (em(2,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.92_r_kind,0.91_r_kind,0.90_r_kind,&
       0.90_r_kind,0.91_r_kind,0.91_r_kind,0.91_r_kind,0.86_r_kind/
  data  (em(3,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,0.86_r_kind,&
       0.86_r_kind,0.85_r_kind,0.85_r_kind,0.82_r_kind,0.82_r_kind/
  data  (em(4,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.93_r_kind,0.93_r_kind,0.93_r_kind,&
       0.93_r_kind,0.89_r_kind,0.88_r_kind,0.79_r_kind,0.79_r_kind/
  data  (em(5,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.85_r_kind,0.84_r_kind,&
       0.83_r_kind,0.83_r_kind,0.82_r_kind,0.79_r_kind,0.73_r_kind/
  data  (em(6,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.86_r_kind,0.82_r_kind,0.80_r_kind,&
       0.79_r_kind,0.78_r_kind,0.78_r_kind,0.77_r_kind,0.77_r_kind/
  data  (em(7,k),k=1,nch)/0.88_r_kind,0.86_r_kind,0.85_r_kind,0.80_r_kind,0.78_r_kind,&
       0.77_r_kind,0.77_r_kind,0.76_r_kind,0.72_r_kind,0.72_r_kind/
  data  (em(8,k),k=1,nch)/0.93_r_kind,0.94_r_kind,0.96_r_kind,0.96_r_kind,0.95_r_kind,&
       0.93_r_kind,0.87_r_kind,0.86_r_kind,0.74_r_kind,0.65_r_kind/
  data  (em(9,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.84_r_kind,0.80_r_kind,0.76_r_kind,&
       0.76_r_kind,0.75_r_kind,0.75_r_kind,0.70_r_kind,0.69_r_kind/
  data  (em(10,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.83_r_kind,0.77_r_kind,0.73_r_kind,&
       0.68_r_kind,0.66_r_kind,0.66_r_kind,0.68_r_kind,0.67_r_kind/
  data  (em(11,k),k=1,nch)/0.89_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,0.86_r_kind,&
       0.82_r_kind,0.77_r_kind,0.76_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(12,k),k=1,nch)/0.88_r_kind,0.87_r_kind,0.86_r_kind,0.83_r_kind,0.81_r_kind,&
       0.77_r_kind,0.74_r_kind,0.73_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(13,k),k=1,nch)/0.86_r_kind,0.86_r_kind,0.86_r_kind,0.85_r_kind,0.82_r_kind,&
       0.78_r_kind,0.69_r_kind,0.68_r_kind,0.51_r_kind,0.47_r_kind/
  data  (em(14,k),k=1,nch)/0.89_r_kind,0.88_r_kind,0.87_r_kind,0.83_r_kind,0.80_r_kind,&
       0.75_r_kind,0.70_r_kind,0.70_r_kind,0.64_r_kind,0.60_r_kind/
  data  (em(15,k),k=1,nch)/0.91_r_kind,0.92_r_kind,0.93_r_kind,0.88_r_kind,0.84_r_kind,&
       0.76_r_kind,0.66_r_kind,0.64_r_kind,0.48_r_kind,0.44_r_kind/
  data  (em(16,k),k=1,nch)/0.94_r_kind,0.95_r_kind,0.97_r_kind,0.91_r_kind,0.86_r_kind,&
       0.74_r_kind,0.63_r_kind,0.63_r_kind,0.50_r_kind,0.45_r_kind/
  save em

! Adjust unreasonable discriminator
  if (discriminator(4) > discriminator(2))    &
       discriminator(4) = discriminator(2) +(discriminator(5) - discriminator(2))*  &
       (150.0_r_kind - 89.0_r_kind)/(150.0_r_kind - 31.4_r_kind)
  if ( (discriminator(3) /= -999.9_r_kind) .and.       &
       ( ((discriminator(3)-0.01_r_kind) > discriminator(2)) .or.     &
       ((discriminator(3)-0.01_r_kind) < discriminator(4)))    )    &
       discriminator(3) = discriminator(2) +  &
       (discriminator(4) - discriminator(2))*(89.0_r_kind - 50.3_r_kind) &
       / (89.0_r_kind - 31.4_r_kind)
  
! Find a snow emissivity spectrum
  if(snow_type .eq. -999) then
     demmin0 = 10.0_r_kind
     do k = 1, ncand
        dem = zero
        ichmin = 1
        ichmax = 3
        if(discriminator(1) == -999.9_r_kind) then
           ichmin = 2
           ichmax = 2
        end if
        do ich = ichmin,ichmax
           dem = dem + abs(discriminator(ich) - em(k,ich+4))
        end do
        do ich = 4,5
           dem = dem + abs(discriminator(ich) - em(k,ich+5))
        end do
        if (dem < demmin0) then
           demmin0 = dem
           snow_type = k
        end if
     end do
  end if
   
! Shift snow emissivity according to discriminator at 31.4 GHz
  cor_factor = discriminator(2) - em(snow_type,6)
  do ich = 1, nch
     emis(ich) = em(snow_type,ich) + cor_factor
     if(emis(ich) .gt. one)         emis(ich) = one
     if(emis(ich) .lt. 0.3_r_kind) emis(ich) = 0.3_r_kind
  end do
   
! Emisivity data quality control
  adjust_check = zero
  do ich = 5, 9
     if (ich .le. 7) then
        if (discriminator(ich - 4) .ne. -999.9_r_kind) &
             adjust_check = adjust_check + abs(emis(ich) - discriminator(ich - 4))
     else
        if (discriminator(ich - 4) .ne. -999.9_r_kind)  &
             adjust_check = adjust_check + abs(emis(ich+1) - discriminator(ich - 4))
     end if
  end do
   
  if (adjust_check >= 0.04_r_kind) then
     if (discriminator(1) /= -999.9_r_kind) then
        if (discriminator(1) < emis(4)) then
           emis(5) = emis(4) + &
                (31.4_r_kind - 23.8_r_kind) * &
                (discriminator(2) - emis(4))/(31.4_r_kind - 18.7_r_kind)
        else
           emis(5) = discriminator(1)
        end if
     end if
     emis(6) = discriminator(2)
     if (discriminator(3) /= -999.9_r_kind) then
        emis(7) = discriminator(3)
     else
!       In case of missing the emissivity discriminator at 50.3 GHz
        emis(7) = emis(6) + (89.0_r_kind - 50.3_r_kind) * &
             (discriminator(4) - emis(6))/(89.0_r_kind - 31.4_r_kind)
     end if
     emis(8) = emis(7)
     emis(9) = discriminator(4)
     emis(10) = discriminator(5)
  end if
  
! Estimate snow emissivity at a required frequency
  do i = 2, nch
     if(frequency <  freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency <  freq(i)) then
        emissivity = emis(i-1) + (emis(i) - emis(i-1))*(frequency - freq(i-1))  &
             /(freq(i) - freq(i-1))
        exit
     end if
  end do
  
! Extrapolate to lower frequencies than 4.9GHz
  if (frequency <= freq(1)) then
     kratio = (emis(2) - emis(1))/(freq(2) - freq(1))
     bconst = emis(1) - kratio*freq(1)
     emissivity =  kratio*frequency + bconst
     if(emissivity > one)          emissivity = one
     if(emissivity <= 0.8_r_kind) emissivity = 0.8_r_kind
  end if
  
! Assume emissivity = constant at frequencies >= 150 GHz
  if (frequency >= freq(nch)) emissivity = emis(nch)
  
  return
end subroutine em_interpolate


!---------------------------------------------------------------------!
subroutine sem_ABTs(theta,frequency,tb,ts,snow_type,em_vector)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at a required frequency with respect to secenery ABTs
!
! program history log:
!
! input argument list:
!
!     frequency        -  frequency in GHz
!     theta            -  local zenith angle (not used here)
!     tb[1] ~ tb[5]    -  brightness temperature at five AMSU window channels:
!                              tb[1] : 23.8 GHz
!                              tb[2] : 31.4 GHz
!                              tb[3] : 50.3 GHz
!                              tb[4] : 89.0 GHz
!                              tb[5] : 150  GHz
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
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind),parameter:: ncand = 16,nch =10,nthresh=38
  integer(i_kind),parameter:: nind=6,ncoe=8,nLIcoe=6,nHIcoe=12
  integer(i_kind):: ich,i,j,k,num,npass,snow_type,md0,md1,nmodel(ncand-1)
  real(r_kind)   :: theta,frequency,tb150,LI,HI,DS1,DS2,DS3
  real(r_kind)   :: em(ncand,nch), em_vector(*)
  real(r_kind)   :: tb(*),freq(nch),DTB(nind-1),DI(nind-1),       &
       DI_coe(nind-1,0:ncoe-1),threshold(nthresh,nind),       &
       index_in(nind),threshold0(nind)
  real(r_kind)   :: LI_coe(0:nLIcoe-1),HI_coe(0:nHIcoe-1)
  real(r_kind)   :: ts,emissivity
  real(r_kind)   :: discriminator(5)
  logical:: pick_status,tindex(nind)
  save      em,threshold,DI_coe,LI_coe, HI_coe,nmodel,freq
  
  data  freq/4.9_r_kind,6.93_r_kind,10.65_r_kind,18.7_r_kind,23.8_r_kind, &
       31.4_r_kind, 50.3_r_kind,52.5_r_kind, 89.0_r_kind, 150._r_kind/
  data  nmodel/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/
  
! Fitting coefficients for five discriminators
  data (DI_coe(1,k),k=0,ncoe-1)/ &
       3.285557e-002_r_kind,  2.677179e-005_r_kind,  &
       4.553101e-003_r_kind,  5.639352e-005_r_kind,  &
       -1.825188e-004_r_kind,  1.636145e-004_r_kind,  &
       1.680881e-005_r_kind, -1.708405e-004_r_kind/
  data (DI_coe(2,k),k=0,ncoe-1)/ &
       -4.275539e-002_r_kind, -2.541453e-005_r_kind,  &
       4.154796e-004_r_kind,  1.703443e-004_r_kind,  &
       4.350142e-003_r_kind,  2.452873e-004_r_kind,  &
       -4.748506e-003_r_kind,  2.293836e-004_r_kind/
  data (DI_coe(3,k),k=0,ncoe-1)/ &
       -1.870173e-001_r_kind, -1.061678e-004_r_kind,  &
      2.364055e-004_r_kind, -2.834876e-005_r_kind,  &
      4.899651e-003_r_kind, -3.418847e-004_r_kind,  &
      -2.312224e-004_r_kind,  9.498600e-004_r_kind/
  data (DI_coe(4,k),k=0,ncoe-1)/ &
       -2.076519e-001_r_kind,  8.475901e-004_r_kind,  &
       -2.072679e-003_r_kind, -2.064717e-003_r_kind,  &
       2.600452e-003_r_kind,  2.503923e-003_r_kind,  &
       5.179711e-004_r_kind,  4.667157e-005_r_kind/
  data (DI_coe(5,k),k=0,ncoe-1)/ &
       -1.442609e-001_r_kind, -8.075003e-005_r_kind,  &
       -1.790933e-004_r_kind, -1.986887e-004_r_kind,  &
       5.495115e-004_r_kind, -5.871732e-004_r_kind,  &
       4.517280e-003_r_kind,  7.204695e-004_r_kind/
  
! Fitting coefficients for emissivity index at 31.4 GHz
  data  LI_coe/ &
       7.963632e-001_r_kind,  7.215580e-003_r_kind,  &
       -2.015921e-005_r_kind, -1.508286e-003_r_kind,  &
       1.731405e-005_r_kind, -4.105358e-003_r_kind/

! Fitting coefficients for emissivity index at 150 GHz
  data  HI_coe/ &
       1.012160e+000_r_kind,  6.100397e-003_r_kind, &
       -1.774347e-005_r_kind, -4.028211e-003_r_kind, &
       1.224470e-005_r_kind,  2.345612e-003_r_kind, &
       -5.376814e-006_r_kind, -2.795332e-003_r_kind, &
       8.072756e-006_r_kind,  3.529615e-003_r_kind, &
       1.955293e-006_r_kind, -4.942230e-003_r_kind/

! Six thresholds for sixteen candidate snow types
! Note: some snow type contains several possible
!      selections for six thresholds

!1 Wet Snow
  data (threshold(1,k),k=1,6)/0.88_r_kind,0.86_r_kind,-999.9_r_kind,&
       0.01_r_kind,0.01_r_kind,200._r_kind/
  data (threshold(2,k),k=1,6)/0.88_r_kind,0.85_r_kind,-999.9_r_kind,&
       0.06_r_kind,0.10_r_kind,200._r_kind/
  data (threshold(3,k),k=1,6)/0.88_r_kind,0.83_r_kind,-0.02_r_kind,&
       0.12_r_kind,0.16_r_kind,204._r_kind/
  data (threshold(4,k),k=1,6)/0.90_r_kind,0.89_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(5,k),k=1,6)/0.92_r_kind,0.85_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!2 Grass_after_Snow
  data (threshold(6,k),k=1,6)/0.84_r_kind,0.83_r_kind,-999.9_r_kind,&
       0.08_r_kind,0.10_r_kind,195._r_kind/
  data (threshold(7,k),k=1,6)/0.85_r_kind,0.85_r_kind,-999.9_r_kind,&
       0.10_r_kind,-999.9_r_kind,190._r_kind/
  data (threshold(8,k),k=1,6)/0.86_r_kind,0.81_r_kind,-999.9_r_kind,&
       0.12_r_kind,-999.9_r_kind,200._r_kind/
  data (threshold(9,k),k=1,6)/0.86_r_kind,0.81_r_kind,0.0_r_kind,&
       0.12_r_kind,-999.9_r_kind,189._r_kind/
  data (threshold(10,k),k=1,6)/0.90_r_kind,0.81_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,195._r_kind/
  
!3 RS_Snow (A)
  data (threshold(11,k),k=1,6)/0.80_r_kind,0.76_r_kind,-999.9_r_kind,&
       0.05_r_kind,-999.9_r_kind,185._r_kind/
  data (threshold(12,k),k=1,6)/0.82_r_kind,0.78_r_kind,-999.9_r_kind,&
       -999.9_r_kind,0.25_r_kind,180._r_kind/
  data (threshold(13,k),k=1,6)/0.90_r_kind,0.76_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,180._r_kind/
  
!4 Powder  Snow
  data (threshold(14,k),k=1,6)/0.89_r_kind,0.73_r_kind,-999.9_r_kind,&
       0.20_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(15,k),k=1,6)/0.89_r_kind,0.75_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(16,k),k=1,6)/0.93_r_kind,0.72_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!5 RS_Snow (B)
  data (threshold(17,k),k=1,6)/0.82_r_kind,0.70_r_kind,-999.9_r_kind,&
       0.20_r_kind,-999.9_r_kind,160._r_kind/
  data (threshold(18,k),k=1,6)/0.83_r_kind,0.70_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,160._r_kind/

!6 RS_Snow (C)
  data (threshold(19,k),k=1,6)/0.75_r_kind,0.76_r_kind,-999.9_r_kind,&
       0.08_r_kind,-999.9_r_kind,172._r_kind/
  data (threshold(20,k),k=1,6)/0.77_r_kind,0.72_r_kind,-999.9_r_kind,&
       0.12_r_kind,0.15_r_kind,175._r_kind/
  data (threshold(21,k),k=1,6)/0.78_r_kind,0.74_r_kind,-999.9_r_kind,&
       -999.9_r_kind,0.20_r_kind,172._r_kind/
  data (threshold(22,k),k=1,6)/0.80_r_kind,0.77_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,170._r_kind/
  data (threshold(23,k),k=1,6)/0.82_r_kind,-999.9_r_kind,-999.9_r_kind,&
       0.15_r_kind,0.22_r_kind,170._r_kind/
  data (threshold(24,k),k=1,6)/0.82_r_kind,0.73_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,170._r_kind/

!7 RS_Snow (D)
  data (threshold(25,k),k=1,6)/0.75_r_kind,0.70_r_kind,-999.9_r_kind,&
       0.15_r_kind,0.25_r_kind,167._r_kind/
  data (threshold(26,k),k=1,6)/0.77_r_kind,0.76_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(27,k),k=1,6)/0.80_r_kind,0.72_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(28,k),k=1,6)/0.77_r_kind,0.73_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  
  data (threshold(29,k),k=1,6)/0.81_r_kind,0.71_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  data (threshold(30,k),k=1,6)/0.82_r_kind,0.69_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  
!8 Thin Crust Snow
  data (threshold(31,k),k=1,6)/0.88_r_kind,0.58_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  
!9 RS_Snow (E)
  data (threshold(32,k),k=1,6)/0.73_r_kind,0.67_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  
!10 Bottom Crust Snow (A)
  data (threshold(33,k),k=1,6)/0.83_r_kind,0.66_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
  
!11 Shallow Snow
  data (threshold(34,k),k=1,6)/0.82_r_kind,0.60_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!12 Deep Snow
  data (threshold(35,k),k=1,6)/0.77_r_kind,0.60_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!13 Crust Snow
  data (threshold(36,k),k=1,6)/0.77_r_kind,0.7_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!14 Medium Snow
  data (threshold(37,k),k=1,6)/-999.9_r_kind,0.55_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!15 Bottom Crust Snow(B)
  data (threshold(38,k),k=1,6)/0.74_r_kind,-999.9_r_kind,-999.9_r_kind,&
       -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

!16 Thick Crust Snow
! lowest priority: No constraints

! Sixteen candidate snow emissivity spectra
  data  (em(1,k),k=1,nch)/0.87_r_kind,0.89_r_kind,0.91_r_kind,0.93_r_kind,&
       0.94_r_kind,0.94_r_kind,0.94_r_kind,0.93_r_kind,0.92_r_kind,0.90_r_kind/
  data  (em(2,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.92_r_kind,0.91_r_kind,&
       0.90_r_kind,0.90_r_kind,0.91_r_kind,0.91_r_kind,0.91_r_kind,0.86_r_kind/
  data  (em(3,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,&
       0.86_r_kind,0.86_r_kind,0.85_r_kind,0.85_r_kind,0.82_r_kind,0.82_r_kind/
  data  (em(4,k),k=1,nch)/0.91_r_kind,0.91_r_kind,0.93_r_kind,0.93_r_kind,&
       0.93_r_kind,0.93_r_kind,0.89_r_kind,0.88_r_kind,0.79_r_kind,0.79_r_kind/
  data  (em(5,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.88_r_kind,0.85_r_kind,&
       0.84_r_kind,0.83_r_kind,0.83_r_kind,0.82_r_kind,0.79_r_kind,0.73_r_kind/
  data  (em(6,k),k=1,nch)/0.90_r_kind,0.89_r_kind,0.86_r_kind,0.82_r_kind,&
       0.80_r_kind,0.79_r_kind,0.78_r_kind,0.78_r_kind,0.77_r_kind,0.77_r_kind/
  data  (em(7,k),k=1,nch)/0.88_r_kind,0.86_r_kind,0.85_r_kind,0.80_r_kind,&
       0.78_r_kind,0.77_r_kind,0.77_r_kind,0.76_r_kind,0.72_r_kind,0.72_r_kind/
  data  (em(8,k),k=1,nch)/0.93_r_kind,0.94_r_kind,0.96_r_kind,0.96_r_kind,&
       0.95_r_kind,0.93_r_kind,0.87_r_kind,0.86_r_kind,0.74_r_kind,0.65_r_kind/
  data  (em(9,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.84_r_kind,0.80_r_kind,&
       0.76_r_kind,0.76_r_kind,0.75_r_kind,0.75_r_kind,0.70_r_kind,0.69_r_kind/
  data  (em(10,k),k=1,nch)/0.87_r_kind,0.86_r_kind,0.83_r_kind,0.77_r_kind,&
       .73_r_kind,0.68_r_kind,0.66_r_kind,0.66_r_kind,0.68_r_kind,0.67_r_kind/
  data  (em(11,k),k=1,nch)/0.89_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,&
       0.86_r_kind,0.82_r_kind,0.77_r_kind,0.76_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(12,k),k=1,nch)/0.88_r_kind,0.87_r_kind,0.86_r_kind,0.83_r_kind,&
       0.81_r_kind,0.77_r_kind,0.74_r_kind,0.73_r_kind,0.69_r_kind,0.64_r_kind/
  data  (em(13,k),k=1,nch)/0.86_r_kind,0.86_r_kind,0.86_r_kind,0.85_r_kind,&
       0.82_r_kind,0.78_r_kind,0.69_r_kind,0.68_r_kind,0.51_r_kind,0.47_r_kind/
  data  (em(14,k),k=1,nch)/0.89_r_kind,0.88_r_kind,0.87_r_kind,0.83_r_kind,&
       0.80_r_kind,0.75_r_kind,0.70_r_kind,0.70_r_kind,0.64_r_kind,0.60_r_kind/
  data  (em(15,k),k=1,nch)/0.91_r_kind,0.92_r_kind,0.93_r_kind,0.88_r_kind,&
       0.84_r_kind,0.76_r_kind,0.66_r_kind,0.64_r_kind,0.48_r_kind,0.44_r_kind/
  data  (em(16,k),k=1,nch)/0.94_r_kind,0.95_r_kind,0.97_r_kind,0.91_r_kind,&
       0.86_r_kind,0.74_r_kind,0.63_r_kind,0.63_r_kind,0.50_r_kind,0.45_r_kind/

!***  DEFINE SIX DISCRIMINATORS

  dtb(1) = tb(1) - tb(2)
  dtb(2) = tb(2) - tb(4)
  dtb(3) = tb(2) - tb(5)
  dtb(4) = tb(3) - tb(5)
  dtb(5) = tb(4) - tb(5)
  tb150  = tb(5)
  
  LI = LI_coe(0)
  do i=0,1
     LI = LI + LI_coe(2*i+1)*tb(i+1) + LI_coe(2*i+2)*tb(i+1)*tb(i+1)
  end do
  LI = LI + LI_coe(nLIcoe-1)*ts
  
  HI = HI_coe(0)
  do i=0,4
     HI = HI + HI_coe(2*i+1)*tb(i+1) + HI_coe(2*i+2)*tb(i+1)*tb(i+1)
  end do
  HI = HI + HI_coe(nHIcoe-1)*ts
  
  do num=1,nind-1
     DI(num) = DI_coe(num,0) + DI_coe(num,1)*tb(2)
     do i=1,5
        DI(num) = DI(num) + DI_coe(num,1+i)*DTB(i)
     end do
     DI(num) = DI(num) +  DI_coe(num,ncoe-1)*ts
  end do
  
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
  pick_status = .false.

! Pick one snow type
! Check all possible selections for six thresholds for each snow type
  do i = 1, ncand - 1
     md1 = nmodel(i)
     do j = md0, md1
        npass = 0
        do k = 1 , nind
           threshold0(k) = threshold(j,k)
        end do
        CALL six_indices(nind,index_in,threshold0,tindex)

! Corrections
        if((i == 5)  .and. (index_in(2) >  0.75_r_kind)) tindex(2) = .false.
        if((i == 5)  .and. (index_in(4) >  0.20_r_kind)                        &
             .and. (index_in(1) >  0.88_r_kind)) tindex(1) = .false.
        if((i == 10) .and. (index_in(1) <= 0.83_r_kind)) tindex(1) = .true.
        if((i == 13) .and. (index_in(2) <  0.52_r_kind)) tindex(2) = .true.
        do k = 1, nind
           if(.not.tindex(k)) exit
           npass = npass + 1
        end do
        if(npass == nind) exit
     end do
     
     if(npass == nind) then
        pick_status = .true.
        snow_type  = i
     end if
     if(pick_status) exit
     md0 = md1 + 1
  end do
  
  discriminator(1) = LI + DI(1)
  discriminator(2) = LI
  discriminator(3) = DI(4) + HI
  discriminator(4) = LI - DI(2)
  discriminator(5) = HI
  
  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine sem_ABTs


!---------------------------------------------------------------------!
subroutine six_indices(nind,index_in,threshold,tindex)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!
! program history log:
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
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind) ::  i,nind
  real(r_kind)    ::  index_in(*),threshold(*)
  logical ::  tindex(*)
  
  do i=1,nind
     tindex(i) = .false.
     if (threshold(i) .eq. -999.9_r_kind) then
        tindex(i) = .true.
     else
        if ( (i .le. 2) .or. (i .gt. (nind-1)) ) then
           if (index_in(i) .ge. threshold(i)) tindex(i) = .true.
        else
           if (index_in(i) .le. threshold(i)) tindex(i) = .true.
        end if
     end if
  end do
  return
  
end subroutine six_indices


!---------------------------------------------------------------------!
subroutine sem_AB(theta,frequency,tb,snow_type,em_vector)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to option AMSUAB
!
! program history log:
!   2004-10-28  treadon - correct problem in declared dimensions of array coe
!
! input argument list:
!
!      frequency    -  frequency in GHz
!      theta        -  local zenith angle (not used here)
!      tb[1]~tb[5]  -  brightness temperature at five AMSU-A & B window channels:
!                              tb[1] : 23.8 GHz
!                              tb[2] : 31.4 GHz
!                              tb[3] : 50.3 GHz
!                              tb[4] : 89   GHz
!                              tb[5] : 150  GHz
!
! output argument list:
!
!     em_vector[1] and [2] - emissivity at two polarizations.
!                            set esv = esh here and will be updated
!     snow_type       - snow type (reference [2])
!
! important internal variables:
!
!     coe23    - fitting coefficients to estimate discriminator at 23.8 GHz
!     coe31    - fitting coefficients to estimate discriminator at 31.4 GHz
!     coe50    - fitting coefficients to estimate discriminator at 50.3 GHz
!     coe89    - fitting coefficients to estimate discriminator at 89   GHz
!     coe150   - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind),parameter:: nch =10,nwch = 5,ncoe = 10
  real(r_kind)    :: tb(*),theta,frequency
  real(r_kind)    :: em_vector(*),emissivity,discriminator(nwch)
  integer(i_kind) :: i,snow_type,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(12),coe31)
  Equivalence (coe(23),coe50)
  Equivalence (coe(34),coe89)
  Equivalence (coe(45),coe150)

! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
  data (coe23(k),k=0,6)/&
       -1.326040e+000_r_kind,  2.475904e-002_r_kind, &
       -5.741361e-005_r_kind, -1.889650e-002_r_kind, &
       6.177911e-005_r_kind,  1.451121e-002_r_kind, &
       -4.925512e-005_r_kind/
  
! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
  data (coe31(k),k=0,6)/ &
       -1.250541e+000_r_kind,  1.911161e-002_r_kind, &
       -5.460238e-005_r_kind, -1.266388e-002_r_kind, &
       5.745064e-005_r_kind,  1.313985e-002_r_kind, &
       -4.574811e-005_r_kind/

! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
  data (coe50(k),k=0,6)/  &
       -1.246754e+000_r_kind,  2.368658e-002_r_kind, &
       -8.061774e-005_r_kind, -3.206323e-002_r_kind, &
       1.148107e-004_r_kind,  2.688353e-002_r_kind, &
       -7.358356e-005_r_kind/
  
! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
  data (coe89(k),k=0,8)/ &
       -1.278780e+000_r_kind,  1.625141e-002_r_kind, &
       -4.764536e-005_r_kind, -1.475181e-002_r_kind, &
       5.107766e-005_r_kind,  1.083021e-002_r_kind, &
       -4.154825e-005_r_kind,  7.703879e-003_r_kind, &
       -6.351148e-006_r_kind/

! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb5
  data coe150/&
     -1.691077e+000_r_kind,  3.352403e-002_r_kind, &
     -7.310338e-005_r_kind, -4.396138e-002_r_kind, &
     1.028994e-004_r_kind,  2.301014e-002_r_kind, &
     -7.070810e-005_r_kind,  1.270231e-002_r_kind, &
     -2.139023e-005_r_kind, -2.257991e-003_r_kind, &
     1.269419e-005_r_kind/
  
  save coe23,coe31,coe50,coe89,coe150

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*11)
     if (ich .le. 3) nvalid_ch = 3
     if (ich .eq. 4) nvalid_ch = 4
     if (ich .eq. 5) nvalid_ch = 5
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*11 + 2*i)*tb(i) +  &
             coe((ich-1)*11 + 2*i+1)*tb(i)*tb(i)
     end do
  end do
!  Identify one snow emissivity spectrum and interpolate/extrapolate emissivity
!  at a required frequency
  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine sem_AB


!---------------------------------------------------------------------!
subroutine sem_ATs(theta,frequency,tba,ts,snow_type,em_vector)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUAB
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature
!      tba[1] ~ tba[4]  -  brightness temperature at five AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89   GHz
! output argument list:
!
!     em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type (reference [2])
!
! important internal variables:
!
!     coe23      - fitting coefficients to estimate discriminator at 23.8 GHz
!     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
!     coe50      - fitting coefficients to estimate discriminator at 50.3 GHz
!     coe89      - fitting coefficients to estimate discriminator at 89   GHz
!     coe150     - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind),parameter:: nch =10,nwch = 5,ncoe = 9
  real(r_kind)    :: tba(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer(i_kind) :: snow_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)

! Fitting Coefficients at 23.8 GHz: Using Tb1, Tb2 and Ts
  data (coe23(k),k=0,5)/ &
       8.210105e-001_r_kind,  1.216432e-002_r_kind,  &
       -2.113875e-005_r_kind, -6.416648e-003_r_kind,  &
       1.809047e-005_r_kind, -4.206605e-003_r_kind/
  
! Fitting Coefficients at 31.4 GHz: Using Tb1, Tb2 and Ts
  data (coe31(k),k=0,5)/ &
       7.963632e-001_r_kind,  7.215580e-003_r_kind,  &
       -2.015921e-005_r_kind, -1.508286e-003_r_kind,  &
       1.731405e-005_r_kind, -4.105358e-003_r_kind/
  
! Fitting Coefficients at 50.3 GHz: Using Tb1, Tb2, Tb3 and Ts
  data (coe50(k),k=0,7)/ &
       1.724160e+000_r_kind,  5.556665e-003_r_kind, &
       -2.915872e-005_r_kind, -1.146713e-002_r_kind, &
       4.724243e-005_r_kind,  3.851791e-003_r_kind, &
       -5.581535e-008_r_kind, -5.413451e-003_r_kind/

! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4 and Ts
  data coe89/ &
       9.962065e-001_r_kind,  1.584161e-004_r_kind, &
       -3.988934e-006_r_kind,  3.427638e-003_r_kind, &
       -5.084836e-006_r_kind, -6.178904e-004_r_kind, &
       1.115315e-006_r_kind,  9.440962e-004_r_kind, &
       9.711384e-006_r_kind, -4.259102e-003_r_kind/

! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4 and Ts
  data coe150/ &
       -5.244422e-002_r_kind,  2.025879e-002_r_kind,  &
       -3.739231e-005_r_kind, -2.922355e-002_r_kind, &
       5.810726e-005_r_kind,  1.376275e-002_r_kind, &
       -3.757061e-005_r_kind,  6.434187e-003_r_kind, &
       6.190403e-007_r_kind, -2.944785e-003_r_kind/

  save coe23,coe31,coe50,coe89,coe150

! Calculate emissivity discriminators at five AMSU window channels
  DO ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     if (ich .le. 2) nvalid_ch = 2
     if (ich .eq. 3) nvalid_ch = 3
     if (ich .ge. 4) nvalid_ch = 4
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
             coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
     end do
     discriminator(ich) = discriminator(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
  end do
  
  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine sem_ATs

!---------------------------------------------------------------------!
subroutine sem_amsua(theta,frequency,tba,snow_type,em_vector)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUA
!
! program history log:
!   2004-10-28  treadon - correct problem in declared dimensions of array coe
!
! input argument list:
!
!      frequency      -  frequency in GHz
!      theta          -  local zenith angle (not used here)
!      tba[1]~tba[4]  -  brightness temperature at five AMSU-A window channels:
!                            tba[1] : 23.8 GHz
!                            tba[2] : 31.4 GHz
!                            tba[3] : 50.3 GHz
!                            tba[4] : 89   GHz
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type
!
! important internal variables:
!
!     coe23      - fitting coefficients to estimate discriminator at 23.8 GHz
!     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
!     coe50      - fitting coefficients to estimate discriminator at 50.3 GHz
!     coe89      - fitting coefficients to estimate discriminator at 89   GHz
!     coe150     - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind),parameter:: nch =10,nwch = 5,ncoe = 8
  real(r_kind)    :: tba(*),theta
  real(r_kind)    :: em_vector(*),emissivity,frequency,discriminator(nwch)
  integer(i_kind) :: snow_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)
  
! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
  data (coe23(k),k=0,6)/ &
       -1.326040e+000_r_kind,  2.475904e-002_r_kind, -5.741361e-005_r_kind, &
       -1.889650e-002_r_kind,  6.177911e-005_r_kind,  1.451121e-002_r_kind, &
       -4.925512e-005_r_kind/
  
! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
  data (coe31(k),k=0,6)/ &
       -1.250541e+000_r_kind,  1.911161e-002_r_kind, -5.460238e-005_r_kind, &
       -1.266388e-002_r_kind,  5.745064e-005_r_kind,  1.313985e-002_r_kind, &
       -4.574811e-005_r_kind/

! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
  data (coe50(k),k=0,6)/ &
       -1.246754e+000_r_kind,  2.368658e-002_r_kind, -8.061774e-005_r_kind, &
       -3.206323e-002_r_kind,  1.148107e-004_r_kind,  2.688353e-002_r_kind, &
       -7.358356e-005_r_kind/
  
! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
  data coe89/ &
       -1.278780e+000_r_kind, 1.625141e-002_r_kind, -4.764536e-005_r_kind, &
       -1.475181e-002_r_kind, 5.107766e-005_r_kind,  1.083021e-002_r_kind, &
       -4.154825e-005_r_kind,  7.703879e-003_r_kind, -6.351148e-006_r_kind/
  
! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4
  data coe150/ &
       -1.624857e+000_r_kind, 3.138243e-002_r_kind, -6.757028e-005_r_kind, &
       -4.178496e-002_r_kind, 9.691893e-005_r_kind,  2.165964e-002_r_kind, &
       -6.702349e-005_r_kind, 1.111658e-002_r_kind, -1.050708e-005_r_kind/
  
  save coe23,coe31,coe50,coe150


! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     if (ich .le. 2) nvalid_ch = 3
     if (ich .ge. 3) nvalid_ch = 4
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tba(i) +  &
             coe((ich-1)*10 + 2*i+1)*tba(i)*tba(i)
     end do
  end do

! Quality Control
  if(discriminator(4) .gt. discriminator(2))   &
       discriminator(4) = discriminator(2) + (150.0_r_kind - 89.0_r_kind)*  &
       (discriminator(5) - discriminator(2))/ &
       (150.0_r_kind - 31.4_r_kind)
  
! Quality control at 50.3 GHz
  if((discriminator(3) .gt. discriminator(2)) .or.  &
       (discriminator(3) .lt. discriminator(4)))      &
       discriminator(3) = discriminator(2) + (89.0_r_kind - 50.3_r_kind)*   &
       (discriminator(4) - discriminator(2))/(89.0_r_kind - 31.4_r_kind)
  
  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine sem_amsua


!---------------------------------------------------------------------!
subroutine sem_BTs(theta,frequency,tbb,ts,snow_type,em_vector)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery BTs
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature in degree
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type
!
! important internal variables:
!
!     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
!     coe89      - fitting coefficients to estimate discriminator at 89   GHz
!     coe150     - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
!  use kinds, only: r_kind,i_kind
  implicit none

  integer(i_kind),parameter:: nch =10,nwch = 3,ncoe = 5
  real(r_kind)    :: tbb(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,ed0(nwch),discriminator(5)
  integer(i_kind) :: snow_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe31(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe31)
  Equivalence (coe(11),coe89)
  Equivalence (coe(21),coe150)
  
! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5 and Ts
  data coe31/ 3.110967e-001_r_kind,  1.100175e-002_r_kind, -1.677626e-005_r_kind,    &
       -4.020427e-003_r_kind,  9.242240e-006_r_kind, -2.363207e-003_r_kind/
! Fitting Coefficients at 89 GHz: Using Tb4, Tb5 and Ts
  data coe89/  1.148098e+000_r_kind,  1.452926e-003_r_kind,  1.037081e-005_r_kind, &
       1.340696e-003_r_kind, -5.185640e-006_r_kind, -4.546382e-003_r_kind /
! Fitting Coefficients at 150 GHz: Using Tb4, Tb5 and Ts
  data coe150/ 1.165323e+000_r_kind, -1.030435e-003_r_kind,  4.828009e-006_r_kind,  &
       4.851731e-003_r_kind, -2.588049e-006_r_kind, -4.990193e-003_r_kind/
  save coe31,coe89,coe150

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     ed0(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +   &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
     ed0(ich) = ed0(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
  end do

! Quality control
  if(ed0(2) .gt. ed0(1))     &
       ed0(2) = ed0(1) + (150.0_r_kind - 89.0_r_kind)*(ed0(3) - ed0(1)) / &
       (150.0_r_kind - 31.4_r_kind)

! Match the format of the input variable
! Missing value at 23.8 GHz
  discriminator(1) = -999.9_r_kind;  discriminator(2) = ed0(1)
! Missing value at 50.3 GHz
  discriminator(3) = -999.9_r_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine sem_BTs


!---------------------------------------------------------------------!
subroutine sem_amsub(theta,frequency,tbb,snow_type,em_vector)


!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUB
!
! program history log:
!   2004-10-28  treadon - correct problem in declared dimensions of array coe
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!     em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!     snow_type        -  snow type (reference [2])
!
! important internal variables:
!
!     coe31    - fitting coefficients to estimate discriminator at 31.4 GHz
!     coe89    - fitting coefficients to estimate discriminator at 89   GHz
!     coe150   - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
!  use kinds, only: r_kind,i_kind
  implicit none
  
  integer(i_kind),parameter:: nch =10,nwch = 3,ncoe = 4
  real(r_kind)    :: tbb(*)
  real(r_kind)    :: em_vector(*),emissivity,frequency,ed0(nwch),discriminator(5)
  integer(i_kind) :: snow_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe31(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  real(r_kind)    :: theta,dem,demmin0
  
  Equivalence (coe(1),coe31)
  Equivalence (coe(11),coe89)
  Equivalence (coe(21),coe150)

! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5
  data coe31/-4.015636e-001_r_kind,9.297894e-003_r_kind, -1.305068e-005_r_kind, &
       3.717131e-004_r_kind, -4.364877e-006_r_kind/
! Fitting Coefficients at 89 GHz: Using Tb4, Tb5
  data coe89/-2.229547e-001_r_kind, -1.828402e-003_r_kind,1.754807e-005_r_kind, &
       9.793681e-003_r_kind, -3.137189e-005_r_kind/
! Fitting Coefficients at 150 GHz: Using Tb4, Tb5
  data coe150/-3.395416e-001_r_kind,-4.632656e-003_r_kind,1.270735e-005_r_kind, &
       1.413038e-002_r_kind,-3.133239e-005_r_kind/
  save coe31,coe89,coe150

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch
     ed0(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*tbb(i) +  &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
  end do

! Quality Control
  if(ed0(2) .gt. ed0(1))     &
       ed0(2) = ed0(1) + (150.0_r_kind - 89.0_r_kind) * &
       (ed0(3) - ed0(1))/(150.0_r_kind - 31.4_r_kind)

! Match the format of the input variable
! Missing value at 23.8 GHz
  discriminator(1) = -999.9_r_kind; discriminator(2) = ed0(1)
! Missing value at 50.3 GHz
  discriminator(3) = -999.9_r_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

  return
end subroutine sem_amsub


!---------------------------------------------------------------------!
subroutine ALandEM_Snow(theta,frequency,snow_depth,t_skin,snow_type,em_vector)


!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity at required frequency with respect to option MODL
!   using the LandEM and a bias correction algorithm, where the original LandEM with a
!   bias correction algorithm is referred to as value-added LandEM or AlandEM.
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      snow_depth       -  snow depth in mm
!      t_skin           -  surface temperature
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
!                           at three AMSU window channels computed using LandEM
!    esv_3w[1] and esh_3w[1] : 31.4 GHz
!    esv_3w[2] and esh_3w[2] : 89   GHz
!    esv_3w[3] and esh_3w[3] : 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
!  use constants, only: zero, one
  implicit none
  
  integer(i_kind) :: nw_ind
  parameter(nw_ind=3)
  real(r_kind) theta, frequency, freq,snow_depth, mv, t_soil, t_skin, em_vector(2)
  real(r_kind) esv,esh,esh0,esv0,theta0,b
  integer(i_kind) snow_type,ich
  real(r_kind)   freq_3w(nw_ind),esh_3w(nw_ind),esv_3w(nw_ind)
  complex(r_kind)  eair
  data   freq_3w/31.4_r_kind,89.0_r_kind,150.0_r_kind/
  
  eair = dcmplx(one,-zero)
  b = t_skin
  snow_type = -999
  
  call snowem_default(theta,frequency,snow_depth,t_skin,b,esv0,esh0)
  
  theta0 = theta
  do ich = 1, nw_ind
     freq =freq_3w(ich)
     theta = theta0
     call snowem_default(theta,freq,snow_depth,t_skin,b,esv,esh)
     esv_3w(ich) = esv
     esh_3w(ich) = esh
  end do
  
  call ems_adjust(theta,frequency,snow_depth,t_skin,esv_3w,esh_3w,em_vector,snow_type)
  
  return
end subroutine ALandEM_Snow


!---------------------------------------------------------------------!
subroutine snowem_default(theta,freq,snow_depth,t_soil,b,esv,esh)

!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Initialize discriminator using LandEM
!
! program history log:
!   2004-09-22  todling - replaced zsqrt by general interface sqrt
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle in radian
!      snow_depth       -  snow depth in mm
!      t_skin           - surface temperature
!
! output argument list:
!
!       esv              -  initial discriminator at vertical polarization
!       esh              -                        at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
!  use kinds, only: r_kind
!  use constants, only: zero, one
  implicit none
  
  real(r_kind) rhob,rhos,sand,clay
  Parameter(rhob = 1.18_r_kind, rhos = 2.65_r_kind, &
       sand = 0.8_r_kind, clay = 0.2_r_kind)
  real(r_kind) theta, freq, mv, t_soil, snow_depth,b
  real(r_kind) theta_i,theta_t, mu, r12_h, r12_v, r21_h, r21_v, r23_h, r23_v, &
       t21_v, t21_h, t12_v, t12_h, gv, gh, ssalb_h,ssalb_v,tau_h,     &
       tau_v, esh, esv,rad, sigma, va ,ep_real,ep_imag
  complex(r_kind) esoil, esnow, eair
  
  eair = dcmplx(one,-zero)
!     ep = dcmplx(3.2_r_kind,-0.0005_r_kind)
  sigma = one
  theta_i  = theta
  mv = 0.1_r_kind
  ep_real = 3.2_r_kind
  ep_imag = -0.0005_r_kind
  va = 0.4_r_kind + 0.0004_r_kind*snow_depth
  rad = one + 0.005_r_kind*snow_depth

  call snow_diel(freq, ep_real, ep_imag, rad, va, esnow)
!    call snow_diel(freq, ep, rad, va, esnow)
  call soil_diel(freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
  theta_t = asin(real(sin(theta_i)*sqrt(eair)/sqrt(esnow)))
  call reflectance(eair, esnow, theta_i, theta_t, r12_v, r12_h)
  call transmitance(eair, esnow, theta_i, theta_t, t12_v, t12_h)
  
  theta_t  = theta
  theta_i = asin(real(sin(theta_t)*sqrt(eair)/sqrt(esnow)))
  call reflectance(esnow, eair, theta_i,  theta_t, r21_v, r21_h)
  call transmitance(esnow, eair, theta_i, theta_t, t21_v, t21_h)
  
  mu  = cos(theta_i)
  theta_t = asin(real(sin(theta_i)*sqrt(esnow)/sqrt(esoil)))
  call reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)
  call rough_reflectance(freq, theta_i, sigma, r23_v, r23_h)

!    call snow_optic(freq, rad, snow_depth, va, ep, gv, gh, ssalb_v, ssalb_h, tau_v, tau_h)
  call snow_optic(freq,rad,snow_depth,va,ep_real, ep_imag,gv,gh,&
       ssalb_v,ssalb_h,tau_v,tau_h)
  
  call two_stream_solution(b,mu,gv,gh,ssalb_h, ssalb_v, tau_h, tau_v, r12_h, &
       r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh)
  return
end subroutine snowem_default


!---------------------------------------------------------------------!
subroutine ems_adjust(theta,frequency,depth,ts,esv_3w,esh_3w,em_vector,snow_type)


!$$$  subprogram documentation block
!
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery MODL
!
! program history log:
!   2004-10-28  treadon - remove nch from parameter declaration below (not used)
!
! input argument list:
!
!      frequency   -  frequency in GHz
!      theta       -  local zenith angle (not used here)
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
!              dem_coe[2,*]   : 89   GHz
!              dem_coe[3,*]   : 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,r_double,i_kind
!  use constants, only: one,deg2rad
  implicit none
  
  integer(i_kind),parameter:: nw_3=3
  integer(i_kind),parameter:: ncoe=6
  real(r_kind),parameter  :: earthrad = 6374._r_kind, satheight = 833.4_r_kind
  integer(i_kind)         :: snow_type,ich,j,k
  real(r_kind)    :: theta,frequency,depth,ts,esv_3w(*),esh_3w(*)
  real(r_kind)    :: discriminator(5),emmod(nw_3),dem(nw_3)
  real(r_kind)    :: emissivity,em_vector(2)
  real(r_double)  :: dem_coe(nw_3,0:ncoe-1),sinthetas,costhetas
  
  save  dem_coe
  
  data (dem_coe(1,k),k=0,ncoe-1)/ 2.306844e+000_r_double, -7.287718e-003_r_double, &
       -6.433248e-004_r_double,  1.664216e-005_r_double,  &
       4.766508e-007_r_double, -1.754184e+000_r_double/
  data (dem_coe(2,k),k=0,ncoe-1)/ 3.152527e+000_r_double, -1.823670e-002_r_double, &
       -9.535361e-004_r_double,  3.675516e-005_r_double,  &
       9.609477e-007_r_double, -1.113725e+000_r_double/
  data (dem_coe(3,k),k=0,ncoe-1)/ 3.492495e+000_r_double, -2.184545e-002_r_double,  &
       6.536696e-005_r_double,  4.464352e-005_r_double, &
       -6.305717e-008_r_double, -1.221087e+000_r_double/
  
  sinthetas = sin(theta*deg2rad)* earthrad/(earthrad + satheight)
  sinthetas = sinthetas*sinthetas
  costhetas = one - sinthetas
  do ich = 1, nw_3
     emmod(ich) = costhetas*esv_3w(ich) + sinthetas*esh_3w(ich)
  end do
  do ich=1,nw_3
     dem(ich) = dem_coe(ich,0) + dem_coe(ich,1)*ts + dem_coe(ich,2)*depth +   &
          dem_coe(ich,3)*ts*ts + dem_coe(ich,4)*depth*depth         +   &
          dem_coe(ich,5)*emmod(ich)
  end do
  emmod(1) = emmod(1) + dem(1)
  emmod(2) = emmod(2) + dem(2)
  emmod(3) = emmod(3) + dem(3)

! Match the format of the input variable

! Missing value at 23.8 GHz
  discriminator(1) = -999.9_r_kind; discriminator(2) = emmod(1)

! Missing value at 50.3 GHz
  discriminator(3) = -999.9_r_kind; discriminator(4) = emmod(2); discriminator(5) = emmod(3)

  call em_interpolate(frequency,discriminator,emissivity,snow_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  return
end subroutine ems_adjust
