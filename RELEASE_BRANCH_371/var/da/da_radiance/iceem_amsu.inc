subroutine  iceem_amsu(theta,frequency,depth,ts,tba,tbb,esv,esh)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis emissivity model over ice for  AMSU-A/B
!
!   prgmmr: Banghua Yan      org: nesdis              date: 2004-03-01
!           Fuzhong Weng
!
! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
!       ice for AMSU-A/B
!
!    reference:
!    Yan, B., F. Weng and K.Okamoto,2004:
!       "A microwave snow emissivity model, submitted to TGRS
!
!   version: beta (sea ice type is to be determined)
!
! program history log:
!   2004-01-01  yan,b   - implement the algorithm for the ice emissivity
!   2004-03-01  yan,b   - modify the code for SSI
!   2004-07-23  okamoto - modify the code for GSI
!
! input argument list:
!      theta            -  local zenith angle in radian
!      frequency        -  frequency in GHz
!      ts               -  surface temperature (K)    (GDAS)
!      depth            -  scatter medium depth (mm)  (not used here) (GDAS) !
!      tba[1] ~ tba[4]  -  brightness temperature at four AMSU-A window channels
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89 GHz
!      tbb[1] ~ tbb[2]  -  brightness temperature at two AMSU-B window channels:
!                              tbb[1] : 89 GHz
!                              tbb[2] : 150 GHz
!                          When tba[ ] or tbb[ ] = -999.9, it means a missing value (no available data)
!
! output argument list:
!   em_vector        -  esv, esh
!       esv       : emissivity at vertical polarization
!       esh       : emissivity at horizontal polarization
!       sea ice_type (to be determined)
!
! remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

!  use kinds, only: r_kind,i_kind
!  use constants, only: zero, one
  implicit none
  
  integer(i_kind)      :: nch,nwcha,nwchb,nwch,nalg
  Parameter(nwcha = 4, nwchb = 2, nwch = 5,nalg = 7)
  real(r_kind)    :: theta,frequency,depth,ts
  real(r_kind)    :: em_vector(2),esv,esh
  real(r_kind)    :: tb(nwch),tba(nwcha),tbb(nwchb)
  logical :: INDATA(nalg),AMSUAB,AMSUA,AMSUB,ABTs,ATs,BTs,MODL
  integer(i_kind) :: seaice_type,input_type,i,ich,np,k
  
  Equivalence(INDATA(1), ABTs)
  Equivalence(INDATA(2), ATs)
  Equivalence(INDATA(3), AMSUAB)
  Equivalence(INDATA(4), AMSUA)
  Equivalence(INDATA(5), BTs)
  Equivalence(INDATA(6), AMSUB)
  Equivalence(INDATA(7), MODL)

!  Initialization

  em_vector(1) = 0.85_r_kind
  em_vector(2) = 0.82_r_kind
  seaice_type  = -999
  input_type = -999
  do k = 1, nalg
     INDATA(k) = .TRUE.
  end do
  
! Read AMSU & Ts data and set available option
! Get five AMSU-A/B window measurements
  tb(1) = tba(1); tb(2) = tba(2);  tb(3) = tba(3)
  tb(4) = tba(4); tb(5) = tbb(2)

! Check available data
  if((ts <= 100.0_r_kind) .or. (ts >= 320.0_r_kind) ) then
     ABTs = .false.;   ATs = .false.;   BTs = .false.;  MODL = .false.
  end if
  do i=1,nwcha
     if((tba(i) <= 100.0_r_kind) .or. (tba(i) >= 320.0_r_kind) ) then
        ABTs = .false.;  ATs = .false.;   AMSUAB = .false.;  AMSUA = .false.
        exit
     end if
  end do
  do i=1,nwchb
     if((tbb(i) <= 100.0_r_kind) .or. (tbb(i) >= 320.0_r_kind) ) then
        ABTs = .false.;  AMSUAB = .false.;  BTs  = .false.;  AMSUB  = .false.
        exit
     end if
  end do
  if((depth <  zero) .or. (depth >= 3000.0_r_kind)) MODL = .false.
  if((frequency >= 80.0_r_kind) .and. (BTs)) then
     ATs = .false.;   AMSUAB = .false.
  end if
  
! Check input type and call a specific Option/subroutine
  DO np = 1, nalg
     if (INDATA(np)) then
        input_type = np
        exit
     end if
  ENDDO
  
  GET_option: SELECT CASE (input_type)
  CASE (1)
!        call siem_abts(theta,frequency,tb,ts,seaice_type,em_vector)
  CASE (2)
     call siem_ats(theta,frequency,tba,ts,seaice_type,em_vector)
  CASE (3)
!        call siem_ab(theta,frequency,tb,seaice_type,em_vector)
  CASE (4)
!        call siem_amsua(theta,frequency,tba,seaice_type,em_vector)
  CASE(5)
     call siem_bts(theta,frequency,tbb,ts,seaice_type,em_vector)
  CASE(6)
!        call siem_amsub(theta,frequency,tbb,seaice_type,em_vector)
  CASE(7)
!        call siem_default(theta,frequency,depth,ts,seaice_type,em_vector)
  END SELECT GET_option
  
  if (em_vector(1) > one)         em_vector(1) = one
  if (em_vector(2) > one)         em_vector(2) = one
  if (em_vector(1) < 0.6_r_kind) em_vector(1) = 0.6_r_kind
  if (em_vector(2) < 0.6_r_kind) em_vector(2) = 0.6_r_kind
  esv = em_vector(1)
  esh = em_vector(2)
  
end subroutine iceem_amsu
