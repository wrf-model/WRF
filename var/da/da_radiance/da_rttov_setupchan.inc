!create this subroutine for RTTOV9_3
Subroutine da_rttov_setupchan (&
     & nprofiles,       & ! in
     & nchan,           & ! in
     & coef,            & ! in
     & nfrequencies,    & ! out
     & nchannels,       & ! out
     & nbtout)            ! out
  !
  ! Description:
  !
  ! Setup default number of frequencies, channels , output BTs
  ! for the coeff file. These are then used by rttov_setupindex
  ! to set up channel and polarisation indices.
  !
  !
  ! Copyright:
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2004, EUMETSAT, All Rights Reserved.
  !
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version    Date       Comment
  !  1.0    31/03/2004   Original code (R Saunders)
  !
  ! Code Description:
  !   FORTRAN 90, following AAPP standards
  !
  ! Declarations
  !
  ! Global variables:
  ! Modules used:
  !
  ! Imported Type Definitions:

!RTTOV9_3
#ifdef RTTOV

  Use rttov_types, Only : &
     &  rttov_coef
  Use rttov_const, Only :   &
     &  sensor_id_mw,     &
     &  npolar_return,    &
     &  npolar_compute

  Use parkind1, Only : jpim
#endif

  Implicit None
  !
  ! Subroutine arguments
  Integer(Kind=jpim),  Intent(in)    :: nprofiles       ! Number of profiles
  Integer(Kind=jpim),  Intent(in)    :: nchan(nprofiles)! Number of channels requested
  Type( rttov_coef ),  Intent (in)   :: coef            ! coefficients
  Integer(Kind=jpim),  Intent(out)   :: nchannels       ! Number of radiances computed
  Integer(Kind=jpim),  Intent(out)   :: nfrequencies    ! Number of frequencies
                                                        !  (= channels used * profiles)
  Integer(Kind=jpim),  Intent(out)   :: nbtout          ! Number of BTs returned
  ! Local scalars/arrays
  Integer(Kind=jpim) :: j ,jch          ! loop index
  Integer(Kind=jpim) :: nch , pol_id
  Integer(Kind=jpim) :: ichannels, ibtout        ! counters
  !- End of header --------------------------------------------------------
  !
  nfrequencies = 0
  Do j = 1 , nprofiles
    nfrequencies = nfrequencies + nchan (j)
  End Do
  ! Find out size of channel arrays summing all polarisation states required.
  nch = 0
  ichannels=0
  ibtout=0
  DO j = 1 , nprofiles
     Do jch = 1 , nchan(j)
        nch = nch +1
        If( coef % id_sensor /= sensor_id_mw) then
           ichannels=ichannels+1
           ibtout=ibtout+1
        End If
        If( coef % id_sensor == sensor_id_mw) then
           pol_id = coef % fastem_polar(jch) + 1
           ichannels=ichannels+npolar_compute(pol_id)
           ibtout=ibtout+npolar_return(pol_id)
        End If
     End Do
  End Do
  nchannels = ichannels
  nbtout = ibtout
  




End Subroutine da_rttov_setupchan
