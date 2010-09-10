!create this subroutine for RTTOV9_3
Subroutine da_rttov_setupindex (&
     & mchan,           & ! in
     & nprofiles,       & ! in
     & nfrequencies,    & ! in
     & nchannels,       & ! in
     & nbtout,          & ! in
     & coef,            & ! in
     & surfem,          & ! in
     & lprofiles,       & ! out
     & channels,        & ! out
     & polarisations,   & ! out
     & emissivity)        ! out

  !
  ! Description:
  !
  ! Setup profile, channel and polarisation indices and emissivity
  ! for RTTOV.
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
  !  1.1    11/11/2004   Corrected idexing of emissivity array
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
  Use rttov_const, Only : &
     &  sensor_id_mw,     &
     &  npolar_compute

  Use parkind1, Only : jpim     ,jprb
#endif
 
 Implicit None
  !
  ! Subroutine arguments
  Integer(Kind=jpim),  Intent(in)    :: nprofiles       ! Number of profiles
  Integer(Kind=jpim),  Intent(in)    :: mchan(nprofiles)! nfrequencies/nprofiles
  Integer(Kind=jpim),  Intent(in)    :: nchannels       ! Total Number of radiances computed
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies    ! Total Number of frequencies
                                                        !  (= channels used * profiles)
  Integer(Kind=jpim),  Intent(in)    :: nbtout          ! Total Number of BTs returned
  Integer(Kind=jpim),  Intent(out)   :: channels(nfrequencies)    ! Channel indices
  Integer(Kind=jpim),  Intent(out)   :: polarisations(nchannels,3)  ! Channel indices
  Integer(Kind=jpim),  Intent(out)   :: lprofiles(nfrequencies)  !Profiles indices
  Real(Kind=jprb),     Intent(in)    :: surfem(nchannels)       !Inputsurface emissivity for first profile
  Real(Kind=jprb),     Intent(out)   :: emissivity(nchannels)   ! Surface emissivity array for RTTOV

  Type( rttov_coef ),  Intent (in)   :: coef            ! coefficients

  ! Local scalars/arrays
  Integer(Kind=jpim) :: j ,jch ,n         ! loop index
  Integer(Kind=jpim) :: nch , pol_id, ich2
  Integer(Kind=jpim) :: ichannels, ibtout        ! counters
  !- End of header --------------------------------------------------------
  !
  nch = 0
  ichannels=0
  ibtout=0
  polarisations(:,:) = 0
     !
  DO  J = 1,nprofiles
     DO  jch = 1,mchan(j)
        nch = nch +1
        lprofiles ( nch ) = j
        ! because we have read the coefficient file with a selection of channels
        ! we have now to introduce the indices of the channels for rttov runs
        channels  ( nch ) = jch
        polarisations(nch,1) = ichannels+1
        If( coef % id_sensor /= sensor_id_mw) then
           ichannels=ichannels+1
           emissivity(nch) = surfem(jch)
           polarisations(nch,2) = nch
           polarisations(nch,3) = 1
        End If
        If( coef % id_sensor == sensor_id_mw) then
           ichannels=ichannels+1
           emissivity(nch) = surfem(jch)
           polarisations(nch,2) = nch
           polarisations(nch,3) = 1
        End If
     End Do
  End Do




End Subroutine da_rttov_setupindex
