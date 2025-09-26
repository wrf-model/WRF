#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!
! Common block and statement functions for saturation vapor pressure
! look-up procedure, J. J. Hack, February 1990
!
! Ported to WRF by William.Gustafson@pnl.gov, Nov. 2009
! Updated to version from CESM1_0_1, Nov. 2010
! Updated to version CESM1.0.3 (CAM5.1.01)- Balwinder.Singh@pnnl.gov
!
! $Id$
!
module wv_saturation
  use shr_kind_mod, only: r8 => shr_kind_r8
#ifndef WRF_PORT
  use abortutils,   only: endrun
  use cam_logfile,  only: iulog
#else
  use module_cam_support, only: endrun, iulog
  use module_wrf_error
#endif

  implicit none
  private
  save
!
! Public interfaces
!
  public gestbl   ! Initialization subroutine
  public estblf   ! saturation pressure table lookup
  public aqsat    ! Returns saturation vapor pressure
#ifndef WRF_PORT
  public aqsatd   ! Same as aqsat, but also returns a temperature derivitive
#endif
  public vqsatd   ! Vector version of aqsatd
  public fqsatd   ! Function version of vqsatd
  public qsat_water  ! saturation mixing ration with respect to liquid water
#ifndef WRF_PORT
  public vqsat_water ! vector version of qsat_water
  public qsat_ice    ! saturation mixing ration with respect to ice
  public vqsat_ice   ! vector version of qsat_ice
#endif
  public vqsatd_water
#ifndef WRF_PORT
  public aqsat_water
  public vqsatd2_water         ! Variant of vqsatd_water to print out dqsdT
  public vqsatd2_water_single  ! Single value version of vqsatd2_water
#endif
  public vqsatd2
  public vqsatd2_single
  public polysvp
!
! Data used by cldwat
!
  public hlatv, tmin, hlatf, rgasv, pcf, cp, epsqs, ttrice
!
! Data
!
  integer plenest  ! length of saturation vapor pressure table
  parameter (plenest=250)
!
! Table of saturation vapor pressure values es from tmin degrees
! to tmax+1 degrees k in one degree increments.  ttrice defines the
! transition region where es is a combination of ice & water values
!
  real(r8) estbl(plenest)      ! table values of saturation vapor pressure
  real(r8) tmin       ! min temperature (K) for table
  real(r8) tmax       ! max temperature (K) for table
  real(r8) ttrice     ! transition range from es over H2O to es over ice
  real(r8) pcf(6)     ! polynomial coeffs -> es transition water to ice
  real(r8) epsqs      ! Ratio of h2o to dry air molecular weights 
  real(r8) rgasv      ! Gas constant for water vapor
  real(r8) hlatf      ! Latent heat of vaporization
  real(r8) hlatv      ! Latent heat of fusion
  real(r8) cp         ! specific heat of dry air
  real(r8) tmelt      ! Melting point of water (K)
  logical icephs  ! false => saturation vapor press over water only

contains

   real(r8) function estblf( td )
!
! Saturation vapor pressure table lookup
!
   real(r8), intent(in) :: td         ! Temperature for saturation lookup
!
   real(r8) :: e       ! intermediate variable for es look-up
   real(r8) :: ai
   integer  :: i
!
   e = max(min(td,tmax),tmin)   ! partial pressure
   i = int(e-tmin)+1
   ai = aint(e-tmin)
   estblf = (tmin+ai-e+1._r8)* &
            estbl(i)-(tmin+ai-e)* &
            estbl(i+1)
   end function estblf

subroutine gestbl(tmn     ,tmx     ,trice   ,ip      ,epsil   , &
                  latvap  ,latice  ,rh2o    ,cpair   ,tmeltx   )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Builds saturation vapor pressure table for later lookup procedure.
! 
! Method: 
! Uses Goff & Gratch (1946) relationships to generate the table
! according to a set of free parameters defined below.  Auxiliary
! routines are also included for making rapid estimates (well with 1%)
! of both es and d(es)/dt for the particular table configuration.
! 
! Author: J. Hack
! 
!-----------------------------------------------------------------------
#ifndef WRF_PORT
   use spmd_utils, only: masterproc
#else
   use module_cam_support, only: masterproc
   use module_cam_gffgch
#endif
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: tmn           ! Minimum temperature entry in es lookup table
   real(r8), intent(in) :: tmx           ! Maximum temperature entry in es lookup table
   real(r8), intent(in) :: epsil         ! Ratio of h2o to dry air molecular weights
   real(r8), intent(in) :: trice         ! Transition range from es over range to es over ice
   real(r8), intent(in) :: latvap        ! Latent heat of vaporization
   real(r8), intent(in) :: latice        ! Latent heat of fusion
   real(r8), intent(in) :: rh2o          ! Gas constant for water vapor
   real(r8), intent(in) :: cpair         ! Specific heat of dry air
   real(r8), intent(in) :: tmeltx        ! Melting point of water (K)
!
!---------------------------Local variables-----------------------------
!
   real(r8) t             ! Temperature
   integer n          ! Increment counter
   integer lentbl     ! Calculated length of lookup table
   integer itype      ! Ice phase: 0 -> no ice phase
!            1 -> ice phase, no transition
!           -x -> ice phase, x degree transition
   logical ip         ! Ice phase logical flag
!
!-----------------------------------------------------------------------
!
! Set es table parameters
!
   tmin   = tmn       ! Minimum temperature entry in table
   tmax   = tmx       ! Maximum temperature entry in table
   ttrice = trice     ! Trans. range from es over h2o to es over ice
   icephs = ip        ! Ice phase (true or false)
!
! Set physical constants required for es calculation
!
   epsqs  = epsil
   hlatv  = latvap
   hlatf  = latice
   rgasv  = rh2o
   cp     = cpair
   tmelt  = tmeltx
!
   lentbl = INT(tmax-tmin+2.000001_r8)
   if (lentbl .gt. plenest) then
      write(iulog,9000) tmax, tmin, plenest
#ifdef WRF_PORT
      call wrf_message(iulog)
#endif
      call endrun ('GESTBL')    ! Abnormal termination
   end if
!
! Begin building es table.
! Check whether ice phase requested.
! If so, set appropriate transition range for temperature
!
   if (icephs) then
      if (ttrice /= 0.0_r8) then
         itype = -ttrice
      else
         itype = 1
      end if
   else
      itype = 0
   end if
!
   t = tmin - 1.0_r8
   do n=1,lentbl
      t = t + 1.0_r8
      call gffgch(t,estbl(n),itype)
   end do
!
   do n=lentbl+1,plenest
      estbl(n) = -99999.0_r8
   end do
!
! Table complete -- Set coefficients for polynomial approximation of
! difference between saturation vapor press over water and saturation
! pressure over ice for -ttrice < t < 0 (degrees C). NOTE: polynomial
! is valid in the range -40 < t < 0 (degrees C).
!
!                  --- Degree 5 approximation ---
!
   pcf(1) =  5.04469588506e-01_r8
   pcf(2) = -5.47288442819e+00_r8
   pcf(3) = -3.67471858735e-01_r8
   pcf(4) = -8.95963532403e-03_r8
   pcf(5) = -7.78053686625e-05_r8
!
!                  --- Degree 6 approximation ---
!
!-----pcf(1) =  7.63285250063e-02
!-----pcf(2) = -5.86048427932e+00
!-----pcf(3) = -4.38660831780e-01
!-----pcf(4) = -1.37898276415e-02
!-----pcf(5) = -2.14444472424e-04
!-----pcf(6) = -1.36639103771e-06
!
   if (masterproc) then
      write(iulog,*)' *** SATURATION VAPOR PRESSURE TABLE COMPLETED ***'
#ifdef WRF_PORT
      call wrf_message(iulog)
#endif
   end if

   return
!
9000 format('GESTBL: FATAL ERROR *********************************',/, &
            ' TMAX AND TMIN REQUIRE A LARGER DIMENSION ON THE LENGTH', &
            ' OF THE SATURATION VAPOR PRESSURE TABLE ESTBL(PLENEST)',/, &
            ' TMAX, TMIN, AND PLENEST => ', 2f7.2, i3)
!
end subroutine gestbl

subroutine aqsat(t       ,p       ,es      ,qs        ,ii      , &
                 ilen    ,kk      ,kstart  ,kend      )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g),for input arrays of temperature and pressure (dimensioned ii,kk)
! This routine is useful for evaluating only a selected region in the
! vertical.
! 
! Method: 
! <Describe the algorithm(s) used in the routine.> 
! <Also include any applicable external references.> 
! 
! Author: J. Hack
! 
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ii             ! I dimension of arrays t, p, es, qs
   integer, intent(in) :: kk             ! K dimension of arrays t, p, es, qs
   integer, intent(in) :: ilen           ! Length of vectors in I direction which
   integer, intent(in) :: kstart         ! Starting location in K direction
   integer, intent(in) :: kend           ! Ending location in K direction
   real(r8), intent(in) :: t(ii,kk)          ! Temperature
   real(r8), intent(in) :: p(ii,kk)          ! Pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(ii,kk)         ! Saturation vapor pressure
   real(r8), intent(out) :: qs(ii,kk)         ! Saturation specific humidity
!
!---------------------------Local workspace-----------------------------
!
   real(r8) omeps             ! 1 - 0.622
   integer i, k           ! Indices
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))
!
! Saturation specific humidity
!
         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
!
! The following check is to avoid the generation of negative values
! that can occur in the upper stratosphere and mesosphere
!
         qs(i,k) = min(1.0_r8,qs(i,k))
!
         if (qs(i,k) < 0.0_r8) then
            qs(i,k) = 1.0_r8
            es(i,k) = p(i,k)
         end if
      end do
   end do
!
   return
end subroutine aqsat

!++xl
#ifndef WRF_PORT
subroutine aqsat_water(t       ,p       ,es      ,qs        ,ii      , &
                       ilen    ,kk      ,kstart  ,kend      )
!-----------------------------------------------------------------------
!
! Purpose:
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g),for input arrays of temperature and pressure (dimensioned ii,kk)
! This routine is useful for evaluating only a selected region in the
! vertical.
!
! Method:
! <Describe the algorithm(s) used in the routine.>
! <Also include any applicable external references.>
!
! Author: J. Hack
!
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ii             ! I dimension of arrays t, p, es, qs
   integer, intent(in) :: kk             ! K dimension of arrays t, p, es, qs
   integer, intent(in) :: ilen           ! Length of vectors in I direction which
   integer, intent(in) :: kstart         ! Starting location in K direction
   integer, intent(in) :: kend           ! Ending location in K direction
   real(r8), intent(in) :: t(ii,kk)          ! Temperature
   real(r8), intent(in) :: p(ii,kk)          ! Pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(ii,kk)         ! Saturation vapor pressure
   real(r8), intent(out) :: qs(ii,kk)         ! Saturation specific humidity
!
!---------------------------Local workspace-----------------------------
!
   real(r8) omeps             ! 1 - 0.622
   integer i, k           ! Indices
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do k=kstart,kend
      do i=1,ilen
!        es(i,k) = estblf(t(i,k))
         es(i,k) = polysvp(t(i,k),0)
!
! Saturation specific humidity
!
         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
!
! The following check is to avoid the generation of negative values
! that can occur in the upper stratosphere and mesosphere
!
         qs(i,k) = min(1.0_r8,qs(i,k))
!
         if (qs(i,k) < 0.0_r8) then
            qs(i,k) = 1.0_r8
            es(i,k) = p(i,k)
         end if
      end do
   end do
!
   return
end subroutine aqsat_water
!--xl


subroutine aqsatd(t       ,p       ,es      ,qs      ,gam     , &
                  ii      ,ilen    ,kk      ,kstart  ,kend    )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g).   
! 
! Method: 
! Differs from aqsat by also calculating and returning
! gamma (l/cp)*(d(qsat)/dT)
! Input arrays temperature and pressure (dimensioned ii,kk).
! 
! Author: J. Hack
! 
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: ii            ! I dimension of arrays t, p, es, qs
   integer, intent(in) :: ilen          ! Vector length in I direction
   integer, intent(in) :: kk            ! K dimension of arrays t, p, es, qs
   integer, intent(in) :: kstart        ! Starting location in K direction
   integer, intent(in) :: kend          ! Ending location in K direction

   real(r8), intent(in) :: t(ii,kk)         ! Temperature
   real(r8), intent(in) :: p(ii,kk)         ! Pressure

!
! Output arguments
!
   real(r8), intent(out) :: es(ii,kk)        ! Saturation vapor pressure
   real(r8), intent(out) :: qs(ii,kk)        ! Saturation specific humidity
   real(r8), intent(out) :: gam(ii,kk)       ! (l/cp)*(d(qs)/dt)
!
!---------------------------Local workspace-----------------------------
!
   logical lflg          ! True if in temperature transition region
   integer i             ! i index for vector calculations
   integer k             ! k index
   real(r8) omeps            ! 1. - 0.622
   real(r8) trinv            ! Reciprocal of ttrice (transition range)
   real(r8) tc               ! Temperature (in degrees C)
   real(r8) weight           ! Weight for es transition from water to ice
   real(r8) hltalt           ! Appropriately modified hlat for T derivatives
   real(r8) hlatsb           ! hlat weighted in transition region
   real(r8) hlatvp           ! hlat modified for t changes above freezing
   real(r8) tterm            ! Account for d(es)/dT in transition region
   real(r8) desdt            ! d(es)/dT
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do k=kstart,kend
      do i=1,ilen
         es(i,k) = estblf(t(i,k))
!
! Saturation specific humidity
!
         qs(i,k) = epsqs*es(i,k)/(p(i,k) - omeps*es(i,k))
!
! The following check is to avoid the generation of negative qs
! values which can occur in the upper stratosphere and mesosphere
!
         qs(i,k) = min(1.0_r8,qs(i,k))
!
         if (qs(i,k) < 0.0_r8) then
            qs(i,k) = 1.0_r8
            es(i,k) = p(i,k)
         end if
      end do
   end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice
!
   do k=kstart,kend
      do i=1,ilen
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where constant slope is given by -2369 j/(kg c) =cpv - cw
!
         tc     = t(i,k) - tmelt
         lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
         weight = min(-tc*trinv,1.0_r8)
         hlatsb = hlatv + weight*hlatf
         hlatvp = hlatv - 2369.0_r8*tc
         if (t(i,k) < tmelt) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         if (lflg) then
            tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
         else
            tterm = 0.0_r8
         end if
         desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k)) + tterm*trinv
         gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/(cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
         if (qs(i,k) == 1.0_r8) gam(i,k) = 0.0_r8
      end do
   end do
!
   go to 20
!
! No icephs or water to ice transition
!
10 do k=kstart,kend
      do i=1,ilen
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
         hlatvp = hlatv - 2369.0_r8*(t(i,k)-tmelt)
         if (icephs) then
            hlatsb = hlatv + hlatf
         else
            hlatsb = hlatv
         end if
         if (t(i,k) < tmelt) then
            hltalt = hlatsb
         else
            hltalt = hlatvp
         end if
         desdt    = hltalt*es(i,k)/(rgasv*t(i,k)*t(i,k))
         gam(i,k) = hltalt*qs(i,k)*p(i,k)*desdt/(cp*es(i,k)*(p(i,k) - omeps*es(i,k)))
         if (qs(i,k) == 1.0_r8) gam(i,k) = 0.0_r8
      end do
   end do
!
20 return
end subroutine aqsatd
#endif
subroutine vqsatd(t       ,p       ,es      ,qs      ,gam      , &
                  len     )
!----------------------------------------------------------------------- 
! 
! Purpose: 
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g), and calculate and return gamma (l/cp)*(d(qsat)/dT).  The same
! function as qsatd, but operates on vectors of temperature and pressure
! 
! Method: 
! 
! Author: J. Hack
! 
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: len       ! vector length
   real(r8), intent(in) :: t(len)       ! temperature
   real(r8), intent(in) :: p(len)       ! pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(len)   ! saturation vapor pressure
   real(r8), intent(out) :: qs(len)   ! saturation specific humidity
   real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
!
!--------------------------Local Variables------------------------------
!
   logical lflg   ! true if in temperature transition region
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) trinv     ! reciprocal of ttrice (transition range)
   real(r8) tc        ! temperature (in degrees C)
   real(r8) weight    ! weight for es transition from water to ice
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) tterm     ! account for d(es)/dT in transition region
   real(r8) desdt     ! d(es)/dT
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = estblf(t(i))
!
! Saturation specific humidity
!
      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs(i) = min(1.0_r8,qs(i))
!
      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice
   do i=1,len
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
      tc     = t(i) - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do
   return
!
! No icephs or water to ice transition
!
10 do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do
!
   return
!
end subroutine vqsatd

!++xl
subroutine vqsatd_water(t       ,p       ,es      ,qs      ,gam      , &
                        len     )

!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: len       ! vector length
   real(r8), intent(in) :: t(len)       ! temperature
   real(r8), intent(in) :: p(len)       ! pressure

!
! Output arguments
!
   real(r8), intent(out) :: es(len)   ! saturation vapor pressure
   real(r8), intent(out) :: qs(len)   ! saturation specific humidity
   real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)

!
!--------------------------Local Variables------------------------------
!
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) desdt     ! d(es)/dT
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = polysvp(t(i),0)
!
! Saturation specific humidity
!
      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs(i) = min(1.0_r8,qs(i))
!
      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do
!
! No icephs or water to ice transition
!
   do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      hlatsb = hlatv
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
   end do
!
   return
!
end subroutine vqsatd_water

      function polysvp (T,type)
!  Compute saturation vapor pressure by using
! function from Goff and Gatch (1946)

!  Polysvp returned in units of pa.
!  T is input in units of K.
!  type refers to saturation with respect to liquid (0) or ice (1)

      real(r8) dum

      real(r8) T,polysvp

      integer type

! ice

      if (type.eq.1) then

! Goff Gatch equation (good down to -100 C)

         polysvp = 10._r8**(-9.09718_r8*(273.16_r8/t-1._r8)-3.56654_r8* &
          log10(273.16_r8/t)+0.876793_r8*(1._r8-t/273.16_r8)+ &
          log10(6.1071_r8))*100._r8

      end if

! Goff Gatch equation, uncertain below -70 C

      if (type.eq.0) then
         polysvp = 10._r8**(-7.90298_r8*(373.16_r8/t-1._r8)+ &
             5.02808_r8*log10(373.16_r8/t)- &
             1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/373.16_r8))-1._r8)+ &
             8.1328e-3_r8*(10._r8**(-3.49149_r8*(373.16_r8/t-1._r8))-1._r8)+ &
             log10(1013.246_r8))*100._r8
         end if


      end function polysvp
!--xl

integer function fqsatd(t    ,p    ,es    ,qs   ,gam   , len     )
  !----------------------------------------------------------------------- 
  ! Purpose: 
  ! This is merely a function interface vqsatd.
  !------------------------------Arguments--------------------------------
  ! Input arguments
  integer, intent(in) :: len       ! vector length
  real(r8), intent(in) :: t(len)       ! temperature
  real(r8), intent(in) :: p(len)       ! pressure
  ! Output arguments
  real(r8), intent(out) :: es(len)   ! saturation vapor pressure
  real(r8), intent(out) :: qs(len)   ! saturation specific humidity
  real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
  ! Call vqsatd
  call vqsatd(t       ,p       ,es      ,qs      ,gam  , len     )
  fqsatd = 1
  return
end function fqsatd

real(r8) function qsat_water(t,p)
  !  saturation mixing ratio w/respect to liquid water
  real(r8) t ! temperature
  real(r8) p ! pressure (Pa)
  real(r8) es ! saturation vapor pressure (Pa)
  real(r8) ps, ts, e1, e2, f1, f2, f3, f4, f5, f
  !  real(r8) t0inv ! 1/273.
  !  data t0inv/0.003663/
  !  save t0inv
  !  es = 611.*exp(hlatv/rgasv*(t0inv-1./t))

  ps = 1013.246_r8
  ts = 373.16_r8
  e1 = 11.344_r8*(1.0_r8 - t/ts)
  e2 = -3.49149_r8*(ts/t - 1.0_r8)
  f1 = -7.90298_r8*(ts/t - 1.0_r8)
  f2 = 5.02808_r8*log10(ts/t)
  f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
  f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
  f5 = log10(ps)
  f  = f1 + f2 + f3 + f4 + f5
  es = (10.0_r8**f)*100.0_r8

  qsat_water = epsqs*es/(p-(1.-epsqs)*es) ! saturation w/respect to liquid only
  if(qsat_water < 0.) qsat_water = 1.

end function qsat_water
#ifndef WRF_PORT
subroutine vqsat_water(t,p,qsat_water,len)
  !  saturation mixing ratio w/respect to liquid water
  integer, intent(in)  :: len
  real(r8) t(len) ! temperature
  real(r8) p(len) ! pressure (Pa)
  real(r8) qsat_water(len)
  real(r8) es ! saturation vapor pressure (Pa)
  real(r8), parameter :: t0inv = 1._r8/273._r8
  real(r8) coef
  integer :: i

  coef = hlatv/rgasv
  do i=1,len
     es = 611._r8*exp(coef*(t0inv-1./t(i)))
     qsat_water(i) = epsqs*es/(p(i)-(1.-epsqs)*es) ! saturation w/respect to liquid only
     if(qsat_water(i) < 0.) qsat_water(i) = 1.
  enddo

  return

end subroutine vqsat_water

real(r8) function qsat_ice(t,p)
  !  saturation mixing ratio w/respect to ice
  real(r8) t ! temperature
  real(r8) p ! pressure (Pa)
  real(r8) es ! saturation vapor pressure (Pa)
  real(r8), parameter :: t0inv = 1._r8/273._r8
  es = 611.*exp((hlatv+hlatf)/rgasv*(t0inv-1./t))
  qsat_ice = epsqs*es/(p-(1.-epsqs)*es) ! saturation w/respect to liquid only
  if(qsat_ice < 0.) qsat_ice = 1.

end function qsat_ice

subroutine vqsat_ice(t,p,qsat_ice,len)
  !  saturation mixing ratio w/respect to liquid water
  integer,intent(in) :: len
  real(r8) t(len) ! temperature
  real(r8) p(len) ! pressure (Pa)
  real(r8) qsat_ice(len)
  real(r8) es ! saturation vapor pressure (Pa)
  real(r8), parameter :: t0inv = 1._r8/273._r8
  real(r8) coef
  integer :: i

  coef = (hlatv+hlatf)/rgasv
  do i=1,len
     es = 611.*exp(coef*(t0inv-1./t(i)))
     qsat_ice(i) = epsqs*es/(p(i)-(1.-epsqs)*es) ! saturation w/respect to liquid only
     if(qsat_ice(i) < 0.) qsat_ice(i) = 1.
  enddo

  return

end subroutine vqsat_ice

! Sungsu
! Below two subroutines (vqsatd2_water,vqsatd2_water_single) are by Sungsu
! Replace 'gam -> dqsdt'
! Sungsu

subroutine vqsatd2_water(t       ,p       ,es      ,qs      ,dqsdt      , &
                        len     )

!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: len       ! vector length
   real(r8), intent(in) :: t(len)       ! temperature
   real(r8), intent(in) :: p(len)       ! pressure

!
! Output arguments
!
   real(r8), intent(out) :: es(len)   ! saturation vapor pressure
   real(r8), intent(out) :: qs(len)   ! saturation specific humidity
 ! real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
 ! Sungsu
   real(r8), intent(out) :: dqsdt(len)  ! (d(qs)/dt)
 ! End by Sungsu

!
!--------------------------Local Variables------------------------------
!
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) desdt     ! d(es)/dT

 ! Sungsu
   real(r8) gam(len)  ! (l/cp)*(d(qs)/dt)
 ! End by Sungsu

!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = polysvp(t(i),0)
!
! Saturation specific humidity
!
      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs(i) = min(1.0_r8,qs(i))
!
      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do
!
! No icephs or water to ice transition
!
   do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      hlatsb = hlatv
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
    ! Sungsu
      dqsdt(i) = (cp/hltalt)*gam(i)
    ! End by Sungsu
   end do
!
   return
!
end subroutine vqsatd2_water

subroutine vqsatd2_water_single(t       ,p       ,es      ,qs      ,dqsdt)

!------------------------------Arguments--------------------------------
!
! Input arguments
!

   real(r8), intent(in) :: t       ! temperature
   real(r8), intent(in) :: p       ! pressure

!
! Output arguments
!
   real(r8), intent(out) :: es   ! saturation vapor pressure
   real(r8), intent(out) :: qs   ! saturation specific humidity
 ! real(r8), intent(out) :: gam  ! (l/cp)*(d(qs)/dt)
 ! Sungsu
   real(r8), intent(out) :: dqsdt  ! (d(qs)/dt)
 ! End by Sungsu

!
!--------------------------Local Variables------------------------------
!
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) desdt     ! d(es)/dT

 ! Sungsu
   real(r8) gam  ! (l/cp)*(d(qs)/dt)
 ! End by Sungsu

!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
!  do i=1,len
      es = polysvp(t,0)
!
! Saturation specific humidity
!
      qs = epsqs*es/(p - omeps*es)
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs = min(1.0_r8,qs)
!
      if (qs < 0.0_r8) then
         qs = 1.0_r8
         es = p
      end if
!  end do
!
! No icephs or water to ice transition
!
!  do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t-tmelt)
      hlatsb = hlatv
      if (t < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es/(rgasv*t*t)
      gam = hltalt*qs*p*desdt/(cp*es*(p - omeps*es))
      if (qs == 1.0_r8) gam = 0.0_r8
    ! Sungsu
      dqsdt = (cp/hltalt)*gam
    ! End by Sungsu
!  end do
!
   return
!
end subroutine vqsatd2_water_single

#endif
subroutine vqsatd2(t       ,p       ,es      ,qs      ,dqsdt      , &
                   len     )
!----------------------------------------------------------------------- 
! Sungsu : This is directly copied from 'vqsatd' but 'dqsdt' is output
!          instead of gam for use in Sungsu's equilibrium stratiform
!          macrophysics scheme.
! 
! Purpose: 
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g), and calculate and return gamma (l/cp)*(d(qsat)/dT).  The same
! function as qsatd, but operates on vectors of temperature and pressure
! 
! Method: 
! 
! Author: J. Hack
! 
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   integer, intent(in) :: len       ! vector length
   real(r8), intent(in) :: t(len)       ! temperature
   real(r8), intent(in) :: p(len)       ! pressure
!
! Output arguments
!
   real(r8), intent(out) :: es(len)   ! saturation vapor pressure
   real(r8), intent(out) :: qs(len)   ! saturation specific humidity
 ! real(r8), intent(out) :: gam(len)  ! (l/cp)*(d(qs)/dt)
 ! Sungsu
   real(r8), intent(out) :: dqsdt(len)  ! (d(qs)/dt)
 ! End by Sungsu 

!
!--------------------------Local Variables------------------------------
!
   logical lflg   ! true if in temperature transition region
!
   integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) trinv     ! reciprocal of ttrice (transition range)
   real(r8) tc        ! temperature (in degrees C)
   real(r8) weight    ! weight for es transition from water to ice
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) tterm     ! account for d(es)/dT in transition region
   real(r8) desdt     ! d(es)/dT

 ! Sungsu
   real(r8) gam(len)  ! (l/cp)*(d(qs)/dt)
 ! End by Sungsu
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs
   do i=1,len
      es(i) = estblf(t(i))
!
! Saturation specific humidity
!
      qs(i) = epsqs*es(i)/(p(i) - omeps*es(i))
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs(i) = min(1.0_r8,qs(i))
!
      if (qs(i) < 0.0_r8) then
         qs(i) = 1.0_r8
         es(i) = p(i)
      end if
   end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice
   do i=1,len
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
      tc     = t(i) - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i)) + tterm*trinv
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
    ! Sungsu
      dqsdt(i) = (cp/hltalt)*gam(i)
    ! End by Sungsu
   end do
   return
!
! No icephs or water to ice transition
!
10 do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t(i)-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t(i) < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es(i)/(rgasv*t(i)*t(i))
      gam(i) = hltalt*qs(i)*p(i)*desdt/(cp*es(i)*(p(i) - omeps*es(i)))
      if (qs(i) == 1.0_r8) gam(i) = 0.0_r8
    ! Sungsu
      dqsdt(i) = (cp/hltalt)*gam(i)
    ! End by Sungsu
   end do
!
   return
!
end subroutine vqsatd2


! Below routine is by Sungsu

subroutine vqsatd2_single(t       ,p       ,es      ,qs      ,dqsdt)
!----------------------------------------------------------------------- 
! Sungsu : This is directly copied from 'vqsatd' but 'dqsdt' is output
!          instead of gam for use in Sungsu's equilibrium stratiform
!          macrophysics scheme.
! 
! Purpose: 
! Utility procedure to look up and return saturation vapor pressure from
! precomputed table, calculate and return saturation specific humidity
! (g/g), and calculate and return gamma (l/cp)*(d(qsat)/dT).  The same
! function as qsatd, but operates on vectors of temperature and pressure
! 
! Method: 
! 
! Author: J. Hack
! 
!------------------------------Arguments--------------------------------
!
! Input arguments
!
   real(r8), intent(in) :: t       ! temperature
   real(r8), intent(in) :: p       ! pressure
!
! Output arguments
!
   real(r8), intent(out) :: es     ! saturation vapor pressure
   real(r8), intent(out) :: qs     ! saturation specific humidity
 ! real(r8), intent(out) :: gam    ! (l/cp)*(d(qs)/dt)
 ! Sungsu
   real(r8), intent(out) :: dqsdt  ! (d(qs)/dt)
 ! End by Sungsu 

!
!--------------------------Local Variables------------------------------
!
   logical lflg   ! true if in temperature transition region
!
!  integer i      ! index for vector calculations
!
   real(r8) omeps     ! 1. - 0.622
   real(r8) trinv     ! reciprocal of ttrice (transition range)
   real(r8) tc        ! temperature (in degrees C)
   real(r8) weight    ! weight for es transition from water to ice
   real(r8) hltalt    ! appropriately modified hlat for T derivatives
!
   real(r8) hlatsb    ! hlat weighted in transition region
   real(r8) hlatvp    ! hlat modified for t changes above freezing
   real(r8) tterm     ! account for d(es)/dT in transition region
   real(r8) desdt     ! d(es)/dT

 ! Sungsu
   real(r8) gam       ! (l/cp)*(d(qs)/dt)
 ! End by Sungsu
!
!-----------------------------------------------------------------------
!
   omeps = 1.0_r8 - epsqs

!  do i=1,len

      es = estblf(t)
!
! Saturation specific humidity
!
      qs = epsqs*es/(p - omeps*es)
!
! The following check is to avoid the generation of negative
! values that can occur in the upper stratosphere and mesosphere
!
      qs = min(1.0_r8,qs)
!
      if (qs < 0.0_r8) then
         qs = 1.0_r8
         es = p
      end if

!  end do
!
! "generalized" analytic expression for t derivative of es
! accurate to within 1 percent for 173.16 < t < 373.16
!
   trinv = 0.0_r8
   if ((.not. icephs) .or. (ttrice.eq.0.0_r8)) go to 10
   trinv = 1.0_r8/ttrice

!  do i=1,len
!
! Weighting of hlat accounts for transition from water to ice
! polynomial expression approximates difference between es over
! water and es over ice from 0 to -ttrice (C) (min of ttrice is
! -40): required for accurate estimate of es derivative in transition
! range from ice to water also accounting for change of hlatv with t
! above freezing where const slope is given by -2369 j/(kg c) = cpv - cw
!
      tc     = t - tmelt
      lflg   = (tc >= -ttrice .and. tc < 0.0_r8)
      weight = min(-tc*trinv,1.0_r8)
      hlatsb = hlatv + weight*hlatf
      hlatvp = hlatv - 2369.0_r8*tc
      if (t < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      if (lflg) then
         tterm = pcf(1) + tc*(pcf(2) + tc*(pcf(3) + tc*(pcf(4) + tc*pcf(5))))
      else
         tterm = 0.0_r8
      end if
      desdt  = hltalt*es/(rgasv*t*t) + tterm*trinv
      gam = hltalt*qs*p*desdt/(cp*es*(p - omeps*es))
      if (qs == 1.0_r8) gam = 0.0_r8
    ! Sungsu
      dqsdt = (cp/hltalt)*gam
    ! End by Sungsu
!  end do
   return
!
! No icephs or water to ice transition
!

10 continue

!10 do i=1,len
!
! Account for change of hlatv with t above freezing where
! constant slope is given by -2369 j/(kg c) = cpv - cw
!
      hlatvp = hlatv - 2369.0_r8*(t-tmelt)
      if (icephs) then
         hlatsb = hlatv + hlatf
      else
         hlatsb = hlatv
      end if
      if (t < tmelt) then
         hltalt = hlatsb
      else
         hltalt = hlatvp
      end if
      desdt  = hltalt*es/(rgasv*t*t)
      gam = hltalt*qs*p*desdt/(cp*es*(p - omeps*es))
      if (qs == 1.0_r8) gam = 0.0_r8
    ! Sungsu
      dqsdt = (cp/hltalt)*gam
    ! End by Sungsu

!  end do
!
   return
!
end subroutine vqsatd2_single


end module wv_saturation 
