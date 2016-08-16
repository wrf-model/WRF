!--------------------------------------------------------------------------
!Input :   ifREQ = (1,2,3, or 4 ) for (19.35, 22.235, 37, or 85.5 GHz)
!          THETA = incidence angle (deg.)
!                                                 (approximate  range)
!          P0    = surface pressure (mb)                    (940 -1030)
!          WV    = precipitable water (kg/m**2)               (0 - 70)
!          HWV   = water vapor density scale height (km)    (0.5 - 3.0)
!          TA, GAMMA = "effective" surface air temperature 
!                        and lapse rate (K ; km)
!               T(z) = Ta - gamma*z              (263 - 303 ; 4.0 - 6.5)
!
!          SST = sea surface temperature (K)     (273 - 303)
!          U  = wind speed (m/sec)               (0 - 30)
!          ALW = column liquid water (kg/m**2)   (0 - 0.5)
!              note: ALW > 0.5 usually indicates precipitation
!
!          ZCLD = effective cloud height (km)    
!              (Ta - GAMMA*ZCLD)  should be warmer than 243 K in order
!               to be reasonably realistic.
!
!Output:   TBV, TBH = vertically and horizontally polarized brightness
!               temperature (K) at frequency given by ifREQ  
!
!----------------------- tbmod_ssmi.f ---  cut here ----------------------
! VERSION:  July 1997
! This has been carefully calibrated against cloud-free pixels over the 
! globe for the month of Nov 87.   It's not perfect, but don't change it 
! unless you come up with 
! a better calibration data set.  One remaining task it to add splines for
! foam between 5 and 12 m/sec
!
!  Reference:
!
!       Petty, 1990: "On the Response of the Special Sensor Microwave/Imager
!         to the Marine Environment - Implications for Atmospheric Parameter
!         Retrievals"  Ph.D. Dissertation, University of Washington, 291 pp.
!         (See below for J.Atmos.Ocean.Tech. articles in press or pending)
!
!   coded in a quick-and-dirty fashion, and without any warranty, by:
!   Grant W. Petty
!   Earth and Atmospheric Sciences Dept.
!   Purdue University
!   West Lafayette, in  47907
!   USA
!
!   Tel. No. (317) 494-2544
!   Internet address : gpetty@rain.atms.purdue.edu
!
!----------------------------------------------------------------------
      subroutine tb(ifreq,theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld, &
         tbv,tbh)
      implicit none
      integer ifreq

      real   , intent(in   ) :: theta,p0,ta,gamma,sst,wv,hwv,u,alw,zcld
      real   , intent(  out) :: tbv,tbh
      real freq(4),ebiasv(4),ebiash(4),cf1(4),cf2(4),cg(4)

      real :: f,costhet,gx2,sigma,remv,remh,tbup,tbdn,tauatm
      real :: effangv,effangh,tbdnv,dum,foam,emissv,emissh
      real :: refv,refh,semv,semh,scatv,scath,tbdnh

      data freq/19.35,22.235,37.0,85.5/

! empirical bias corrections for surface emissivity

!
      data ebiasv/0.0095, 0.005,-0.014, -0.0010/
      data ebiash/0.004,   0.0,-0.023, 0.043/


      data cf1/0.0015,0.004,0.0027,0.005/
      data cg/4.50e-3, 5.200e-3, 5.5e-3, 7.2e-3 /

      data cf2/0.0023,0.000,0.0002,-0.006/

! 'foam' emissivity
      real :: fem
      data fem /1.0/
!
      f = freq(ifreq)
      costhet = cos(theta*0.017453)

! effective surface slope variance

      gx2 = cg(ifreq)*u

! get upwelling atmospheric brightness temperature

      call tbatmos(ifreq,theta,p0,wv,hwv,ta,gamma,alw,zcld, &
                                   tbup,tbdn,tauatm)

! convert transmittance to optical depth

      sigma = -alog(tauatm)*costhet

! get rough surface emissivity

      call roughem(ifreq,gx2,sst,theta,remv,remh)

! get effective zenith angles for scattered radiation at surface

      call effang(ifreq,theta,gx2,sigma,effangv,effangh)

! get effective sky brightness temperatures for scattered radiation

      call tbatmos(ifreq,effangv,p0,wv,hwv,ta,gamma,alw,zcld, &
                   dum,tbdnv,dum)

      call tbatmos(ifreq,effangh,p0,wv,hwv,ta,gamma,alw,zcld, &
                   dum,tbdnh,dum)
! compute 'foam' coverage
      foam = cf1(ifreq)*u

      if (u .gt. 5.0) then
        foam = foam + cf2(ifreq)*(u-5.0)
      end if

! compute surface emissivities and reflectivity
      emissv = foam*fem + (1.0 - foam)*(remv + ebiasv(ifreq))
      emissh = foam*fem + (1.0 - foam)*(remh + ebiash(ifreq))
      refv = 1.0 - emissv
      refh = 1.0 - emissh
! compute surface emission term
      semv = sst*emissv
      semh = sst*emissh
! compute surface scattering term
      scatv = refv*tbdnv
      scath = refh*tbdnh
! combine to get space-observed brightness temperature
      tbv = tbup + tauatm*(semv + scatv)
      tbh = tbup + tauatm*(semh + scath)


      end subroutine tb

