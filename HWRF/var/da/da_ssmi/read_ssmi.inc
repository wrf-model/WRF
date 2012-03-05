subroutine filter(tb19v, tb19h, tb22v, tb37v,          &
                     tb37h, tb85v, tb85h, ipass, iprecip, xlat )
   implicit none

   real    ,  intent (in)  :: tb19v, tb19h, tb22v, tb37v, &
                              tb37h, tb85v, tb85h, xlat
   real                     :: tb(7)
   logical ,  intent (inout):: ipass
   integer ,  intent (inout):: iprecip

   integer                 :: ipass1, ipass2
   real                    :: sst, dsst
   real                    :: theta,tc,wv,u,alw19,alw37,alw85

   !  write (unit=stdout,fmt=*) 'tbdata',tb19v, tb19h, tb22v, tb37v,tb37h, tb85v, tb85h
   ! tc : cloud top temperature (C)

   theta = 53.1
   tc = 10.0
   sst = 23.0
   dsst = 1000.0

   ipass1 = 0
   ipass2 = 0

   tb(1) = tb19v 
   tb(2) = tb19h
   tb(3) = tb22v
   tb(4) = tb37v
   tb(5) = tb37h
   tb(6) = tb85v
   tb(7) = tb85h

   ! upper and lower boundary 19h (90,280), others (100,280)

   if (tb(1) .lt. 280 .and. tb(1) .gt. 100. .and. &
       tb(2) .lt. 280 .and. tb(2) .gt.  90. .and. &
       tb(3) .lt. 280 .and. tb(3) .gt. 100. .and. &
       tb(4) .lt. 280 .and. tb(4) .gt. 100. .and. &
       tb(5) .lt. 280 .and. tb(5) .gt. 100. .and. &
       tb(6) .lt. 280 .and. tb(6) .gt. 100. .and. &
       tb(7) .lt. 280 .and. tb(7) .gt. 100. .and. &

       !    horizontal always much less than vertical

       tb(1) .gt. tb(2) .and. &
       tb(4) .gt. tb(5) .and. &
       tb(6) .gt. tb(7) .and. &

       ! T19V always at least 4 K less than  T22V, except in heavy rain

       tb(3)-tb(1) .gt. 4.) then
      ipass1 = 1
   end if

   ! second check

   if (ipass1 .eq. 1) then

      call pettyalgos(tb,theta,tc,sst,dsst,          &
                          wv,u,alw19,alw37,alw85,iprecip, xlat)

         if (wv    .gt. -999. .and. u     .gt. -999. .and. &
             alw19 .gt. -999. .and. alw37 .gt. -999. .and. &
             alw85 .gt. -999. ) then
            ipass2 = 1
            ! write(unit=stdout,*)a1,a2,lat,long,theta,tb(1),tb(2),tb(3), &
            !    tb(4),tb(5),tb(6),tb(7)
         end if

      ! third check

      ! if (ipass2 .eq. 1) then
      !    if (wv    .ge. 5.    .and. wv .le. 70 .and.     &
      !        u     .ge. -5    .and. u  .le. 30 .and.     &
      !        alw19 .ge. -0.01 .and. alw19 .le. 0.5 .and. &
      !        alw37 .ge. -0.01 .and. alw37 .le. 0.5 .and. &
      !        alw85 .ge. -0.01 .and. alw85 .le. 0.5) then
      !       write(unit=stdout,*)a1,a2,lat,long,theta,tb(1),tb(2),tb(3), &
      !          tb(4),tb(5),tb(6),tb(7)
      !       ipass = .true.
      !    end if
      ! end if
      ! if (iprecip .eq. 1) then
      !    ipass = .true.
      !    write(unit=stdout,*) 'precipitation',lat,long,wv,u,alw19,alw37,alw85,iprecip
      ! else
      !    ipass = .true.
      ! end if
   end if

   ! write(unit=stdout,*) 'pass=',ipass1,ipass2,iprecip,ipass

end subroutine filter

   !***************************************************************
   !                    NOTICE
   ! 
   ! This package of subroutines has been developed by Grant W. Petty
   ! (Purdue University) for the retrieval of column water vapor, column
   ! cloud liquid water, and surface wind speed from SSM/I brightness
   ! temperatures.  The algorithm versions contained herein were developed
   ! in part using data sets provided by NASDA/EORC, Japan and are
   ! submitted to NASDA for evaluation as part of ADEOS-2/AMSR algorithm
   ! development activities.  Because of the need to meet a submission
   ! deadline, they have been subject only to limited testing, and it is
   ! possible that undetected bugs or problems remain in the code that
   ! follows.
   ! 
   ! Problems or questions should be directed to 
   !       Grant W. Petty
   !       Earth and Atmospheric Sciences Dept.
   !       Purdue University
   !       West Lafayette, in 47907-1397
   !       (765) 494-2544
   !       gpetty@rain.atms.purdue.edu
   ! 
   ! Version:   July 13, 1997
   ! Copyright 1997, Grant W. Petty 
   !
   ! Permission is granted to all scientific users to utilize
   ! and/or test this set of algorithms without restriction, provided only
   ! that
   !       (1) they are not used for commercial Purposes without the
   !          author's permission
   ! 
   !       (2) the author is given due credit for their development
   ! 
   !       (3) any modifications to the algorithms by 3d parties must be
   !          clearly identified as such and distinguished from the author's
   !          original code
   ! 
   !********************************************************************
   !      GENERAL COMMENTS ON USAGE
   ! 
   ! The algorithms that follow can act on single-pixel values of brightness
   ! temperature and derived variables, such as water vapor, wind speed,
   ! etc.  However, the way they are normally used at Purdue University is
   ! as follows:
   ! 
   !  1    Retrieve Water Vapor and Surface Wind Speed on a pixel-by-pixel basis
   ! 
   !  2    Apply mild spatial smoothing to derived wind speed and water vapor
   !       fields
   ! 
   !  3    Used filtered values of wind speed and water vapor to compute
   !       cloud-free polarization differences
   ! 
   !  4    Compute liquid water from unsmoothed, full-resolution brightness
   !       temperatures, normalized by smoothed cloud-free polarizations.
   ! 
   ! The smoothing step described above is optional and should not have a
   ! strong effect on the results outside of precipitation.  Normally the
   ! smoothing step is necessary only for interpolating water vapor and
   ! wind speed into areas of precipitation, where normalized polarizations
   ! at 19, 37 and 85 GHz are needed for precipitation retrievals.
   ! 
   ! 
   !********************************************************************
   ! This subroutine accepts SSM/I  brightness temperatures and returns
   ! all over-ocean variables using the algorithms of Petty (1997, unpublished)
   ! It, and the algorithms it calls, are generally intended for use only outside
   ! of precipitation, except for the water vapor algorithm, which is believed to
   !  tolerate significant precipitation without serious errors.
   !
   ! Inputs:   tb(7) = 19v,t19h,t22v,t37v,t37h,t85v,t85h
   !                --- SSM/I brightness temperatures (K) 
   !           theta -- sensor viewing angle
   !           tc  --- assumed cloud temperature in degrees Celsius
   !           Note: If TC not known, set to a reasonable 
   !                 average value, such as 8.0 degrees
   !
   !           sst   -- sea surface temperature (deg. C)
   !           dsst   -- uncertainty in the above SST (deg. C)
   !                     (if dsst > 2.8 C, then an wind speed algorithm is
   !                      used that doesn't require SST)
   !
   ! Output:   wv     ---- column water vapor (kg/m**2) from 19.35 GHz
   !           u      ---- surface wind speed (m/sec)
   !           alw19  ---- column cloud liquid water (kg/m**2) from 19.35 GHz
   !           alw37  ---- column cloud liquid water (kg/m**2) from 37 GHz
   !           alw85  ---- column cloud liquid water (kg/m**2) from 85.5 GHz
   !           iprecip --- set to 1 if precipitation flag invoked, else 0
   ! 
   ! 
   ! NOTE: It is not expected that liquid water values from different SSM/I
   ! frequencies will always closely agree, on account of significant
   ! differences in spatial resolution, sensitivity, and dynamic range.
   ! For best comparisons, average results to a common spatial resolution
   ! first.  19 GHz channels have the least sensitivity to thin clouds; 85
   ! GHz channels are most severely affected by precipitation and/or strong
   ! inhomogeneity in cloud fields.
   ! 
   ! Also, liquid water values in excess of 0.5 kg/m**2 are generally
   ! flagged as precipitating, and a value of MISSinG is returned.  There
   ! is no known method for separating cloud water from precipitation
   ! water.  Furthermore, any attempt to retrieve total liquid water path
   ! when precipitation is present will require a considerably more
   ! sophisticated algorithm than the ones provided here.
   !******************************************************************

subroutine pettyalgos(tb,theta,tc,sst,dsst, &
   wv,u,alw19,alw37,alw85,iprecip,xlat)

   implicit none

   real tb(7),theta,tc,wv,u,alw19,alw37,alw85,xlat, sst,dsst
   real, parameter :: BAD = -1000.0
   integer iprecip

   ! get water vapor
   !     write (unit=stdout,fmt=*) 'tb=',tb
   call pettyvapor(tb,wv)
   !     write (unit=stdout,fmt=*) 'wv=',wv

   ! get surface wind speed
   call pettywind(tb,theta,sst,dsst,u)

   ! get column cloud liquid water, using 3 different algorithms
   call pettylwp(tb,theta,1,tc,alw19,iprecip,xlat)
   call pettylwp(tb,theta,2,tc,alw37,iprecip,xlat)
   call pettylwp(tb,theta,3,tc,alw85,iprecip,xlat)
   if (alw19 .eq. BAD .or. alw19 .eq. BAD .or. alw85 .eq. BAD) iprecip=1
   !     if (iprecip .eq. 1) write (unit=stdout,fmt=*) 'iprecip B=',iprecip,'alw=',alw19,alw37,alw85

end subroutine pettyalgos


   !********************************************************************
   ! This subroutine accepts SSM/I brightness temperatures and returns
   ! cloud liquid water in kg/m**2.  
   ! 
   ! Inputs:   tb(7) = 19v,t19h,t22v,t37v,t37h,t85v,t85h
   !                --- SSM/I brightness temperatures (K) 
   !           theta -- sensor viewing angle
   !           ifreq -- frequency to use for computing cloud liquid water
   !                    (1 = 19.35 GHz  2 = 37 GHz  3 = 85.5 GHz)
   !           tc  --- assumed cloud temperature in degrees Celsius
   !           Note: If not known, set to a reasonable value, such as 8.0 degrees
   !
   ! Output:   alw  ---- column water vapor (mm)
   !           iprecip --- set to 1 if precipitation flag invoked, else 0
 
subroutine pettylwp(tb,theta,ifreq,tc,alw,iprecip,xlat)

   implicit none
   real tb(7),theta,alw,tc,xlat
   integer ifreq,iprecip
   real, parameter :: BAD = -1000.0
   real t19v,t19h,t22v,t37v,t37h,t85v,t85h,MISSinG,pclr
   real :: p,s85,v0,wv,u
   integer :: ich
 
   iprecip = 0
   MISSinG = -8888.
   pclr=0.
   if (ifreq.lt.1 .or. ifreq.gt.3) then
      pause "BAD VALUE OF ifREQ in PettyLWP"
      alw = BAD
      return
   end if

   ! initialize brightness temperatures

   t19v = tb(1)
   t19h = tb(2)
   t22v = tb(3)
   t37v = tb(4)
   t37h = tb(5)
   t85v = tb(6)
   t85h = tb(7)

   ! estimate the clear-sky polarization difference for the scene

   call clearpol(tb,theta,ifreq,pclr,iprecip)

   ! if value of Tc is invalid, then set to reasonable intermediate value
   if (tc .lt. -20.0 .or. tc .gt. 40.0) then
      tc = 8.0
   end if

   ! calculate normalized polarization
   if (ifreq .eq. 1) then
      p = (t19v-t19h)/pclr
   else if (ifreq .eq. 2) then
      p = (t37v-t37h)/pclr
   else if (ifreq .eq. 3) then
      p = (t85v-t85h)/pclr

      ! at 85.5 GHz, there could be errors due to ice, so check S85
      call PettyVapor(tb,wv)
      if (wv .eq. BAD) then
         alw = BAD
         return
      end if
      call PettyWind(tb,theta,-99.9,1000.0,u)
      if (u .eq. BAD) then
         u = 5.0
         alw = BAD
      end if
     ich = 6 
     call tb_clear(ich,u,wv,v0)
     s85 = p*v0 + (1.0-p)*t_kelvin - t85v 
     if (s85 .gt. 10.0 .or. (t85v-t85h) .lt. 5.0) p = MISSinG 
   end if

   ! convert normalized polarization to LWP
   if (p .ne. MISSinG .and. p .lt. 1.4 .and. p .gt. 0.1) then
      call P2LWP(ifreq,p,tc,theta,alw,xlat)
   else
      alw = BAD
   end if

end subroutine PettyLWP

   !*********************************************************
   ! This routine computes liquid water path from normalized polarization
   ! at 19.35, 37, or 85.5 GHz  (indicated by ifREQ)

subroutine p2lwp(ifreq,p,tc,theta,alw,xlat) 

   implicit none        
   integer ifreq
   real p,alw,xlat,tc,theta
   real alpha(3),threshhold
   data alpha/2.08,2.05,1.78/
   real, parameter :: BAD = -1000.0
   real :: ext, costheta
   integer :: iprecip

   ! check for extreme value due to precipitation
   if (p .lt. 0.01) then
      alw = BAD
      iprecip = 1
      return
   end if

   !  get liquid water mass extinction coefficient
   ext = alw_ext(ifreq,tc)

   ! compute colum cloud liquid water using Beer's Law
   costheta = cos(theta*pi/180.0)
   alw = (-costheta/(alpha(ifreq)*ext))*log(p)

   ! check for precipitation
   threshhold=0.5
   !     write (unit=stdout,fmt=*) 'xlat=',xlat
   if (xlat .gt. 20) threshhold=0.3
   if (xlat .gt. 30) threshhold=0.1
   !     if (xlat .gt. 40) threshhold=0.25
   !     if (alw .gt. 0.5) then
   if (alw .gt. threshhold) then
      alw = BAD
      iprecip = 1
   end if

end subroutine p2lwp

   !***********************************************************************
   ! This subroutine accepts SSM/I brightness temperatures from a scene and returns
   ! the predicted cloud-free polarization difference for that scene
   ! 
   ! Inputs:   tb(7) = 19v,t19h,t22v,t37v,t37h,t85v,t85h
   !                --- SSM/I brightness temperatures (K) 
   !           theta -- sensor viewing angle
   !           ifreq -- frequency to use for computing cloud liquid water
   !                    (1 = 19.35 GHz  2 = 37 GHz  3 = 85.5 GHz)
   !
   ! Output:   pclr  ---- cloud free polarization difference (K)
   !           iprecip --- set to 1 if precipitation flag invoked, else 0

subroutine ClearPol(tb,theta,ifreq,pclr,iprecip)

   implicit none
   real tb(7),theta,pclr
   integer ifreq,iprecip
   real, parameter :: BAD = -1000.0
   real :: wv,u
   integer :: ich

   ! check for valid ifREQ
   if (ifreq.lt.1 .or. ifreq.gt.3) then
      pause 'BAD VALUE OF ifREQ in ClearPol'
      pclr = BAD
      return
   end if

   iprecip = 0

   ! get water vapor estimate.  If this value is returned as 'bad', then
   ! either the brightness temperatures are not valid for over ocean or
   ! else there is unusually heavy precipitation

   call PettyVapor(tb,wv)
   if (wv .lt. 0.0) then
      pclr = BAD
      return
   end if

   ! get wind speed estimate.  If this value is returned as 'bad', assume
   ! that it is due to precipitation, and substitute a reasonable 'average'
   ! wind speed.

   call PettyWind(tb,theta,-99.9,1000.0,u)
   if (u .eq. BAD) then
      iprecip = 1
      u = 5.0
   end if

   ! now that we finally have estimates of water vapor and wind speed,
   ! compute the clear-sky polarization difference at the desired frequency

   ich = ifreq + 7
   call tb_clear(ich,u,wv,pclr)

         end subroutine ClearPol

   !**********************************************************
   ! The following function returns the microwave mass extinction coefficient
   ! of cloud water at 19.35, 37.0, or 85.5 GHz.
   !
   !     inputs:
   !           ifreq -- frequency to use for computing cloud liquid water
   !                    (1 = 19.35 GHz  2 = 37 GHz  3 = 85.5 GHz)
   !           tc  --- cloud temperature in degrees Celsius
   !
   !      returned value:  Mass extinction coefficient (m**2/kg)
   !

function alw_ext(ifreq,tc)

   implicit none

   integer, intent(in) :: ifreq
   real, intent(in) :: tc

   real :: alw_ext
   real :: tc2,tc3

   tc2 = tc*tc
   tc3 = tc2*tc

   if (ifreq .eq. 1) then
      alw_ext = exp(-2.55-2.98e-2*tc-6.81e-4*tc2+3.35e-6*tc3)
   else if (ifreq .eq. 2) then
      alw_ext = exp(-1.35-0.0234*tc-1.22e-4*tc2+5.48e-6*tc3) 
   else if (ifreq .eq. 3) then
      alw_ext = exp(-0.0713-8.00e-3*tc-1.62e-4*tc2+1.18e-6*tc3)
   end if

end function alw_ext

   !*****************************************************************************
   ! The following routine returns approximate clear-sky SSM/I brightness temperature 
   ! over the ocean as a function of column water vapor and surface wind speed.
   ! This implementation is a bivariate polynomial fit to model computed brightness 
   ! temperatures (see Petty (1992,1994)), where the model was in turn carefully calibrated
   ! empirically against 200,000 cloud-free SSM/I pixels.
   ! This routine assumes a viewing angle not too different from 53.4 degrees, and
   ! it assumes "typical" values for other ocean and atmospheric parameters.
   ! 
   !      inputs:
   !         ich = channel no. (1 = 19V, 2 = 19H, ..., 7 = 85V, 
   !                            8 = 19V-19H, 9 = 37V-37H, 10 = 85V-85H)
   !         u = estimated surface wind speed (m/sec)
   !         wv = estimated column water vapor (kg/m**2)
   !
   !      output:
   !         tb = brightness temperature

subroutine tb_clear(ich,u,wv,tb)
   implicit none
   integer, intent(in)  :: ich
   real,    intent(in)  :: u
   real,    intent(in)  :: wv
   real,    intent(out) :: tb

   real :: wvp,up,x,x2,x3,y,y2,y3,xy,x2y,xy2

   real a(10,10)
   data a/ &
    0.1984E+03, 0.1674E+02, 0.1998E+00,-0.3868E+00,-0.1818E+00, &
    0.7065E-01,-0.8148E+00, 0.6116E-01, 0.1400E-01, 0.0000E+00, &
    0.1334E+03, 0.2587E+02, 0.3819E+01,-0.7762E-01, 0.1564E+00, &
    0.4968E-01,-0.1232E+01,-0.6779E-01,-0.3083E-01, 0.0000E+00, &
    0.2301E+03, 0.2865E+02, 0.5589E+00,-0.4668E+01,-0.7274E-01, &
    0.1043E-01,-0.6731E+00, 0.8141E-02,-0.1081E-01, 0.0000E+00, &
    0.2126E+03, 0.1103E+02,-0.2488E+00, 0.8614E+00,-0.1298E+00, &
    0.4531E-01,-0.8315E+00, 0.5104E-01, 0.1785E-01, 0.0000E+00, &
    0.1500E+03, 0.1864E+02, 0.3950E+01, 0.2095E+01,-0.1743E+00, &
    0.1170E+00,-0.1345E+01,-0.8584E-01, 0.8143E-02, 0.0000E+00, &
    0.2589E+03, 0.1706E+02,-0.7729E+00,-0.2905E+01, 0.2821E+00, &
    0.3375E-02,-0.4366E+00,-0.1150E+00, 0.1881E-01, 0.0000E+00, &
    0.2265E+03, 0.3363E+02, 0.2050E+01,-0.5701E+01,-0.4185E+00, &
    0.8181E-01,-0.4101E+00,-0.1836E+00, 0.2666E-01, 0.0000E+00, &
    0.6512E+02,-0.9112E+01,-0.3430E+01,-0.4870E+00, 0.3890E-01, &
    0.1031E+00, 0.4424E+00, 0.7264E-01, 0.3514E-02, 0.0000E+00, &
    0.6242E+02,-0.7553E+01,-0.4117E+01,-0.1154E+01, 0.2788E+00, &
    0.1618E+00, 0.5605E+00, 0.6024E-01,-0.1659E-01, 0.0000E+00, &
    0.3255E+02,-0.1698E+02,-0.3049E+01, 0.2481E+01, 0.9171E+00, &
    0.1086E+00, 0.2261E+00, 0.4896E-01,-0.3257E-01, 0.0000E+00/

   wvp = (wv-30.0)/20.0
   up = (u-6.0)/3.0

   x = wvp
   x2 = x*x
   x3 = x2*x
   y = up
   y2 = y*y
   y3 = y2*y
   xy = x*y
   x2y = x2*y
   xy2 = x*y2

   tb = a(1,ich) + a(2,ich)*x + a(3,ich)*y + a(4,ich)*x2  &
        + a(5,ich)*xy + a(6,ich)*y2 + a(7,ich)*x3         &
        + a(8,ich)*x2y + a(9,ich)*xy2 + a(10,ich)*y3

end subroutine tb_clear

   !**************************************************************
   ! This subroutine accepts SSM/I brightness temperatures and returns
   ! column water vapor in mm (or kg per m**2).  The algorithm is described
   ! in the document by Petty (1997) accompanying this FORTRAN module.
   ! 
   ! Inputs:   tb(5) = 19v,t19h,t22v,t37v,t37h
   !                --- SSM/I brightness temperatures (K) 
   !  
   ! Output:   wv  --- column water vapor (mm)
   ! 
   ! 

subroutine PettyVapor(tb,wv)
   implicit none
   real tb(*),wv

   real t19v,t19h,t22v,t37v,t37h
   real tbold(5),wvold
   save tbold,wvold

   logical flag
   real, parameter :: BAD = -1000.0
   integer :: i
   real :: del, t1,t2,t3,wv2,wv3

   ! check for same TBs used in previous call in order to avoid
   ! redundant calculations

   flag = .false. 
   do i=1,5
      if (tbold(i) .ne. tb(i)) flag = .true.
      tbold(i) = tb(i)
   end do
   if (.not. flag) then
      wv = wvold
      return
   end if

   ! initialize brightness temperatures

   t19v = tb(1)
   t19h = tb(2)
   t22v = tb(3)
   t37v = tb(4)
   t37h = tb(5)

   ! check for land, ice, and heavy precipitation

   del = t22v-t19v
   if (del .lt. 4.0) then
      wv = BAD
      wvold = wv
      return
   end if

   ! special check for sea ice

   t1 = 260.0 - 2.0*del
   t2 = 270.0 - (120.0/15.0)*del
   t3 = 130 + (20./15.)*del

   if (t19h .lt. t1 .and. t19h .lt. t2 .and. t19h .gt. t3) then
      wv = BAD
      wvold = wv
      return
   end if

   ! check for abnormally warm brightness temperatures
   if (t22v .gt. 285.0 .or.  &
        t19h .gt. 285.0 .or. &
        t37h .gt. 285.0) then
      wv = BAD
      wvold = wv
      return
   end if

   ! compute water vapor from first-stage regression algorithm

   wv = 0.1670E+03  &
        + 0.4037E+01*log(295-t19h) &
        - 0.5322E+02*log(295-t22v) &
        + 0.1296E+02*log(295-t37h)

   ! apply polynomial correction to eliminate biases at high
   ! and low end of range

   wv2 = wv*wv
   wv3 = wv*wv2
   !      wv = -0.2079E+01 + 0.1366E+01*wv+  &
   !           -0.1504E-01*wv2+0.1639E-03*wv3
   wv = -2.079 + 1.366*wv-0.01504*wv2+0.0001639*wv3
   wvold = wv

end subroutine PettyVapor

   !**********************************************************************
   ! This subroutine accepts SSM/I brightness temperatures and returns
   ! surface wind speed in  m/sec.  The algorithm is described
   ! in the document accompanying this FORTRAN module.
   ! 
   ! Inputs:   t19v,t19h,t22v,t37v,t37h  
   !                --- SSM/I brightness temperatures (K) 
   !           theta -- sensor viewing angle
   !           sst   -- sea surface temperature (deg. C)
   !           dsst   -- uncertainty in the above SST (deg. C)
   !                     (if dsst > 2.8 C, then an algorithm is
   !                      used that doesn't require SST)
   !
   ! Output:   u  ---- column water vapor (mm)
   ! 
   ! 

subroutine PettyWind(tb,theta,sst,dsst,u)
   implicit none
   real tb(5),theta,sst,dsst,u

   logical flag
   real t19v,t19h,t22v,t37v,t37h
   real, parameter :: BAD = -1000.0

   real wv, alw37,errwv,p37clr,errlw,erru,p37,wv2,wv3
   integer iaold,i,ia
   real tbold(5),uold
   save tbold,uold,iaold

   ! choose wind algorithm to use, depending on how well SST is
   ! known

   if (abs(dsst).gt.2.8.or.sst.lt.-4.0.or.sst.gt.35.0) then
      ia = 2
   else
      ia = 1
   end if

   ! check for same TBs used in previous call in order to avoid
   ! redundant calculations

   flag = .false. 

   if (ia .ne. iaold)  flag = .true.
   iaold = ia

   do i=1,5
      if (tbold(i) .ne. tb(i)) flag = .true.
      tbold(i) = tb(i)
   end do
   if (.not. flag) then
      u = uold
      return
   end if

   ! initialize brightness temperatures

   t19v = tb(1)
   t19h = tb(2)
   t22v = tb(3)
   t37v = tb(4)
   t37h = tb(5)

   ! get estimate of column water vapor for use in 
   ! cross talk corrections

   call PettyVapor(tb,wv)

   ! check for exceptionally high or bad water vapor values
   if (wv .gt. 68.0 .or. wv .lt. 0.0) then
      u = BAD
      uold = u
      return
   end if


   call ualg(ia,theta,sst,tb,u)

   ! calculate 37 GHz liquid water using simple algorithm
   p37clr = 77.68 -.1782*wv -1.1546*u +  &
        0.001838*wv*u -.003160*wv*wv + 0.01127*u*u
   p37 = (t37v-t37h)/p37clr
   alw37 = -(1.0/0.8614)*log(p37)

   ! check for cloud water exceeding threshold

   if (alw37 .gt. 0.5) then
      u = BAD
      uold = u
      return
   end if

   ! select appropriate corrections
   if (ia .eq. 1) then

      ! water vapor cross-talk correction
      wv2 = wv*wv
      wv3 = wv*wv2
      errwv = -0.7173 + 0.1160*wv -0.4238E-02*wv2 + 0.4372E-04*wv3
      u = u - errwv

      ! cloud liquid water cross-talk correction
      if (alw37 .lt.0.08) then
         errlw =  -6.3889*alw37 + 0.36111
      else
         errlw = 0.83333*alw37 - 0.21667
      end if
      u = u - errlw

      ! check for unphysical values
      if (u .lt. -2.0) then
         u = BAD
         uold = u
         return
      end if

      ! apply correction for non-linearity
      if (u .lt. 2.5) then
         erru  =  -1.267 + 0.7142*u -0.8395E-01*u*u
      else if (u .gt. 10.0 ) then
         erru  =  3.243 -0.3478*u + 0.3679E-02*u*u
      else
         erru = 0.0
      end if
      u = u - erru

   else if (ia .eq. 2) then

      ! water vapor cross-talk correction
      wv2 = wv*wv
      wv3 = wv*wv2
      errwv = 0.2945 + 0.01270*wv -0.001654*wv2+ 0.2596E-04*wv3
      u = u - errwv

      ! cloud liquid water cross-talk correction
      if (alw37 .lt.0.0) then
         errlw =  4.0*alw37 + 0.4
      else if (alw37 .ge. 0.0 .and. alw37 .lt. 0.08) then
         errlw = -8.125*alw37 + 0.4
      else
         errlw = -0.25
      end if
      u = u - errlw

      ! check for unphysical values
      if (u .lt. -2.0) then
         u = BAD
         uold = u
         return
      end if

      ! apply correction for non-linearity
      if (u .lt. 2.5) then
         erru = -1.219+ 0.9327*u -0.1852*u*u
      else if (u .gt. 10.0 ) then
         erru = 3.360-0.3918*u + 0.8121E-02*u*u
      else
         erru = 0.0
      end if
      u = u - erru
   end if
   uold = u

end subroutine PettyWind

subroutine ualg(ia,theta,sst,tb,u)

   !***********************************************************
   !  This subroutine implements the actual first stage wind speed algorithm.
   !  When IA=1, an algorithm is employed that makes use of SST.
   !  When IA=2, the SST information is not used.

   implicit none

   integer ia,i
   real theta,sst,tb(5),u

   real coeff1(8)
   real coeff2(8)
   data coeff1/ 0.1862E+03,0.9951E-01,0.0,  &
        -0.1829E-01, -0.1438E+01,0.7029,    &
        0.2329E+01,0.1687/
   data coeff2/0.1719E+03,0.2827, 0.0, -0.2549E-01, &
        -0.1473E+01, 0.6425, 0.2045E+01, 0.0/

   ! if theta is out of range, then substitute default value
   if (theta .lt. 50.0 .or. theta .gt. 55.0) theta = 53.1

   if (ia .eq. 1) then
      u = coeff1(1)
      do i=1,5
         u = u + coeff1(i+1)*tb(i)
      end do
      u = u + coeff1(7)*(theta-53.0)
      u = u + coeff1(8)*sst
   else if (ia .eq. 2) then
      u = coeff2(1)
      do i=1,5
         u = u + coeff2(i+1)*tb(i)
      end do
      u = u + coeff2(7)*(theta-53.0)
   else
      write (0,*) "Invalid algorithm ID"
   end if

end subroutine ualg



