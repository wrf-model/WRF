!**********************************************************************************  
! This computer software was prepared by Battelle Memorial Institute, hereinafter
! the Contractor, under Contract No. DE-AC05-76RL0 1830 with the Department of 
! Energy (DOE). NEITHER THE GOVERNMENT NOR THE CONTRACTOR MAKES ANY WARRANTY,
! EXPRESS OR IMPLIED, OR ASSUMES ANY LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! MOSAIC module: see module_mosaic_driver.F for references and terms of use
!**********************************************************************************  
      module module_mosaic_csuesat

!-----------------------------------------------------------------------

      implicit none

      integer, parameter :: nebins=149, nebinsi=110

      real, save :: estbar(nebins+1), esitbar(nebinsi+1)

      real, save :: tmin = -1.0
      real, save :: tmini = -1.0



      contains



!-----------------------------------------------------------------------
!   following funcs from pegasus file csuesat01.f (timestamp = 09-apr-2002)
!-----------------------------------------------------------------------
!   file csuesat01.f - from stratcld.F,v on 8-oct-97
!   routines and common blocks renamed to allow running either
!	standalone gchm or coupled gchm-ccm2
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
      real function esat_gchm( t )

!     saturation vapor pressure (dynes/cm2) with respect to water

      real t
      real av
      integer it

      if (tmin .lt. 0.0) then
        call init_csuesat
      endif

      it=max0(1,min0(ifix(t-tmin),nebins))
      av=amax1(amin1(t-tmin-float(it),1.),0.)
      esat_gchm=estbar(it)*(1.-av)+estbar(it+1)*av
      return
      end function esat_gchm     


!-----------------------------------------------------------------------
      real function esati_gchm( t )

!     saturation vapor pressure (dynes/cm2) with respect to ice

      real t
      real av
      integer it

      if (tmin .lt. 0.0) then
        call init_csuesat
      endif

      it=max0(1,min0(ifix(t-tmini),nebinsi))
      av=amax1(amin1(t-tmini-float(it),1.),0.)
      esati_gchm=esitbar(it)*(1.-av)+esitbar(it+1)*av
      return
      end function esati_gchm     


!-----------------------------------------------------------------------
      subroutine init_csuesat

!     calculate table of saturation vapor pressure (dynes/cm2) with respect
!     to water(estbar) and ice (esitbar)

      integer jd, k
      real a0, a2, a3, a3dtf, a4, a5, a6, arg, ax
      real t, tf, tinver, z1, z2

      a0=5.75185606e10
      ax=-20.947031
      a2=-3.56654
      a3=-2.018890949
      tf=273.16
      a3dtf=a3/tf
      tmini=163.
      t=tmini

      do 3 k=1,nebinsi+1
      t=t+1.
      tinver=1./t
      arg=ax*tf*tinver+a2*alog(tf*tinver)+a3*t/tf
      esitbar(k)=a0*exp(arg)*1.e3
3     continue

      a0=7.95357242e+10
      ax=-18.1972839
      a2=5.02808
      a3=-70242.1852
      a4=-26.1205253
      a5=58.0691913
      a6=-8.03945282
      tf=373.16
      tmin=163.
      t=tmin

      do 4 jd=1,nebins+1
      t=t+1.
      z1=exp(a4*t/tf)
      z2=exp(a6*tf/t)
      arg=ax*tf/t+a2*alog(tf/t)+a3*z1+a5*z2
4     estbar(jd)=a0*exp(arg)*1.e3

      return
      end subroutine init_csuesat


!-----------------------------------------------------------------------
      end module module_mosaic_csuesat
