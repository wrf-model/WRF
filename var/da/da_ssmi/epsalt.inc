
      subroutine epsalt(f,t,ssw,epsr,epsi)
!     returns the complex dielectric constant of sea water, using the
!     model of Klein and Swift (1977)
!
!     Input   f = frequency (GHz)
!             t = temperature (C)
!             ssw = salinity (permil) (if ssw < 0, ssw = 32.54)
!     Output  epsr,epsi  = real and imaginary parts of dielectric constant

      real, intent(in    ) :: f,t
      real, intent(inout ) :: ssw
      real, intent(out   ) :: epsr,epsi
      real :: ssw2,ssw3,t2,t3,es,a,tau,b,delt
      real :: beta,sig,delt2,om

      complex cdum1,cdum2,cdum3
!
      if (ssw .lt. 0.0) ssw = 32.54
      ssw2 = ssw*ssw
      ssw3 = ssw2*ssw
      t2 = t*t
      t3 = t2*t
      es = 87.134 - 1.949e-1*t - 1.276e-2*t2 + 2.491e-4*t3
      a = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
         4.232e-7*ssw3
      es = es*a
!
      tau = 1.768e-11 - 6.086e-13*t + 1.104e-14*t2 - 8.111e-17*t3
      b = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
         1.105e-8*ssw3
      tau = tau*b
!
      sig = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
         1.28205e-7*ssw3)
      delt = 25.0 - t
      delt2 = delt*delt
      beta = 2.033e-2 + 1.266e-4*delt + 2.464e-6*delt2 - &
         ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
      sig = sig*exp(-beta*delt)
!
      om = 2.0e9*pi*f
      cdum1 = cmplx(0.0,om*tau)
      cdum2 = cmplx(0.0,sig/(om*8.854e-12))

      cdum3 = 4.9 + (es-4.9)/(1.0 + cdum1) - cdum2
      epsr = real(cdum3)
      epsi = -aimag(cdum3)

      end subroutine epsalt

