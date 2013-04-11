subroutine da_epsalt_tl(f,t,ssw,epsr,epsi,TGL_t, TGL_epsr, TGL_epsi)

   !-----------------------------------------------------------------------
   ! Purpose: returns the complex dielectric constant of sea water, using the
   !  model of Klein and Swift (1977)
   !
   ! Input   f = frequency (GHz)
   !         t = temperature (C)
   !         ssw = salinity (permil) (if ssw < 0, ssw = 32.54)
   ! Output  epsr,epsi  = real and imaginary parts of dielectric constant
   ! Input  : TGL_t      (ssw is treated as a constant now)
   ! Output : TGL_epsr, TGL_epsi, epsr, epsi
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)    :: f, t, TGL_t
   real, intent(inout) :: ssw
   real, intent(out)   :: TGL_epsr, TGL_epsi, epsr, epsi

   complex :: cdum1,cdum2,cdum3
   complex :: TGL_cdum1,TGL_cdum2,TGL_cdum3
   real    :: ssw2,ssw3,t2,t3,es,a,esnew,tau,b,sig,taunew
   real    :: delt,delt2,beta,signew,om
   real    :: TGL_t2,TGL_t3,TGL_es,TGL_a,TGL_esnew,TGL_tau,TGL_b,TGL_taunew
   real    :: TGL_delt,TGL_delt2,TGL_beta,TGL_signew

   if (trace_use) call da_trace_entry("da_epsalt_tl")

   if (ssw .lt. 0.0) ssw = 32.54
   ssw2       = ssw*ssw
   ssw3       = ssw2*ssw
       t2     = t*t
   TGL_t2     = 2.0*t*TGL_t
       t3     = t2*t
   TGL_t3     = TGL_t2*t + t2*TGL_t
       es     = 87.134 - 1.949e-1*t     - 1.276e-2*t2     + 2.491e-4*t3
   TGL_es     =        - 1.949e-1*TGL_t - 1.276e-2*TGL_t2 + 2.491e-4*TGL_t3

       a      = 1.0 + 1.613e-5*ssw*t - 3.656e-3*ssw + 3.21e-5*ssw2 - &
                4.232e-7*ssw3
   TGL_a      = 1.613e-5*ssw*TGL_t 
       esnew  = es*a
   TGL_esnew  = TGL_es*a + es*TGL_a

       tau    = 1.768e-11 - 6.086e-13*t     + 1.104e-14*t2     - 8.111e-17*t3
   TGL_tau    =           - 6.086e-13*TGL_t + 1.104e-14*TGL_t2 - 8.111e-17*TGL_t3
       b      = 1.0 + 2.282e-5*ssw*t - 7.638e-4*ssw - 7.760e-6*ssw2 + &
                1.105e-8*ssw3
   TGL_b      = 2.282e-5*ssw*TGL_t 
       taunew = tau*b
   TGL_taunew = TGL_tau*b + tau*TGL_b

   sig        = ssw*(0.182521 - 1.46192e-3*ssw + 2.09324e-5*ssw2 - &
                1.28205e-7*ssw3)
       delt   = 25.0 - t
   TGL_delt   =      - TGL_t
       delt2  = delt*delt
   TGL_delt2  = 2.0*delt*TGL_delt
       beta   =   2.033e-2 + 1.266e-4*delt      + 2.464e-6*delt2       &
                - ssw*(1.849e-5 - 2.551e-7*delt + 2.551e-8*delt2)
   TGL_beta   =              1.266e-4*TGL_delt  + 2.464e-6*TGL_delt2   &
                - ssw*(-2.551e-7*TGL_delt + 2.551e-8*TGL_delt2)
   signew     =   sig*exp(-beta*delt)
   TGL_signew = - signew*(TGL_beta*delt+beta*TGL_delt)

   om         = 2.0e9*pi*f
       cdum1  = cmplx(0.0,om*taunew)
   TGL_cdum1  = cmplx(0.0,om*TGL_taunew)
       cdum2  = cmplx(0.0,signew/(om*8.854e-12))
   TGL_cdum2  = cmplx(0.0,TGL_signew/(om*8.854e-12))

   cdum3      = 4.9 + (esnew-4.9)/(1.0 + cdum1) - cdum2
   TGL_cdum3  =  TGL_esnew/(1.0 + cdum1)  &
               - TGL_cdum1*(esnew-4.9)/((1.0 + cdum1)*(1.0 + cdum1))  &
               - TGL_cdum2
   epsr       = real(cdum3)
   TGL_epsr   = real(TGL_cdum3)
   epsi       = -aimag(cdum3)
   TGL_epsi   = -aimag(TGL_cdum3)

   if (trace_use) call da_trace_exit("da_epsalt_tl")

end subroutine da_epsalt_tl


