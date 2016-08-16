subroutine da_spemiss_adj(f,tk,theta,ssw,ev,eh, ADJ_tk,ADJ_ev,ADJ_eh)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   !------------------------------------------------------------------------
   ! Output :: ADJ_tk
   ! Input  :: ADJ_ev, ADJ_eh
   !------------------------------------------------------------------------

   real, intent(in ) :: f, tk, theta, ADJ_ev,ADJ_eh
   real, intent(inout) :: ssw
   real, intent(inout) :: ADJ_tk
   real, intent(out)   :: ev, eh

   real   epsr,epsi,ADJ_epsr,ADJ_epsi

   real      tc,costh,sinth,rthet
   complex   etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h,epsnew
   complex   ADJ_etav,ADJ_eps,ADJ_cterm1v,ADJ_cterm2,ADJ_cterm3v
   complex   ADJ_cterm3h,ADJ_epsnew
   real      tmp1r,tmp1i,tmp2r,tmp2i,tmp0r,tmp0i,rnorm
   real      ADJ_tc,ADJ_tmp0r,ADJ_tmp0i,ADJ_rnorm,ADJ_tmp1r
   real      ADJ_tmp1i,ADJ_tmp2r,ADJ_tmp2i

   if (trace_use) call da_trace_entry("da_spemiss_adj")

   epsr=0.0
   epsi=0.0
   ADJ_epsr=0.0
   ADJ_epsi=0.0
   ev=0.0
   eh=0.0
   tc=0.0
   costh=0.0
   sinth=0.0
   rthet=0.0
   tmp1r=0.0
   tmp1i=0.0
   tmp2r=0.0
   tmp2i=0.0
   tmp0r=0.0
   tmp0i=0.0
   rnorm=0.0
   ADJ_tc=0.0
   ADJ_tmp0r=0.0
   ADJ_tmp0i=0.0
   ADJ_rnorm=0.0
   ADJ_tmp1r=0.0
   ADJ_tmp1i=0.0
   ADJ_tmp2r=0.0
   ADJ_tmp2i=0.0

   tc     =      tk - t_kelvin

   call epsalt(f,tc,ssw,epsr,epsi)

   eps     =  cmplx(epsr,epsi)
   etav    =  eps
   etah    =  (1.0,0.0)
   rthet   =  theta*0.017453292
   costh   =  cos(rthet)
   sinth   =  sin(rthet)
   sinth   =  sinth*sinth
   cterm1v   =  etav*costh
   cterm1h   =  etah*costh
   epsnew   =  eps - sinth
   cterm2   =  csqrt(epsnew)

   cterm3v   =  (cterm1v - cterm2)/(cterm1v + cterm2)
   cterm3h   =  (cterm1h - cterm2)/(cterm1h + cterm2)
   tmp1r   =  real(cterm3v)
   tmp1i   = -aimag(cterm3v)
   ! ev   =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)

   tmp2r   =  real(cterm3h)
   tmp2i   = -aimag(cterm3h)
   ! eh   =  1.0 - (tmp2r*tmp2r+tmp2i*tmp2i)

   ADJ_tmp2r   = - 2.0*tmp2r*ADJ_eh
   ADJ_tmp2i   = - 2.0*tmp2i*ADJ_eh

   ADJ_cterm3h =  ADJ_tmp2r + ADJ_tmp2i*(0.0,1.0)

   ADJ_tmp1r   = - 2.0*tmp1r*ADJ_ev
   ADJ_tmp1i   = - 2.0*tmp1i*ADJ_ev

   ADJ_cterm3v =  ADJ_tmp1r + ADJ_tmp1i*(0.0,1.0)

   ADJ_cterm2  = - ADJ_cterm3h/(cterm1h + cterm2)
   ADJ_cterm2  = - cterm3h*ADJ_cterm3h/(cterm1h + cterm2) + ADJ_cterm2

   ADJ_cterm1v =   ADJ_cterm3v/(cterm1v + cterm2)
   ADJ_cterm2  = - ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm2
   ADJ_cterm1v = - cterm3v*ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm1v
   ADJ_cterm2  = - cterm3v*ADJ_cterm3v/(cterm1v + cterm2) + ADJ_cterm2

   if (cabs(epsnew) .gt. 0.0) then
      ADJ_epsnew  = ADJ_cterm2*0.5/cterm2
   else
      ADJ_epsnew  =  0.0
   end if

   ADJ_eps     =  ADJ_epsnew

   ADJ_etav    =  ADJ_cterm1v*costh

   ADJ_eps     =  ADJ_etav + ADJ_eps

   ADJ_epsr    =  real(ADJ_eps)
   ADJ_epsi    =  -aimag(ADJ_eps) 
   ADJ_tc      =  0.0
   call da_epsalt_adj(f,tc,ssw,ADJ_tc, ADJ_epsr, ADJ_epsi)

   ADJ_tk      =  ADJ_tc + ADJ_tk

   if (trace_use) call da_trace_exit("da_spemiss_adj")

end subroutine da_spemiss_adj


