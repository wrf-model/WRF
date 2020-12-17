subroutine da_spemiss_tl(f,tk,theta,ssw,ev,eh,TGL_tk,TGL_ev,TGL_eh                               )

   !-----------------------------------------------------------------------
   ! Purpose: returns the specular emissivity of sea water for given 
   ! freq. (GHz), temperature T (K), incidence angle theta (degrees), 
   ! salinity (permil)
   !     
   ! Returned values verified against data in Klein and Swift (1977) and
   ! against Table 3.8 in Olson (1987, Ph.D. Thesis)
   !
   ! Input  : TGL_tk
   ! Output : TGL_ev, TGL_eh, ev, eh
   !------------------------------------------------------------------------

   implicit none

   real, intent(in)    :: f, tk, theta, TGL_tk
   real, intent(inout) :: ssw
   real, intent(out)   :: TGL_ev, TGL_eh, ev, eh

   real :: epsr,epsi,TGL_epsr,TGL_epsi

   real    ::  tc,costh,sinth,rthet
   complex ::  etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h,epsnew
   complex ::  TGL_etav,TGL_eps,TGL_cterm1v,TGL_cterm2,TGL_cterm3v
   complex ::  TGL_cterm3h,TGL_epsnew
   ! complex   uniti
   real    ::  tmp1r,tmp1i,tmp2r,tmp2i
   ! real :: rnorm,tmp0i,tmp0r
   real    ::  TGL_tc,TGL_tmp1r
   ! real :: TGL_tmp0r,TGL_tmp0i,TGL_rnorm
   real    ::  TGL_tmp1i,TGL_tmp2r,TGL_tmp2i 

   if (trace_use) call da_trace_entry("da_spemiss_tl")


   tc          =      tk - t_kelvin
   TGL_tc      =  TGL_tk

   call da_epsalt_tl(f,tc,ssw,epsr,epsi,TGL_tc, TGL_epsr, TGL_epsi )

       eps     =  cmplx(epsr,epsi)
   TGL_eps     =  cmplx(TGL_epsr,TGL_epsi)
       etav    =  eps
   TGL_etav    =  TGL_eps
   etah        =  (1.0,0.0)
   rthet       =  theta*0.017453292
   costh       =  cos(rthet)
   sinth       =  sin(rthet)
   sinth       =  sinth*sinth
       cterm1v =  etav*costh
   TGL_cterm1v =  TGL_etav*costh
   cterm1h     =  etah*costh
       epsnew  =      eps - sinth
   TGL_epsnew  =  TGL_eps
   cterm2      =  csqrt(epsnew)

   ! calculate TGL_cterm2

   if (cabs(epsnew) .gt. 0.0) then
      TGL_cterm2      =  TGL_epsnew*0.5/cterm2
   else
      TGL_cterm2      =  0.0
   end if

   ! Wei's Comment
   !     It is not a standard fortran if statement here.

   !     if (0) then
   !               tmp0r   =  real(epsnew)
   !           TGL_tmp0r   =  real(TGL_epsnew)
   !               tmp0i   = -aimag(epsnew)
   !           TGL_tmp0i   = -aimag(TGL_epsnew)
   !               rnorm   =  sqrt(tmp0r*tmp0r+tmp0i*tmp0i)
   !               uniti   =  (0,1)
   !           if (rnorm .gt. 0.0) then
   !             if (abs(tmp0i) .gt. 0.0) then
   !                TGL_rnorm =  (tmp0r*TGL_tmp0r + tmp0i*TGL_tmp0i)/rnorm
   !                TGL_cterm2=  cterm2*0.5*(TGL_rnorm/rnorm  &
   !                                      -uniti*(TGL_tmp0r*rnorm-TGL_rnorm*tmp0r)/(rnorm*tmp0i))
   !             else
   !                TGL_rnorm =  TGL_tmp0r
   !                TGL_cterm2=  TGL_tmp0r*0.5/sqrt(tmp0r)
   !             end if
   !           else 
   !             TGL_rnorm =  0.0
   !             TGL_cterm2=  0.0
   !           end if
   !     end if

   ! End Wei's Comment

       cterm3v =  (cterm1v - cterm2)/(cterm1v + cterm2)
   TGL_cterm3v =  (TGL_cterm1v - TGL_cterm2)/(cterm1v + cterm2) &
                 -cterm3v*(TGL_cterm1v + TGL_cterm2)/(cterm1v + cterm2)
       cterm3h =  (cterm1h - cterm2)/(cterm1h + cterm2)
   TGL_cterm3h = -TGL_cterm2/(cterm1h + cterm2) &
                 -cterm3h*TGL_cterm2/(cterm1h + cterm2)
       tmp1r   =  real(cterm3v)
   TGL_tmp1r   =  real(TGL_cterm3v)
       tmp1i   = -aimag(cterm3v)
   TGL_tmp1i   = -aimag(TGL_cterm3v)
   ! ev      =  1.0 - cabs(cterm3v)**2
       ev      =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)
   TGL_ev      = -2.0*tmp1r*TGL_tmp1r - 2.0*tmp1i*TGL_tmp1i

       tmp2r   =  real(cterm3h)
   TGL_tmp2r   =  real(TGL_cterm3h)
       tmp2i   = -aimag(cterm3h)
   TGL_tmp2i   = -aimag(TGL_cterm3h)
   ! eh      =  1.0 - cabs(cterm3h)**2
       eh      =  1.0 - (tmp2r*tmp2r+tmp2i*tmp2i)
   TGL_eh      = -2.0*tmp2r*TGL_tmp2r - 2.0*tmp2i*TGL_tmp2i

   if (trace_use) call da_trace_exit("da_spemiss_tl")

end subroutine da_spemiss_tl


