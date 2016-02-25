      subroutine spemiss(f,tk,theta,ssw,ev,eh)
!     returns the specular emissivity of sea water for given freq. (GHz), 
!     temperature T (K), incidence angle theta (degrees), salinity (permil)
!     
!     Returned values verified against data in Klein and Swift (1977) and
!     against Table 3.8 in Olson (1987, Ph.D. Thesis)
!
      real,intent(in   ) :: f,tk,theta
      real,intent(out  ) :: ev,eh
      real   epsr,epsi,ssw

      real   tc,costh,sinth,rthet,tmp1r,tmp1i
      complex   etav,etah,eps,cterm1v,cterm1h,cterm2,cterm3v,cterm3h
!

      tc = tk - t_kelvin
      call epsalt(f,tc,ssw,epsr,epsi)
      eps = cmplx(epsr,epsi)
      etav = eps
      etah = (1.0,0.0)
      rthet = theta*0.017453292
      costh = cos(rthet)
      sinth = sin(rthet)
      sinth = sinth*sinth
      cterm1v = etav*costh
      cterm1h = etah*costh
      eps = eps - sinth
      cterm2 = csqrt(eps)
      cterm3v = (cterm1v - cterm2)/(cterm1v + cterm2)
      cterm3h = (cterm1h - cterm2)/(cterm1h + cterm2)
      tmp1r   =  real(cterm3v)
      tmp1i   = -aimag(cterm3v)
!     ev = 1.0 - cabs(cterm3v)**2
      ev =  1.0 - (tmp1r*tmp1r+tmp1i*tmp1i)

      eh = 1.0 - cabs(cterm3h)**2

      end subroutine spemiss
!



