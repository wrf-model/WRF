subroutine xytoll(rx,ry,lat,lon,stagger)

  implicit none

  ! Arguments
  integer, intent(in) :: stagger
  real, intent(in) :: rx, ry
  real, intent(out) :: lat, lon
  
  ! Local variables
  integer :: ih,jh
  integer :: midcol,midrow,ncol,iadd1,iadd2,imt,jh2,knrow,krem,kv,nrow
  real :: dphd,dlmd !Grid increments, degrees
  real :: arg1,arg2,d2r,fctr,glatr,glatd,glond,pi, &
          r2d,tlatd,tlond,tlatr,tlonr,tlm0,tph0

    ih = nint(rx)
    jh = nint(ry)

    dphd = phi/real((jydim(current_nest_number)-1)/2)
    dlmd = lambda/real(ixdim(current_nest_number)-1)
  
    pi = dacos(-1.0)
    d2r = pi/180.
    r2d = 1./d2r
    tph0 = known_lat*d2r
    tlm0 = known_lon*d2r
   
    midrow = (jydim(current_nest_number)+1)/2
    midcol = ixdim(current_nest_number)
   
    if (stagger == HH) then
      ncol = 2*ih-1+mod(jh+1,2)
      tlatd = (jh-midrow)*dphd
      tlond = (ncol-midcol)*dlmd
    else if (stagger == VV) then
      imt = 2*ixdim(current_nest_number)-1
      jh2 = jh/2
      iadd1 = 0
      iadd2 = 0
  
      if (2*jh2 == jh) then
        iadd1 = -1
        iadd2 = ixdim(current_nest_number)-1
      end if
  
      kv = (jh2+iadd1)*imt+iadd2+ih
  
      nrow = 2*((kv-1)/imt)
      knrow = imt*nrow/2
      krem = kv-knrow

      if (krem <= ixdim(current_nest_number)-1) then
        nrow = nrow+1
        ncol = 2*krem
      else
        nrow = nrow+2
        ncol = 2*(krem-ixdim(current_nest_number))+1
      end if
      tlatd = (nrow-(jydim(current_nest_number)+1)/2)*dphd
      tlond = (ncol-ixdim(current_nest_number))*dlmd
    end if
  
    tlatr = tlatd*d2r
    tlonr = tlond*d2r
    arg1 = sin(tlatr)*cos(tph0)+cos(tlatr)*sin(tph0)*cos(tlonr)
    glatr = asin(arg1)
   
    glatd = glatr*r2d
   
    arg2 = dcos(tlatr)*dcos(tlonr)/(dcos(glatr)*dcos(tph0))-dtan(glatr)*dtan(tph0)
    if (abs(arg2) > 1.) arg2 = abs(arg2)/arg2
    fctr = 1.
    if (tlond > 0.) fctr = -1.
   
    glond = known_lon+fctr*dacos(arg2)*r2d
   
    xlat = glatd
    xlon = glond

end subroutine xytoll
