subroutine lltoxy(lat,lon,rx,ry,stagger)

  implicit none

  ! Arguments
  integer, intent(in) :: stagger 
  real, intent(in) :: lat, lon
  real, intent(out) :: rx, ry
  
  ! Local variables
  real :: dphd,dlmd !Grid increments, degrees
  integer :: ii,imt,jj,jmt,k,krows,ncol,nrow
  real :: glatd  !Geographic latitude, positive north
  real :: glond  !Geographic longitude, positive west
  real :: col,d1,d2,d2r,dlm,dlm1,dlm2,dph,glat,glon,    &
          pi,r2d,row,tlat,tlat1,tlat2,              &
          tlon,tlon1,tlon2,tph0,tlm0,x,y,z

    glatd = lat
    glond = lon

    dphd = phi/real((jydim(current_nest_number)-1)/2)
    dlmd = lambda/real(ixdim(current_nest_number)-1)

    pi = dacos(-1.0)
    d2r = pi/180.
    r2d = 1./d2r

    imt = 2*ixdim(current_nest_number)-1
    jmt = jydim(current_nest_number)/2+1

    glat = glatd*d2r
    glon = glond*d2r
    dph = dphd*d2r
    dlm = dlmd*d2r
    tph0 = known_lat*d2r
    tlm0 = known_lon*d2r

    x = cos(tph0)*cos(glat)*cos(glon-tlm0)+sin(tph0)*sin(glat)
    y = -cos(glat)*sin(glon-tlm0)
    z = cos(tph0)*sin(glat)-sin(tph0)*cos(glat)*cos(glon-tlm0)
    tlat = r2d*atan(z/sqrt(x*x+y*y))
    tlon = r2d*atan(y/x)

    row = tlat/dphd+jmt
    col = tlon/dlmd+ixdim(current_nest_number)
    nrow = int(row)
    ncol = int(col)
    tlat = tlat*d2r
    tlon = tlon*d2r

    if (stagger == HH) then

      if ((mod(nrow,2) == 1 .and. mod(ncol,2) == 1) .or. &
          (mod(nrow,2) == 0 .and. mod(ncol,2) == 0)) then
        tlat1 = (nrow-jmt)*dph
        tlat2 = tlat1+dph
        tlon1 = (ncol-ixdim(current_nest_number))*dlm
        tlon2 = tlon1+dlm
        dlm1 = tlon-tlon1
        dlm2 = tlon-tlon2
        d1 = acos(cos(tlat)*cos(tlat1)*cos(dlm1)+sin(tlat)*sin(tlat1))
        d2 = acos(cos(tlat)*cos(tlat2)*cos(dlm2)+sin(tlat)*sin(tlat2))

        if (d1 > d2) then
          nrow = nrow+1
          ncol = ncol+1
        end if

      else
        tlat1 = (nrow+1-jmt)*dph
        tlat2 = tlat1-dph
        tlon1 = (ncol-ixdim(current_nest_number))*dlm
        tlon2 = tlon1+dlm
        dlm1 = tlon-tlon1
        dlm2 = tlon-tlon2
        d1 = acos(cos(tlat)*cos(tlat1)*cos(dlm1)+sin(tlat)*sin(tlat1))
        d2 = acos(cos(tlat)*cos(tlat2)*cos(dlm2)+sin(tlat)*sin(tlat2))

        if (d1 < d2) then
          nrow = nrow+1
        else
          ncol = ncol+1
        end if
      end if

    else if (stagger == VV) then

      if ((mod(nrow,2) == 0 .and. mod(ncol,2) == 1) .or. &
          (mod(nrow,2) == 1 .and. mod(ncol,2) == 0)) then
        tlat1 = (nrow-jmt)*dph
        tlat2 = tlat1+dph
        tlon1 = (ncol-ixdim(current_nest_number))*dlm
        tlon2 = tlon1+dlm
        dlm1 = tlon-tlon1
        dlm2 = tlon-tlon2
        d1 = acos(cos(tlat)*cos(tlat1)*cos(dlm1)+sin(tlat)*sin(tlat1))
        d2 = acos(cos(tlat)*cos(tlat2)*cos(dlm2)+sin(tlat)*sin(tlat2))

        if (d1 > d2) then
          nrow = nrow+1
          ncol = ncol+1
        end if

      else
        tlat1 = (nrow+1-jmt)*dph
        tlat2 = tlat1-dph
        tlon1 = (ncol-ixdim(current_nest_number))*dlm
        tlon2 = tlon1+dlm
        dlm1 = tlon-tlon1
        dlm2 = tlon-tlon2
        d1 = acos(cos(tlat)*cos(tlat1)*cos(dlm1)+sin(tlat)*sin(tlat1))
        d2 = acos(cos(tlat)*cos(tlat2)*cos(dlm2)+sin(tlat)*sin(tlat2))

        if (d1 < d2) then
          nrow = nrow+1
        else
          ncol = ncol+1
        end if
      end if
    end if

    jj = nrow
    ii = ncol/2
    if (stagger == HH) then
      if (mod(jj,2) == 1) ii = ii+1
      krows = ((nrow-1)/2)*imt
      if (mod(nrow,2) == 1) then
        k = krows+(ncol+1)/2
      else
        k = krows+ixdim(current_nest_number)+ncol/2
      end if

    else if (stagger == VV) then
      if (mod(jj,2) == 0) ii=ii+1

      krows = ((nrow-1)/2)*imt
      if (mod(nrow,2) == 1) then
        k = krows+ncol/2
      else
        k = krows+ixdim(current_nest_number)-1+(ncol+1)/2
      end if
    end if

    rx = real(ii)
    ry = real(jj)

end subroutine lltoxy
