subroutine lltoxy_generic (xlat,xlon,x,y,&
     project,dskm,reflat,reflon,refx,refy,&
     cenlon,truelat1,truelat2)

!*****************************************************************************!
!  Notes    - Modeled after XYTOLL in the plots.o library                     !
!*****************************************************************************!

  implicit none

! Input: ---------------------------------------------------------------------!

  real ::             xlat       ! latitude of the point of interest.
  real ::             xlon       ! longitude of the point of interest.
  character(LEN=2) :: project    ! projection indicator ("ME", "CE", "LC", "ST")
  real ::             dskm         ! grid distance in km
  real ::             reflat
  real ::             reflon
  real ::             refx
  real ::             refy
  real ::             cenlon     ! Grid ratio with respect to MOAD
  real ::             truelat1   ! True latitude 1, closest to equator
  real ::             truelat2   ! True latitude 2, closest to pole

! Output: --------------------------------------------------------------------!

  real ::             x          ! x location of the given (lat,lon) point
  real ::             y          ! y location of the given (lat,lon) point

!  Parameters: ---------------------------------------------------------------!

  real, parameter :: pi = 3.141592653589793   ! you know!  pi = 180 degrees
  real, parameter :: piovr2 = pi/2.
!  real, parameter :: re = 6370.949     ! the radius of the earth in km
  real, parameter :: re = 6371.2       ! the radius of the earth in km
  real, parameter :: ce = 2.*pi*re     ! the circumference of the earth in km
  real, parameter :: degrad = pi/180.

!  Real variables: -----------------------------------------------------------!

  real          :: flat1
  real          :: confac            ! cone factor
  real          :: rcln              ! center longitude in radians  (local)
  real          :: dj                ! distance from pole to point  (local)
  real          :: di                ! distance from the central
                                     !  meridian to the point        (local)
  real          :: bm                ! calculation variable         (local)

  real :: rflt, rfln, rlat, rlon, diovrdj, disdjs, ri, rj
  real :: ct1, st1, tt1, drp
  real :: isn, pi2
  real :: londiff

!****************************  subroutine begin  *****************************!


  rlat =  xlat * degrad
  rlon =  xlon * degrad
  rcln = cenlon * degrad
  flat1 = truelat1 * degrad
  rflt = reflat * degrad
  rfln = reflon * degrad

  if ((xlon - cenlon) < -180) then
     londiff = ((xlon-cenlon)+360.)*degrad
  else if ((xlon - cenlon) > 180) then
     londiff = ((xlon-cenlon)-360.)*degrad
  else
     londiff = (xlon-cenlon)*degrad
  endif
     

  if (project(1:2) .eq. 'ME') then

     ct1 = re*cos(flat1)
     dj = ct1 * log(tan (0.5*(rlat + piovr2)))
     di = ct1 * (rlon - rfln)
     y = refy +(dj + ct1 * log(cos(rflt)/(1 + sin(rflt))))/dskm
     x = refx + di/dskm

  else if (project(1:2) .eq. 'CE') then

!KWM     dj = re*(rlat-rflt)
!KWM     di = re*(rlon-rfln)
!KWM     y = refy + dj/dskm
!KWM     x = refx + di/dskm
     
     ! NOTE:  Cylindrical Equidistant grid increment expressed in terms 
     ! of thousandths of degrees!
     !  Calculate the distance from the horizontal axis to (J,I)
     dj = xlat-reflat
     di = xlon-reflon
     y = refy + (dj/dskm)
     x = refx + (di/dskm)

  else if (project(1:2) .eq. 'LC') then

     isn = sign(1.0, truelat2)
     pi2 = piovr2*isn

     call lccone(truelat1,truelat2,int(sign(1.0, truelat2)),confac)
     tt1 = tan((pi2 - flat1)*0.5) ! Tangent Term 1.
     st1 = sin (pi2 - flat1) * re/(confac*dskm)     ! Sine Term 1.
     bm =  tan((pi2 - rlat)*0.5)
     if ((rlon-rcln) > pi) then
        diovrdj = -tan(((rlon-rcln)-2.*pi)*confac)
     elseif ((rlon-rcln) < -pi) then
        diovrdj = -tan(((rlon-rcln)+2.*pi)*confac)
     else
        diovrdj = -tan((rlon-rcln)*confac)
     endif
     disdjs = ( (bm/tt1)**confac * st1)**2
     ! Dj: y distance (km) from pole to given x/y point.
     dj = -sqrt(disdjs/(1+diovrdj*diovrdj))
     ! Di: x distance (km) from central longitude to given x/y point.
     di = dj * diovrdj

     bm = tan((pi2-rflt)*0.5)
     if ((rfln-rcln) > pi) then
        diovrdj = -tan(((rfln-rcln)-2.*pi)*confac)
     else if ((rfln-rcln) < -pi) then
        diovrdj = -tan(((rfln-rcln)+2.*pi)*confac)
     else
        diovrdj = -tan((rfln - rcln)*confac)
     endif
     disdjs = ( (bm/tt1)**confac * st1)**2
     ! Rj: y distance (km) from pole to reference point.
     rj = -sqrt(disdjs/(1+diovrdj*diovrdj))
     ! Ri: x distance (km) from central longitude to reference x/y point.
     ri = rj * diovrdj
     y = refy + isn*(dj - rj)
     x = refx + (di - ri)

  else if (project(1:2) .eq. 'ST') then

     isn = sign(1.0, truelat1)
     pi2 = piovr2*isn

     diovrdj = -tan(rfln-rcln)
     bm = (re/dskm)*tan(0.5*(pi2-rflt))
     disdjs = (bm*(1.0 + cos(pi2-flat1)))**2
     ! RJ:  Distance from pole to reference latitude along the center lon
     rj = -sign(sqrt(disdjs/(1+diovrdj**2)),cos(rfln-rcln))
     ! RI:  Distance from center reference to requested point rlat, rlon.
     ri = rj * diovrdj
     diovrdj = -tan(rlon-rcln)
     bm = (re/dskm)*tan(0.5*(pi2-rlat))
     disdjs = (bm*(1.0 + cos(pi2-flat1)))**2
     dj = -sign(sqrt(disdjs/(1+diovrdj**2)),cos(rlon-rcln))
     di = dj * diovrdj
     y = refy + isn*(dj-rj)
     x = refx + (di-ri)
  endif

!*****************************  Subroutine End  ******************************!

end subroutine lltoxy_generic

subroutine xytoll_generic (x,y,xlat,xlon,&
     project,dskm,reflat,reflon,refx,refy,&
     cenlon, truelat1,truelat2)

!*****************************************************************************!
!  xytoll                                                                     !
!  Purpose  - To  transform  mesoscale grid point coordinates (x,y) into      !
!             latitude and longitude coordinates.                             !
!                                                                             !
!  On entry - X  and  Y are an ordered pair representing a grid point in  the !
!             mesoscale grid.                                                 !
!                                                                             !
!  On exit  - XLAT, XLON contain  the latitude and longitude respectively     !
!             that resulted from the transformation.                          !
!                                                                             !
!*****************************************************************************!
  implicit none

! Input
  real ::             x         ! x (i) coordinate of point of interest
  real ::             y         ! y (j) coordinate of point of interest
  character(LEN=2) :: project   ! projection indicator ("ME", "CE", "LC", "ST")
  real ::             dskm      ! grid distance in km
  real ::             reflat    ! latitude of the reference point
  real ::             reflon    ! longitude of the reference point
  real ::             refx
  real ::             refy
  real ::             cenlon    ! longitude of the center of the projection
  real ::             truelat1  ! True latitude 1.
  real ::             truelat2  ! True latitude 2.

! Output
  real ::             xlat      ! latitude of point (x,y)
  real ::             xlon      ! longitude of point (x,y)

! Parameters

  real, parameter :: pi = 3.141592653589793   ! you know!  pi = 180 degrees
  real, parameter :: piovr2 = pi/2.
  real, parameter :: twopi  = pi*2.
!  real, parameter :: re = 6370.949     ! the radius of the earth in km
  real, parameter :: re = 6371.2
  real, parameter :: degrad = pi/180.

!  Real variables

  real ::            confac           ! cone factor
  real ::            rfln             ! reference longitude in radians  (local)
  real ::            rflt             ! reference latitude in radians   (local)
  real ::            rcln             ! center longitude in radians  (local)
  real ::            dj, drp, djj     ! distance from the central
!                                       meridian to the point        (local)
  real ::            di,dii           ! distance from pole to point  (local)
  real ::            bm               ! calculation variable         (local)
  real ::            flat1
  real ::            ct1, tt1, tt2, pi2
  integer :: isn
!****************************  subroutine begin  *****************************!

  rflt = reflat * degrad
  rfln = reflon * degrad
  rcln = cenlon * degrad
  flat1 = truelat1 * degrad

!  If the projection is mercator ('ME') then ...

  if (project(1:2) .eq. 'ME') then
     di = (x-refx) * dskm
     !  Calculate the distance the point in question is from the pole
     dj = -re * cos(flat1)*log(cos(rflt)/(1 + sin(rflt))) + &
          (y - refy) * dskm
     !  Calculate the latitude desired in radians
     xlat = 2.0 * atan(exp(dj/(re*cos(flat1)))) - piovr2
     !  Calculate the longitude desired in radians
     xlon = rfln + di/(re*cos(flat1))

     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi


! If the projection is cylindrical equidistant ('CE') then ...
  else if (project(1:2) .eq. 'CE') then
     ! NOTE:  Cylindrical Equidistant grid increment expressed in terms 
     ! of thousandths of degrees!
     di = (x-refx) * dskm
     !  Calculate the distance from the horizontal axis to (J,I)
     dj = (y-refy) * dskm
     !  Determine the shift north-south
     xlat = reflat + dj
     !  Determine the shift east-west
     xlon = reflon + di

! If the projection is lambert conic conformal ('LC') then ...
  else if (project(1:2) .eq. 'LC') then

     isn = sign(1.0, truelat2)
     pi2 = piovr2*isn

     call lccone(truelat1,truelat2,isn,confac)

     tt1 = tan((pi2 - flat1)*0.5)    ! Tangent Term 1.
     tt2 = tan((pi2 - rflt  )*0.5)    ! Tangent Term 2.
     ct1 = -cos(flat1) * re/confac   ! cosine Term 1.

     ! Calculate the (projected) distance from the pole to 
     ! the reference lat/lon.
     drp = ct1 * (tt2/tt1)**confac
     ! Now from the pole to the reference y along the center lon.
     djj = drp * cos((rcln-rfln)*confac)

     ! Now from the pole to the requested y along the center lon.
     dj = djj + isn * ((y-refy)*dskm)
     ! Now the (projected) distance from center longitude to reference x
     dii = drp*sin((rcln-rfln)*confac)
     ! And now from center longitude to requested X
     di = dii + ((x-refx)*dskm)
     !  Calculate the Big Messy equation
     bm = tt1 * (sqrt(di**2+dj**2) /  abs(ct1))**(1.0/confac)
     !  Calculate the desired latitude in radians
     xlat = pi2 - 2.0*atan(bm)
     !  Calculate the desired longitude in radians
     xlon = rcln + (1.0/confac) * atan2(di,-dj)
     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi


!  If the projection is polar stereographic ('ST') then ...

  else if (project(1:2) .eq. 'ST') then

     isn = sign(1.0, truelat1)
     pi2 = piovr2*isn


!  Calculate the (projected) y-distance I,J lies from the pole

     drp = -re*cos(rflt) * (1.0 + cos(pi2-flat1)) / (1.0 + cos(pi2-rflt))
     djj = drp * cos(rcln-rfln)
     dj = djj + isn*( (y-refy) * dskm )
     dii = drp * sin(rcln-rfln)
     di = dii + ((x-refx)*dskm)
     ! Calculate the Big Messy quantity as would be done for LC
     ! projections.  This quantity is different in value, same 
     ! in purpose of BM above
     bm = (1.0/re) * sqrt(di*di + dj*dj) / (1.0 + cos(pi2-flat1))
     !  Calculate the desired latitude in radians
     xlat = pi2 - isn*2.0 * atan(bm)
     !  Calculate the desired longitude in radians
     xlon = rcln + atan2(di,-dj)

     !  Convert the calculated lat,lon pair into degrees
     xlat = xlat * 180.0/pi
     xlon = xlon * 180.0/pi

  else
     print*, 'Unrecognized project:  ', project
     stop
  end if

!  Make sure no values are greater than 180 degrees and none
!  are less than -180 degrees

  if (xlon .gt. 180.0)  xlon = xlon - 360.0
  if (xlon .lt. -180.0) xlon = xlon + 360.0

!*****************************  Subroutine End  ******************************!

end subroutine xytoll_generic
