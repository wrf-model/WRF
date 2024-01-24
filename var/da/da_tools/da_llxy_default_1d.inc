subroutine da_llxy_default_1d (xlati,xloni,x,y)

   !----------------------------------------------------------------------------
   ! Purpose:  calculates the (x,y) location (dot) in the mesoscale grids
   ! -------   from latitudes and longitudes
   !
   !           for global domain co-ordinates
   !
   !  input:
   !  -----
   !   xlat:    latitudes
   !   xlon:    longitudes
   !
   ! output:
   ! -----
   !   x:        the coordinate in x (i)-direction.
   !   y:        the coordinate in y (j)-direction.
   !
   !----------------------------------------------------------------------------
   
   implicit none
   
   real, intent(in)  :: xlati(:), xloni(:)
   real, intent(out) :: x(:), y(:)

   real, allocatable :: dxlon(:)
   real, allocatable :: xlat(:), xlon(:)
   real, allocatable :: xx(:), yy(:), cell(:), psx(:), r(:), flp(:)
   real              :: xc, yc
   real              :: psi0
   real              :: centri, centrj
   real              :: ratio
   real              :: bb
   real, parameter   :: conv = 180.0 / pi
   integer           :: n

   if (trace_use_frequent) call da_trace_entry("da_llxy_default_1d")

   n = size(xlati)
   allocate ( dxlon(n), xlat(n), xlon(n), xx(n), yy(n), cell(n), psx(n), r(n), flp(n) )
 
   xlon = xloni
   xlat = xlati

   where (xlat .lt. -89.95) xlat = -89.95
   where (xlat .gt. +89.95) xlat = +89.95
  
   dxlon = xlon - xlonc
   where (dxlon >  180) dxlon = dxlon - 360.0
   where (dxlon < -180) dxlon = dxlon + 360.0
   
   if (map_projection == 3) then
      xc = 0.0
      yc = YCNTR

      cell = cos(xlat/conv)/(1.0+sin(xlat/conv))
      yy = -c2*alog(cell)
      xx = c2*dxlon/conv
   else
      psi0 = (pole - phic)/conv
      xc = 0.0

      ! calculate x,y coords. relative to pole

      flp = cone_factor*dxlon/conv
   
      psx = (pole - xlat)/conv
   
      if (map_projection == 2) then
         ! Polar stereographics:
         bb = 2.0*(cos(psi1/2.0)**2)
         yc = -earth_radius*bb*tan(psi0/2.0)
         r  = -earth_radius*bb*tan(psx/2.0)
      else
         ! Lambert conformal:
         bb = -earth_radius/cone_factor*sin(psi1)
         yc = bb*(tan(psi0/2.0)/tan(psi1/2.0))**cone_factor
         r  = bb*(tan(psx /2.0)/tan(psi1/2.0))**cone_factor
      end if

      if (phic < 0.0) then
         xx = r*sin(flp)
         yy = r*cos(flp)
      else
         xx = -r*sin(flp)
         yy =  r*cos(flp)
      end if
   end if

   ! transform (1,1) to the origin
   ! the location of the center in the coarse domain

   centri = real (coarse_ix + 1)/2.0  
   centrj = real (coarse_jy + 1)/2.0  

   ! the (x,y) coordinates in the coarse domain

   x = (xx - xc)/coarse_ds + centri 
   y = (yy - yc)/coarse_ds + centrj  

   ratio = coarse_ds / dsm

   ! only add 0.5 so that x/y is relative to first cross points:

   x = (x - start_x) * ratio + 0.5
   y = (y - start_y) * ratio + 0.5

   deallocate ( dxlon, xlat, xlon, xx, yy, cell, psx, r, flp )

   if (trace_use_frequent) call da_trace_exit("da_llxy_default_1d")

end subroutine da_llxy_default_1d


