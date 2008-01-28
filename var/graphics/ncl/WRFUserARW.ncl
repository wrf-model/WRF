external wrf_user_fortran_util_0 "./wrf_user_fortran_util_0.so"
;external wrf_user_fortran_util_1 "./wrf_user_fortran_util_1.so"

;-----------------------------------------------------------------
;
;  mass coordinate version of ncl user routines

undef("wrf_user_set_xy")

function wrf_user_set_xy( var:float, xp:float, yp:float, angle:float )

local dims,x,y,slope,intercept,x0,y0,x1,y1,distance,dx,dy,dxy,npts,xy

begin

; find intersection of line and domain boundaries

  dims = dimsizes(var)

  if ((angle .gt. 315.) .or. (angle .lt. 45.) .or. \
      ((angle .gt. 135.) .and. (angle .lt. 225.)) )  then

;  x = y*slope + intercept

       slope = -(360.-angle)/45.
       if( angle .lt. 45. ) then
         slope = angle/45.
       end if
       if( angle .gt. 135.) then
         slope = (angle-180.)/45.
       end if
       intercept = xp - yp*slope

; find intersections with domain boundaries

       y0 = 0.
       x0 = y0*slope + intercept

       if( x0 .lt. 0.) then  ; intersect outside of left boundary
         x0 = 0.
         y0 =  (x0 - intercept)/slope
       end if
       if( x0 .gt. dims(2)-1) then  ; intersect outside of right boundary
         x0 = dims(2)-1
         y0 =  (x0 - intercept)/slope
       end if

       y1 = dims(1)-1.  ;  need to make sure this will be a float?
       x1 = y1*slope + intercept

       if( x1 .lt. 0.) then  ; intersect outside of left boundary
         x1 = 0.
         y1 =  (x1 - intercept)/slope
       end if
       if( x1 .gt. dims(2)-1) then  ; intersect outside of right boundary
         x1 = dims(2)-1
         y1 =  (x1 - intercept)/slope
       end if

  else

;  y = x*slope + intercept

       slope = (90.-angle)/45.
       if( angle .gt. 225. ) then
         slope = (270.-angle)/45.
       end if
       intercept = yp - xp*slope

; find intersections with domain boundaries

       x0 = 0.
       y0 = x0*slope + intercept

       if( y0 .lt. 0.) then  ; intersect outside of bottom boundary
         y0 = 0.
         x0 =  (y0 - intercept)/slope
       end if
       if( y0 .gt. dims(1)-1) then  ; intersect outside of top boundary
         y0 = dims(1)-1
         x0 =  (y0 - intercept)/slope
       end if

       x1 = dims(2)-1.  ;  need to make sure this will be a float?
       y1 = x1*slope + intercept

       if( y1 .lt. 0.) then  ; intersect outside of bottom boundary
         y1 = 0.
         x1 =  (y1 - intercept)/slope
       end if
       if( y1 .gt. dims(1)-1) then  ; intersect outside of top boundary
         y1 = dims(1)-1
         x1 =  (y1 - intercept)/slope
       end if

  end if  ; we have beginning and ending points

  dx = x1 - x0
  dy = y1 - y0
  distance = (dx*dx + dy*dy)^0.5
  npts = floattointeger(distance)
  dxy = new(1,float)
  dxy = distance/npts

  xy = new((/ npts, 2 /),float)

  dx = dx/npts
  dy = dy/npts

  do i=0,npts-1
    xy(i,0) = x0 + i*dx
    xy(i,1) = y0 + i*dy
  end do

; print(xy)
  return(xy)

end

;--------------------------------------------------------------

undef("wrf_user_intrp3d")

function wrf_user_intrp3d( var3d:float, z:float, terrain:float, plot_type:string, \
                           loc_param:float, angle:float                        )
local dims,dimz,var2d,ndims,staggered,znew,xy,xp, \
      var2dtmp,var2dz,z_max,z_min,dz,nlevels,z_var2d,i

begin

  if(plot_type .eq. "h" ) then   ;  horizontal cross section needed

    var2d = var3d(0,:,:) ; initial plane for data, should init to missing_value
    var2d@_FillValue = -999999
    var2d = var2d@_FillValue 
    if(isatt(var3d,"description")) then
      var2d@description = var3d@description 
    else
      var2d@description = " "
    end if

    dims = dimsizes(var3d)
    dimz = dimsizes(z)
    ndims = dimsizes(dims)

    staggered = 0

    do i=0,ndims-1
      staggered = staggered + dims(i) - dimz(i)
    end do

    if(staggered .eq. 0) then
      wrf_user_fortran_util_0 :: interp_3dz( var3d,var2d,z,loc_param,   \
                                             dims(2), dims(1), dims(0)  )
    else
      znew = var3d
      wrf_user_fortran_util_0 :: z_stag( znew, dims(2), dims(1), dims(0), \
                                         z   , dimz(2), dimz(1),dimz(0),  \
                                        terrain                          ) 
      wrf_user_fortran_util_0 :: interp_3dz( var3d,var2d,znew,loc_param,   \
                                             dims(2), dims(1), dims(0)  )
    end if 

    if ( loc_param .gt. 100.) then
      var2d@PlotLevelID = loc_param + " mb"
    else
      var2d@PlotLevelID = loc_param + " km"
    end if

  else

; set vertical cross section, base it on the unstaggered z.
    xy = wrf_user_set_xy( z, loc_param(0), loc_param(1), angle )
; do interp to proper z, we'll do this for each potential staggering
    dims = dimsizes(var3d)
    dimz = dimsizes(z)

; ---->  x staggered variable <-----------

    if(dims(2) .ne. dimz(2)) then 
      znew = var3d
      wrf_user_fortran_util_0 :: z_stag( znew, dims(2), dims(1), dims(0), \
                                         z   , dimz(2), dimz(1),dimz(0),  \
                                         terrain ) 
      xy(:,0) = xy(:,0) + 0.5
      xp = dimsizes(xy)
      var2dtmp = new( (/ dims(0), xp(0) /), float)
      var2dz    = new( (/ dims(0), xp(0) /), float)
; first we interp the variable, then z
      wrf_user_fortran_util_0 :: interp_2d_xy( var3d, var2dtmp, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
      wrf_user_fortran_util_0 :: interp_2d_xy( znew, var2dz, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
; here we could interp to a constant dz grid, or just send it all back
; to contour in the irregular coordinate

;      var2d = var2dtmp(:dimz(2)-2,:)   

      xy(:,0) = xy(:,0) - 0.5

    end if

; ---->  y staggered variable <-----------

    if(dims(1) .ne. dimz(1)) then 
      znew = var3d
      wrf_user_fortran_util_0 :: z_stag( znew, dims(2), dims(1), dims(0), \
                                         z   , dimz(2), dimz(1),dimz(0),  \
                                         terrain ) 
      xy(:,1) = xy(:,1) + 0.5
      xp = dimsizes(xy)
      var2dtmp = new( (/ dimz(0), xp(0) /), float)
      var2dz    = new( (/ dimz(0), xp(0) /), float)
; first we interp the variable, then z
      wrf_user_fortran_util_0 :: interp_2d_xy( var3d, var2dtmp, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
      wrf_user_fortran_util_0 :: interp_2d_xy( znew, var2dz, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
; here we could interp to a constant dz grid, or just send it all back
; to contour in the irregular coordinate

;      var2d = var2dtmp   ; we'll just pass back the irregular grid right now
      xy(:,1) = xy(:,1) - 0.5

    end if

; ---->  z staggered variable <-----------

    if(dims(0) .ne. dimz(0)) then 
      znew = var3d
      wrf_user_fortran_util_0 :: z_stag( znew, dims(2), dims(1), dims(0), \
                                         z   , dimz(2), dimz(1),dimz(0),  \
                                         terrain ) 
      xp = dimsizes(xy)
      var2dtmp = new( (/ dims(0), xp(0) /), float)
      var2dz    = new( (/ dims(0), xp(0) /), float)
; first we interp the variable, then z
      wrf_user_fortran_util_0 :: interp_2d_xy( var3d, var2dtmp, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
      wrf_user_fortran_util_0 :: interp_2d_xy( znew, var2dz, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
; here we could interp to a constant dz grid, or just send it all back
; to contour in the irregular coordinate

;      var2d = var2dtmp(:dims(2)-2,:)   

    end if

; ---->  unstaggered variable <-----------

    if( (dims(0) .eq. dimz(0)) .and. \
        (dims(1) .eq. dimz(1)) .and. \
        (dims(2) .eq. dimz(2))       ) then

      xp = dimsizes(xy)
      var2dtmp = new( (/ dims(0), xp(0) /), float)
      var2dz    = new( (/ dims(0), xp(0) /), float)

; first we interp the variable, then z

      wrf_user_fortran_util_0 :: interp_2d_xy( var3d, var2dtmp, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
      wrf_user_fortran_util_0 :: interp_2d_xy( z, var2dz, xy, \
                                               dims(2), dims(1), dims(0), xp(0) )
    end if

;  interp to constant z grid

      z_max = max(var2dz)
      z_min = min(var2dz)
      dz = 0.01*(z_max - z_min)
      nlevels = 101
      z_var2d = new( (/nlevels/), float)
      z_var2d(0) = z_min
;     print (z_max)
;     print (z_min)
;     print (dz)
;     print (xp(0))

      var2d = new( (/nlevels, xp(0)/), float)
 
      if(var2dz(0,0) .gt. var2dz(1,0) ) then  ; monotonically decreasing coordinate
        dz = -dz
        z_var2d(0) = z_max
      end if

   
      do i=1, nlevels-1
        z_var2d(i) = z_var2d(0)+i*dz
      end do

      do i=0,xp(0)-1
        wrf_user_fortran_util_0 :: interp_1d( var2dtmp(:,i), var2d(:,i), \
                                              var2dz(:,i), z_var2d,      \
                                              dims(0), nlevels     )
      end do


    if(isatt(var3d,"units")) then
      var2d@units = var3d@units
    else
      var2d@units = ""
    end if

    if(isatt(var3d,"description")) then
      var2d@description = var3d@description 
    else
      var2d@description = ""
    end if

   end if

  return(var2d)

end

;------------------------------------------------------------------

undef("wrf_user_getvar")

function wrf_user_getvar( nc_file:file, variable:string, time:integer )
local var,density,dimv,dimd,base,qv,thetap, \
      msft,dzetadz,vartmp,vartheta,varthetap,varpi

begin

  if( (variable .eq. "umet") .or. (variable .eq. "vmet") ) then

       vartmp = nc_file->V(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       
       dimv = dimsizes(vartmp)

       v = 0.5*(vartmp(:,:dimv(1)-2,:)+vartmp(:,1:dimv(1)-1,:))
       delete(vartmp)

       vartmp = nc_file->U(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       dimv = dimsizes(vartmp)
       u = 0.5*(vartmp(:,:,:dimv(2)-2) + vartmp(:,:,1:dimv(2)-1))

       pii = 3.14159265
       radians_per_degree = pii/180.

       map_projection = nc_file@MAP_PROJ

       if(map_projection .eq. 0) then  ; no projection

         if(variable .eq. "umet") then
           var = u
           var@description = "u met velocity"
           var@units = "m/s"
           return(var)
         else  ; must want vmet
           var = v
           var@description = "v met velocity"
           var@units = "m/s"
           return(var)
         end if
       end if

       cen_lat  = nc_file@CEN_LAT
       if(isatt(nc_file,"STAND_LON")) then
           cen_long = nc_file@STAND_LON
       else
           cen_long = nc_file@CEN_LON
       end if
       true_lat1 = nc_file@TRUELAT1
       true_lat2 = nc_file@TRUELAT2
       latitude  = nc_file->XLAT(0,:,:)
         TypeCheck = latitude@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = latitude
           delete(latitude)
           latitude = doubletofloat(vardouble)
         end if
       longitude = nc_file->XLONG(0,:,:)
         TypeCheck = longitude@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = longitude
           delete(longitude)
           longitude = doubletofloat(vardouble)
         end if

       cone = 1.
       if( map_projection .eq. 1) then    ; Lambert Conformal mapping
         if( (fabs(true_lat1 - true_lat2) .gt. 0.1) .and.  \
             (fabs(true_lat2 - 90. )      .gt. 0.1)       ) then
             cone = 10^(cos(true_lat1*radians_per_degree)) \
                   -10^(cos(true_lat2*radians_per_degree))
             cone = cone/(10^(tan(45. -fabs(true_lat1/2.)*radians_per_degree)) - \
                          10^(tan(45. -fabs(true_lat2/2.)*radians_per_degree))   )
         else
             cone = sin(fabs(true_lat1)*radians_per_degree)
         end if
       end if
       if(map_projection .eq. 2) then      ; polar steraographic
         cone = 1.
       end if
       if(map_projection .eq. 3) then      ; Mercator
         cone = 0.
       end if

       diff = longitude - cen_long
       dims = dimsizes(longitude)

       do i = 0, dims(0)-1
       do j = 0, dims(1)-1
         if(diff(i,j) .gt. 180.) then
           diff(i,j) = diff(i,j) - 360.
         end if
         if(diff(i,j) .lt. -180.) then
           diff(i,j) = diff(i,j) + 360.
         end if
       end do
       end do

;      alpha = diff * cone * radians_per_degree *sign(1.,latitude)
       alpha = diff
       do i = 0, dims(0)-1
       do j = 0, dims(1)-1
          if(latitude(i,j) .lt. 0.) then
            alpha(i,j) = - diff(i,j) * cone * radians_per_degree
          else
            alpha(i,j) = diff(i,j) * cone * radians_per_degree
          end if
       end do
       end do


       delete(dims)
       dims=dimsizes(v)
       var = new( (/ dims(0), dims(1), dims(2)/), float)
       if(variable .eq. "umet") then
         do k=0,dims(0)-1
           var(k,:,:) = v(k,:,:)*sin(alpha) + u(k,:,:)*cos(alpha)
         end do
         var@description = "u met velocity"
         var@units = "m/s"
       else  ; must want vmet
         do k=0,dims(0)-1
           var(k,:,:) = v(k,:,:)*cos(alpha) - u(k,:,:)*sin(alpha)
         end do
         var@description = "v met velocity"
         var@units = "m/s"
       end if

     return(var)

  end if

  if( (variable .eq. "umeta") .or. (variable .eq. "vmeta") ) then

       v = nc_file->V(time,:,:,:)
         TypeCheck = v@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = v
           delete(v)
           v = doubletofloat(vardouble)
         end if
       dimv = dimsizes(v)

       u = nc_file->U(time,:,:,:)
         TypeCheck = u@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = u
           delete(u)
           u = doubletofloat(vardouble)
         end if
       dimu = dimsizes(u)

       pii = 3.14159265
       radians_per_degree = pii/180.

       map_projection = nc_file@MAP_PROJ

       if(map_projection .eq. 0) then  ; no projection

         if(variable .eq. "umeta") then
           var = 0.5*(u(:,:,:dimv(2)-2) + u(:,:,1:dimv(2)-1))
           var@description = "u met velocity"
           var@units = "m/s"
           return(var)
         else  ; must want vmeta
           var = 0.5*(v(:,:dimv(1)-2,:)+v(:,1:dimv(1)-1,:))
           var@description = "v met velocity"
           var@units = "m/s"
           return(var)
         end if
       end if

       cen_lat  = nc_file@CEN_LAT
       if(isatt(nc_file,"STAND_LON")) then
           cen_long = nc_file@STAND_LON
       else
           cen_long = nc_file@CEN_LON
       end if
       true_lat1 = nc_file@TRUELAT1
       true_lat2 = nc_file@TRUELAT2
       latitude  = nc_file->XLAT(0,:,:)
         TypeCheck = latitude@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = latitude
           delete(latitude)
           latitude = doubletofloat(vardouble)
         end if
       longitude = nc_file->XLONG(0,:,:)
         TypeCheck = longitude@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = longitude
           delete(longitude)
           longitude = doubletofloat(vardouble)
         end if

       cone = 1.
       if( map_projection .eq. 1) then    ; Lambert Conformal mapping
         if( (fabs(true_lat1 - true_lat2) .gt. 0.1) .and.  \
             (fabs(true_lat2 - 90. )      .gt. 0.1)       ) then
             cone = 10^(cos(true_lat1*radians_per_degree)) \
                   -10^(cos(true_lat2*radians_per_degree))
             cone = cone/(10^(tan(45. -fabs(true_lat1/2.)*radians_per_degree)) - \
                          10^(tan(45. -fabs(true_lat2/2.)*radians_per_degree))   )
         else
             cone = sin(fabs(true_lat1)*radians_per_degree)
         end if
       end if
       if(map_projection .eq. 2) then      ; polar steraographic
         cone = 1.
       end if
       if(map_projection .eq. 3) then      ; Mercator
         cone = 0.
       end if

       dims = dimsizes(longitude)
       diff = new( (/ dims(0), dims(1) /), float)
       alpha = new( (/ dims(0), dims(1) /), float)

       uvmet = new( (/ 2, dimu(0), dimu(1), dimv(2)/), float)

       wrf_user_fortran_util_0 :: compute_uvmet( u,v, uvmet,  \
                                                 diff, alpha, \
                                                 longitude, latitude, \
                                                 cen_long, cone, \
                                                 radians_per_degree, \
                                                 dimv(2), dimu(1),dimv(0), \
                                                 dimu(2), dimv(1) )


       ;print(" returned from compute ")

         uvmet@description = " u,v met velocity"
         uvmet@units = "m/s"

     return(uvmet)

  end if

  if( variable .eq. "ua" ) then
       vartmp = nc_file->U(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       dimv = dimsizes(vartmp)
       var = 0.5*(vartmp(:,:,:dimv(2)-2) + vartmp(:,:,1:dimv(2)-1))
       var@description = "u Velocity"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "u" ) then
       var = nc_file->U(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       var@description = "u Velocity"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "va" ) then
       vartmp = nc_file->V(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       dimv = dimsizes(vartmp)
       var = 0.5*(vartmp(:,:dimv(1)-2,:)+vartmp(:,1:dimv(1)-1,:))
       var@description = "v Velocity"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "v" ) then
       var = nc_file->V(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       var@description = "v Velocity"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "wa" ) then
       vartmp = nc_file->W(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       dimv = dimsizes(vartmp)
       var = 0.5*(vartmp(0:dimv(0)-2,:,:)+vartmp(1:dimv(0)-1,:,:))
       var@description = "Vertical Velocity w"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "w" ) then
       var = nc_file->W(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       var@description = "Vertical Velocity w"
       var@units = "m/s"

       return(var)

  end if

  if( variable .eq. "eta-dot" ) then
       var = nc_file->WW(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       dimv = dimsizes(var)
       var@description = "Coordinate Vertical Velocity"
       var@units = "s-1"

       return(var)

  end if

  if( variable .eq. "th" ) then
       var = nc_file->T(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       var = var + 300.
       var@description = "Potential Temperature (theta) "
       var@units = "K"

       return(var)

  end if

  if( variable .eq. "p" ) then
       var = nc_file->P(time,:,:,:)
         TypeCheck = var@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = var
           delete(var)
           var = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       var = 0.01*(var+base)
                                              
       var@description = "Pressure"
       var@units = "mb"

       return(var)

  end if

  if( variable .eq. "Z" ) then
       vartmp = nc_file->PH(time,:,:,:)
         TypeCheck = vartmp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartmp
           delete(vartmp)
           vartmp = doubletofloat(vardouble)
         end if
       base = nc_file->PHB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       vartmp = (vartmp+base)/9.81
       dimv = dimsizes(vartmp)
       var= 0.5*(vartmp(0:dimv(0)-2,:,:)+vartmp(1:dimv(0)-1,:,:))
       var@description = "Height"
       var@units = "m"

       return(var)

  end if

  if( variable .eq. "tc" ) then

; compute theta, p, and then tc

       vartheta = nc_file->T(time,:,:,:)
         TypeCheck = vartheta@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartheta
           delete(vartheta)
           vartheta = doubletofloat(vardouble)
         end if
       vartheta = vartheta+300.

       varp = nc_file->P(time,:,:,:)
         TypeCheck = varp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = varp
           delete(varp)
           varp = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       varp = (varp+base)

       dimv = dimsizes(vartheta)
       var = new( (/ dimv(0), dimv(1), dimv(2) /), float)

       wrf_user_fortran_util_0 :: compute_tk( var,varp,vartheta, \
                                              dimv(2),dimv(1),dimv(0) )

       var = var - 273.16
       var@description = "Temperature"
       var@units = "C"

       return(var)

  end if

  if( variable .eq. "tc2" ) then

       vartheta = nc_file->TH2(time,:,:)
         TypeCheck = vartheta@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartheta
           delete(vartheta)
           vartheta = doubletofloat(vardouble)
         end if

       varp = nc_file->P(time,0,:,:)
         TypeCheck = varp@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = varp
           delete(varp)
           varp = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,0,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       varp = (varp+base)

       dimv = dimsizes(vartheta)
       var = new( (/ dimv(0), dimv(1) /), float)

       wrf_user_fortran_util_0 :: compute_tk_2d( var,varp,vartheta, \
                                                 dimv(1),dimv(0) )

       var = var - 273.16
       var@description = "2m Temperature"
       var@units = "C"

       delete(varp)
       delete(vartheta)

       return(var)

  end if

  if( variable .eq. "td" ) then

; compute p, then td

       qv = nc_file->QVAPOR(time,:,:,:)
         TypeCheck = qv@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = qv
           delete(qv)
           qv = doubletofloat(vardouble)
         end if
       p = nc_file->P(time,:,:,:)
         TypeCheck = p@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = p
           delete(p)
           p = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       p = 0.01*(p+base)

       dimv = dimsizes(qv)
       var = new( (/ dimv(0), dimv(1), dimv(2) /), float)

       wrf_user_fortran_util_0 :: compute_td( var,p,qv,   \
                                              dimv(2),dimv(1),dimv(0) )

;      qv = qv > 0.000
;      var = qv*p/(.622+qv)  ; vapor pressure 
;      var = var > 0.001            ; avoid problems near zero
;      var = (243.5/( (17.67/log(var/6.112)) - 1.0)) ; Bolton's approximation
;      var = (243.5*log(var)-440.8)/(19.48-log(var))

       var@description = "Dewpoint Temperature"
       var@units = "C"

       return(var)

  end if

  if( variable .eq. "td2" ) then

       qv = nc_file->Q2(time,:,:)
         TypeCheck = qv@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = qv
           delete(qv)
           qv = doubletofloat(vardouble)
         end if
       p = nc_file->P(time,0,:,:)
         TypeCheck = p@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = p
           delete(p)
           p = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,0,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       p = 0.01*(p+base)

       dimv = dimsizes(qv)
       var = new( (/ dimv(0), dimv(1) /), float)

       wrf_user_fortran_util_0 :: compute_td_2d( var,p,qv,dimv(1),dimv(0) )

       var@description = "2m Dewpoint Temperature"
       var@units = "C"

       delete(p)
       delete(qv)

       return(var)

  end if

  if( variable .eq. "iclw" ) then

; compute p, then iclw (p (or P) is in Pa)

       qc = nc_file->QCLOUD(time,:,:,:)
         TypeCheck = qc@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = qc
           delete(qc)
           qc = doubletofloat(vardouble)
         end if
       p = nc_file->P(time,:,:,:)
         TypeCheck = p@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = p
           delete(p)
           p = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       p = 0.01*(p+base)

       dimv = dimsizes(qc)
       var = new( (/ dimv(1), dimv(2) /), float)

       wrf_user_fortran_util_0 :: compute_iclw( var,p,qc,   \
                                              dimv(2),dimv(1),dimv(0) )
       var@description = "Int Cloud Water"
       var@units = "mm"

       return(var)

  end if

  if( variable .eq. "slvl" ) then

; compute theta

       vartheta = nc_file->T(time,:,:,:)
         TypeCheck = vartheta@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartheta
           delete(vartheta)
           vartheta = doubletofloat(vardouble)
         end if
       vartheta = vartheta+300.

       p = nc_file->P(time,:,:,:)
         TypeCheck = p@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = p
           delete(p)
           p = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       p = (p+base)

       dimv = dimsizes(vartheta)
       tk = new( (/ dimv(0), dimv(1), dimv(2) /), float)

       wrf_user_fortran_util_0 :: compute_tk( tk,p,vartheta, \
                                              dimv(2),dimv(1),dimv(0) )

       qv = nc_file->QVAPOR(time,:,:,:)
         TypeCheck = qv@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = qv
           delete(qv)
           qv = doubletofloat(vardouble)
         end if
       qv = qv > 0.000

       surf = new( (/ dimv(1), dimv(2) /), float)
       t_surf = new( (/ dimv(1), dimv(2) /), float)
       tmp1 = new( (/ dimv(1), dimv(2) /), float)
       itmp1 = new( (/ dimv(1), dimv(2) /), integer)

       zw = nc_file->PH(time,:,:,:)
         TypeCheck = zw@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = zw
           delete(zw)
           zw = doubletofloat(vardouble)
         end if
       zbase = nc_file->PHB(time,:,:,:)
         TypeCheck = zbase@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = zbase
           delete(zbase)
           zbase = doubletofloat(vardouble)
         end if
       zw = (zw + zbase)/9.81
       dimw = dimsizes(zw)
       z = 0.5*(zw(0:dimw(0)-2,:,:)+zw(1:dimw(0)-1,:,:))

       wrf_user_fortran_util_0 :: compute_seaprs( dimv(2),dimv(1),dimv(0),  \
                                                  z, tk, p, qv,             \
                                                  surf, t_surf,             \
                                                  tmp1, itmp1 )


       surf = 0.01*surf
       surf@description = "Sea Level Pressure"
       surf@units = "mb"

       return(surf)

  end if

  if( variable .eq. "rh" ) then

; compute p, tc, then rh

       qv = nc_file->QVAPOR(time,:,:,:)
         TypeCheck = qv@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = qv
           delete(qv)
           qv = doubletofloat(vardouble)
         end if
       qv = qv > 0.000

       p = nc_file->P(time,:,:,:)
         TypeCheck = p@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = p
           delete(p)
           p = doubletofloat(vardouble)
         end if
       base = nc_file->PB(time,:,:,:)
         TypeCheck = base@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = base
           delete(base)
           base = doubletofloat(vardouble)
         end if
       p = (p+base)

       vartheta = nc_file->T(time,:,:,:)
         TypeCheck = vartheta@FieldType
         if ( TypeCheck .eq. 105 ) then
           vardouble = vartheta
           delete(vartheta)
           vartheta = doubletofloat(vardouble)
         end if
       vartheta = vartheta+300.

       dimv = dimsizes(vartheta)
       tk  = new( (/ dimv(0), dimv(1), dimv(2) /), float)
       var = new( (/ dimv(0), dimv(1), dimv(2) /), float)

       wrf_user_fortran_util_0 :: compute_tk( tk,p,vartheta, \
                                              dimv(2),dimv(1),dimv(0) )
       wrf_user_fortran_util_0 :: compute_rh( qv,p,tk,var,  \
                                              dimv(2),dimv(1),dimv(0) )

       var@description = "Relative Humidty"
       var@units = "%"

       return(var)

  end if

;  end of diagnostic variable list - we must want a variable already in
;  the file.  check variable dimensionality and pull proper time  out of file

  ndims = dimsizes(filevardimsizes(nc_file,variable))
  if( ndims .eq. 4) then
    var = nc_file->$variable$(time,:,:,:)
  end if
  if( ndims .eq. 3) then
    var = nc_file->$variable$(time,:,:)
  end if
  if( ndims .eq. 2) then
    var = nc_file->$variable$(time,:)
  end if
  if( ndims .eq. 1) then
    var = nc_file->$variable$(time)
  end if

  TypeCheck = var@FieldType
  if ( TypeCheck .eq. 105 ) then
    vartemp = var
    delete(var)
    var = doubletofloat(vartemp)
  end if

  return(var)

end

;------------------------------------------------------------------

undef("wrf_user_list_times")

function wrf_user_list_times( nc_file:file )

local times, times_in_file, dims, i
begin

  times_in_file = nc_file->Times
  dims = dimsizes(times_in_file)
  times = new(dims(0),string)
  do i=0,dims(0)-1
    times(i) = chartostring(times_in_file(i,:))
  end do
  times@description = "times in file"
  print(times)

  return(times)

end


;------------------------------------------------------------------

undef("wrf_user_filter2d")

procedure wrf_user_filter2d( a:float, it:integer )

local dims

begin

  dims = dimsizes(a)
  b = a

  if( it .gt. 0) 
    wrf_user_fortran_util_0 :: filter2d( a, b, dims(1), dims(0), it )
  end if

end

;------------------------------------------------------------------

undef("wrf_user_find_ij_lat_long")

function wrf_user_find_ij_lat_long( nc_file:file, latitude:float, longitude:float )

local dims, real_i, real_j, loc, real_i, real_j

begin

  longitude_grid = nc_file->XLONG(0,:,:)
    TypeCheck = longitude_grid@FieldType
    if ( TypeCheck .eq. 105 ) then
      vardouble = longitude_grid
      delete(longitude_grid)
      longitude_grid = doubletofloat(vardouble)
    end if
  latitude_grid = nc_file->XLAT(0,:,:)
    TypeCheck = latitude_grid@FieldType
    if ( TypeCheck .eq. 105 ) then
      vardouble = latitude_grid
      delete(latitude_grid)
      latitude_grid = doubletofloat(vardouble)
    end if
  dims = dimsizes(latitude_grid)

  real_i = 0.
  real_j = 0.

    wrf_user_fortran_util_0 :: get_ij_lat_long( latitude_grid, \
                                                longitude_grid,\
                                                latitude,       \
                                                longitude,      \
                                                real_i, real_j, \
                                                dims(1), dims(0) )

    real_j = real_j - 1.
    real_i = real_i - 1.
    loc = (/ real_j, real_i /)
    return(loc)

end
