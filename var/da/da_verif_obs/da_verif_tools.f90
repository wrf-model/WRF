

module da_verif_tools
   
   
   implicit none
   
   
   ! Define some private constants

   real, private, parameter    :: pi = 3.1415926
   real, private, parameter    :: deg_per_rad = 180.0/pi
   real, private, parameter    :: rad_per_deg = pi / 180.0
   logical, private, parameter :: print_detail_map = .false.

   integer, private, parameter :: stdout = 6

   ! Mean Earth Radius in m.  The value below is consistent
   ! with NCEP's routines and grids.
   ! real, public , parameter    :: earth_radius_m = 6371200.0 ! from Brent
   real, public , parameter    :: earth_radius_m = 6370000.0
   real, public , parameter    :: radians_per_degree = pi / 180.0

   ! Define public parameters
 
   ! Projection codes for proj_info structure:
   integer, public, parameter  :: PROJ_LATLON = 0
   integer, public, parameter  :: PROJ_MERC = 1
   integer, public, parameter  :: PROJ_LC = 3
   integer, public, parameter  :: PROJ_PS = 5

   
  ! Define data structures to define various projections

   type proj_info
      integer          :: code     ! integer code for projection type
      real             :: lat1     ! SW latitude (1,1) in degrees (-90->90N)
      real             :: lon1     ! SW longitude (1,1) in degrees (-180->180E)
      real             :: dx       ! Grid spacing in meters at truelats, used
                                   ! only for ps, lc, and merc projections
      real             :: dlat     ! Lat increment for lat/lon grids
      real             :: dlon     ! Lon increment for lat/lon grids
      real             :: stdlon   ! Longitude parallel to y-axis (-180->180E)
      real             :: truelat1 ! First true latitude (all projections)
      real             :: truelat2 ! Second true lat (LC only)
      real             :: hemi     ! 1 for NH, -1 for SH
      real             :: cone     ! Cone factor for LC projections
      real             :: polei    ! Computed i-location of pole point
      real             :: polej    ! Computed j-location of pole point
      real             :: rsw      ! Computed radius to SW corner
      real             :: rebydx   ! Earth radius divided by dx
      real             :: knowni   ! X-location of known lat/lon
      real             :: knownj   ! Y-location of known lat/lon
      real             :: latinc   ! Latitude increments in degrees 
      real             :: loninc   ! Longitude increments in degrees 
      logical          :: init     ! Flag to indicate if this struct is 
                                 ! ready for use
   end type proj_info

   type(proj_info) :: map_info

   character(len=10000),private :: message(50)


contains

subroutine da_llxy_latlon(lat, lon, proj, x, y)

   !----------------------------------------------------------------------- 
   ! Purpose: Compute the x/y location of a lat/lon on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: lat
   real, intent(in)             :: lon
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: x
   real, intent(out)            :: y

   real                         :: deltalat
   real                         :: deltalon
   real                         :: lon360
   real                         :: latinc
   real                         :: loninc

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   latinc = proj%truelat1
   loninc = proj%stdlon

   ! Compute deltalat and deltalon as the difference between the input 
   ! lat/lon and the origin lat/lon

   deltalat = lat - proj%lat1

   ! To account for issues around the dateline, convert the incoming
   ! longitudes to be 0->360.0
   if (lon < 0) then 
      lon360 = lon + 360.0 
   else 
      lon360 = lon
   end if    
   deltalon = lon360 - proj%lon1      
    
   ! Compute x/y
   x = deltalon/loninc + 1.0
   y = deltalat/latinc + 1.0

end subroutine da_llxy_latlon


subroutine da_llxy_lc(lat, lon, proj, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: compute the geographical latitude and longitude values
   ! to the cartesian x/y on a Lambert Conformal projection.
   !-----------------------------------------------------------------------
    
   implicit none

   real, intent(in)              :: lat      ! Latitude (-90->90 deg N)
   real, intent(in)              :: lon      ! Longitude (-180->180 E)
   type(proj_info),intent(in)    :: proj     ! Projection info structure

   real, intent(out)             :: x        ! Cartesian X coordinate
   real, intent(out)             :: y        ! Cartesian Y coordinate

   real                          :: arg
   real                          :: deltalon
   real                          :: tl1r
   real                          :: rm
   real                          :: ctl1r

   ! Compute deltalon between known longitude and standard lon and ensure
   ! it is not in the cut zone
   deltalon = lon - proj%stdlon
   if (deltalon > +180.0) deltalon = deltalon - 360.0
   if (deltalon < -180.0) deltalon = deltalon + 360.0
    
   ! Convert truelat1 to radian and compute COS for later use
   tl1r = proj%truelat1 * rad_per_deg
   ctl1r = COS(tl1r)     
   
   ! Radius to desired point
   rm = proj%rebydx * ctl1r/proj%cone * &
       (TAN((90.0*proj%hemi-lat)*rad_per_deg/2.0) / &
        TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

   arg = proj%cone*(deltalon*rad_per_deg)
   x = proj%polei + proj%hemi * rm * Sin(arg)
   y = proj%polej - rm * COS(arg)

   ! Finally, if we are in the southern hemisphere, flip the i/j
   ! values to a coordinate system where (1,1) is the SW corner
   ! (what we assume) which is different than the original NCEP
   ! algorithms which used the NE corner as the origin in the 
   ! southern hemisphere (left-hand vs. right-hand coordinate?)
   if (proj%hemi == -1.0) then
      x = 2.0 - x  
      y = 2.0 - y
   end if

end subroutine da_llxy_lc


subroutine da_llxy_merc(lat, lon, proj, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: Compute x,y coordinate from lat lon for mercator projection
   !-----------------------------------------------------------------------
  
   implicit none

   real, intent(in)              :: lat
   real, intent(in)              :: lon
   type(proj_info),intent(in)    :: proj
   real,intent(out)              :: x
   real,intent(out)              :: y
   real                          :: deltalon

   deltalon = lon - proj%lon1
   if (deltalon < -180.0) deltalon = deltalon + 360.0
   if (deltalon > 180.0) deltalon = deltalon - 360.0
   x = 1.0 + (deltalon/(proj%dlon*deg_per_rad))
   y = 1.0 + (ALOG(TAN(0.5*((lat + 90.0) * rad_per_deg)))) / &
           proj%dlon - proj%rsw

end subroutine da_llxy_merc


subroutine da_llxy_ps(lat,lon,proj,x,y)

   !-----------------------------------------------------------------------
   ! Purpose: Given latitude (-90 to 90), longitude (-180 to 180), and the
   ! standard polar-stereographic projection information via the 
   ! public proj structure, this routine returns the x/y indices which
   ! if within the domain range from 1->nx and 1->ny, respectively.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)               :: lat
   real, intent(in)               :: lon
   type(proj_info),intent(in)     :: proj

   real, intent(out)              :: x !(x-index)
   real, intent(out)              :: y !(y-index)
   
   real                           :: reflon
   real                           :: scale_top
   real                           :: ala
   real                           :: alo
   real                           :: rm

   reflon = proj%stdlon + 90.0
   
   ! Compute numerator term of map scale factor

   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Find radius to desired point
   ala = lat * rad_per_deg
   rm = proj%rebydx * COS(ala) * scale_top/(1.0 + proj%hemi *Sin(ala))
   alo = (lon - reflon) * rad_per_deg
   x = proj%polei + rm * COS(alo)
   y = proj%polej + proj%hemi * rm * Sin(alo)

end subroutine da_llxy_ps


subroutine da_llxy_wrf(proj, lat, lon, x, y)

   !-----------------------------------------------------------------------
   ! Purpose: Converts input lat/lon values to the cartesian (x, y) value
   ! for the given projection. 
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)  :: proj
   real,            intent(in)  :: lat
   real,            intent(in)  :: lon
   real,            intent(out) :: x
   real,            intent(out) :: y

   if (.NOT.proj%init) then
      write(stdout,*) "da_llxy_wrf.inc"// &
           "You have not called map_set for this projection!"
      stop
   end if

   select case(proj%code)
 
      case(PROJ_LATLON)
         call da_llxy_latlon(lat,lon,proj,x,y)

      case(PROJ_MERC)
         call da_llxy_merc(lat,lon,proj,x,y)
         x = x + proj%knowni - 1.0
         y = y + proj%knownj - 1.0

      case(PROJ_PS)
         call da_llxy_ps(lat,lon,proj,x,y)
      
      case(PROJ_LC)
         call da_llxy_lc(lat,lon,proj,x,y)
         x = x + proj%knowni - 1.0
         y = y + proj%knownj - 1.0

      case default
         write(unit=message(1),fmt='(A,I2)') &
            'Unrecognized map projection code: ', proj%code
         write(stdout,*) "da_llxy_wrf.inc"//message(1:1)
         stop 
   end select

end subroutine da_llxy_wrf


subroutine da_xyll(proj, xx, yy, lat, lon)

   !-----------------------------------------------------------------------
   ! Purpose: Computes geographical latitude and longitude for a given (i,j) 
   ! point in a grid with a projection of proj
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)  :: proj
   real,            intent(in)  :: xx
   real,            intent(in)  :: yy
   real,            intent(out) :: lat
   real,            intent(out) :: lon

   real :: x, y

   if (.NOT.proj%init) then
      write(stdout,*) "da_xyll.inc"// &
           "You have not called map_set for this projection!"
      stop
   end if

   x = xx
   y = yy

   select case (proj%code)
      case (PROJ_LATLON)
         call da_xyll_latlon(x, y, proj, lat, lon)

      case (PROJ_MERC)
         x = xx - proj%knowni + 1.0
         y = yy - proj%knownj + 1.0
         call da_xyll_merc(x, y, proj, lat, lon)

      case (PROJ_PS)
         call da_xyll_ps(x, y, proj, lat, lon)

      case (PROJ_LC)

         x = xx - proj%knowni + 1.0
         y = yy - proj%knownj + 1.0
         call da_xyll_lc(x, y, proj, lat, lon)

      case default
         write(unit=message(1),fmt='(A,I2)') &
            "Unrecognized map projection code: ", proj%code
         write(stdout,*) "da_xyll.inc"//message(1:1)
         stop

   end select

end subroutine da_xyll


subroutine da_xyll_latlon(x, y, proj, lat, lon)

   !-----------------------------------------------------------------------
   ! Purpose: Compute the lat/lon location of an x/y on a LATLON grid.
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)             :: x
   real, intent(in)             :: y
   type(proj_info), intent(in)  :: proj
   real, intent(out)            :: lat
   real, intent(out)            :: lon

   real                         :: deltalat
   real                         :: deltalon
   real                         :: latinc
   real                         :: loninc

   ! Extract the latitude and longitude increments for this grid
   ! (e.g., 2.5 deg for NCEP reanalysis) from the proj structure, where
   ! loninc is saved in the stdlon tag and latinc is saved in truelat1

   latinc = proj%truelat1
   loninc = proj%stdlon

   ! Compute deltalat and deltalon 

   deltalat = (x-1.0)*latinc
   deltalon = (y-1.0)*loninc
   lat = proj%lat1 + deltalat
   lon = proj%lon1 + deltalon

   if ((ABS(lat) > 90.0).OR.(ABS(deltalon) > 360.0)) then
      ! Off the earth for this grid
      lat = -999.0
      lon = -999.0
   else
      lon = lon + 360.0
      lon = MOD(lon,360.0)
      if (lon > 180.0) lon = lon -360.0
   end if

end subroutine da_xyll_latlon


subroutine da_xyll_lc(x, y, proj, lat, lon)

   ! subroutine da_to convert from the (x,y) cartesian coordinate to the 
   ! geographical latitude and longitude for a Lambert Conformal projection.

   implicit none

   real, intent(in)              :: x        ! Cartesian X coordinate
   real, intent(in)              :: y        ! Cartesian Y coordinate
   type(proj_info),intent(in)    :: proj     ! Projection info structure

                
   real, intent(out)             :: lat      ! Latitude (-90->90 deg N)
   real, intent(out)             :: lon      ! Longitude (-180->180 E)

   real                          :: inew
   real                          :: jnew
   real                          :: r
   real                          :: chi,chi1,chi2
   real                          :: r2
   real                          :: xx
   real                          :: yy

   chi1 = (90.0 - proj%hemi*proj%truelat1)*rad_per_deg
   chi2 = (90.0 - proj%hemi*proj%truelat2)*rad_per_deg

   ! See if we are in the southern hemispere and flip the indices
   ! if we are. 
   if (proj%hemi == -1.0) then 
      inew = -x + 2.0
      jnew = -y + 2.0
   else
      inew = x
      jnew = y
   end if

   ! Compute radius**2 to i/j location
   xx = inew - proj%polei
   yy = proj%polej - jnew
   r2 = (xx*xx + yy*yy)
   r = sqrt(r2)/proj%rebydx
   
   ! Convert to lat/lon
   if (r2 == 0.0) then
      lat = proj%hemi * 90.0
      lon = proj%stdlon
   else
       
      ! Longitude
      lon = proj%stdlon + deg_per_rad * ATAN2(proj%hemi*xx,yy)/proj%cone
      lon = MOD(lon+360.0, 360.0)

      ! Latitude.  Latitude determined by solving an equation adapted 
      ! from:
      !  Maling, D.H., 1973: Coordinate Systems and Map Projections
      ! Equations #20 in Appendix I.  
        
      if (chi1 == chi2) then
         chi = 2.0*ATAN((r/TAN(chi1))**(1.0/proj%cone) * TAN(chi1*0.5))
      else
         chi = 2.0*ATAN((r*proj%cone/Sin(chi1))**(1.0/proj%cone) * TAN(chi1*0.5)) 
      end if
      lat = (90.0-chi*deg_per_rad)*proj%hemi
   end if

   if (lon > +180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

end subroutine da_xyll_lc


subroutine da_xyll_merc(x, y, proj, lat, lon)

   !-----------------------------------------------------------------------
   ! Compute the lat/lon from i/j for mercator projection
   !-----------------------------------------------------------------------

   implicit none

   real,intent(in)               :: x
   real,intent(in)               :: y    
   type(proj_info),intent(in)    :: proj
   real, intent(out)             :: lat
   real, intent(out)             :: lon 

   lat = 2.0*ATAN(EXP(proj%dlon*(proj%rsw + y-1.0)))*deg_per_rad - 90.0
   lon = (x-1.0)*proj%dlon*deg_per_rad + proj%lon1
   if (lon > 180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

end subroutine da_xyll_merc


subroutine da_xyll_ps(x, y, proj, lat, lon)

   ! This is the inverse subroutine da_of llij_ps.  It returns the 
   ! latitude and longitude of an x/y point given the projection info 
   ! structure.  

   implicit none

   real, intent(in)                    :: x    ! Column
   real, intent(in)                    :: y    ! Row
   type (proj_info), intent(in)        :: proj
    
   real, intent(out)                   :: lat     ! -90 -> 90 North
   real, intent(out)                   :: lon     ! -180 -> 180 East

   real                                :: reflon
   real                                :: scale_top
   real                                :: xx,yy
   real                                :: gi2, r2
   real                                :: arccos

   ! Compute the reference longitude by rotating 90 degrees to the east
   ! to find the longitude line parallel to the positive x-axis.
   reflon = proj%stdlon + 90.0
   
   ! Compute numerator term of map scale factor
   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Compute radius to point of interest
   xx = x - proj%polei
   yy = (y - proj%polej) * proj%hemi
   r2 = xx**2 + yy**2

   ! Now the magic code
   if (r2 == 0.0) then 
      lat = proj%hemi * 90.0
      lon = reflon
   else
      gi2 = (proj%rebydx * scale_top)**2.0
      lat = deg_per_rad * proj%hemi * ASin((gi2-r2)/(gi2+r2))
      arccos = ACOS(xx/sqrt(r2))
      if (yy > 0) then
         lon = reflon + deg_per_rad * arccos
      else
         lon = reflon - deg_per_rad * arccos
      end if
   end if
  
   ! Convert to a -180 -> 180 East convention
   if (lon > 180.0) lon = lon - 360.0
   if (lon < -180.0) lon = lon + 360.0

end subroutine da_xyll_ps


subroutine da_set_lc(proj)

   !-----------------------------------------------------------------------
   ! Purpose: Initialize the remaining items in the proj structure for a
   ! lambert conformal grid.
   !-----------------------------------------------------------------------

   implicit none
    
   type(proj_info), intent(inout) :: proj

   real :: arg
   real :: deltalon1
   real :: tl1r
   real :: ctl1r

   ! Compute cone factor
   call da_lc_cone(proj%truelat1, proj%truelat2, proj%cone)
   if (print_detail_map) then
      write(unit=stdout, fmt='(A,F8.6)') 'Computed cone factor: ', proj%cone
   end if
   ! Compute longitude differences and ensure we stay out of the
   ! forbidden "cut zone"
   deltalon1 = proj%lon1 - proj%stdlon
   if (deltalon1 .gt. +180.0) deltalon1 = deltalon1 - 360.0
   if (deltalon1 .lt. -180.0) deltalon1 = deltalon1 + 360.0

   ! Convert truelat1 to radian and compute COS for later use
   tl1r = proj%truelat1 * rad_per_deg
   ctl1r = COS(tl1r)

   ! Compute the radius to our known lower-left (SW) corner
   proj%rsw = proj%rebydx * ctl1r/proj%cone * &
           (TAN((90.0*proj%hemi-proj%lat1)*rad_per_deg/2.0) / &
            TAN((90.0*proj%hemi-proj%truelat1)*rad_per_deg/2.0))**proj%cone

   ! Find pole point
   arg = proj%cone*(deltalon1*rad_per_deg)
   proj%polei = 1.0 - proj%hemi * proj%rsw * Sin(arg)
   proj%polej = 1.0 + proj%rsw * COS(arg)  
   if (print_detail_map) then
      write(unit=stdout,fmt='(A,2F10.3)') 'Computed pole i/j = ', proj%polei, proj%polej
   end if

end subroutine da_set_lc                             


subroutine da_set_ps(proj)

   ! Initializes a polar-stereographic map projection from the partially
   ! filled proj structure. This routine computes the radius to the
   ! southwest corner and computes the i/j location of the pole for use
   ! in llij_ps and ijll_ps.

   implicit none
 
   type(proj_info), intent(inout)    :: proj

   real :: ala1
   real :: alo1
   real :: reflon
   real :: scale_top

   ! To define the cone factor for polar stereographic projection 
   proj%cone = 1.0

   reflon = proj%stdlon + 90.0

   ! Compute numerator term of map scale factor
   scale_top = 1.0 + proj%hemi * Sin(proj%truelat1 * rad_per_deg)

   ! Compute radius to lower-left (SW) corner
   ala1 = proj%lat1 * rad_per_deg
   proj%rsw = proj%rebydx*COS(ala1)*scale_top/(1.0+proj%hemi*Sin(ala1))

   ! Find the pole point
   alo1 = (proj%lon1 - reflon) * rad_per_deg
   proj%polei = proj%knowni - proj%rsw * COS(alo1)
   proj%polej = proj%knownj - proj%hemi * proj%rsw * Sin(alo1)
   if (print_detail_map) then
      write(unit=stdout,fmt='(A,2F10.1)') 'Computed (I,J) of pole point: ',proj%polei,proj%polej
   end if

end subroutine da_set_ps


subroutine da_map_init(proj)

   !-----------------------------------------------------------------------
   ! Purpose: Initializes the map projection structure to missing values
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(inout)  :: proj

   proj%lat1     = -999.9
   proj%lon1     = -999.9
   proj%dx       = -999.9
   proj%stdlon   = -999.9
   proj%truelat1 = -999.9
   proj%truelat2 = -999.9
   proj%hemi     = 0.0
   proj%cone     = -999.9
   proj%polei    = -999.9
   proj%polej    = -999.9
   proj%rsw      = -999.9
   proj%knowni   = -999.9
   proj%knownj   = -999.9
   proj%latinc   = -999.9
   proj%loninc   = -999.9
   proj%init     = .false.

end subroutine da_map_init


subroutine da_map_set(proj_code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,latinc,loninc,proj)
   ! Given a partially filled proj_info structure, this routine computes
   ! polei, polej, rsw, and cone (if LC projection) to complete the 
   ! structure.  This allows us to eliminate redundant calculations when
   ! calling the coordinate conversion routines multiple times for the
   ! same map.
   ! This will generally be the first routine called when a user wants
   ! to be able to use the coordinate conversion routines, and it
   ! will call the appropriate subroutines based on the 
   ! proj%code which indicates which projection type  this is.

   implicit none
   
   integer,         intent(in)  :: proj_code
   real,            intent(in)  :: lat1
   real,            intent(in)  :: lon1
   real,            intent(in)  :: dx
   real,            intent(in)  :: stdlon
   real,            intent(in)  :: truelat1
   real,            intent(in)  :: truelat2
   real,            intent(in)  :: knowni , knownj
   real,            intent(in)  :: latinc, loninc 
   type(proj_info), intent(out) :: proj

   ! First, check for validity of mandatory variables in proj
   if (ABS(lat1) > 90.0) then
      write(stdout,*) "da_map_set.inc"// &
         "Latitude of origin corner required as follows: -90N <= lat1 < = 90N"
      stop
   end if
   if (ABS(lon1) > 180.0) then
      write(stdout,*) "da_map_set.inc"// &
           "Longitude of origin required as follows: -180E <= lon1 <= 180W" 
      stop
   end if
   if ((dx .LE. 0.0).AND.(proj_code .NE. PROJ_LATLON)) then
      write(stdout,*)  "da_map_set.inc"// &
           "Require grid spacing (dx) in meters be positive!" 
      stop
   end if
   if ((ABS(stdlon) > 180.0).AND.(proj_code .NE. PROJ_MERC)) then
      write(stdout,*) "da_map_set.inc"// &
           "Need orientation longitude (stdlon) as: -180E <= lon1 <= 180W"  
      stop
   end if
   if (proj%code .NE. PROJ_LATLON .and. ABS(truelat1)>90.0) then
      write(stdout,*) "da_map_set.inc"// &
           "Set true latitude 1 for all projections!" 
      stop
   end if
   
   call da_map_init(proj) 
   proj%code  = proj_code
   proj%lat1 = lat1
   proj%lon1 = lon1
   proj%knowni = knowni
   proj%knownj = knownj
   proj%dx    = dx
   proj%stdlon = stdlon
   proj%truelat1 = truelat1
   proj%truelat2 = truelat2
   proj%latinc   = latinc  
   proj%loninc   = loninc  
   if (proj%code .NE. PROJ_LATLON) then
      proj%dx = dx
      if (truelat1 < 0.0) then
         proj%hemi = -1.0 
      else
         proj%hemi = 1.0
      end if
      proj%rebydx = earth_radius_m / dx
   end if
   pick_proj: select case(proj%code)

      case(PROJ_PS)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up POLAR STEREOGRAPHIC map...'
         end if
         call da_set_ps(proj)

      case(PROJ_LC)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up LAMBERT CONFORMAL map...'
         end if
         if (ABS(proj%truelat2) > 90.0) then
            if (print_detail_map) then
               write(unit=stdout,fmt='(A)') 'Second true latitude not set, assuming a tangent'
               write(unit=stdout,fmt='(A,F10.3)') 'projection at truelat1: ', proj%truelat1
            end if
            proj%truelat2=proj%truelat1
         end if
         call da_set_lc(proj)
   
      case (PROJ_MERC)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up MERCATOR map...'
         end if
         call da_set_merc(proj)
   
      case (PROJ_LATLON)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up CYLinDRICAL EQUIDISTANT LATLON map...'
         end if
         ! Convert lon1 to 0->360 notation
         if (proj%lon1 < 0.0) proj%lon1 = proj%lon1 + 360.0
   
         proj%cone = 1.0                                  
      case default
         write(unit=message(1),fmt='(A,I2)') 'Unknown projection code: ', proj%code
         write(stdout,*) "da_map_set.inc"//message(1:1)
         stop

   end select pick_proj
   proj%init = .true.

end subroutine da_map_set


subroutine da_set_merc(proj)
  
   !--------------------------------------------------------------------------
   ! Purpose: Sets up the remaining basic elements for the mercator projection
   !--------------------------------------------------------------------------

   implicit none

   type(proj_info), intent(inout)       :: proj

   real :: clain

   !  Preliminary variables

   clain = COS(rad_per_deg*proj%truelat1)
   proj%dlon = proj%dx / (earth_radius_m * clain)

   ! Compute distance from equator to origin, and store in the 
   ! proj%rsw tag.

   proj%rsw = 0.0
   if (proj%lat1 .NE. 0.0) then
      proj%rsw = (alog(tan(0.5*((proj%lat1+90.)*rad_per_deg))))/proj%dlon
   end if

end subroutine da_set_merc


subroutine da_lc_cone(truelat1, truelat2, cone)

   !------------------------------------------------------------------------
   ! Purpose: compute the cone factor of a Lambert Conformal projection
   !------------------------------------------------------------------------

   implicit none
    
   real, intent(in)             :: truelat1  ! (-90 -> 90 degrees N)
   real, intent(in)             :: truelat2  !   "   "  "   "     "
   real, intent(out)            :: cone

   ! First, see if this is a secant or tangent projection.  For tangent
   ! projections, truelat1 = truelat2 and the cone is tangent to the 
   ! Earth's surface at this latitude.  For secant projections, the cone
   ! intersects the Earth's surface at each of the distinctly different
   ! latitudes
   if (abs(truelat1-truelat2) > 0.1) then
      cone = alog10(cos(truelat1*rad_per_deg)) - &
             alog10(cos(truelat2*rad_per_deg))
      cone = cone /(alog10(tan((45.0 - abs(truelat1)/2.0) * rad_per_deg)) - &
             alog10(tan((45.0 - abs(truelat2)/2.0) * rad_per_deg)))        
   else
      cone = sin(abs(truelat1)*rad_per_deg)  
   end if

end subroutine da_lc_cone

end module da_verif_tools

